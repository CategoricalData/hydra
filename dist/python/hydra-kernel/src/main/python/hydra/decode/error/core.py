# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.error.core."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.decode.paths
import hydra.decode.variants
import hydra.error.core
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def constant_condition_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_constant_condition_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralBoolean(value=b):
                            return Right(b)

                        case _:
                            return Left(hydra.errors.DecodingError("expected boolean literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("value", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_value: Right(hydra.error.core.ConstantConditionError(field_location, field_value))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_constant_condition_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def duplicate_binding_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_duplicate_binding_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.DuplicateBindingError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_duplicate_binding_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def duplicate_field_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_duplicate_field_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.DuplicateFieldError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_duplicate_field_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def duplicate_record_type_field_names_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_duplicate_record_type_field_names_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.DuplicateRecordTypeFieldNamesError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_duplicate_record_type_field_names_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def duplicate_union_type_field_names_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_duplicate_union_type_field_names_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.DuplicateUnionTypeFieldNamesError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_duplicate_union_type_field_names_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_case_statement_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_case_statement_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("typeName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_type_name: Right(hydra.error.core.EmptyCaseStatementError(field_location, field_type_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_case_statement_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_let_bindings_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_let_bindings_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.EmptyLetBindingsError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_let_bindings_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_record_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_record_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.EmptyRecordTypeError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_record_type_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_term_annotation_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_term_annotation_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.EmptyTermAnnotationError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_term_annotation_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_type_annotation_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_type_annotation_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.EmptyTypeAnnotationError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_type_annotation_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_type_name_in_term_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_type_name_in_term_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.EmptyTypeNameInTermError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_type_name_in_term_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_union_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_empty_union_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.EmptyUnionTypeError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_empty_union_type_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_forall_parameter_name_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_forall_parameter_name_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.InvalidForallParameterNameError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_forall_parameter_name_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_lambda_parameter_name_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_lambda_parameter_name_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.InvalidLambdaParameterNameError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_lambda_parameter_name_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_let_binding_name_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_let_binding_name_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.InvalidLetBindingNameError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_let_binding_name_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_type_lambda_parameter_name_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_type_lambda_parameter_name_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.InvalidTypeLambdaParameterNameError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_type_lambda_parameter_name_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def nested_term_annotation_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_nested_term_annotation_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.NestedTermAnnotationError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_nested_term_annotation_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def redundant_wrap_unwrap_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_redundant_wrap_unwrap_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("typeName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_type_name: Right(hydra.error.core.RedundantWrapUnwrapError(field_location, field_type_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_redundant_wrap_unwrap_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def self_application_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_self_application_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.SelfApplicationError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_self_application_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def term_variable_shadowing_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_term_variable_shadowing_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.TermVariableShadowingError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_term_variable_shadowing_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def type_variable_shadowing_in_type_lambda_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_type_variable_shadowing_in_type_lambda_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.TypeVariableShadowingInTypeLambdaError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_type_variable_shadowing_in_type_lambda_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def undefined_term_variable_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_term_variable_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTermVariableError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_undefined_term_variable_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def undefined_type_variable_in_binding_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_type_variable_in_binding_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTypeVariableInBindingTypeError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_undefined_type_variable_in_binding_type_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def undefined_type_variable_in_lambda_domain_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_type_variable_in_lambda_domain_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTypeVariableInLambdaDomainError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_undefined_type_variable_in_lambda_domain_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def undefined_type_variable_in_type_application_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_type_variable_in_type_application_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTypeVariableInTypeApplicationError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_undefined_type_variable_in_type_application_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def unknown_primitive_name_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_unknown_primitive_name_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UnknownPrimitiveNameError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_unknown_primitive_name_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def unnecessary_identity_application_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_unnecessary_identity_application_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.UnnecessaryIdentityApplicationError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_unnecessary_identity_application_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def untyped_term_variable_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_untyped_term_variable_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UntypedTermVariableError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_untyped_term_variable_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_term_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_term_error_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("constantCondition"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorConstantCondition(t))), constant_condition_error(cx, input)))), (hydra.core.Name("duplicateBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateBinding(t))), duplicate_binding_error(cx, input)))), (hydra.core.Name("duplicateField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateField(t))), duplicate_field_error(cx, input)))), (hydra.core.Name("emptyCaseStatement"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyCaseStatement(t))), empty_case_statement_error(cx, input)))), (hydra.core.Name("emptyLetBindings"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyLetBindings(t))), empty_let_bindings_error(cx, input)))), (hydra.core.Name("emptyTermAnnotation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTermAnnotation(t))), empty_term_annotation_error(cx, input)))), (hydra.core.Name("emptyTypeNameInTerm"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorEmptyTypeNameInTerm(t))), empty_type_name_in_term_error(cx, input)))), (hydra.core.Name("invalidLambdaParameterName"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorInvalidLambdaParameterName(t))), invalid_lambda_parameter_name_error(cx, input)))), (hydra.core.Name("invalidLetBindingName"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorInvalidLetBindingName(t))), invalid_let_binding_name_error(cx, input)))), (hydra.core.Name("invalidTypeLambdaParameterName"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorInvalidTypeLambdaParameterName(t))), invalid_type_lambda_parameter_name_error(cx, input)))), (hydra.core.Name("nestedTermAnnotation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorNestedTermAnnotation(t))), nested_term_annotation_error(cx, input)))), (hydra.core.Name("redundantWrapUnwrap"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorRedundantWrapUnwrap(t))), redundant_wrap_unwrap_error(cx, input)))), (hydra.core.Name("selfApplication"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorSelfApplication(t))), self_application_error(cx, input)))), (hydra.core.Name("termVariableShadowing"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorTermVariableShadowing(t))), term_variable_shadowing_error(cx, input)))), (hydra.core.Name("typeVariableShadowingInTypeLambda"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorTypeVariableShadowingInTypeLambda(t))), type_variable_shadowing_in_type_lambda_error(cx, input)))), (hydra.core.Name("undefinedTermVariable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTermVariable(t))), undefined_term_variable_error(cx, input)))), (hydra.core.Name("undefinedTypeVariableInBindingType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTypeVariableInBindingType(t))), undefined_type_variable_in_binding_type_error(cx, input)))), (hydra.core.Name("undefinedTypeVariableInLambdaDomain"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTypeVariableInLambdaDomain(t))), undefined_type_variable_in_lambda_domain_error(cx, input)))), (hydra.core.Name("undefinedTypeVariableInTypeApplication"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUndefinedTypeVariableInTypeApplication(t))), undefined_type_variable_in_type_application_error(cx, input)))), (hydra.core.Name("unknownPrimitiveName"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUnknownPrimitiveName(t))), unknown_primitive_name_error(cx, input)))), (hydra.core.Name("unnecessaryIdentityApplication"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUnnecessaryIdentityApplication(t))), unnecessary_identity_application_error(cx, input)))), (hydra.core.Name("untypedTermVariable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorUntypedTermVariable(t))), untyped_term_variable_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_term_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_type_scheme_variable_name_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_type_scheme_variable_name_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.InvalidTypeSchemeVariableNameError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_type_scheme_variable_name_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def nested_type_annotation_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_nested_type_annotation_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.NestedTypeAnnotationError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_nested_type_annotation_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def non_comparable_map_key_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_non_comparable_map_key_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("keyType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_key_type: Right(hydra.error.core.NonComparableMapKeyTypeError(field_location, field_key_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_non_comparable_map_key_type_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def non_comparable_set_element_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_non_comparable_set_element_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("elementType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_element_type: Right(hydra.error.core.NonComparableSetElementTypeError(field_location, field_element_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_non_comparable_set_element_type_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def single_variant_union_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_single_variant_union_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("fieldName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_field_name: Right(hydra.error.core.SingleVariantUnionError(field_location, field_field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_single_variant_union_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def type_variable_shadowing_in_forall_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_type_variable_shadowing_in_forall_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.TypeVariableShadowingInForallError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_type_variable_shadowing_in_forall_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def undefined_type_variable_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_type_variable_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTypeVariableError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_undefined_type_variable_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def void_in_non_bottom_position_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_void_in_non_bottom_position_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("location", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_location: Right(hydra.error.core.VoidInNonBottomPositionError(field_location))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_void_in_non_bottom_position_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def invalid_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermInject(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.error.core.InvalidTypeError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("duplicateRecordTypeFieldNames"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorDuplicateRecordTypeFieldNames(t))), duplicate_record_type_field_names_error(cx, input)))), (hydra.core.Name("duplicateUnionTypeFieldNames"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorDuplicateUnionTypeFieldNames(t))), duplicate_union_type_field_names_error(cx, input)))), (hydra.core.Name("emptyRecordType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorEmptyRecordType(t))), empty_record_type_error(cx, input)))), (hydra.core.Name("emptyTypeAnnotation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorEmptyTypeAnnotation(t))), empty_type_annotation_error(cx, input)))), (hydra.core.Name("emptyUnionType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorEmptyUnionType(t))), empty_union_type_error(cx, input)))), (hydra.core.Name("invalidForallParameterName"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorInvalidForallParameterName(t))), invalid_forall_parameter_name_error(cx, input)))), (hydra.core.Name("invalidTypeSchemeVariableName"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorInvalidTypeSchemeVariableName(t))), invalid_type_scheme_variable_name_error(cx, input)))), (hydra.core.Name("nestedTypeAnnotation"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorNestedTypeAnnotation(t))), nested_type_annotation_error(cx, input)))), (hydra.core.Name("nonComparableMapKeyType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorNonComparableMapKeyType(t))), non_comparable_map_key_type_error(cx, input)))), (hydra.core.Name("nonComparableSetElementType"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorNonComparableSetElementType(t))), non_comparable_set_element_type_error(cx, input)))), (hydra.core.Name("singleVariantUnion"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorSingleVariantUnion(t))), single_variant_union_error(cx, input)))), (hydra.core.Name("typeVariableShadowingInForall"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorTypeVariableShadowingInForall(t))), type_variable_shadowing_in_forall_error(cx, input)))), (hydra.core.Name("undefinedTypeVariable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorUndefinedTypeVariable(t))), undefined_type_variable_error(cx, input)))), (hydra.core.Name("voidInNonBottomPosition"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTypeError, hydra.error.core.InvalidTypeErrorVoidInNonBottomPosition(t))), void_in_non_bottom_position_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_invalid_type_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def undefined_field_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_field_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("fieldName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_field_name: hydra.lib.eithers.bind(hydra.extract.core.require_field("typeName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_type_name: Right(hydra.error.core.UndefinedFieldError(field_field_name, field_type_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_undefined_field_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def unexpected_term_variant_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_unexpected_term_variant_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("expectedVariant", (lambda x1, x2: hydra.decode.variants.term_variant(x1, x2)), field_map(), cx), (lambda field_expected_variant: hydra.lib.eithers.bind(hydra.extract.core.require_field("actualTerm", (lambda x1, x2: hydra.decode.core.term(x1, x2)), field_map(), cx), (lambda field_actual_term: Right(hydra.error.core.UnexpectedTermVariantError(field_expected_variant, field_actual_term))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_unexpected_term_variant_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def unexpected_type_variant_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_unexpected_type_variant_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("expectedVariant", (lambda x1, x2: hydra.decode.variants.type_variant(x1, x2)), field_map(), cx), (lambda field_expected_variant: hydra.lib.eithers.bind(hydra.extract.core.require_field("actualType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_actual_type: Right(hydra.error.core.UnexpectedTypeVariantError(field_expected_variant, field_actual_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_error_core_unexpected_type_variant_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
