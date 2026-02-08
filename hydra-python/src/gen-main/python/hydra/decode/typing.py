# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.typing."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Left, Maybe, Right, frozenlist
from typing import TypeVar, cast
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.typing
import hydra.util

T0 = TypeVar("T0")
T1 = TypeVar("T1")

def function_structure(env: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T0]], cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.FunctionStructure[T0]]:
    def _hoist_hydra_decode_typing_function_structure_1(cx: hydra.graph.Graph, env: Callable[[hydra.graph.Graph, hydra.core.Term], Either[hydra.util.DecodingError, T1]], v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.FunctionStructure[T1]]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeParams", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_type_params: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("params", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_params: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("bindings", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.binding(x1, x2)), v12, v2)), field_map(), cx), (lambda field_bindings: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("body", (lambda x1, x2: hydra.decode.core.term(x1, x2)), field_map(), cx), (lambda field_body: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("domains", (lambda v12, v2: hydra.extract.helpers.decode_list((lambda x1, x2: hydra.decode.core.type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_domains: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("codomain", (lambda v12, v2: hydra.extract.helpers.decode_maybe((lambda x1, x2: hydra.decode.core.type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_codomain: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("environment", env, field_map(), cx), (lambda field_environment: Right(hydra.typing.FunctionStructure(field_type_params, field_params, field_bindings, field_body, field_domains, field_codomain, field_environment))))))))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.FunctionStructure"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_function_structure_1(cx, env, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def inference_context(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceContext]:
    def _hoist_hydra_decode_typing_inference_context_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceContext]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, bool]:
                    match v12:
                        case hydra.core.LiteralBoolean(value=b):
                            return Right(b)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected boolean literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, bool]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("schemaTypes", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type_scheme(x1, x2)), v12, v2)), field_map(), cx), (lambda field_schema_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("primitiveTypes", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type_scheme(x1, x2)), v12, v2)), field_map(), cx), (lambda field_primitive_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("dataTypes", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type_scheme(x1, x2)), v12, v2)), field_map(), cx), (lambda field_data_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("classConstraints", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type_variable_metadata(x1, x2)), v12, v2)), field_map(), cx), (lambda field_class_constraints: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("debug", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_debug: Right(hydra.typing.InferenceContext(field_schema_types, field_primitive_types, field_data_types, field_class_constraints, field_debug))))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.InferenceContext"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_inference_context_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_subst(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeSubst]:
    def _hoist_hydra_decode_typing_type_subst_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeSubst]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.typing.TypeSubst(b)), hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type(x1, x2)), cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.typing.TypeSubst"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_type_subst_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def inference_result(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceResult]:
    def _hoist_hydra_decode_typing_inference_result_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceResult]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", (lambda x1, x2: hydra.decode.core.term(x1, x2)), field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("subst", (lambda x1, x2: type_subst(x1, x2)), field_map(), cx), (lambda field_subst: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("classConstraints", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type_variable_metadata(x1, x2)), v12, v2)), field_map(), cx), (lambda field_class_constraints: Right(hydra.typing.InferenceResult(field_term, field_type, field_subst, field_class_constraints))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.InferenceResult"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_inference_result_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_subst(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TermSubst]:
    def _hoist_hydra_decode_typing_term_subst_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TermSubst]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.typing.TermSubst(b)), hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.term(x1, x2)), cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.typing.TermSubst"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_term_subst_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_constraint(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeConstraint]:
    def _hoist_hydra_decode_typing_type_constraint_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeConstraint]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v12: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v12: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("left", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_left: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("right", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_right: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("comment", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_comment: Right(hydra.typing.TypeConstraint(field_left, field_right, field_comment))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.TypeConstraint"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_type_constraint_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_context(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeContext]:
    def _hoist_hydra_decode_typing_type_context_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeContext]:
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("types", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.type(x1, x2)), v12, v2)), field_map(), cx), (lambda field_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("metadata", (lambda v12, v2: hydra.extract.helpers.decode_map((lambda x1, x2: hydra.decode.core.name(x1, x2)), (lambda x1, x2: hydra.decode.core.term(x1, x2)), v12, v2)), field_map(), cx), (lambda field_metadata: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeVariables", (lambda v12, v2: hydra.extract.helpers.decode_set((lambda x1, x2: hydra.decode.core.name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_type_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("lambdaVariables", (lambda v12, v2: hydra.extract.helpers.decode_set((lambda x1, x2: hydra.decode.core.name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_lambda_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("letVariables", (lambda v12, v2: hydra.extract.helpers.decode_set((lambda x1, x2: hydra.decode.core.name(x1, x2)), v12, v2)), field_map(), cx), (lambda field_let_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("inferenceContext", (lambda x1, x2: inference_context(x1, x2)), field_map(), cx), (lambda field_inference_context: Right(hydra.typing.TypeContext(field_types, field_metadata, field_type_variables, field_lambda_variables, field_let_variables, field_inference_context))))))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.TypeContext"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_type_context_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
