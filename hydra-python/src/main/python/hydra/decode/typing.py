# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.typing."""

from __future__ import annotations
from hydra.dsl.python import Either, FrozenDict, Left, Right
import hydra.core
import hydra.decode.core
import hydra.extract.helpers
import hydra.graph
import hydra.lexical
import hydra.lib.eithers
import hydra.typing
import hydra.util

def inference_context(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceContext]:
    def _hoist_hydra_decode_typing_inference_context_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceContext]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, bool]:
                    match v1:
                        case hydra.core.LiteralBoolean(value=b):
                            return Right(b)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected boolean literal"))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, bool]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("schemaTypes", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type_scheme, v1, v2)), field_map(), cx), (lambda field_schema_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("primitiveTypes", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type_scheme, v1, v2)), field_map(), cx), (lambda field_primitive_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("dataTypes", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type_scheme, v1, v2)), field_map(), cx), (lambda field_data_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("classConstraints", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type_variable_metadata, v1, v2)), field_map(), cx), (lambda field_class_constraints: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("debug", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_debug: Right(hydra.typing.InferenceContext(field_schema_types, field_primitive_types, field_data_types, field_class_constraints, field_debug))))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.InferenceContext"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_inference_context_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_subst(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeSubst]:
    def _hoist_hydra_decode_typing_type_subst_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeSubst]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.typing.TypeSubst(b)), hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type, cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.typing.TypeSubst"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_type_subst_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def inference_result(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceResult]:
    def _hoist_hydra_decode_typing_inference_result_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.InferenceResult]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("term", hydra.decode.core.term, field_map(), cx), (lambda field_term: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("type", hydra.decode.core.type, field_map(), cx), (lambda field_type: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("subst", type_subst, field_map(), cx), (lambda field_subst: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("classConstraints", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type_variable_metadata, v1, v2)), field_map(), cx), (lambda field_class_constraints: Right(hydra.typing.InferenceResult(field_term, field_type, field_subst, field_class_constraints))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.InferenceResult"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_inference_result_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def term_subst(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TermSubst]:
    def _hoist_hydra_decode_typing_term_subst_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TermSubst]:
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.typing.TermSubst(b)), hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.term, cx, wrapped_term.body))
            
            case _:
                return Left(hydra.util.DecodingError("expected wrapped type hydra.typing.TermSubst"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_term_subst_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_constraint(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeConstraint]:
    def _hoist_hydra_decode_typing_type_constraint_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeConstraint]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                def _hoist_body_1(v1: hydra.core.Literal) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected string literal"))
                def _hoist_body_2(v1: hydra.core.Term) -> Either[hydra.util.DecodingError, str]:
                    match v1:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_body_1(v)
                        
                        case _:
                            return Left(hydra.util.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("left", hydra.decode.core.type, field_map(), cx), (lambda field_left: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("right", hydra.decode.core.type, field_map(), cx), (lambda field_right: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("comment", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped2: _hoist_body_2(stripped2)), hydra.lexical.strip_and_dereference_term_either(cx2, raw2))), field_map(), cx), (lambda field_comment: Right(hydra.typing.TypeConstraint(field_left, field_right, field_comment))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.TypeConstraint"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_type_constraint_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def type_context(cx: hydra.graph.Graph, raw: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeContext]:
    def _hoist_hydra_decode_typing_type_context_1(cx: hydra.graph.Graph, v1: hydra.core.Term) -> Either[hydra.util.DecodingError, hydra.typing.TypeContext]:
        match v1:
            case hydra.core.TermRecord(value=record):
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("types", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.type, v1, v2)), field_map(), cx), (lambda field_types: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("metadata", (lambda v1, v2: hydra.extract.helpers.decode_map(hydra.decode.core.name, hydra.decode.core.term, v1, v2)), field_map(), cx), (lambda field_metadata: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeVariables", (lambda v1, v2: hydra.extract.helpers.decode_set(hydra.decode.core.name, v1, v2)), field_map(), cx), (lambda field_type_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("lambdaVariables", (lambda v1, v2: hydra.extract.helpers.decode_set(hydra.decode.core.name, v1, v2)), field_map(), cx), (lambda field_lambda_variables: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("inferenceContext", inference_context, field_map(), cx), (lambda field_inference_context: Right(hydra.typing.TypeContext(field_types, field_metadata, field_type_variables, field_lambda_variables, field_inference_context))))))))))))
            
            case _:
                return Left(hydra.util.DecodingError("expected record of type hydra.typing.TypeContext"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.util.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_typing_type_context_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
