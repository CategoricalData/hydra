# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.errors."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.core
import hydra.decode.core
import hydra.decode.error.checking
import hydra.decode.error.core
import hydra.decode.paths
import hydra.errors
import hydra.extract.core
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def decoding_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_decoding_error_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_errors_decoding_error_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_errors_decoding_error_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_errors_decoding_error_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.errors.DecodingError(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_errors_decoding_error_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_decoding_error_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def empty_list_error(cx: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.DecodingError, None]:
    return hydra.extract.core.decode_unit(cx, t)

def multiple_bindings_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_multiple_bindings_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.errors.MultipleBindingsError(field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_multiple_bindings_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def multiple_fields_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_multiple_fields_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("fieldName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_field_name: Right(hydra.errors.MultipleFieldsError(field_field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_multiple_fields_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def no_matching_field_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_no_matching_field_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("fieldName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_field_name: Right(hydra.errors.NoMatchingFieldError(field_field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_no_matching_field_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def no_such_binding_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_no_such_binding_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.errors.NoSuchBindingError(field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_no_such_binding_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def not_enough_cases_error(cx: hydra.graph.Graph, t: hydra.core.Term) -> Either[hydra.errors.DecodingError, None]:
    return hydra.extract.core.decode_unit(cx, t)

def unexpected_shape_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_unexpected_shape_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                def _hoist_field_map_body_3(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_4(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_3(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("expected", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_expected: hydra.lib.eithers.bind(hydra.extract.core.require_field("actual", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_4(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_actual: Right(hydra.errors.UnexpectedShapeError(field_expected, field_actual))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_unexpected_shape_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def extraction_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_extraction_error_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.errors.ExtractionError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("emptyList"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorEmptyList(t))), empty_list_error(cx, input)))), (hydra.core.Name("multipleBindings"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorMultipleBindings(t))), multiple_bindings_error(cx, input)))), (hydra.core.Name("multipleFields"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorMultipleFields(t))), multiple_fields_error(cx, input)))), (hydra.core.Name("noMatchingField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorNoMatchingField(t))), no_matching_field_error(cx, input)))), (hydra.core.Name("noSuchBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorNoSuchBinding(t))), no_such_binding_error(cx, input)))), (hydra.core.Name("notEnoughCases"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorNotEnoughCases(t))), not_enough_cases_error(cx, input)))), (hydra.core.Name("unexpectedShape"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ExtractionError, hydra.errors.ExtractionErrorUnexpectedShape(t))), unexpected_shape_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_extraction_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def other_inference_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_other_inference_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("path", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_path: hydra.lib.eithers.bind(hydra.extract.core.require_field("message", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_message: Right(hydra.errors.OtherInferenceError(field_path, field_message))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_other_inference_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def unification_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_unification_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                def _hoist_field_map_body_1(v12):
                    match v12:
                        case hydra.core.LiteralString(value=s):
                            return Right(s)

                        case _:
                            return Left(hydra.errors.DecodingError("expected string literal"))
                def _hoist_field_map_body_2(v12):
                    match v12:
                        case hydra.core.TermLiteral(value=v):
                            return _hoist_field_map_body_1(v)

                        case _:
                            return Left(hydra.errors.DecodingError("expected literal"))
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("leftType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_left_type: hydra.lib.eithers.bind(hydra.extract.core.require_field("rightType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_right_type: hydra.lib.eithers.bind(hydra.extract.core.require_field("message", (lambda cx2, raw2: hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_field_map_body_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx2, raw2))), field_map(), cx), (lambda field_message: Right(hydra.errors.UnificationError(field_left_type, field_right_type, field_message))))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_unification_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def unification_inference_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_unification_inference_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("path", (lambda x1, x2: hydra.decode.paths.subterm_path(x1, x2)), field_map(), cx), (lambda field_path: hydra.lib.eithers.bind(hydra.extract.core.require_field("cause", (lambda x1, x2: unification_error(x1, x2)), field_map(), cx), (lambda field_cause: Right(hydra.errors.UnificationInferenceError(field_path, field_cause))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_unification_inference_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def inference_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_inference_error_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.errors.InferenceError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("checking"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.InferenceError, hydra.errors.InferenceErrorChecking(t))), hydra.decode.error.checking.checking_error(cx, input)))), (hydra.core.Name("other"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.InferenceError, hydra.errors.InferenceErrorOther(t))), other_inference_error(cx, input)))), (hydra.core.Name("unification"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.InferenceError, hydra.errors.InferenceErrorUnification(t))), unification_inference_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_inference_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def other_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_other_error_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_errors_other_error_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_errors_other_error_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_errors_other_error_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.errors.OtherError(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_errors_other_error_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_other_error_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def no_such_primitive_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_no_such_primitive_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.core.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.core.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.errors.NoSuchPrimitiveError(field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_no_such_primitive_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def other_resolution_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_other_resolution_error_1(v1):
        match v1:
            case hydra.core.LiteralString(value=s):
                return Right(s)

            case _:
                return Left(hydra.errors.DecodingError("expected string literal"))
    def _hoist_hydra_decode_errors_other_resolution_error_2(v1):
        match v1:
            case hydra.core.TermLiteral(value=v):
                return _hoist_hydra_decode_errors_other_resolution_error_1(v)

            case _:
                return Left(hydra.errors.DecodingError("expected literal"))
    def _hoist_hydra_decode_errors_other_resolution_error_3(cx, v1):
        match v1:
            case hydra.core.TermWrap(value=wrapped_term):
                return hydra.lib.eithers.map((lambda b: hydra.errors.OtherResolutionError(b)), hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped2: _hoist_hydra_decode_errors_other_resolution_error_2(stripped2)), hydra.extract.core.strip_with_decoding_error(cx, wrapped_term.body)))

            case _:
                return Left(hydra.errors.DecodingError("expected wrapped type"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_other_resolution_error_3(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def resolution_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_resolution_error_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.errors.ResolutionError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("noSuchBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoSuchBinding(t))), no_such_binding_error(cx, input)))), (hydra.core.Name("noSuchPrimitive"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoSuchPrimitive(t))), no_such_primitive_error(cx, input)))), (hydra.core.Name("noMatchingField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorNoMatchingField(t))), no_matching_field_error(cx, input)))), (hydra.core.Name("other"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorOther(t))), other_resolution_error(cx, input)))), (hydra.core.Name("unexpectedShape"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.ResolutionError, hydra.errors.ResolutionErrorUnexpectedShape(t))), unexpected_shape_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_resolution_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))

def error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_errors_error_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.errors.Error]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("checking"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorChecking(t))), hydra.decode.error.checking.checking_error(cx, input)))), (hydra.core.Name("decoding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorDecoding(t))), decoding_error(cx, input)))), (hydra.core.Name("duplicateBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorDuplicateBinding(t))), hydra.decode.error.core.duplicate_binding_error(cx, input)))), (hydra.core.Name("duplicateField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorDuplicateField(t))), hydra.decode.error.core.duplicate_field_error(cx, input)))), (hydra.core.Name("extraction"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorExtraction(t))), extraction_error(cx, input)))), (hydra.core.Name("inference"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorInference(t))), inference_error(cx, input)))), (hydra.core.Name("other"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorOther(t))), other_error(cx, input)))), (hydra.core.Name("resolution"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorResolution(t))), resolution_error(cx, input)))), (hydra.core.Name("undefinedField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorUndefinedField(t))), hydra.decode.error.core.undefined_field_error(cx, input)))), (hydra.core.Name("undefinedTermVariable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorUndefinedTermVariable(t))), hydra.decode.error.core.undefined_term_variable_error(cx, input)))), (hydra.core.Name("untypedTermVariable"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorUntypedTermVariable(t))), hydra.decode.error.core.untyped_term_variable_error(cx, input)))), (hydra.core.Name("unexpectedTermVariant"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorUnexpectedTermVariant(t))), hydra.decode.error.core.unexpected_term_variant_error(cx, input)))), (hydra.core.Name("unexpectedTypeVariant"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorUnexpectedTypeVariant(t))), hydra.decode.error.core.unexpected_type_variant_error(cx, input)))), (hydra.core.Name("unification"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.errors.Error, hydra.errors.ErrorUnification(t))), unification_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(err)), (lambda stripped: _hoist_hydra_decode_errors_error_1(cx, stripped)), hydra.extract.core.strip_with_decoding_error(cx, raw))
