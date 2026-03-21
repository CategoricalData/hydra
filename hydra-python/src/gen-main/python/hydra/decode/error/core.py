# Note: this is an automatically generated file. Do not edit.

r"""Term decoders for hydra.error.core."""

from __future__ import annotations
from collections.abc import Callable
from functools import lru_cache
from hydra.dsl.python import Either, Left, Right
from typing import cast
import hydra.core
import hydra.decode.accessors
import hydra.decode.core
import hydra.decode.variants
import hydra.error.core
import hydra.errors
import hydra.extract.helpers
import hydra.lexical
import hydra.lib.eithers
import hydra.lib.maps
import hydra.lib.maybes
import hydra.lib.strings

def duplicate_binding_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_duplicate_binding_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("location", (lambda x1, x2: hydra.decode.accessors.accessor_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.DuplicateBindingError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_duplicate_binding_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def duplicate_field_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_duplicate_field_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("location", (lambda x1, x2: hydra.decode.accessors.accessor_path(x1, x2)), field_map(), cx), (lambda field_location: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.DuplicateFieldError(field_location, field_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_duplicate_field_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def invalid_term_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_invalid_term_error_1(cx, v1):
        match v1:
            case hydra.core.TermUnion(value=inj):
                field = inj.field
                fname = field.name
                fterm = field.term
                @lru_cache(1)
                def variant_map() -> FrozenDict[hydra.core.Name, Callable[[hydra.core.Term], Either[hydra.errors.DecodingError, hydra.error.core.InvalidTermError]]]:
                    return hydra.lib.maps.from_list(((hydra.core.Name("duplicateBinding"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateBinding(t))), duplicate_binding_error(cx, input)))), (hydra.core.Name("duplicateField"), (lambda input: hydra.lib.eithers.map((lambda t: cast(hydra.error.core.InvalidTermError, hydra.error.core.InvalidTermErrorDuplicateField(t))), duplicate_field_error(cx, input))))))
                return hydra.lib.maybes.maybe((lambda : Left(hydra.errors.DecodingError(hydra.lib.strings.cat(("no such field ", fname.value, " in union"))))), (lambda f: f(fterm)), hydra.lib.maps.lookup(fname, variant_map()))

            case _:
                return Left(hydra.errors.DecodingError("expected union"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_invalid_term_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def undefined_field_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_field_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("fieldName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_field_name: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("typeName", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_type_name: Right(hydra.error.core.UndefinedFieldError(field_field_name, field_type_name))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_undefined_field_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def undefined_term_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_term_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTermError(field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_undefined_term_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def undefined_type_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_undefined_type_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("name", (lambda x1, x2: hydra.decode.core.name(x1, x2)), field_map(), cx), (lambda field_name: Right(hydra.error.core.UndefinedTypeError(field_name))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_undefined_type_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def unexpected_term_variant_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_unexpected_term_variant_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expectedVariant", (lambda x1, x2: hydra.decode.variants.term_variant(x1, x2)), field_map(), cx), (lambda field_expected_variant: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("actualTerm", (lambda x1, x2: hydra.decode.core.term(x1, x2)), field_map(), cx), (lambda field_actual_term: Right(hydra.error.core.UnexpectedTermVariantError(field_expected_variant, field_actual_term))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_unexpected_term_variant_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))

def unexpected_type_variant_error(cx: hydra.graph.Graph, raw: hydra.core.Term):
    def _hoist_hydra_decode_error_core_unexpected_type_variant_error_1(cx, v1):
        match v1:
            case hydra.core.TermRecord(value=record):
                @lru_cache(1)
                def field_map() -> FrozenDict[hydra.core.Name, hydra.core.Term]:
                    return hydra.extract.helpers.to_field_map(record)
                return hydra.lib.eithers.bind(hydra.extract.helpers.require_field("expectedVariant", (lambda x1, x2: hydra.decode.variants.type_variant(x1, x2)), field_map(), cx), (lambda field_expected_variant: hydra.lib.eithers.bind(hydra.extract.helpers.require_field("actualType", (lambda x1, x2: hydra.decode.core.type(x1, x2)), field_map(), cx), (lambda field_actual_type: Right(hydra.error.core.UnexpectedTypeVariantError(field_expected_variant, field_actual_type))))))

            case _:
                return Left(hydra.errors.DecodingError("expected record"))
    return hydra.lib.eithers.either((lambda err: Left(hydra.errors.DecodingError(err))), (lambda stripped: _hoist_hydra_decode_error_core_unexpected_type_variant_error_1(cx, stripped)), hydra.lexical.strip_and_dereference_term_either(cx, raw))
