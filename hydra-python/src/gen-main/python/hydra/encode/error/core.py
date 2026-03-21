# Note: this is an automatically generated file. Do not edit.

r"""Term encoders for hydra.error.core."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.encode.accessors
import hydra.encode.core
import hydra.encode.variants
import hydra.error.core

def duplicate_binding_error(x: hydra.error.core.DuplicateBindingError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateBindingError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.accessors.accessor_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def duplicate_field_error(x: hydra.error.core.DuplicateFieldError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.DuplicateFieldError"), (hydra.core.Field(hydra.core.Name("location"), hydra.encode.accessors.accessor_path(x.location)), hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name))))))

def invalid_term_error(v1: hydra.error.core.InvalidTermError) -> hydra.core.Term:
    match v1:
        case hydra.error.core.InvalidTermErrorDuplicateBinding(value=y):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateBinding"), duplicate_binding_error(y)))))

        case hydra.error.core.InvalidTermErrorDuplicateField(value=y2):
            return cast(hydra.core.Term, hydra.core.TermUnion(hydra.core.Injection(hydra.core.Name("hydra.error.core.InvalidTermError"), hydra.core.Field(hydra.core.Name("duplicateField"), duplicate_field_error(y2)))))

        case _:
            raise AssertionError("Unreachable: all variants handled")

def undefined_field_error(x: hydra.error.core.UndefinedFieldError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedFieldError"), (hydra.core.Field(hydra.core.Name("fieldName"), hydra.encode.core.name(x.field_name)), hydra.core.Field(hydra.core.Name("typeName"), hydra.encode.core.name(x.type_name))))))

def undefined_term_error(x: hydra.error.core.UndefinedTermError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTermError"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)),))))

def undefined_type_error(x: hydra.error.core.UndefinedTypeError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UndefinedTypeError"), (hydra.core.Field(hydra.core.Name("name"), hydra.encode.core.name(x.name)),))))

def unexpected_term_variant_error(x: hydra.error.core.UnexpectedTermVariantError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTermVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), hydra.encode.variants.term_variant(x.expected_variant)), hydra.core.Field(hydra.core.Name("actualTerm"), hydra.encode.core.term(x.actual_term))))))

def unexpected_type_variant_error(x: hydra.error.core.UnexpectedTypeVariantError) -> hydra.core.Term:
    return cast(hydra.core.Term, hydra.core.TermRecord(hydra.core.Record(hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError"), (hydra.core.Field(hydra.core.Name("expectedVariant"), hydra.encode.variants.type_variant(x.expected_variant)), hydra.core.Field(hydra.core.Name("actualType"), hydra.encode.core.type(x.actual_type))))))
