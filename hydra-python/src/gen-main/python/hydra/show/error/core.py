# Note: this is an automatically generated file. Do not edit.

r"""String representations of hydra.errors.core types."""

from __future__ import annotations
from functools import lru_cache
from typing import cast
import hydra.core
import hydra.error.core
import hydra.lib.strings
import hydra.show.core
import hydra.show.meta

def duplicate_binding_error(e: hydra.error.core.DuplicateBindingError) -> str:
    r"""Show a duplicate binding error as a string."""

    return hydra.lib.strings.cat(("duplicate binding: ", e.name.value))

def duplicate_field_error(e: hydra.error.core.DuplicateFieldError) -> str:
    r"""Show a duplicate field error as a string."""

    return hydra.lib.strings.cat(("duplicate field: ", e.name.value))

def invalid_term_error(e: hydra.error.core.InvalidTermError):
    def _hoist_hydra_show_error_core_invalid_term_error_1(v1):
        match v1:
            case hydra.error.core.InvalidTermErrorDuplicateBinding(value=v12):
                return duplicate_binding_error(v12)

            case hydra.error.core.InvalidTermErrorDuplicateField(value=v12):
                return duplicate_field_error(v12)

            case _:
                raise AssertionError("Unreachable: all variants handled")
    return hydra.lib.strings.cat2("invalid term: ", _hoist_hydra_show_error_core_invalid_term_error_1(e))

def undefined_field_error(e: hydra.error.core.UndefinedFieldError) -> str:
    r"""Show an undefined field error as a string."""

    fname = e.field_name
    tname = e.type_name
    return hydra.lib.strings.cat(("no such field \"", fname.value, "\" in type \"", tname.value, "\""))

def undefined_term_error(e: hydra.error.core.UndefinedTermError) -> str:
    r"""Show an undefined term error as a string."""

    return hydra.lib.strings.cat2("undefined term: ", e.name.value)

def undefined_type_error(e: hydra.error.core.UndefinedTypeError) -> str:
    r"""Show an undefined type error as a string."""

    return hydra.lib.strings.cat2("undefined type: ", e.name.value)

def unexpected_term_variant_error(e: hydra.error.core.UnexpectedTermVariantError) -> str:
    r"""Show an unexpected term variant error as a string."""

    expected = e.expected_variant
    actual = e.actual_term
    return hydra.lib.strings.cat(("expected ", hydra.show.meta.term_variant(expected), " term but found ", hydra.show.core.term(actual)))

def unexpected_type_variant_error(e: hydra.error.core.UnexpectedTypeVariantError) -> str:
    r"""Show an unexpected type variant error as a string."""

    expected = e.expected_variant
    actual = e.actual_type
    return hydra.lib.strings.cat(("expected ", hydra.show.meta.type_variant(expected), " type but found ", hydra.show.core.type(actual)))
