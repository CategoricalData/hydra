# Note: this is an automatically generated file. Do not edit.

r"""Error types for core type and term validation."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node
from typing import Annotated, TypeAlias, cast
import hydra.accessors
import hydra.core
import hydra.variants

@dataclass(frozen=True)
class DuplicateBindingError:
    r"""A duplicate binding name in a let expression."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the duplicate binding within the term"]
    name: Annotated[hydra.core.Name, "The duplicated binding name"]

    TYPE_ = hydra.core.Name("hydra.error.core.DuplicateBindingError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class DuplicateFieldError:
    r"""A duplicate field name in a record or union type."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the duplicate field within the term"]
    name: Annotated[hydra.core.Name, "The duplicated field name"]

    TYPE_ = hydra.core.Name("hydra.error.core.DuplicateFieldError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

class InvalidTermErrorDuplicateBinding(Node["DuplicateBindingError"]):
    r"""A duplicate binding name in a let expression"""

class InvalidTermErrorDuplicateField(Node["DuplicateFieldError"]):
    r"""A duplicate field name in a record or union type"""

class _InvalidTermErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a term is invalid.
class InvalidTermError(metaclass=_InvalidTermErrorMeta):
    r"""InvalidTermErrorDuplicateBinding | InvalidTermErrorDuplicateField"""

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidTermError")
    DUPLICATE_BINDING = hydra.core.Name("duplicateBinding")
    DUPLICATE_FIELD = hydra.core.Name("duplicateField")

@dataclass(frozen=True)
class UndefinedFieldError:
    r"""A reference to a field that does not exist in the given type."""

    field_name: Annotated[hydra.core.Name, "The name of the undefined field"]
    type_name: Annotated[hydra.core.Name, "The name of the type in which the field was expected"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedFieldError")
    FIELD_NAME = hydra.core.Name("fieldName")
    TYPE_NAME = hydra.core.Name("typeName")

@dataclass(frozen=True)
class UndefinedTermError:
    r"""A reference to a term (element, binding, or primitive) that is not defined."""

    name: Annotated[hydra.core.Name, "The name of the undefined term"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTermError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTypeError:
    r"""A reference to a type or type variable that is not defined."""

    name: Annotated[hydra.core.Name, "The name of the undefined type"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTypeError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UnexpectedTermVariantError:
    r"""An unexpected term variant was encountered."""

    expected_variant: Annotated[hydra.variants.TermVariant, "The expected term variant"]
    actual_term: Annotated[hydra.core.Term, "The actual term that was encountered"]

    TYPE_ = hydra.core.Name("hydra.error.core.UnexpectedTermVariantError")
    EXPECTED_VARIANT = hydra.core.Name("expectedVariant")
    ACTUAL_TERM = hydra.core.Name("actualTerm")

@dataclass(frozen=True)
class UnexpectedTypeVariantError:
    r"""An unexpected type variant was encountered."""

    expected_variant: Annotated[hydra.variants.TypeVariant, "The expected type variant"]
    actual_type: Annotated[hydra.core.Type, "The actual type that was encountered"]

    TYPE_ = hydra.core.Name("hydra.error.core.UnexpectedTypeVariantError")
    EXPECTED_VARIANT = hydra.core.Name("expectedVariant")
    ACTUAL_TYPE = hydra.core.Name("actualType")
