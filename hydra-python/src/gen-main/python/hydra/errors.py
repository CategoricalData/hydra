# Note: this is an automatically generated file. Do not edit.

r"""Top-level error types for the Hydra kernel."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.error.checking
import hydra.error.core

class DecodingError(Node[str]):
    r"""An error that occurred during decoding of a term."""

DecodingError.TYPE_ = hydra.core.Name("hydra.errors.DecodingError")

class ErrorChecking(Node["hydra.error.checking.CheckingError"]):
    r"""A type checking error"""

class ErrorDecoding(Node["DecodingError"]):
    r"""An error that occurred during decoding of a term"""

class ErrorDuplicateBinding(Node["hydra.error.core.DuplicateBindingError"]):
    r"""A duplicate binding name error"""

class ErrorDuplicateField(Node["hydra.error.core.DuplicateFieldError"]):
    r"""A duplicate field name error"""

class ErrorOther(Node["OtherError"]):
    r"""Any other error"""

class ErrorUndefinedField(Node["hydra.error.core.UndefinedFieldError"]):
    r"""A reference to an undefined field"""

class ErrorUndefinedTerm(Node["hydra.error.core.UndefinedTermError"]):
    r"""A reference to an undefined term"""

class ErrorUndefinedType(Node["hydra.error.core.UndefinedTypeError"]):
    r"""A reference to an undefined type"""

class ErrorUnexpectedTermVariant(Node["hydra.error.core.UnexpectedTermVariantError"]):
    r"""An unexpected term variant"""

class ErrorUnexpectedTypeVariant(Node["hydra.error.core.UnexpectedTypeVariantError"]):
    r"""An unexpected type variant"""

class ErrorUnification(Node["UnificationError"]):
    r"""A type unification error"""

class _ErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error of any kind, with kernel errors particularly differentiated.
class Error(metaclass=_ErrorMeta):
    r"""ErrorChecking | ErrorDecoding | ErrorDuplicateBinding | ErrorDuplicateField | ErrorOther | ErrorUndefinedField | ErrorUndefinedTerm | ErrorUndefinedType | ErrorUnexpectedTermVariant | ErrorUnexpectedTypeVariant | ErrorUnification"""

    TYPE_ = hydra.core.Name("hydra.errors.Error")
    CHECKING = hydra.core.Name("checking")
    DECODING = hydra.core.Name("decoding")
    DUPLICATE_BINDING = hydra.core.Name("duplicateBinding")
    DUPLICATE_FIELD = hydra.core.Name("duplicateField")
    OTHER = hydra.core.Name("other")
    UNDEFINED_FIELD = hydra.core.Name("undefinedField")
    UNDEFINED_TERM = hydra.core.Name("undefinedTerm")
    UNDEFINED_TYPE = hydra.core.Name("undefinedType")
    UNEXPECTED_TERM_VARIANT = hydra.core.Name("unexpectedTermVariant")
    UNEXPECTED_TYPE_VARIANT = hydra.core.Name("unexpectedTypeVariant")
    UNIFICATION = hydra.core.Name("unification")

class OtherError(Node[str]):
    r"""Any other error."""

OtherError.TYPE_ = hydra.core.Name("hydra.errors.OtherError")

@dataclass(frozen=True)
class UnificationError:
    r"""An error that occurred during type unification."""

    left_type: Annotated[hydra.core.Type, "The left-hand type in the unification"]
    right_type: Annotated[hydra.core.Type, "The right-hand type in the unification"]
    message: Annotated[str, "A human-readable error message"]

    TYPE_ = hydra.core.Name("hydra.errors.UnificationError")
    LEFT_TYPE = hydra.core.Name("leftType")
    RIGHT_TYPE = hydra.core.Name("rightType")
    MESSAGE = hydra.core.Name("message")
