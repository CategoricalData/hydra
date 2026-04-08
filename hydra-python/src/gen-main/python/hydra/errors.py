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
import hydra.paths

class DecodingError(Node[str]):
    r"""An error that occurred during decoding of a term."""

DecodingError.TYPE_ = hydra.core.Name("hydra.errors.DecodingError")

# An empty list was encountered where a non-empty list was required.
EmptyListError: TypeAlias = "None"

class ErrorChecking(Node["hydra.error.checking.CheckingError"]):
    r"""A type checking error"""

class ErrorDecoding(Node["DecodingError"]):
    r"""An error that occurred during decoding of a term"""

class ErrorDuplicateBinding(Node["hydra.error.core.DuplicateBindingError"]):
    r"""A duplicate binding name error"""

class ErrorDuplicateField(Node["hydra.error.core.DuplicateFieldError"]):
    r"""A duplicate field name error"""

class ErrorExtraction(Node["ExtractionError"]):
    r"""An error that occurred while extracting a value from a term"""

class ErrorInference(Node["InferenceError"]):
    r"""A type inference error"""

class ErrorOther(Node["OtherError"]):
    r"""Any other error"""

class ErrorResolution(Node["ResolutionError"]):
    r"""A name-resolution error"""

class ErrorUndefinedField(Node["hydra.error.core.UndefinedFieldError"]):
    r"""A reference to an undefined field"""

class ErrorUndefinedTermVariable(Node["hydra.error.core.UndefinedTermVariableError"]):
    r"""A reference to an undefined term variable"""

class ErrorUntypedTermVariable(Node["hydra.error.core.UntypedTermVariableError"]):
    r"""A term variable whose type is not known"""

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
    r"""ErrorChecking | ErrorDecoding | ErrorDuplicateBinding | ErrorDuplicateField | ErrorExtraction | ErrorInference | ErrorOther | ErrorResolution | ErrorUndefinedField | ErrorUndefinedTermVariable | ErrorUntypedTermVariable | ErrorUnexpectedTermVariant | ErrorUnexpectedTypeVariant | ErrorUnification"""

    TYPE_ = hydra.core.Name("hydra.errors.Error")
    CHECKING = hydra.core.Name("checking")
    DECODING = hydra.core.Name("decoding")
    DUPLICATE_BINDING = hydra.core.Name("duplicateBinding")
    DUPLICATE_FIELD = hydra.core.Name("duplicateField")
    EXTRACTION = hydra.core.Name("extraction")
    INFERENCE = hydra.core.Name("inference")
    OTHER = hydra.core.Name("other")
    RESOLUTION = hydra.core.Name("resolution")
    UNDEFINED_FIELD = hydra.core.Name("undefinedField")
    UNDEFINED_TERM_VARIABLE = hydra.core.Name("undefinedTermVariable")
    UNTYPED_TERM_VARIABLE = hydra.core.Name("untypedTermVariable")
    UNEXPECTED_TERM_VARIANT = hydra.core.Name("unexpectedTermVariant")
    UNEXPECTED_TYPE_VARIANT = hydra.core.Name("unexpectedTypeVariant")
    UNIFICATION = hydra.core.Name("unification")

class ExtractionErrorEmptyList(Node["EmptyListError"]):
    r"""An empty list was encountered where a non-empty list was required"""

class ExtractionErrorMultipleBindings(Node["MultipleBindingsError"]):
    r"""Multiple let bindings were found with the same name"""

class ExtractionErrorMultipleFields(Node["MultipleFieldsError"]):
    r"""Multiple record fields were found with the same field name"""

class ExtractionErrorNoMatchingField(Node["NoMatchingFieldError"]):
    r"""No field with the expected name was found in a record"""

class ExtractionErrorNoSuchBinding(Node["NoSuchBindingError"]):
    r"""No let binding with the expected name was found"""

class ExtractionErrorNotEnoughCases(Node["NotEnoughCasesError"]):
    r"""A case statement did not contain enough cases to match the target"""

class ExtractionErrorUnexpectedShape(Node["UnexpectedShapeError"]):
    r"""A term, type, literal, or other value had an unexpected shape"""

class _ExtractionErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error that occurred while extracting a typed value from a term.
class ExtractionError(metaclass=_ExtractionErrorMeta):
    r"""ExtractionErrorEmptyList | ExtractionErrorMultipleBindings | ExtractionErrorMultipleFields | ExtractionErrorNoMatchingField | ExtractionErrorNoSuchBinding | ExtractionErrorNotEnoughCases | ExtractionErrorUnexpectedShape"""

    TYPE_ = hydra.core.Name("hydra.errors.ExtractionError")
    EMPTY_LIST = hydra.core.Name("emptyList")
    MULTIPLE_BINDINGS = hydra.core.Name("multipleBindings")
    MULTIPLE_FIELDS = hydra.core.Name("multipleFields")
    NO_MATCHING_FIELD = hydra.core.Name("noMatchingField")
    NO_SUCH_BINDING = hydra.core.Name("noSuchBinding")
    NOT_ENOUGH_CASES = hydra.core.Name("notEnoughCases")
    UNEXPECTED_SHAPE = hydra.core.Name("unexpectedShape")

class InferenceErrorChecking(Node["hydra.error.checking.CheckingError"]):
    r"""A type checking error encountered during inference"""

class InferenceErrorOther(Node["OtherInferenceError"]):
    r"""A generic inference error carrying a message and a subterm path. Placeholder arm; sites should migrate to typed variants."""

class InferenceErrorUnification(Node["UnificationInferenceError"]):
    r"""A unification failure encountered while inferring types"""

class _InferenceErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error that occurred during type inference.
class InferenceError(metaclass=_InferenceErrorMeta):
    r"""InferenceErrorChecking | InferenceErrorOther | InferenceErrorUnification"""

    TYPE_ = hydra.core.Name("hydra.errors.InferenceError")
    CHECKING = hydra.core.Name("checking")
    OTHER = hydra.core.Name("other")
    UNIFICATION = hydra.core.Name("unification")

@dataclass(frozen=True)
class MultipleBindingsError:
    r"""Multiple let bindings with the same name were found."""

    name: Annotated[hydra.core.Name, "The binding name which was duplicated"]

    TYPE_ = hydra.core.Name("hydra.errors.MultipleBindingsError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class MultipleFieldsError:
    r"""Multiple fields with the same name were found in a record."""

    field_name: Annotated[hydra.core.Name, "The field name which appeared more than once"]

    TYPE_ = hydra.core.Name("hydra.errors.MultipleFieldsError")
    FIELD_NAME = hydra.core.Name("fieldName")

@dataclass(frozen=True)
class NoMatchingFieldError:
    r"""No field with the expected name was present."""

    field_name: Annotated[hydra.core.Name, "The field name which was not found"]

    TYPE_ = hydra.core.Name("hydra.errors.NoMatchingFieldError")
    FIELD_NAME = hydra.core.Name("fieldName")

@dataclass(frozen=True)
class NoSuchBindingError:
    r"""No let binding with the expected name was present."""

    name: Annotated[hydra.core.Name, "The binding name which was not found"]

    TYPE_ = hydra.core.Name("hydra.errors.NoSuchBindingError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class NoSuchPrimitiveError:
    r"""No primitive function with the expected name was registered in the graph."""

    name: Annotated[hydra.core.Name, "The primitive name which was not found"]

    TYPE_ = hydra.core.Name("hydra.errors.NoSuchPrimitiveError")
    NAME = hydra.core.Name("name")

# A case statement was missing a case for the requested variant.
NotEnoughCasesError: TypeAlias = "None"

class OtherError(Node[str]):
    r"""Any other error."""

OtherError.TYPE_ = hydra.core.Name("hydra.errors.OtherError")

@dataclass(frozen=True)
class OtherInferenceError:
    r"""A generic inference error: message + subterm path."""

    path: Annotated[hydra.paths.SubtermPath, "The subterm path at which the error was observed"]
    message: Annotated[str, "A human-readable error message"]

    TYPE_ = hydra.core.Name("hydra.errors.OtherInferenceError")
    PATH = hydra.core.Name("path")
    MESSAGE = hydra.core.Name("message")

class OtherResolutionError(Node[str]):
    r"""A generic resolution error: message."""

OtherResolutionError.TYPE_ = hydra.core.Name("hydra.errors.OtherResolutionError")

class ResolutionErrorNoSuchBinding(Node["NoSuchBindingError"]):
    r"""No binding with the expected name was found in the graph"""

class ResolutionErrorNoSuchPrimitive(Node["NoSuchPrimitiveError"]):
    r"""No primitive function with the expected name was found in the graph"""

class ResolutionErrorNoMatchingField(Node["NoMatchingFieldError"]):
    r"""No field with the expected name was present in a record or case statement"""

class ResolutionErrorOther(Node["OtherResolutionError"]):
    r"""A generic resolution error carrying a message"""

class ResolutionErrorUnexpectedShape(Node["UnexpectedShapeError"]):
    r"""A term had a shape other than the one expected (e.g. a record, an injection)"""

class _ResolutionErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error that occurred while resolving a name, primitive, or record/union shape in a graph.
class ResolutionError(metaclass=_ResolutionErrorMeta):
    r"""ResolutionErrorNoSuchBinding | ResolutionErrorNoSuchPrimitive | ResolutionErrorNoMatchingField | ResolutionErrorOther | ResolutionErrorUnexpectedShape"""

    TYPE_ = hydra.core.Name("hydra.errors.ResolutionError")
    NO_SUCH_BINDING = hydra.core.Name("noSuchBinding")
    NO_SUCH_PRIMITIVE = hydra.core.Name("noSuchPrimitive")
    NO_MATCHING_FIELD = hydra.core.Name("noMatchingField")
    OTHER = hydra.core.Name("other")
    UNEXPECTED_SHAPE = hydra.core.Name("unexpectedShape")

@dataclass(frozen=True)
class UnexpectedShapeError:
    r"""A term, type, literal, or related value had a shape other than the one expected."""

    expected: Annotated[str, "A description of the expected shape"]
    actual: Annotated[str, "A description of the shape actually encountered"]

    TYPE_ = hydra.core.Name("hydra.errors.UnexpectedShapeError")
    EXPECTED = hydra.core.Name("expected")
    ACTUAL = hydra.core.Name("actual")

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

@dataclass(frozen=True)
class UnificationInferenceError:
    r"""A unification failure at a specific subterm locus during inference."""

    path: Annotated[hydra.paths.SubtermPath, "The subterm path at which the unification failure was observed"]
    cause: Annotated[UnificationError, "The underlying unification error"]

    TYPE_ = hydra.core.Name("hydra.errors.UnificationInferenceError")
    PATH = hydra.core.Name("path")
    CAUSE = hydra.core.Name("cause")
