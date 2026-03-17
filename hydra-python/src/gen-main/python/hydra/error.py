# Note: this is an automatically generated file. Do not edit.

r"""Error types specific to the Hydra kernel."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.typing
import hydra.variants

class CheckingErrorIncorrectUnification(Node["IncorrectUnificationError"]):
    r"""A post-unification consistency check failure"""

class CheckingErrorNotAForallType(Node["NotAForallTypeError"]):
    r"""A type that is not a forall type when one was expected"""

class CheckingErrorNotAFunctionType(Node["NotAFunctionTypeError"]):
    r"""A type that is not a function type when one was expected"""

class CheckingErrorTypeArityMismatch(Node["TypeArityMismatchError"]):
    r"""A type constructor applied to the wrong number of arguments"""

class CheckingErrorTypeMismatch(Node["TypeMismatchError"]):
    r"""A type mismatch between expected and actual types"""

class CheckingErrorUnboundTypeVariables(Node["UnboundTypeVariablesError"]):
    r"""Type variables that are not bound in scope"""

class CheckingErrorUnequalTypes(Node["UnequalTypesError"]):
    r"""Multiple types that should be equal but are not"""

class CheckingErrorUnsupportedTermVariant(Node["UnsupportedTermVariantError"]):
    r"""A term variant that the type checker does not support"""

class CheckingErrorUntypedLambda(Node["UntypedLambdaError"]):
    r"""A lambda expression without a type annotation on its parameter"""

class CheckingErrorUntypedLetBinding(Node["UntypedLetBindingError"]):
    r"""A let binding without a type annotation"""

class _CheckingErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error that occurred during type checking.
class CheckingError(metaclass=_CheckingErrorMeta):
    r"""CheckingErrorIncorrectUnification | CheckingErrorNotAForallType | CheckingErrorNotAFunctionType | CheckingErrorTypeArityMismatch | CheckingErrorTypeMismatch | CheckingErrorUnboundTypeVariables | CheckingErrorUnequalTypes | CheckingErrorUnsupportedTermVariant | CheckingErrorUntypedLambda | CheckingErrorUntypedLetBinding"""

    TYPE_ = hydra.core.Name("hydra.error.CheckingError")
    INCORRECT_UNIFICATION = hydra.core.Name("incorrectUnification")
    NOT_A_FORALL_TYPE = hydra.core.Name("notAForallType")
    NOT_A_FUNCTION_TYPE = hydra.core.Name("notAFunctionType")
    TYPE_ARITY_MISMATCH = hydra.core.Name("typeArityMismatch")
    TYPE_MISMATCH = hydra.core.Name("typeMismatch")
    UNBOUND_TYPE_VARIABLES = hydra.core.Name("unboundTypeVariables")
    UNEQUAL_TYPES = hydra.core.Name("unequalTypes")
    UNSUPPORTED_TERM_VARIANT = hydra.core.Name("unsupportedTermVariant")
    UNTYPED_LAMBDA = hydra.core.Name("untypedLambda")
    UNTYPED_LET_BINDING = hydra.core.Name("untypedLetBinding")

class DecodingError(Node[str]):
    r"""An error that occurred during decoding of a term."""

DecodingError.TYPE_ = hydra.core.Name("hydra.error.DecodingError")

@dataclass(frozen=True)
class DuplicateBindingError:
    r"""A duplicate binding name in a let expression."""

    name: Annotated[hydra.core.Name, "The duplicated binding name"]

    TYPE_ = hydra.core.Name("hydra.error.DuplicateBindingError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class DuplicateFieldError:
    r"""A duplicate field name in a record or union type."""

    name: Annotated[hydra.core.Name, "The duplicated field name"]

    TYPE_ = hydra.core.Name("hydra.error.DuplicateFieldError")
    NAME = hydra.core.Name("name")

class ErrorChecking(Node["CheckingError"]):
    r"""A type checking error"""

class ErrorDecoding(Node["DecodingError"]):
    r"""An error that occurred during decoding of a term"""

class ErrorDuplicateBinding(Node["DuplicateBindingError"]):
    r"""A duplicate binding name error"""

class ErrorDuplicateField(Node["DuplicateFieldError"]):
    r"""A duplicate field name error"""

class ErrorOther(Node["OtherError"]):
    r"""Any other error"""

class ErrorUndefinedField(Node["UndefinedFieldError"]):
    r"""A reference to an undefined field"""

class ErrorUndefinedTerm(Node["UndefinedTermError"]):
    r"""A reference to an undefined term"""

class ErrorUndefinedType(Node["UndefinedTypeError"]):
    r"""A reference to an undefined type"""

class ErrorUnexpectedTermVariant(Node["UnexpectedTermVariantError"]):
    r"""An unexpected term variant"""

class ErrorUnexpectedTypeVariant(Node["UnexpectedTypeVariantError"]):
    r"""An unexpected type variant"""

class ErrorUnification(Node["UnificationError"]):
    r"""A type unification error"""

class _ErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error of any kind, with kernel errors particularly differentiated.
class Error(metaclass=_ErrorMeta):
    r"""ErrorChecking | ErrorDecoding | ErrorDuplicateBinding | ErrorDuplicateField | ErrorOther | ErrorUndefinedField | ErrorUndefinedTerm | ErrorUndefinedType | ErrorUnexpectedTermVariant | ErrorUnexpectedTypeVariant | ErrorUnification"""

    TYPE_ = hydra.core.Name("hydra.error.Error")
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

@dataclass(frozen=True)
class IncorrectUnificationError:
    r"""A post-unification consistency check failure."""

    substitution: Annotated[hydra.typing.TypeSubst, "The substitution that failed the consistency check"]

    TYPE_ = hydra.core.Name("hydra.error.IncorrectUnificationError")
    SUBSTITUTION = hydra.core.Name("substitution")

@dataclass(frozen=True)
class NotAForallTypeError:
    r"""A type that is not a forall type when type arguments are being applied."""

    type: Annotated[hydra.core.Type, "The actual type encountered"]
    type_arguments: Annotated[frozenlist[hydra.core.Type], "The type arguments that were being applied"]

    TYPE_ = hydra.core.Name("hydra.error.NotAForallTypeError")
    TYPE = hydra.core.Name("type")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class NotAFunctionTypeError:
    r"""A type that is not a function type when one was expected in an application."""

    type: Annotated[hydra.core.Type, "The actual type encountered"]

    TYPE_ = hydra.core.Name("hydra.error.NotAFunctionTypeError")
    TYPE = hydra.core.Name("type")

class OtherError(Node[str]):
    r"""Any other error."""

OtherError.TYPE_ = hydra.core.Name("hydra.error.OtherError")

@dataclass(frozen=True)
class TypeArityMismatchError:
    r"""A type constructor applied to the wrong number of type arguments."""

    type: Annotated[hydra.core.Type, "The type being checked"]
    expected_arity: Annotated[int, "The expected number of type arguments"]
    actual_arity: Annotated[int, "The actual number of type arguments provided"]
    type_arguments: Annotated[frozenlist[hydra.core.Type], "The type arguments that were provided"]

    TYPE_ = hydra.core.Name("hydra.error.TypeArityMismatchError")
    TYPE = hydra.core.Name("type")
    EXPECTED_ARITY = hydra.core.Name("expectedArity")
    ACTUAL_ARITY = hydra.core.Name("actualArity")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class TypeMismatchError:
    r"""A type mismatch between expected and actual types."""

    expected_type: Annotated[hydra.core.Type, "The expected type"]
    actual_type: Annotated[hydra.core.Type, "The actual type encountered"]

    TYPE_ = hydra.core.Name("hydra.error.TypeMismatchError")
    EXPECTED_TYPE = hydra.core.Name("expectedType")
    ACTUAL_TYPE = hydra.core.Name("actualType")

@dataclass(frozen=True)
class UnboundTypeVariablesError:
    r"""Type variables that appear free in a type but are not bound in scope."""

    variables: Annotated[frozenset[hydra.core.Name], "The set of unbound type variable names"]
    type: Annotated[hydra.core.Type, "The type containing the unbound variables"]

    TYPE_ = hydra.core.Name("hydra.error.UnboundTypeVariablesError")
    VARIABLES = hydra.core.Name("variables")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class UndefinedFieldError:
    r"""A reference to a field that does not exist in the given type."""

    field_name: Annotated[hydra.core.Name, "The name of the undefined field"]
    type_name: Annotated[hydra.core.Name, "The name of the type in which the field was expected"]

    TYPE_ = hydra.core.Name("hydra.error.UndefinedFieldError")
    FIELD_NAME = hydra.core.Name("fieldName")
    TYPE_NAME = hydra.core.Name("typeName")

@dataclass(frozen=True)
class UndefinedTermError:
    r"""A reference to a term (element, binding, or primitive) that is not defined."""

    name: Annotated[hydra.core.Name, "The name of the undefined term"]

    TYPE_ = hydra.core.Name("hydra.error.UndefinedTermError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTypeError:
    r"""A reference to a type or type variable that is not defined."""

    name: Annotated[hydra.core.Name, "The name of the undefined type"]

    TYPE_ = hydra.core.Name("hydra.error.UndefinedTypeError")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UnequalTypesError:
    r"""Multiple types that should all be equal but are not."""

    types: Annotated[frozenlist[hydra.core.Type], "The list of types that are not all equal"]
    description: Annotated[str, "A description of the context in which the types were expected to be equal"]

    TYPE_ = hydra.core.Name("hydra.error.UnequalTypesError")
    TYPES = hydra.core.Name("types")
    DESCRIPTION = hydra.core.Name("description")

@dataclass(frozen=True)
class UnexpectedTermVariantError:
    r"""An unexpected term variant was encountered."""

    expected_variant: Annotated[hydra.variants.TermVariant, "The expected term variant"]
    actual_term: Annotated[hydra.core.Term, "The actual term that was encountered"]

    TYPE_ = hydra.core.Name("hydra.error.UnexpectedTermVariantError")
    EXPECTED_VARIANT = hydra.core.Name("expectedVariant")
    ACTUAL_TERM = hydra.core.Name("actualTerm")

@dataclass(frozen=True)
class UnexpectedTypeVariantError:
    r"""An unexpected type variant was encountered."""

    expected_variant: Annotated[hydra.variants.TypeVariant, "The expected type variant"]
    actual_type: Annotated[hydra.core.Type, "The actual type that was encountered"]

    TYPE_ = hydra.core.Name("hydra.error.UnexpectedTypeVariantError")
    EXPECTED_VARIANT = hydra.core.Name("expectedVariant")
    ACTUAL_TYPE = hydra.core.Name("actualType")

@dataclass(frozen=True)
class UnificationError:
    r"""An error that occurred during type unification."""

    left_type: Annotated[hydra.core.Type, "The left-hand type in the unification"]
    right_type: Annotated[hydra.core.Type, "The right-hand type in the unification"]
    message: Annotated[str, "A human-readable error message"]

    TYPE_ = hydra.core.Name("hydra.error.UnificationError")
    LEFT_TYPE = hydra.core.Name("leftType")
    RIGHT_TYPE = hydra.core.Name("rightType")
    MESSAGE = hydra.core.Name("message")

@dataclass(frozen=True)
class UnsupportedTermVariantError:
    r"""A term variant that the type checker does not support."""

    term_variant: Annotated[hydra.variants.TermVariant, "The unsupported term variant"]

    TYPE_ = hydra.core.Name("hydra.error.UnsupportedTermVariantError")
    TERM_VARIANT = hydra.core.Name("termVariant")

@dataclass(frozen=True)
class UntypedLambdaError:
    r"""A lambda expression without a type annotation on its parameter."""

    TYPE_ = hydra.core.Name("hydra.error.UntypedLambdaError")

@dataclass(frozen=True)
class UntypedLetBindingError:
    r"""A let binding without a type annotation."""

    binding: Annotated[hydra.core.Binding, "The untyped binding"]

    TYPE_ = hydra.core.Name("hydra.error.UntypedLetBindingError")
    BINDING = hydra.core.Name("binding")
