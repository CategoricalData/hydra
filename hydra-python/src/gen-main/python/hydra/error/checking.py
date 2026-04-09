# Note: this is an automatically generated file. Do not edit.

r"""Error types for type checking."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core
import hydra.paths
import hydra.typing
import hydra.variants

class CheckingErrorIncorrectUnification(Node["IncorrectUnificationError"]):
    r"""A post-unification consistency check failure"""

class CheckingErrorNotAForallType(Node["NotAForallTypeError"]):
    r"""A type that is not a forall type when one was expected"""

class CheckingErrorNotAFunctionType(Node["NotAFunctionTypeError"]):
    r"""A type that is not a function type when one was expected"""

class CheckingErrorOther(Node["OtherCheckingError"]):
    r"""A generic checking error"""

class CheckingErrorTypeArityMismatch(Node["TypeArityMismatchError"]):
    r"""A type constructor applied to the wrong number of arguments"""

class CheckingErrorTypeMismatch(Node["TypeMismatchError"]):
    r"""A type mismatch between expected and actual types"""

class CheckingErrorUnboundTypeVariables(Node["UnboundTypeVariablesError"]):
    r"""Type variables that are not bound in scope"""

class CheckingErrorUndefinedTermVariable(Node["UndefinedTermVariableCheckingError"]):
    r"""A reference to a term variable that is not bound in scope, encountered during checking"""

class CheckingErrorUnequalTypes(Node["UnequalTypesError"]):
    r"""Multiple types that should be equal but are not"""

class CheckingErrorUnsupportedTermVariant(Node["UnsupportedTermVariantError"]):
    r"""A term variant that the type checker does not support"""

class CheckingErrorUntypedLambda(Node["UntypedLambdaError"]):
    r"""A lambda expression without a type annotation on its parameter"""

class CheckingErrorUntypedLetBinding(Node["UntypedLetBindingError"]):
    r"""A let binding without a type annotation"""

class CheckingErrorUntypedTermVariable(Node["UntypedTermVariableCheckingError"]):
    r"""A reference to a term variable whose type is not known, encountered during checking"""

class _CheckingErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error that occurred during type checking.
class CheckingError(metaclass=_CheckingErrorMeta):
    r"""CheckingErrorIncorrectUnification | CheckingErrorNotAForallType | CheckingErrorNotAFunctionType | CheckingErrorOther | CheckingErrorTypeArityMismatch | CheckingErrorTypeMismatch | CheckingErrorUnboundTypeVariables | CheckingErrorUndefinedTermVariable | CheckingErrorUnequalTypes | CheckingErrorUnsupportedTermVariant | CheckingErrorUntypedLambda | CheckingErrorUntypedLetBinding | CheckingErrorUntypedTermVariable"""

    TYPE_ = hydra.core.Name("hydra.error.checking.CheckingError")
    INCORRECT_UNIFICATION = hydra.core.Name("incorrectUnification")
    NOT_A_FORALL_TYPE = hydra.core.Name("notAForallType")
    NOT_A_FUNCTION_TYPE = hydra.core.Name("notAFunctionType")
    OTHER = hydra.core.Name("other")
    TYPE_ARITY_MISMATCH = hydra.core.Name("typeArityMismatch")
    TYPE_MISMATCH = hydra.core.Name("typeMismatch")
    UNBOUND_TYPE_VARIABLES = hydra.core.Name("unboundTypeVariables")
    UNDEFINED_TERM_VARIABLE = hydra.core.Name("undefinedTermVariable")
    UNEQUAL_TYPES = hydra.core.Name("unequalTypes")
    UNSUPPORTED_TERM_VARIANT = hydra.core.Name("unsupportedTermVariant")
    UNTYPED_LAMBDA = hydra.core.Name("untypedLambda")
    UNTYPED_LET_BINDING = hydra.core.Name("untypedLetBinding")
    UNTYPED_TERM_VARIABLE = hydra.core.Name("untypedTermVariable")

@dataclass(frozen=True)
class IncorrectUnificationError:
    r"""A post-unification consistency check failure."""

    substitution: Annotated[hydra.typing.TypeSubst, "The substitution that failed the consistency check"]

    TYPE_ = hydra.core.Name("hydra.error.checking.IncorrectUnificationError")
    SUBSTITUTION = hydra.core.Name("substitution")

@dataclass(frozen=True)
class NotAForallTypeError:
    r"""A type that is not a forall type when type arguments are being applied."""

    type: Annotated[hydra.core.Type, "The actual type encountered"]
    type_arguments: Annotated[frozenlist[hydra.core.Type], "The type arguments that were being applied"]

    TYPE_ = hydra.core.Name("hydra.error.checking.NotAForallTypeError")
    TYPE = hydra.core.Name("type")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class NotAFunctionTypeError:
    r"""A type that is not a function type when one was expected in an application."""

    type: Annotated[hydra.core.Type, "The actual type encountered"]

    TYPE_ = hydra.core.Name("hydra.error.checking.NotAFunctionTypeError")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class OtherCheckingError:
    r"""A generic checking error: message + subterm path."""

    path: Annotated[hydra.paths.SubtermPath, "The subterm path at which the error was observed"]
    message: Annotated[str, "A human-readable error message"]

    TYPE_ = hydra.core.Name("hydra.error.checking.OtherCheckingError")
    PATH = hydra.core.Name("path")
    MESSAGE = hydra.core.Name("message")

@dataclass(frozen=True)
class TypeArityMismatchError:
    r"""A type constructor applied to the wrong number of type arguments."""

    type: Annotated[hydra.core.Type, "The type being checked"]
    expected_arity: Annotated[int, "The expected number of type arguments"]
    actual_arity: Annotated[int, "The actual number of type arguments provided"]
    type_arguments: Annotated[frozenlist[hydra.core.Type], "The type arguments that were provided"]

    TYPE_ = hydra.core.Name("hydra.error.checking.TypeArityMismatchError")
    TYPE = hydra.core.Name("type")
    EXPECTED_ARITY = hydra.core.Name("expectedArity")
    ACTUAL_ARITY = hydra.core.Name("actualArity")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class TypeMismatchError:
    r"""A type mismatch between expected and actual types."""

    expected_type: Annotated[hydra.core.Type, "The expected type"]
    actual_type: Annotated[hydra.core.Type, "The actual type encountered"]

    TYPE_ = hydra.core.Name("hydra.error.checking.TypeMismatchError")
    EXPECTED_TYPE = hydra.core.Name("expectedType")
    ACTUAL_TYPE = hydra.core.Name("actualType")

@dataclass(frozen=True)
class UnboundTypeVariablesError:
    r"""Type variables that appear free in a type but are not bound in scope."""

    variables: Annotated[frozenset[hydra.core.Name], "The set of unbound type variable names"]
    type: Annotated[hydra.core.Type, "The type containing the unbound variables"]

    TYPE_ = hydra.core.Name("hydra.error.checking.UnboundTypeVariablesError")
    VARIABLES = hydra.core.Name("variables")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class UndefinedTermVariableCheckingError:
    r"""A reference to a term variable that is not bound in scope, encountered during checking."""

    path: Annotated[hydra.paths.SubtermPath, "The subterm path at which the variable was referenced"]
    name: Annotated[hydra.core.Name, "The name of the undefined variable"]

    TYPE_ = hydra.core.Name("hydra.error.checking.UndefinedTermVariableCheckingError")
    PATH = hydra.core.Name("path")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UnequalTypesError:
    r"""Multiple types that should all be equal but are not."""

    types: Annotated[frozenlist[hydra.core.Type], "The list of types that are not all equal"]
    description: Annotated[str, "A description of the context in which the types were expected to be equal"]

    TYPE_ = hydra.core.Name("hydra.error.checking.UnequalTypesError")
    TYPES = hydra.core.Name("types")
    DESCRIPTION = hydra.core.Name("description")

@dataclass(frozen=True)
class UnsupportedTermVariantError:
    r"""A term variant that the type checker does not support."""

    term_variant: Annotated[hydra.variants.TermVariant, "The unsupported term variant"]

    TYPE_ = hydra.core.Name("hydra.error.checking.UnsupportedTermVariantError")
    TERM_VARIANT = hydra.core.Name("termVariant")

@dataclass(frozen=True)
class UntypedLambdaError:
    r"""A lambda expression without a type annotation on its parameter."""

    TYPE_ = hydra.core.Name("hydra.error.checking.UntypedLambdaError")

@dataclass(frozen=True)
class UntypedLetBindingError:
    r"""A let binding without a type annotation."""

    binding: Annotated[hydra.core.Binding, "The untyped binding"]

    TYPE_ = hydra.core.Name("hydra.error.checking.UntypedLetBindingError")
    BINDING = hydra.core.Name("binding")

@dataclass(frozen=True)
class UntypedTermVariableCheckingError:
    r"""A reference to a term variable whose type is not known, encountered during checking."""

    path: Annotated[hydra.paths.SubtermPath, "The subterm path at which the variable was referenced"]
    name: Annotated[hydra.core.Name, "The name of the untyped variable"]

    TYPE_ = hydra.core.Name("hydra.error.checking.UntypedTermVariableCheckingError")
    PATH = hydra.core.Name("path")
    NAME = hydra.core.Name("name")
