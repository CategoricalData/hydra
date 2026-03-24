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

@dataclass(frozen=True)
class UndefinedFieldError:
    r"""A reference to a field that does not exist in the given type."""

    field_name: Annotated[hydra.core.Name, "The name of the undefined field"]
    type_name: Annotated[hydra.core.Name, "The name of the type in which the field was expected"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedFieldError")
    FIELD_NAME = hydra.core.Name("fieldName")
    TYPE_NAME = hydra.core.Name("typeName")

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

@dataclass(frozen=True)
class ConstantConditionError:
    r"""An application of ifElse where the condition is a literal boolean, creating a dead branch (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the constant condition within the term"]
    value: Annotated[bool, "The constant boolean value of the condition"]

    TYPE_ = hydra.core.Name("hydra.error.core.ConstantConditionError")
    LOCATION = hydra.core.Name("location")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class EmptyCaseStatementError:
    r"""A case statement with no cases and no default (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the empty case statement within the term"]
    type_name: Annotated[hydra.core.Name, "The name of the union type being matched"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyCaseStatementError")
    LOCATION = hydra.core.Name("location")
    TYPE_NAME = hydra.core.Name("typeName")

@dataclass(frozen=True)
class EmptyLetBindingsError:
    r"""A let expression with an empty list of bindings (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the empty let expression within the term"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyLetBindingsError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class EmptyTermAnnotationError:
    r"""A term annotation with an empty annotation map (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the empty annotation within the term"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyTermAnnotationError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class EmptyTypeNameInTermError:
    r"""A record, injection, projection, or case statement with an empty type name (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the term with the empty type name"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyTypeNameInTermError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class InvalidLambdaParameterNameError:
    r"""A lambda parameter name that violates naming conventions (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the lambda within the term"]
    name: Annotated[hydra.core.Name, "The invalid parameter name"]

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidLambdaParameterNameError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class InvalidLetBindingNameError:
    r"""A let binding name that violates naming conventions (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the binding within the term"]
    name: Annotated[hydra.core.Name, "The invalid binding name"]

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidLetBindingNameError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class InvalidTypeLambdaParameterNameError:
    r"""A type lambda parameter name that violates naming conventions (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the type lambda within the term"]
    name: Annotated[hydra.core.Name, "The invalid type lambda parameter name"]

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidTypeLambdaParameterNameError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class NestedTermAnnotationError:
    r"""A term annotation directly wrapping another term annotation; annotations should be merged (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the outer annotation within the term"]

    TYPE_ = hydra.core.Name("hydra.error.core.NestedTermAnnotationError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class RedundantWrapUnwrapError:
    r"""An unwrap elimination applied to a wrap term of the same type, forming a no-op round-trip (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the redundant wrap/unwrap within the term"]
    type_name: Annotated[hydra.core.Name, "The type name of the wrapper"]

    TYPE_ = hydra.core.Name("hydra.error.core.RedundantWrapUnwrapError")
    LOCATION = hydra.core.Name("location")
    TYPE_NAME = hydra.core.Name("typeName")

@dataclass(frozen=True)
class SelfApplicationError:
    r"""A variable applied to itself, which is almost always a mistake in Hydra's type system (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the self-application within the term"]
    name: Annotated[hydra.core.Name, "The name of the variable applied to itself"]

    TYPE_ = hydra.core.Name("hydra.error.core.SelfApplicationError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class TermVariableShadowingError:
    r"""A lambda parameter or let binding name that shadows a variable already in scope (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the shadowing binding within the term"]
    name: Annotated[hydra.core.Name, "The name of the shadowed variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.TermVariableShadowingError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class TypeVariableShadowingInTypeLambdaError:
    r"""A type lambda parameter that shadows a type variable already in scope (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the type lambda within the term"]
    name: Annotated[hydra.core.Name, "The name of the shadowed type variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.TypeVariableShadowingInTypeLambdaError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTermVariableError:
    r"""A variable reference to a term name that is not bound in scope."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the undefined variable within the term"]
    name: Annotated[hydra.core.Name, "The name of the undefined variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTermVariableError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTypeVariableInBindingTypeError:
    r"""A type variable in a let binding's type scheme that is not bound by the scheme or enclosing scope."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the binding within the term"]
    name: Annotated[hydra.core.Name, "The name of the undefined type variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTypeVariableInBindingTypeError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTypeVariableInLambdaDomainError:
    r"""A type variable in a lambda domain annotation that is not bound in scope."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the lambda within the term"]
    name: Annotated[hydra.core.Name, "The name of the undefined type variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTypeVariableInLambdaDomainError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTypeVariableInTypeApplicationError:
    r"""A type variable in a type application term that is not bound in scope."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the type application within the term"]
    name: Annotated[hydra.core.Name, "The name of the undefined type variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTypeVariableInTypeApplicationError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UnknownPrimitiveNameError:
    r"""A primitive function reference to a name not in the known primitive registry."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the primitive reference within the term"]
    name: Annotated[hydra.core.Name, "The unknown primitive name"]

    TYPE_ = hydra.core.Name("hydra.error.core.UnknownPrimitiveNameError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UnnecessaryIdentityApplicationError:
    r"""An application of an identity lambda to an argument, which simplifies to the argument (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the identity application within the term"]

    TYPE_ = hydra.core.Name("hydra.error.core.UnnecessaryIdentityApplicationError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class UntypedTermVariableError:
    r"""A term variable whose type is not known in the current scope."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the untyped variable within the term"]
    name: Annotated[hydra.core.Name, "The name of the untyped variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.UntypedTermVariableError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

class InvalidTermErrorConstantCondition(Node["ConstantConditionError"]):
    r"""An ifElse with a literal boolean condition (optional)"""

class InvalidTermErrorDuplicateBinding(Node["DuplicateBindingError"]):
    r"""A duplicate binding name in a let expression"""

class InvalidTermErrorDuplicateField(Node["DuplicateFieldError"]):
    r"""A duplicate field name in a record or case statement"""

class InvalidTermErrorEmptyCaseStatement(Node["EmptyCaseStatementError"]):
    r"""A case statement with no cases and no default (optional)"""

class InvalidTermErrorEmptyLetBindings(Node["EmptyLetBindingsError"]):
    r"""A let expression with no bindings (optional)"""

class InvalidTermErrorEmptyTermAnnotation(Node["EmptyTermAnnotationError"]):
    r"""A term annotation with an empty annotation map (optional)"""

class InvalidTermErrorEmptyTypeNameInTerm(Node["EmptyTypeNameInTermError"]):
    r"""A term with an empty type name (optional)"""

class InvalidTermErrorInvalidLambdaParameterName(Node["InvalidLambdaParameterNameError"]):
    r"""A lambda parameter name violating naming conventions (optional)"""

class InvalidTermErrorInvalidLetBindingName(Node["InvalidLetBindingNameError"]):
    r"""A let binding name violating naming conventions (optional)"""

class InvalidTermErrorInvalidTypeLambdaParameterName(Node["InvalidTypeLambdaParameterNameError"]):
    r"""A type lambda parameter name violating naming conventions (optional)"""

class InvalidTermErrorNestedTermAnnotation(Node["NestedTermAnnotationError"]):
    r"""Nested term annotations that should be merged (optional)"""

class InvalidTermErrorRedundantWrapUnwrap(Node["RedundantWrapUnwrapError"]):
    r"""A no-op unwrap-of-wrap round-trip (optional)"""

class InvalidTermErrorSelfApplication(Node["SelfApplicationError"]):
    r"""A variable applied to itself (optional)"""

class InvalidTermErrorTermVariableShadowing(Node["TermVariableShadowingError"]):
    r"""A binding that shadows a variable already in scope (optional)"""

class InvalidTermErrorTypeVariableShadowingInTypeLambda(Node["TypeVariableShadowingInTypeLambdaError"]):
    r"""A type lambda parameter that shadows a type variable in scope (optional)"""

class InvalidTermErrorUndefinedTermVariable(Node["UndefinedTermVariableError"]):
    r"""A variable reference to an unbound term name"""

class InvalidTermErrorUndefinedTypeVariableInBindingType(Node["UndefinedTypeVariableInBindingTypeError"]):
    r"""An unbound type variable in a let binding's type scheme"""

class InvalidTermErrorUndefinedTypeVariableInLambdaDomain(Node["UndefinedTypeVariableInLambdaDomainError"]):
    r"""An unbound type variable in a lambda domain annotation"""

class InvalidTermErrorUndefinedTypeVariableInTypeApplication(Node["UndefinedTypeVariableInTypeApplicationError"]):
    r"""An unbound type variable in a type application term"""

class InvalidTermErrorUnknownPrimitiveName(Node["UnknownPrimitiveNameError"]):
    r"""A reference to an unknown primitive function"""

class InvalidTermErrorUnnecessaryIdentityApplication(Node["UnnecessaryIdentityApplicationError"]):
    r"""An identity lambda applied to an argument (optional)"""

class InvalidTermErrorUntypedTermVariable(Node["UntypedTermVariableError"]):
    r"""A term variable whose type is not known"""

class _InvalidTermErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a term is invalid.
class InvalidTermError(metaclass=_InvalidTermErrorMeta):
    r"""InvalidTermErrorConstantCondition | InvalidTermErrorDuplicateBinding | InvalidTermErrorDuplicateField | InvalidTermErrorEmptyCaseStatement | InvalidTermErrorEmptyLetBindings | InvalidTermErrorEmptyTermAnnotation | InvalidTermErrorEmptyTypeNameInTerm | InvalidTermErrorInvalidLambdaParameterName | InvalidTermErrorInvalidLetBindingName | InvalidTermErrorInvalidTypeLambdaParameterName | InvalidTermErrorNestedTermAnnotation | InvalidTermErrorRedundantWrapUnwrap | InvalidTermErrorSelfApplication | InvalidTermErrorTermVariableShadowing | InvalidTermErrorTypeVariableShadowingInTypeLambda | InvalidTermErrorUndefinedTermVariable | InvalidTermErrorUndefinedTypeVariableInBindingType | InvalidTermErrorUndefinedTypeVariableInLambdaDomain | InvalidTermErrorUndefinedTypeVariableInTypeApplication | InvalidTermErrorUnknownPrimitiveName | InvalidTermErrorUnnecessaryIdentityApplication | InvalidTermErrorUntypedTermVariable"""

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidTermError")
    CONSTANT_CONDITION = hydra.core.Name("constantCondition")
    DUPLICATE_BINDING = hydra.core.Name("duplicateBinding")
    DUPLICATE_FIELD = hydra.core.Name("duplicateField")
    EMPTY_CASE_STATEMENT = hydra.core.Name("emptyCaseStatement")
    EMPTY_LET_BINDINGS = hydra.core.Name("emptyLetBindings")
    EMPTY_TERM_ANNOTATION = hydra.core.Name("emptyTermAnnotation")
    EMPTY_TYPE_NAME_IN_TERM = hydra.core.Name("emptyTypeNameInTerm")
    INVALID_LAMBDA_PARAMETER_NAME = hydra.core.Name("invalidLambdaParameterName")
    INVALID_LET_BINDING_NAME = hydra.core.Name("invalidLetBindingName")
    INVALID_TYPE_LAMBDA_PARAMETER_NAME = hydra.core.Name("invalidTypeLambdaParameterName")
    NESTED_TERM_ANNOTATION = hydra.core.Name("nestedTermAnnotation")
    REDUNDANT_WRAP_UNWRAP = hydra.core.Name("redundantWrapUnwrap")
    SELF_APPLICATION = hydra.core.Name("selfApplication")
    TERM_VARIABLE_SHADOWING = hydra.core.Name("termVariableShadowing")
    TYPE_VARIABLE_SHADOWING_IN_TYPE_LAMBDA = hydra.core.Name("typeVariableShadowingInTypeLambda")
    UNDEFINED_TERM_VARIABLE = hydra.core.Name("undefinedTermVariable")
    UNDEFINED_TYPE_VARIABLE_IN_BINDING_TYPE = hydra.core.Name("undefinedTypeVariableInBindingType")
    UNDEFINED_TYPE_VARIABLE_IN_LAMBDA_DOMAIN = hydra.core.Name("undefinedTypeVariableInLambdaDomain")
    UNDEFINED_TYPE_VARIABLE_IN_TYPE_APPLICATION = hydra.core.Name("undefinedTypeVariableInTypeApplication")
    UNKNOWN_PRIMITIVE_NAME = hydra.core.Name("unknownPrimitiveName")
    UNNECESSARY_IDENTITY_APPLICATION = hydra.core.Name("unnecessaryIdentityApplication")
    UNTYPED_TERM_VARIABLE = hydra.core.Name("untypedTermVariable")

@dataclass(frozen=True)
class DuplicateRecordTypeFieldNamesError:
    r"""A record type with duplicate field names."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the record type with duplicate fields"]
    name: Annotated[hydra.core.Name, "The duplicated field name"]

    TYPE_ = hydra.core.Name("hydra.error.core.DuplicateRecordTypeFieldNamesError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class DuplicateUnionTypeFieldNamesError:
    r"""A union type with duplicate field names."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the union type with duplicate fields"]
    name: Annotated[hydra.core.Name, "The duplicated field name"]

    TYPE_ = hydra.core.Name("hydra.error.core.DuplicateUnionTypeFieldNamesError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class EmptyRecordTypeError:
    r"""A record type with no fields; TypeUnit is preferred for the unit-like case (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the empty record type"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyRecordTypeError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class EmptyTypeAnnotationError:
    r"""A type annotation with an empty annotation map (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the empty annotation"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyTypeAnnotationError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class EmptyUnionTypeError:
    r"""A union type with no alternatives; TypeVoid is preferred (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the empty union type"]

    TYPE_ = hydra.core.Name("hydra.error.core.EmptyUnionTypeError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class InvalidForallParameterNameError:
    r"""A forall type parameter name that violates type variable naming conventions (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the forall type"]
    name: Annotated[hydra.core.Name, "The invalid parameter name"]

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidForallParameterNameError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class InvalidTypeSchemeVariableNameError:
    r"""A type scheme variable name that violates type variable naming conventions (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the type scheme"]
    name: Annotated[hydra.core.Name, "The invalid variable name"]

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidTypeSchemeVariableNameError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class NestedTypeAnnotationError:
    r"""A type annotation directly wrapping another type annotation; annotations should be merged (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the outer annotation"]

    TYPE_ = hydra.core.Name("hydra.error.core.NestedTypeAnnotationError")
    LOCATION = hydra.core.Name("location")

@dataclass(frozen=True)
class NonComparableMapKeyTypeError:
    r"""A map type whose key type is or directly contains a function type, which cannot be compared for equality."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the map type"]
    key_type: Annotated[hydra.core.Type, "The non-comparable key type"]

    TYPE_ = hydra.core.Name("hydra.error.core.NonComparableMapKeyTypeError")
    LOCATION = hydra.core.Name("location")
    KEY_TYPE = hydra.core.Name("keyType")

@dataclass(frozen=True)
class NonComparableSetElementTypeError:
    r"""A set type whose element type is or directly contains a function type, which cannot be compared for equality."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the set type"]
    element_type: Annotated[hydra.core.Type, "The non-comparable element type"]

    TYPE_ = hydra.core.Name("hydra.error.core.NonComparableSetElementTypeError")
    LOCATION = hydra.core.Name("location")
    ELEMENT_TYPE = hydra.core.Name("elementType")

@dataclass(frozen=True)
class SingleVariantUnionError:
    r"""A union type with exactly one field; could be a wrapped type or record instead (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the single-variant union type"]
    field_name: Annotated[hydra.core.Name, "The name of the single field"]

    TYPE_ = hydra.core.Name("hydra.error.core.SingleVariantUnionError")
    LOCATION = hydra.core.Name("location")
    FIELD_NAME = hydra.core.Name("fieldName")

@dataclass(frozen=True)
class TypeVariableShadowingInForallError:
    r"""A forall type parameter that shadows a type variable already in scope (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the shadowing forall type"]
    name: Annotated[hydra.core.Name, "The name of the shadowed type variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.TypeVariableShadowingInForallError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class UndefinedTypeVariableError:
    r"""A type variable reference to a name that is not bound in scope."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the undefined type variable"]
    name: Annotated[hydra.core.Name, "The name of the undefined type variable"]

    TYPE_ = hydra.core.Name("hydra.error.core.UndefinedTypeVariableError")
    LOCATION = hydra.core.Name("location")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class VoidInNonBottomPositionError:
    r"""TypeVoid appearing in a position where no value can be constructed, such as a record field, list element, map key/value, set element, pair component, or function codomain (optional)."""

    location: Annotated[hydra.accessors.AccessorPath, "The path to the void type in a non-bottom position"]

    TYPE_ = hydra.core.Name("hydra.error.core.VoidInNonBottomPositionError")
    LOCATION = hydra.core.Name("location")

class InvalidTypeErrorDuplicateRecordTypeFieldNames(Node["DuplicateRecordTypeFieldNamesError"]):
    r"""A record type with duplicate field names"""

class InvalidTypeErrorDuplicateUnionTypeFieldNames(Node["DuplicateUnionTypeFieldNamesError"]):
    r"""A union type with duplicate field names"""

class InvalidTypeErrorEmptyRecordType(Node["EmptyRecordTypeError"]):
    r"""A record type with no fields (optional)"""

class InvalidTypeErrorEmptyTypeAnnotation(Node["EmptyTypeAnnotationError"]):
    r"""A type annotation with an empty annotation map (optional)"""

class InvalidTypeErrorEmptyUnionType(Node["EmptyUnionTypeError"]):
    r"""A union type with no alternatives (optional)"""

class InvalidTypeErrorInvalidForallParameterName(Node["InvalidForallParameterNameError"]):
    r"""A forall parameter name violating naming conventions (optional)"""

class InvalidTypeErrorInvalidTypeSchemeVariableName(Node["InvalidTypeSchemeVariableNameError"]):
    r"""A type scheme variable name violating naming conventions (optional)"""

class InvalidTypeErrorNestedTypeAnnotation(Node["NestedTypeAnnotationError"]):
    r"""Nested type annotations that should be merged (optional)"""

class InvalidTypeErrorNonComparableMapKeyType(Node["NonComparableMapKeyTypeError"]):
    r"""A map with a non-comparable key type"""

class InvalidTypeErrorNonComparableSetElementType(Node["NonComparableSetElementTypeError"]):
    r"""A set with a non-comparable element type"""

class InvalidTypeErrorSingleVariantUnion(Node["SingleVariantUnionError"]):
    r"""A union type with only one variant (optional)"""

class InvalidTypeErrorTypeVariableShadowingInForall(Node["TypeVariableShadowingInForallError"]):
    r"""A forall parameter that shadows a type variable in scope (optional)"""

class InvalidTypeErrorUndefinedTypeVariable(Node["UndefinedTypeVariableError"]):
    r"""A type variable reference to an unbound name"""

class InvalidTypeErrorVoidInNonBottomPosition(Node["VoidInNonBottomPositionError"]):
    r"""TypeVoid in a position where no value can be constructed (optional)"""

class _InvalidTypeErrorMeta(type):
    def __getitem__(cls, item):
        return object

# An error indicating that a type is invalid.
class InvalidTypeError(metaclass=_InvalidTypeErrorMeta):
    r"""InvalidTypeErrorDuplicateRecordTypeFieldNames | InvalidTypeErrorDuplicateUnionTypeFieldNames | InvalidTypeErrorEmptyRecordType | InvalidTypeErrorEmptyTypeAnnotation | InvalidTypeErrorEmptyUnionType | InvalidTypeErrorInvalidForallParameterName | InvalidTypeErrorInvalidTypeSchemeVariableName | InvalidTypeErrorNestedTypeAnnotation | InvalidTypeErrorNonComparableMapKeyType | InvalidTypeErrorNonComparableSetElementType | InvalidTypeErrorSingleVariantUnion | InvalidTypeErrorTypeVariableShadowingInForall | InvalidTypeErrorUndefinedTypeVariable | InvalidTypeErrorVoidInNonBottomPosition"""

    TYPE_ = hydra.core.Name("hydra.error.core.InvalidTypeError")
    DUPLICATE_RECORD_TYPE_FIELD_NAMES = hydra.core.Name("duplicateRecordTypeFieldNames")
    DUPLICATE_UNION_TYPE_FIELD_NAMES = hydra.core.Name("duplicateUnionTypeFieldNames")
    EMPTY_RECORD_TYPE = hydra.core.Name("emptyRecordType")
    EMPTY_TYPE_ANNOTATION = hydra.core.Name("emptyTypeAnnotation")
    EMPTY_UNION_TYPE = hydra.core.Name("emptyUnionType")
    INVALID_FORALL_PARAMETER_NAME = hydra.core.Name("invalidForallParameterName")
    INVALID_TYPE_SCHEME_VARIABLE_NAME = hydra.core.Name("invalidTypeSchemeVariableName")
    NESTED_TYPE_ANNOTATION = hydra.core.Name("nestedTypeAnnotation")
    NON_COMPARABLE_MAP_KEY_TYPE = hydra.core.Name("nonComparableMapKeyType")
    NON_COMPARABLE_SET_ELEMENT_TYPE = hydra.core.Name("nonComparableSetElementType")
    SINGLE_VARIANT_UNION = hydra.core.Name("singleVariantUnion")
    TYPE_VARIABLE_SHADOWING_IN_FORALL = hydra.core.Name("typeVariableShadowingInForall")
    UNDEFINED_TYPE_VARIABLE = hydra.core.Name("undefinedTypeVariable")
    VOID_IN_NON_BOTTOM_POSITION = hydra.core.Name("voidInNonBottomPosition")
