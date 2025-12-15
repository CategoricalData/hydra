# Note: this is an automatically generated file. Do not edit.

r"""Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from hydra.dsl.python import Either, FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, TypeAlias

class Name(Node[str]):
    r"""A unique identifier in some context; a string-valued key."""

NAME__NAME = Name("hydra.core.Name")

@dataclass(frozen=True)
class AnnotatedTerm:
    r"""A term together with an annotation."""
    
    body: Annotated[Term, "The term being annotated"]
    annotation: Annotated[FrozenDict[Name, Term], "The annotation as a map from keys to values"]

ANNOTATED_TERM__NAME = Name("hydra.core.AnnotatedTerm")
ANNOTATED_TERM__BODY__NAME = Name("body")
ANNOTATED_TERM__ANNOTATION__NAME = Name("annotation")

@dataclass(frozen=True)
class AnnotatedType:
    r"""A type together with an annotation."""
    
    body: Annotated[Type, "The type being annotated"]
    annotation: Annotated[FrozenDict[Name, Term], "The annotation as a map from keys to values"]

ANNOTATED_TYPE__NAME = Name("hydra.core.AnnotatedType")
ANNOTATED_TYPE__BODY__NAME = Name("body")
ANNOTATED_TYPE__ANNOTATION__NAME = Name("annotation")

@dataclass(frozen=True)
class Application:
    r"""A term which applies a function to an argument."""
    
    function: Annotated[Term, "The left-hand side of the application"]
    argument: Annotated[Term, "The right-hand side of the application"]

APPLICATION__NAME = Name("hydra.core.Application")
APPLICATION__FUNCTION__NAME = Name("function")
APPLICATION__ARGUMENT__NAME = Name("argument")

@dataclass(frozen=True)
class ApplicationType:
    r"""The type-level analog of an application term."""
    
    function: Annotated[Type, "The left-hand side of the application"]
    argument: Annotated[Type, "The right-hand side of the application"]

APPLICATION_TYPE__NAME = Name("hydra.core.ApplicationType")
APPLICATION_TYPE__FUNCTION__NAME = Name("function")
APPLICATION_TYPE__ARGUMENT__NAME = Name("argument")

@dataclass(frozen=True)
class Binding:
    r"""A field with an optional type scheme, used to bind variables to terms in a 'let' expression."""
    
    name: Annotated[Name, "The name of the bound variable"]
    term: Annotated[Term, "The term to which the variable is bound"]
    type: Annotated[Maybe[TypeScheme], "The optional type of the bound term"]

BINDING__NAME = Name("hydra.core.Binding")
BINDING__NAME__NAME = Name("name")
BINDING__TERM__NAME = Name("term")
BINDING__TYPE__NAME = Name("type")

@dataclass(frozen=True)
class CaseStatement:
    r"""A union elimination; a case statement."""
    
    type_name: Annotated[Name, "The name of the union type"]
    default: Annotated[Maybe[Term], "An optional default case, used if none of the explicit cases match"]
    cases: Annotated[frozenlist[Field], "A list of case alternatives, one per union field"]

CASE_STATEMENT__NAME = Name("hydra.core.CaseStatement")
CASE_STATEMENT__TYPE_NAME__NAME = Name("typeName")
CASE_STATEMENT__DEFAULT__NAME = Name("default")
CASE_STATEMENT__CASES__NAME = Name("cases")

@dataclass(frozen=True)
class EitherType:
    r"""A type which provides a choice between a 'left' type and a 'right' type."""
    
    left: Annotated[Type, "The 'left' alternative"]
    right: Annotated[Type, "The 'right' alternative"]

EITHER_TYPE__NAME = Name("hydra.core.EitherType")
EITHER_TYPE__LEFT__NAME = Name("left")
EITHER_TYPE__RIGHT__NAME = Name("right")

@dataclass(frozen=True)
class PairType:
    r"""A type which pairs a 'first' type and a 'second' type."""
    
    first: Annotated[Type, "The first component of the pair"]
    second: Annotated[Type, "The second component of the pair"]

PAIR_TYPE__NAME = Name("hydra.core.PairType")
PAIR_TYPE__FIRST__NAME = Name("first")
PAIR_TYPE__SECOND__NAME = Name("second")

class EliminationRecord(Node["Projection"]):
    r"""Eliminates a record by projecting a given field."""

class EliminationUnion(Node["CaseStatement"]):
    r"""Eliminates a union term by matching over the fields of the union. This is a case statement."""

class EliminationWrap(Node["Name"]):
    r"""Unwrap a wrapped term."""

class _EliminationMeta(type):
    def __getitem__(cls, item):
        return object

# A corresponding elimination for an introduction term.
class Elimination(metaclass=_EliminationMeta):
    r"""EliminationRecord | EliminationUnion | EliminationWrap"""
    
    pass

ELIMINATION__NAME = Name("hydra.core.Elimination")
ELIMINATION__RECORD__NAME = Name("record")
ELIMINATION__UNION__NAME = Name("union")
ELIMINATION__WRAP__NAME = Name("wrap")

@dataclass(frozen=True)
class Field:
    r"""A name/term pair."""
    
    name: Annotated[Name, "The name of the field"]
    term: Annotated[Term, "The term value of the field"]

FIELD__NAME = Name("hydra.core.Field")
FIELD__NAME__NAME = Name("name")
FIELD__TERM__NAME = Name("term")

@dataclass(frozen=True)
class FieldType:
    r"""A name/type pair."""
    
    name: Annotated[Name, "The name of the field"]
    type: Annotated[Type, "The type of the field"]

FIELD_TYPE__NAME = Name("hydra.core.FieldType")
FIELD_TYPE__NAME__NAME = Name("name")
FIELD_TYPE__TYPE__NAME = Name("type")

class FloatType(Enum):
    r"""A floating-point type."""
    
    BIGFLOAT = "bigfloat"
    r"""An arbitrary-precision floating-point type."""
    
    FLOAT32 = "float32"
    r"""A 32-bit floating-point type."""
    
    FLOAT64 = "float64"
    r"""A 64-bit floating-point type."""

FLOAT_TYPE__NAME = Name("hydra.core.FloatType")
FLOAT_TYPE__BIGFLOAT__NAME = Name("bigfloat")
FLOAT_TYPE__FLOAT32__NAME = Name("float32")
FLOAT_TYPE__FLOAT64__NAME = Name("float64")

class FloatValueBigfloat(Node[Decimal]):
    r"""An arbitrary-precision floating-point value."""

class FloatValueFloat32(Node[float]):
    r"""A 32-bit floating-point value."""

class FloatValueFloat64(Node[float]):
    r"""A 64-bit floating-point value."""

class _FloatValueMeta(type):
    def __getitem__(cls, item):
        return object

# A floating-point literal value.
class FloatValue(metaclass=_FloatValueMeta):
    r"""FloatValueBigfloat | FloatValueFloat32 | FloatValueFloat64"""
    
    pass

FLOAT_VALUE__NAME = Name("hydra.core.FloatValue")
FLOAT_VALUE__BIGFLOAT__NAME = Name("bigfloat")
FLOAT_VALUE__FLOAT32__NAME = Name("float32")
FLOAT_VALUE__FLOAT64__NAME = Name("float64")

@dataclass(frozen=True)
class ForallType:
    r"""A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term."""
    
    parameter: Annotated[Name, "The variable which is bound by the lambda"]
    body: Annotated[Type, "The body of the lambda"]

FORALL_TYPE__NAME = Name("hydra.core.ForallType")
FORALL_TYPE__PARAMETER__NAME = Name("parameter")
FORALL_TYPE__BODY__NAME = Name("body")

class FunctionElimination(Node["Elimination"]):
    r"""An elimination for any of a few term variants."""

class FunctionLambda(Node["Lambda"]):
    r"""A function abstraction (lambda)."""

class FunctionPrimitive(Node["Name"]):
    r"""A reference to a built-in (primitive) function."""

class _FunctionMeta(type):
    def __getitem__(cls, item):
        return object

# A function.
class Function(metaclass=_FunctionMeta):
    r"""FunctionElimination | FunctionLambda | FunctionPrimitive"""
    
    pass

FUNCTION__NAME = Name("hydra.core.Function")
FUNCTION__ELIMINATION__NAME = Name("elimination")
FUNCTION__LAMBDA__NAME = Name("lambda")
FUNCTION__PRIMITIVE__NAME = Name("primitive")

@dataclass(frozen=True)
class FunctionType:
    r"""A function type, also known as an arrow type."""
    
    domain: Annotated[Type, "The domain (input) type of the function"]
    codomain: Annotated[Type, "The codomain (output) type of the function"]

FUNCTION_TYPE__NAME = Name("hydra.core.FunctionType")
FUNCTION_TYPE__DOMAIN__NAME = Name("domain")
FUNCTION_TYPE__CODOMAIN__NAME = Name("codomain")

@dataclass(frozen=True)
class Injection:
    r"""An instance of a union type; i.e. a string-indexed generalization of inl() or inr()."""
    
    type_name: Annotated[Name, "The name of the union type"]
    field: Annotated[Field, "The field being injected, including its name and value"]

INJECTION__NAME = Name("hydra.core.Injection")
INJECTION__TYPE_NAME__NAME = Name("typeName")
INJECTION__FIELD__NAME = Name("field")

class IntegerType(Enum):
    r"""An integer type."""
    
    BIGINT = "bigint"
    r"""An arbitrary-precision integer type."""
    
    INT8 = "int8"
    r"""An 8-bit signed integer type."""
    
    INT16 = "int16"
    r"""A 16-bit signed integer type."""
    
    INT32 = "int32"
    r"""A 32-bit signed integer type."""
    
    INT64 = "int64"
    r"""A 64-bit signed integer type."""
    
    UINT8 = "uint8"
    r"""An 8-bit unsigned integer type."""
    
    UINT16 = "uint16"
    r"""A 16-bit unsigned integer type."""
    
    UINT32 = "uint32"
    r"""A 32-bit unsigned integer type."""
    
    UINT64 = "uint64"
    r"""A 64-bit unsigned integer type."""

INTEGER_TYPE__NAME = Name("hydra.core.IntegerType")
INTEGER_TYPE__BIGINT__NAME = Name("bigint")
INTEGER_TYPE__INT8__NAME = Name("int8")
INTEGER_TYPE__INT16__NAME = Name("int16")
INTEGER_TYPE__INT32__NAME = Name("int32")
INTEGER_TYPE__INT64__NAME = Name("int64")
INTEGER_TYPE__UINT8__NAME = Name("uint8")
INTEGER_TYPE__UINT16__NAME = Name("uint16")
INTEGER_TYPE__UINT32__NAME = Name("uint32")
INTEGER_TYPE__UINT64__NAME = Name("uint64")

class IntegerValueBigint(Node[int]):
    r"""An arbitrary-precision integer value."""

class IntegerValueInt8(Node[int]):
    r"""An 8-bit signed integer value."""

class IntegerValueInt16(Node[int]):
    r"""A 16-bit signed integer value (short value)."""

class IntegerValueInt32(Node[int]):
    r"""A 32-bit signed integer value (int value)."""

class IntegerValueInt64(Node[int]):
    r"""A 64-bit signed integer value (long value)."""

class IntegerValueUint8(Node[int]):
    r"""An 8-bit unsigned integer value (byte)."""

class IntegerValueUint16(Node[int]):
    r"""A 16-bit unsigned integer value."""

class IntegerValueUint32(Node[int]):
    r"""A 32-bit unsigned integer value (unsigned int)."""

class IntegerValueUint64(Node[int]):
    r"""A 64-bit unsigned integer value (unsigned long)."""

class _IntegerValueMeta(type):
    def __getitem__(cls, item):
        return object

# An integer literal value.
class IntegerValue(metaclass=_IntegerValueMeta):
    r"""IntegerValueBigint | IntegerValueInt8 | IntegerValueInt16 | IntegerValueInt32 | IntegerValueInt64 | IntegerValueUint8 | IntegerValueUint16 | IntegerValueUint32 | IntegerValueUint64"""
    
    pass

INTEGER_VALUE__NAME = Name("hydra.core.IntegerValue")
INTEGER_VALUE__BIGINT__NAME = Name("bigint")
INTEGER_VALUE__INT8__NAME = Name("int8")
INTEGER_VALUE__INT16__NAME = Name("int16")
INTEGER_VALUE__INT32__NAME = Name("int32")
INTEGER_VALUE__INT64__NAME = Name("int64")
INTEGER_VALUE__UINT8__NAME = Name("uint8")
INTEGER_VALUE__UINT16__NAME = Name("uint16")
INTEGER_VALUE__UINT32__NAME = Name("uint32")
INTEGER_VALUE__UINT64__NAME = Name("uint64")

@dataclass(frozen=True)
class Lambda:
    r"""A function abstraction (lambda)."""
    
    parameter: Annotated[Name, "The parameter of the lambda"]
    domain: Annotated[Maybe[Type], "An optional domain type for the lambda"]
    body: Annotated[Term, "The body of the lambda"]

LAMBDA__NAME = Name("hydra.core.Lambda")
LAMBDA__PARAMETER__NAME = Name("parameter")
LAMBDA__DOMAIN__NAME = Name("domain")
LAMBDA__BODY__NAME = Name("body")

@dataclass(frozen=True)
class Let:
    r"""A set of (possibly recursive) 'let' bindings together with a body in which they are bound."""
    
    bindings: Annotated[frozenlist[Binding], "The list of variable bindings"]
    body: Annotated[Term, "The body term in which the variables are bound"]

LET__NAME = Name("hydra.core.Let")
LET__BINDINGS__NAME = Name("bindings")
LET__BODY__NAME = Name("body")

class LiteralBinary(Node[bytes]):
    r"""A binary literal."""

class LiteralBoolean(Node[bool]):
    r"""A boolean literal."""

class LiteralFloat(Node["FloatValue"]):
    r"""A floating-point literal."""

class LiteralInteger(Node["IntegerValue"]):
    r"""An integer literal."""

class LiteralString(Node[str]):
    r"""A string literal."""

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A term constant; an instance of a literal type.
class Literal(metaclass=_LiteralMeta):
    r"""LiteralBinary | LiteralBoolean | LiteralFloat | LiteralInteger | LiteralString"""
    
    pass

LITERAL__NAME = Name("hydra.core.Literal")
LITERAL__BINARY__NAME = Name("binary")
LITERAL__BOOLEAN__NAME = Name("boolean")
LITERAL__FLOAT__NAME = Name("float")
LITERAL__INTEGER__NAME = Name("integer")
LITERAL__STRING__NAME = Name("string")

class LiteralTypeBinary:
    r"""The type of a binary (byte string) value."""

class LiteralTypeBoolean:
    r"""The type of a boolean (true/false) value."""

class LiteralTypeFloat(Node["FloatType"]):
    r"""The type of a floating-point value."""

class LiteralTypeInteger(Node["IntegerType"]):
    r"""The type of an integer value."""

class LiteralTypeString:
    r"""The type of a string value."""

class _LiteralTypeMeta(type):
    def __getitem__(cls, item):
        return object

# Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants.
class LiteralType(metaclass=_LiteralTypeMeta):
    r"""LiteralTypeBinary | LiteralTypeBoolean | LiteralTypeFloat | LiteralTypeInteger | LiteralTypeString"""
    
    pass

LITERAL_TYPE__NAME = Name("hydra.core.LiteralType")
LITERAL_TYPE__BINARY__NAME = Name("binary")
LITERAL_TYPE__BOOLEAN__NAME = Name("boolean")
LITERAL_TYPE__FLOAT__NAME = Name("float")
LITERAL_TYPE__INTEGER__NAME = Name("integer")
LITERAL_TYPE__STRING__NAME = Name("string")

@dataclass(frozen=True)
class MapType:
    r"""A map type."""
    
    keys: Annotated[Type, "The type of keys in the map"]
    values: Annotated[Type, "The type of values in the map"]

MAP_TYPE__NAME = Name("hydra.core.MapType")
MAP_TYPE__KEYS__NAME = Name("keys")
MAP_TYPE__VALUES__NAME = Name("values")

@dataclass(frozen=True)
class Projection:
    r"""A record elimination; a projection."""
    
    type_name: Annotated[Name, "The name of the record type"]
    field: Annotated[Name, "The name of the projected field"]

PROJECTION__NAME = Name("hydra.core.Projection")
PROJECTION__TYPE_NAME__NAME = Name("typeName")
PROJECTION__FIELD__NAME = Name("field")

@dataclass(frozen=True)
class Record:
    r"""A record, or labeled tuple; a map of field names to terms."""
    
    type_name: Annotated[Name, "The name of the record type"]
    fields: Annotated[frozenlist[Field], "The fields of the record, as a list of name/term pairs"]

RECORD__NAME = Name("hydra.core.Record")
RECORD__TYPE_NAME__NAME = Name("typeName")
RECORD__FIELDS__NAME = Name("fields")

@dataclass(frozen=True)
class RowType:
    r"""A labeled record or union type."""
    
    type_name: Annotated[Name, "The name of the row type, which must correspond to the name of a Type element"]
    fields: Annotated[frozenlist[FieldType], "The fields of this row type, excluding any inherited fields"]

ROW_TYPE__NAME = Name("hydra.core.RowType")
ROW_TYPE__TYPE_NAME__NAME = Name("typeName")
ROW_TYPE__FIELDS__NAME = Name("fields")

class TermAnnotated(Node["AnnotatedTerm"]):
    r"""A term annotated with metadata."""

class TermApplication(Node["Application"]):
    r"""A function application."""

class TermEither(Node["Either[Term, Term]"]):
    r"""An either value."""

class TermFunction(Node["Function"]):
    r"""A function term."""

class TermLet(Node["Let"]):
    r"""A 'let' term, which binds variables to terms."""

class TermList(Node["frozenlist[Term]"]):
    r"""A list."""

class TermLiteral(Node["Literal"]):
    r"""A literal value."""

class TermMap(Node["FrozenDict[Term, Term]"]):
    r"""A map of keys to values."""

class TermMaybe(Node["Maybe[Term]"]):
    r"""An optional value."""

class TermPair(Node["tuple[Term, Term]"]):
    r"""A pair (2-tuple)."""

class TermRecord(Node["Record"]):
    r"""A record term."""

class TermSet(Node["frozenset[Term]"]):
    r"""A set of values."""

class TermTypeApplication(Node["TypeApplicationTerm"]):
    r"""A System F type application term."""

class TermTypeLambda(Node["TypeLambda"]):
    r"""A System F type abstraction term."""

class TermUnion(Node["Injection"]):
    r"""An injection; an instance of a union type."""

class TermUnit:
    r"""A unit value; a term with no value."""

class TermVariable(Node["Name"]):
    r"""A variable reference."""

class TermWrap(Node["WrappedTerm"]):
    r"""A wrapped term; an instance of a wrapper type (newtype)."""

class _TermMeta(type):
    def __getitem__(cls, item):
        return object

# A data term.
class Term(metaclass=_TermMeta):
    r"""TermAnnotated | TermApplication | TermEither | TermFunction | TermLet | TermList | TermLiteral | TermMap | TermMaybe | TermPair | TermRecord | TermSet | TermTypeApplication | TermTypeLambda | TermUnion | TermUnit | TermVariable | TermWrap"""
    
    pass

TERM__NAME = Name("hydra.core.Term")
TERM__ANNOTATED__NAME = Name("annotated")
TERM__APPLICATION__NAME = Name("application")
TERM__EITHER__NAME = Name("either")
TERM__FUNCTION__NAME = Name("function")
TERM__LET__NAME = Name("let")
TERM__LIST__NAME = Name("list")
TERM__LITERAL__NAME = Name("literal")
TERM__MAP__NAME = Name("map")
TERM__MAYBE__NAME = Name("maybe")
TERM__PAIR__NAME = Name("pair")
TERM__RECORD__NAME = Name("record")
TERM__SET__NAME = Name("set")
TERM__TYPE_APPLICATION__NAME = Name("typeApplication")
TERM__TYPE_LAMBDA__NAME = Name("typeLambda")
TERM__UNION__NAME = Name("union")
TERM__UNIT__NAME = Name("unit")
TERM__VARIABLE__NAME = Name("variable")
TERM__WRAP__NAME = Name("wrap")

class TypeAnnotated(Node["AnnotatedType"]):
    r"""An annotated type."""

class TypeApplication(Node["ApplicationType"]):
    r"""A type application."""

class TypeEither(Node["EitherType"]):
    r"""An either (sum) type."""

class TypeForall(Node["ForallType"]):
    r"""A universally quantified (polymorphic) type."""

class TypeFunction(Node["FunctionType"]):
    r"""A function type."""

class TypeList(Node["Type"]):
    r"""A list type."""

class TypeLiteral(Node["LiteralType"]):
    r"""A literal type."""

class TypeMap(Node["MapType"]):
    r"""A map type."""

class TypeMaybe(Node["Type"]):
    r"""An optional type."""

class TypePair(Node["PairType"]):
    r"""A pair (2-tuple) type."""

class TypeRecord(Node["RowType"]):
    r"""A record type."""

class TypeSet(Node["Type"]):
    r"""A set type."""

class TypeUnion(Node["RowType"]):
    r"""A union type with field names."""

class TypeUnit:
    r"""The unit type."""

class TypeVariable(Node["Name"]):
    r"""A type variable."""

class TypeWrap(Node["WrappedType"]):
    r"""A wrapped type (newtype)."""

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

# A data type.
class Type(metaclass=_TypeMeta):
    r"""TypeAnnotated | TypeApplication | TypeEither | TypeForall | TypeFunction | TypeList | TypeLiteral | TypeMap | TypeMaybe | TypePair | TypeRecord | TypeSet | TypeUnion | TypeUnit | TypeVariable | TypeWrap"""
    
    pass

TYPE__NAME = Name("hydra.core.Type")
TYPE__ANNOTATED__NAME = Name("annotated")
TYPE__APPLICATION__NAME = Name("application")
TYPE__EITHER__NAME = Name("either")
TYPE__FORALL__NAME = Name("forall")
TYPE__FUNCTION__NAME = Name("function")
TYPE__LIST__NAME = Name("list")
TYPE__LITERAL__NAME = Name("literal")
TYPE__MAP__NAME = Name("map")
TYPE__MAYBE__NAME = Name("maybe")
TYPE__PAIR__NAME = Name("pair")
TYPE__RECORD__NAME = Name("record")
TYPE__SET__NAME = Name("set")
TYPE__UNION__NAME = Name("union")
TYPE__UNIT__NAME = Name("unit")
TYPE__VARIABLE__NAME = Name("variable")
TYPE__WRAP__NAME = Name("wrap")

@dataclass(frozen=True)
class TypeApplicationTerm:
    r"""A term applied to a type; a type application."""
    
    body: Annotated[Term, "The term being applied to a type"]
    type: Annotated[Type, "The type argument"]

TYPE_APPLICATION_TERM__NAME = Name("hydra.core.TypeApplicationTerm")
TYPE_APPLICATION_TERM__BODY__NAME = Name("body")
TYPE_APPLICATION_TERM__TYPE__NAME = Name("type")

@dataclass(frozen=True)
class TypeLambda:
    r"""A System F type abstraction term."""
    
    parameter: Annotated[Name, "The type variable introduced by the abstraction"]
    body: Annotated[Term, "The body of the abstraction"]

TYPE_LAMBDA__NAME = Name("hydra.core.TypeLambda")
TYPE_LAMBDA__PARAMETER__NAME = Name("parameter")
TYPE_LAMBDA__BODY__NAME = Name("body")

@dataclass(frozen=True)
class TypeScheme:
    r"""A type expression together with free type variables occurring in the expression."""
    
    variables: Annotated[frozenlist[Name], "The free type variables"]
    type: Annotated[Type, "The type expression"]

TYPE_SCHEME__NAME = Name("hydra.core.TypeScheme")
TYPE_SCHEME__VARIABLES__NAME = Name("variables")
TYPE_SCHEME__TYPE__NAME = Name("type")

@dataclass(frozen=True)
class WrappedTerm:
    r"""A term wrapped in a type name."""
    
    type_name: Annotated[Name, "The name of the wrapper type"]
    body: Annotated[Term, "The wrapped term"]

WRAPPED_TERM__NAME = Name("hydra.core.WrappedTerm")
WRAPPED_TERM__TYPE_NAME__NAME = Name("typeName")
WRAPPED_TERM__BODY__NAME = Name("body")

@dataclass(frozen=True)
class WrappedType:
    r"""A type wrapped in a type name; a newtype."""
    
    type_name: Annotated[Name, "The name of the wrapper (newtype)"]
    body: Annotated[Type, "The wrapped type"]

WRAPPED_TYPE__NAME = Name("hydra.core.WrappedType")
WRAPPED_TYPE__TYPE_NAME__NAME = Name("typeName")
WRAPPED_TYPE__BODY__NAME = Name("body")
