# Note: this is an automatically generated file. Do not edit.

r"""Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from hydra.dsl.python import Either, FrozenDict, Maybe, Node, frozenlist
from typing import Annotated

class Name(Node[str]):
    r"""A unique identifier in some context; a string-valued key."""

NAME__NAME = Name("hydra.core.Name")

@dataclass
class AnnotatedTerm:
    r"""A term together with an annotation."""
    
    body: Term
    annotation: FrozenDict[Name, Term]

ANNOTATED_TERM__NAME = Name("hydra.core.AnnotatedTerm")
ANNOTATED_TERM__BODY__NAME = Name("body")
ANNOTATED_TERM__ANNOTATION__NAME = Name("annotation")

@dataclass
class AnnotatedType:
    r"""A type together with an annotation."""
    
    body: Type
    annotation: FrozenDict[Name, Term]

ANNOTATED_TYPE__NAME = Name("hydra.core.AnnotatedType")
ANNOTATED_TYPE__BODY__NAME = Name("body")
ANNOTATED_TYPE__ANNOTATION__NAME = Name("annotation")

@dataclass
class Application:
    r"""A term which applies a function to an argument."""
    
    function: Annotated[Term, "The left-hand side of the application"]
    argument: Annotated[Term, "The right-hand side of the application"]

APPLICATION__NAME = Name("hydra.core.Application")
APPLICATION__FUNCTION__NAME = Name("function")
APPLICATION__ARGUMENT__NAME = Name("argument")

@dataclass
class ApplicationType:
    r"""The type-level analog of an application term."""
    
    function: Annotated[Type, "The left-hand side of the application"]
    argument: Annotated[Type, "The right-hand side of the application"]

APPLICATION_TYPE__NAME = Name("hydra.core.ApplicationType")
APPLICATION_TYPE__FUNCTION__NAME = Name("function")
APPLICATION_TYPE__ARGUMENT__NAME = Name("argument")

@dataclass
class Binding:
    r"""A field with an optional type scheme, used to bind variables to terms in a 'let' expression."""
    
    name: Name
    term: Term
    type: Maybe[TypeScheme]

BINDING__NAME = Name("hydra.core.Binding")
BINDING__NAME__NAME = Name("name")
BINDING__TERM__NAME = Name("term")
BINDING__TYPE__NAME = Name("type")

@dataclass
class CaseStatement:
    r"""A union elimination; a case statement."""
    
    type_name: Name
    default: Maybe[Term]
    cases: frozenlist[Field]

CASE_STATEMENT__NAME = Name("hydra.core.CaseStatement")
CASE_STATEMENT__TYPE_NAME__NAME = Name("typeName")
CASE_STATEMENT__DEFAULT__NAME = Name("default")
CASE_STATEMENT__CASES__NAME = Name("cases")

@dataclass
class EitherType:
    r"""A type which provides a choice between a 'left' type and a 'right' type."""
    
    left: Type
    right: Type

EITHER_TYPE__NAME = Name("hydra.core.EitherType")
EITHER_TYPE__LEFT__NAME = Name("left")
EITHER_TYPE__RIGHT__NAME = Name("right")

class EliminationProduct(Node["TupleProjection"]):
    r"""Eliminates a tuple by projecting the component at a given 0-indexed offset."""

class EliminationRecord(Node["Projection"]):
    r"""Eliminates a record by projecting a given field."""

class EliminationUnion(Node["CaseStatement"]):
    r"""Eliminates a union term by matching over the fields of the union. This is a case statement."""

class EliminationWrap(Node["Name"]):
    r"""Unwrap a wrapped term."""

# A corresponding elimination for an introduction term.
type Elimination = EliminationProduct | EliminationRecord | EliminationUnion | EliminationWrap

ELIMINATION__NAME = Name("hydra.core.Elimination")
ELIMINATION__PRODUCT__NAME = Name("product")
ELIMINATION__RECORD__NAME = Name("record")
ELIMINATION__UNION__NAME = Name("union")
ELIMINATION__WRAP__NAME = Name("wrap")

@dataclass
class Field:
    r"""A name/term pair."""
    
    name: Name
    term: Term

FIELD__NAME = Name("hydra.core.Field")
FIELD__NAME__NAME = Name("name")
FIELD__TERM__NAME = Name("term")

@dataclass
class FieldType:
    r"""A name/type pair."""
    
    name: Name
    type: Type

FIELD_TYPE__NAME = Name("hydra.core.FieldType")
FIELD_TYPE__NAME__NAME = Name("name")
FIELD_TYPE__TYPE__NAME = Name("type")

class FloatType(Enum):
    r"""A floating-point type."""
    
    BIGFLOAT = "bigfloat"
    
    FLOAT32 = "float32"
    
    FLOAT64 = "float64"

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

# A floating-point literal value.
type FloatValue = FloatValueBigfloat | FloatValueFloat32 | FloatValueFloat64

FLOAT_VALUE__NAME = Name("hydra.core.FloatValue")
FLOAT_VALUE__BIGFLOAT__NAME = Name("bigfloat")
FLOAT_VALUE__FLOAT32__NAME = Name("float32")
FLOAT_VALUE__FLOAT64__NAME = Name("float64")

@dataclass
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

# A function.
type Function = FunctionElimination | FunctionLambda | FunctionPrimitive

FUNCTION__NAME = Name("hydra.core.Function")
FUNCTION__ELIMINATION__NAME = Name("elimination")
FUNCTION__LAMBDA__NAME = Name("lambda")
FUNCTION__PRIMITIVE__NAME = Name("primitive")

@dataclass
class FunctionType:
    r"""A function type, also known as an arrow type."""
    
    domain: Type
    codomain: Type

FUNCTION_TYPE__NAME = Name("hydra.core.FunctionType")
FUNCTION_TYPE__DOMAIN__NAME = Name("domain")
FUNCTION_TYPE__CODOMAIN__NAME = Name("codomain")

@dataclass
class Injection:
    r"""An instance of a union type; i.e. a string-indexed generalization of inl() or inr()."""
    
    type_name: Name
    field: Field

INJECTION__NAME = Name("hydra.core.Injection")
INJECTION__TYPE_NAME__NAME = Name("typeName")
INJECTION__FIELD__NAME = Name("field")

class IntegerType(Enum):
    r"""An integer type."""
    
    BIGINT = "bigint"
    
    INT8 = "int8"
    
    INT16 = "int16"
    
    INT32 = "int32"
    
    INT64 = "int64"
    
    UINT8 = "uint8"
    
    UINT16 = "uint16"
    
    UINT32 = "uint32"
    
    UINT64 = "uint64"

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

# An integer literal value.
type IntegerValue = IntegerValueBigint | IntegerValueInt8 | IntegerValueInt16 | IntegerValueInt32 | IntegerValueInt64 | IntegerValueUint8 | IntegerValueUint16 | IntegerValueUint32 | IntegerValueUint64

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

@dataclass
class Lambda:
    r"""A function abstraction (lambda)."""
    
    parameter: Annotated[Name, "The parameter of the lambda"]
    domain: Annotated[Maybe[Type], "An optional domain type for the lambda"]
    body: Annotated[Term, "The body of the lambda"]

LAMBDA__NAME = Name("hydra.core.Lambda")
LAMBDA__PARAMETER__NAME = Name("parameter")
LAMBDA__DOMAIN__NAME = Name("domain")
LAMBDA__BODY__NAME = Name("body")

@dataclass
class Let:
    r"""A set of (possibly recursive) 'let' bindings together with a body in which they are bound."""
    
    bindings: frozenlist[Binding]
    body: Term

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

# A term constant; an instance of a literal type.
type Literal = LiteralBinary | LiteralBoolean | LiteralFloat | LiteralInteger | LiteralString

LITERAL__NAME = Name("hydra.core.Literal")
LITERAL__BINARY__NAME = Name("binary")
LITERAL__BOOLEAN__NAME = Name("boolean")
LITERAL__FLOAT__NAME = Name("float")
LITERAL__INTEGER__NAME = Name("integer")
LITERAL__STRING__NAME = Name("string")

class LiteralTypeBinary(Node[None]):
    r"""The type of a binary (byte string) value."""

class LiteralTypeBoolean(Node[None]):
    r"""The type of a boolean (true/false) value."""

class LiteralTypeFloat(Node["FloatType"]):
    r"""The type of a floating-point value."""

class LiteralTypeInteger(Node["IntegerType"]):
    r"""The type of an integer value."""

class LiteralTypeString(Node[None]):
    r"""The type of a string value."""

# Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants.
type LiteralType = LiteralTypeBinary | LiteralTypeBoolean | LiteralTypeFloat | LiteralTypeInteger | LiteralTypeString

LITERAL_TYPE__NAME = Name("hydra.core.LiteralType")
LITERAL_TYPE__BINARY__NAME = Name("binary")
LITERAL_TYPE__BOOLEAN__NAME = Name("boolean")
LITERAL_TYPE__FLOAT__NAME = Name("float")
LITERAL_TYPE__INTEGER__NAME = Name("integer")
LITERAL_TYPE__STRING__NAME = Name("string")

@dataclass
class MapType:
    r"""A map type."""
    
    keys: Type
    values: Type

MAP_TYPE__NAME = Name("hydra.core.MapType")
MAP_TYPE__KEYS__NAME = Name("keys")
MAP_TYPE__VALUES__NAME = Name("values")

@dataclass
class Projection:
    r"""A record elimination; a projection."""
    
    type_name: Annotated[Name, "The name of the record type"]
    field: Annotated[Name, "The name of the projected field"]

PROJECTION__NAME = Name("hydra.core.Projection")
PROJECTION__TYPE_NAME__NAME = Name("typeName")
PROJECTION__FIELD__NAME = Name("field")

@dataclass
class Record:
    r"""A record, or labeled tuple; a map of field names to terms."""
    
    type_name: Name
    fields: frozenlist[Field]

RECORD__NAME = Name("hydra.core.Record")
RECORD__TYPE_NAME__NAME = Name("typeName")
RECORD__FIELDS__NAME = Name("fields")

@dataclass
class RowType:
    r"""A labeled record or union type."""
    
    type_name: Annotated[Name, "The name of the row type, which must correspond to the name of a Type element"]
    fields: Annotated[frozenlist[FieldType], "The fields of this row type, excluding any inherited fields"]

ROW_TYPE__NAME = Name("hydra.core.RowType")
ROW_TYPE__TYPE_NAME__NAME = Name("typeName")
ROW_TYPE__FIELDS__NAME = Name("fields")

@dataclass
class Sum:
    r"""The unlabeled equivalent of an Injection term."""
    
    index: int
    size: int
    term: Term

SUM__NAME = Name("hydra.core.Sum")
SUM__INDEX__NAME = Name("index")
SUM__SIZE__NAME = Name("size")
SUM__TERM__NAME = Name("term")

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

class TermProduct(Node["frozenlist[Term]"]):
    r"""A tuple."""

class TermRecord(Node["Record"]):
    r"""A record term."""

class TermSet(Node["frozenset[Term]"]):
    r"""A set of values."""

class TermSum(Node["Sum"]):
    r"""A variant tuple."""

class TermTypeApplication(Node["TypeApplicationTerm"]):
    r"""A System F type application term."""

class TermTypeLambda(Node["TypeLambda"]):
    r"""A System F type abstraction term."""

class TermUnion(Node["Injection"]):
    r"""An injection; an instance of a union type."""

class TermUnit(Node[None]):
    r"""A unit value; a term with no value."""

class TermVariable(Node["Name"]):
    r"""A variable reference."""

class TermWrap(Node["WrappedTerm"]):
    r"""A wrapped term; an instance of a wrapper type (newtype)."""

# A data term.
type Term = TermAnnotated | TermApplication | TermEither | TermFunction | TermLet | TermList | TermLiteral | TermMap | TermMaybe | TermProduct | TermRecord | TermSet | TermSum | TermTypeApplication | TermTypeLambda | TermUnion | TermUnit | TermVariable | TermWrap

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
TERM__PRODUCT__NAME = Name("product")
TERM__RECORD__NAME = Name("record")
TERM__SET__NAME = Name("set")
TERM__SUM__NAME = Name("sum")
TERM__TYPE_APPLICATION__NAME = Name("typeApplication")
TERM__TYPE_LAMBDA__NAME = Name("typeLambda")
TERM__UNION__NAME = Name("union")
TERM__UNIT__NAME = Name("unit")
TERM__VARIABLE__NAME = Name("variable")
TERM__WRAP__NAME = Name("wrap")

@dataclass
class TupleProjection:
    r"""A tuple elimination; a projection from an integer-indexed product."""
    
    arity: Annotated[int, "The arity of the tuple"]
    index: Annotated[int, "The 0-indexed offset from the beginning of the tuple"]
    domain: Annotated[Maybe[frozenlist[Type]], "An optional domain for the projection; this is a list of component types"]

TUPLE_PROJECTION__NAME = Name("hydra.core.TupleProjection")
TUPLE_PROJECTION__ARITY__NAME = Name("arity")
TUPLE_PROJECTION__INDEX__NAME = Name("index")
TUPLE_PROJECTION__DOMAIN__NAME = Name("domain")

class TypeAnnotated(Node["AnnotatedType"]): ...

class TypeApplication(Node["ApplicationType"]): ...

class TypeEither(Node["EitherType"]): ...

class TypeForall(Node["ForallType"]): ...

class TypeFunction(Node["FunctionType"]): ...

class TypeList(Node["Type"]): ...

class TypeLiteral(Node["LiteralType"]): ...

class TypeMap(Node["MapType"]): ...

class TypeMaybe(Node["Type"]): ...

class TypeProduct(Node["frozenlist[Type]"]): ...

class TypeRecord(Node["RowType"]): ...

class TypeSet(Node["Type"]): ...

class TypeSum(Node["frozenlist[Type]"]): ...

class TypeUnion(Node["RowType"]): ...

class TypeUnit(Node[None]): ...

class TypeVariable(Node["Name"]): ...

class TypeWrap(Node["WrappedType"]): ...

# A data type.
type Type = TypeAnnotated | TypeApplication | TypeEither | TypeForall | TypeFunction | TypeList | TypeLiteral | TypeMap | TypeMaybe | TypeProduct | TypeRecord | TypeSet | TypeSum | TypeUnion | TypeUnit | TypeVariable | TypeWrap

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
TYPE__PRODUCT__NAME = Name("product")
TYPE__RECORD__NAME = Name("record")
TYPE__SET__NAME = Name("set")
TYPE__SUM__NAME = Name("sum")
TYPE__UNION__NAME = Name("union")
TYPE__UNIT__NAME = Name("unit")
TYPE__VARIABLE__NAME = Name("variable")
TYPE__WRAP__NAME = Name("wrap")

@dataclass
class TypeApplicationTerm:
    r"""A term applied to a type; a type application."""
    
    body: Term
    type: Type

TYPE_APPLICATION_TERM__NAME = Name("hydra.core.TypeApplicationTerm")
TYPE_APPLICATION_TERM__BODY__NAME = Name("body")
TYPE_APPLICATION_TERM__TYPE__NAME = Name("type")

@dataclass
class TypeLambda:
    r"""A System F type abstraction term."""
    
    parameter: Annotated[Name, "The type variable introduced by the abstraction"]
    body: Annotated[Term, "The body of the abstraction"]

TYPE_LAMBDA__NAME = Name("hydra.core.TypeLambda")
TYPE_LAMBDA__PARAMETER__NAME = Name("parameter")
TYPE_LAMBDA__BODY__NAME = Name("body")

@dataclass
class TypeScheme:
    r"""A type expression together with free type variables occurring in the expression."""
    
    variables: frozenlist[Name]
    type: Type

TYPE_SCHEME__NAME = Name("hydra.core.TypeScheme")
TYPE_SCHEME__VARIABLES__NAME = Name("variables")
TYPE_SCHEME__TYPE__NAME = Name("type")

@dataclass
class WrappedTerm:
    r"""A term wrapped in a type name."""
    
    type_name: Name
    body: Term

WRAPPED_TERM__NAME = Name("hydra.core.WrappedTerm")
WRAPPED_TERM__TYPE_NAME__NAME = Name("typeName")
WRAPPED_TERM__BODY__NAME = Name("body")

@dataclass
class WrappedType:
    r"""A type wrapped in a type name; a newtype."""
    
    type_name: Name
    body: Type

WRAPPED_TYPE__NAME = Name("hydra.core.WrappedType")
WRAPPED_TYPE__TYPE_NAME__NAME = Name("typeName")
WRAPPED_TYPE__BODY__NAME = Name("body")
