# Note: this is an automatically generated file. Do not edit.

r"""Hydra's core data model, consisting of the fundamental hydra.core.Term type and all of its dependencies."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Either, FrozenDict, Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast

class Name(Node[str]):
    r"""A unique identifier in some context; a string-valued key."""

Name.TYPE_ = Name("hydra.core.Name")

@dataclass(frozen=True)
class AnnotatedTerm:
    r"""A term together with an annotation."""

    body: Annotated[Term, "The term being annotated"]
    annotation: Annotated[FrozenDict[Name, Term], "The annotation as a map from keys to values"]

    TYPE_ = Name("hydra.core.AnnotatedTerm")
    BODY = Name("body")
    ANNOTATION = Name("annotation")

@dataclass(frozen=True)
class AnnotatedType:
    r"""A type together with an annotation."""

    body: Annotated[Type, "The type being annotated"]
    annotation: Annotated[FrozenDict[Name, Term], "The annotation as a map from keys to values"]

    TYPE_ = Name("hydra.core.AnnotatedType")
    BODY = Name("body")
    ANNOTATION = Name("annotation")

@dataclass(frozen=True)
class Application:
    r"""A term which applies a function to an argument."""

    function: Annotated[Term, "The left-hand side of the application"]
    argument: Annotated[Term, "The right-hand side of the application"]

    TYPE_ = Name("hydra.core.Application")
    FUNCTION = Name("function")
    ARGUMENT = Name("argument")

@dataclass(frozen=True)
class ApplicationType:
    r"""The type-level analog of an application term."""

    function: Annotated[Type, "The left-hand side of the application"]
    argument: Annotated[Type, "The right-hand side of the application"]

    TYPE_ = Name("hydra.core.ApplicationType")
    FUNCTION = Name("function")
    ARGUMENT = Name("argument")

@dataclass(frozen=True)
class Binding:
    r"""A field with an optional type scheme, used to bind variables to terms in a 'let' expression."""

    name: Annotated[Name, "The name of the bound variable"]
    term: Annotated[Term, "The term to which the variable is bound"]
    type: Annotated[Maybe[TypeScheme], "The optional type of the bound term"]

    TYPE_ = Name("hydra.core.Binding")
    NAME = Name("name")
    TERM = Name("term")
    TYPE = Name("type")

@dataclass(frozen=True)
class CaseStatement:
    r"""A union elimination; a case statement."""

    type_name: Annotated[Name, "The name of the union type"]
    default: Annotated[Maybe[Term], "An optional default case, used if none of the explicit cases match"]
    cases: Annotated[frozenlist[Field], "A list of case alternatives, one per union field"]

    TYPE_ = Name("hydra.core.CaseStatement")
    TYPE_NAME = Name("typeName")
    DEFAULT = Name("default")
    CASES = Name("cases")

@dataclass(frozen=True)
class EitherType:
    r"""A type which provides a choice between a 'left' type and a 'right' type."""

    left: Annotated[Type, "The 'left' alternative"]
    right: Annotated[Type, "The 'right' alternative"]

    TYPE_ = Name("hydra.core.EitherType")
    LEFT = Name("left")
    RIGHT = Name("right")

@dataclass(frozen=True)
class PairType:
    r"""A type which pairs a 'first' type and a 'second' type."""

    first: Annotated[Type, "The first component of the pair"]
    second: Annotated[Type, "The second component of the pair"]

    TYPE_ = Name("hydra.core.PairType")
    FIRST = Name("first")
    SECOND = Name("second")

class EliminationRecord(Node["Projection"]):
    r"""Eliminates a record by projecting a given field"""

class EliminationUnion(Node["CaseStatement"]):
    r"""Eliminates a union term by matching over the fields of the union. This is a case statement."""

class EliminationWrap(Node["Name"]):
    r"""Unwrap a wrapped term"""

class _EliminationMeta(type):
    def __getitem__(cls, item):
        return object

# A corresponding elimination for an introduction term.
class Elimination(metaclass=_EliminationMeta):
    r"""EliminationRecord | EliminationUnion | EliminationWrap"""

    TYPE_ = Name("hydra.core.Elimination")
    RECORD = Name("record")
    UNION = Name("union")
    WRAP = Name("wrap")

@dataclass(frozen=True)
class Field:
    r"""A name/term pair."""

    name: Annotated[Name, "The name of the field"]
    term: Annotated[Term, "The term value of the field"]

    TYPE_ = Name("hydra.core.Field")
    NAME = Name("name")
    TERM = Name("term")

@dataclass(frozen=True)
class FieldType:
    r"""A name/type pair."""

    name: Annotated[Name, "The name of the field"]
    type: Annotated[Type, "The type of the field"]

    TYPE_ = Name("hydra.core.FieldType")
    NAME = Name("name")
    TYPE = Name("type")

class FloatType(Enum):
    r"""A floating-point type."""

    BIGFLOAT = Name("bigfloat")
    r"""An arbitrary-precision floating-point type"""

    FLOAT32 = Name("float32")
    r"""A 32-bit floating-point type"""

    FLOAT64 = Name("float64")
    r"""A 64-bit floating-point type"""

FloatType.TYPE_ = Name("hydra.core.FloatType")

class FloatValueBigfloat(Node[Decimal]):
    r"""An arbitrary-precision floating-point value"""

class FloatValueFloat32(Node[float]):
    r"""A 32-bit floating-point value"""

class FloatValueFloat64(Node[float]):
    r"""A 64-bit floating-point value"""

class _FloatValueMeta(type):
    def __getitem__(cls, item):
        return object

# A floating-point literal value.
class FloatValue(metaclass=_FloatValueMeta):
    r"""FloatValueBigfloat | FloatValueFloat32 | FloatValueFloat64"""

    TYPE_ = Name("hydra.core.FloatValue")
    BIGFLOAT = Name("bigfloat")
    FLOAT32 = Name("float32")
    FLOAT64 = Name("float64")

@dataclass(frozen=True)
class ForallType:
    r"""A universally quantified type; the System F equivalent of a type scheme, and the type-level equivalent of a lambda term."""

    parameter: Annotated[Name, "The variable which is bound by the lambda"]
    body: Annotated[Type, "The body of the lambda"]

    TYPE_ = Name("hydra.core.ForallType")
    PARAMETER = Name("parameter")
    BODY = Name("body")

class FunctionElimination(Node["Elimination"]):
    r"""An elimination for any of a few term variants"""

class FunctionLambda(Node["Lambda"]):
    r"""A function abstraction (lambda)"""

class _FunctionMeta(type):
    def __getitem__(cls, item):
        return object

# A function.
class Function(metaclass=_FunctionMeta):
    r"""FunctionElimination | FunctionLambda"""

    TYPE_ = Name("hydra.core.Function")
    ELIMINATION = Name("elimination")
    LAMBDA = Name("lambda")

@dataclass(frozen=True)
class FunctionType:
    r"""A function type, also known as an arrow type."""

    domain: Annotated[Type, "The domain (input) type of the function"]
    codomain: Annotated[Type, "The codomain (output) type of the function"]

    TYPE_ = Name("hydra.core.FunctionType")
    DOMAIN = Name("domain")
    CODOMAIN = Name("codomain")

@dataclass(frozen=True)
class Injection:
    r"""An instance of a union type; i.e. a string-indexed generalization of inl() or inr()."""

    type_name: Annotated[Name, "The name of the union type"]
    field: Annotated[Field, "The field being injected, including its name and value"]

    TYPE_ = Name("hydra.core.Injection")
    TYPE_NAME = Name("typeName")
    FIELD = Name("field")

class IntegerType(Enum):
    r"""An integer type."""

    BIGINT = Name("bigint")
    r"""An arbitrary-precision integer type"""

    INT8 = Name("int8")
    r"""An 8-bit signed integer type"""

    INT16 = Name("int16")
    r"""A 16-bit signed integer type"""

    INT32 = Name("int32")
    r"""A 32-bit signed integer type"""

    INT64 = Name("int64")
    r"""A 64-bit signed integer type"""

    UINT8 = Name("uint8")
    r"""An 8-bit unsigned integer type"""

    UINT16 = Name("uint16")
    r"""A 16-bit unsigned integer type"""

    UINT32 = Name("uint32")
    r"""A 32-bit unsigned integer type"""

    UINT64 = Name("uint64")
    r"""A 64-bit unsigned integer type"""

IntegerType.TYPE_ = Name("hydra.core.IntegerType")

class IntegerValueBigint(Node[int]):
    r"""An arbitrary-precision integer value"""

class IntegerValueInt8(Node[int]):
    r"""An 8-bit signed integer value"""

class IntegerValueInt16(Node[int]):
    r"""A 16-bit signed integer value (short value)"""

class IntegerValueInt32(Node[int]):
    r"""A 32-bit signed integer value (int value)"""

class IntegerValueInt64(Node[int]):
    r"""A 64-bit signed integer value (long value)"""

class IntegerValueUint8(Node[int]):
    r"""An 8-bit unsigned integer value (byte)"""

class IntegerValueUint16(Node[int]):
    r"""A 16-bit unsigned integer value"""

class IntegerValueUint32(Node[int]):
    r"""A 32-bit unsigned integer value (unsigned int)"""

class IntegerValueUint64(Node[int]):
    r"""A 64-bit unsigned integer value (unsigned long)"""

class _IntegerValueMeta(type):
    def __getitem__(cls, item):
        return object

# An integer literal value.
class IntegerValue(metaclass=_IntegerValueMeta):
    r"""IntegerValueBigint | IntegerValueInt8 | IntegerValueInt16 | IntegerValueInt32 | IntegerValueInt64 | IntegerValueUint8 | IntegerValueUint16 | IntegerValueUint32 | IntegerValueUint64"""

    TYPE_ = Name("hydra.core.IntegerValue")
    BIGINT = Name("bigint")
    INT8 = Name("int8")
    INT16 = Name("int16")
    INT32 = Name("int32")
    INT64 = Name("int64")
    UINT8 = Name("uint8")
    UINT16 = Name("uint16")
    UINT32 = Name("uint32")
    UINT64 = Name("uint64")

@dataclass(frozen=True)
class Lambda:
    r"""A function abstraction (lambda)."""

    parameter: Annotated[Name, "The parameter of the lambda"]
    domain: Annotated[Maybe[Type], "An optional domain type for the lambda"]
    body: Annotated[Term, "The body of the lambda"]

    TYPE_ = Name("hydra.core.Lambda")
    PARAMETER = Name("parameter")
    DOMAIN = Name("domain")
    BODY = Name("body")

@dataclass(frozen=True)
class Let:
    r"""A set of (possibly recursive) 'let' bindings together with a body in which they are bound."""

    bindings: Annotated[frozenlist[Binding], "The list of variable bindings"]
    body: Annotated[Term, "The body term in which the variables are bound"]

    TYPE_ = Name("hydra.core.Let")
    BINDINGS = Name("bindings")
    BODY = Name("body")

class LiteralBinary(Node[bytes]):
    r"""A binary literal"""

class LiteralBoolean(Node[bool]):
    r"""A boolean literal"""

class LiteralFloat(Node["FloatValue"]):
    r"""A floating-point literal"""

class LiteralInteger(Node["IntegerValue"]):
    r"""An integer literal"""

class LiteralString(Node[str]):
    r"""A string literal"""

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A term constant; an instance of a literal type.
class Literal(metaclass=_LiteralMeta):
    r"""LiteralBinary | LiteralBoolean | LiteralFloat | LiteralInteger | LiteralString"""

    TYPE_ = Name("hydra.core.Literal")
    BINARY = Name("binary")
    BOOLEAN = Name("boolean")
    FLOAT = Name("float")
    INTEGER = Name("integer")
    STRING = Name("string")

class LiteralTypeBinary:
    r"""The type of a binary (byte string) value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralTypeBinary)
    def __hash__(self):
        return hash("LiteralTypeBinary")

class LiteralTypeBoolean:
    r"""The type of a boolean (true/false) value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralTypeBoolean)
    def __hash__(self):
        return hash("LiteralTypeBoolean")

class LiteralTypeFloat(Node["FloatType"]):
    r"""The type of a floating-point value"""

class LiteralTypeInteger(Node["IntegerType"]):
    r"""The type of an integer value"""

class LiteralTypeString:
    r"""The type of a string value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralTypeString)
    def __hash__(self):
        return hash("LiteralTypeString")

class _LiteralTypeMeta(type):
    def __getitem__(cls, item):
        return object

# Any of a fixed set of literal types, also called atomic types, base types, primitive types, or type constants.
class LiteralType(metaclass=_LiteralTypeMeta):
    r"""LiteralTypeBinary | LiteralTypeBoolean | LiteralTypeFloat | LiteralTypeInteger | LiteralTypeString"""

    TYPE_ = Name("hydra.core.LiteralType")
    BINARY = Name("binary")
    BOOLEAN = Name("boolean")
    FLOAT = Name("float")
    INTEGER = Name("integer")
    STRING = Name("string")

@dataclass(frozen=True)
class MapType:
    r"""A map type."""

    keys: Annotated[Type, "The type of keys in the map"]
    values: Annotated[Type, "The type of values in the map"]

    TYPE_ = Name("hydra.core.MapType")
    KEYS = Name("keys")
    VALUES = Name("values")

@dataclass(frozen=True)
class Projection:
    r"""A record elimination; a projection."""

    type_name: Annotated[Name, "The name of the record type"]
    field: Annotated[Name, "The name of the projected field"]

    TYPE_ = Name("hydra.core.Projection")
    TYPE_NAME = Name("typeName")
    FIELD = Name("field")

@dataclass(frozen=True)
class Record:
    r"""A record, or labeled tuple; a map of field names to terms."""

    type_name: Annotated[Name, "The name of the record type"]
    fields: Annotated[frozenlist[Field], "The fields of the record, as a list of name/term pairs"]

    TYPE_ = Name("hydra.core.Record")
    TYPE_NAME = Name("typeName")
    FIELDS = Name("fields")

class TermAnnotated(Node["AnnotatedTerm"]):
    r"""A term annotated with metadata"""

class TermApplication(Node["Application"]):
    r"""A function application"""

class TermEither(Node["Either[Term, Term]"]):
    r"""An either value"""

class TermFunction(Node["Function"]):
    r"""A function term"""

class TermLet(Node["Let"]):
    r"""A 'let' term, which binds variables to terms"""

class TermList(Node["frozenlist[Term]"]):
    r"""A list"""

class TermLiteral(Node["Literal"]):
    r"""A literal value"""

class TermMap(Node["FrozenDict[Term, Term]"]):
    r"""A map of keys to values"""

class TermMaybe(Node["Maybe[Term]"]):
    r"""An optional value"""

class TermPair(Node["tuple[Term, Term]"]):
    r"""A pair (2-tuple)"""

class TermRecord(Node["Record"]):
    r"""A record term"""

class TermSet(Node["frozenset[Term]"]):
    r"""A set of values"""

class TermTypeApplication(Node["TypeApplicationTerm"]):
    r"""A System F type application term"""

class TermTypeLambda(Node["TypeLambda"]):
    r"""A System F type abstraction term"""

class TermUnion(Node["Injection"]):
    r"""An injection; an instance of a union type"""

class TermUnit:
    r"""A unit value; a term with no value"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TermUnit)
    def __hash__(self):
        return hash("TermUnit")

class TermVariable(Node["Name"]):
    r"""A variable reference"""

class TermWrap(Node["WrappedTerm"]):
    r"""A wrapped term; an instance of a wrapper type (newtype)"""

class _TermMeta(type):
    def __getitem__(cls, item):
        return object

# A data term.
class Term(metaclass=_TermMeta):
    r"""TermAnnotated | TermApplication | TermEither | TermFunction | TermLet | TermList | TermLiteral | TermMap | TermMaybe | TermPair | TermRecord | TermSet | TermTypeApplication | TermTypeLambda | TermUnion | TermUnit | TermVariable | TermWrap"""

    TYPE_ = Name("hydra.core.Term")
    ANNOTATED = Name("annotated")
    APPLICATION = Name("application")
    EITHER = Name("either")
    FUNCTION = Name("function")
    LET = Name("let")
    LIST = Name("list")
    LITERAL = Name("literal")
    MAP = Name("map")
    MAYBE = Name("maybe")
    PAIR = Name("pair")
    RECORD = Name("record")
    SET = Name("set")
    TYPE_APPLICATION = Name("typeApplication")
    TYPE_LAMBDA = Name("typeLambda")
    UNION = Name("union")
    UNIT = Name("unit")
    VARIABLE = Name("variable")
    WRAP = Name("wrap")

class TypeAnnotated(Node["AnnotatedType"]):
    r"""An annotated type"""

class TypeApplication(Node["ApplicationType"]):
    r"""A type application"""

class TypeEither(Node["EitherType"]):
    r"""An either (sum) type"""

class TypeForall(Node["ForallType"]):
    r"""A universally quantified (polymorphic) type"""

class TypeFunction(Node["FunctionType"]):
    r"""A function type"""

class TypeList(Node["Type"]):
    r"""A list type"""

class TypeLiteral(Node["LiteralType"]):
    r"""A literal type"""

class TypeMap(Node["MapType"]):
    r"""A map type"""

class TypeMaybe(Node["Type"]):
    r"""An optional type"""

class TypePair(Node["PairType"]):
    r"""A pair (2-tuple) type"""

class TypeRecord(Node["frozenlist[FieldType]"]):
    r"""A record type"""

class TypeSet(Node["Type"]):
    r"""A set type"""

class TypeUnion(Node["frozenlist[FieldType]"]):
    r"""A union type with field names"""

class TypeUnit:
    r"""The unit type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeUnit)
    def __hash__(self):
        return hash("TypeUnit")

class TypeVariable(Node["Name"]):
    r"""A type variable"""

class TypeVoid:
    r"""The void (uninhabited, or bottom) type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeVoid)
    def __hash__(self):
        return hash("TypeVoid")

class TypeWrap(Node["Type"]):
    r"""A wrapped type (newtype)"""

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

# A data type.
class Type(metaclass=_TypeMeta):
    r"""TypeAnnotated | TypeApplication | TypeEither | TypeForall | TypeFunction | TypeList | TypeLiteral | TypeMap | TypeMaybe | TypePair | TypeRecord | TypeSet | TypeUnion | TypeUnit | TypeVariable | TypeVoid | TypeWrap"""

    TYPE_ = Name("hydra.core.Type")
    ANNOTATED = Name("annotated")
    APPLICATION = Name("application")
    EITHER = Name("either")
    FORALL = Name("forall")
    FUNCTION = Name("function")
    LIST = Name("list")
    LITERAL = Name("literal")
    MAP = Name("map")
    MAYBE = Name("maybe")
    PAIR = Name("pair")
    RECORD = Name("record")
    SET = Name("set")
    UNION = Name("union")
    UNIT = Name("unit")
    VARIABLE = Name("variable")
    VOID = Name("void")
    WRAP = Name("wrap")

@dataclass(frozen=True)
class TypeApplicationTerm:
    r"""A term applied to a type; a type application."""

    body: Annotated[Term, "The term being applied to a type"]
    type: Annotated[Type, "The type argument"]

    TYPE_ = Name("hydra.core.TypeApplicationTerm")
    BODY = Name("body")
    TYPE = Name("type")

@dataclass(frozen=True)
class TypeLambda:
    r"""A System F type abstraction term."""

    parameter: Annotated[Name, "The type variable introduced by the abstraction"]
    body: Annotated[Term, "The body of the abstraction"]

    TYPE_ = Name("hydra.core.TypeLambda")
    PARAMETER = Name("parameter")
    BODY = Name("body")

@dataclass(frozen=True)
class TypeScheme:
    r"""A type expression together with free type variables occurring in the expression."""

    variables: Annotated[frozenlist[Name], "The free type variables"]
    type: Annotated[Type, "The type expression"]
    constraints: Annotated[Maybe[FrozenDict[Name, TypeVariableMetadata]], "Optional metadata for type variables, including typeclass constraints. The map keys are type variable names."]

    TYPE_ = Name("hydra.core.TypeScheme")
    VARIABLES = Name("variables")
    TYPE = Name("type")
    CONSTRAINTS = Name("constraints")

@dataclass(frozen=True)
class TypeVariableMetadata:
    r"""Metadata associated with a type variable, including typeclass constraints."""

    classes: Annotated[frozenset[Name], "The set of typeclass constraints on this type variable"]

    TYPE_ = Name("hydra.core.TypeVariableMetadata")
    CLASSES = Name("classes")

@dataclass(frozen=True)
class WrappedTerm:
    r"""A term wrapped in a type name."""

    type_name: Annotated[Name, "The name of the wrapper type"]
    body: Annotated[Term, "The wrapped term"]

    TYPE_ = Name("hydra.core.WrappedTerm")
    TYPE_NAME = Name("typeName")
    BODY = Name("body")
