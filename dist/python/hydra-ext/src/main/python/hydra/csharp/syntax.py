# Note: this is an automatically generated file. Do not edit.

r"""A C# syntax module based on the ANTLR grammar dated 02/07/2024 and available at:
  https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class Identifier(Node[str]):
    ...

Identifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.Identifier")

class Keyword(Node[str]):
    ...

Keyword.TYPE_ = hydra.core.Name("hydra.csharp.syntax.Keyword")

class LiteralBoolean(Node[bool]):
    ...

class LiteralInteger(Node["IntegerLiteral"]):
    ...

class LiteralReal(Node[Decimal]):
    ...

class LiteralCharacter(Node[str]):
    ...

class LiteralString(Node[str]):
    ...

class LiteralNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralNull)
    def __hash__(self):
        return hash("LiteralNull")

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

class Literal(metaclass=_LiteralMeta):
    r"""LiteralBoolean | LiteralInteger | LiteralReal | LiteralCharacter | LiteralString | LiteralNull"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Literal")
    BOOLEAN = hydra.core.Name("boolean")
    INTEGER = hydra.core.Name("integer")
    REAL = hydra.core.Name("real")
    CHARACTER = hydra.core.Name("character")
    STRING = hydra.core.Name("string")
    NULL = hydra.core.Name("null")

class IntegerLiteralDecimal(Node[str]):
    ...

class IntegerLiteralHexadecimal(Node[str]):
    ...

class IntegerLiteralBinary(Node[int]):
    ...

class _IntegerLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class IntegerLiteral(metaclass=_IntegerLiteralMeta):
    r"""IntegerLiteralDecimal | IntegerLiteralHexadecimal | IntegerLiteralBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IntegerLiteral")
    DECIMAL = hydra.core.Name("decimal")
    HEXADECIMAL = hydra.core.Name("hexadecimal")
    BINARY = hydra.core.Name("binary")

class NamespaceName(Node["NamespaceOrTypeName"]):
    ...

NamespaceName.TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamespaceName")

class TypeName(Node["NamespaceOrTypeName"]):
    ...

TypeName.TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeName")

class NamespaceOrTypeNameIdentifier(Node["IdentifierNamespaceOrTypeName"]):
    ...

class NamespaceOrTypeNameQualified(Node["QualifiedNamespaceOrTypeName"]):
    ...

class NamespaceOrTypeNameAlias(Node["QualifiedAliasMember"]):
    ...

class _NamespaceOrTypeNameMeta(type):
    def __getitem__(cls, item):
        return object

class NamespaceOrTypeName(metaclass=_NamespaceOrTypeNameMeta):
    r"""NamespaceOrTypeNameIdentifier | NamespaceOrTypeNameQualified | NamespaceOrTypeNameAlias"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamespaceOrTypeName")
    IDENTIFIER = hydra.core.Name("identifier")
    QUALIFIED = hydra.core.Name("qualified")
    ALIAS = hydra.core.Name("alias")

@dataclass(frozen=True)
class IdentifierNamespaceOrTypeName:
    identifier: Identifier
    arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IdentifierNamespaceOrTypeName")
    IDENTIFIER = hydra.core.Name("identifier")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class QualifiedNamespaceOrTypeName:
    namespace_or_type: NamespaceOrTypeName
    identifier: Identifier
    arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.QualifiedNamespaceOrTypeName")
    NAMESPACE_OR_TYPE = hydra.core.Name("namespaceOrType")
    IDENTIFIER = hydra.core.Name("identifier")
    ARGUMENTS = hydra.core.Name("arguments")

class TypeReference(Node["ReferenceType"]):
    ...

class TypeValue(Node["ValueType"]):
    ...

class TypeParam(Node["TypeParameter"]):
    ...

class TypePointer(Node["PointerType"]):
    ...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypeReference | TypeValue | TypeParam | TypePointer"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Type")
    REFERENCE = hydra.core.Name("reference")
    VALUE = hydra.core.Name("value")
    PARAM = hydra.core.Name("param")
    POINTER = hydra.core.Name("pointer")

class ReferenceTypeClass(Node["ClassType"]):
    ...

class ReferenceTypeInterface(Node["InterfaceType"]):
    ...

class ReferenceTypeArray(Node["ArrayType"]):
    ...

class ReferenceTypeDelegate(Node["DelegateType"]):
    ...

class ReferenceTypeDynamic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ReferenceTypeDynamic)
    def __hash__(self):
        return hash("ReferenceTypeDynamic")

class _ReferenceTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ReferenceType(metaclass=_ReferenceTypeMeta):
    r"""ReferenceTypeClass | ReferenceTypeInterface | ReferenceTypeArray | ReferenceTypeDelegate | ReferenceTypeDynamic"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ReferenceType")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")
    ARRAY = hydra.core.Name("array")
    DELEGATE = hydra.core.Name("delegate")
    DYNAMIC = hydra.core.Name("dynamic")

class ClassTypeTypeName(Node["TypeName"]):
    ...

class ClassTypeObject:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassTypeObject)
    def __hash__(self):
        return hash("ClassTypeObject")

class ClassTypeString:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassTypeString)
    def __hash__(self):
        return hash("ClassTypeString")

class _ClassTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ClassType(metaclass=_ClassTypeMeta):
    r"""ClassTypeTypeName | ClassTypeObject | ClassTypeString"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ClassType")
    TYPE_NAME = hydra.core.Name("typeName")
    OBJECT = hydra.core.Name("object")
    STRING = hydra.core.Name("string")

class InterfaceType(Node["TypeName"]):
    ...

InterfaceType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceType")

@dataclass(frozen=True)
class ArrayType:
    type: NonArrayType
    rank: frozenlist[RankSpecifier]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ArrayType")
    TYPE = hydra.core.Name("type")
    RANK = hydra.core.Name("rank")

class NonArrayTypeValue(Node["ValueType"]):
    ...

class NonArrayTypeClass(Node["ClassType"]):
    ...

class NonArrayTypeInterface(Node["InterfaceType"]):
    ...

class NonArrayTypeDelegate(Node["DelegateType"]):
    ...

class NonArrayTypeDynamic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NonArrayTypeDynamic)
    def __hash__(self):
        return hash("NonArrayTypeDynamic")

class NonArrayTypeParameter(Node["TypeParameter"]):
    ...

class NonArrayTypePointer(Node["PointerType"]):
    ...

class _NonArrayTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NonArrayType(metaclass=_NonArrayTypeMeta):
    r"""NonArrayTypeValue | NonArrayTypeClass | NonArrayTypeInterface | NonArrayTypeDelegate | NonArrayTypeDynamic | NonArrayTypeParameter | NonArrayTypePointer"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NonArrayType")
    VALUE = hydra.core.Name("value")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")
    DELEGATE = hydra.core.Name("delegate")
    DYNAMIC = hydra.core.Name("dynamic")
    PARAMETER = hydra.core.Name("parameter")
    POINTER = hydra.core.Name("pointer")

class RankSpecifier(Node[int]):
    ...

RankSpecifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.RankSpecifier")

class DelegateType(Node["TypeName"]):
    ...

DelegateType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.DelegateType")

class ValueTypeNonNullable(Node["StructOrEnumType"]):
    ...

class ValueTypeNullable(Node["StructOrEnumType"]):
    ...

class _ValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ValueType(metaclass=_ValueTypeMeta):
    r"""ValueTypeNonNullable | ValueTypeNullable"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ValueType")
    NON_NULLABLE = hydra.core.Name("nonNullable")
    NULLABLE = hydra.core.Name("nullable")

class StructOrEnumTypeStruct(Node["StructType"]):
    ...

class StructOrEnumTypeEnum(Node["EnumType"]):
    ...

class _StructOrEnumTypeMeta(type):
    def __getitem__(cls, item):
        return object

class StructOrEnumType(metaclass=_StructOrEnumTypeMeta):
    r"""StructOrEnumTypeStruct | StructOrEnumTypeEnum"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StructOrEnumType")
    STRUCT = hydra.core.Name("struct")
    ENUM = hydra.core.Name("enum")

class StructTypeTypeName(Node["TypeName"]):
    ...

class StructTypeSimple(Node["SimpleType"]):
    ...

class StructTypeTuple(Node["TupleType"]):
    ...

class _StructTypeMeta(type):
    def __getitem__(cls, item):
        return object

class StructType(metaclass=_StructTypeMeta):
    r"""StructTypeTypeName | StructTypeSimple | StructTypeTuple"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StructType")
    TYPE_NAME = hydra.core.Name("typeName")
    SIMPLE = hydra.core.Name("simple")
    TUPLE = hydra.core.Name("tuple")

class SimpleTypeNumeric(Node["NumericType"]):
    ...

class SimpleTypeBool:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SimpleTypeBool)
    def __hash__(self):
        return hash("SimpleTypeBool")

class _SimpleTypeMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleType(metaclass=_SimpleTypeMeta):
    r"""SimpleTypeNumeric | SimpleTypeBool"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SimpleType")
    NUMERIC = hydra.core.Name("numeric")
    BOOL = hydra.core.Name("bool")

class NumericTypeIntegral(Node["IntegralType"]):
    ...

class NumericTypeFloatingPoint(Node["FloatingPointType"]):
    ...

class NumericTypeDecimal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NumericTypeDecimal)
    def __hash__(self):
        return hash("NumericTypeDecimal")

class _NumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NumericType(metaclass=_NumericTypeMeta):
    r"""NumericTypeIntegral | NumericTypeFloatingPoint | NumericTypeDecimal"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NumericType")
    INTEGRAL = hydra.core.Name("integral")
    FLOATING_POINT = hydra.core.Name("floatingPoint")
    DECIMAL = hydra.core.Name("decimal")

class IntegralType(Enum):
    SBYTE = hydra.core.Name("sbyte")

    BYTE = hydra.core.Name("byte")

    SHORT = hydra.core.Name("short")

    USHORT = hydra.core.Name("ushort")

    INT = hydra.core.Name("int")

    UINT = hydra.core.Name("uint")

    LONG = hydra.core.Name("long")

    ULONG = hydra.core.Name("ulong")

    CHAR = hydra.core.Name("char")

IntegralType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.IntegralType")

class FloatingPointType(Enum):
    FLOAT = hydra.core.Name("float")

    DOUBLE = hydra.core.Name("double")

FloatingPointType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.FloatingPointType")

class TupleType(Node["frozenlist[TupleTypeElement]"]):
    ...

TupleType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.TupleType")

@dataclass(frozen=True)
class TupleTypeElement:
    type: Type
    identifier: Maybe[Identifier]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TupleTypeElement")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")

class EnumType(Node["TypeName"]):
    ...

EnumType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.EnumType")

class TypeArgumentList(Node["frozenlist[Type]"]):
    ...

TypeArgumentList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeArgumentList")

class TypeParameter(Node["Identifier"]):
    ...

TypeParameter.TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeParameter")

class UnmanagedTypeValue(Node["ValueType"]):
    ...

class UnmanagedTypePointer(Node["PointerType"]):
    ...

class _UnmanagedTypeMeta(type):
    def __getitem__(cls, item):
        return object

class UnmanagedType(metaclass=_UnmanagedTypeMeta):
    r"""UnmanagedTypeValue | UnmanagedTypePointer"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UnmanagedType")
    VALUE = hydra.core.Name("value")
    POINTER = hydra.core.Name("pointer")

class VariableReference(Node["Expression"]):
    ...

VariableReference.TYPE_ = hydra.core.Name("hydra.csharp.syntax.VariableReference")

class PatternDeclaration(Node["DeclarationPattern"]):
    ...

class PatternConstant(Node["Expression"]):
    ...

class PatternVar(Node["Designation"]):
    ...

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

class Pattern(metaclass=_PatternMeta):
    r"""PatternDeclaration | PatternConstant | PatternVar"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Pattern")
    DECLARATION = hydra.core.Name("declaration")
    CONSTANT = hydra.core.Name("constant")
    VAR = hydra.core.Name("var")

@dataclass(frozen=True)
class DeclarationPattern:
    type: Type
    designation: Designation

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DeclarationPattern")
    TYPE = hydra.core.Name("type")
    DESIGNATION = hydra.core.Name("designation")

class Designation(Node["Identifier"]):
    ...

Designation.TYPE_ = hydra.core.Name("hydra.csharp.syntax.Designation")

class ArgumentList(Node["frozenlist[Argument]"]):
    ...

ArgumentList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ArgumentList")

@dataclass(frozen=True)
class Argument:
    name: Maybe[Identifier]
    value: ArgumentValue

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Argument")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class ArgumentValueExpression(Node["Expression"]):
    ...

class ArgumentValueIn(Node["VariableReference"]):
    ...

class ArgumentValueRef(Node["VariableReference"]):
    ...

class ArgumentValueOut(Node["VariableReference"]):
    ...

class _ArgumentValueMeta(type):
    def __getitem__(cls, item):
        return object

class ArgumentValue(metaclass=_ArgumentValueMeta):
    r"""ArgumentValueExpression | ArgumentValueIn | ArgumentValueRef | ArgumentValueOut"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ArgumentValue")
    EXPRESSION = hydra.core.Name("expression")
    IN = hydra.core.Name("in")
    REF = hydra.core.Name("ref")
    OUT = hydra.core.Name("out")

class PrimaryExpressionNoArray(Node["PrimaryNoArrayCreationExpression"]):
    ...

class PrimaryExpressionArray(Node["ArrayCreationExpression"]):
    ...

class _PrimaryExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryExpression(metaclass=_PrimaryExpressionMeta):
    r"""PrimaryExpressionNoArray | PrimaryExpressionArray"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PrimaryExpression")
    NO_ARRAY = hydra.core.Name("noArray")
    ARRAY = hydra.core.Name("array")

class PrimaryNoArrayCreationExpressionLiteral(Node["Literal"]):
    ...

class PrimaryNoArrayCreationExpressionInterpolatedString(Node["InterpolatedStringExpression"]):
    ...

class PrimaryNoArrayCreationExpressionSimpleName(Node["SimpleName"]):
    ...

class PrimaryNoArrayCreationExpressionParenthesized(Node["Expression"]):
    ...

class PrimaryNoArrayCreationExpressionTuple(Node["TupleExpression"]):
    ...

class PrimaryNoArrayCreationExpressionMemberAccess(Node["MemberAccess"]):
    ...

class PrimaryNoArrayCreationExpressionNullConditionalMemberAccess(Node["NullConditionalMemberAccess"]):
    ...

class PrimaryNoArrayCreationExpressionInvocation(Node["InvocationExpression"]):
    ...

class PrimaryNoArrayCreationExpressionElementAccess(Node["ElementAccess"]):
    ...

class PrimaryNoArrayCreationExpressionNullConditionalElementAccess(Node["NullConditionalElementAccess"]):
    ...

class PrimaryNoArrayCreationExpressionThisAccess:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimaryNoArrayCreationExpressionThisAccess)
    def __hash__(self):
        return hash("PrimaryNoArrayCreationExpressionThisAccess")

class PrimaryNoArrayCreationExpressionBaseAccess(Node["BaseAccess"]):
    ...

class PrimaryNoArrayCreationExpressionPostIncrement(Node["PrimaryExpression"]):
    ...

class PrimaryNoArrayCreationExpressionPostDecrement(Node["PrimaryExpression"]):
    ...

class PrimaryNoArrayCreationExpressionObjectCreation(Node["ObjectCreationExpression"]):
    ...

class PrimaryNoArrayCreationExpressionDelegateCreation(Node["DelegateCreationExpression"]):
    ...

class PrimaryNoArrayCreationExpressionAnonymousObjectCreation(Node["Maybe[MemberDeclaratorList]"]):
    ...

class PrimaryNoArrayCreationExpressionTypeof(Node["TypeofExpression"]):
    ...

class PrimaryNoArrayCreationExpressionSizeof(Node["UnmanagedType"]):
    ...

class PrimaryNoArrayCreationExpressionChecked(Node["Expression"]):
    ...

class PrimaryNoArrayCreationExpressionUnchecked(Node["Expression"]):
    ...

class PrimaryNoArrayCreationExpressionDefaultValue(Node["DefaultValueExpression"]):
    ...

class PrimaryNoArrayCreationExpressionNameof(Node["NamedEntity"]):
    ...

class PrimaryNoArrayCreationExpressionAnonymousMethod(Node["AnonymousMethodExpression"]):
    ...

class PrimaryNoArrayCreationExpressionPointerMemberAccess(Node["PointerMemberAccess"]):
    ...

class PrimaryNoArrayCreationExpressionPointerElementAccess(Node["PointerElementAccess"]):
    ...

class PrimaryNoArrayCreationExpressionStackalloc(Node["StackallocExpression"]):
    ...

class _PrimaryNoArrayCreationExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryNoArrayCreationExpression(metaclass=_PrimaryNoArrayCreationExpressionMeta):
    r"""PrimaryNoArrayCreationExpressionLiteral | PrimaryNoArrayCreationExpressionInterpolatedString | PrimaryNoArrayCreationExpressionSimpleName | PrimaryNoArrayCreationExpressionParenthesized | PrimaryNoArrayCreationExpressionTuple | PrimaryNoArrayCreationExpressionMemberAccess | PrimaryNoArrayCreationExpressionNullConditionalMemberAccess | PrimaryNoArrayCreationExpressionInvocation | PrimaryNoArrayCreationExpressionElementAccess | PrimaryNoArrayCreationExpressionNullConditionalElementAccess | PrimaryNoArrayCreationExpressionThisAccess | PrimaryNoArrayCreationExpressionBaseAccess | PrimaryNoArrayCreationExpressionPostIncrement | PrimaryNoArrayCreationExpressionPostDecrement | PrimaryNoArrayCreationExpressionObjectCreation | PrimaryNoArrayCreationExpressionDelegateCreation | PrimaryNoArrayCreationExpressionAnonymousObjectCreation | PrimaryNoArrayCreationExpressionTypeof | PrimaryNoArrayCreationExpressionSizeof | PrimaryNoArrayCreationExpressionChecked | PrimaryNoArrayCreationExpressionUnchecked | PrimaryNoArrayCreationExpressionDefaultValue | PrimaryNoArrayCreationExpressionNameof | PrimaryNoArrayCreationExpressionAnonymousMethod | PrimaryNoArrayCreationExpressionPointerMemberAccess | PrimaryNoArrayCreationExpressionPointerElementAccess | PrimaryNoArrayCreationExpressionStackalloc"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PrimaryNoArrayCreationExpression")
    LITERAL = hydra.core.Name("literal")
    INTERPOLATED_STRING = hydra.core.Name("interpolatedString")
    SIMPLE_NAME = hydra.core.Name("simpleName")
    PARENTHESIZED = hydra.core.Name("parenthesized")
    TUPLE = hydra.core.Name("tuple")
    MEMBER_ACCESS = hydra.core.Name("memberAccess")
    NULL_CONDITIONAL_MEMBER_ACCESS = hydra.core.Name("nullConditionalMemberAccess")
    INVOCATION = hydra.core.Name("invocation")
    ELEMENT_ACCESS = hydra.core.Name("elementAccess")
    NULL_CONDITIONAL_ELEMENT_ACCESS = hydra.core.Name("nullConditionalElementAccess")
    THIS_ACCESS = hydra.core.Name("thisAccess")
    BASE_ACCESS = hydra.core.Name("baseAccess")
    POST_INCREMENT = hydra.core.Name("postIncrement")
    POST_DECREMENT = hydra.core.Name("postDecrement")
    OBJECT_CREATION = hydra.core.Name("objectCreation")
    DELEGATE_CREATION = hydra.core.Name("delegateCreation")
    ANONYMOUS_OBJECT_CREATION = hydra.core.Name("anonymousObjectCreation")
    TYPEOF = hydra.core.Name("typeof")
    SIZEOF = hydra.core.Name("sizeof")
    CHECKED = hydra.core.Name("checked")
    UNCHECKED = hydra.core.Name("unchecked")
    DEFAULT_VALUE = hydra.core.Name("defaultValue")
    NAMEOF = hydra.core.Name("nameof")
    ANONYMOUS_METHOD = hydra.core.Name("anonymousMethod")
    POINTER_MEMBER_ACCESS = hydra.core.Name("pointerMemberAccess")
    POINTER_ELEMENT_ACCESS = hydra.core.Name("pointerElementAccess")
    STACKALLOC = hydra.core.Name("stackalloc")

class InterpolatedStringExpressionRegular(Node["InterpolatedRegularStringExpression"]):
    ...

class InterpolatedStringExpressionVerbatim(Node["InterpolatedVerbatimStringExpression"]):
    ...

class _InterpolatedStringExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class InterpolatedStringExpression(metaclass=_InterpolatedStringExpressionMeta):
    r"""InterpolatedStringExpressionRegular | InterpolatedStringExpressionVerbatim"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterpolatedStringExpression")
    REGULAR = hydra.core.Name("regular")
    VERBATIM = hydra.core.Name("verbatim")

class InterpolatedRegularStringExpression(Node[str]):
    ...

InterpolatedRegularStringExpression.TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterpolatedRegularStringExpression")

@dataclass(frozen=True)
class RegularInterpolation:
    expression: Expression
    width: Maybe[Expression]
    format: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RegularInterpolation")
    EXPRESSION = hydra.core.Name("expression")
    WIDTH = hydra.core.Name("width")
    FORMAT = hydra.core.Name("format")

class InterpolatedVerbatimStringExpression(Node[str]):
    ...

InterpolatedVerbatimStringExpression.TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterpolatedVerbatimStringExpression")

@dataclass(frozen=True)
class VerbatimInterpolation:
    expression: Expression
    width: Maybe[ConstantExpression]
    format: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.VerbatimInterpolation")
    EXPRESSION = hydra.core.Name("expression")
    WIDTH = hydra.core.Name("width")
    FORMAT = hydra.core.Name("format")

@dataclass(frozen=True)
class SimpleName:
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SimpleName")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

class TupleExpressionElements(Node["frozenlist[TupleElement]"]):
    ...

class TupleExpressionDeconstruction(Node["DeconstructionTuple"]):
    ...

class _TupleExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class TupleExpression(metaclass=_TupleExpressionMeta):
    r"""TupleExpressionElements | TupleExpressionDeconstruction"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TupleExpression")
    ELEMENTS = hydra.core.Name("elements")
    DECONSTRUCTION = hydra.core.Name("deconstruction")

@dataclass(frozen=True)
class TupleElement:
    name: Maybe[Identifier]
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TupleElement")
    NAME = hydra.core.Name("name")
    EXPRESSION = hydra.core.Name("expression")

class DeconstructionTuple(Node["frozenlist[DeconstructionElement]"]):
    ...

DeconstructionTuple.TYPE_ = hydra.core.Name("hydra.csharp.syntax.DeconstructionTuple")

class DeconstructionElementTuple(Node["DeconstructionTuple"]):
    ...

class DeconstructionElementIdentifier(Node["Identifier"]):
    ...

class _DeconstructionElementMeta(type):
    def __getitem__(cls, item):
        return object

class DeconstructionElement(metaclass=_DeconstructionElementMeta):
    r"""DeconstructionElementTuple | DeconstructionElementIdentifier"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DeconstructionElement")
    TUPLE = hydra.core.Name("tuple")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MemberAccess:
    head: MemberAccessHead
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MemberAccess")
    HEAD = hydra.core.Name("head")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

class MemberAccessHeadPrimary(Node["PrimaryExpression"]):
    ...

class MemberAccessHeadPredefined(Node["PredefinedType"]):
    ...

class MemberAccessHeadQualifiedAlias(Node["QualifiedAliasMember"]):
    ...

class _MemberAccessHeadMeta(type):
    def __getitem__(cls, item):
        return object

class MemberAccessHead(metaclass=_MemberAccessHeadMeta):
    r"""MemberAccessHeadPrimary | MemberAccessHeadPredefined | MemberAccessHeadQualifiedAlias"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MemberAccessHead")
    PRIMARY = hydra.core.Name("primary")
    PREDEFINED = hydra.core.Name("predefined")
    QUALIFIED_ALIAS = hydra.core.Name("qualifiedAlias")

class PredefinedType(Enum):
    BOOL = hydra.core.Name("bool")

    BYTE = hydra.core.Name("byte")

    CHAR = hydra.core.Name("char")

    DECIMAL = hydra.core.Name("decimal")

    DOUBLE = hydra.core.Name("double")

    FLOAT = hydra.core.Name("float")

    INT = hydra.core.Name("int")

    LONG = hydra.core.Name("long")

    OBJECT = hydra.core.Name("object")

    SBYTE = hydra.core.Name("sbyte")

    SHORT = hydra.core.Name("short")

    STRING = hydra.core.Name("string")

    UINT = hydra.core.Name("uint")

    ULONG = hydra.core.Name("ulong")

    USHORT = hydra.core.Name("ushort")

PredefinedType.TYPE_ = hydra.core.Name("hydra.csharp.syntax.PredefinedType")

@dataclass(frozen=True)
class NullConditionalMemberAccess:
    expression: PrimaryExpression
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]
    dependent_access: frozenlist[DependentAccess]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NullConditionalMemberAccess")
    EXPRESSION = hydra.core.Name("expression")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    DEPENDENT_ACCESS = hydra.core.Name("dependentAccess")

class DependentAccessMemberAccess(Node["DependentAccessForMember"]):
    ...

class DependentAccessElementAccess(Node["ArgumentList"]):
    ...

class DependentAccessInvocation(Node["Maybe[ArgumentList]"]):
    ...

class _DependentAccessMeta(type):
    def __getitem__(cls, item):
        return object

class DependentAccess(metaclass=_DependentAccessMeta):
    r"""DependentAccessMemberAccess | DependentAccessElementAccess | DependentAccessInvocation"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DependentAccess")
    MEMBER_ACCESS = hydra.core.Name("memberAccess")
    ELEMENT_ACCESS = hydra.core.Name("elementAccess")
    INVOCATION = hydra.core.Name("invocation")

@dataclass(frozen=True)
class DependentAccessForMember:
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DependentAccessForMember")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class NullConditionalProjectionInitializer:
    expression: PrimaryExpression
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NullConditionalProjectionInitializer")
    EXPRESSION = hydra.core.Name("expression")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class InvocationExpression:
    expression: PrimaryExpression
    arguments: Maybe[ArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InvocationExpression")
    EXPRESSION = hydra.core.Name("expression")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class NullConditionalInvocationExpression:
    head: NullConditionalInvocationExpressionHead
    arguments: Maybe[ArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NullConditionalInvocationExpression")
    HEAD = hydra.core.Name("head")
    ARGUMENTS = hydra.core.Name("arguments")

class NullConditionalInvocationExpressionHeadMember(Node["NullConditionalMemberAccess"]):
    ...

class NullConditionalInvocationExpressionHeadElement(Node["NullConditionalElementAccess"]):
    ...

class _NullConditionalInvocationExpressionHeadMeta(type):
    def __getitem__(cls, item):
        return object

class NullConditionalInvocationExpressionHead(metaclass=_NullConditionalInvocationExpressionHeadMeta):
    r"""NullConditionalInvocationExpressionHeadMember | NullConditionalInvocationExpressionHeadElement"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NullConditionalInvocationExpressionHead")
    MEMBER = hydra.core.Name("member")
    ELEMENT = hydra.core.Name("element")

@dataclass(frozen=True)
class ElementAccess:
    expression: PrimaryNoArrayCreationExpression
    arguments: ArgumentList

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ElementAccess")
    EXPRESSION = hydra.core.Name("expression")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class NullConditionalElementAccess:
    expression: PrimaryNoArrayCreationExpression
    arguments: ArgumentList
    dependent_access: frozenlist[DependentAccess]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NullConditionalElementAccess")
    EXPRESSION = hydra.core.Name("expression")
    ARGUMENTS = hydra.core.Name("arguments")
    DEPENDENT_ACCESS = hydra.core.Name("dependentAccess")

class BaseAccessIdentifier(Node["BaseAccessWithIdentifier"]):
    ...

class BaseAccessArguments(Node["ArgumentList"]):
    ...

class _BaseAccessMeta(type):
    def __getitem__(cls, item):
        return object

class BaseAccess(metaclass=_BaseAccessMeta):
    r"""BaseAccessIdentifier | BaseAccessArguments"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BaseAccess")
    IDENTIFIER = hydra.core.Name("identifier")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class BaseAccessWithIdentifier:
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BaseAccessWithIdentifier")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class ObjectCreationExpression:
    type: Type
    arguments: Maybe[ArgumentList]
    initializer: Maybe[ObjectOrCollectionInitializer]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ObjectCreationExpression")
    TYPE = hydra.core.Name("type")
    ARGUMENTS = hydra.core.Name("arguments")
    INITIALIZER = hydra.core.Name("initializer")

class ObjectOrCollectionInitializerObject(Node["frozenlist[MemberInitializer]"]):
    ...

class ObjectOrCollectionInitializerCollection(Node["frozenlist[ElementInitializer]"]):
    ...

class _ObjectOrCollectionInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectOrCollectionInitializer(metaclass=_ObjectOrCollectionInitializerMeta):
    r"""ObjectOrCollectionInitializerObject | ObjectOrCollectionInitializerCollection"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ObjectOrCollectionInitializer")
    OBJECT = hydra.core.Name("object")
    COLLECTION = hydra.core.Name("collection")

@dataclass(frozen=True)
class MemberInitializer:
    target: InitializerTarget
    value: InitializerValue

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MemberInitializer")
    TARGET = hydra.core.Name("target")
    VALUE = hydra.core.Name("value")

class InitializerTargetIdentifier(Node["Identifier"]):
    ...

class InitializerTargetArguments(Node["ArgumentList"]):
    ...

class _InitializerTargetMeta(type):
    def __getitem__(cls, item):
        return object

class InitializerTarget(metaclass=_InitializerTargetMeta):
    r"""InitializerTargetIdentifier | InitializerTargetArguments"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InitializerTarget")
    IDENTIFIER = hydra.core.Name("identifier")
    ARGUMENTS = hydra.core.Name("arguments")

class InitializerValueExpression(Node["Expression"]):
    ...

class InitializerValueObjectOrCollection(Node["ObjectOrCollectionInitializer"]):
    ...

class _InitializerValueMeta(type):
    def __getitem__(cls, item):
        return object

class InitializerValue(metaclass=_InitializerValueMeta):
    r"""InitializerValueExpression | InitializerValueObjectOrCollection"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InitializerValue")
    EXPRESSION = hydra.core.Name("expression")
    OBJECT_OR_COLLECTION = hydra.core.Name("objectOrCollection")

class ElementInitializerSingle(Node["NonAssignmentExpression"]):
    ...

class ElementInitializerList(Node["ExpressionList"]):
    ...

class _ElementInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class ElementInitializer(metaclass=_ElementInitializerMeta):
    r"""ElementInitializerSingle | ElementInitializerList"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ElementInitializer")
    SINGLE = hydra.core.Name("single")
    LIST = hydra.core.Name("list")

class ExpressionList(Node["frozenlist[Expression]"]):
    ...

ExpressionList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExpressionList")

class ArrayCreationExpressionNonArrayType(Node["NonArrayTypeArrayCreationExpression"]):
    ...

class ArrayCreationExpressionArrayType(Node["ArrayTypeArrayCreationExpression"]):
    ...

class ArrayCreationExpressionRankSpecifier(Node["RankSpecifierArrayCreationExpression"]):
    ...

class _ArrayCreationExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayCreationExpression(metaclass=_ArrayCreationExpressionMeta):
    r"""ArrayCreationExpressionNonArrayType | ArrayCreationExpressionArrayType | ArrayCreationExpressionRankSpecifier"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ArrayCreationExpression")
    NON_ARRAY_TYPE = hydra.core.Name("nonArrayType")
    ARRAY_TYPE = hydra.core.Name("arrayType")
    RANK_SPECIFIER = hydra.core.Name("rankSpecifier")

@dataclass(frozen=True)
class NonArrayTypeArrayCreationExpression:
    type: NonArrayType
    expressions: ExpressionList
    rank_specifiers: frozenlist[RankSpecifier]
    initializer: Maybe[ArrayInitializer]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NonArrayTypeArrayCreationExpression")
    TYPE = hydra.core.Name("type")
    EXPRESSIONS = hydra.core.Name("expressions")
    RANK_SPECIFIERS = hydra.core.Name("rankSpecifiers")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class ArrayTypeArrayCreationExpression:
    type: ArrayType
    initializer: ArrayInitializer

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ArrayTypeArrayCreationExpression")
    TYPE = hydra.core.Name("type")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class RankSpecifierArrayCreationExpression:
    rank_specifier: RankSpecifier
    initializer: ArrayInitializer

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RankSpecifierArrayCreationExpression")
    RANK_SPECIFIER = hydra.core.Name("rankSpecifier")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class DelegateCreationExpression:
    type: DelegateType
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DelegateCreationExpression")
    TYPE = hydra.core.Name("type")
    EXPRESSION = hydra.core.Name("expression")

class MemberDeclaratorList(Node["frozenlist[MemberDeclarator]"]):
    ...

MemberDeclaratorList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.MemberDeclaratorList")

class MemberDeclaratorName(Node["SimpleName"]):
    ...

class MemberDeclaratorMemberAccess(Node["MemberAccess"]):
    ...

class MemberDeclaratorNullConditionalProjectionInitializer(Node["NullConditionalProjectionInitializer"]):
    ...

class MemberDeclaratorBaseAccess(Node["BaseAccess"]):
    ...

class MemberDeclaratorAssignment(Node["AssignmentMemberDeclarator"]):
    ...

class _MemberDeclaratorMeta(type):
    def __getitem__(cls, item):
        return object

class MemberDeclarator(metaclass=_MemberDeclaratorMeta):
    r"""MemberDeclaratorName | MemberDeclaratorMemberAccess | MemberDeclaratorNullConditionalProjectionInitializer | MemberDeclaratorBaseAccess | MemberDeclaratorAssignment"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MemberDeclarator")
    NAME = hydra.core.Name("name")
    MEMBER_ACCESS = hydra.core.Name("memberAccess")
    NULL_CONDITIONAL_PROJECTION_INITIALIZER = hydra.core.Name("nullConditionalProjectionInitializer")
    BASE_ACCESS = hydra.core.Name("baseAccess")
    ASSIGNMENT = hydra.core.Name("assignment")

@dataclass(frozen=True)
class AssignmentMemberDeclarator:
    identifier: Identifier
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AssignmentMemberDeclarator")
    IDENTIFIER = hydra.core.Name("identifier")
    EXPRESSION = hydra.core.Name("expression")

class TypeofExpressionType(Node["Type"]):
    ...

class TypeofExpressionUnboundTypeName(Node["UnboundTypeName"]):
    ...

class TypeofExpressionVoid:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeofExpressionVoid)
    def __hash__(self):
        return hash("TypeofExpressionVoid")

class _TypeofExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeofExpression(metaclass=_TypeofExpressionMeta):
    r"""TypeofExpressionType | TypeofExpressionUnboundTypeName | TypeofExpressionVoid"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeofExpression")
    TYPE = hydra.core.Name("type")
    UNBOUND_TYPE_NAME = hydra.core.Name("unboundTypeName")
    VOID = hydra.core.Name("void")

class UnboundTypeName(Node["frozenlist[UnboundTypeNamePart]"]):
    ...

UnboundTypeName.TYPE_ = hydra.core.Name("hydra.csharp.syntax.UnboundTypeName")

@dataclass(frozen=True)
class UnboundTypeNamePart:
    identifier: Identifier
    aliased: bool
    dimension: Maybe[int]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UnboundTypeNamePart")
    IDENTIFIER = hydra.core.Name("identifier")
    ALIASED = hydra.core.Name("aliased")
    DIMENSION = hydra.core.Name("dimension")

class DefaultValueExpressionExplicitlyTyped(Node["Type"]):
    ...

class DefaultValueExpressionDefaultLiteral:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, DefaultValueExpressionDefaultLiteral)
    def __hash__(self):
        return hash("DefaultValueExpressionDefaultLiteral")

class _DefaultValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class DefaultValueExpression(metaclass=_DefaultValueExpressionMeta):
    r"""DefaultValueExpressionExplicitlyTyped | DefaultValueExpressionDefaultLiteral"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DefaultValueExpression")
    EXPLICITLY_TYPED = hydra.core.Name("explicitlyTyped")
    DEFAULT_LITERAL = hydra.core.Name("defaultLiteral")

@dataclass(frozen=True)
class StackallocExpression:
    type: Maybe[UnmanagedType]
    expression: Maybe[ConstantExpression]
    initializer: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StackallocExpression")
    TYPE = hydra.core.Name("type")
    EXPRESSION = hydra.core.Name("expression")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class NamedEntity:
    target: NamedEntityTarget
    parts: frozenlist[NamedEntityPart]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamedEntity")
    TARGET = hydra.core.Name("target")
    PARTS = hydra.core.Name("parts")

@dataclass(frozen=True)
class NamedEntityPart:
    identifier: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamedEntityPart")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

class NamedEntityTargetName(Node["SimpleName"]):
    ...

class NamedEntityTargetThis:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NamedEntityTargetThis)
    def __hash__(self):
        return hash("NamedEntityTargetThis")

class NamedEntityTargetBase:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, NamedEntityTargetBase)
    def __hash__(self):
        return hash("NamedEntityTargetBase")

class NamedEntityTargetPredefinedType(Node["PredefinedType"]):
    ...

class NamedEntityTargetQualifiedAliasMember(Node["QualifiedAliasMember"]):
    ...

class _NamedEntityTargetMeta(type):
    def __getitem__(cls, item):
        return object

class NamedEntityTarget(metaclass=_NamedEntityTargetMeta):
    r"""NamedEntityTargetName | NamedEntityTargetThis | NamedEntityTargetBase | NamedEntityTargetPredefinedType | NamedEntityTargetQualifiedAliasMember"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamedEntityTarget")
    NAME = hydra.core.Name("name")
    THIS = hydra.core.Name("this")
    BASE = hydra.core.Name("base")
    PREDEFINED_TYPE = hydra.core.Name("predefinedType")
    QUALIFIED_ALIAS_MEMBER = hydra.core.Name("qualifiedAliasMember")

class UnaryExpressionPrimary(Node["PrimaryExpression"]):
    ...

class UnaryExpressionPlus(Node["UnaryExpression"]):
    ...

class UnaryExpressionMinus(Node["UnaryExpression"]):
    ...

class UnaryExpressionNot(Node["UnaryExpression"]):
    ...

class UnaryExpressionBitwiseComplement(Node["UnaryExpression"]):
    ...

class UnaryExpressionPreIncrement(Node["UnaryExpression"]):
    ...

class UnaryExpressionPreDecrement(Node["UnaryExpression"]):
    ...

class UnaryExpressionCast(Node["CastExpression"]):
    ...

class UnaryExpressionAwait(Node["UnaryExpression"]):
    ...

class UnaryExpressionPointerIndirection(Node["UnaryExpression"]):
    ...

class UnaryExpressionAddressOf(Node["UnaryExpression"]):
    ...

class _UnaryExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryExpression(metaclass=_UnaryExpressionMeta):
    r"""UnaryExpressionPrimary | UnaryExpressionPlus | UnaryExpressionMinus | UnaryExpressionNot | UnaryExpressionBitwiseComplement | UnaryExpressionPreIncrement | UnaryExpressionPreDecrement | UnaryExpressionCast | UnaryExpressionAwait | UnaryExpressionPointerIndirection | UnaryExpressionAddressOf"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UnaryExpression")
    PRIMARY = hydra.core.Name("primary")
    PLUS = hydra.core.Name("plus")
    MINUS = hydra.core.Name("minus")
    NOT = hydra.core.Name("not")
    BITWISE_COMPLEMENT = hydra.core.Name("bitwiseComplement")
    PRE_INCREMENT = hydra.core.Name("preIncrement")
    PRE_DECREMENT = hydra.core.Name("preDecrement")
    CAST = hydra.core.Name("cast")
    AWAIT = hydra.core.Name("await")
    POINTER_INDIRECTION = hydra.core.Name("pointerIndirection")
    ADDRESS_OF = hydra.core.Name("addressOf")

@dataclass(frozen=True)
class CastExpression:
    type: Type
    expression: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.CastExpression")
    TYPE = hydra.core.Name("type")
    EXPRESSION = hydra.core.Name("expression")

class MultiplicativeExpressionSimple(Node["UnaryExpression"]):
    ...

class MultiplicativeExpressionBinary(Node["BinaryMultiplicativeExpression"]):
    ...

class _MultiplicativeExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class MultiplicativeExpression(metaclass=_MultiplicativeExpressionMeta):
    r"""MultiplicativeExpressionSimple | MultiplicativeExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MultiplicativeExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryMultiplicativeExpression:
    left: MultiplicativeExpression
    operator: MultiplicativeOperator
    right: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryMultiplicativeExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class MultiplicativeOperator(Enum):
    TIMES = hydra.core.Name("times")

    DIVIDE = hydra.core.Name("divide")

    MODULO = hydra.core.Name("modulo")

MultiplicativeOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.MultiplicativeOperator")

class AdditiveExpressionSimple(Node["MultiplicativeExpression"]):
    ...

class AdditiveExpressionBinary(Node["BinaryAdditiveExpression"]):
    ...

class _AdditiveExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AdditiveExpression(metaclass=_AdditiveExpressionMeta):
    r"""AdditiveExpressionSimple | AdditiveExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AdditiveExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryAdditiveExpression:
    left: AdditiveExpression
    operator: AdditiveOperator
    right: MultiplicativeExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryAdditiveExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class AdditiveOperator(Enum):
    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

AdditiveOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.AdditiveOperator")

class ShiftExpressionSimple(Node["AdditiveExpression"]):
    ...

class ShiftExpressionBinary(Node["BinaryShiftExpression"]):
    ...

class _ShiftExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ShiftExpression(metaclass=_ShiftExpressionMeta):
    r"""ShiftExpressionSimple | ShiftExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ShiftExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryShiftExpression:
    left: ShiftExpression
    operator: ShiftOperator
    right: AdditiveExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryShiftExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class ShiftOperator(Enum):
    LEFT = hydra.core.Name("left")

    RIGHT = hydra.core.Name("right")

ShiftOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ShiftOperator")

class RelationalExpressionSimple(Node["ShiftExpression"]):
    ...

class RelationalExpressionBinary(Node["BinaryRelationalExpression"]):
    ...

class RelationalExpressionIsType(Node["IsTypeExpression"]):
    ...

class RelationalExpressionIsPattern(Node["IsPatternExpression"]):
    ...

class RelationalExpressionAsType(Node["AsTypeExpression"]):
    ...

class _RelationalExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class RelationalExpression(metaclass=_RelationalExpressionMeta):
    r"""RelationalExpressionSimple | RelationalExpressionBinary | RelationalExpressionIsType | RelationalExpressionIsPattern | RelationalExpressionAsType"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RelationalExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")
    IS_TYPE = hydra.core.Name("isType")
    IS_PATTERN = hydra.core.Name("isPattern")
    AS_TYPE = hydra.core.Name("asType")

@dataclass(frozen=True)
class BinaryRelationalExpression:
    left: RelationalExpression
    operator: RelationalOperator
    right: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryRelationalExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class RelationalOperator(Enum):
    LESS_THAN = hydra.core.Name("lessThan")

    GREATER_THAN = hydra.core.Name("greaterThan")

    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")

    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")

RelationalOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.RelationalOperator")

@dataclass(frozen=True)
class IsTypeExpression:
    expression: RelationalExpression
    type: Type

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IsTypeExpression")
    EXPRESSION = hydra.core.Name("expression")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class IsPatternExpression:
    expression: RelationalExpression
    pattern: Pattern

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IsPatternExpression")
    EXPRESSION = hydra.core.Name("expression")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class AsTypeExpression:
    expression: RelationalExpression
    type: Type

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AsTypeExpression")
    EXPRESSION = hydra.core.Name("expression")
    TYPE = hydra.core.Name("type")

class EqualityExpressionSimple(Node["RelationalExpression"]):
    ...

class EqualityExpressionBinary(Node["BinaryEqualityExpression"]):
    ...

class _EqualityExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class EqualityExpression(metaclass=_EqualityExpressionMeta):
    r"""EqualityExpressionSimple | EqualityExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EqualityExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryEqualityExpression:
    left: EqualityExpression
    operator: EqualityOperator
    right: RelationalExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryEqualityExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class EqualityOperator(Enum):
    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

EqualityOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.EqualityOperator")

class AndExpressionSimple(Node["EqualityExpression"]):
    ...

class AndExpressionBinary(Node["BinaryAndExpression"]):
    ...

class _AndExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AndExpression(metaclass=_AndExpressionMeta):
    r"""AndExpressionSimple | AndExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AndExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryAndExpression:
    left: AndExpression
    right: EqualityExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryAndExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class ExclusiveOrExpressionSimple(Node["AndExpression"]):
    ...

class ExclusiveOrExpressionBinary(Node["BinaryExclusiveOrExpression"]):
    ...

class _ExclusiveOrExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ExclusiveOrExpression(metaclass=_ExclusiveOrExpressionMeta):
    r"""ExclusiveOrExpressionSimple | ExclusiveOrExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExclusiveOrExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryExclusiveOrExpression:
    left: ExclusiveOrExpression
    right: AndExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryExclusiveOrExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class InclusiveOrExpressionSimple(Node["ExclusiveOrExpression"]):
    ...

class InclusiveOrExpressionBinary(Node["BinaryInclusiveOrExpression"]):
    ...

class _InclusiveOrExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class InclusiveOrExpression(metaclass=_InclusiveOrExpressionMeta):
    r"""InclusiveOrExpressionSimple | InclusiveOrExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InclusiveOrExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryInclusiveOrExpression:
    left: InclusiveOrExpression
    right: ExclusiveOrExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryInclusiveOrExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class ConditionalAndExpressionSimple(Node["InclusiveOrExpression"]):
    ...

class ConditionalAndExpressionBinary(Node["BinaryConditionalAndExpression"]):
    ...

class _ConditionalAndExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ConditionalAndExpression(metaclass=_ConditionalAndExpressionMeta):
    r"""ConditionalAndExpressionSimple | ConditionalAndExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConditionalAndExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryConditionalAndExpression:
    left: ConditionalAndExpression
    right: InclusiveOrExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryConditionalAndExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class ConditionalOrExpressionSimple(Node["ConditionalAndExpression"]):
    ...

class ConditionalOrExpressionBinary(Node["BinaryConditionalOrExpression"]):
    ...

class _ConditionalOrExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ConditionalOrExpression(metaclass=_ConditionalOrExpressionMeta):
    r"""ConditionalOrExpressionSimple | ConditionalOrExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConditionalOrExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")

@dataclass(frozen=True)
class BinaryConditionalOrExpression:
    left: ConditionalOrExpression
    right: ConditionalAndExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryConditionalOrExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class NullCoalescingExpressionSimple(Node["ConditionalOrExpression"]):
    ...

class NullCoalescingExpressionBinary(Node["BinaryNullCoalescingExpression"]):
    ...

class NullCoalescingExpressionThrow(Node["NullCoalescingExpression"]):
    ...

class _NullCoalescingExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class NullCoalescingExpression(metaclass=_NullCoalescingExpressionMeta):
    r"""NullCoalescingExpressionSimple | NullCoalescingExpressionBinary | NullCoalescingExpressionThrow"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NullCoalescingExpression")
    SIMPLE = hydra.core.Name("simple")
    BINARY = hydra.core.Name("binary")
    THROW = hydra.core.Name("throw")

@dataclass(frozen=True)
class BinaryNullCoalescingExpression:
    left: ConditionalOrExpression
    right: NullCoalescingExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryNullCoalescingExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class DeclarationExpression:
    type: LocalVariableType
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DeclarationExpression")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")

class LocalVariableTypeType(Node["Type"]):
    ...

class LocalVariableTypeVar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LocalVariableTypeVar)
    def __hash__(self):
        return hash("LocalVariableTypeVar")

class _LocalVariableTypeMeta(type):
    def __getitem__(cls, item):
        return object

class LocalVariableType(metaclass=_LocalVariableTypeMeta):
    r"""LocalVariableTypeType | LocalVariableTypeVar"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalVariableType")
    TYPE = hydra.core.Name("type")
    VAR = hydra.core.Name("var")

class ConditionalExpressionSimple(Node["NullCoalescingExpression"]):
    ...

class ConditionalExpressionSimpleConditional(Node["SimpleConditionalExpression"]):
    ...

class ConditionalExpressionRefConditional(Node["RefConditionalExpression"]):
    ...

class _ConditionalExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ConditionalExpression(metaclass=_ConditionalExpressionMeta):
    r"""ConditionalExpressionSimple | ConditionalExpressionSimpleConditional | ConditionalExpressionRefConditional"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConditionalExpression")
    SIMPLE = hydra.core.Name("simple")
    SIMPLE_CONDITIONAL = hydra.core.Name("simpleConditional")
    REF_CONDITIONAL = hydra.core.Name("refConditional")

@dataclass(frozen=True)
class SimpleConditionalExpression:
    condition: NullCoalescingExpression
    true: Expression
    false: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SimpleConditionalExpression")
    CONDITION = hydra.core.Name("condition")
    TRUE = hydra.core.Name("true")
    FALSE = hydra.core.Name("false")

@dataclass(frozen=True)
class RefConditionalExpression:
    condition: NullCoalescingExpression
    true: VariableReference
    false: VariableReference

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefConditionalExpression")
    CONDITION = hydra.core.Name("condition")
    TRUE = hydra.core.Name("true")
    FALSE = hydra.core.Name("false")

@dataclass(frozen=True)
class LambdaExpression:
    async_: bool
    signature: AnonymousFunctionSignature
    body: AnonymousFunctionBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LambdaExpression")
    ASYNC = hydra.core.Name("async")
    SIGNATURE = hydra.core.Name("signature")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class AnonymousMethodExpression:
    async_: bool
    signature: frozenlist[ExplicitAnonymousFunctionParameter]
    body: Block

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AnonymousMethodExpression")
    ASYNC = hydra.core.Name("async")
    SIGNATURE = hydra.core.Name("signature")
    BODY = hydra.core.Name("body")

class AnonymousFunctionSignatureExplicit(Node["frozenlist[ExplicitAnonymousFunctionParameter]"]):
    ...

class AnonymousFunctionSignatureImplicit(Node["frozenlist[Identifier]"]):
    ...

class _AnonymousFunctionSignatureMeta(type):
    def __getitem__(cls, item):
        return object

class AnonymousFunctionSignature(metaclass=_AnonymousFunctionSignatureMeta):
    r"""AnonymousFunctionSignatureExplicit | AnonymousFunctionSignatureImplicit"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AnonymousFunctionSignature")
    EXPLICIT = hydra.core.Name("explicit")
    IMPLICIT = hydra.core.Name("implicit")

@dataclass(frozen=True)
class ExplicitAnonymousFunctionParameter:
    modifier: Maybe[AnonymousFunctionParameterModifier]
    type: Type
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExplicitAnonymousFunctionParameter")
    MODIFIER = hydra.core.Name("modifier")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")

class AnonymousFunctionParameterModifier(Enum):
    REF = hydra.core.Name("ref")

    OUT = hydra.core.Name("out")

    IN = hydra.core.Name("in")

AnonymousFunctionParameterModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.AnonymousFunctionParameterModifier")

class AnonymousFunctionBodyNullConditionalInvocation(Node["NullConditionalInvocationExpression"]):
    ...

class AnonymousFunctionBodyExpression(Node["Expression"]):
    ...

class AnonymousFunctionBodyRef(Node["VariableReference"]):
    ...

class AnonymousFunctionBodyBlock(Node["Block"]):
    ...

class _AnonymousFunctionBodyMeta(type):
    def __getitem__(cls, item):
        return object

class AnonymousFunctionBody(metaclass=_AnonymousFunctionBodyMeta):
    r"""AnonymousFunctionBodyNullConditionalInvocation | AnonymousFunctionBodyExpression | AnonymousFunctionBodyRef | AnonymousFunctionBodyBlock"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AnonymousFunctionBody")
    NULL_CONDITIONAL_INVOCATION = hydra.core.Name("nullConditionalInvocation")
    EXPRESSION = hydra.core.Name("expression")
    REF = hydra.core.Name("ref")
    BLOCK = hydra.core.Name("block")

@dataclass(frozen=True)
class QueryExpression:
    from_: FromClause
    body: QueryBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.QueryExpression")
    FROM = hydra.core.Name("from")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class FromClause:
    type: Maybe[Type]
    identifier: Identifier
    in_: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FromClause")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")
    IN = hydra.core.Name("in")

@dataclass(frozen=True)
class QueryBody:
    clauses: frozenlist[QueryBodyClause]
    select_or_group: SelectOrGroupClause
    continuation: Maybe[QueryContinuation]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.QueryBody")
    CLAUSES = hydra.core.Name("clauses")
    SELECT_OR_GROUP = hydra.core.Name("selectOrGroup")
    CONTINUATION = hydra.core.Name("continuation")

class QueryBodyClauseFrom(Node["FromClause"]):
    ...

class QueryBodyClauseLet(Node["LetClause"]):
    ...

class QueryBodyClauseWhere(Node["BooleanExpression"]):
    ...

class QueryBodyClauseJoin(Node["JoinClause"]):
    ...

class QueryBodyClauseOrderby(Node["frozenlist[Ordering]"]):
    ...

class _QueryBodyClauseMeta(type):
    def __getitem__(cls, item):
        return object

class QueryBodyClause(metaclass=_QueryBodyClauseMeta):
    r"""QueryBodyClauseFrom | QueryBodyClauseLet | QueryBodyClauseWhere | QueryBodyClauseJoin | QueryBodyClauseOrderby"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.QueryBodyClause")
    FROM = hydra.core.Name("from")
    LET = hydra.core.Name("let")
    WHERE = hydra.core.Name("where")
    JOIN = hydra.core.Name("join")
    ORDERBY = hydra.core.Name("orderby")

@dataclass(frozen=True)
class LetClause:
    left: Identifier
    right: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LetClause")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class JoinClause:
    type: Maybe[Type]
    identifier: Identifier
    in_: Expression
    on: Expression
    equals: Expression
    into: Maybe[Identifier]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.JoinClause")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")
    IN = hydra.core.Name("in")
    ON = hydra.core.Name("on")
    EQUALS = hydra.core.Name("equals")
    INTO = hydra.core.Name("into")

@dataclass(frozen=True)
class Ordering:
    expression: Expression
    direction: Maybe[OrderingDirection]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Ordering")
    EXPRESSION = hydra.core.Name("expression")
    DIRECTION = hydra.core.Name("direction")

class OrderingDirection(Enum):
    ASCENDING = hydra.core.Name("ascending")

    DESCENDING = hydra.core.Name("descending")

OrderingDirection.TYPE_ = hydra.core.Name("hydra.csharp.syntax.OrderingDirection")

class SelectOrGroupClauseSelect(Node["Expression"]):
    ...

class SelectOrGroupClauseGroup(Node["GroupClause"]):
    ...

class _SelectOrGroupClauseMeta(type):
    def __getitem__(cls, item):
        return object

class SelectOrGroupClause(metaclass=_SelectOrGroupClauseMeta):
    r"""SelectOrGroupClauseSelect | SelectOrGroupClauseGroup"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SelectOrGroupClause")
    SELECT = hydra.core.Name("select")
    GROUP = hydra.core.Name("group")

@dataclass(frozen=True)
class GroupClause:
    grouped: Expression
    by: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.GroupClause")
    GROUPED = hydra.core.Name("grouped")
    BY = hydra.core.Name("by")

@dataclass(frozen=True)
class QueryContinuation:
    into: Identifier
    body: QueryBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.QueryContinuation")
    INTO = hydra.core.Name("into")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Assignment:
    left: UnaryExpression
    operator: AssignmentOperator
    right: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Assignment")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class AssignmentOperatorSimple(Node[bool]):
    ...

class AssignmentOperatorPlusEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorPlusEquals)
    def __hash__(self):
        return hash("AssignmentOperatorPlusEquals")

class AssignmentOperatorMinusEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorMinusEquals)
    def __hash__(self):
        return hash("AssignmentOperatorMinusEquals")

class AssignmentOperatorTimesEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorTimesEquals)
    def __hash__(self):
        return hash("AssignmentOperatorTimesEquals")

class AssignmentOperatorDivideEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorDivideEquals)
    def __hash__(self):
        return hash("AssignmentOperatorDivideEquals")

class AssignmentOperatorModEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorModEquals)
    def __hash__(self):
        return hash("AssignmentOperatorModEquals")

class AssignmentOperatorAndEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorAndEquals)
    def __hash__(self):
        return hash("AssignmentOperatorAndEquals")

class AssignmentOperatorOrEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorOrEquals)
    def __hash__(self):
        return hash("AssignmentOperatorOrEquals")

class AssignmentOperatorXorEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorXorEquals)
    def __hash__(self):
        return hash("AssignmentOperatorXorEquals")

class AssignmentOperatorLeftShiftEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorLeftShiftEquals)
    def __hash__(self):
        return hash("AssignmentOperatorLeftShiftEquals")

class AssignmentOperatorRightShiftEquals:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignmentOperatorRightShiftEquals)
    def __hash__(self):
        return hash("AssignmentOperatorRightShiftEquals")

class _AssignmentOperatorMeta(type):
    def __getitem__(cls, item):
        return object

class AssignmentOperator(metaclass=_AssignmentOperatorMeta):
    r"""AssignmentOperatorSimple | AssignmentOperatorPlusEquals | AssignmentOperatorMinusEquals | AssignmentOperatorTimesEquals | AssignmentOperatorDivideEquals | AssignmentOperatorModEquals | AssignmentOperatorAndEquals | AssignmentOperatorOrEquals | AssignmentOperatorXorEquals | AssignmentOperatorLeftShiftEquals | AssignmentOperatorRightShiftEquals"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AssignmentOperator")
    SIMPLE = hydra.core.Name("simple")
    PLUS_EQUALS = hydra.core.Name("plusEquals")
    MINUS_EQUALS = hydra.core.Name("minusEquals")
    TIMES_EQUALS = hydra.core.Name("timesEquals")
    DIVIDE_EQUALS = hydra.core.Name("divideEquals")
    MOD_EQUALS = hydra.core.Name("modEquals")
    AND_EQUALS = hydra.core.Name("andEquals")
    OR_EQUALS = hydra.core.Name("orEquals")
    XOR_EQUALS = hydra.core.Name("xorEquals")
    LEFT_SHIFT_EQUALS = hydra.core.Name("leftShiftEquals")
    RIGHT_SHIFT_EQUALS = hydra.core.Name("rightShiftEquals")

class ExpressionNonAssignment(Node["NonAssignmentExpression"]):
    ...

class ExpressionAssignment(Node["Assignment"]):
    ...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionNonAssignment | ExpressionAssignment"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Expression")
    NON_ASSIGNMENT = hydra.core.Name("nonAssignment")
    ASSIGNMENT = hydra.core.Name("assignment")

class NonAssignmentExpressionDeclaration(Node["DeclarationExpression"]):
    ...

class NonAssignmentExpressionConditional(Node["ConditionalExpression"]):
    ...

class NonAssignmentExpressionLambda(Node["LambdaExpression"]):
    ...

class NonAssignmentExpressionQuery(Node["QueryExpression"]):
    ...

class _NonAssignmentExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class NonAssignmentExpression(metaclass=_NonAssignmentExpressionMeta):
    r"""NonAssignmentExpressionDeclaration | NonAssignmentExpressionConditional | NonAssignmentExpressionLambda | NonAssignmentExpressionQuery"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NonAssignmentExpression")
    DECLARATION = hydra.core.Name("declaration")
    CONDITIONAL = hydra.core.Name("conditional")
    LAMBDA = hydra.core.Name("lambda")
    QUERY = hydra.core.Name("query")

class ConstantExpression(Node["Expression"]):
    ...

ConstantExpression.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstantExpression")

class BooleanExpression(Node["Expression"]):
    ...

BooleanExpression.TYPE_ = hydra.core.Name("hydra.csharp.syntax.BooleanExpression")

class StatementLabeled(Node["LabeledStatement"]):
    ...

class StatementDeclaration(Node["DeclarationStatement"]):
    ...

class StatementEmbedded(Node["EmbeddedStatement"]):
    ...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementLabeled | StatementDeclaration | StatementEmbedded"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Statement")
    LABELED = hydra.core.Name("labeled")
    DECLARATION = hydra.core.Name("declaration")
    EMBEDDED = hydra.core.Name("embedded")

class EmbeddedStatementBlock(Node["Block"]):
    ...

class EmbeddedStatementEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, EmbeddedStatementEmpty)
    def __hash__(self):
        return hash("EmbeddedStatementEmpty")

class EmbeddedStatementExpression(Node["StatementExpression"]):
    ...

class EmbeddedStatementSelection(Node["SelectionStatement"]):
    ...

class EmbeddedStatementIteration(Node["IterationStatement"]):
    ...

class EmbeddedStatementJump(Node["JumpStatement"]):
    ...

class EmbeddedStatementTry(Node["TryStatement"]):
    ...

class EmbeddedStatementChecked(Node["Block"]):
    ...

class EmbeddedStatementUnchecked(Node["Block"]):
    ...

class EmbeddedStatementLock(Node["LockStatement"]):
    ...

class EmbeddedStatementUsing(Node["UsingStatement"]):
    ...

class EmbeddedStatementYield(Node["YieldStatement"]):
    ...

class EmbeddedStatementUnsafe(Node["Block"]):
    ...

class EmbeddedStatementFixed(Node["FixedStatement"]):
    ...

class _EmbeddedStatementMeta(type):
    def __getitem__(cls, item):
        return object

class EmbeddedStatement(metaclass=_EmbeddedStatementMeta):
    r"""EmbeddedStatementBlock | EmbeddedStatementEmpty | EmbeddedStatementExpression | EmbeddedStatementSelection | EmbeddedStatementIteration | EmbeddedStatementJump | EmbeddedStatementTry | EmbeddedStatementChecked | EmbeddedStatementUnchecked | EmbeddedStatementLock | EmbeddedStatementUsing | EmbeddedStatementYield | EmbeddedStatementUnsafe | EmbeddedStatementFixed"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EmbeddedStatement")
    BLOCK = hydra.core.Name("block")
    EMPTY = hydra.core.Name("empty")
    EXPRESSION = hydra.core.Name("expression")
    SELECTION = hydra.core.Name("selection")
    ITERATION = hydra.core.Name("iteration")
    JUMP = hydra.core.Name("jump")
    TRY = hydra.core.Name("try")
    CHECKED = hydra.core.Name("checked")
    UNCHECKED = hydra.core.Name("unchecked")
    LOCK = hydra.core.Name("lock")
    USING = hydra.core.Name("using")
    YIELD = hydra.core.Name("yield")
    UNSAFE = hydra.core.Name("unsafe")
    FIXED = hydra.core.Name("fixed")

class Block(Node["frozenlist[Statement]"]):
    ...

Block.TYPE_ = hydra.core.Name("hydra.csharp.syntax.Block")

@dataclass(frozen=True)
class LabeledStatement:
    label: Identifier
    statement: Statement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LabeledStatement")
    LABEL = hydra.core.Name("label")
    STATEMENT = hydra.core.Name("statement")

class DeclarationStatementVariable(Node["LocalVariableDeclaration"]):
    ...

class DeclarationStatementConstant(Node["LocalConstantDeclaration"]):
    ...

class DeclarationStatementFunction(Node["LocalFunctionDeclaration"]):
    ...

class _DeclarationStatementMeta(type):
    def __getitem__(cls, item):
        return object

class DeclarationStatement(metaclass=_DeclarationStatementMeta):
    r"""DeclarationStatementVariable | DeclarationStatementConstant | DeclarationStatementFunction"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DeclarationStatement")
    VARIABLE = hydra.core.Name("variable")
    CONSTANT = hydra.core.Name("constant")
    FUNCTION = hydra.core.Name("function")

class LocalVariableDeclarationImplicitlyTyped(Node["ImplicitlyTypedLocalVariableDeclaration"]):
    ...

class LocalVariableDeclarationExplicitlyTyped(Node["ExplicitlyTypedLocalVariableDeclaration"]):
    ...

class LocalVariableDeclarationRef(Node["RefLocalVariableDeclaration"]):
    ...

class _LocalVariableDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class LocalVariableDeclaration(metaclass=_LocalVariableDeclarationMeta):
    r"""LocalVariableDeclarationImplicitlyTyped | LocalVariableDeclarationExplicitlyTyped | LocalVariableDeclarationRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalVariableDeclaration")
    IMPLICITLY_TYPED = hydra.core.Name("implicitlyTyped")
    EXPLICITLY_TYPED = hydra.core.Name("explicitlyTyped")
    REF = hydra.core.Name("ref")

class ImplicitlyTypedLocalVariableDeclarationVar(Node["ImplicitlyTypedLocalVariableDeclarator"]):
    ...

class ImplicitlyTypedLocalVariableDeclarationRefVar(Node["RefVarImplicitlyTypedLocalVariableDeclaration"]):
    ...

class _ImplicitlyTypedLocalVariableDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ImplicitlyTypedLocalVariableDeclaration(metaclass=_ImplicitlyTypedLocalVariableDeclarationMeta):
    r"""ImplicitlyTypedLocalVariableDeclarationVar | ImplicitlyTypedLocalVariableDeclarationRefVar"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ImplicitlyTypedLocalVariableDeclaration")
    VAR = hydra.core.Name("var")
    REF_VAR = hydra.core.Name("refVar")

@dataclass(frozen=True)
class RefVarImplicitlyTypedLocalVariableDeclaration:
    ref_kind: RefKind
    declarator: RefLocalVariableDeclarator

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefVarImplicitlyTypedLocalVariableDeclaration")
    REF_KIND = hydra.core.Name("refKind")
    DECLARATOR = hydra.core.Name("declarator")

@dataclass(frozen=True)
class ImplicitlyTypedLocalVariableDeclarator:
    identifier: Identifier
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ImplicitlyTypedLocalVariableDeclarator")
    IDENTIFIER = hydra.core.Name("identifier")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class ExplicitlyTypedLocalVariableDeclaration:
    type: Type
    declarators: frozenlist[ExplicitlyTypedLocalVariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExplicitlyTypedLocalVariableDeclaration")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

@dataclass(frozen=True)
class ExplicitlyTypedLocalVariableDeclarator:
    identifier: Identifier
    initializer: Maybe[LocalVariableInitializer]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExplicitlyTypedLocalVariableDeclarator")
    IDENTIFIER = hydra.core.Name("identifier")
    INITIALIZER = hydra.core.Name("initializer")

class LocalVariableInitializerExpression(Node["Expression"]):
    ...

class LocalVariableInitializerInitializer(Node["ArrayInitializer"]):
    ...

class _LocalVariableInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class LocalVariableInitializer(metaclass=_LocalVariableInitializerMeta):
    r"""LocalVariableInitializerExpression | LocalVariableInitializerInitializer"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalVariableInitializer")
    EXPRESSION = hydra.core.Name("expression")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class RefLocalVariableDeclaration:
    ref_kind: RefKind
    type: Type
    declarators: frozenlist[RefLocalVariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefLocalVariableDeclaration")
    REF_KIND = hydra.core.Name("refKind")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

@dataclass(frozen=True)
class RefLocalVariableDeclarator:
    left: Identifier
    right: VariableReference

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefLocalVariableDeclarator")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class LocalConstantDeclaration:
    type: Type
    declarators: frozenlist[ConstantDeclarator]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalConstantDeclaration")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

@dataclass(frozen=True)
class ConstantDeclarator:
    identifier: Identifier
    expression: ConstantExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstantDeclarator")
    IDENTIFIER = hydra.core.Name("identifier")
    EXPRESSION = hydra.core.Name("expression")

class LocalFunctionDeclarationStandard(Node["StandardLocalFunctionDeclaration"]):
    ...

class LocalFunctionDeclarationRef(Node["RefLocalFunctionDeclaration"]):
    ...

class _LocalFunctionDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class LocalFunctionDeclaration(metaclass=_LocalFunctionDeclarationMeta):
    r"""LocalFunctionDeclarationStandard | LocalFunctionDeclarationRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalFunctionDeclaration")
    STANDARD = hydra.core.Name("standard")
    REF = hydra.core.Name("ref")

@dataclass(frozen=True)
class StandardLocalFunctionDeclaration:
    modifiers: frozenlist[LocalFunctionModifier]
    return_type: ReturnType
    header: LocalFunctionHeader
    body: LocalFunctionBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StandardLocalFunctionDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    RETURN_TYPE = hydra.core.Name("returnType")
    HEADER = hydra.core.Name("header")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class RefLocalFunctionDeclaration:
    modifiers: frozenlist[RefLocalFunctionModifier]
    ref_kind: RefKind
    return_type: Type
    header: LocalFunctionHeader
    body: RefLocalFunctionBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefLocalFunctionDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    REF_KIND = hydra.core.Name("refKind")
    RETURN_TYPE = hydra.core.Name("returnType")
    HEADER = hydra.core.Name("header")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class LocalFunctionHeader:
    identifier: Identifier
    type_parameters: Maybe[TypeParameterList]
    parameters: FormalParameterList
    constraints: frozenlist[TypeParameterConstraintsClause]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalFunctionHeader")
    IDENTIFIER = hydra.core.Name("identifier")
    TYPE_PARAMETERS = hydra.core.Name("typeParameters")
    PARAMETERS = hydra.core.Name("parameters")
    CONSTRAINTS = hydra.core.Name("constraints")

class LocalFunctionModifierRef(Node["RefLocalFunctionModifier"]):
    ...

class LocalFunctionModifierAsync:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LocalFunctionModifierAsync)
    def __hash__(self):
        return hash("LocalFunctionModifierAsync")

class _LocalFunctionModifierMeta(type):
    def __getitem__(cls, item):
        return object

class LocalFunctionModifier(metaclass=_LocalFunctionModifierMeta):
    r"""LocalFunctionModifierRef | LocalFunctionModifierAsync"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalFunctionModifier")
    REF = hydra.core.Name("ref")
    ASYNC = hydra.core.Name("async")

class RefLocalFunctionModifier(Enum):
    STATIC = hydra.core.Name("static")

    UNSAFE = hydra.core.Name("unsafe")

RefLocalFunctionModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefLocalFunctionModifier")

class LocalFunctionBodyBlock(Node["Block"]):
    ...

class LocalFunctionBodyNullConditionalInvocation(Node["NullConditionalInvocationExpression"]):
    ...

class LocalFunctionBodyExpression(Node["Expression"]):
    ...

class _LocalFunctionBodyMeta(type):
    def __getitem__(cls, item):
        return object

class LocalFunctionBody(metaclass=_LocalFunctionBodyMeta):
    r"""LocalFunctionBodyBlock | LocalFunctionBodyNullConditionalInvocation | LocalFunctionBodyExpression"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LocalFunctionBody")
    BLOCK = hydra.core.Name("block")
    NULL_CONDITIONAL_INVOCATION = hydra.core.Name("nullConditionalInvocation")
    EXPRESSION = hydra.core.Name("expression")

class RefLocalFunctionBodyBlock(Node["Block"]):
    ...

class RefLocalFunctionBodyRef(Node["VariableReference"]):
    ...

class _RefLocalFunctionBodyMeta(type):
    def __getitem__(cls, item):
        return object

class RefLocalFunctionBody(metaclass=_RefLocalFunctionBodyMeta):
    r"""RefLocalFunctionBodyBlock | RefLocalFunctionBodyRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefLocalFunctionBody")
    BLOCK = hydra.core.Name("block")
    REF = hydra.core.Name("ref")

class StatementExpressionNullConditionalInvocation(Node["NullConditionalInvocationExpression"]):
    ...

class StatementExpressionInvocation(Node["InvocationExpression"]):
    ...

class StatementExpressionObjectCreation(Node["ObjectCreationExpression"]):
    ...

class StatementExpressionAssignment(Node["Assignment"]):
    ...

class StatementExpressionPostIncrement(Node["PrimaryExpression"]):
    ...

class StatementExpressionPostDecrement(Node["PrimaryExpression"]):
    ...

class StatementExpressionPreIncrement(Node["UnaryExpression"]):
    ...

class StatementExpressionPreDecrement(Node["UnaryExpression"]):
    ...

class StatementExpressionAwait(Node["UnaryExpression"]):
    ...

class _StatementExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class StatementExpression(metaclass=_StatementExpressionMeta):
    r"""StatementExpressionNullConditionalInvocation | StatementExpressionInvocation | StatementExpressionObjectCreation | StatementExpressionAssignment | StatementExpressionPostIncrement | StatementExpressionPostDecrement | StatementExpressionPreIncrement | StatementExpressionPreDecrement | StatementExpressionAwait"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StatementExpression")
    NULL_CONDITIONAL_INVOCATION = hydra.core.Name("nullConditionalInvocation")
    INVOCATION = hydra.core.Name("invocation")
    OBJECT_CREATION = hydra.core.Name("objectCreation")
    ASSIGNMENT = hydra.core.Name("assignment")
    POST_INCREMENT = hydra.core.Name("postIncrement")
    POST_DECREMENT = hydra.core.Name("postDecrement")
    PRE_INCREMENT = hydra.core.Name("preIncrement")
    PRE_DECREMENT = hydra.core.Name("preDecrement")
    AWAIT = hydra.core.Name("await")

class SelectionStatementIf(Node["IfStatement"]):
    ...

class SelectionStatementSwitch(Node["SwitchStatement"]):
    ...

class _SelectionStatementMeta(type):
    def __getitem__(cls, item):
        return object

class SelectionStatement(metaclass=_SelectionStatementMeta):
    r"""SelectionStatementIf | SelectionStatementSwitch"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SelectionStatement")
    IF = hydra.core.Name("if")
    SWITCH = hydra.core.Name("switch")

@dataclass(frozen=True)
class IfStatement:
    condition: BooleanExpression
    if_branch: EmbeddedStatement
    else_branch: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IfStatement")
    CONDITION = hydra.core.Name("condition")
    IF_BRANCH = hydra.core.Name("ifBranch")
    ELSE_BRANCH = hydra.core.Name("elseBranch")

@dataclass(frozen=True)
class SwitchStatement:
    expression: Expression
    branches: frozenlist[SwitchSection]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SwitchStatement")
    EXPRESSION = hydra.core.Name("expression")
    BRANCHES = hydra.core.Name("branches")

@dataclass(frozen=True)
class SwitchSection:
    labels: frozenlist[SwitchLabel]
    statements: frozenlist[Statement]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SwitchSection")
    LABELS = hydra.core.Name("labels")
    STATEMENTS = hydra.core.Name("statements")

class SwitchLabelBranch(Node["SwitchBranch"]):
    ...

class SwitchLabelDefault:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SwitchLabelDefault)
    def __hash__(self):
        return hash("SwitchLabelDefault")

class _SwitchLabelMeta(type):
    def __getitem__(cls, item):
        return object

class SwitchLabel(metaclass=_SwitchLabelMeta):
    r"""SwitchLabelBranch | SwitchLabelDefault"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SwitchLabel")
    BRANCH = hydra.core.Name("branch")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class SwitchBranch:
    pattern: Pattern
    guard: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SwitchBranch")
    PATTERN = hydra.core.Name("pattern")
    GUARD = hydra.core.Name("guard")

class IterationStatementWhile(Node["WhileStatement"]):
    ...

class IterationStatementDo(Node["DoStatement"]):
    ...

class IterationStatementFor(Node["ForStatement"]):
    ...

class IterationStatementForeach(Node["ForeachStatement"]):
    ...

class _IterationStatementMeta(type):
    def __getitem__(cls, item):
        return object

class IterationStatement(metaclass=_IterationStatementMeta):
    r"""IterationStatementWhile | IterationStatementDo | IterationStatementFor | IterationStatementForeach"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IterationStatement")
    WHILE = hydra.core.Name("while")
    DO = hydra.core.Name("do")
    FOR = hydra.core.Name("for")
    FOREACH = hydra.core.Name("foreach")

@dataclass(frozen=True)
class WhileStatement:
    condition: BooleanExpression
    body: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.WhileStatement")
    CONDITION = hydra.core.Name("condition")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class DoStatement:
    body: EmbeddedStatement
    while_: BooleanExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DoStatement")
    BODY = hydra.core.Name("body")
    WHILE = hydra.core.Name("while")

@dataclass(frozen=True)
class ForStatement:
    initializer: Maybe[ForInitializer]
    condition: Maybe[BooleanExpression]
    iterator: Maybe[StatementExpressionList]
    body: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ForStatement")
    INITIALIZER = hydra.core.Name("initializer")
    CONDITION = hydra.core.Name("condition")
    ITERATOR = hydra.core.Name("iterator")
    BODY = hydra.core.Name("body")

class ForInitializerVariable(Node["LocalVariableDeclaration"]):
    ...

class ForInitializerStatements(Node["StatementExpressionList"]):
    ...

class _ForInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class ForInitializer(metaclass=_ForInitializerMeta):
    r"""ForInitializerVariable | ForInitializerStatements"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ForInitializer")
    VARIABLE = hydra.core.Name("variable")
    STATEMENTS = hydra.core.Name("statements")

class StatementExpressionList(Node["frozenlist[StatementExpression]"]):
    ...

StatementExpressionList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.StatementExpressionList")

@dataclass(frozen=True)
class ForeachStatement:
    kind: Maybe[RefKind]
    type: LocalVariableType
    identifier: Identifier
    expression: Expression
    body: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ForeachStatement")
    KIND = hydra.core.Name("kind")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")
    EXPRESSION = hydra.core.Name("expression")
    BODY = hydra.core.Name("body")

class JumpStatementBreak:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JumpStatementBreak)
    def __hash__(self):
        return hash("JumpStatementBreak")

class JumpStatementContinue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JumpStatementContinue)
    def __hash__(self):
        return hash("JumpStatementContinue")

class JumpStatementGoto(Node["GotoStatement"]):
    ...

class JumpStatementReturn(Node["ReturnStatement"]):
    ...

class JumpStatementThrow(Node["Maybe[Expression]"]):
    ...

class _JumpStatementMeta(type):
    def __getitem__(cls, item):
        return object

class JumpStatement(metaclass=_JumpStatementMeta):
    r"""JumpStatementBreak | JumpStatementContinue | JumpStatementGoto | JumpStatementReturn | JumpStatementThrow"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.JumpStatement")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    GOTO = hydra.core.Name("goto")
    RETURN = hydra.core.Name("return")
    THROW = hydra.core.Name("throw")

class GotoStatementIdentifier(Node["Identifier"]):
    ...

class GotoStatementCase(Node["ConstantExpression"]):
    ...

class GotoStatementDefault:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GotoStatementDefault)
    def __hash__(self):
        return hash("GotoStatementDefault")

class _GotoStatementMeta(type):
    def __getitem__(cls, item):
        return object

class GotoStatement(metaclass=_GotoStatementMeta):
    r"""GotoStatementIdentifier | GotoStatementCase | GotoStatementDefault"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.GotoStatement")
    IDENTIFIER = hydra.core.Name("identifier")
    CASE = hydra.core.Name("case")
    DEFAULT = hydra.core.Name("default")

class ReturnStatementSimple:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ReturnStatementSimple)
    def __hash__(self):
        return hash("ReturnStatementSimple")

class ReturnStatementValue(Node["Expression"]):
    ...

class ReturnStatementRef(Node["VariableReference"]):
    ...

class _ReturnStatementMeta(type):
    def __getitem__(cls, item):
        return object

class ReturnStatement(metaclass=_ReturnStatementMeta):
    r"""ReturnStatementSimple | ReturnStatementValue | ReturnStatementRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ReturnStatement")
    SIMPLE = hydra.core.Name("simple")
    VALUE = hydra.core.Name("value")
    REF = hydra.core.Name("ref")

@dataclass(frozen=True)
class TryStatement:
    body: Block
    catches: CatchClauses
    finally_: Maybe[Block]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TryStatement")
    BODY = hydra.core.Name("body")
    CATCHES = hydra.core.Name("catches")
    FINALLY = hydra.core.Name("finally")

class CatchClausesSpecific(Node["frozenlist[SpecificCatchClause]"]):
    ...

class CatchClausesGeneral(Node["Block"]):
    ...

class _CatchClausesMeta(type):
    def __getitem__(cls, item):
        return object

class CatchClauses(metaclass=_CatchClausesMeta):
    r"""CatchClausesSpecific | CatchClausesGeneral"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.CatchClauses")
    SPECIFIC = hydra.core.Name("specific")
    GENERAL = hydra.core.Name("general")

@dataclass(frozen=True)
class SpecificCatchClause:
    specifier: Maybe[ExceptionSpecifier]
    filter: Maybe[BooleanExpression]
    body: Block

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SpecificCatchClause")
    SPECIFIER = hydra.core.Name("specifier")
    FILTER = hydra.core.Name("filter")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class ExceptionSpecifier:
    type: Type
    identifier: Maybe[Identifier]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExceptionSpecifier")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class LockStatement:
    expression: Expression
    body: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.LockStatement")
    EXPRESSION = hydra.core.Name("expression")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class UsingStatement:
    acquisition: ResourceAcquisition
    body: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UsingStatement")
    ACQUISITION = hydra.core.Name("acquisition")
    BODY = hydra.core.Name("body")

class ResourceAcquisitionLocal(Node["LocalVariableDeclaration"]):
    ...

class ResourceAcquisitionExpression(Node["Expression"]):
    ...

class _ResourceAcquisitionMeta(type):
    def __getitem__(cls, item):
        return object

class ResourceAcquisition(metaclass=_ResourceAcquisitionMeta):
    r"""ResourceAcquisitionLocal | ResourceAcquisitionExpression"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ResourceAcquisition")
    LOCAL = hydra.core.Name("local")
    EXPRESSION = hydra.core.Name("expression")

class YieldStatementReturn(Node["Expression"]):
    ...

class YieldStatementBreak:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, YieldStatementBreak)
    def __hash__(self):
        return hash("YieldStatementBreak")

class _YieldStatementMeta(type):
    def __getitem__(cls, item):
        return object

class YieldStatement(metaclass=_YieldStatementMeta):
    r"""YieldStatementReturn | YieldStatementBreak"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.YieldStatement")
    RETURN = hydra.core.Name("return")
    BREAK = hydra.core.Name("break")

@dataclass(frozen=True)
class CompilationUnit:
    externs: frozenlist[ExternAliasDirective]
    usings: frozenlist[UsingDirective]
    attributes: frozenlist[GlobalAttributeSection]
    members: frozenlist[NamespaceMemberDeclaration]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.CompilationUnit")
    EXTERNS = hydra.core.Name("externs")
    USINGS = hydra.core.Name("usings")
    ATTRIBUTES = hydra.core.Name("attributes")
    MEMBERS = hydra.core.Name("members")

@dataclass(frozen=True)
class NamespaceDeclaration:
    name: QualifiedIdentifier
    body: NamespaceBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamespaceDeclaration")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")

class QualifiedIdentifier(Node["frozenlist[Identifier]"]):
    ...

QualifiedIdentifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.QualifiedIdentifier")

@dataclass(frozen=True)
class NamespaceBody:
    externs: frozenlist[ExternAliasDirective]
    usings: frozenlist[UsingDirective]
    members: frozenlist[NamespaceMemberDeclaration]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamespaceBody")
    EXTERNS = hydra.core.Name("externs")
    USINGS = hydra.core.Name("usings")
    MEMBERS = hydra.core.Name("members")

class ExternAliasDirective(Node["Identifier"]):
    ...

ExternAliasDirective.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ExternAliasDirective")

class UsingDirectiveAlias(Node["UsingAliasDirective"]):
    ...

class UsingDirectiveNamespace(Node["NamespaceName"]):
    ...

class UsingDirectiveStatic(Node["TypeName"]):
    ...

class _UsingDirectiveMeta(type):
    def __getitem__(cls, item):
        return object

class UsingDirective(metaclass=_UsingDirectiveMeta):
    r"""UsingDirectiveAlias | UsingDirectiveNamespace | UsingDirectiveStatic"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UsingDirective")
    ALIAS = hydra.core.Name("alias")
    NAMESPACE = hydra.core.Name("namespace")
    STATIC = hydra.core.Name("static")

@dataclass(frozen=True)
class UsingAliasDirective:
    alias: Identifier
    name: NamespaceOrTypeName

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UsingAliasDirective")
    ALIAS = hydra.core.Name("alias")
    NAME = hydra.core.Name("name")

class NamespaceMemberDeclarationNamespace(Node["NamespaceDeclaration"]):
    ...

class NamespaceMemberDeclarationType(Node["TypeDeclaration"]):
    ...

class _NamespaceMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class NamespaceMemberDeclaration(metaclass=_NamespaceMemberDeclarationMeta):
    r"""NamespaceMemberDeclarationNamespace | NamespaceMemberDeclarationType"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamespaceMemberDeclaration")
    NAMESPACE = hydra.core.Name("namespace")
    TYPE = hydra.core.Name("type")

class TypeDeclarationClass(Node["ClassDeclaration"]):
    ...

class TypeDeclarationStruct(Node["StructDeclaration"]):
    ...

class TypeDeclarationInterface(Node["InterfaceDeclaration"]):
    ...

class TypeDeclarationEnum(Node["EnumDeclaration"]):
    ...

class TypeDeclarationDelegate(Node["DelegateDeclaration"]):
    ...

class _TypeDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class TypeDeclaration(metaclass=_TypeDeclarationMeta):
    r"""TypeDeclarationClass | TypeDeclarationStruct | TypeDeclarationInterface | TypeDeclarationEnum | TypeDeclarationDelegate"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeDeclaration")
    CLASS = hydra.core.Name("class")
    STRUCT = hydra.core.Name("struct")
    INTERFACE = hydra.core.Name("interface")
    ENUM = hydra.core.Name("enum")
    DELEGATE = hydra.core.Name("delegate")

@dataclass(frozen=True)
class QualifiedAliasMember:
    alias: Identifier
    member: Identifier
    arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.QualifiedAliasMember")
    ALIAS = hydra.core.Name("alias")
    MEMBER = hydra.core.Name("member")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class ClassDeclaration:
    attributes: frozenlist[AttributeSection]
    modifiers: frozenlist[ClassModifier]
    partial: bool
    name: Identifier
    parameters: Maybe[TypeParameterList]
    base: Maybe[ClassBase]
    constraints: frozenlist[TypeParameterConstraintsClause]
    body: ClassBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ClassDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    PARTIAL = hydra.core.Name("partial")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    BASE = hydra.core.Name("base")
    CONSTRAINTS = hydra.core.Name("constraints")
    BODY = hydra.core.Name("body")

class ClassModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    ABSTRACT = hydra.core.Name("abstract")

    SEALED = hydra.core.Name("sealed")

    STATIC = hydra.core.Name("static")

    UNSAFE = hydra.core.Name("unsafe")

ClassModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ClassModifier")

class TypeParameterList(Node["frozenlist[TypeParameterPart]"]):
    ...

TypeParameterList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeParameterList")

@dataclass(frozen=True)
class TypeParameterPart:
    attributes: Maybe[Attributes]
    name: TypeParameter

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeParameterPart")
    ATTRIBUTES = hydra.core.Name("attributes")
    NAME = hydra.core.Name("name")

class ClassBaseClass(Node["Maybe[ClassType]"]):
    ...

class ClassBaseInterfaces(Node["frozenlist[InterfaceType]"]):
    ...

class _ClassBaseMeta(type):
    def __getitem__(cls, item):
        return object

class ClassBase(metaclass=_ClassBaseMeta):
    r"""ClassBaseClass | ClassBaseInterfaces"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ClassBase")
    CLASS = hydra.core.Name("class")
    INTERFACES = hydra.core.Name("interfaces")

@dataclass(frozen=True)
class TypeParameterConstraintsClause:
    parameter: TypeParameter
    constraints: frozenlist[TypeParameterConstraints]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeParameterConstraintsClause")
    PARAMETER = hydra.core.Name("parameter")
    CONSTRAINTS = hydra.core.Name("constraints")

@dataclass(frozen=True)
class TypeParameterConstraints:
    primary: Maybe[PrimaryConstraint]
    secondary: Maybe[SecondaryConstraints]
    constructor: bool

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.TypeParameterConstraints")
    PRIMARY = hydra.core.Name("primary")
    SECONDARY = hydra.core.Name("secondary")
    CONSTRUCTOR = hydra.core.Name("constructor")

class PrimaryConstraintClassType(Node["ClassType"]):
    ...

class PrimaryConstraintClass:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimaryConstraintClass)
    def __hash__(self):
        return hash("PrimaryConstraintClass")

class PrimaryConstraintStruct:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimaryConstraintStruct)
    def __hash__(self):
        return hash("PrimaryConstraintStruct")

class PrimaryConstraintUnmanaged:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimaryConstraintUnmanaged)
    def __hash__(self):
        return hash("PrimaryConstraintUnmanaged")

class _PrimaryConstraintMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryConstraint(metaclass=_PrimaryConstraintMeta):
    r"""PrimaryConstraintClassType | PrimaryConstraintClass | PrimaryConstraintStruct | PrimaryConstraintUnmanaged"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PrimaryConstraint")
    CLASS_TYPE = hydra.core.Name("classType")
    CLASS = hydra.core.Name("class")
    STRUCT = hydra.core.Name("struct")
    UNMANAGED = hydra.core.Name("unmanaged")

class SecondaryConstraints(Node["frozenlist[SecondaryConstraint]"]):
    ...

SecondaryConstraints.TYPE_ = hydra.core.Name("hydra.csharp.syntax.SecondaryConstraints")

class SecondaryConstraintInterface(Node["InterfaceType"]):
    ...

class SecondaryConstraintParameter(Node["TypeParameter"]):
    ...

class _SecondaryConstraintMeta(type):
    def __getitem__(cls, item):
        return object

class SecondaryConstraint(metaclass=_SecondaryConstraintMeta):
    r"""SecondaryConstraintInterface | SecondaryConstraintParameter"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.SecondaryConstraint")
    INTERFACE = hydra.core.Name("interface")
    PARAMETER = hydra.core.Name("parameter")

class ClassBody(Node["frozenlist[ClassMemberDeclaration]"]):
    ...

ClassBody.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ClassBody")

class ClassMemberDeclarationConstant(Node["ConstantDeclaration"]):
    ...

class ClassMemberDeclarationField(Node["FieldDeclaration"]):
    ...

class ClassMemberDeclarationMethod(Node["MethodDeclaration"]):
    ...

class ClassMemberDeclarationProperty(Node["PropertyDeclaration"]):
    ...

class ClassMemberDeclarationEvent(Node["EventDeclaration"]):
    ...

class ClassMemberDeclarationIndexer(Node["IndexerDeclaration"]):
    ...

class ClassMemberDeclarationOperator(Node["OperatorDeclaration"]):
    ...

class ClassMemberDeclarationConstructor(Node["ConstructorDeclaration"]):
    ...

class ClassMemberDeclarationFinalizer(Node["FinalizerDeclaration"]):
    ...

class ClassMemberDeclarationStaticConstructor(Node["StaticConstructorDeclaration"]):
    ...

class ClassMemberDeclarationType(Node["TypeDeclaration"]):
    ...

class _ClassMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ClassMemberDeclaration(metaclass=_ClassMemberDeclarationMeta):
    r"""ClassMemberDeclarationConstant | ClassMemberDeclarationField | ClassMemberDeclarationMethod | ClassMemberDeclarationProperty | ClassMemberDeclarationEvent | ClassMemberDeclarationIndexer | ClassMemberDeclarationOperator | ClassMemberDeclarationConstructor | ClassMemberDeclarationFinalizer | ClassMemberDeclarationStaticConstructor | ClassMemberDeclarationType"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ClassMemberDeclaration")
    CONSTANT = hydra.core.Name("constant")
    FIELD = hydra.core.Name("field")
    METHOD = hydra.core.Name("method")
    PROPERTY = hydra.core.Name("property")
    EVENT = hydra.core.Name("event")
    INDEXER = hydra.core.Name("indexer")
    OPERATOR = hydra.core.Name("operator")
    CONSTRUCTOR = hydra.core.Name("constructor")
    FINALIZER = hydra.core.Name("finalizer")
    STATIC_CONSTRUCTOR = hydra.core.Name("staticConstructor")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class ConstantDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[ConstantModifier]
    type: Type
    declarators: frozenlist[ConstantDeclarator]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstantDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

class ConstantModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

ConstantModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstantModifier")

@dataclass(frozen=True)
class FieldDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[FieldModifier]
    type: Type
    declarators: frozenlist[VariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FieldDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

class FieldModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    STATIC = hydra.core.Name("static")

    READONLY = hydra.core.Name("readonly")

    VOLATILE = hydra.core.Name("volatile")

    UNSAFE = hydra.core.Name("unsafe")

FieldModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.FieldModifier")

class VariableDeclarators(Node["frozenlist[VariableDeclarator]"]):
    ...

VariableDeclarators.TYPE_ = hydra.core.Name("hydra.csharp.syntax.VariableDeclarators")

@dataclass(frozen=True)
class VariableDeclarator:
    identifier: Identifier
    initializer: Maybe[VariableInitializer]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.VariableDeclarator")
    IDENTIFIER = hydra.core.Name("identifier")
    INITIALIZER = hydra.core.Name("initializer")

class MethodDeclarationStandard(Node["StandardMethodDeclaration"]):
    ...

class MethodDeclarationRefReturn(Node["RefReturnMethodDeclaration"]):
    ...

class _MethodDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class MethodDeclaration(metaclass=_MethodDeclarationMeta):
    r"""MethodDeclarationStandard | MethodDeclarationRefReturn"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MethodDeclaration")
    STANDARD = hydra.core.Name("standard")
    REF_RETURN = hydra.core.Name("refReturn")

@dataclass(frozen=True)
class StandardMethodDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[MethodModifier]
    return_type: ReturnType
    header: MethodHeader
    body: MethodBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StandardMethodDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    RETURN_TYPE = hydra.core.Name("returnType")
    HEADER = hydra.core.Name("header")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class RefReturnMethodDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[RefMethodModifier]
    kind: RefKind
    return_type: ReturnType
    header: MethodHeader
    body: RefMethodBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefReturnMethodDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    KIND = hydra.core.Name("kind")
    RETURN_TYPE = hydra.core.Name("returnType")
    HEADER = hydra.core.Name("header")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class MethodModifiers:
    modifiers: frozenlist[MethodModifier]
    partial: bool

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MethodModifiers")
    MODIFIERS = hydra.core.Name("modifiers")
    PARTIAL = hydra.core.Name("partial")

class RefKind(Enum):
    REF = hydra.core.Name("ref")

    REF_READONLY = hydra.core.Name("refReadonly")

RefKind.TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefKind")

@dataclass(frozen=True)
class MethodHeader:
    name: MemberName
    type_parameters: Maybe[TypeParameterList]
    parameters: Maybe[FormalParameterList]
    constraints: frozenlist[TypeParameterConstraintsClause]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MethodHeader")
    NAME = hydra.core.Name("name")
    TYPE_PARAMETERS = hydra.core.Name("typeParameters")
    PARAMETERS = hydra.core.Name("parameters")
    CONSTRAINTS = hydra.core.Name("constraints")

class MethodModifierRef(Node["RefMethodModifier"]):
    ...

class MethodModifierAsync:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierAsync)
    def __hash__(self):
        return hash("MethodModifierAsync")

class _MethodModifierMeta(type):
    def __getitem__(cls, item):
        return object

class MethodModifier(metaclass=_MethodModifierMeta):
    r"""MethodModifierRef | MethodModifierAsync"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MethodModifier")
    REF = hydra.core.Name("ref")
    ASYNC = hydra.core.Name("async")

class RefMethodModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    STATIC = hydra.core.Name("static")

    VIRTUAL = hydra.core.Name("virtual")

    SEALED = hydra.core.Name("sealed")

    OVERRIDE = hydra.core.Name("override")

    ABSTRACT = hydra.core.Name("abstract")

    EXTERN = hydra.core.Name("extern")

    UNSAFE = hydra.core.Name("unsafe")

RefMethodModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefMethodModifier")

class ReturnTypeRef(Node["Type"]):
    ...

class ReturnTypeVoid:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ReturnTypeVoid)
    def __hash__(self):
        return hash("ReturnTypeVoid")

class _ReturnTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ReturnType(metaclass=_ReturnTypeMeta):
    r"""ReturnTypeRef | ReturnTypeVoid"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ReturnType")
    REF = hydra.core.Name("ref")
    VOID = hydra.core.Name("void")

@dataclass(frozen=True)
class MemberName:
    interface_type: Maybe[TypeName]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MemberName")
    INTERFACE_TYPE = hydra.core.Name("interfaceType")
    IDENTIFIER = hydra.core.Name("identifier")

class MethodBodyBlock(Node["Block"]):
    ...

class MethodBodyNullConditionalInvocation(Node["NullConditionalInvocationExpression"]):
    ...

class MethodBodyExpression(Node["Expression"]):
    ...

class MethodBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodBodyEmpty)
    def __hash__(self):
        return hash("MethodBodyEmpty")

class _MethodBodyMeta(type):
    def __getitem__(cls, item):
        return object

class MethodBody(metaclass=_MethodBodyMeta):
    r"""MethodBodyBlock | MethodBodyNullConditionalInvocation | MethodBodyExpression | MethodBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.MethodBody")
    BLOCK = hydra.core.Name("block")
    NULL_CONDITIONAL_INVOCATION = hydra.core.Name("nullConditionalInvocation")
    EXPRESSION = hydra.core.Name("expression")
    EMPTY = hydra.core.Name("empty")

class RefMethodBodyBlock(Node["Block"]):
    ...

class RefMethodBodyRef(Node["VariableReference"]):
    ...

class RefMethodBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RefMethodBodyEmpty)
    def __hash__(self):
        return hash("RefMethodBodyEmpty")

class _RefMethodBodyMeta(type):
    def __getitem__(cls, item):
        return object

class RefMethodBody(metaclass=_RefMethodBodyMeta):
    r"""RefMethodBodyBlock | RefMethodBodyRef | RefMethodBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefMethodBody")
    BLOCK = hydra.core.Name("block")
    REF = hydra.core.Name("ref")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class FormalParameterList:
    fixed: frozenlist[FixedParameter]
    array: Maybe[ParameterArray]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FormalParameterList")
    FIXED = hydra.core.Name("fixed")
    ARRAY = hydra.core.Name("array")

@dataclass(frozen=True)
class FixedParameter:
    attributes: Maybe[Attributes]
    modifier: Maybe[ParameterModifier]
    type: Type
    identifier: Identifier
    default_argument: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FixedParameter")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIER = hydra.core.Name("modifier")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")
    DEFAULT_ARGUMENT = hydra.core.Name("defaultArgument")

class ParameterModifierMode(Node["ParameterModeModifier"]):
    ...

class ParameterModifierThis:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ParameterModifierThis)
    def __hash__(self):
        return hash("ParameterModifierThis")

class _ParameterModifierMeta(type):
    def __getitem__(cls, item):
        return object

class ParameterModifier(metaclass=_ParameterModifierMeta):
    r"""ParameterModifierMode | ParameterModifierThis"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ParameterModifier")
    MODE = hydra.core.Name("mode")
    THIS = hydra.core.Name("this")

class ParameterModeModifier(Enum):
    REF = hydra.core.Name("ref")

    OUT = hydra.core.Name("out")

    IN = hydra.core.Name("in")

ParameterModeModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ParameterModeModifier")

@dataclass(frozen=True)
class ParameterArray:
    attributes: Maybe[Attributes]
    type: ArrayType
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ParameterArray")
    ATTRIBUTES = hydra.core.Name("attributes")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")

class PropertyDeclarationStandard(Node["StandardPropertyDeclaration"]):
    ...

class PropertyDeclarationRefReturn(Node["RefReturnPropertyDeclaration"]):
    ...

class _PropertyDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class PropertyDeclaration(metaclass=_PropertyDeclarationMeta):
    r"""PropertyDeclarationStandard | PropertyDeclarationRefReturn"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PropertyDeclaration")
    STANDARD = hydra.core.Name("standard")
    REF_RETURN = hydra.core.Name("refReturn")

@dataclass(frozen=True)
class StandardPropertyDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[PropertyModifier]
    type: Type
    name: MemberName
    body: PropertyBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StandardPropertyDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class RefReturnPropertyDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[PropertyModifier]
    ref_kind: RefKind
    type: Type
    name: MemberName
    body: RefPropertyBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefReturnPropertyDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    REF_KIND = hydra.core.Name("refKind")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")

class PropertyModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    STATIC = hydra.core.Name("static")

    VIRTUAL = hydra.core.Name("virtual")

    SEALED = hydra.core.Name("sealed")

    OVERRIDE = hydra.core.Name("override")

    ABSTRACT = hydra.core.Name("abstract")

    EXTERN = hydra.core.Name("extern")

    UNSAFE = hydra.core.Name("unsafe")

PropertyModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.PropertyModifier")

class PropertyBodyBlock(Node["BlockPropertyBody"]):
    ...

class PropertyBodyExpression(Node["Expression"]):
    ...

class _PropertyBodyMeta(type):
    def __getitem__(cls, item):
        return object

class PropertyBody(metaclass=_PropertyBodyMeta):
    r"""PropertyBodyBlock | PropertyBodyExpression"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PropertyBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class BlockPropertyBody:
    accessors: AccessorDeclarations
    initializer: Maybe[VariableInitializer]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BlockPropertyBody")
    ACCESSORS = hydra.core.Name("accessors")
    INITIALIZER = hydra.core.Name("initializer")

class RefPropertyBodyBlock(Node["RefGetAccessorDeclaration"]):
    ...

class RefPropertyBodyRef(Node["VariableReference"]):
    ...

class _RefPropertyBodyMeta(type):
    def __getitem__(cls, item):
        return object

class RefPropertyBody(metaclass=_RefPropertyBodyMeta):
    r"""RefPropertyBodyBlock | RefPropertyBodyRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefPropertyBody")
    BLOCK = hydra.core.Name("block")
    REF = hydra.core.Name("ref")

class AccessorDeclarationsGet(Node["Maybe[AccessorDeclaration]"]):
    ...

class AccessorDeclarationsSet(Node["Maybe[AccessorDeclaration]"]):
    ...

class _AccessorDeclarationsMeta(type):
    def __getitem__(cls, item):
        return object

class AccessorDeclarations(metaclass=_AccessorDeclarationsMeta):
    r"""AccessorDeclarationsGet | AccessorDeclarationsSet"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AccessorDeclarations")
    GET = hydra.core.Name("get")
    SET = hydra.core.Name("set")

@dataclass(frozen=True)
class AccessorDeclaration:
    attributes: Maybe[Attributes]
    modifier: Maybe[AccessorModifier]
    body: AccessorBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AccessorDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIER = hydra.core.Name("modifier")
    BODY = hydra.core.Name("body")

class AccessorModifier(Enum):
    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    PROTECTED_INTERNAL = hydra.core.Name("protectedInternal")

    INTERNAL_PROTECTED = hydra.core.Name("internalProtected")

    PROTECTED_PRIVATE = hydra.core.Name("protectedPrivate")

    PRIVATE_PROTECTED = hydra.core.Name("privateProtected")

AccessorModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.AccessorModifier")

class AccessorBodyBlock(Node["Block"]):
    ...

class AccessorBodyExpression(Node["Expression"]):
    ...

class AccessorBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AccessorBodyEmpty)
    def __hash__(self):
        return hash("AccessorBodyEmpty")

class _AccessorBodyMeta(type):
    def __getitem__(cls, item):
        return object

class AccessorBody(metaclass=_AccessorBodyMeta):
    r"""AccessorBodyBlock | AccessorBodyExpression | AccessorBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AccessorBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class RefGetAccessorDeclaration:
    attributes: Maybe[Attributes]
    modifier: Maybe[AccessorModifier]
    body: RefAccessorBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefGetAccessorDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIER = hydra.core.Name("modifier")
    BODY = hydra.core.Name("body")

class RefAccessorBodyBlock(Node["Block"]):
    ...

class RefAccessorBodyRef(Node["VariableReference"]):
    ...

class RefAccessorBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, RefAccessorBodyEmpty)
    def __hash__(self):
        return hash("RefAccessorBodyEmpty")

class _RefAccessorBodyMeta(type):
    def __getitem__(cls, item):
        return object

class RefAccessorBody(metaclass=_RefAccessorBodyMeta):
    r"""RefAccessorBodyBlock | RefAccessorBodyRef | RefAccessorBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefAccessorBody")
    BLOCK = hydra.core.Name("block")
    REF = hydra.core.Name("ref")
    EMPTY = hydra.core.Name("empty")

class EventDeclarationStandard(Node["StandardEventDeclaration"]):
    ...

class EventDeclarationAccessors(Node["AccessorsEventDeclaration"]):
    ...

class _EventDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class EventDeclaration(metaclass=_EventDeclarationMeta):
    r"""EventDeclarationStandard | EventDeclarationAccessors"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EventDeclaration")
    STANDARD = hydra.core.Name("standard")
    ACCESSORS = hydra.core.Name("accessors")

@dataclass(frozen=True)
class StandardEventDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[EventModifier]
    type: Type
    declarators: VariableDeclarators

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StandardEventDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

@dataclass(frozen=True)
class AccessorsEventDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[EventModifier]
    type: Type
    name: MemberName
    accessors: EventAccessorDeclarations

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AccessorsEventDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    ACCESSORS = hydra.core.Name("accessors")

class EventModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    STATIC = hydra.core.Name("static")

    VIRTUAL = hydra.core.Name("virtual")

    SEALED = hydra.core.Name("sealed")

    OVERRIDE = hydra.core.Name("override")

    ABSTRACT = hydra.core.Name("abstract")

    EXTERN = hydra.core.Name("extern")

    UNSAFE = hydra.core.Name("unsafe")

EventModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.EventModifier")

class EventAccessorDeclarationsAdd(Node["AddRemoveAccessorDeclaration"]):
    ...

class EventAccessorDeclarationsRemove(Node["AddRemoveAccessorDeclaration"]):
    ...

class _EventAccessorDeclarationsMeta(type):
    def __getitem__(cls, item):
        return object

class EventAccessorDeclarations(metaclass=_EventAccessorDeclarationsMeta):
    r"""EventAccessorDeclarationsAdd | EventAccessorDeclarationsRemove"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EventAccessorDeclarations")
    ADD = hydra.core.Name("add")
    REMOVE = hydra.core.Name("remove")

@dataclass(frozen=True)
class AddRemoveAccessorDeclaration:
    attributes: Maybe[Attributes]
    body: Block

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AddRemoveAccessorDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    BODY = hydra.core.Name("body")

class IndexerDeclarationStandard(Node["StandardIndexerDeclaration"]):
    ...

class IndexerDeclarationRef(Node["RefIndexerDeclaration"]):
    ...

class _IndexerDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class IndexerDeclaration(metaclass=_IndexerDeclarationMeta):
    r"""IndexerDeclarationStandard | IndexerDeclarationRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IndexerDeclaration")
    STANDARD = hydra.core.Name("standard")
    REF = hydra.core.Name("ref")

@dataclass(frozen=True)
class StandardIndexerDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[IndexerModifier]
    declarator: IndexerDeclarator
    body: IndexerBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StandardIndexerDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    DECLARATOR = hydra.core.Name("declarator")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class RefIndexerDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[IndexerModifier]
    ref_kind: RefKind
    declarator: IndexerDeclarator
    body: RefIndexerBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefIndexerDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    REF_KIND = hydra.core.Name("refKind")
    DECLARATOR = hydra.core.Name("declarator")
    BODY = hydra.core.Name("body")

class IndexerModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    VIRTUAL = hydra.core.Name("virtual")

    SEALED = hydra.core.Name("sealed")

    OVERRIDE = hydra.core.Name("override")

    ABSTRACT = hydra.core.Name("abstract")

    EXTERN = hydra.core.Name("extern")

    UNSAFE = hydra.core.Name("unsafe")

IndexerModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.IndexerModifier")

@dataclass(frozen=True)
class IndexerDeclarator:
    type: Type
    interface: Maybe[InterfaceType]
    parameters: FormalParameterList

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IndexerDeclarator")
    TYPE = hydra.core.Name("type")
    INTERFACE = hydra.core.Name("interface")
    PARAMETERS = hydra.core.Name("parameters")

class IndexerBodyBlock(Node["AccessorDeclarations"]):
    ...

class IndexerBodyExpression(Node["Expression"]):
    ...

class _IndexerBodyMeta(type):
    def __getitem__(cls, item):
        return object

class IndexerBody(metaclass=_IndexerBodyMeta):
    r"""IndexerBodyBlock | IndexerBodyExpression"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.IndexerBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")

class RefIndexerBodyBlock(Node["RefGetAccessorDeclaration"]):
    ...

class RefIndexerBodyRef(Node["VariableReference"]):
    ...

class _RefIndexerBodyMeta(type):
    def __getitem__(cls, item):
        return object

class RefIndexerBody(metaclass=_RefIndexerBodyMeta):
    r"""RefIndexerBodyBlock | RefIndexerBodyRef"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.RefIndexerBody")
    BLOCK = hydra.core.Name("block")
    REF = hydra.core.Name("ref")

@dataclass(frozen=True)
class OperatorDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[OperatorModifier]
    declarator: OperatorDeclarator
    body: OperatorBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.OperatorDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    DECLARATOR = hydra.core.Name("declarator")
    BODY = hydra.core.Name("body")

class OperatorModifier(Enum):
    PUBLIC = hydra.core.Name("public")

    STATIC = hydra.core.Name("static")

    EXTERN = hydra.core.Name("extern")

    UNSAFE = hydra.core.Name("unsafe")

OperatorModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.OperatorModifier")

class OperatorDeclaratorUnary(Node["UnaryOperatorDeclarator"]):
    ...

class OperatorDeclaratorBinary(Node["BinaryOperatorDeclarator"]):
    ...

class OperatorDeclaratorConversion(Node["ConversionOperatorDeclarator"]):
    ...

class _OperatorDeclaratorMeta(type):
    def __getitem__(cls, item):
        return object

class OperatorDeclarator(metaclass=_OperatorDeclaratorMeta):
    r"""OperatorDeclaratorUnary | OperatorDeclaratorBinary | OperatorDeclaratorConversion"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.OperatorDeclarator")
    UNARY = hydra.core.Name("unary")
    BINARY = hydra.core.Name("binary")
    CONVERSION = hydra.core.Name("conversion")

@dataclass(frozen=True)
class UnaryOperatorDeclarator:
    type: Type
    operator: OverloadableUnaryOperator
    parameter: FixedParameter

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.UnaryOperatorDeclarator")
    TYPE = hydra.core.Name("type")
    OPERATOR = hydra.core.Name("operator")
    PARAMETER = hydra.core.Name("parameter")

class OverloadableUnaryOperator(Enum):
    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

    NOT = hydra.core.Name("not")

    COMPLEMENT = hydra.core.Name("complement")

    INCREMENT = hydra.core.Name("increment")

    DECREMENT = hydra.core.Name("decrement")

    TRUE = hydra.core.Name("true")

    FALSE = hydra.core.Name("false")

OverloadableUnaryOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.OverloadableUnaryOperator")

@dataclass(frozen=True)
class BinaryOperatorDeclarator:
    type: Type
    operator: OverloadableBinaryOperator
    left: FixedParameter
    right: FixedParameter

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.BinaryOperatorDeclarator")
    TYPE = hydra.core.Name("type")
    OPERATOR = hydra.core.Name("operator")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class OverloadableBinaryOperator(Enum):
    ADD = hydra.core.Name("add")

    SUBTRACT = hydra.core.Name("subtract")

    MULTIPLY = hydra.core.Name("multiply")

    DIVIDE = hydra.core.Name("divide")

    MODULUS = hydra.core.Name("modulus")

    AND = hydra.core.Name("and")

    OR = hydra.core.Name("or")

    XOR = hydra.core.Name("xor")

    LEFT_SHIFT = hydra.core.Name("leftShift")

    RIGHT_SHIFT = hydra.core.Name("rightShift")

    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    GREATER_THAN = hydra.core.Name("greaterThan")

    LESS_THAN = hydra.core.Name("lessThan")

    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")

    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")

OverloadableBinaryOperator.TYPE_ = hydra.core.Name("hydra.csharp.syntax.OverloadableBinaryOperator")

@dataclass(frozen=True)
class ConversionOperatorDeclarator:
    kind: ConversionKind
    type: Type
    parameter: FixedParameter

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConversionOperatorDeclarator")
    KIND = hydra.core.Name("kind")
    TYPE = hydra.core.Name("type")
    PARAMETER = hydra.core.Name("parameter")

class ConversionKind(Enum):
    IMPLICIT = hydra.core.Name("implicit")

    EXPLICIT = hydra.core.Name("explicit")

ConversionKind.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConversionKind")

class OperatorBodyBlock(Node["Block"]):
    ...

class OperatorBodyExpression(Node["Expression"]):
    ...

class OperatorBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, OperatorBodyEmpty)
    def __hash__(self):
        return hash("OperatorBodyEmpty")

class _OperatorBodyMeta(type):
    def __getitem__(cls, item):
        return object

class OperatorBody(metaclass=_OperatorBodyMeta):
    r"""OperatorBodyBlock | OperatorBodyExpression | OperatorBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.OperatorBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class ConstructorDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[ConstructorModifier]
    declarator: ConstructorDeclarator
    body: ConstructorBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstructorDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    DECLARATOR = hydra.core.Name("declarator")
    BODY = hydra.core.Name("body")

class ConstructorModifier(Enum):
    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    EXTERN = hydra.core.Name("extern")

    UNSAFE = hydra.core.Name("unsafe")

ConstructorModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstructorModifier")

@dataclass(frozen=True)
class ConstructorDeclarator:
    name: Identifier
    parameters: Maybe[FormalParameterList]
    initializer: Maybe[ConstructorInitializer]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstructorDeclarator")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    INITIALIZER = hydra.core.Name("initializer")

class ConstructorInitializerBase(Node["Maybe[ArgumentList]"]):
    ...

class ConstructorInitializerThis(Node["Maybe[ArgumentList]"]):
    ...

class _ConstructorInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class ConstructorInitializer(metaclass=_ConstructorInitializerMeta):
    r"""ConstructorInitializerBase | ConstructorInitializerThis"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstructorInitializer")
    BASE = hydra.core.Name("base")
    THIS = hydra.core.Name("this")

class ConstructorBodyBlock(Node["Block"]):
    ...

class ConstructorBodyExpression(Node["Expression"]):
    ...

class ConstructorBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstructorBodyEmpty)
    def __hash__(self):
        return hash("ConstructorBodyEmpty")

class _ConstructorBodyMeta(type):
    def __getitem__(cls, item):
        return object

class ConstructorBody(metaclass=_ConstructorBodyMeta):
    r"""ConstructorBodyBlock | ConstructorBodyExpression | ConstructorBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.ConstructorBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class StaticConstructorDeclaration:
    attributes: Maybe[Attributes]
    modifiers: StaticConstructorModifiers
    name: Identifier
    body: StaticConstructorBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StaticConstructorDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class StaticConstructorModifiers:
    extern: bool
    unsafe: bool

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StaticConstructorModifiers")
    EXTERN = hydra.core.Name("extern")
    UNSAFE = hydra.core.Name("unsafe")

class StaticConstructorBodyBlock(Node["Block"]):
    ...

class StaticConstructorBodyExpression(Node["Expression"]):
    ...

class StaticConstructorBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StaticConstructorBodyEmpty)
    def __hash__(self):
        return hash("StaticConstructorBodyEmpty")

class _StaticConstructorBodyMeta(type):
    def __getitem__(cls, item):
        return object

class StaticConstructorBody(metaclass=_StaticConstructorBodyMeta):
    r"""StaticConstructorBodyBlock | StaticConstructorBodyExpression | StaticConstructorBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StaticConstructorBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class FinalizerDeclaration:
    attributes: Maybe[Attributes]
    extern: bool
    unsafe: bool
    name: Identifier
    body: FinalizerBody

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FinalizerDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    EXTERN = hydra.core.Name("extern")
    UNSAFE = hydra.core.Name("unsafe")
    NAME = hydra.core.Name("name")
    BODY = hydra.core.Name("body")

class FinalizerBodyBlock(Node["Block"]):
    ...

class FinalizerBodyExpression(Node["Expression"]):
    ...

class FinalizerBodyEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FinalizerBodyEmpty)
    def __hash__(self):
        return hash("FinalizerBodyEmpty")

class _FinalizerBodyMeta(type):
    def __getitem__(cls, item):
        return object

class FinalizerBody(metaclass=_FinalizerBodyMeta):
    r"""FinalizerBodyBlock | FinalizerBodyExpression | FinalizerBodyEmpty"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FinalizerBody")
    BLOCK = hydra.core.Name("block")
    EXPRESSION = hydra.core.Name("expression")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class StructDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[StructModifier]
    ref: bool
    partial: bool
    name: Identifier
    parameters: Maybe[TypeParameterList]
    interfaces: frozenlist[InterfaceType]
    constraints: frozenlist[TypeParameterConstraintsClause]
    body: frozenlist[StructMemberDeclaration]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StructDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    REF = hydra.core.Name("ref")
    PARTIAL = hydra.core.Name("partial")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    INTERFACES = hydra.core.Name("interfaces")
    CONSTRAINTS = hydra.core.Name("constraints")
    BODY = hydra.core.Name("body")

class StructModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    READONLY = hydra.core.Name("readonly")

    UNSAFE = hydra.core.Name("unsafe")

StructModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.StructModifier")

class StructMemberDeclarationConstant(Node["ConstantDeclaration"]):
    ...

class StructMemberDeclarationField(Node["FieldDeclaration"]):
    ...

class StructMemberDeclarationMethod(Node["MethodDeclaration"]):
    ...

class StructMemberDeclarationProperty(Node["PropertyDeclaration"]):
    ...

class StructMemberDeclarationEvent(Node["EventDeclaration"]):
    ...

class StructMemberDeclarationIndexer(Node["IndexerDeclaration"]):
    ...

class StructMemberDeclarationOperator(Node["OperatorDeclaration"]):
    ...

class StructMemberDeclarationConstructor(Node["ConstructorDeclaration"]):
    ...

class StructMemberDeclarationStaticConstructor(Node["StaticConstructorDeclaration"]):
    ...

class StructMemberDeclarationType(Node["TypeDeclaration"]):
    ...

class StructMemberDeclarationFixedSizeBuffer(Node["FixedSizeBufferDeclaration"]):
    ...

class _StructMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class StructMemberDeclaration(metaclass=_StructMemberDeclarationMeta):
    r"""StructMemberDeclarationConstant | StructMemberDeclarationField | StructMemberDeclarationMethod | StructMemberDeclarationProperty | StructMemberDeclarationEvent | StructMemberDeclarationIndexer | StructMemberDeclarationOperator | StructMemberDeclarationConstructor | StructMemberDeclarationStaticConstructor | StructMemberDeclarationType | StructMemberDeclarationFixedSizeBuffer"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.StructMemberDeclaration")
    CONSTANT = hydra.core.Name("constant")
    FIELD = hydra.core.Name("field")
    METHOD = hydra.core.Name("method")
    PROPERTY = hydra.core.Name("property")
    EVENT = hydra.core.Name("event")
    INDEXER = hydra.core.Name("indexer")
    OPERATOR = hydra.core.Name("operator")
    CONSTRUCTOR = hydra.core.Name("constructor")
    STATIC_CONSTRUCTOR = hydra.core.Name("staticConstructor")
    TYPE = hydra.core.Name("type")
    FIXED_SIZE_BUFFER = hydra.core.Name("fixedSizeBuffer")

class ArrayInitializer(Node["frozenlist[VariableInitializer]"]):
    ...

ArrayInitializer.TYPE_ = hydra.core.Name("hydra.csharp.syntax.ArrayInitializer")

class VariableInitializerExpression(Node["Expression"]):
    ...

class VariableInitializerArray(Node["ArrayInitializer"]):
    ...

class _VariableInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class VariableInitializer(metaclass=_VariableInitializerMeta):
    r"""VariableInitializerExpression | VariableInitializerArray"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.VariableInitializer")
    EXPRESSION = hydra.core.Name("expression")
    ARRAY = hydra.core.Name("array")

@dataclass(frozen=True)
class InterfaceDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[InterfaceModifier]
    partial: bool
    name: Identifier
    parameters: Maybe[VariantTypeParameters]
    base: frozenlist[InterfaceType]
    constraints: frozenlist[TypeParameterConstraintsClause]
    body: frozenlist[InterfaceMemberDeclaration]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    PARTIAL = hydra.core.Name("partial")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    BASE = hydra.core.Name("base")
    CONSTRAINTS = hydra.core.Name("constraints")
    BODY = hydra.core.Name("body")

class InterfaceModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    UNSAFE = hydra.core.Name("unsafe")

InterfaceModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceModifier")

class VariantTypeParameters(Node["frozenlist[VariantTypeParameter]"]):
    ...

VariantTypeParameters.TYPE_ = hydra.core.Name("hydra.csharp.syntax.VariantTypeParameters")

@dataclass(frozen=True)
class VariantTypeParameter:
    attributes: Maybe[Attributes]
    variance: Maybe[VarianceAnnotation]
    parameter: TypeParameter

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.VariantTypeParameter")
    ATTRIBUTES = hydra.core.Name("attributes")
    VARIANCE = hydra.core.Name("variance")
    PARAMETER = hydra.core.Name("parameter")

class VarianceAnnotation(Enum):
    IN = hydra.core.Name("in")

    OUT = hydra.core.Name("out")

VarianceAnnotation.TYPE_ = hydra.core.Name("hydra.csharp.syntax.VarianceAnnotation")

class InterfaceMemberDeclarationMethod(Node["InterfaceMethodDeclaration"]):
    ...

class InterfaceMemberDeclarationProperty(Node["InterfacePropertyDeclaration"]):
    ...

class InterfaceMemberDeclarationEvent(Node["InterfaceEventDeclaration"]):
    ...

class InterfaceMemberDeclarationIndexer(Node["InterfaceIndexerDeclaration"]):
    ...

class _InterfaceMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceMemberDeclaration(metaclass=_InterfaceMemberDeclarationMeta):
    r"""InterfaceMemberDeclarationMethod | InterfaceMemberDeclarationProperty | InterfaceMemberDeclarationEvent | InterfaceMemberDeclarationIndexer"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceMemberDeclaration")
    METHOD = hydra.core.Name("method")
    PROPERTY = hydra.core.Name("property")
    EVENT = hydra.core.Name("event")
    INDEXER = hydra.core.Name("indexer")

@dataclass(frozen=True)
class InterfaceMethodDeclaration:
    attributes: Maybe[Attributes]
    new: bool
    return_type: ReturnType
    ref_kind: Maybe[RefKind]
    header: InterfaceMethodHeader

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceMethodDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    NEW = hydra.core.Name("new")
    RETURN_TYPE = hydra.core.Name("returnType")
    REF_KIND = hydra.core.Name("refKind")
    HEADER = hydra.core.Name("header")

@dataclass(frozen=True)
class InterfaceMethodHeader:
    name: Identifier
    parameters: Maybe[FormalParameterList]
    type_parameters: Maybe[TypeParameterList]
    constraints: frozenlist[TypeParameterConstraintsClause]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceMethodHeader")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    TYPE_PARAMETERS = hydra.core.Name("typeParameters")
    CONSTRAINTS = hydra.core.Name("constraints")

@dataclass(frozen=True)
class InterfacePropertyDeclaration:
    attributes: Maybe[Attributes]
    new: bool
    ref_kind: Maybe[RefKind]
    type: Type
    name: Identifier
    accessors: InterfaceAccessors

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfacePropertyDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    NEW = hydra.core.Name("new")
    REF_KIND = hydra.core.Name("refKind")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    ACCESSORS = hydra.core.Name("accessors")

@dataclass(frozen=True)
class InterfaceAccessors:
    attributes: Maybe[Attributes]
    get: Maybe[Attributes]
    set: Maybe[Attributes]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceAccessors")
    ATTRIBUTES = hydra.core.Name("attributes")
    GET = hydra.core.Name("get")
    SET = hydra.core.Name("set")

@dataclass(frozen=True)
class InterfaceEventDeclaration:
    attributes: Maybe[Attributes]
    new: bool
    type: Type
    name: Identifier

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceEventDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    NEW = hydra.core.Name("new")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class InterfaceIndexerDeclaration:
    attributes: Maybe[Attributes]
    new: bool
    ref_kind: Maybe[RefKind]
    type: Type
    parameters: FormalParameterList
    accessors: InterfaceAccessors

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.InterfaceIndexerDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    NEW = hydra.core.Name("new")
    REF_KIND = hydra.core.Name("refKind")
    TYPE = hydra.core.Name("type")
    PARAMETERS = hydra.core.Name("parameters")
    ACCESSORS = hydra.core.Name("accessors")

@dataclass(frozen=True)
class EnumDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[EnumModifier]
    name: Identifier
    base: Maybe[EnumBase]
    body: Maybe[EnumBody]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EnumDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    NAME = hydra.core.Name("name")
    BASE = hydra.core.Name("base")
    BODY = hydra.core.Name("body")

class EnumBaseType(Node["IntegralType"]):
    ...

class EnumBaseName(Node["TypeName"]):
    ...

class _EnumBaseMeta(type):
    def __getitem__(cls, item):
        return object

class EnumBase(metaclass=_EnumBaseMeta):
    r"""EnumBaseType | EnumBaseName"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EnumBase")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")

class EnumBody(Node["frozenlist[EnumMemberDeclaration]"]):
    ...

EnumBody.TYPE_ = hydra.core.Name("hydra.csharp.syntax.EnumBody")

class EnumModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

EnumModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.EnumModifier")

@dataclass(frozen=True)
class EnumMemberDeclaration:
    attributes: Maybe[Attributes]
    name: Identifier
    value: Maybe[ConstantExpression]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.EnumMemberDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class DelegateDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[DelegateModifier]
    return_type: ReturnType
    ref_kind: Maybe[RefKind]
    ref_return_type: Maybe[Type]
    header: DelegateHeader

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DelegateDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    RETURN_TYPE = hydra.core.Name("returnType")
    REF_KIND = hydra.core.Name("refKind")
    REF_RETURN_TYPE = hydra.core.Name("refReturnType")
    HEADER = hydra.core.Name("header")

@dataclass(frozen=True)
class DelegateHeader:
    name: Identifier
    type_parameters: Maybe[VariantTypeParameters]
    parameters: Maybe[FormalParameterList]
    constraints: frozenlist[TypeParameterConstraintsClause]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.DelegateHeader")
    NAME = hydra.core.Name("name")
    TYPE_PARAMETERS = hydra.core.Name("typeParameters")
    PARAMETERS = hydra.core.Name("parameters")
    CONSTRAINTS = hydra.core.Name("constraints")

class DelegateModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    UNSAFE = hydra.core.Name("unsafe")

DelegateModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.DelegateModifier")

@dataclass(frozen=True)
class GlobalAttributeSection:
    target: Identifier
    attributes: AttributeList

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.GlobalAttributeSection")
    TARGET = hydra.core.Name("target")
    ATTRIBUTES = hydra.core.Name("attributes")

class Attributes(Node["frozenlist[AttributeSection]"]):
    ...

Attributes.TYPE_ = hydra.core.Name("hydra.csharp.syntax.Attributes")

@dataclass(frozen=True)
class AttributeSection:
    target: Maybe[AttributeTarget]
    attributes: AttributeList

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AttributeSection")
    TARGET = hydra.core.Name("target")
    ATTRIBUTES = hydra.core.Name("attributes")

class AttributeTargetIdentifier(Node["Identifier"]):
    ...

class AttributeTargetKeyword(Node["Keyword"]):
    ...

class _AttributeTargetMeta(type):
    def __getitem__(cls, item):
        return object

class AttributeTarget(metaclass=_AttributeTargetMeta):
    r"""AttributeTargetIdentifier | AttributeTargetKeyword"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AttributeTarget")
    IDENTIFIER = hydra.core.Name("identifier")
    KEYWORD = hydra.core.Name("keyword")

class AttributeList(Node["frozenlist[Attribute]"]):
    ...

AttributeList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.AttributeList")

@dataclass(frozen=True)
class Attribute:
    name: AttributeName
    arguments: Maybe[AttributeArguments]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.Attribute")
    NAME = hydra.core.Name("name")
    ARGUMENTS = hydra.core.Name("arguments")

class AttributeName(Node["TypeName"]):
    ...

AttributeName.TYPE_ = hydra.core.Name("hydra.csharp.syntax.AttributeName")

@dataclass(frozen=True)
class AttributeArguments:
    positonal: Maybe[PositionalArgumentList]
    named: Maybe[NamedArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.AttributeArguments")
    POSITONAL = hydra.core.Name("positonal")
    NAMED = hydra.core.Name("named")

class PositionalArgumentList(Node["frozenlist[PositionalArgument]"]):
    ...

PositionalArgumentList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.PositionalArgumentList")

@dataclass(frozen=True)
class PositionalArgument:
    name: Maybe[Identifier]
    value: AttributeArgumentExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PositionalArgument")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class NamedArgumentList(Node["frozenlist[NamedArgument]"]):
    ...

NamedArgumentList.TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamedArgumentList")

@dataclass(frozen=True)
class NamedArgument:
    name: Identifier
    value: AttributeArgumentExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.NamedArgument")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class AttributeArgumentExpression(Node["NonAssignmentExpression"]):
    ...

AttributeArgumentExpression.TYPE_ = hydra.core.Name("hydra.csharp.syntax.AttributeArgumentExpression")

class PointerTypeValueType(Node["Maybe[ValueType]"]):
    ...

class PointerTypePointerDepth(Node[int]):
    ...

class _PointerTypeMeta(type):
    def __getitem__(cls, item):
        return object

class PointerType(metaclass=_PointerTypeMeta):
    r"""PointerTypeValueType | PointerTypePointerDepth"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PointerType")
    VALUE_TYPE = hydra.core.Name("valueType")
    POINTER_DEPTH = hydra.core.Name("pointerDepth")

@dataclass(frozen=True)
class PointerMemberAccess:
    pointer: PrimaryExpression
    member: Identifier
    type_arguments: Maybe[TypeArgumentList]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PointerMemberAccess")
    POINTER = hydra.core.Name("pointer")
    MEMBER = hydra.core.Name("member")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class PointerElementAccess:
    pointer: PrimaryNoArrayCreationExpression
    index: Expression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.PointerElementAccess")
    POINTER = hydra.core.Name("pointer")
    INDEX = hydra.core.Name("index")

@dataclass(frozen=True)
class FixedStatement:
    pointer_type: PointerType
    declarators: frozenlist[FixedPointerDeclarator]
    statement: EmbeddedStatement

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FixedStatement")
    POINTER_TYPE = hydra.core.Name("pointerType")
    DECLARATORS = hydra.core.Name("declarators")
    STATEMENT = hydra.core.Name("statement")

class FixedPointerDeclaratorReference(Node["VariableReference"]):
    ...

class FixedPointerDeclaratorExpression(Node["Expression"]):
    ...

class _FixedPointerDeclaratorMeta(type):
    def __getitem__(cls, item):
        return object

class FixedPointerDeclarator(metaclass=_FixedPointerDeclaratorMeta):
    r"""FixedPointerDeclaratorReference | FixedPointerDeclaratorExpression"""

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FixedPointerDeclarator")
    REFERENCE = hydra.core.Name("reference")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class FixedSizeBufferDeclaration:
    attributes: Maybe[Attributes]
    modifiers: frozenlist[FixedSizeBufferModifier]
    element_type: Type
    declarators: frozenlist[FixedSizeBufferDeclarator]

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FixedSizeBufferDeclaration")
    ATTRIBUTES = hydra.core.Name("attributes")
    MODIFIERS = hydra.core.Name("modifiers")
    ELEMENT_TYPE = hydra.core.Name("elementType")
    DECLARATORS = hydra.core.Name("declarators")

class FixedSizeBufferModifier(Enum):
    NEW = hydra.core.Name("new")

    PUBLIC = hydra.core.Name("public")

    INTERNAL = hydra.core.Name("internal")

    PRIVATE = hydra.core.Name("private")

    UNSAFE = hydra.core.Name("unsafe")

FixedSizeBufferModifier.TYPE_ = hydra.core.Name("hydra.csharp.syntax.FixedSizeBufferModifier")

@dataclass(frozen=True)
class FixedSizeBufferDeclarator:
    name: Identifier
    size: ConstantExpression

    TYPE_ = hydra.core.Name("hydra.csharp.syntax.FixedSizeBufferDeclarator")
    NAME = hydra.core.Name("name")
    SIZE = hydra.core.Name("size")
