# Note: this is an automatically generated file. Do not edit.

r"""A Go syntax model, based on the Go Language Specification retrieved on 2025-02-05 from https://go.dev/ref/spec."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class AnnotatedDeclaration:
    comment: str
    declaration: TopLevelDecl

    TYPE_ = hydra.core.Name("hydra.go.syntax.AnnotatedDeclaration")
    COMMENT = hydra.core.Name("comment")
    DECLARATION = hydra.core.Name("declaration")

@dataclass(frozen=True)
class Module:
    package: PackageClause
    imports: frozenlist[ImportDecl]
    declarations: frozenlist[TopLevelDecl]

    TYPE_ = hydra.core.Name("hydra.go.syntax.Module")
    PACKAGE = hydra.core.Name("package")
    IMPORTS = hydra.core.Name("imports")
    DECLARATIONS = hydra.core.Name("declarations")

class Identifier(Node[str]):
    ...

Identifier.TYPE_ = hydra.core.Name("hydra.go.syntax.Identifier")

class IntLit(Node[int]):
    ...

IntLit.TYPE_ = hydra.core.Name("hydra.go.syntax.IntLit")

class FloatLit(Node[float]):
    ...

FloatLit.TYPE_ = hydra.core.Name("hydra.go.syntax.FloatLit")

class ImaginaryLit(Node[float]):
    ...

ImaginaryLit.TYPE_ = hydra.core.Name("hydra.go.syntax.ImaginaryLit")

class RuneLit(Node[int]):
    ...

RuneLit.TYPE_ = hydra.core.Name("hydra.go.syntax.RuneLit")

class StringLitRaw(Node["RawStringLit"]):
    ...

class StringLitInterpreted(Node["InterpretedStringLit"]):
    ...

class _StringLitMeta(type):
    def __getitem__(cls, item):
        return object

class StringLit(metaclass=_StringLitMeta):
    r"""StringLitRaw | StringLitInterpreted"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.StringLit")
    RAW = hydra.core.Name("raw")
    INTERPRETED = hydra.core.Name("interpreted")

class RawStringLit(Node[str]):
    ...

RawStringLit.TYPE_ = hydra.core.Name("hydra.go.syntax.RawStringLit")

class InterpretedStringLit(Node[str]):
    ...

InterpretedStringLit.TYPE_ = hydra.core.Name("hydra.go.syntax.InterpretedStringLit")

@dataclass(frozen=True)
class SourceFile:
    package: PackageClause
    imports: frozenlist[ImportDecl]
    declarations: frozenlist[TopLevelDecl]

    TYPE_ = hydra.core.Name("hydra.go.syntax.SourceFile")
    PACKAGE = hydra.core.Name("package")
    IMPORTS = hydra.core.Name("imports")
    DECLARATIONS = hydra.core.Name("declarations")

class PackageClause(Node["Identifier"]):
    ...

PackageClause.TYPE_ = hydra.core.Name("hydra.go.syntax.PackageClause")

class ImportDecl(Node["frozenlist[ImportSpec]"]):
    ...

ImportDecl.TYPE_ = hydra.core.Name("hydra.go.syntax.ImportDecl")

@dataclass(frozen=True)
class ImportSpec:
    alias: Maybe[ImportAlias]
    path: ImportPath

    TYPE_ = hydra.core.Name("hydra.go.syntax.ImportSpec")
    ALIAS = hydra.core.Name("alias")
    PATH = hydra.core.Name("path")

class ImportAliasDot:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ImportAliasDot)
    def __hash__(self):
        return hash("ImportAliasDot")

class ImportAliasName(Node["Identifier"]):
    ...

class _ImportAliasMeta(type):
    def __getitem__(cls, item):
        return object

class ImportAlias(metaclass=_ImportAliasMeta):
    r"""ImportAliasDot | ImportAliasName"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.ImportAlias")
    DOT = hydra.core.Name("dot")
    NAME = hydra.core.Name("name")

class ImportPath(Node["StringLit"]):
    ...

ImportPath.TYPE_ = hydra.core.Name("hydra.go.syntax.ImportPath")

class DeclarationConst(Node["ConstDecl"]):
    ...

class DeclarationType(Node["TypeDecl"]):
    ...

class DeclarationVar(Node["VarDecl"]):
    ...

class _DeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class Declaration(metaclass=_DeclarationMeta):
    r"""DeclarationConst | DeclarationType | DeclarationVar"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Declaration")
    CONST = hydra.core.Name("const")
    TYPE = hydra.core.Name("type")
    VAR = hydra.core.Name("var")

class TopLevelDeclDeclaration(Node["Declaration"]):
    ...

class TopLevelDeclFunction(Node["FunctionDecl"]):
    ...

class TopLevelDeclMethod(Node["MethodDecl"]):
    ...

class _TopLevelDeclMeta(type):
    def __getitem__(cls, item):
        return object

class TopLevelDecl(metaclass=_TopLevelDeclMeta):
    r"""TopLevelDeclDeclaration | TopLevelDeclFunction | TopLevelDeclMethod"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.TopLevelDecl")
    DECLARATION = hydra.core.Name("declaration")
    FUNCTION = hydra.core.Name("function")
    METHOD = hydra.core.Name("method")

class ConstDecl(Node["frozenlist[ConstSpec]"]):
    ...

ConstDecl.TYPE_ = hydra.core.Name("hydra.go.syntax.ConstDecl")

@dataclass(frozen=True)
class ConstSpec:
    names: frozenlist[Identifier]
    type: Maybe[Type]
    values: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.go.syntax.ConstSpec")
    NAMES = hydra.core.Name("names")
    TYPE = hydra.core.Name("type")
    VALUES = hydra.core.Name("values")

class VarDecl(Node["frozenlist[VarSpec]"]):
    ...

VarDecl.TYPE_ = hydra.core.Name("hydra.go.syntax.VarDecl")

@dataclass(frozen=True)
class VarSpec:
    names: frozenlist[Identifier]
    type: Maybe[Type]
    values: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.go.syntax.VarSpec")
    NAMES = hydra.core.Name("names")
    TYPE = hydra.core.Name("type")
    VALUES = hydra.core.Name("values")

@dataclass(frozen=True)
class ShortVarDecl:
    names: frozenlist[Identifier]
    values: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.go.syntax.ShortVarDecl")
    NAMES = hydra.core.Name("names")
    VALUES = hydra.core.Name("values")

class TypeDecl(Node["frozenlist[TypeSpec]"]):
    ...

TypeDecl.TYPE_ = hydra.core.Name("hydra.go.syntax.TypeDecl")

class TypeSpecAlias(Node["AliasDecl"]):
    ...

class TypeSpecDefinition(Node["TypeDef"]):
    ...

class _TypeSpecMeta(type):
    def __getitem__(cls, item):
        return object

class TypeSpec(metaclass=_TypeSpecMeta):
    r"""TypeSpecAlias | TypeSpecDefinition"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeSpec")
    ALIAS = hydra.core.Name("alias")
    DEFINITION = hydra.core.Name("definition")

@dataclass(frozen=True)
class AliasDecl:
    name: Identifier
    type: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.AliasDecl")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeDef:
    name: Identifier
    type_params: Maybe[TypeParameters]
    type: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeDef")
    NAME = hydra.core.Name("name")
    TYPE_PARAMS = hydra.core.Name("typeParams")
    TYPE = hydra.core.Name("type")

class TypeParameters(Node["frozenlist[TypeParamDecl]"]):
    ...

TypeParameters.TYPE_ = hydra.core.Name("hydra.go.syntax.TypeParameters")

@dataclass(frozen=True)
class TypeParamDecl:
    names: frozenlist[Identifier]
    constraint: TypeConstraint

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeParamDecl")
    NAMES = hydra.core.Name("names")
    CONSTRAINT = hydra.core.Name("constraint")

class TypeConstraint(Node["TypeElem"]):
    ...

TypeConstraint.TYPE_ = hydra.core.Name("hydra.go.syntax.TypeConstraint")

@dataclass(frozen=True)
class FunctionDecl:
    name: Identifier
    type_params: Maybe[TypeParameters]
    signature: Signature
    body: Maybe[FunctionBody]

    TYPE_ = hydra.core.Name("hydra.go.syntax.FunctionDecl")
    NAME = hydra.core.Name("name")
    TYPE_PARAMS = hydra.core.Name("typeParams")
    SIGNATURE = hydra.core.Name("signature")
    BODY = hydra.core.Name("body")

class FunctionBody(Node["Block"]):
    ...

FunctionBody.TYPE_ = hydra.core.Name("hydra.go.syntax.FunctionBody")

@dataclass(frozen=True)
class MethodDecl:
    receiver: Receiver
    name: Identifier
    signature: Signature
    body: Maybe[FunctionBody]

    TYPE_ = hydra.core.Name("hydra.go.syntax.MethodDecl")
    RECEIVER = hydra.core.Name("receiver")
    NAME = hydra.core.Name("name")
    SIGNATURE = hydra.core.Name("signature")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class Receiver:
    name: Maybe[Identifier]
    type: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.Receiver")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

class TypeName_(Node["TypeName"]):
    ...

class TypeLiteral(Node["TypeLit"]):
    ...

class TypeParen(Node["Type"]):
    ...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypeName | TypeLiteral | TypeParen"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Type")
    NAME = hydra.core.Name("name")
    LITERAL = hydra.core.Name("literal")
    PAREN = hydra.core.Name("paren")

@dataclass(frozen=True)
class TypeName:
    name: QualifiedIdent
    type_args: frozenlist[Type]

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeName")
    NAME = hydra.core.Name("name")
    TYPE_ARGS = hydra.core.Name("typeArgs")

@dataclass(frozen=True)
class QualifiedIdent:
    package: Maybe[Identifier]
    name: Identifier

    TYPE_ = hydra.core.Name("hydra.go.syntax.QualifiedIdent")
    PACKAGE = hydra.core.Name("package")
    NAME = hydra.core.Name("name")

class TypeLitArray(Node["ArrayType"]):
    ...

class TypeLitStruct(Node["StructType"]):
    ...

class TypeLitPointer(Node["PointerType"]):
    ...

class TypeLitFunction(Node["FunctionType"]):
    ...

class TypeLitInterface(Node["InterfaceType"]):
    ...

class TypeLitSlice(Node["SliceType"]):
    ...

class TypeLitMap(Node["MapType"]):
    ...

class TypeLitChannel(Node["ChannelType"]):
    ...

class _TypeLitMeta(type):
    def __getitem__(cls, item):
        return object

class TypeLit(metaclass=_TypeLitMeta):
    r"""TypeLitArray | TypeLitStruct | TypeLitPointer | TypeLitFunction | TypeLitInterface | TypeLitSlice | TypeLitMap | TypeLitChannel"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeLit")
    ARRAY = hydra.core.Name("array")
    STRUCT = hydra.core.Name("struct")
    POINTER = hydra.core.Name("pointer")
    FUNCTION = hydra.core.Name("function")
    INTERFACE = hydra.core.Name("interface")
    SLICE = hydra.core.Name("slice")
    MAP = hydra.core.Name("map")
    CHANNEL = hydra.core.Name("channel")

@dataclass(frozen=True)
class ArrayType:
    length: Expression
    element: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.ArrayType")
    LENGTH = hydra.core.Name("length")
    ELEMENT = hydra.core.Name("element")

class SliceType(Node["Type"]):
    ...

SliceType.TYPE_ = hydra.core.Name("hydra.go.syntax.SliceType")

class StructType(Node["frozenlist[FieldDecl]"]):
    ...

StructType.TYPE_ = hydra.core.Name("hydra.go.syntax.StructType")

class FieldDeclNamed(Node["NamedField"]):
    ...

class FieldDeclEmbedded(Node["EmbeddedField"]):
    ...

class _FieldDeclMeta(type):
    def __getitem__(cls, item):
        return object

class FieldDecl(metaclass=_FieldDeclMeta):
    r"""FieldDeclNamed | FieldDeclEmbedded"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.FieldDecl")
    NAMED = hydra.core.Name("named")
    EMBEDDED = hydra.core.Name("embedded")

@dataclass(frozen=True)
class NamedField:
    names: frozenlist[Identifier]
    type: Type
    tag: Maybe[Tag]

    TYPE_ = hydra.core.Name("hydra.go.syntax.NamedField")
    NAMES = hydra.core.Name("names")
    TYPE = hydra.core.Name("type")
    TAG = hydra.core.Name("tag")

@dataclass(frozen=True)
class EmbeddedField:
    pointer: bool
    type: TypeName
    tag: Maybe[Tag]

    TYPE_ = hydra.core.Name("hydra.go.syntax.EmbeddedField")
    POINTER = hydra.core.Name("pointer")
    TYPE = hydra.core.Name("type")
    TAG = hydra.core.Name("tag")

class Tag(Node["StringLit"]):
    ...

Tag.TYPE_ = hydra.core.Name("hydra.go.syntax.Tag")

class PointerType(Node["Type"]):
    ...

PointerType.TYPE_ = hydra.core.Name("hydra.go.syntax.PointerType")

class FunctionType(Node["Signature"]):
    ...

FunctionType.TYPE_ = hydra.core.Name("hydra.go.syntax.FunctionType")

@dataclass(frozen=True)
class Signature:
    parameters: Parameters
    result: Maybe[Result]

    TYPE_ = hydra.core.Name("hydra.go.syntax.Signature")
    PARAMETERS = hydra.core.Name("parameters")
    RESULT = hydra.core.Name("result")

class ResultParameters(Node["Parameters"]):
    ...

class ResultType(Node["Type"]):
    ...

class _ResultMeta(type):
    def __getitem__(cls, item):
        return object

class Result(metaclass=_ResultMeta):
    r"""ResultParameters | ResultType"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Result")
    PARAMETERS = hydra.core.Name("parameters")
    TYPE = hydra.core.Name("type")

class Parameters(Node["frozenlist[ParameterDecl]"]):
    ...

Parameters.TYPE_ = hydra.core.Name("hydra.go.syntax.Parameters")

@dataclass(frozen=True)
class ParameterDecl:
    names: frozenlist[Identifier]
    variadic: bool
    type: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.ParameterDecl")
    NAMES = hydra.core.Name("names")
    VARIADIC = hydra.core.Name("variadic")
    TYPE = hydra.core.Name("type")

class InterfaceType(Node["frozenlist[InterfaceElem]"]):
    ...

InterfaceType.TYPE_ = hydra.core.Name("hydra.go.syntax.InterfaceType")

class InterfaceElemMethod(Node["MethodElem"]):
    ...

class InterfaceElemType(Node["TypeElem"]):
    ...

class _InterfaceElemMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceElem(metaclass=_InterfaceElemMeta):
    r"""InterfaceElemMethod | InterfaceElemType"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.InterfaceElem")
    METHOD = hydra.core.Name("method")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class MethodElem:
    name: Identifier
    signature: Signature

    TYPE_ = hydra.core.Name("hydra.go.syntax.MethodElem")
    NAME = hydra.core.Name("name")
    SIGNATURE = hydra.core.Name("signature")

class TypeElem(Node["frozenlist[TypeTerm]"]):
    ...

TypeElem.TYPE_ = hydra.core.Name("hydra.go.syntax.TypeElem")

@dataclass(frozen=True)
class TypeTerm:
    underlying: bool
    type: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeTerm")
    UNDERLYING = hydra.core.Name("underlying")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class MapType:
    key: Type
    value: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.MapType")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class ChannelType:
    direction: ChannelDirection
    element: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.ChannelType")
    DIRECTION = hydra.core.Name("direction")
    ELEMENT = hydra.core.Name("element")

class ChannelDirection(Enum):
    BIDIRECTIONAL = hydra.core.Name("bidirectional")

    SEND = hydra.core.Name("send")

    RECEIVE = hydra.core.Name("receive")

ChannelDirection.TYPE_ = hydra.core.Name("hydra.go.syntax.ChannelDirection")

class ExpressionUnary(Node["UnaryExpr"]):
    ...

class ExpressionBinary(Node["BinaryExpr"]):
    ...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionUnary | ExpressionBinary"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Expression")
    UNARY = hydra.core.Name("unary")
    BINARY = hydra.core.Name("binary")

class UnaryExprPrimary(Node["PrimaryExpr"]):
    ...

class UnaryExprOp(Node["UnaryOperation"]):
    ...

class _UnaryExprMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryExpr(metaclass=_UnaryExprMeta):
    r"""UnaryExprPrimary | UnaryExprOp"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.UnaryExpr")
    PRIMARY = hydra.core.Name("primary")
    OP = hydra.core.Name("op")

@dataclass(frozen=True)
class UnaryOperation:
    op: UnaryOp
    operand: UnaryExpr

    TYPE_ = hydra.core.Name("hydra.go.syntax.UnaryOperation")
    OP = hydra.core.Name("op")
    OPERAND = hydra.core.Name("operand")

@dataclass(frozen=True)
class BinaryExpr:
    left: Expression
    op: BinaryOp
    right: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.BinaryExpr")
    LEFT = hydra.core.Name("left")
    OP = hydra.core.Name("op")
    RIGHT = hydra.core.Name("right")

class BinaryOp(Enum):
    OR = hydra.core.Name("or")

    AND = hydra.core.Name("and")

    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    LESS = hydra.core.Name("less")

    LESS_EQUAL = hydra.core.Name("lessEqual")

    GREATER = hydra.core.Name("greater")

    GREATER_EQUAL = hydra.core.Name("greaterEqual")

    ADD = hydra.core.Name("add")

    SUBTRACT = hydra.core.Name("subtract")

    BITWISE_OR = hydra.core.Name("bitwiseOr")

    BITWISE_XOR = hydra.core.Name("bitwiseXor")

    MULTIPLY = hydra.core.Name("multiply")

    DIVIDE = hydra.core.Name("divide")

    REMAINDER = hydra.core.Name("remainder")

    LEFT_SHIFT = hydra.core.Name("leftShift")

    RIGHT_SHIFT = hydra.core.Name("rightShift")

    BITWISE_AND = hydra.core.Name("bitwiseAnd")

    BIT_CLEAR = hydra.core.Name("bitClear")

BinaryOp.TYPE_ = hydra.core.Name("hydra.go.syntax.BinaryOp")

class PrimaryExprOperand(Node["Operand"]):
    ...

class PrimaryExprConversion(Node["Conversion"]):
    ...

class PrimaryExprMethodExpr(Node["MethodExpr"]):
    ...

class PrimaryExprSelector(Node["SelectorExpr"]):
    ...

class PrimaryExprIndex(Node["IndexExpr"]):
    ...

class PrimaryExprSlice(Node["SliceExpr"]):
    ...

class PrimaryExprTypeAssertion(Node["TypeAssertionExpr"]):
    ...

class PrimaryExprCall(Node["CallExpr"]):
    ...

class _PrimaryExprMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryExpr(metaclass=_PrimaryExprMeta):
    r"""PrimaryExprOperand | PrimaryExprConversion | PrimaryExprMethodExpr | PrimaryExprSelector | PrimaryExprIndex | PrimaryExprSlice | PrimaryExprTypeAssertion | PrimaryExprCall"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.PrimaryExpr")
    OPERAND = hydra.core.Name("operand")
    CONVERSION = hydra.core.Name("conversion")
    METHOD_EXPR = hydra.core.Name("methodExpr")
    SELECTOR = hydra.core.Name("selector")
    INDEX = hydra.core.Name("index")
    SLICE = hydra.core.Name("slice")
    TYPE_ASSERTION = hydra.core.Name("typeAssertion")
    CALL = hydra.core.Name("call")

@dataclass(frozen=True)
class SelectorExpr:
    expr: PrimaryExpr
    selector: Identifier

    TYPE_ = hydra.core.Name("hydra.go.syntax.SelectorExpr")
    EXPR = hydra.core.Name("expr")
    SELECTOR = hydra.core.Name("selector")

@dataclass(frozen=True)
class IndexExpr:
    expr: PrimaryExpr
    index: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.IndexExpr")
    EXPR = hydra.core.Name("expr")
    INDEX = hydra.core.Name("index")

@dataclass(frozen=True)
class SliceExpr:
    expr: PrimaryExpr
    slice: Slice

    TYPE_ = hydra.core.Name("hydra.go.syntax.SliceExpr")
    EXPR = hydra.core.Name("expr")
    SLICE = hydra.core.Name("slice")

@dataclass(frozen=True)
class TypeAssertionExpr:
    expr: PrimaryExpr
    type: Type

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeAssertionExpr")
    EXPR = hydra.core.Name("expr")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class CallExpr:
    function: PrimaryExpr
    arguments: Arguments

    TYPE_ = hydra.core.Name("hydra.go.syntax.CallExpr")
    FUNCTION = hydra.core.Name("function")
    ARGUMENTS = hydra.core.Name("arguments")

class OperandLiteral(Node["Literal"]):
    ...

class OperandName_(Node["OperandName"]):
    ...

class OperandParen(Node["Expression"]):
    ...

class _OperandMeta(type):
    def __getitem__(cls, item):
        return object

class Operand(metaclass=_OperandMeta):
    r"""OperandLiteral | OperandName | OperandParen"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Operand")
    LITERAL = hydra.core.Name("literal")
    NAME = hydra.core.Name("name")
    PAREN = hydra.core.Name("paren")

@dataclass(frozen=True)
class OperandName:
    name: QualifiedIdent
    type_args: frozenlist[Type]

    TYPE_ = hydra.core.Name("hydra.go.syntax.OperandName")
    NAME = hydra.core.Name("name")
    TYPE_ARGS = hydra.core.Name("typeArgs")

class LiteralBasic(Node["BasicLit"]):
    ...

class LiteralComposite(Node["CompositeLit"]):
    ...

class LiteralFunction(Node["FunctionLit"]):
    ...

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

class Literal(metaclass=_LiteralMeta):
    r"""LiteralBasic | LiteralComposite | LiteralFunction"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Literal")
    BASIC = hydra.core.Name("basic")
    COMPOSITE = hydra.core.Name("composite")
    FUNCTION = hydra.core.Name("function")

class BasicLitInt(Node["IntLit"]):
    ...

class BasicLitFloat(Node["FloatLit"]):
    ...

class BasicLitImaginary(Node["ImaginaryLit"]):
    ...

class BasicLitRune(Node["RuneLit"]):
    ...

class BasicLitString(Node["StringLit"]):
    ...

class _BasicLitMeta(type):
    def __getitem__(cls, item):
        return object

class BasicLit(metaclass=_BasicLitMeta):
    r"""BasicLitInt | BasicLitFloat | BasicLitImaginary | BasicLitRune | BasicLitString"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.BasicLit")
    INT = hydra.core.Name("int")
    FLOAT = hydra.core.Name("float")
    IMAGINARY = hydra.core.Name("imaginary")
    RUNE = hydra.core.Name("rune")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class CompositeLit:
    type: LiteralType
    value: LiteralValue

    TYPE_ = hydra.core.Name("hydra.go.syntax.CompositeLit")
    TYPE = hydra.core.Name("type")
    VALUE = hydra.core.Name("value")

class LiteralTypeStruct(Node["StructType"]):
    ...

class LiteralTypeArray(Node["ArrayType"]):
    ...

class LiteralTypeInferredArray(Node["Type"]):
    ...

class LiteralTypeSlice(Node["SliceType"]):
    ...

class LiteralTypeMap(Node["MapType"]):
    ...

class LiteralTypeName(Node["TypeName"]):
    ...

class _LiteralTypeMeta(type):
    def __getitem__(cls, item):
        return object

class LiteralType(metaclass=_LiteralTypeMeta):
    r"""LiteralTypeStruct | LiteralTypeArray | LiteralTypeInferredArray | LiteralTypeSlice | LiteralTypeMap | LiteralTypeName"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.LiteralType")
    STRUCT = hydra.core.Name("struct")
    ARRAY = hydra.core.Name("array")
    INFERRED_ARRAY = hydra.core.Name("inferredArray")
    SLICE = hydra.core.Name("slice")
    MAP = hydra.core.Name("map")
    NAME = hydra.core.Name("name")

class LiteralValue(Node["frozenlist[KeyedElement]"]):
    ...

LiteralValue.TYPE_ = hydra.core.Name("hydra.go.syntax.LiteralValue")

class ElementList(Node["frozenlist[KeyedElement]"]):
    ...

ElementList.TYPE_ = hydra.core.Name("hydra.go.syntax.ElementList")

@dataclass(frozen=True)
class KeyedElement:
    key: Maybe[Key]
    element: Element

    TYPE_ = hydra.core.Name("hydra.go.syntax.KeyedElement")
    KEY = hydra.core.Name("key")
    ELEMENT = hydra.core.Name("element")

class KeyField(Node["Identifier"]):
    ...

class KeyExpression(Node["Expression"]):
    ...

class KeyLiteral(Node["LiteralValue"]):
    ...

class _KeyMeta(type):
    def __getitem__(cls, item):
        return object

class Key(metaclass=_KeyMeta):
    r"""KeyField | KeyExpression | KeyLiteral"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Key")
    FIELD = hydra.core.Name("field")
    EXPRESSION = hydra.core.Name("expression")
    LITERAL = hydra.core.Name("literal")

class ElementExpression(Node["Expression"]):
    ...

class ElementLiteral(Node["LiteralValue"]):
    ...

class _ElementMeta(type):
    def __getitem__(cls, item):
        return object

class Element(metaclass=_ElementMeta):
    r"""ElementExpression | ElementLiteral"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Element")
    EXPRESSION = hydra.core.Name("expression")
    LITERAL = hydra.core.Name("literal")

@dataclass(frozen=True)
class FunctionLit:
    signature: Signature
    body: FunctionBody

    TYPE_ = hydra.core.Name("hydra.go.syntax.FunctionLit")
    SIGNATURE = hydra.core.Name("signature")
    BODY = hydra.core.Name("body")

class Selector(Node["Identifier"]):
    ...

Selector.TYPE_ = hydra.core.Name("hydra.go.syntax.Selector")

class Index(Node["frozenlist[Expression]"]):
    ...

Index.TYPE_ = hydra.core.Name("hydra.go.syntax.Index")

class SliceSimple(Node["SimpleSlice"]):
    ...

class SliceFull(Node["FullSlice"]):
    ...

class _SliceMeta(type):
    def __getitem__(cls, item):
        return object

class Slice(metaclass=_SliceMeta):
    r"""SliceSimple | SliceFull"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Slice")
    SIMPLE = hydra.core.Name("simple")
    FULL = hydra.core.Name("full")

@dataclass(frozen=True)
class SimpleSlice:
    low: Maybe[Expression]
    high: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.go.syntax.SimpleSlice")
    LOW = hydra.core.Name("low")
    HIGH = hydra.core.Name("high")

@dataclass(frozen=True)
class FullSlice:
    low: Maybe[Expression]
    high: Expression
    max: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.FullSlice")
    LOW = hydra.core.Name("low")
    HIGH = hydra.core.Name("high")
    MAX = hydra.core.Name("max")

class TypeAssertion(Node["Type"]):
    ...

TypeAssertion.TYPE_ = hydra.core.Name("hydra.go.syntax.TypeAssertion")

@dataclass(frozen=True)
class Arguments:
    type_arg: Maybe[Type]
    expressions: frozenlist[Expression]
    ellipsis: bool

    TYPE_ = hydra.core.Name("hydra.go.syntax.Arguments")
    TYPE_ARG = hydra.core.Name("typeArg")
    EXPRESSIONS = hydra.core.Name("expressions")
    ELLIPSIS = hydra.core.Name("ellipsis")

@dataclass(frozen=True)
class MethodExpr:
    receiver: Type
    method: Identifier

    TYPE_ = hydra.core.Name("hydra.go.syntax.MethodExpr")
    RECEIVER = hydra.core.Name("receiver")
    METHOD = hydra.core.Name("method")

@dataclass(frozen=True)
class Conversion:
    type: Type
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.Conversion")
    TYPE = hydra.core.Name("type")
    EXPRESSION = hydra.core.Name("expression")

class StatementDeclaration(Node["Declaration"]):
    ...

class StatementLabeled(Node["LabeledStmt"]):
    ...

class StatementSimple(Node["SimpleStmt"]):
    ...

class StatementGo(Node["GoStmt"]):
    ...

class StatementReturn(Node["ReturnStmt"]):
    ...

class StatementBreak(Node["BreakStmt"]):
    ...

class StatementContinue(Node["ContinueStmt"]):
    ...

class StatementGoto(Node["GotoStmt"]):
    ...

class StatementFallthrough(Node["FallthroughStmt"]):
    ...

class StatementBlock(Node["Block"]):
    ...

class StatementIf(Node["IfStmt"]):
    ...

class StatementSwitch(Node["SwitchStmt"]):
    ...

class StatementSelect(Node["SelectStmt"]):
    ...

class StatementFor(Node["ForStmt"]):
    ...

class StatementDefer(Node["DeferStmt"]):
    ...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementDeclaration | StatementLabeled | StatementSimple | StatementGo | StatementReturn | StatementBreak | StatementContinue | StatementGoto | StatementFallthrough | StatementBlock | StatementIf | StatementSwitch | StatementSelect | StatementFor | StatementDefer"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.Statement")
    DECLARATION = hydra.core.Name("declaration")
    LABELED = hydra.core.Name("labeled")
    SIMPLE = hydra.core.Name("simple")
    GO = hydra.core.Name("go")
    RETURN = hydra.core.Name("return")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    GOTO = hydra.core.Name("goto")
    FALLTHROUGH = hydra.core.Name("fallthrough")
    BLOCK = hydra.core.Name("block")
    IF = hydra.core.Name("if")
    SWITCH = hydra.core.Name("switch")
    SELECT = hydra.core.Name("select")
    FOR = hydra.core.Name("for")
    DEFER = hydra.core.Name("defer")

class SimpleStmtEmpty(Node["EmptyStmt"]):
    ...

class SimpleStmtExpression(Node["ExpressionStmt"]):
    ...

class SimpleStmtSend(Node["SendStmt"]):
    ...

class SimpleStmtIncDec(Node["IncDecStmt"]):
    ...

class SimpleStmtAssignment(Node["Assignment"]):
    ...

class SimpleStmtShortVarDecl(Node["ShortVarDecl"]):
    ...

class _SimpleStmtMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleStmt(metaclass=_SimpleStmtMeta):
    r"""SimpleStmtEmpty | SimpleStmtExpression | SimpleStmtSend | SimpleStmtIncDec | SimpleStmtAssignment | SimpleStmtShortVarDecl"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.SimpleStmt")
    EMPTY = hydra.core.Name("empty")
    EXPRESSION = hydra.core.Name("expression")
    SEND = hydra.core.Name("send")
    INC_DEC = hydra.core.Name("incDec")
    ASSIGNMENT = hydra.core.Name("assignment")
    SHORT_VAR_DECL = hydra.core.Name("shortVarDecl")

class EmptyStmt(Node[None]):
    ...

EmptyStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.EmptyStmt")

@dataclass(frozen=True)
class LabeledStmt:
    label: Identifier
    statement: Statement

    TYPE_ = hydra.core.Name("hydra.go.syntax.LabeledStmt")
    LABEL = hydra.core.Name("label")
    STATEMENT = hydra.core.Name("statement")

class ExpressionStmt(Node["Expression"]):
    ...

ExpressionStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.ExpressionStmt")

@dataclass(frozen=True)
class SendStmt:
    channel: Expression
    value: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.SendStmt")
    CHANNEL = hydra.core.Name("channel")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class IncDecStmt:
    expression: Expression
    increment: bool

    TYPE_ = hydra.core.Name("hydra.go.syntax.IncDecStmt")
    EXPRESSION = hydra.core.Name("expression")
    INCREMENT = hydra.core.Name("increment")

@dataclass(frozen=True)
class Assignment:
    lhs: frozenlist[Expression]
    op: AssignOp
    rhs: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.go.syntax.Assignment")
    LHS = hydra.core.Name("lhs")
    OP = hydra.core.Name("op")
    RHS = hydra.core.Name("rhs")

class AssignOpSimple:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AssignOpSimple)
    def __hash__(self):
        return hash("AssignOpSimple")

class AssignOpAdd(Node["AddOp"]):
    ...

class AssignOpMul(Node["MulOp"]):
    ...

class _AssignOpMeta(type):
    def __getitem__(cls, item):
        return object

class AssignOp(metaclass=_AssignOpMeta):
    r"""AssignOpSimple | AssignOpAdd | AssignOpMul"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.AssignOp")
    SIMPLE = hydra.core.Name("simple")
    ADD = hydra.core.Name("add")
    MUL = hydra.core.Name("mul")

@dataclass(frozen=True)
class IfStmt:
    init: Maybe[SimpleStmt]
    condition: Expression
    then: Block
    else_: Maybe[ElseClause]

    TYPE_ = hydra.core.Name("hydra.go.syntax.IfStmt")
    INIT = hydra.core.Name("init")
    CONDITION = hydra.core.Name("condition")
    THEN = hydra.core.Name("then")
    ELSE = hydra.core.Name("else")

class ElseClauseIf(Node["IfStmt"]):
    ...

class ElseClauseBlock(Node["Block"]):
    ...

class _ElseClauseMeta(type):
    def __getitem__(cls, item):
        return object

class ElseClause(metaclass=_ElseClauseMeta):
    r"""ElseClauseIf | ElseClauseBlock"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.ElseClause")
    IF = hydra.core.Name("if")
    BLOCK = hydra.core.Name("block")

class SwitchStmtExpression(Node["ExprSwitchStmt"]):
    ...

class SwitchStmtType(Node["TypeSwitchStmt"]):
    ...

class _SwitchStmtMeta(type):
    def __getitem__(cls, item):
        return object

class SwitchStmt(metaclass=_SwitchStmtMeta):
    r"""SwitchStmtExpression | SwitchStmtType"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.SwitchStmt")
    EXPRESSION = hydra.core.Name("expression")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class ExprSwitchStmt:
    init: Maybe[SimpleStmt]
    expression: Maybe[Expression]
    cases: frozenlist[ExprCaseClause]

    TYPE_ = hydra.core.Name("hydra.go.syntax.ExprSwitchStmt")
    INIT = hydra.core.Name("init")
    EXPRESSION = hydra.core.Name("expression")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class ExprCaseClause:
    case: Maybe[frozenlist[Expression]]
    statements: frozenlist[Statement]

    TYPE_ = hydra.core.Name("hydra.go.syntax.ExprCaseClause")
    CASE = hydra.core.Name("case")
    STATEMENTS = hydra.core.Name("statements")

@dataclass(frozen=True)
class TypeSwitchStmt:
    init: Maybe[SimpleStmt]
    guard: TypeSwitchGuard
    cases: frozenlist[TypeCaseClause]

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeSwitchStmt")
    INIT = hydra.core.Name("init")
    GUARD = hydra.core.Name("guard")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class TypeSwitchGuard:
    name: Maybe[Identifier]
    expression: PrimaryExpr

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeSwitchGuard")
    NAME = hydra.core.Name("name")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class TypeCaseClause:
    case: Maybe[frozenlist[Type]]
    statements: frozenlist[Statement]

    TYPE_ = hydra.core.Name("hydra.go.syntax.TypeCaseClause")
    CASE = hydra.core.Name("case")
    STATEMENTS = hydra.core.Name("statements")

@dataclass(frozen=True)
class ForStmt:
    clause: Maybe[ForClauseOrRange]
    body: Block

    TYPE_ = hydra.core.Name("hydra.go.syntax.ForStmt")
    CLAUSE = hydra.core.Name("clause")
    BODY = hydra.core.Name("body")

class ForClauseOrRangeCondition(Node["Expression"]):
    ...

class ForClauseOrRangeClause(Node["ForClause"]):
    ...

class ForClauseOrRangeRange(Node["RangeClause"]):
    ...

class _ForClauseOrRangeMeta(type):
    def __getitem__(cls, item):
        return object

class ForClauseOrRange(metaclass=_ForClauseOrRangeMeta):
    r"""ForClauseOrRangeCondition | ForClauseOrRangeClause | ForClauseOrRangeRange"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.ForClauseOrRange")
    CONDITION = hydra.core.Name("condition")
    CLAUSE = hydra.core.Name("clause")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class ForClause:
    init: Maybe[SimpleStmt]
    condition: Maybe[Expression]
    post: Maybe[SimpleStmt]

    TYPE_ = hydra.core.Name("hydra.go.syntax.ForClause")
    INIT = hydra.core.Name("init")
    CONDITION = hydra.core.Name("condition")
    POST = hydra.core.Name("post")

@dataclass(frozen=True)
class RangeClause:
    vars: Maybe[RangeVars]
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.RangeClause")
    VARS = hydra.core.Name("vars")
    EXPRESSION = hydra.core.Name("expression")

class RangeVarsAssign(Node["frozenlist[Expression]"]):
    ...

class RangeVarsDeclare(Node["frozenlist[Identifier]"]):
    ...

class _RangeVarsMeta(type):
    def __getitem__(cls, item):
        return object

class RangeVars(metaclass=_RangeVarsMeta):
    r"""RangeVarsAssign | RangeVarsDeclare"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.RangeVars")
    ASSIGN = hydra.core.Name("assign")
    DECLARE = hydra.core.Name("declare")

class GoStmt(Node["Expression"]):
    ...

GoStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.GoStmt")

class SelectStmt(Node["frozenlist[CommClause]"]):
    ...

SelectStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.SelectStmt")

@dataclass(frozen=True)
class CommClause:
    case: CommCase
    statements: frozenlist[Statement]

    TYPE_ = hydra.core.Name("hydra.go.syntax.CommClause")
    CASE = hydra.core.Name("case")
    STATEMENTS = hydra.core.Name("statements")

class CommCaseSend(Node["SendStmt"]):
    ...

class CommCaseReceive(Node["ReceiveCase"]):
    ...

class CommCaseDefault:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CommCaseDefault)
    def __hash__(self):
        return hash("CommCaseDefault")

class _CommCaseMeta(type):
    def __getitem__(cls, item):
        return object

class CommCase(metaclass=_CommCaseMeta):
    r"""CommCaseSend | CommCaseReceive | CommCaseDefault"""

    TYPE_ = hydra.core.Name("hydra.go.syntax.CommCase")
    SEND = hydra.core.Name("send")
    RECEIVE = hydra.core.Name("receive")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class ReceiveCase:
    vars: Maybe[RangeVars]
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.go.syntax.ReceiveCase")
    VARS = hydra.core.Name("vars")
    EXPRESSION = hydra.core.Name("expression")

class ReturnStmt(Node["frozenlist[Expression]"]):
    ...

ReturnStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.ReturnStmt")

class BreakStmt(Node["Maybe[Identifier]"]):
    ...

BreakStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.BreakStmt")

class ContinueStmt(Node["Maybe[Identifier]"]):
    ...

ContinueStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.ContinueStmt")

class GotoStmt(Node["Identifier"]):
    ...

GotoStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.GotoStmt")

class FallthroughStmt(Node[None]):
    ...

FallthroughStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.FallthroughStmt")

class DeferStmt(Node["Expression"]):
    ...

DeferStmt.TYPE_ = hydra.core.Name("hydra.go.syntax.DeferStmt")

class Block(Node["frozenlist[Statement]"]):
    ...

Block.TYPE_ = hydra.core.Name("hydra.go.syntax.Block")

class UnaryOp(Enum):
    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

    NOT = hydra.core.Name("not")

    XOR = hydra.core.Name("xor")

    DEREF = hydra.core.Name("deref")

    ADDRESS_OF = hydra.core.Name("addressOf")

    RECEIVE = hydra.core.Name("receive")

UnaryOp.TYPE_ = hydra.core.Name("hydra.go.syntax.UnaryOp")

class MulOp(Enum):
    MULTIPLY = hydra.core.Name("multiply")

    DIVIDE = hydra.core.Name("divide")

    REMAINDER = hydra.core.Name("remainder")

    LEFT_SHIFT = hydra.core.Name("leftShift")

    RIGHT_SHIFT = hydra.core.Name("rightShift")

    BITWISE_AND = hydra.core.Name("bitwiseAnd")

    BIT_CLEAR = hydra.core.Name("bitClear")

MulOp.TYPE_ = hydra.core.Name("hydra.go.syntax.MulOp")

class AddOp(Enum):
    ADD = hydra.core.Name("add")

    SUBTRACT = hydra.core.Name("subtract")

    BITWISE_OR = hydra.core.Name("bitwiseOr")

    BITWISE_XOR = hydra.core.Name("bitwiseXor")

AddOp.TYPE_ = hydra.core.Name("hydra.go.syntax.AddOp")

class RelOp(Enum):
    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    LESS = hydra.core.Name("less")

    LESS_EQUAL = hydra.core.Name("lessEqual")

    GREATER = hydra.core.Name("greater")

    GREATER_EQUAL = hydra.core.Name("greaterEqual")

RelOp.TYPE_ = hydra.core.Name("hydra.go.syntax.RelOp")
