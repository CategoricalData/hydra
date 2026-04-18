# Note: this is an automatically generated file. Do not edit.

r"""A C++ syntax model, focusing on features for representing algebraic data types and declarative computations."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class AccessSpecifier(Enum):
    PUBLIC = hydra.core.Name("public")

    PROTECTED = hydra.core.Name("protected")

    PRIVATE = hydra.core.Name("private")

    NONE = hydra.core.Name("none")

AccessSpecifier.TYPE_ = hydra.core.Name("hydra.cpp.syntax.AccessSpecifier")

@dataclass(frozen=True)
class Program:
    preprocessor_directives: frozenlist[PreprocessorDirective]
    includes: frozenlist[IncludeDirective]
    declarations: frozenlist[Declaration]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Program")
    PREPROCESSOR_DIRECTIVES = hydra.core.Name("preprocessorDirectives")
    INCLUDES = hydra.core.Name("includes")
    DECLARATIONS = hydra.core.Name("declarations")

@dataclass(frozen=True)
class IncludeDirective:
    name: str
    is_system: bool

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.IncludeDirective")
    NAME = hydra.core.Name("name")
    IS_SYSTEM = hydra.core.Name("isSystem")

class DeclarationPreprocessor(Node["PreprocessorDirective"]):
    ...

class DeclarationClass(Node["ClassDeclaration"]):
    ...

class DeclarationFunction(Node["FunctionDeclaration"]):
    ...

class DeclarationVariable(Node["VariableDeclaration"]):
    ...

class DeclarationTypedef(Node["TypedefDeclaration"]):
    ...

class DeclarationNamespace(Node["NamespaceDeclaration"]):
    ...

class DeclarationTemplate(Node["TemplateDeclaration"]):
    ...

class _DeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class Declaration(metaclass=_DeclarationMeta):
    r"""DeclarationPreprocessor | DeclarationClass | DeclarationFunction | DeclarationVariable | DeclarationTypedef | DeclarationNamespace | DeclarationTemplate"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Declaration")
    PREPROCESSOR = hydra.core.Name("preprocessor")
    CLASS = hydra.core.Name("class")
    FUNCTION = hydra.core.Name("function")
    VARIABLE = hydra.core.Name("variable")
    TYPEDEF = hydra.core.Name("typedef")
    NAMESPACE = hydra.core.Name("namespace")
    TEMPLATE = hydra.core.Name("template")

@dataclass(frozen=True)
class NamespaceDeclaration:
    name: str
    declarations: frozenlist[Declaration]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.NamespaceDeclaration")
    NAME = hydra.core.Name("name")
    DECLARATIONS = hydra.core.Name("declarations")

@dataclass(frozen=True)
class TypedefDeclaration:
    name: str
    type: TypeExpression
    is_using: bool

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TypedefDeclaration")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    IS_USING = hydra.core.Name("isUsing")

@dataclass(frozen=True)
class ClassDeclaration:
    specifier: ClassSpecifier
    body: Maybe[ClassBody]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ClassDeclaration")
    SPECIFIER = hydra.core.Name("specifier")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class TemplateDeclaration:
    inline: bool
    parameters: frozenlist[str]
    declaration: Declaration

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TemplateDeclaration")
    INLINE = hydra.core.Name("inline")
    PARAMETERS = hydra.core.Name("parameters")
    DECLARATION = hydra.core.Name("declaration")

class PreprocessorDirectiveInclude(Node["IncludeDirective"]):
    ...

class PreprocessorDirectivePragma(Node["PragmaDirective"]):
    ...

class PreprocessorDirectiveDefine(Node["DefineDirective"]):
    ...

class PreprocessorDirectiveUndef(Node["UndefDirective"]):
    ...

class PreprocessorDirectiveIfdef(Node["IfdefDirective"]):
    ...

class PreprocessorDirectiveIfndef(Node["IfndefDirective"]):
    ...

class PreprocessorDirectiveIf(Node["IfDirective"]):
    ...

class PreprocessorDirectiveElif(Node["ElifDirective"]):
    ...

class PreprocessorDirectiveElse(Node["ElseDirective"]):
    ...

class PreprocessorDirectiveEndif(Node["EndifDirective"]):
    ...

class PreprocessorDirectiveLine(Node["LineDirective"]):
    ...

class PreprocessorDirectiveError(Node["ErrorDirective"]):
    ...

class PreprocessorDirectiveWarning(Node["WarningDirective"]):
    ...

class _PreprocessorDirectiveMeta(type):
    def __getitem__(cls, item):
        return object

class PreprocessorDirective(metaclass=_PreprocessorDirectiveMeta):
    r"""PreprocessorDirectiveInclude | PreprocessorDirectivePragma | PreprocessorDirectiveDefine | PreprocessorDirectiveUndef | PreprocessorDirectiveIfdef | PreprocessorDirectiveIfndef | PreprocessorDirectiveIf | PreprocessorDirectiveElif | PreprocessorDirectiveElse | PreprocessorDirectiveEndif | PreprocessorDirectiveLine | PreprocessorDirectiveError | PreprocessorDirectiveWarning"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.PreprocessorDirective")
    INCLUDE = hydra.core.Name("include")
    PRAGMA = hydra.core.Name("pragma")
    DEFINE = hydra.core.Name("define")
    UNDEF = hydra.core.Name("undef")
    IFDEF = hydra.core.Name("ifdef")
    IFNDEF = hydra.core.Name("ifndef")
    IF = hydra.core.Name("if")
    ELIF = hydra.core.Name("elif")
    ELSE = hydra.core.Name("else")
    ENDIF = hydra.core.Name("endif")
    LINE = hydra.core.Name("line")
    ERROR = hydra.core.Name("error")
    WARNING = hydra.core.Name("warning")

@dataclass(frozen=True)
class PragmaDirective:
    content: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.PragmaDirective")
    CONTENT = hydra.core.Name("content")

@dataclass(frozen=True)
class DefineDirective:
    name: str
    parameters: Maybe[frozenlist[str]]
    replacement: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.DefineDirective")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    REPLACEMENT = hydra.core.Name("replacement")

@dataclass(frozen=True)
class UndefDirective:
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.UndefDirective")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class IfdefDirective:
    identifier: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.IfdefDirective")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class IfndefDirective:
    identifier: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.IfndefDirective")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class IfDirective:
    condition: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.IfDirective")
    CONDITION = hydra.core.Name("condition")

@dataclass(frozen=True)
class ElifDirective:
    condition: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ElifDirective")
    CONDITION = hydra.core.Name("condition")

ElseDirective: TypeAlias = "None"

EndifDirective: TypeAlias = "None"

@dataclass(frozen=True)
class LineDirective:
    line_number: int
    filename: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LineDirective")
    LINE_NUMBER = hydra.core.Name("lineNumber")
    FILENAME = hydra.core.Name("filename")

@dataclass(frozen=True)
class ErrorDirective:
    message: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ErrorDirective")
    MESSAGE = hydra.core.Name("message")

@dataclass(frozen=True)
class WarningDirective:
    message: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.WarningDirective")
    MESSAGE = hydra.core.Name("message")

@dataclass(frozen=True)
class ClassSpecifier:
    key: ClassKey
    name: str
    inheritance: frozenlist[BaseSpecifier]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ClassSpecifier")
    KEY = hydra.core.Name("key")
    NAME = hydra.core.Name("name")
    INHERITANCE = hydra.core.Name("inheritance")

class ClassKey(Enum):
    CLASS = hydra.core.Name("class")

    ENUM = hydra.core.Name("enum")

    ENUM_CLASS = hydra.core.Name("enumClass")

    STRUCT = hydra.core.Name("struct")

ClassKey.TYPE_ = hydra.core.Name("hydra.cpp.syntax.ClassKey")

@dataclass(frozen=True)
class BaseSpecifier:
    access: AccessSpecifier
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.BaseSpecifier")
    ACCESS = hydra.core.Name("access")
    NAME = hydra.core.Name("name")

class ClassBody(Node["frozenlist[MemberSpecification]"]):
    ...

ClassBody.TYPE_ = hydra.core.Name("hydra.cpp.syntax.ClassBody")

class MemberSpecificationAccessLabel(Node["AccessSpecifier"]):
    ...

class MemberSpecificationMember(Node["MemberDeclaration"]):
    ...

class _MemberSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class MemberSpecification(metaclass=_MemberSpecificationMeta):
    r"""MemberSpecificationAccessLabel | MemberSpecificationMember"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MemberSpecification")
    ACCESS_LABEL = hydra.core.Name("accessLabel")
    MEMBER = hydra.core.Name("member")

class MemberDeclarationFunction(Node["FunctionDeclaration"]):
    ...

class MemberDeclarationVariable(Node["VariableDeclaration"]):
    ...

class MemberDeclarationConstructor(Node["ConstructorDeclaration"]):
    ...

class MemberDeclarationDestructor(Node["DestructorDeclaration"]):
    ...

class MemberDeclarationNestedClass(Node["ClassDeclaration"]):
    ...

class MemberDeclarationTemplate(Node["TemplateDeclaration"]):
    ...

class _MemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class MemberDeclaration(metaclass=_MemberDeclarationMeta):
    r"""MemberDeclarationFunction | MemberDeclarationVariable | MemberDeclarationConstructor | MemberDeclarationDestructor | MemberDeclarationNestedClass | MemberDeclarationTemplate"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MemberDeclaration")
    FUNCTION = hydra.core.Name("function")
    VARIABLE = hydra.core.Name("variable")
    CONSTRUCTOR = hydra.core.Name("constructor")
    DESTRUCTOR = hydra.core.Name("destructor")
    NESTED_CLASS = hydra.core.Name("nestedClass")
    TEMPLATE = hydra.core.Name("template")

@dataclass(frozen=True)
class ConstructorDeclaration:
    name: str
    parameters: frozenlist[Parameter]
    initializers: frozenlist[MemInitializer]
    body: FunctionBody

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ConstructorDeclaration")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    INITIALIZERS = hydra.core.Name("initializers")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class MemInitializer:
    name: str
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MemInitializer")
    NAME = hydra.core.Name("name")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class DestructorDeclaration:
    prefix_specifiers: frozenlist[FunctionSpecifierPrefix]
    name: str
    suffix_specifiers: frozenlist[FunctionSpecifierSuffix]
    body: FunctionBody

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.DestructorDeclaration")
    PREFIX_SPECIFIERS = hydra.core.Name("prefixSpecifiers")
    NAME = hydra.core.Name("name")
    SUFFIX_SPECIFIERS = hydra.core.Name("suffixSpecifiers")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class FunctionDeclaration:
    prefix_specifiers: frozenlist[FunctionSpecifierPrefix]
    return_type: TypeExpression
    name: str
    parameters: frozenlist[Parameter]
    suffix_specifiers: frozenlist[FunctionSpecifierSuffix]
    body: FunctionBody

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionDeclaration")
    PREFIX_SPECIFIERS = hydra.core.Name("prefixSpecifiers")
    RETURN_TYPE = hydra.core.Name("returnType")
    NAME = hydra.core.Name("name")
    PARAMETERS = hydra.core.Name("parameters")
    SUFFIX_SPECIFIERS = hydra.core.Name("suffixSpecifiers")
    BODY = hydra.core.Name("body")

class FunctionSpecifierPrefix(Enum):
    INLINE = hydra.core.Name("inline")

    VIRTUAL = hydra.core.Name("virtual")

    STATIC = hydra.core.Name("static")

    EXPLICIT = hydra.core.Name("explicit")

FunctionSpecifierPrefix.TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionSpecifierPrefix")

class FunctionSpecifierSuffix(Enum):
    CONST = hydra.core.Name("const")

    NOEXCEPT = hydra.core.Name("noexcept")

    OVERRIDE = hydra.core.Name("override")

    FINAL = hydra.core.Name("final")

FunctionSpecifierSuffix.TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionSpecifierSuffix")

@dataclass(frozen=True)
class Parameter:
    type: TypeExpression
    name: str
    unnamed: bool
    default_value: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Parameter")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    UNNAMED = hydra.core.Name("unnamed")
    DEFAULT_VALUE = hydra.core.Name("defaultValue")

class FunctionBodyCompound(Node["CompoundStatement"]):
    ...

class FunctionBodyDeclaration:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FunctionBodyDeclaration)
    def __hash__(self):
        return hash("FunctionBodyDeclaration")

class FunctionBodyPure:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FunctionBodyPure)
    def __hash__(self):
        return hash("FunctionBodyPure")

class FunctionBodyDefault:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FunctionBodyDefault)
    def __hash__(self):
        return hash("FunctionBodyDefault")

class _FunctionBodyMeta(type):
    def __getitem__(cls, item):
        return object

class FunctionBody(metaclass=_FunctionBodyMeta):
    r"""FunctionBodyCompound | FunctionBodyDeclaration | FunctionBodyPure | FunctionBodyDefault"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionBody")
    COMPOUND = hydra.core.Name("compound")
    DECLARATION = hydra.core.Name("declaration")
    PURE = hydra.core.Name("pure")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class VariableDeclaration:
    type: Maybe[TypeExpression]
    name: str
    initializer: Maybe[Expression]
    is_auto: bool

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.VariableDeclaration")
    TYPE = hydra.core.Name("type")
    NAME = hydra.core.Name("name")
    INITIALIZER = hydra.core.Name("initializer")
    IS_AUTO = hydra.core.Name("isAuto")

@dataclass(frozen=True)
class VariantDeclaration:
    types: frozenlist[TypeExpression]
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.VariantDeclaration")
    TYPES = hydra.core.Name("types")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class ProductDeclaration:
    name: str
    fields: frozenlist[VariableDeclaration]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ProductDeclaration")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")

class ContainerDeclarationList(Node["ListDeclaration"]):
    ...

class ContainerDeclarationMap(Node["MapDeclaration"]):
    ...

class ContainerDeclarationSet(Node["SetDeclaration"]):
    ...

class ContainerDeclarationOptional(Node["OptionalDeclaration"]):
    ...

class _ContainerDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ContainerDeclaration(metaclass=_ContainerDeclarationMeta):
    r"""ContainerDeclarationList | ContainerDeclarationMap | ContainerDeclarationSet | ContainerDeclarationOptional"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ContainerDeclaration")
    LIST = hydra.core.Name("list")
    MAP = hydra.core.Name("map")
    SET = hydra.core.Name("set")
    OPTIONAL = hydra.core.Name("optional")

@dataclass(frozen=True)
class ListDeclaration:
    element_type: TypeExpression
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ListDeclaration")
    ELEMENT_TYPE = hydra.core.Name("elementType")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class MapDeclaration:
    key_type: TypeExpression
    value_type: TypeExpression
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MapDeclaration")
    KEY_TYPE = hydra.core.Name("keyType")
    VALUE_TYPE = hydra.core.Name("valueType")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class SetDeclaration:
    element_type: TypeExpression
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.SetDeclaration")
    ELEMENT_TYPE = hydra.core.Name("elementType")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class OptionalDeclaration:
    value_type: TypeExpression
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.OptionalDeclaration")
    VALUE_TYPE = hydra.core.Name("valueType")
    NAME = hydra.core.Name("name")

class ExpressionAssignment(Node["AssignmentExpression"]):
    ...

class ExpressionComma(Node["CommaExpression"]):
    ...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionAssignment | ExpressionComma"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Expression")
    ASSIGNMENT = hydra.core.Name("assignment")
    COMMA = hydra.core.Name("comma")

@dataclass(frozen=True)
class CommaExpression:
    left: Expression
    right: AssignmentExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.CommaExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class AssignmentExpressionConditional(Node["ConditionalExpression"]):
    ...

class AssignmentExpressionAssignment(Node["ExplicitAssignment"]):
    ...

class _AssignmentExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AssignmentExpression(metaclass=_AssignmentExpressionMeta):
    r"""AssignmentExpressionConditional | AssignmentExpressionAssignment"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.AssignmentExpression")
    CONDITIONAL = hydra.core.Name("conditional")
    ASSIGNMENT = hydra.core.Name("assignment")

@dataclass(frozen=True)
class ExplicitAssignment:
    left: LogicalOrExpression
    op: AssignmentOperator
    right: AssignmentExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ExplicitAssignment")
    LEFT = hydra.core.Name("left")
    OP = hydra.core.Name("op")
    RIGHT = hydra.core.Name("right")

class AssignmentOperator(Enum):
    ASSIGN = hydra.core.Name("assign")

    PLUS_ASSIGN = hydra.core.Name("plusAssign")

    MINUS_ASSIGN = hydra.core.Name("minusAssign")

    MULTIPLY_ASSIGN = hydra.core.Name("multiplyAssign")

    DIVIDE_ASSIGN = hydra.core.Name("divideAssign")

    MODULO_ASSIGN = hydra.core.Name("moduloAssign")

    LEFT_SHIFT_ASSIGN = hydra.core.Name("leftShiftAssign")

    RIGHT_SHIFT_ASSIGN = hydra.core.Name("rightShiftAssign")

    BITWISE_AND_ASSIGN = hydra.core.Name("bitwiseAndAssign")

    BITWISE_XOR_ASSIGN = hydra.core.Name("bitwiseXorAssign")

    BITWISE_OR_ASSIGN = hydra.core.Name("bitwiseOrAssign")

AssignmentOperator.TYPE_ = hydra.core.Name("hydra.cpp.syntax.AssignmentOperator")

class ConditionalExpressionLogicalOr(Node["LogicalOrExpression"]):
    ...

class ConditionalExpressionTernary(Node["TernaryExpression"]):
    ...

class _ConditionalExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ConditionalExpression(metaclass=_ConditionalExpressionMeta):
    r"""ConditionalExpressionLogicalOr | ConditionalExpressionTernary"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ConditionalExpression")
    LOGICAL_OR = hydra.core.Name("logicalOr")
    TERNARY = hydra.core.Name("ternary")

@dataclass(frozen=True)
class TernaryExpression:
    condition: LogicalOrExpression
    true_expr: Expression
    false_expr: ConditionalExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TernaryExpression")
    CONDITION = hydra.core.Name("condition")
    TRUE_EXPR = hydra.core.Name("trueExpr")
    FALSE_EXPR = hydra.core.Name("falseExpr")

class LogicalOrExpressionLogicalAnd(Node["LogicalAndExpression"]):
    ...

class LogicalOrExpressionLogicalOr(Node["LogicalOrOperation"]):
    ...

class _LogicalOrExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class LogicalOrExpression(metaclass=_LogicalOrExpressionMeta):
    r"""LogicalOrExpressionLogicalAnd | LogicalOrExpressionLogicalOr"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LogicalOrExpression")
    LOGICAL_AND = hydra.core.Name("logicalAnd")
    LOGICAL_OR = hydra.core.Name("logicalOr")

@dataclass(frozen=True)
class LogicalOrOperation:
    left: LogicalOrExpression
    right: LogicalAndExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LogicalOrOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class LogicalAndExpressionInclusiveOr(Node["InclusiveOrExpression"]):
    ...

class LogicalAndExpressionLogicalAnd(Node["LogicalAndOperation"]):
    ...

class _LogicalAndExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class LogicalAndExpression(metaclass=_LogicalAndExpressionMeta):
    r"""LogicalAndExpressionInclusiveOr | LogicalAndExpressionLogicalAnd"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LogicalAndExpression")
    INCLUSIVE_OR = hydra.core.Name("inclusiveOr")
    LOGICAL_AND = hydra.core.Name("logicalAnd")

@dataclass(frozen=True)
class LogicalAndOperation:
    left: LogicalAndExpression
    right: InclusiveOrExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LogicalAndOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class InclusiveOrExpressionExclusiveOr(Node["ExclusiveOrExpression"]):
    ...

class InclusiveOrExpressionBitwiseOr(Node["BitwiseOrOperation"]):
    ...

class _InclusiveOrExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class InclusiveOrExpression(metaclass=_InclusiveOrExpressionMeta):
    r"""InclusiveOrExpressionExclusiveOr | InclusiveOrExpressionBitwiseOr"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.InclusiveOrExpression")
    EXCLUSIVE_OR = hydra.core.Name("exclusiveOr")
    BITWISE_OR = hydra.core.Name("bitwiseOr")

@dataclass(frozen=True)
class BitwiseOrOperation:
    left: InclusiveOrExpression
    right: ExclusiveOrExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.BitwiseOrOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class ExclusiveOrExpressionAnd(Node["AndExpression"]):
    ...

class ExclusiveOrExpressionBitwiseXor(Node["BitwiseXorOperation"]):
    ...

class _ExclusiveOrExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ExclusiveOrExpression(metaclass=_ExclusiveOrExpressionMeta):
    r"""ExclusiveOrExpressionAnd | ExclusiveOrExpressionBitwiseXor"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ExclusiveOrExpression")
    AND = hydra.core.Name("and")
    BITWISE_XOR = hydra.core.Name("bitwiseXor")

@dataclass(frozen=True)
class BitwiseXorOperation:
    left: ExclusiveOrExpression
    right: AndExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.BitwiseXorOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class AndExpressionEquality(Node["EqualityExpression"]):
    ...

class AndExpressionBitwiseAnd(Node["BitwiseAndOperation"]):
    ...

class _AndExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AndExpression(metaclass=_AndExpressionMeta):
    r"""AndExpressionEquality | AndExpressionBitwiseAnd"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.AndExpression")
    EQUALITY = hydra.core.Name("equality")
    BITWISE_AND = hydra.core.Name("bitwiseAnd")

@dataclass(frozen=True)
class BitwiseAndOperation:
    left: AndExpression
    right: EqualityExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.BitwiseAndOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class EqualityExpressionRelational(Node["RelationalExpression"]):
    ...

class EqualityExpressionEqual(Node["EqualOperation"]):
    ...

class EqualityExpressionNotEqual(Node["NotEqualOperation"]):
    ...

class _EqualityExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class EqualityExpression(metaclass=_EqualityExpressionMeta):
    r"""EqualityExpressionRelational | EqualityExpressionEqual | EqualityExpressionNotEqual"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.EqualityExpression")
    RELATIONAL = hydra.core.Name("relational")
    EQUAL = hydra.core.Name("equal")
    NOT_EQUAL = hydra.core.Name("notEqual")

@dataclass(frozen=True)
class EqualOperation:
    left: EqualityExpression
    right: RelationalExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.EqualOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class NotEqualOperation:
    left: EqualityExpression
    right: RelationalExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.NotEqualOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class RelationalExpressionShift(Node["ShiftExpression"]):
    ...

class RelationalExpressionLess(Node["LessOperation"]):
    ...

class RelationalExpressionGreater(Node["GreaterOperation"]):
    ...

class RelationalExpressionLessEqual(Node["LessEqualOperation"]):
    ...

class RelationalExpressionGreaterEqual(Node["GreaterEqualOperation"]):
    ...

class _RelationalExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class RelationalExpression(metaclass=_RelationalExpressionMeta):
    r"""RelationalExpressionShift | RelationalExpressionLess | RelationalExpressionGreater | RelationalExpressionLessEqual | RelationalExpressionGreaterEqual"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.RelationalExpression")
    SHIFT = hydra.core.Name("shift")
    LESS = hydra.core.Name("less")
    GREATER = hydra.core.Name("greater")
    LESS_EQUAL = hydra.core.Name("lessEqual")
    GREATER_EQUAL = hydra.core.Name("greaterEqual")

@dataclass(frozen=True)
class LessOperation:
    left: RelationalExpression
    right: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LessOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class GreaterOperation:
    left: RelationalExpression
    right: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.GreaterOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class LessEqualOperation:
    left: RelationalExpression
    right: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LessEqualOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class GreaterEqualOperation:
    left: RelationalExpression
    right: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.GreaterEqualOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class ShiftExpressionAdditive(Node["AdditiveExpression"]):
    ...

class ShiftExpressionLeftShift(Node["LeftShiftOperation"]):
    ...

class ShiftExpressionRightShift(Node["RightShiftOperation"]):
    ...

class _ShiftExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ShiftExpression(metaclass=_ShiftExpressionMeta):
    r"""ShiftExpressionAdditive | ShiftExpressionLeftShift | ShiftExpressionRightShift"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ShiftExpression")
    ADDITIVE = hydra.core.Name("additive")
    LEFT_SHIFT = hydra.core.Name("leftShift")
    RIGHT_SHIFT = hydra.core.Name("rightShift")

@dataclass(frozen=True)
class LeftShiftOperation:
    left: ShiftExpression
    right: AdditiveExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LeftShiftOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class RightShiftOperation:
    left: ShiftExpression
    right: AdditiveExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.RightShiftOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class AdditiveExpressionMultiplicative(Node["MultiplicativeExpression"]):
    ...

class AdditiveExpressionAdd(Node["AddOperation"]):
    ...

class AdditiveExpressionSubtract(Node["SubtractOperation"]):
    ...

class _AdditiveExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AdditiveExpression(metaclass=_AdditiveExpressionMeta):
    r"""AdditiveExpressionMultiplicative | AdditiveExpressionAdd | AdditiveExpressionSubtract"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.AdditiveExpression")
    MULTIPLICATIVE = hydra.core.Name("multiplicative")
    ADD = hydra.core.Name("add")
    SUBTRACT = hydra.core.Name("subtract")

@dataclass(frozen=True)
class AddOperation:
    left: AdditiveExpression
    right: MultiplicativeExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.AddOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class SubtractOperation:
    left: AdditiveExpression
    right: MultiplicativeExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.SubtractOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class MultiplicativeExpressionUnary(Node["UnaryExpression"]):
    ...

class MultiplicativeExpressionMultiply(Node["MultiplyOperation"]):
    ...

class MultiplicativeExpressionDivide(Node["DivideOperation"]):
    ...

class MultiplicativeExpressionModulo(Node["ModuloOperation"]):
    ...

class _MultiplicativeExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class MultiplicativeExpression(metaclass=_MultiplicativeExpressionMeta):
    r"""MultiplicativeExpressionUnary | MultiplicativeExpressionMultiply | MultiplicativeExpressionDivide | MultiplicativeExpressionModulo"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MultiplicativeExpression")
    UNARY = hydra.core.Name("unary")
    MULTIPLY = hydra.core.Name("multiply")
    DIVIDE = hydra.core.Name("divide")
    MODULO = hydra.core.Name("modulo")

@dataclass(frozen=True)
class MultiplyOperation:
    left: MultiplicativeExpression
    right: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MultiplyOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class DivideOperation:
    left: MultiplicativeExpression
    right: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.DivideOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class ModuloOperation:
    left: MultiplicativeExpression
    right: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ModuloOperation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class UnaryExpressionPostfix(Node["PostfixExpression"]):
    ...

class UnaryExpressionUnaryOp(Node["UnaryOperation"]):
    ...

class UnaryExpressionSizeof(Node["SizeofExpression"]):
    ...

class _UnaryExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryExpression(metaclass=_UnaryExpressionMeta):
    r"""UnaryExpressionPostfix | UnaryExpressionUnaryOp | UnaryExpressionSizeof"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.UnaryExpression")
    POSTFIX = hydra.core.Name("postfix")
    UNARY_OP = hydra.core.Name("unaryOp")
    SIZEOF = hydra.core.Name("sizeof")

@dataclass(frozen=True)
class UnaryOperation:
    operator: UnaryOperator
    operand: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.UnaryOperation")
    OPERATOR = hydra.core.Name("operator")
    OPERAND = hydra.core.Name("operand")

class UnaryOperator(Enum):
    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

    LOGICAL_NOT = hydra.core.Name("logicalNot")

    BITWISE_NOT = hydra.core.Name("bitwiseNot")

    DEREFERENCE = hydra.core.Name("dereference")

    ADDRESS_OF = hydra.core.Name("addressOf")

    PRE_INCREMENT = hydra.core.Name("preIncrement")

    PRE_DECREMENT = hydra.core.Name("preDecrement")

UnaryOperator.TYPE_ = hydra.core.Name("hydra.cpp.syntax.UnaryOperator")

class SizeofExpression(Node["TypeExpression"]):
    ...

SizeofExpression.TYPE_ = hydra.core.Name("hydra.cpp.syntax.SizeofExpression")

class PostfixExpressionPrimary(Node["PrimaryExpression"]):
    ...

class PostfixExpressionSubscript(Node["SubscriptOperation"]):
    ...

class PostfixExpressionFunctionCall(Node["FunctionCallOperation"]):
    ...

class PostfixExpressionTemplateFunctionCall(Node["TemplateFunctionCallOperation"]):
    ...

class PostfixExpressionMemberAccess(Node["MemberAccessOperation"]):
    ...

class PostfixExpressionPointerMemberAccess(Node["PointerMemberAccessOperation"]):
    ...

class PostfixExpressionPostIncrement(Node["PostfixExpression"]):
    ...

class PostfixExpressionPostDecrement(Node["PostfixExpression"]):
    ...

class _PostfixExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PostfixExpression(metaclass=_PostfixExpressionMeta):
    r"""PostfixExpressionPrimary | PostfixExpressionSubscript | PostfixExpressionFunctionCall | PostfixExpressionTemplateFunctionCall | PostfixExpressionMemberAccess | PostfixExpressionPointerMemberAccess | PostfixExpressionPostIncrement | PostfixExpressionPostDecrement"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.PostfixExpression")
    PRIMARY = hydra.core.Name("primary")
    SUBSCRIPT = hydra.core.Name("subscript")
    FUNCTION_CALL = hydra.core.Name("functionCall")
    TEMPLATE_FUNCTION_CALL = hydra.core.Name("templateFunctionCall")
    MEMBER_ACCESS = hydra.core.Name("memberAccess")
    POINTER_MEMBER_ACCESS = hydra.core.Name("pointerMemberAccess")
    POST_INCREMENT = hydra.core.Name("postIncrement")
    POST_DECREMENT = hydra.core.Name("postDecrement")

@dataclass(frozen=True)
class SubscriptOperation:
    array: PostfixExpression
    index: Expression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.SubscriptOperation")
    ARRAY = hydra.core.Name("array")
    INDEX = hydra.core.Name("index")

@dataclass(frozen=True)
class FunctionCallOperation:
    function: PostfixExpression
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionCallOperation")
    FUNCTION = hydra.core.Name("function")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class MemberAccessOperation:
    object: PostfixExpression
    member: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MemberAccessOperation")
    OBJECT = hydra.core.Name("object")
    MEMBER = hydra.core.Name("member")

@dataclass(frozen=True)
class PointerMemberAccessOperation:
    pointer: PostfixExpression
    member: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.PointerMemberAccessOperation")
    POINTER = hydra.core.Name("pointer")
    MEMBER = hydra.core.Name("member")

@dataclass(frozen=True)
class TemplateFunctionCallOperation:
    function: PostfixExpression
    template_arguments: frozenlist[TemplateArgument]
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TemplateFunctionCallOperation")
    FUNCTION = hydra.core.Name("function")
    TEMPLATE_ARGUMENTS = hydra.core.Name("templateArguments")
    ARGUMENTS = hydra.core.Name("arguments")

class PrimaryExpressionIdentifier(Node[str]):
    ...

class PrimaryExpressionLiteral(Node["Literal"]):
    ...

class PrimaryExpressionParenthesized(Node["Expression"]):
    ...

class PrimaryExpressionLambda(Node["LambdaExpression"]):
    ...

class _PrimaryExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryExpression(metaclass=_PrimaryExpressionMeta):
    r"""PrimaryExpressionIdentifier | PrimaryExpressionLiteral | PrimaryExpressionParenthesized | PrimaryExpressionLambda"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.PrimaryExpression")
    IDENTIFIER = hydra.core.Name("identifier")
    LITERAL = hydra.core.Name("literal")
    PARENTHESIZED = hydra.core.Name("parenthesized")
    LAMBDA = hydra.core.Name("lambda")

@dataclass(frozen=True)
class LambdaExpression:
    captures: CaptureList
    parameters: frozenlist[Parameter]
    return_type: Maybe[TypeExpression]
    body: CompoundStatement

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LambdaExpression")
    CAPTURES = hydra.core.Name("captures")
    PARAMETERS = hydra.core.Name("parameters")
    RETURN_TYPE = hydra.core.Name("returnType")
    BODY = hydra.core.Name("body")

class CaptureListCaptureByValue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CaptureListCaptureByValue)
    def __hash__(self):
        return hash("CaptureListCaptureByValue")

class CaptureListCaptures(Node["frozenlist[Capture]"]):
    ...

class _CaptureListMeta(type):
    def __getitem__(cls, item):
        return object

class CaptureList(metaclass=_CaptureListMeta):
    r"""CaptureListCaptureByValue | CaptureListCaptures"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.CaptureList")
    CAPTURE_BY_VALUE = hydra.core.Name("captureByValue")
    CAPTURES = hydra.core.Name("captures")

@dataclass(frozen=True)
class Capture:
    name: str
    by_reference: bool

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Capture")
    NAME = hydra.core.Name("name")
    BY_REFERENCE = hydra.core.Name("byReference")

@dataclass(frozen=True)
class PatternMatch:
    visitor: Visitor
    variant: Expression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.PatternMatch")
    VISITOR = hydra.core.Name("visitor")
    VARIANT = hydra.core.Name("variant")

class VisitorLambda(Node["LambdaExpression"]):
    ...

class VisitorOverloaded(Node["OverloadedLambdas"]):
    ...

class _VisitorMeta(type):
    def __getitem__(cls, item):
        return object

class Visitor(metaclass=_VisitorMeta):
    r"""VisitorLambda | VisitorOverloaded"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Visitor")
    LAMBDA = hydra.core.Name("lambda")
    OVERLOADED = hydra.core.Name("overloaded")

class OverloadedLambdas(Node["frozenlist[LambdaExpression]"]):
    ...

OverloadedLambdas.TYPE_ = hydra.core.Name("hydra.cpp.syntax.OverloadedLambdas")

@dataclass(frozen=True)
class FunctionApplication:
    function: FunctionIdentifier
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionApplication")
    FUNCTION = hydra.core.Name("function")
    ARGUMENTS = hydra.core.Name("arguments")

class FunctionIdentifierSimple(Node[str]):
    ...

class FunctionIdentifierQualified(Node["QualifiedIdentifier"]):
    ...

class _FunctionIdentifierMeta(type):
    def __getitem__(cls, item):
        return object

class FunctionIdentifier(metaclass=_FunctionIdentifierMeta):
    r"""FunctionIdentifierSimple | FunctionIdentifierQualified"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionIdentifier")
    SIMPLE = hydra.core.Name("simple")
    QUALIFIED = hydra.core.Name("qualified")

@dataclass(frozen=True)
class QualifiedIdentifier:
    namespace: str
    name: str

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.QualifiedIdentifier")
    NAMESPACE = hydra.core.Name("namespace")
    NAME = hydra.core.Name("name")

class StatementLabeled(Node["LabeledStatement"]):
    ...

class StatementCompound(Node["CompoundStatement"]):
    ...

class StatementSelection(Node["SelectionStatement"]):
    ...

class StatementSwitch(Node["SwitchStatement"]):
    ...

class StatementIteration(Node["IterationStatement"]):
    ...

class StatementJump(Node["JumpStatement"]):
    ...

class StatementDeclaration(Node["VariableDeclaration"]):
    ...

class StatementExpression(Node["Expression"]):
    ...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementLabeled | StatementCompound | StatementSelection | StatementSwitch | StatementIteration | StatementJump | StatementDeclaration | StatementExpression"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Statement")
    LABELED = hydra.core.Name("labeled")
    COMPOUND = hydra.core.Name("compound")
    SELECTION = hydra.core.Name("selection")
    SWITCH = hydra.core.Name("switch")
    ITERATION = hydra.core.Name("iteration")
    JUMP = hydra.core.Name("jump")
    DECLARATION = hydra.core.Name("declaration")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class LabeledStatement:
    label: str
    statement: Statement

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.LabeledStatement")
    LABEL = hydra.core.Name("label")
    STATEMENT = hydra.core.Name("statement")

class CompoundStatement(Node["frozenlist[Statement]"]):
    ...

CompoundStatement.TYPE_ = hydra.core.Name("hydra.cpp.syntax.CompoundStatement")

@dataclass(frozen=True)
class SelectionStatement:
    condition: Expression
    then_branch: Statement
    else_branch: Maybe[Statement]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.SelectionStatement")
    CONDITION = hydra.core.Name("condition")
    THEN_BRANCH = hydra.core.Name("thenBranch")
    ELSE_BRANCH = hydra.core.Name("elseBranch")

@dataclass(frozen=True)
class SwitchStatement:
    value: Expression
    cases: frozenlist[CaseStatement]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.SwitchStatement")
    VALUE = hydra.core.Name("value")
    CASES = hydra.core.Name("cases")

class CaseStatementCase(Node["CaseValue"]):
    ...

class CaseStatementDefault(Node["Statement"]):
    ...

class _CaseStatementMeta(type):
    def __getitem__(cls, item):
        return object

class CaseStatement(metaclass=_CaseStatementMeta):
    r"""CaseStatementCase | CaseStatementDefault"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.CaseStatement")
    CASE = hydra.core.Name("case")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class CaseValue:
    value: Expression
    statement: Statement

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.CaseValue")
    VALUE = hydra.core.Name("value")
    STATEMENT = hydra.core.Name("statement")

class IterationStatementWhile(Node["WhileStatement"]):
    ...

class IterationStatementDo(Node["DoStatement"]):
    ...

class IterationStatementFor(Node["ForStatement"]):
    ...

class IterationStatementRangeFor(Node["RangeForStatement"]):
    ...

class _IterationStatementMeta(type):
    def __getitem__(cls, item):
        return object

class IterationStatement(metaclass=_IterationStatementMeta):
    r"""IterationStatementWhile | IterationStatementDo | IterationStatementFor | IterationStatementRangeFor"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.IterationStatement")
    WHILE = hydra.core.Name("while")
    DO = hydra.core.Name("do")
    FOR = hydra.core.Name("for")
    RANGE_FOR = hydra.core.Name("rangeFor")

@dataclass(frozen=True)
class WhileStatement:
    condition: Expression
    body: Statement

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.WhileStatement")
    CONDITION = hydra.core.Name("condition")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class DoStatement:
    body: Statement
    condition: Expression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.DoStatement")
    BODY = hydra.core.Name("body")
    CONDITION = hydra.core.Name("condition")

@dataclass(frozen=True)
class ForStatement:
    init: ForInit
    condition: Expression
    increment: Expression
    body: Statement

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ForStatement")
    INIT = hydra.core.Name("init")
    CONDITION = hydra.core.Name("condition")
    INCREMENT = hydra.core.Name("increment")
    BODY = hydra.core.Name("body")

class ForInitExpression(Node["Expression"]):
    ...

class ForInitDeclaration(Node["VariableDeclaration"]):
    ...

class ForInitEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ForInitEmpty)
    def __hash__(self):
        return hash("ForInitEmpty")

class _ForInitMeta(type):
    def __getitem__(cls, item):
        return object

class ForInit(metaclass=_ForInitMeta):
    r"""ForInitExpression | ForInitDeclaration | ForInitEmpty"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.ForInit")
    EXPRESSION = hydra.core.Name("expression")
    DECLARATION = hydra.core.Name("declaration")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class RangeForStatement:
    type: TypeExpression
    variable: str
    range_: Expression
    body: Statement

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.RangeForStatement")
    TYPE = hydra.core.Name("type")
    VARIABLE = hydra.core.Name("variable")
    RANGE = hydra.core.Name("range")
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

class JumpStatementReturnValue(Node["Expression"]):
    ...

class JumpStatementReturnVoid:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, JumpStatementReturnVoid)
    def __hash__(self):
        return hash("JumpStatementReturnVoid")

class JumpStatementThrow(Node["Expression"]):
    ...

class _JumpStatementMeta(type):
    def __getitem__(cls, item):
        return object

class JumpStatement(metaclass=_JumpStatementMeta):
    r"""JumpStatementBreak | JumpStatementContinue | JumpStatementReturnValue | JumpStatementReturnVoid | JumpStatementThrow"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.JumpStatement")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    RETURN_VALUE = hydra.core.Name("returnValue")
    RETURN_VOID = hydra.core.Name("returnVoid")
    THROW = hydra.core.Name("throw")

class ExpressionStatement(Node["Expression"]):
    ...

ExpressionStatement.TYPE_ = hydra.core.Name("hydra.cpp.syntax.ExpressionStatement")

class TypeExpressionBasic(Node["BasicType"]):
    ...

class TypeExpressionQualified(Node["QualifiedType"]):
    ...

class TypeExpressionTemplate(Node["TemplateType"]):
    ...

class TypeExpressionFunction(Node["FunctionType"]):
    ...

class TypeExpressionAuto:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeExpressionAuto)
    def __hash__(self):
        return hash("TypeExpressionAuto")

class _TypeExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeExpression(metaclass=_TypeExpressionMeta):
    r"""TypeExpressionBasic | TypeExpressionQualified | TypeExpressionTemplate | TypeExpressionFunction | TypeExpressionAuto"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TypeExpression")
    BASIC = hydra.core.Name("basic")
    QUALIFIED = hydra.core.Name("qualified")
    TEMPLATE = hydra.core.Name("template")
    FUNCTION = hydra.core.Name("function")
    AUTO = hydra.core.Name("auto")

class BasicTypeVoid:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeVoid)
    def __hash__(self):
        return hash("BasicTypeVoid")

class BasicTypeBool:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeBool)
    def __hash__(self):
        return hash("BasicTypeBool")

class BasicTypeChar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeChar)
    def __hash__(self):
        return hash("BasicTypeChar")

class BasicTypeInt:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeInt)
    def __hash__(self):
        return hash("BasicTypeInt")

class BasicTypeFloat:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeFloat)
    def __hash__(self):
        return hash("BasicTypeFloat")

class BasicTypeDouble:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeDouble)
    def __hash__(self):
        return hash("BasicTypeDouble")

class BasicTypeString:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeString)
    def __hash__(self):
        return hash("BasicTypeString")

class BasicTypeAuto:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BasicTypeAuto)
    def __hash__(self):
        return hash("BasicTypeAuto")

class BasicTypeNamed(Node[str]):
    ...

class _BasicTypeMeta(type):
    def __getitem__(cls, item):
        return object

class BasicType(metaclass=_BasicTypeMeta):
    r"""BasicTypeVoid | BasicTypeBool | BasicTypeChar | BasicTypeInt | BasicTypeFloat | BasicTypeDouble | BasicTypeString | BasicTypeAuto | BasicTypeNamed"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.BasicType")
    VOID = hydra.core.Name("void")
    BOOL = hydra.core.Name("bool")
    CHAR = hydra.core.Name("char")
    INT = hydra.core.Name("int")
    FLOAT = hydra.core.Name("float")
    DOUBLE = hydra.core.Name("double")
    STRING = hydra.core.Name("string")
    AUTO = hydra.core.Name("auto")
    NAMED = hydra.core.Name("named")

@dataclass(frozen=True)
class QualifiedType:
    base_type: TypeExpression
    qualifier: TypeQualifier

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.QualifiedType")
    BASE_TYPE = hydra.core.Name("baseType")
    QUALIFIER = hydra.core.Name("qualifier")

class TypeQualifier(Enum):
    CONST = hydra.core.Name("const")

    LVALUE_REF = hydra.core.Name("lvalueRef")

    RVALUE_REF = hydra.core.Name("rvalueRef")

    POINTER = hydra.core.Name("pointer")

TypeQualifier.TYPE_ = hydra.core.Name("hydra.cpp.syntax.TypeQualifier")

@dataclass(frozen=True)
class TemplateType:
    name: str
    arguments: frozenlist[TemplateArgument]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TemplateType")
    NAME = hydra.core.Name("name")
    ARGUMENTS = hydra.core.Name("arguments")

class TemplateArgumentType(Node["TypeExpression"]):
    ...

class TemplateArgumentValue(Node["Expression"]):
    ...

class _TemplateArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TemplateArgument(metaclass=_TemplateArgumentMeta):
    r"""TemplateArgumentType | TemplateArgumentValue"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.TemplateArgument")
    TYPE = hydra.core.Name("type")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class FunctionType:
    return_type: TypeExpression
    parameters: frozenlist[Parameter]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.FunctionType")
    RETURN_TYPE = hydra.core.Name("returnType")
    PARAMETERS = hydra.core.Name("parameters")

class LiteralInteger(Node["IntegerLiteral"]):
    ...

class LiteralFloating(Node["FloatingLiteral"]):
    ...

class LiteralCharacter(Node["CharacterLiteral"]):
    ...

class LiteralString(Node["StringLiteral"]):
    ...

class LiteralBoolean(Node["BooleanLiteral"]):
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
    r"""LiteralInteger | LiteralFloating | LiteralCharacter | LiteralString | LiteralBoolean | LiteralNull"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Literal")
    INTEGER = hydra.core.Name("integer")
    FLOATING = hydra.core.Name("floating")
    CHARACTER = hydra.core.Name("character")
    STRING = hydra.core.Name("string")
    BOOLEAN = hydra.core.Name("boolean")
    NULL = hydra.core.Name("null")

class IntegerLiteralDecimal(Node[int]):
    ...

class IntegerLiteralHexadecimal(Node[str]):
    ...

class IntegerLiteralOctal(Node[str]):
    ...

class IntegerLiteralBinary(Node[str]):
    ...

class _IntegerLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class IntegerLiteral(metaclass=_IntegerLiteralMeta):
    r"""IntegerLiteralDecimal | IntegerLiteralHexadecimal | IntegerLiteralOctal | IntegerLiteralBinary"""

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.IntegerLiteral")
    DECIMAL = hydra.core.Name("decimal")
    HEXADECIMAL = hydra.core.Name("hexadecimal")
    OCTAL = hydra.core.Name("octal")
    BINARY = hydra.core.Name("binary")

class FloatingLiteral(Node[Decimal]):
    ...

FloatingLiteral.TYPE_ = hydra.core.Name("hydra.cpp.syntax.FloatingLiteral")

class CharacterLiteral(Node[str]):
    ...

CharacterLiteral.TYPE_ = hydra.core.Name("hydra.cpp.syntax.CharacterLiteral")

class StringLiteral(Node[str]):
    ...

StringLiteral.TYPE_ = hydra.core.Name("hydra.cpp.syntax.StringLiteral")

class BooleanLiteral(Node[bool]):
    ...

BooleanLiteral.TYPE_ = hydra.core.Name("hydra.cpp.syntax.BooleanLiteral")

@dataclass(frozen=True)
class Vector:
    element_type: TypeExpression
    elements: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Vector")
    ELEMENT_TYPE = hydra.core.Name("elementType")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class Map:
    key_type: TypeExpression
    value_type: TypeExpression
    entries: frozenlist[MapEntry]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Map")
    KEY_TYPE = hydra.core.Name("keyType")
    VALUE_TYPE = hydra.core.Name("valueType")
    ENTRIES = hydra.core.Name("entries")

@dataclass(frozen=True)
class MapEntry:
    key: Expression
    value: Expression

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.MapEntry")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class Set:
    element_type: TypeExpression
    elements: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Set")
    ELEMENT_TYPE = hydra.core.Name("elementType")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class Optional:
    value_type: TypeExpression
    value: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Optional")
    VALUE_TYPE = hydra.core.Name("valueType")
    VALUE = hydra.core.Name("value")

Identifier: TypeAlias = "str"

@dataclass(frozen=True)
class Comment:
    text: str
    is_multiline: bool

    TYPE_ = hydra.core.Name("hydra.cpp.syntax.Comment")
    TEXT = hydra.core.Name("text")
    IS_MULTILINE = hydra.core.Name("isMultiline")

class BinaryOperator(Enum):
    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

    MULTIPLY = hydra.core.Name("multiply")

    DIVIDE = hydra.core.Name("divide")

    MODULO = hydra.core.Name("modulo")

    BITWISE_AND = hydra.core.Name("bitwiseAnd")

    BITWISE_OR = hydra.core.Name("bitwiseOr")

    BITWISE_XOR = hydra.core.Name("bitwiseXor")

    LOGICAL_AND = hydra.core.Name("logicalAnd")

    LOGICAL_OR = hydra.core.Name("logicalOr")

    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    LESS = hydra.core.Name("less")

    GREATER = hydra.core.Name("greater")

    LESS_EQUAL = hydra.core.Name("lessEqual")

    GREATER_EQUAL = hydra.core.Name("greaterEqual")

    LEFT_SHIFT = hydra.core.Name("leftShift")

    RIGHT_SHIFT = hydra.core.Name("rightShift")

BinaryOperator.TYPE_ = hydra.core.Name("hydra.cpp.syntax.BinaryOperator")
