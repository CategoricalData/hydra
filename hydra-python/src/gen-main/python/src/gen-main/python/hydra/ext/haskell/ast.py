# Note: this is an automatically generated file. Do not edit.

r"""A Haskell syntax model, loosely based on Language.Haskell.Tools.AST."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias
import hydra.core

@dataclass(frozen=True)
class Alternative:
    r"""A pattern-matching alternative."""
    
    pattern: Annotated[Pattern, "The pattern to match"]
    rhs: Annotated[CaseRhs, "The right-hand side of the alternative"]
    binds: Annotated[Maybe[LocalBindings], "Optional local bindings"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Alternative")
    PATTERN = hydra.core.Name("pattern")
    RHS = hydra.core.Name("rhs")
    BINDS = hydra.core.Name("binds")

class AssertionClass(Node["ClassAssertion"]):
    r"""A class assertion"""

class AssertionTuple(Node["frozenlist[Assertion]"]):
    r"""A tuple of assertions"""

class _AssertionMeta(type):
    def __getitem__(cls, item):
        return object

# A type assertion.
class Assertion(metaclass=_AssertionMeta):
    r"""AssertionClass | AssertionTuple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Assertion")
    CLASS = hydra.core.Name("class")
    TUPLE = hydra.core.Name("tuple")

@dataclass(frozen=True)
class ClassAssertion:
    r"""A class assertion."""
    
    name: Annotated[Name, "The name of the class"]
    types: Annotated[frozenlist[Type], "The types to which the class is applied"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ClassAssertion")
    NAME = hydra.core.Name("name")
    TYPES = hydra.core.Name("types")

class CaseRhs(Node["Expression"]):
    r"""The right-hand side of a pattern-matching alternative."""

CaseRhs.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.CaseRhs")

class ConstructorOrdinary(Node["OrdinaryConstructor"]):
    r"""An ordinary (positional) constructor"""

class ConstructorRecord(Node["RecordConstructor"]):
    r"""A record constructor"""

class _ConstructorMeta(type):
    def __getitem__(cls, item):
        return object

# A data constructor.
class Constructor(metaclass=_ConstructorMeta):
    r"""ConstructorOrdinary | ConstructorRecord"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Constructor")
    ORDINARY = hydra.core.Name("ordinary")
    RECORD = hydra.core.Name("record")

@dataclass(frozen=True)
class OrdinaryConstructor:
    r"""An ordinary (positional) data constructor."""
    
    name: Annotated[Name, "The name of the constructor"]
    fields: Annotated[frozenlist[Type], "The types of the positional fields"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.OrdinaryConstructor")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")

@dataclass(frozen=True)
class RecordConstructor:
    r"""A record-style data constructor."""
    
    name: Annotated[Name, "The name of the constructor"]
    fields: Annotated[frozenlist[FieldWithComments], "The named fields of the record"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.RecordConstructor")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")

@dataclass(frozen=True)
class ConstructorWithComments:
    r"""A data constructor together with any comments."""
    
    body: Annotated[Constructor, "The constructor"]
    comments: Annotated[Maybe[str], "Optional comments"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ConstructorWithComments")
    BODY = hydra.core.Name("body")
    COMMENTS = hydra.core.Name("comments")

@dataclass(frozen=True)
class DataDeclaration:
    r"""A data type declaration."""
    
    keyword: Annotated[DataOrNewtype, "The 'data' or 'newtype' keyword"]
    context: Annotated[frozenlist[Assertion], "Type class constraints"]
    head: Annotated[DeclarationHead, "The declaration head"]
    constructors: Annotated[frozenlist[ConstructorWithComments], "The data constructors"]
    deriving: Annotated[frozenlist[Deriving], "Derived type class instances"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.DataDeclaration")
    KEYWORD = hydra.core.Name("keyword")
    CONTEXT = hydra.core.Name("context")
    HEAD = hydra.core.Name("head")
    CONSTRUCTORS = hydra.core.Name("constructors")
    DERIVING = hydra.core.Name("deriving")

class DataOrNewtype(Enum):
    r"""The 'data' versus 'newtype keyword."""
    
    DATA = hydra.core.Name("data")
    
    NEWTYPE = hydra.core.Name("newtype")

DataOrNewtype.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.DataOrNewtype")

@dataclass(frozen=True)
class DeclarationWithComments:
    r"""A data declaration together with any comments."""
    
    body: Annotated[Declaration, "The declaration"]
    comments: Annotated[Maybe[str], "Optional comments"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.DeclarationWithComments")
    BODY = hydra.core.Name("body")
    COMMENTS = hydra.core.Name("comments")

class DeclarationData(Node["DataDeclaration"]):
    r"""A data type declaration"""

class DeclarationType(Node["TypeDeclaration"]):
    r"""A type synonym declaration"""

class DeclarationValueBinding(Node["ValueBinding"]):
    r"""A value binding"""

class DeclarationTypedBinding(Node["TypedBinding"]):
    r"""A typed binding"""

class _DeclarationMeta(type):
    def __getitem__(cls, item):
        return object

# A data or value declaration.
class Declaration(metaclass=_DeclarationMeta):
    r"""DeclarationData | DeclarationType | DeclarationValueBinding | DeclarationTypedBinding"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Declaration")
    DATA = hydra.core.Name("data")
    TYPE = hydra.core.Name("type")
    VALUE_BINDING = hydra.core.Name("valueBinding")
    TYPED_BINDING = hydra.core.Name("typedBinding")

class DeclarationHeadApplication(Node["ApplicationDeclarationHead"]):
    r"""An application-style declaration head"""

class DeclarationHeadParens(Node["DeclarationHead"]):
    r"""A parenthesized declaration head"""

class DeclarationHeadSimple(Node["Name"]):
    r"""A simple name"""

class _DeclarationHeadMeta(type):
    def __getitem__(cls, item):
        return object

# The left-hand side of a declaration.
class DeclarationHead(metaclass=_DeclarationHeadMeta):
    r"""DeclarationHeadApplication | DeclarationHeadParens | DeclarationHeadSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.DeclarationHead")
    APPLICATION = hydra.core.Name("application")
    PARENS = hydra.core.Name("parens")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class ApplicationDeclarationHead:
    r"""An application-style declaration head."""
    
    function: Annotated[DeclarationHead, "The function being applied"]
    operand: Annotated[Variable, "The type variable operand"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ApplicationDeclarationHead")
    FUNCTION = hydra.core.Name("function")
    OPERAND = hydra.core.Name("operand")

class Deriving(Node["frozenlist[Name]"]):
    r"""A 'deriving' statement."""

Deriving.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Deriving")

class ExportDeclaration(Node["ImportExportSpec"]):
    r"""An exported declaration"""

class ExportModule(Node["ModuleName"]):
    r"""An exported module"""

class _ExportMeta(type):
    def __getitem__(cls, item):
        return object

# An export statement.
class Export(metaclass=_ExportMeta):
    r"""ExportDeclaration | ExportModule"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Export")
    DECLARATION = hydra.core.Name("declaration")
    MODULE = hydra.core.Name("module")

class ExpressionApplication(Node["ApplicationExpression"]):
    r"""A function application"""

class ExpressionCase(Node["CaseExpression"]):
    r"""A case expression"""

class ExpressionConstructRecord(Node["ConstructRecordExpression"]):
    r"""A record constructor expression"""

class ExpressionDo(Node["frozenlist[Statement]"]):
    r"""A 'do' expression"""

class ExpressionIf(Node["IfExpression"]):
    r"""An 'if' expression"""

class ExpressionInfixApplication(Node["InfixApplicationExpression"]):
    r"""An infix application"""

class ExpressionLiteral(Node["Literal"]):
    r"""A literal value"""

class ExpressionLambda(Node["LambdaExpression"]):
    r"""A lambda expression"""

class ExpressionLeftSection(Node["SectionExpression"]):
    r"""A left section expression"""

class ExpressionLet(Node["LetExpression"]):
    r"""A 'let' expression"""

class ExpressionList(Node["frozenlist[Expression]"]):
    r"""A list expression"""

class ExpressionParens(Node["Expression"]):
    r"""A parenthesized expression"""

class ExpressionPrefixApplication(Node["PrefixApplicationExpression"]):
    r"""A prefix application"""

class ExpressionRightSection(Node["SectionExpression"]):
    r"""A right section expression"""

class ExpressionTuple(Node["frozenlist[Expression]"]):
    r"""A tuple expression"""

class ExpressionTypeSignature(Node["TypeSignatureExpression"]):
    r"""A type signature expression"""

class ExpressionUpdateRecord(Node["UpdateRecordExpression"]):
    r"""A record update expression"""

class ExpressionVariable(Node["Name"]):
    r"""A variable reference"""

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A data expression.
class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionApplication | ExpressionCase | ExpressionConstructRecord | ExpressionDo | ExpressionIf | ExpressionInfixApplication | ExpressionLiteral | ExpressionLambda | ExpressionLeftSection | ExpressionLet | ExpressionList | ExpressionParens | ExpressionPrefixApplication | ExpressionRightSection | ExpressionTuple | ExpressionTypeSignature | ExpressionUpdateRecord | ExpressionVariable"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Expression")
    APPLICATION = hydra.core.Name("application")
    CASE = hydra.core.Name("case")
    CONSTRUCT_RECORD = hydra.core.Name("constructRecord")
    DO = hydra.core.Name("do")
    IF = hydra.core.Name("if")
    INFIX_APPLICATION = hydra.core.Name("infixApplication")
    LITERAL = hydra.core.Name("literal")
    LAMBDA = hydra.core.Name("lambda")
    LEFT_SECTION = hydra.core.Name("leftSection")
    LET = hydra.core.Name("let")
    LIST = hydra.core.Name("list")
    PARENS = hydra.core.Name("parens")
    PREFIX_APPLICATION = hydra.core.Name("prefixApplication")
    RIGHT_SECTION = hydra.core.Name("rightSection")
    TUPLE = hydra.core.Name("tuple")
    TYPE_SIGNATURE = hydra.core.Name("typeSignature")
    UPDATE_RECORD = hydra.core.Name("updateRecord")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class ApplicationExpression:
    r"""An application expression."""
    
    function: Annotated[Expression, "The function being applied"]
    argument: Annotated[Expression, "The argument"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ApplicationExpression")
    FUNCTION = hydra.core.Name("function")
    ARGUMENT = hydra.core.Name("argument")

@dataclass(frozen=True)
class CaseExpression:
    r"""A case expression."""
    
    case: Annotated[Expression, "The expression being matched"]
    alternatives: Annotated[frozenlist[Alternative], "The pattern-matching alternatives"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.CaseExpression")
    CASE = hydra.core.Name("case")
    ALTERNATIVES = hydra.core.Name("alternatives")

@dataclass(frozen=True)
class ConstructRecordExpression:
    r"""A record constructor expression."""
    
    name: Annotated[Name, "The constructor name"]
    fields: Annotated[frozenlist[FieldUpdate], "The field assignments"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ConstructRecordExpression")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")

@dataclass(frozen=True)
class IfExpression:
    r"""An 'if' expression."""
    
    condition: Annotated[Expression, "The condition expression"]
    then: Annotated[Expression, "The 'then' branch"]
    else_: Annotated[Expression, "The 'else' branch"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.IfExpression")
    CONDITION = hydra.core.Name("condition")
    THEN = hydra.core.Name("then")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class InfixApplicationExpression:
    r"""An infix application expression."""
    
    lhs: Annotated[Expression, "The left-hand operand"]
    operator: Annotated[Operator, "The infix operator"]
    rhs: Annotated[Expression, "The right-hand operand"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.InfixApplicationExpression")
    LHS = hydra.core.Name("lhs")
    OPERATOR = hydra.core.Name("operator")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class LambdaExpression:
    r"""A lambda expression."""
    
    bindings: Annotated[frozenlist[Pattern], "The patterns binding parameters"]
    inner: Annotated[Expression, "The body of the lambda"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.LambdaExpression")
    BINDINGS = hydra.core.Name("bindings")
    INNER = hydra.core.Name("inner")

@dataclass(frozen=True)
class LetExpression:
    r"""A 'let' expression."""
    
    bindings: Annotated[frozenlist[LocalBinding], "The local bindings"]
    inner: Annotated[Expression, "The body of the let expression"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.LetExpression")
    BINDINGS = hydra.core.Name("bindings")
    INNER = hydra.core.Name("inner")

@dataclass(frozen=True)
class PrefixApplicationExpression:
    r"""A prefix expression."""
    
    operator: Annotated[Operator, "The prefix operator"]
    rhs: Annotated[Expression, "The operand"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.PrefixApplicationExpression")
    OPERATOR = hydra.core.Name("operator")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class SectionExpression:
    r"""A section expression."""
    
    operator: Annotated[Operator, "The operator"]
    expression: Annotated[Expression, "The operand"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.SectionExpression")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class TypeSignatureExpression:
    r"""A type signature expression."""
    
    inner: Annotated[Expression, "The expression being typed"]
    type: Annotated[Type, "The type signature"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.TypeSignatureExpression")
    INNER = hydra.core.Name("inner")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class UpdateRecordExpression:
    r"""An update record expression."""
    
    inner: Annotated[Expression, "The record being updated"]
    fields: Annotated[frozenlist[FieldUpdate], "The field updates"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.UpdateRecordExpression")
    INNER = hydra.core.Name("inner")
    FIELDS = hydra.core.Name("fields")

@dataclass(frozen=True)
class Field:
    r"""A field (name/type pair)."""
    
    name: Annotated[Name, "The field name"]
    type: Annotated[Type, "The field type"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Field")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class FieldWithComments:
    r"""A field together with any comments."""
    
    field: Annotated[Field, "The field"]
    comments: Annotated[Maybe[str], "Optional comments"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.FieldWithComments")
    FIELD = hydra.core.Name("field")
    COMMENTS = hydra.core.Name("comments")

@dataclass(frozen=True)
class FieldUpdate:
    r"""A field name and value."""
    
    name: Annotated[Name, "The field name"]
    value: Annotated[Expression, "The field value"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.FieldUpdate")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class Import:
    r"""An import statement."""
    
    qualified: Annotated[bool, "Whether the import is qualified"]
    module: Annotated[ModuleName, "The module being imported"]
    as_: Annotated[Maybe[ModuleName], "Optional alias for the module"]
    spec: Annotated[Maybe[SpecImport], "Optional import specification"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Import")
    QUALIFIED = hydra.core.Name("qualified")
    MODULE = hydra.core.Name("module")
    AS = hydra.core.Name("as")
    SPEC = hydra.core.Name("spec")

class SpecImportList(Node["frozenlist[ImportExportSpec]"]):
    r"""A list of imports to include"""

class SpecImportHiding(Node["frozenlist[ImportExportSpec]"]):
    r"""A list of imports to exclude"""

class _SpecImportMeta(type):
    def __getitem__(cls, item):
        return object

# An import specification.
class SpecImport(metaclass=_SpecImportMeta):
    r"""SpecImportList | SpecImportHiding"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.SpecImport")
    LIST = hydra.core.Name("list")
    HIDING = hydra.core.Name("hiding")

class ImportModifier(Enum):
    r"""An import modifier ('pattern' or 'type')."""
    
    PATTERN = hydra.core.Name("pattern")
    
    TYPE = hydra.core.Name("type")

ImportModifier.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ImportModifier")

@dataclass(frozen=True)
class ImportExportSpec:
    r"""An import or export specification."""
    
    modifier: Annotated[Maybe[ImportModifier], "Optional import modifier"]
    name: Annotated[Name, "The name being imported or exported"]
    subspec: Annotated[Maybe[SubspecImportExportSpec], "Optional subspecification"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ImportExportSpec")
    MODIFIER = hydra.core.Name("modifier")
    NAME = hydra.core.Name("name")
    SUBSPEC = hydra.core.Name("subspec")

class SubspecImportExportSpecAll:
    r"""Import/export all"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SubspecImportExportSpecAll)
    def __hash__(self):
        return hash("SubspecImportExportSpecAll")

class SubspecImportExportSpecList(Node["frozenlist[Name]"]):
    r"""Import/export specific names"""

class _SubspecImportExportSpecMeta(type):
    def __getitem__(cls, item):
        return object

# A subspecification within an import/export.
class SubspecImportExportSpec(metaclass=_SubspecImportExportSpecMeta):
    r"""SubspecImportExportSpecAll | SubspecImportExportSpecList"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.SubspecImportExportSpec")
    ALL = hydra.core.Name("all")
    LIST = hydra.core.Name("list")

class LiteralChar(Node[int]):
    r"""A character literal"""

class LiteralDouble(Node[float]):
    r"""A double-precision floating point literal"""

class LiteralFloat(Node[float]):
    r"""A single-precision floating point literal"""

class LiteralInt(Node[int]):
    r"""A 32-bit integer literal"""

class LiteralInteger(Node[int]):
    r"""An arbitrary-precision integer literal"""

class LiteralString(Node[str]):
    r"""A string literal"""

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A literal value.
class Literal(metaclass=_LiteralMeta):
    r"""LiteralChar | LiteralDouble | LiteralFloat | LiteralInt | LiteralInteger | LiteralString"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Literal")
    CHAR = hydra.core.Name("char")
    DOUBLE = hydra.core.Name("double")
    FLOAT = hydra.core.Name("float")
    INT = hydra.core.Name("int")
    INTEGER = hydra.core.Name("integer")
    STRING = hydra.core.Name("string")

class LocalBindingSignature(Node["TypeSignature"]):
    r"""A type signature"""

class LocalBindingValue(Node["ValueBinding"]):
    r"""A value binding"""

class _LocalBindingMeta(type):
    def __getitem__(cls, item):
        return object

# A local binding.
class LocalBinding(metaclass=_LocalBindingMeta):
    r"""LocalBindingSignature | LocalBindingValue"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.LocalBinding")
    SIGNATURE = hydra.core.Name("signature")
    VALUE = hydra.core.Name("value")

class LocalBindings(Node["frozenlist[LocalBinding]"]):
    r"""A collection of local bindings."""

LocalBindings.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.LocalBindings")

@dataclass(frozen=True)
class Module:
    r"""A Haskell module."""
    
    head: Annotated[Maybe[ModuleHead], "Optional module head"]
    imports: Annotated[frozenlist[Import], "Import statements"]
    declarations: Annotated[frozenlist[DeclarationWithComments], "Module declarations"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Module")
    HEAD = hydra.core.Name("head")
    IMPORTS = hydra.core.Name("imports")
    DECLARATIONS = hydra.core.Name("declarations")

@dataclass(frozen=True)
class ModuleHead:
    r"""A module head."""
    
    comments: Annotated[Maybe[str], "Optional module-level comments"]
    name: Annotated[ModuleName, "The module name"]
    exports: Annotated[frozenlist[Export], "Export list"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ModuleHead")
    COMMENTS = hydra.core.Name("comments")
    NAME = hydra.core.Name("name")
    EXPORTS = hydra.core.Name("exports")

class ModuleName(Node[str]):
    r"""A module name."""

ModuleName.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ModuleName")

class NameImplicit(Node["QualifiedName"]):
    r"""An implicit name"""

class NameNormal(Node["QualifiedName"]):
    r"""A normal name"""

class NameParens(Node["QualifiedName"]):
    r"""A parenthesized name"""

class _NameMeta(type):
    def __getitem__(cls, item):
        return object

# A name.
class Name(metaclass=_NameMeta):
    r"""NameImplicit | NameNormal | NameParens"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Name")
    IMPLICIT = hydra.core.Name("implicit")
    NORMAL = hydra.core.Name("normal")
    PARENS = hydra.core.Name("parens")

class NamePart(Node[str]):
    r"""A component of a qualified name."""

NamePart.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.NamePart")

class OperatorBacktick(Node["QualifiedName"]):
    r"""A function used as an infix operator"""

class OperatorNormal(Node["QualifiedName"]):
    r"""A normal infix operator"""

class _OperatorMeta(type):
    def __getitem__(cls, item):
        return object

# An operator.
class Operator(metaclass=_OperatorMeta):
    r"""OperatorBacktick | OperatorNormal"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Operator")
    BACKTICK = hydra.core.Name("backtick")
    NORMAL = hydra.core.Name("normal")

class PatternApplication(Node["ApplicationPattern"]):
    r"""An application pattern"""

class PatternAs(Node["AsPattern"]):
    r"""An 'as' pattern"""

class PatternList(Node["frozenlist[Pattern]"]):
    r"""A list pattern"""

class PatternLiteral(Node["Literal"]):
    r"""A literal pattern"""

class PatternName(Node["Name"]):
    r"""A name pattern"""

class PatternParens(Node["Pattern"]):
    r"""A parenthesized pattern"""

class PatternRecord(Node["RecordPattern"]):
    r"""A record pattern"""

class PatternTuple(Node["frozenlist[Pattern]"]):
    r"""A tuple pattern"""

class PatternTyped(Node["TypedPattern"]):
    r"""A typed pattern"""

class PatternWildcard:
    r"""A wildcard pattern"""
    
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PatternWildcard)
    def __hash__(self):
        return hash("PatternWildcard")

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A pattern.
class Pattern(metaclass=_PatternMeta):
    r"""PatternApplication | PatternAs | PatternList | PatternLiteral | PatternName | PatternParens | PatternRecord | PatternTuple | PatternTyped | PatternWildcard"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Pattern")
    APPLICATION = hydra.core.Name("application")
    AS = hydra.core.Name("as")
    LIST = hydra.core.Name("list")
    LITERAL = hydra.core.Name("literal")
    NAME = hydra.core.Name("name")
    PARENS = hydra.core.Name("parens")
    RECORD = hydra.core.Name("record")
    TUPLE = hydra.core.Name("tuple")
    TYPED = hydra.core.Name("typed")
    WILDCARD = hydra.core.Name("wildcard")

@dataclass(frozen=True)
class ApplicationPattern:
    r"""An application pattern."""
    
    name: Annotated[Name, "The constructor name"]
    args: Annotated[frozenlist[Pattern], "The pattern arguments"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ApplicationPattern")
    NAME = hydra.core.Name("name")
    ARGS = hydra.core.Name("args")

@dataclass(frozen=True)
class AsPattern:
    r"""An 'as' pattern."""
    
    name: Annotated[Name, "The bound name"]
    inner: Annotated[Pattern, "The inner pattern"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.AsPattern")
    NAME = hydra.core.Name("name")
    INNER = hydra.core.Name("inner")

@dataclass(frozen=True)
class RecordPattern:
    r"""A record pattern."""
    
    name: Annotated[Name, "The constructor name"]
    fields: Annotated[frozenlist[PatternField], "The field patterns"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.RecordPattern")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")

@dataclass(frozen=True)
class TypedPattern:
    r"""A typed pattern."""
    
    inner: Annotated[Pattern, "The inner pattern"]
    type: Annotated[Type, "The type annotation"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.TypedPattern")
    INNER = hydra.core.Name("inner")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class PatternField:
    r"""A pattern field."""
    
    name: Annotated[Name, "The field name"]
    pattern: Annotated[Pattern, "The field pattern"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.PatternField")
    NAME = hydra.core.Name("name")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class QualifiedName:
    r"""A qualified name."""
    
    qualifiers: Annotated[frozenlist[NamePart], "The qualifier parts"]
    unqualified: Annotated[NamePart, "The unqualified name part"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.QualifiedName")
    QUALIFIERS = hydra.core.Name("qualifiers")
    UNQUALIFIED = hydra.core.Name("unqualified")

class RightHandSide(Node["Expression"]):
    r"""A right-hand side of a binding."""

RightHandSide.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.RightHandSide")

class Statement(Node["Expression"]):
    r"""A do-notation statement."""

Statement.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Statement")

class TypeApplication(Node["ApplicationType"]):
    r"""An application type"""

class TypeCtx(Node["ContextType"]):
    r"""A context type"""

class TypeFunction(Node["FunctionType"]):
    r"""A function type"""

class TypeInfix(Node["InfixType"]):
    r"""An infix type"""

class TypeList(Node["Type"]):
    r"""A list type"""

class TypeParens(Node["Type"]):
    r"""A parenthesized type"""

class TypeTuple(Node["frozenlist[Type]"]):
    r"""A tuple type"""

class TypeVariable(Node["Name"]):
    r"""A type variable or type name"""

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

# A type expression.
class Type(metaclass=_TypeMeta):
    r"""TypeApplication | TypeCtx | TypeFunction | TypeInfix | TypeList | TypeParens | TypeTuple | TypeVariable"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Type")
    APPLICATION = hydra.core.Name("application")
    CTX = hydra.core.Name("ctx")
    FUNCTION = hydra.core.Name("function")
    INFIX = hydra.core.Name("infix")
    LIST = hydra.core.Name("list")
    PARENS = hydra.core.Name("parens")
    TUPLE = hydra.core.Name("tuple")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class ApplicationType:
    r"""An application type."""
    
    context: Annotated[Type, "The type being applied"]
    argument: Annotated[Type, "The type argument"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ApplicationType")
    CONTEXT = hydra.core.Name("context")
    ARGUMENT = hydra.core.Name("argument")

@dataclass(frozen=True)
class ContextType:
    r"""A type with a context (type class constraints)."""
    
    ctx: Annotated[Assertion, "The type class context"]
    type: Annotated[Type, "The constrained type"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ContextType")
    CTX = hydra.core.Name("ctx")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class FunctionType:
    r"""A function type."""
    
    domain: Annotated[Type, "The domain type"]
    codomain: Annotated[Type, "The codomain type"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.FunctionType")
    DOMAIN = hydra.core.Name("domain")
    CODOMAIN = hydra.core.Name("codomain")

@dataclass(frozen=True)
class InfixType:
    r"""An infix type application."""
    
    lhs: Annotated[Type, "The left-hand type"]
    operator: Annotated[Operator, "The type operator"]
    rhs: Annotated[Operator, "The right-hand operator"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.InfixType")
    LHS = hydra.core.Name("lhs")
    OPERATOR = hydra.core.Name("operator")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class TypeDeclaration:
    r"""A type synonym declaration."""
    
    name: Annotated[DeclarationHead, "The declaration head"]
    type: Annotated[Type, "The type being defined"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.TypeDeclaration")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeSignature:
    r"""A type signature."""
    
    name: Annotated[Name, "The name being typed"]
    type: Annotated[Type, "The type"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.TypeSignature")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class TypedBinding:
    r"""A binding with its type signature."""
    
    type_signature: Annotated[TypeSignature, "The type signature"]
    value_binding: Annotated[ValueBinding, "The value binding"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.TypedBinding")
    TYPE_SIGNATURE = hydra.core.Name("typeSignature")
    VALUE_BINDING = hydra.core.Name("valueBinding")

class ValueBindingSimple(Node["SimpleValueBinding"]):
    r"""A simple value binding"""

class _ValueBindingMeta(type):
    def __getitem__(cls, item):
        return object

# A value binding.
class ValueBinding(metaclass=_ValueBindingMeta):
    r"""ValueBindingSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.ValueBinding")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class SimpleValueBinding:
    r"""A simple value binding."""
    
    pattern: Annotated[Pattern, "The pattern being bound"]
    rhs: Annotated[RightHandSide, "The right-hand side"]
    local_bindings: Annotated[Maybe[LocalBindings], "Optional local bindings (where clause)"]
    
    TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.SimpleValueBinding")
    PATTERN = hydra.core.Name("pattern")
    RHS = hydra.core.Name("rhs")
    LOCAL_BINDINGS = hydra.core.Name("localBindings")

class Variable(Node["Name"]):
    r"""A type variable."""

Variable.TYPE_ = hydra.core.Name("hydra.ext.haskell.ast.Variable")
