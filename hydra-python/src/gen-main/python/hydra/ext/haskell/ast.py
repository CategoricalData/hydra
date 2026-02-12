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

ALTERNATIVE__NAME = hydra.core.Name("hydra.ext.haskell.ast.Alternative")
ALTERNATIVE__PATTERN__NAME = hydra.core.Name("pattern")
ALTERNATIVE__RHS__NAME = hydra.core.Name("rhs")
ALTERNATIVE__BINDS__NAME = hydra.core.Name("binds")

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
    
    pass

ASSERTION__NAME = hydra.core.Name("hydra.ext.haskell.ast.Assertion")
ASSERTION__CLASS__NAME = hydra.core.Name("class")
ASSERTION__TUPLE__NAME = hydra.core.Name("tuple")

@dataclass(frozen=True)
class ClassAssertion:
    r"""A class assertion."""
    
    name: Annotated[Name, "The name of the class"]
    types: Annotated[frozenlist[Type], "The types to which the class is applied"]

CLASS_ASSERTION__NAME = hydra.core.Name("hydra.ext.haskell.ast.ClassAssertion")
CLASS_ASSERTION__NAME__NAME = hydra.core.Name("name")
CLASS_ASSERTION__TYPES__NAME = hydra.core.Name("types")

class CaseRhs(Node["Expression"]):
    r"""The right-hand side of a pattern-matching alternative."""

CASE_RHS__NAME = hydra.core.Name("hydra.ext.haskell.ast.CaseRhs")

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
    
    pass

CONSTRUCTOR__NAME = hydra.core.Name("hydra.ext.haskell.ast.Constructor")
CONSTRUCTOR__ORDINARY__NAME = hydra.core.Name("ordinary")
CONSTRUCTOR__RECORD__NAME = hydra.core.Name("record")

@dataclass(frozen=True)
class OrdinaryConstructor:
    r"""An ordinary (positional) data constructor."""
    
    name: Annotated[Name, "The name of the constructor"]
    fields: Annotated[frozenlist[Type], "The types of the positional fields"]

ORDINARY_CONSTRUCTOR__NAME = hydra.core.Name("hydra.ext.haskell.ast.OrdinaryConstructor")
ORDINARY_CONSTRUCTOR__NAME__NAME = hydra.core.Name("name")
ORDINARY_CONSTRUCTOR__FIELDS__NAME = hydra.core.Name("fields")

@dataclass(frozen=True)
class RecordConstructor:
    r"""A record-style data constructor."""
    
    name: Annotated[Name, "The name of the constructor"]
    fields: Annotated[frozenlist[FieldWithComments], "The named fields of the record"]

RECORD_CONSTRUCTOR__NAME = hydra.core.Name("hydra.ext.haskell.ast.RecordConstructor")
RECORD_CONSTRUCTOR__NAME__NAME = hydra.core.Name("name")
RECORD_CONSTRUCTOR__FIELDS__NAME = hydra.core.Name("fields")

@dataclass(frozen=True)
class ConstructorWithComments:
    r"""A data constructor together with any comments."""
    
    body: Annotated[Constructor, "The constructor"]
    comments: Annotated[Maybe[str], "Optional comments"]

CONSTRUCTOR_WITH_COMMENTS__NAME = hydra.core.Name("hydra.ext.haskell.ast.ConstructorWithComments")
CONSTRUCTOR_WITH_COMMENTS__BODY__NAME = hydra.core.Name("body")
CONSTRUCTOR_WITH_COMMENTS__COMMENTS__NAME = hydra.core.Name("comments")

@dataclass(frozen=True)
class DataDeclaration:
    r"""A data type declaration."""
    
    keyword: Annotated[DataOrNewtype, "The 'data' or 'newtype' keyword"]
    context: Annotated[frozenlist[Assertion], "Type class constraints"]
    head: Annotated[DeclarationHead, "The declaration head"]
    constructors: Annotated[frozenlist[ConstructorWithComments], "The data constructors"]
    deriving: Annotated[frozenlist[Deriving], "Derived type class instances"]

DATA_DECLARATION__NAME = hydra.core.Name("hydra.ext.haskell.ast.DataDeclaration")
DATA_DECLARATION__KEYWORD__NAME = hydra.core.Name("keyword")
DATA_DECLARATION__CONTEXT__NAME = hydra.core.Name("context")
DATA_DECLARATION__HEAD__NAME = hydra.core.Name("head")
DATA_DECLARATION__CONSTRUCTORS__NAME = hydra.core.Name("constructors")
DATA_DECLARATION__DERIVING__NAME = hydra.core.Name("deriving")

class DataOrNewtype(Enum):
    r"""The 'data' versus 'newtype keyword."""
    
    DATA = "data"
    
    NEWTYPE = "newtype"

DATA_OR_NEWTYPE__NAME = hydra.core.Name("hydra.ext.haskell.ast.DataOrNewtype")
DATA_OR_NEWTYPE__DATA__NAME = hydra.core.Name("data")
DATA_OR_NEWTYPE__NEWTYPE__NAME = hydra.core.Name("newtype")

@dataclass(frozen=True)
class DeclarationWithComments:
    r"""A data declaration together with any comments."""
    
    body: Annotated[Declaration, "The declaration"]
    comments: Annotated[Maybe[str], "Optional comments"]

DECLARATION_WITH_COMMENTS__NAME = hydra.core.Name("hydra.ext.haskell.ast.DeclarationWithComments")
DECLARATION_WITH_COMMENTS__BODY__NAME = hydra.core.Name("body")
DECLARATION_WITH_COMMENTS__COMMENTS__NAME = hydra.core.Name("comments")

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
    
    pass

DECLARATION__NAME = hydra.core.Name("hydra.ext.haskell.ast.Declaration")
DECLARATION__DATA__NAME = hydra.core.Name("data")
DECLARATION__TYPE__NAME = hydra.core.Name("type")
DECLARATION__VALUE_BINDING__NAME = hydra.core.Name("valueBinding")
DECLARATION__TYPED_BINDING__NAME = hydra.core.Name("typedBinding")

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
    
    pass

DECLARATION_HEAD__NAME = hydra.core.Name("hydra.ext.haskell.ast.DeclarationHead")
DECLARATION_HEAD__APPLICATION__NAME = hydra.core.Name("application")
DECLARATION_HEAD__PARENS__NAME = hydra.core.Name("parens")
DECLARATION_HEAD__SIMPLE__NAME = hydra.core.Name("simple")

@dataclass(frozen=True)
class ApplicationDeclarationHead:
    r"""An application-style declaration head."""
    
    function: Annotated[DeclarationHead, "The function being applied"]
    operand: Annotated[Variable, "The type variable operand"]

APPLICATION_DECLARATION_HEAD__NAME = hydra.core.Name("hydra.ext.haskell.ast.ApplicationDeclarationHead")
APPLICATION_DECLARATION_HEAD__FUNCTION__NAME = hydra.core.Name("function")
APPLICATION_DECLARATION_HEAD__OPERAND__NAME = hydra.core.Name("operand")

class Deriving(Node["frozenlist[Name]"]):
    r"""A 'deriving' statement."""

DERIVING__NAME = hydra.core.Name("hydra.ext.haskell.ast.Deriving")

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
    
    pass

EXPORT__NAME = hydra.core.Name("hydra.ext.haskell.ast.Export")
EXPORT__DECLARATION__NAME = hydra.core.Name("declaration")
EXPORT__MODULE__NAME = hydra.core.Name("module")

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
    
    pass

EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.Expression")
EXPRESSION__APPLICATION__NAME = hydra.core.Name("application")
EXPRESSION__CASE__NAME = hydra.core.Name("case")
EXPRESSION__CONSTRUCT_RECORD__NAME = hydra.core.Name("constructRecord")
EXPRESSION__DO__NAME = hydra.core.Name("do")
EXPRESSION__IF__NAME = hydra.core.Name("if")
EXPRESSION__INFIX_APPLICATION__NAME = hydra.core.Name("infixApplication")
EXPRESSION__LITERAL__NAME = hydra.core.Name("literal")
EXPRESSION__LAMBDA__NAME = hydra.core.Name("lambda")
EXPRESSION__LEFT_SECTION__NAME = hydra.core.Name("leftSection")
EXPRESSION__LET__NAME = hydra.core.Name("let")
EXPRESSION__LIST__NAME = hydra.core.Name("list")
EXPRESSION__PARENS__NAME = hydra.core.Name("parens")
EXPRESSION__PREFIX_APPLICATION__NAME = hydra.core.Name("prefixApplication")
EXPRESSION__RIGHT_SECTION__NAME = hydra.core.Name("rightSection")
EXPRESSION__TUPLE__NAME = hydra.core.Name("tuple")
EXPRESSION__TYPE_SIGNATURE__NAME = hydra.core.Name("typeSignature")
EXPRESSION__UPDATE_RECORD__NAME = hydra.core.Name("updateRecord")
EXPRESSION__VARIABLE__NAME = hydra.core.Name("variable")

@dataclass(frozen=True)
class ApplicationExpression:
    r"""An application expression."""
    
    function: Annotated[Expression, "The function being applied"]
    argument: Annotated[Expression, "The argument"]

APPLICATION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.ApplicationExpression")
APPLICATION_EXPRESSION__FUNCTION__NAME = hydra.core.Name("function")
APPLICATION_EXPRESSION__ARGUMENT__NAME = hydra.core.Name("argument")

@dataclass(frozen=True)
class CaseExpression:
    r"""A case expression."""
    
    case: Annotated[Expression, "The expression being matched"]
    alternatives: Annotated[frozenlist[Alternative], "The pattern-matching alternatives"]

CASE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.CaseExpression")
CASE_EXPRESSION__CASE__NAME = hydra.core.Name("case")
CASE_EXPRESSION__ALTERNATIVES__NAME = hydra.core.Name("alternatives")

@dataclass(frozen=True)
class ConstructRecordExpression:
    r"""A record constructor expression."""
    
    name: Annotated[Name, "The constructor name"]
    fields: Annotated[frozenlist[FieldUpdate], "The field assignments"]

CONSTRUCT_RECORD_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.ConstructRecordExpression")
CONSTRUCT_RECORD_EXPRESSION__NAME__NAME = hydra.core.Name("name")
CONSTRUCT_RECORD_EXPRESSION__FIELDS__NAME = hydra.core.Name("fields")

@dataclass(frozen=True)
class IfExpression:
    r"""An 'if' expression."""
    
    condition: Annotated[Expression, "The condition expression"]
    then: Annotated[Expression, "The 'then' branch"]
    else_: Annotated[Expression, "The 'else' branch"]

IF_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.IfExpression")
IF_EXPRESSION__CONDITION__NAME = hydra.core.Name("condition")
IF_EXPRESSION__THEN__NAME = hydra.core.Name("then")
IF_EXPRESSION__ELSE__NAME = hydra.core.Name("else")

@dataclass(frozen=True)
class InfixApplicationExpression:
    r"""An infix application expression."""
    
    lhs: Annotated[Expression, "The left-hand operand"]
    operator: Annotated[Operator, "The infix operator"]
    rhs: Annotated[Expression, "The right-hand operand"]

INFIX_APPLICATION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.InfixApplicationExpression")
INFIX_APPLICATION_EXPRESSION__LHS__NAME = hydra.core.Name("lhs")
INFIX_APPLICATION_EXPRESSION__OPERATOR__NAME = hydra.core.Name("operator")
INFIX_APPLICATION_EXPRESSION__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class LambdaExpression:
    r"""A lambda expression."""
    
    bindings: Annotated[frozenlist[Pattern], "The patterns binding parameters"]
    inner: Annotated[Expression, "The body of the lambda"]

LAMBDA_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.LambdaExpression")
LAMBDA_EXPRESSION__BINDINGS__NAME = hydra.core.Name("bindings")
LAMBDA_EXPRESSION__INNER__NAME = hydra.core.Name("inner")

@dataclass(frozen=True)
class LetExpression:
    r"""A 'let' expression."""
    
    bindings: Annotated[frozenlist[LocalBinding], "The local bindings"]
    inner: Annotated[Expression, "The body of the let expression"]

LET_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.LetExpression")
LET_EXPRESSION__BINDINGS__NAME = hydra.core.Name("bindings")
LET_EXPRESSION__INNER__NAME = hydra.core.Name("inner")

@dataclass(frozen=True)
class PrefixApplicationExpression:
    r"""A prefix expression."""
    
    operator: Annotated[Operator, "The prefix operator"]
    rhs: Annotated[Expression, "The operand"]

PREFIX_APPLICATION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.PrefixApplicationExpression")
PREFIX_APPLICATION_EXPRESSION__OPERATOR__NAME = hydra.core.Name("operator")
PREFIX_APPLICATION_EXPRESSION__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class SectionExpression:
    r"""A section expression."""
    
    operator: Annotated[Operator, "The operator"]
    expression: Annotated[Expression, "The operand"]

SECTION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.SectionExpression")
SECTION_EXPRESSION__OPERATOR__NAME = hydra.core.Name("operator")
SECTION_EXPRESSION__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class TypeSignatureExpression:
    r"""A type signature expression."""
    
    inner: Annotated[Expression, "The expression being typed"]
    type: Annotated[Type, "The type signature"]

TYPE_SIGNATURE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.TypeSignatureExpression")
TYPE_SIGNATURE_EXPRESSION__INNER__NAME = hydra.core.Name("inner")
TYPE_SIGNATURE_EXPRESSION__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class UpdateRecordExpression:
    r"""An update record expression."""
    
    inner: Annotated[Expression, "The record being updated"]
    fields: Annotated[frozenlist[FieldUpdate], "The field updates"]

UPDATE_RECORD_EXPRESSION__NAME = hydra.core.Name("hydra.ext.haskell.ast.UpdateRecordExpression")
UPDATE_RECORD_EXPRESSION__INNER__NAME = hydra.core.Name("inner")
UPDATE_RECORD_EXPRESSION__FIELDS__NAME = hydra.core.Name("fields")

@dataclass(frozen=True)
class Field:
    r"""A field (name/type pair)."""
    
    name: Annotated[Name, "The field name"]
    type: Annotated[Type, "The field type"]

FIELD__NAME = hydra.core.Name("hydra.ext.haskell.ast.Field")
FIELD__NAME__NAME = hydra.core.Name("name")
FIELD__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class FieldWithComments:
    r"""A field together with any comments."""
    
    field: Annotated[Field, "The field"]
    comments: Annotated[Maybe[str], "Optional comments"]

FIELD_WITH_COMMENTS__NAME = hydra.core.Name("hydra.ext.haskell.ast.FieldWithComments")
FIELD_WITH_COMMENTS__FIELD__NAME = hydra.core.Name("field")
FIELD_WITH_COMMENTS__COMMENTS__NAME = hydra.core.Name("comments")

@dataclass(frozen=True)
class FieldUpdate:
    r"""A field name and value."""
    
    name: Annotated[Name, "The field name"]
    value: Annotated[Expression, "The field value"]

FIELD_UPDATE__NAME = hydra.core.Name("hydra.ext.haskell.ast.FieldUpdate")
FIELD_UPDATE__NAME__NAME = hydra.core.Name("name")
FIELD_UPDATE__VALUE__NAME = hydra.core.Name("value")

@dataclass(frozen=True)
class Import:
    r"""An import statement."""
    
    qualified: Annotated[bool, "Whether the import is qualified"]
    module: Annotated[ModuleName, "The module being imported"]
    as_: Annotated[Maybe[ModuleName], "Optional alias for the module"]
    spec: Annotated[Maybe[SpecImport], "Optional import specification"]

IMPORT__NAME = hydra.core.Name("hydra.ext.haskell.ast.Import")
IMPORT__QUALIFIED__NAME = hydra.core.Name("qualified")
IMPORT__MODULE__NAME = hydra.core.Name("module")
IMPORT__AS__NAME = hydra.core.Name("as")
IMPORT__SPEC__NAME = hydra.core.Name("spec")

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
    
    pass

SPEC_IMPORT__NAME = hydra.core.Name("hydra.ext.haskell.ast.SpecImport")
SPEC_IMPORT__LIST__NAME = hydra.core.Name("list")
SPEC_IMPORT__HIDING__NAME = hydra.core.Name("hiding")

class ImportModifier(Enum):
    r"""An import modifier ('pattern' or 'type')."""
    
    PATTERN = "pattern"
    
    TYPE = "type"

IMPORT_MODIFIER__NAME = hydra.core.Name("hydra.ext.haskell.ast.ImportModifier")
IMPORT_MODIFIER__PATTERN__NAME = hydra.core.Name("pattern")
IMPORT_MODIFIER__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class ImportExportSpec:
    r"""An import or export specification."""
    
    modifier: Annotated[Maybe[ImportModifier], "Optional import modifier"]
    name: Annotated[Name, "The name being imported or exported"]
    subspec: Annotated[Maybe[SubspecImportExportSpec], "Optional subspecification"]

IMPORT_EXPORT_SPEC__NAME = hydra.core.Name("hydra.ext.haskell.ast.ImportExportSpec")
IMPORT_EXPORT_SPEC__MODIFIER__NAME = hydra.core.Name("modifier")
IMPORT_EXPORT_SPEC__NAME__NAME = hydra.core.Name("name")
IMPORT_EXPORT_SPEC__SUBSPEC__NAME = hydra.core.Name("subspec")

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
    
    pass

SUBSPEC_IMPORT_EXPORT_SPEC__NAME = hydra.core.Name("hydra.ext.haskell.ast.SubspecImportExportSpec")
SUBSPEC_IMPORT_EXPORT_SPEC__ALL__NAME = hydra.core.Name("all")
SUBSPEC_IMPORT_EXPORT_SPEC__LIST__NAME = hydra.core.Name("list")

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
    
    pass

LITERAL__NAME = hydra.core.Name("hydra.ext.haskell.ast.Literal")
LITERAL__CHAR__NAME = hydra.core.Name("char")
LITERAL__DOUBLE__NAME = hydra.core.Name("double")
LITERAL__FLOAT__NAME = hydra.core.Name("float")
LITERAL__INT__NAME = hydra.core.Name("int")
LITERAL__INTEGER__NAME = hydra.core.Name("integer")
LITERAL__STRING__NAME = hydra.core.Name("string")

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
    
    pass

LOCAL_BINDING__NAME = hydra.core.Name("hydra.ext.haskell.ast.LocalBinding")
LOCAL_BINDING__SIGNATURE__NAME = hydra.core.Name("signature")
LOCAL_BINDING__VALUE__NAME = hydra.core.Name("value")

class LocalBindings(Node["frozenlist[LocalBinding]"]):
    r"""A collection of local bindings."""

LOCAL_BINDINGS__NAME = hydra.core.Name("hydra.ext.haskell.ast.LocalBindings")

@dataclass(frozen=True)
class Module:
    r"""A Haskell module."""
    
    head: Annotated[Maybe[ModuleHead], "Optional module head"]
    imports: Annotated[frozenlist[Import], "Import statements"]
    declarations: Annotated[frozenlist[DeclarationWithComments], "Module declarations"]

MODULE__NAME = hydra.core.Name("hydra.ext.haskell.ast.Module")
MODULE__HEAD__NAME = hydra.core.Name("head")
MODULE__IMPORTS__NAME = hydra.core.Name("imports")
MODULE__DECLARATIONS__NAME = hydra.core.Name("declarations")

@dataclass(frozen=True)
class ModuleHead:
    r"""A module head."""
    
    comments: Annotated[Maybe[str], "Optional module-level comments"]
    name: Annotated[ModuleName, "The module name"]
    exports: Annotated[frozenlist[Export], "Export list"]

MODULE_HEAD__NAME = hydra.core.Name("hydra.ext.haskell.ast.ModuleHead")
MODULE_HEAD__COMMENTS__NAME = hydra.core.Name("comments")
MODULE_HEAD__NAME__NAME = hydra.core.Name("name")
MODULE_HEAD__EXPORTS__NAME = hydra.core.Name("exports")

class ModuleName(Node[str]):
    r"""A module name."""

MODULE_NAME__NAME = hydra.core.Name("hydra.ext.haskell.ast.ModuleName")

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
    
    pass

NAME__NAME = hydra.core.Name("hydra.ext.haskell.ast.Name")
NAME__IMPLICIT__NAME = hydra.core.Name("implicit")
NAME__NORMAL__NAME = hydra.core.Name("normal")
NAME__PARENS__NAME = hydra.core.Name("parens")

class NamePart(Node[str]):
    r"""A component of a qualified name."""

NAME_PART__NAME = hydra.core.Name("hydra.ext.haskell.ast.NamePart")

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
    
    pass

OPERATOR__NAME = hydra.core.Name("hydra.ext.haskell.ast.Operator")
OPERATOR__BACKTICK__NAME = hydra.core.Name("backtick")
OPERATOR__NORMAL__NAME = hydra.core.Name("normal")

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
    
    pass

PATTERN__NAME = hydra.core.Name("hydra.ext.haskell.ast.Pattern")
PATTERN__APPLICATION__NAME = hydra.core.Name("application")
PATTERN__AS__NAME = hydra.core.Name("as")
PATTERN__LIST__NAME = hydra.core.Name("list")
PATTERN__LITERAL__NAME = hydra.core.Name("literal")
PATTERN__NAME__NAME = hydra.core.Name("name")
PATTERN__PARENS__NAME = hydra.core.Name("parens")
PATTERN__RECORD__NAME = hydra.core.Name("record")
PATTERN__TUPLE__NAME = hydra.core.Name("tuple")
PATTERN__TYPED__NAME = hydra.core.Name("typed")
PATTERN__WILDCARD__NAME = hydra.core.Name("wildcard")

@dataclass(frozen=True)
class ApplicationPattern:
    r"""An application pattern."""
    
    name: Annotated[Name, "The constructor name"]
    args: Annotated[frozenlist[Pattern], "The pattern arguments"]

APPLICATION_PATTERN__NAME = hydra.core.Name("hydra.ext.haskell.ast.ApplicationPattern")
APPLICATION_PATTERN__NAME__NAME = hydra.core.Name("name")
APPLICATION_PATTERN__ARGS__NAME = hydra.core.Name("args")

@dataclass(frozen=True)
class AsPattern:
    r"""An 'as' pattern."""
    
    name: Annotated[Name, "The bound name"]
    inner: Annotated[Pattern, "The inner pattern"]

AS_PATTERN__NAME = hydra.core.Name("hydra.ext.haskell.ast.AsPattern")
AS_PATTERN__NAME__NAME = hydra.core.Name("name")
AS_PATTERN__INNER__NAME = hydra.core.Name("inner")

@dataclass(frozen=True)
class RecordPattern:
    r"""A record pattern."""
    
    name: Annotated[Name, "The constructor name"]
    fields: Annotated[frozenlist[PatternField], "The field patterns"]

RECORD_PATTERN__NAME = hydra.core.Name("hydra.ext.haskell.ast.RecordPattern")
RECORD_PATTERN__NAME__NAME = hydra.core.Name("name")
RECORD_PATTERN__FIELDS__NAME = hydra.core.Name("fields")

@dataclass(frozen=True)
class TypedPattern:
    r"""A typed pattern."""
    
    inner: Annotated[Pattern, "The inner pattern"]
    type: Annotated[Type, "The type annotation"]

TYPED_PATTERN__NAME = hydra.core.Name("hydra.ext.haskell.ast.TypedPattern")
TYPED_PATTERN__INNER__NAME = hydra.core.Name("inner")
TYPED_PATTERN__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class PatternField:
    r"""A pattern field."""
    
    name: Annotated[Name, "The field name"]
    pattern: Annotated[Pattern, "The field pattern"]

PATTERN_FIELD__NAME = hydra.core.Name("hydra.ext.haskell.ast.PatternField")
PATTERN_FIELD__NAME__NAME = hydra.core.Name("name")
PATTERN_FIELD__PATTERN__NAME = hydra.core.Name("pattern")

@dataclass(frozen=True)
class QualifiedName:
    r"""A qualified name."""
    
    qualifiers: Annotated[frozenlist[NamePart], "The qualifier parts"]
    unqualified: Annotated[NamePart, "The unqualified name part"]

QUALIFIED_NAME__NAME = hydra.core.Name("hydra.ext.haskell.ast.QualifiedName")
QUALIFIED_NAME__QUALIFIERS__NAME = hydra.core.Name("qualifiers")
QUALIFIED_NAME__UNQUALIFIED__NAME = hydra.core.Name("unqualified")

class RightHandSide(Node["Expression"]):
    r"""A right-hand side of a binding."""

RIGHT_HAND_SIDE__NAME = hydra.core.Name("hydra.ext.haskell.ast.RightHandSide")

class Statement(Node["Expression"]):
    r"""A do-notation statement."""

STATEMENT__NAME = hydra.core.Name("hydra.ext.haskell.ast.Statement")

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
    
    pass

TYPE__NAME = hydra.core.Name("hydra.ext.haskell.ast.Type")
TYPE__APPLICATION__NAME = hydra.core.Name("application")
TYPE__CTX__NAME = hydra.core.Name("ctx")
TYPE__FUNCTION__NAME = hydra.core.Name("function")
TYPE__INFIX__NAME = hydra.core.Name("infix")
TYPE__LIST__NAME = hydra.core.Name("list")
TYPE__PARENS__NAME = hydra.core.Name("parens")
TYPE__TUPLE__NAME = hydra.core.Name("tuple")
TYPE__VARIABLE__NAME = hydra.core.Name("variable")

@dataclass(frozen=True)
class ApplicationType:
    r"""An application type."""
    
    context: Annotated[Type, "The type being applied"]
    argument: Annotated[Type, "The type argument"]

APPLICATION_TYPE__NAME = hydra.core.Name("hydra.ext.haskell.ast.ApplicationType")
APPLICATION_TYPE__CONTEXT__NAME = hydra.core.Name("context")
APPLICATION_TYPE__ARGUMENT__NAME = hydra.core.Name("argument")

@dataclass(frozen=True)
class ContextType:
    r"""A type with a context (type class constraints)."""
    
    ctx: Annotated[Assertion, "The type class context"]
    type: Annotated[Type, "The constrained type"]

CONTEXT_TYPE__NAME = hydra.core.Name("hydra.ext.haskell.ast.ContextType")
CONTEXT_TYPE__CTX__NAME = hydra.core.Name("ctx")
CONTEXT_TYPE__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class FunctionType:
    r"""A function type."""
    
    domain: Annotated[Type, "The domain type"]
    codomain: Annotated[Type, "The codomain type"]

FUNCTION_TYPE__NAME = hydra.core.Name("hydra.ext.haskell.ast.FunctionType")
FUNCTION_TYPE__DOMAIN__NAME = hydra.core.Name("domain")
FUNCTION_TYPE__CODOMAIN__NAME = hydra.core.Name("codomain")

@dataclass(frozen=True)
class InfixType:
    r"""An infix type application."""
    
    lhs: Annotated[Type, "The left-hand type"]
    operator: Annotated[Operator, "The type operator"]
    rhs: Annotated[Operator, "The right-hand operator"]

INFIX_TYPE__NAME = hydra.core.Name("hydra.ext.haskell.ast.InfixType")
INFIX_TYPE__LHS__NAME = hydra.core.Name("lhs")
INFIX_TYPE__OPERATOR__NAME = hydra.core.Name("operator")
INFIX_TYPE__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class TypeDeclaration:
    r"""A type synonym declaration."""
    
    name: Annotated[DeclarationHead, "The declaration head"]
    type: Annotated[Type, "The type being defined"]

TYPE_DECLARATION__NAME = hydra.core.Name("hydra.ext.haskell.ast.TypeDeclaration")
TYPE_DECLARATION__NAME__NAME = hydra.core.Name("name")
TYPE_DECLARATION__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class TypeSignature:
    r"""A type signature."""
    
    name: Annotated[Name, "The name being typed"]
    type: Annotated[Type, "The type"]

TYPE_SIGNATURE__NAME = hydra.core.Name("hydra.ext.haskell.ast.TypeSignature")
TYPE_SIGNATURE__NAME__NAME = hydra.core.Name("name")
TYPE_SIGNATURE__TYPE__NAME = hydra.core.Name("type")

@dataclass(frozen=True)
class TypedBinding:
    r"""A binding with its type signature."""
    
    type_signature: Annotated[TypeSignature, "The type signature"]
    value_binding: Annotated[ValueBinding, "The value binding"]

TYPED_BINDING__NAME = hydra.core.Name("hydra.ext.haskell.ast.TypedBinding")
TYPED_BINDING__TYPE_SIGNATURE__NAME = hydra.core.Name("typeSignature")
TYPED_BINDING__VALUE_BINDING__NAME = hydra.core.Name("valueBinding")

class ValueBindingSimple(Node["SimpleValueBinding"]):
    r"""A simple value binding"""

class _ValueBindingMeta(type):
    def __getitem__(cls, item):
        return object

# A value binding.
class ValueBinding(metaclass=_ValueBindingMeta):
    r"""ValueBindingSimple"""
    
    pass

VALUE_BINDING__NAME = hydra.core.Name("hydra.ext.haskell.ast.ValueBinding")
VALUE_BINDING__SIMPLE__NAME = hydra.core.Name("simple")

@dataclass(frozen=True)
class SimpleValueBinding:
    r"""A simple value binding."""
    
    pattern: Annotated[Pattern, "The pattern being bound"]
    rhs: Annotated[RightHandSide, "The right-hand side"]
    local_bindings: Annotated[Maybe[LocalBindings], "Optional local bindings (where clause)"]

SIMPLE_VALUE_BINDING__NAME = hydra.core.Name("hydra.ext.haskell.ast.SimpleValueBinding")
SIMPLE_VALUE_BINDING__PATTERN__NAME = hydra.core.Name("pattern")
SIMPLE_VALUE_BINDING__RHS__NAME = hydra.core.Name("rhs")
SIMPLE_VALUE_BINDING__LOCAL_BINDINGS__NAME = hydra.core.Name("localBindings")

class Variable(Node["Name"]):
    r"""A type variable."""

VARIABLE__NAME = hydra.core.Name("hydra.ext.haskell.ast.Variable")
