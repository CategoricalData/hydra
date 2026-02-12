# Note: this is an automatically generated file. Do not edit.

r"""A Java syntax module. Based on the Oracle Java SE 12 BNF:
  https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html
Note: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias
import hydra.core

class Identifier(Node[str]):
...

IDENTIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.Identifier")

class TypeIdentifier(Node["Identifier"]):
...

TYPE_IDENTIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeIdentifier")

class LiteralNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralNull)
    def __hash__(self):
        return hash("LiteralNull")

class LiteralInteger(Node["IntegerLiteral"]):
...

class LiteralFloatingPoint(Node["FloatingPointLiteral"]):
...

class LiteralBoolean(Node[bool]):
...

class LiteralCharacter(Node[int]):
...

class LiteralString(Node["StringLiteral"]):
...

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

class Literal(metaclass=_LiteralMeta):
    r"""LiteralNull | LiteralInteger | LiteralFloatingPoint | LiteralBoolean | LiteralCharacter | LiteralString"""
    
    pass

LITERAL__NAME = hydra.core.Name("hydra.ext.java.syntax.Literal")
LITERAL__NULL__NAME = hydra.core.Name("null")
LITERAL__INTEGER__NAME = hydra.core.Name("integer")
LITERAL__FLOATING_POINT__NAME = hydra.core.Name("floatingPoint")
LITERAL__BOOLEAN__NAME = hydra.core.Name("boolean")
LITERAL__CHARACTER__NAME = hydra.core.Name("character")
LITERAL__STRING__NAME = hydra.core.Name("string")

class IntegerLiteral(Node[int]):
    r"""Note: this is an approximation which ignores encoding."""

INTEGER_LITERAL__NAME = hydra.core.Name("hydra.ext.java.syntax.IntegerLiteral")

class FloatingPointLiteral(Node[Decimal]):
    r"""Note: this is an approximation which ignores encoding."""

FLOATING_POINT_LITERAL__NAME = hydra.core.Name("hydra.ext.java.syntax.FloatingPointLiteral")

class StringLiteral(Node[str]):
    r"""Note: this is an approximation which ignores encoding."""

STRING_LITERAL__NAME = hydra.core.Name("hydra.ext.java.syntax.StringLiteral")

class TypePrimitive(Node["PrimitiveTypeWithAnnotations"]):
...

class TypeReference(Node["ReferenceType"]):
...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypePrimitive | TypeReference"""
    
    pass

TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.Type")
TYPE__PRIMITIVE__NAME = hydra.core.Name("primitive")
TYPE__REFERENCE__NAME = hydra.core.Name("reference")

@dataclass(frozen=True)
class PrimitiveTypeWithAnnotations:
    type: PrimitiveType
    annotations: frozenlist[Annotation]

PRIMITIVE_TYPE_WITH_ANNOTATIONS__NAME = hydra.core.Name("hydra.ext.java.syntax.PrimitiveTypeWithAnnotations")
PRIMITIVE_TYPE_WITH_ANNOTATIONS__TYPE__NAME = hydra.core.Name("type")
PRIMITIVE_TYPE_WITH_ANNOTATIONS__ANNOTATIONS__NAME = hydra.core.Name("annotations")

class PrimitiveTypeNumeric(Node["NumericType"]):
...

class PrimitiveTypeBoolean:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimitiveTypeBoolean)
    def __hash__(self):
        return hash("PrimitiveTypeBoolean")

class _PrimitiveTypeMeta(type):
    def __getitem__(cls, item):
        return object

class PrimitiveType(metaclass=_PrimitiveTypeMeta):
    r"""PrimitiveTypeNumeric | PrimitiveTypeBoolean"""
    
    pass

PRIMITIVE_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.PrimitiveType")
PRIMITIVE_TYPE__NUMERIC__NAME = hydra.core.Name("numeric")
PRIMITIVE_TYPE__BOOLEAN__NAME = hydra.core.Name("boolean")

class NumericTypeIntegral(Node["IntegralType"]):
...

class NumericTypeFloatingPoint(Node["FloatingPointType"]):
...

class _NumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NumericType(metaclass=_NumericTypeMeta):
    r"""NumericTypeIntegral | NumericTypeFloatingPoint"""
    
    pass

NUMERIC_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.NumericType")
NUMERIC_TYPE__INTEGRAL__NAME = hydra.core.Name("integral")
NUMERIC_TYPE__FLOATING_POINT__NAME = hydra.core.Name("floatingPoint")

class IntegralType(Enum):
    BYTE = "byte"
    
    SHORT = "short"
    
    INT = "int"
    
    LONG = "long"
    
    CHAR = "char"

INTEGRAL_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.IntegralType")
INTEGRAL_TYPE__BYTE__NAME = hydra.core.Name("byte")
INTEGRAL_TYPE__SHORT__NAME = hydra.core.Name("short")
INTEGRAL_TYPE__INT__NAME = hydra.core.Name("int")
INTEGRAL_TYPE__LONG__NAME = hydra.core.Name("long")
INTEGRAL_TYPE__CHAR__NAME = hydra.core.Name("char")

class FloatingPointType(Enum):
    FLOAT = "float"
    
    DOUBLE = "double"

FLOATING_POINT_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.FloatingPointType")
FLOATING_POINT_TYPE__FLOAT__NAME = hydra.core.Name("float")
FLOATING_POINT_TYPE__DOUBLE__NAME = hydra.core.Name("double")

class ReferenceTypeClassOrInterface(Node["ClassOrInterfaceType"]):
...

class ReferenceTypeVariable(Node["TypeVariable"]):
...

class ReferenceTypeArray(Node["ArrayType"]):
...

class _ReferenceTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ReferenceType(metaclass=_ReferenceTypeMeta):
    r"""ReferenceTypeClassOrInterface | ReferenceTypeVariable | ReferenceTypeArray"""
    
    pass

REFERENCE_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.ReferenceType")
REFERENCE_TYPE__CLASS_OR_INTERFACE__NAME = hydra.core.Name("classOrInterface")
REFERENCE_TYPE__VARIABLE__NAME = hydra.core.Name("variable")
REFERENCE_TYPE__ARRAY__NAME = hydra.core.Name("array")

class ClassOrInterfaceTypeClass(Node["ClassType"]):
...

class ClassOrInterfaceTypeInterface(Node["InterfaceType"]):
...

class _ClassOrInterfaceTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ClassOrInterfaceType(metaclass=_ClassOrInterfaceTypeMeta):
    r"""ClassOrInterfaceTypeClass | ClassOrInterfaceTypeInterface"""
    
    pass

CLASS_OR_INTERFACE_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceType")
CLASS_OR_INTERFACE_TYPE__CLASS__NAME = hydra.core.Name("class")
CLASS_OR_INTERFACE_TYPE__INTERFACE__NAME = hydra.core.Name("interface")

@dataclass(frozen=True)
class ClassType:
    annotations: frozenlist[Annotation]
    qualifier: ClassTypeQualifier
    identifier: TypeIdentifier
    arguments: frozenlist[TypeArgument]

CLASS_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassType")
CLASS_TYPE__ANNOTATIONS__NAME = hydra.core.Name("annotations")
CLASS_TYPE__QUALIFIER__NAME = hydra.core.Name("qualifier")
CLASS_TYPE__IDENTIFIER__NAME = hydra.core.Name("identifier")
CLASS_TYPE__ARGUMENTS__NAME = hydra.core.Name("arguments")

class ClassTypeQualifierNone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassTypeQualifierNone)
    def __hash__(self):
        return hash("ClassTypeQualifierNone")

class ClassTypeQualifierPackage(Node["PackageName"]):
...

class ClassTypeQualifierParent(Node["ClassOrInterfaceType"]):
...

class _ClassTypeQualifierMeta(type):
    def __getitem__(cls, item):
        return object

class ClassTypeQualifier(metaclass=_ClassTypeQualifierMeta):
    r"""ClassTypeQualifierNone | ClassTypeQualifierPackage | ClassTypeQualifierParent"""
    
    pass

CLASS_TYPE_QUALIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassTypeQualifier")
CLASS_TYPE_QUALIFIER__NONE__NAME = hydra.core.Name("none")
CLASS_TYPE_QUALIFIER__PACKAGE__NAME = hydra.core.Name("package")
CLASS_TYPE_QUALIFIER__PARENT__NAME = hydra.core.Name("parent")

class InterfaceType(Node["ClassType"]):
...

INTERFACE_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceType")

@dataclass(frozen=True)
class TypeVariable:
    annotations: frozenlist[Annotation]
    identifier: TypeIdentifier

TYPE_VARIABLE__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeVariable")
TYPE_VARIABLE__ANNOTATIONS__NAME = hydra.core.Name("annotations")
TYPE_VARIABLE__IDENTIFIER__NAME = hydra.core.Name("identifier")

@dataclass(frozen=True)
class ArrayType:
    dims: Dims
    variant: ArrayType_Variant

ARRAY_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayType")
ARRAY_TYPE__DIMS__NAME = hydra.core.Name("dims")
ARRAY_TYPE__VARIANT__NAME = hydra.core.Name("variant")

class ArrayType_VariantPrimitive(Node["PrimitiveTypeWithAnnotations"]):
...

class ArrayType_VariantClassOrInterface(Node["ClassOrInterfaceType"]):
...

class ArrayType_VariantVariable(Node["TypeVariable"]):
...

class _ArrayType_VariantMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayType_Variant(metaclass=_ArrayType_VariantMeta):
    r"""ArrayType_VariantPrimitive | ArrayType_VariantClassOrInterface | ArrayType_VariantVariable"""
    
    pass

ARRAY_TYPE__VARIANT__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayType_Variant")
ARRAY_TYPE__VARIANT__PRIMITIVE__NAME = hydra.core.Name("primitive")
ARRAY_TYPE__VARIANT__CLASS_OR_INTERFACE__NAME = hydra.core.Name("classOrInterface")
ARRAY_TYPE__VARIANT__VARIABLE__NAME = hydra.core.Name("variable")

class Dims(Node["frozenlist[frozenlist[Annotation]]"]):
...

DIMS__NAME = hydra.core.Name("hydra.ext.java.syntax.Dims")

@dataclass(frozen=True)
class TypeParameter:
    modifiers: frozenlist[TypeParameterModifier]
    identifier: TypeIdentifier
    bound: Maybe[TypeBound]

TYPE_PARAMETER__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeParameter")
TYPE_PARAMETER__MODIFIERS__NAME = hydra.core.Name("modifiers")
TYPE_PARAMETER__IDENTIFIER__NAME = hydra.core.Name("identifier")
TYPE_PARAMETER__BOUND__NAME = hydra.core.Name("bound")

class TypeParameterModifier(Node["Annotation"]):
...

TYPE_PARAMETER_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeParameterModifier")

class TypeBoundVariable(Node["TypeVariable"]):
...

class TypeBoundClassOrInterface(Node["TypeBound_ClassOrInterface"]):
...

class _TypeBoundMeta(type):
    def __getitem__(cls, item):
        return object

class TypeBound(metaclass=_TypeBoundMeta):
    r"""TypeBoundVariable | TypeBoundClassOrInterface"""
    
    pass

TYPE_BOUND__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeBound")
TYPE_BOUND__VARIABLE__NAME = hydra.core.Name("variable")
TYPE_BOUND__CLASS_OR_INTERFACE__NAME = hydra.core.Name("classOrInterface")

@dataclass(frozen=True)
class TypeBound_ClassOrInterface:
    type: ClassOrInterfaceType
    additional: frozenlist[AdditionalBound]

TYPE_BOUND__CLASS_OR_INTERFACE__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeBound_ClassOrInterface")
TYPE_BOUND__CLASS_OR_INTERFACE__TYPE__NAME = hydra.core.Name("type")
TYPE_BOUND__CLASS_OR_INTERFACE__ADDITIONAL__NAME = hydra.core.Name("additional")

class AdditionalBound(Node["InterfaceType"]):
...

ADDITIONAL_BOUND__NAME = hydra.core.Name("hydra.ext.java.syntax.AdditionalBound")

class TypeArgumentReference(Node["ReferenceType"]):
...

class TypeArgumentWildcard(Node["Wildcard"]):
...

class _TypeArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TypeArgument(metaclass=_TypeArgumentMeta):
    r"""TypeArgumentReference | TypeArgumentWildcard"""
    
    pass

TYPE_ARGUMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeArgument")
TYPE_ARGUMENT__REFERENCE__NAME = hydra.core.Name("reference")
TYPE_ARGUMENT__WILDCARD__NAME = hydra.core.Name("wildcard")

@dataclass(frozen=True)
class Wildcard:
    annotations: frozenlist[Annotation]
    wildcard: Maybe[WildcardBounds]

WILDCARD__NAME = hydra.core.Name("hydra.ext.java.syntax.Wildcard")
WILDCARD__ANNOTATIONS__NAME = hydra.core.Name("annotations")
WILDCARD__WILDCARD__NAME = hydra.core.Name("wildcard")

class WildcardBoundsExtends(Node["ReferenceType"]):
...

class WildcardBoundsSuper(Node["ReferenceType"]):
...

class _WildcardBoundsMeta(type):
    def __getitem__(cls, item):
        return object

class WildcardBounds(metaclass=_WildcardBoundsMeta):
    r"""WildcardBoundsExtends | WildcardBoundsSuper"""
    
    pass

WILDCARD_BOUNDS__NAME = hydra.core.Name("hydra.ext.java.syntax.WildcardBounds")
WILDCARD_BOUNDS__EXTENDS__NAME = hydra.core.Name("extends")
WILDCARD_BOUNDS__SUPER__NAME = hydra.core.Name("super")

@dataclass(frozen=True)
class ModuleName:
    identifier: Identifier
    name: Maybe[ModuleName]

MODULE_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.ModuleName")
MODULE_NAME__IDENTIFIER__NAME = hydra.core.Name("identifier")
MODULE_NAME__NAME__NAME = hydra.core.Name("name")

class PackageName(Node["frozenlist[Identifier]"]):
...

PACKAGE_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.PackageName")

@dataclass(frozen=True)
class TypeName:
    identifier: TypeIdentifier
    qualifier: Maybe[PackageOrTypeName]

TYPE_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeName")
TYPE_NAME__IDENTIFIER__NAME = hydra.core.Name("identifier")
TYPE_NAME__QUALIFIER__NAME = hydra.core.Name("qualifier")

@dataclass(frozen=True)
class ExpressionName:
    qualifier: Maybe[AmbiguousName]
    identifier: Identifier

EXPRESSION_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.ExpressionName")
EXPRESSION_NAME__QUALIFIER__NAME = hydra.core.Name("qualifier")
EXPRESSION_NAME__IDENTIFIER__NAME = hydra.core.Name("identifier")

class MethodName(Node["Identifier"]):
...

METHOD_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodName")

class PackageOrTypeName(Node["frozenlist[Identifier]"]):
...

PACKAGE_OR_TYPE_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.PackageOrTypeName")

class AmbiguousName(Node["frozenlist[Identifier]"]):
...

AMBIGUOUS_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.AmbiguousName")

class CompilationUnitOrdinary(Node["OrdinaryCompilationUnit"]):
...

class CompilationUnitModular(Node["ModularCompilationUnit"]):
...

class _CompilationUnitMeta(type):
    def __getitem__(cls, item):
        return object

class CompilationUnit(metaclass=_CompilationUnitMeta):
    r"""CompilationUnitOrdinary | CompilationUnitModular"""
    
    pass

COMPILATION_UNIT__NAME = hydra.core.Name("hydra.ext.java.syntax.CompilationUnit")
COMPILATION_UNIT__ORDINARY__NAME = hydra.core.Name("ordinary")
COMPILATION_UNIT__MODULAR__NAME = hydra.core.Name("modular")

@dataclass(frozen=True)
class OrdinaryCompilationUnit:
    package: Maybe[PackageDeclaration]
    imports: frozenlist[ImportDeclaration]
    types: frozenlist[TypeDeclarationWithComments]

ORDINARY_COMPILATION_UNIT__NAME = hydra.core.Name("hydra.ext.java.syntax.OrdinaryCompilationUnit")
ORDINARY_COMPILATION_UNIT__PACKAGE__NAME = hydra.core.Name("package")
ORDINARY_COMPILATION_UNIT__IMPORTS__NAME = hydra.core.Name("imports")
ORDINARY_COMPILATION_UNIT__TYPES__NAME = hydra.core.Name("types")

@dataclass(frozen=True)
class ModularCompilationUnit:
    imports: frozenlist[ImportDeclaration]
    module: ModuleDeclaration

MODULAR_COMPILATION_UNIT__NAME = hydra.core.Name("hydra.ext.java.syntax.ModularCompilationUnit")
MODULAR_COMPILATION_UNIT__IMPORTS__NAME = hydra.core.Name("imports")
MODULAR_COMPILATION_UNIT__MODULE__NAME = hydra.core.Name("module")

@dataclass(frozen=True)
class PackageDeclaration:
    modifiers: frozenlist[PackageModifier]
    identifiers: frozenlist[Identifier]

PACKAGE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.PackageDeclaration")
PACKAGE_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
PACKAGE_DECLARATION__IDENTIFIERS__NAME = hydra.core.Name("identifiers")

class PackageModifier(Node["Annotation"]):
...

PACKAGE_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.PackageModifier")

class ImportDeclarationSingleType(Node["SingleTypeImportDeclaration"]):
...

class ImportDeclarationTypeImportOnDemand(Node["TypeImportOnDemandDeclaration"]):
...

class ImportDeclarationSingleStaticImport(Node["SingleStaticImportDeclaration"]):
...

class ImportDeclarationStaticImportOnDemand(Node["StaticImportOnDemandDeclaration"]):
...

class _ImportDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ImportDeclaration(metaclass=_ImportDeclarationMeta):
    r"""ImportDeclarationSingleType | ImportDeclarationTypeImportOnDemand | ImportDeclarationSingleStaticImport | ImportDeclarationStaticImportOnDemand"""
    
    pass

IMPORT_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ImportDeclaration")
IMPORT_DECLARATION__SINGLE_TYPE__NAME = hydra.core.Name("singleType")
IMPORT_DECLARATION__TYPE_IMPORT_ON_DEMAND__NAME = hydra.core.Name("typeImportOnDemand")
IMPORT_DECLARATION__SINGLE_STATIC_IMPORT__NAME = hydra.core.Name("singleStaticImport")
IMPORT_DECLARATION__STATIC_IMPORT_ON_DEMAND__NAME = hydra.core.Name("staticImportOnDemand")

class SingleTypeImportDeclaration(Node["TypeName"]):
...

SINGLE_TYPE_IMPORT_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.SingleTypeImportDeclaration")

class TypeImportOnDemandDeclaration(Node["PackageOrTypeName"]):
...

TYPE_IMPORT_ON_DEMAND_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeImportOnDemandDeclaration")

@dataclass(frozen=True)
class SingleStaticImportDeclaration:
    type_name: TypeName
    identifier: Identifier

SINGLE_STATIC_IMPORT_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.SingleStaticImportDeclaration")
SINGLE_STATIC_IMPORT_DECLARATION__TYPE_NAME__NAME = hydra.core.Name("typeName")
SINGLE_STATIC_IMPORT_DECLARATION__IDENTIFIER__NAME = hydra.core.Name("identifier")

class StaticImportOnDemandDeclaration(Node["TypeName"]):
...

STATIC_IMPORT_ON_DEMAND_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.StaticImportOnDemandDeclaration")

class TypeDeclarationClass(Node["ClassDeclaration"]):
...

class TypeDeclarationInterface(Node["InterfaceDeclaration"]):
...

class TypeDeclarationNone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeDeclarationNone)
    def __hash__(self):
        return hash("TypeDeclarationNone")

class _TypeDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class TypeDeclaration(metaclass=_TypeDeclarationMeta):
    r"""TypeDeclarationClass | TypeDeclarationInterface | TypeDeclarationNone"""
    
    pass

TYPE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeDeclaration")
TYPE_DECLARATION__CLASS__NAME = hydra.core.Name("class")
TYPE_DECLARATION__INTERFACE__NAME = hydra.core.Name("interface")
TYPE_DECLARATION__NONE__NAME = hydra.core.Name("none")

@dataclass(frozen=True)
class TypeDeclarationWithComments:
    value: TypeDeclaration
    comments: Maybe[str]

TYPE_DECLARATION_WITH_COMMENTS__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeDeclarationWithComments")
TYPE_DECLARATION_WITH_COMMENTS__VALUE__NAME = hydra.core.Name("value")
TYPE_DECLARATION_WITH_COMMENTS__COMMENTS__NAME = hydra.core.Name("comments")

@dataclass(frozen=True)
class ModuleDeclaration:
    annotations: frozenlist[Annotation]
    open: bool
    identifiers: frozenlist[Identifier]
    directives: frozenlist[frozenlist[ModuleDirective]]

MODULE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ModuleDeclaration")
MODULE_DECLARATION__ANNOTATIONS__NAME = hydra.core.Name("annotations")
MODULE_DECLARATION__OPEN__NAME = hydra.core.Name("open")
MODULE_DECLARATION__IDENTIFIERS__NAME = hydra.core.Name("identifiers")
MODULE_DECLARATION__DIRECTIVES__NAME = hydra.core.Name("directives")

class ModuleDirectiveRequires(Node["ModuleDirective_Requires"]):
...

class ModuleDirectiveExports(Node["ModuleDirective_ExportsOrOpens"]):
...

class ModuleDirectiveOpens(Node["ModuleDirective_ExportsOrOpens"]):
...

class ModuleDirectiveUses(Node["TypeName"]):
...

class ModuleDirectiveProvides(Node["ModuleDirective_Provides"]):
...

class _ModuleDirectiveMeta(type):
    def __getitem__(cls, item):
        return object

class ModuleDirective(metaclass=_ModuleDirectiveMeta):
    r"""ModuleDirectiveRequires | ModuleDirectiveExports | ModuleDirectiveOpens | ModuleDirectiveUses | ModuleDirectiveProvides"""
    
    pass

MODULE_DIRECTIVE__NAME = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective")
MODULE_DIRECTIVE__REQUIRES__NAME = hydra.core.Name("requires")
MODULE_DIRECTIVE__EXPORTS__NAME = hydra.core.Name("exports")
MODULE_DIRECTIVE__OPENS__NAME = hydra.core.Name("opens")
MODULE_DIRECTIVE__USES__NAME = hydra.core.Name("uses")
MODULE_DIRECTIVE__PROVIDES__NAME = hydra.core.Name("provides")

@dataclass(frozen=True)
class ModuleDirective_Requires:
    modifiers: frozenlist[RequiresModifier]
    module: ModuleName

MODULE_DIRECTIVE__REQUIRES__NAME = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_Requires")
MODULE_DIRECTIVE__REQUIRES__MODIFIERS__NAME = hydra.core.Name("modifiers")
MODULE_DIRECTIVE__REQUIRES__MODULE__NAME = hydra.core.Name("module")

@dataclass(frozen=True)
class ModuleDirective_ExportsOrOpens:
    package: PackageName
    modules: Annotated[frozenlist[ModuleName], "At least one module"]

MODULE_DIRECTIVE__EXPORTS_OR_OPENS__NAME = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens")
MODULE_DIRECTIVE__EXPORTS_OR_OPENS__PACKAGE__NAME = hydra.core.Name("package")
MODULE_DIRECTIVE__EXPORTS_OR_OPENS__MODULES__NAME = hydra.core.Name("modules")

@dataclass(frozen=True)
class ModuleDirective_Provides:
    to: TypeName
    with_: Annotated[frozenlist[TypeName], "At least one type"]

MODULE_DIRECTIVE__PROVIDES__NAME = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_Provides")
MODULE_DIRECTIVE__PROVIDES__TO__NAME = hydra.core.Name("to")
MODULE_DIRECTIVE__PROVIDES__WITH__NAME = hydra.core.Name("with")

class RequiresModifier(Enum):
    TRANSITIVE = "transitive"
    
    STATIC = "static"

REQUIRES_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.RequiresModifier")
REQUIRES_MODIFIER__TRANSITIVE__NAME = hydra.core.Name("transitive")
REQUIRES_MODIFIER__STATIC__NAME = hydra.core.Name("static")

class ClassDeclarationNormal(Node["NormalClassDeclaration"]):
...

class ClassDeclarationEnum(Node["EnumDeclaration"]):
...

class _ClassDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ClassDeclaration(metaclass=_ClassDeclarationMeta):
    r"""ClassDeclarationNormal | ClassDeclarationEnum"""
    
    pass

CLASS_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassDeclaration")
CLASS_DECLARATION__NORMAL__NAME = hydra.core.Name("normal")
CLASS_DECLARATION__ENUM__NAME = hydra.core.Name("enum")

@dataclass(frozen=True)
class NormalClassDeclaration:
    modifiers: frozenlist[ClassModifier]
    identifier: TypeIdentifier
    parameters: frozenlist[TypeParameter]
    extends: Maybe[ClassType]
    implements: frozenlist[InterfaceType]
    body: ClassBody

NORMAL_CLASS_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.NormalClassDeclaration")
NORMAL_CLASS_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
NORMAL_CLASS_DECLARATION__IDENTIFIER__NAME = hydra.core.Name("identifier")
NORMAL_CLASS_DECLARATION__PARAMETERS__NAME = hydra.core.Name("parameters")
NORMAL_CLASS_DECLARATION__EXTENDS__NAME = hydra.core.Name("extends")
NORMAL_CLASS_DECLARATION__IMPLEMENTS__NAME = hydra.core.Name("implements")
NORMAL_CLASS_DECLARATION__BODY__NAME = hydra.core.Name("body")

class ClassModifierAnnotation(Node["Annotation"]):
...

class ClassModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierPublic)
    def __hash__(self):
        return hash("ClassModifierPublic")

class ClassModifierProtected:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierProtected)
    def __hash__(self):
        return hash("ClassModifierProtected")

class ClassModifierPrivate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierPrivate)
    def __hash__(self):
        return hash("ClassModifierPrivate")

class ClassModifierAbstract:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierAbstract)
    def __hash__(self):
        return hash("ClassModifierAbstract")

class ClassModifierStatic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierStatic)
    def __hash__(self):
        return hash("ClassModifierStatic")

class ClassModifierFinal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierFinal)
    def __hash__(self):
        return hash("ClassModifierFinal")

class ClassModifierStrictfp:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassModifierStrictfp)
    def __hash__(self):
        return hash("ClassModifierStrictfp")

class _ClassModifierMeta(type):
    def __getitem__(cls, item):
        return object

class ClassModifier(metaclass=_ClassModifierMeta):
    r"""ClassModifierAnnotation | ClassModifierPublic | ClassModifierProtected | ClassModifierPrivate | ClassModifierAbstract | ClassModifierStatic | ClassModifierFinal | ClassModifierStrictfp"""
    
    pass

CLASS_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassModifier")
CLASS_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
CLASS_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
CLASS_MODIFIER__PROTECTED__NAME = hydra.core.Name("protected")
CLASS_MODIFIER__PRIVATE__NAME = hydra.core.Name("private")
CLASS_MODIFIER__ABSTRACT__NAME = hydra.core.Name("abstract")
CLASS_MODIFIER__STATIC__NAME = hydra.core.Name("static")
CLASS_MODIFIER__FINAL__NAME = hydra.core.Name("final")
CLASS_MODIFIER__STRICTFP__NAME = hydra.core.Name("strictfp")

class ClassBody(Node["frozenlist[ClassBodyDeclarationWithComments]"]):
...

CLASS_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassBody")

class ClassBodyDeclarationClassMember(Node["ClassMemberDeclaration"]):
...

class ClassBodyDeclarationInstanceInitializer(Node["InstanceInitializer"]):
...

class ClassBodyDeclarationStaticInitializer(Node["StaticInitializer"]):
...

class ClassBodyDeclarationConstructorDeclaration(Node["ConstructorDeclaration"]):
...

class _ClassBodyDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ClassBodyDeclaration(metaclass=_ClassBodyDeclarationMeta):
    r"""ClassBodyDeclarationClassMember | ClassBodyDeclarationInstanceInitializer | ClassBodyDeclarationStaticInitializer | ClassBodyDeclarationConstructorDeclaration"""
    
    pass

CLASS_BODY_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassBodyDeclaration")
CLASS_BODY_DECLARATION__CLASS_MEMBER__NAME = hydra.core.Name("classMember")
CLASS_BODY_DECLARATION__INSTANCE_INITIALIZER__NAME = hydra.core.Name("instanceInitializer")
CLASS_BODY_DECLARATION__STATIC_INITIALIZER__NAME = hydra.core.Name("staticInitializer")
CLASS_BODY_DECLARATION__CONSTRUCTOR_DECLARATION__NAME = hydra.core.Name("constructorDeclaration")

@dataclass(frozen=True)
class ClassBodyDeclarationWithComments:
    value: ClassBodyDeclaration
    comments: Maybe[str]

CLASS_BODY_DECLARATION_WITH_COMMENTS__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassBodyDeclarationWithComments")
CLASS_BODY_DECLARATION_WITH_COMMENTS__VALUE__NAME = hydra.core.Name("value")
CLASS_BODY_DECLARATION_WITH_COMMENTS__COMMENTS__NAME = hydra.core.Name("comments")

class ClassMemberDeclarationField(Node["FieldDeclaration"]):
...

class ClassMemberDeclarationMethod(Node["MethodDeclaration"]):
...

class ClassMemberDeclarationClass(Node["ClassDeclaration"]):
...

class ClassMemberDeclarationInterface(Node["InterfaceDeclaration"]):
...

class ClassMemberDeclarationNone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassMemberDeclarationNone)
    def __hash__(self):
        return hash("ClassMemberDeclarationNone")

class _ClassMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ClassMemberDeclaration(metaclass=_ClassMemberDeclarationMeta):
    r"""ClassMemberDeclarationField | ClassMemberDeclarationMethod | ClassMemberDeclarationClass | ClassMemberDeclarationInterface | ClassMemberDeclarationNone"""
    
    pass

CLASS_MEMBER_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassMemberDeclaration")
CLASS_MEMBER_DECLARATION__FIELD__NAME = hydra.core.Name("field")
CLASS_MEMBER_DECLARATION__METHOD__NAME = hydra.core.Name("method")
CLASS_MEMBER_DECLARATION__CLASS__NAME = hydra.core.Name("class")
CLASS_MEMBER_DECLARATION__INTERFACE__NAME = hydra.core.Name("interface")
CLASS_MEMBER_DECLARATION__NONE__NAME = hydra.core.Name("none")

@dataclass(frozen=True)
class FieldDeclaration:
    modifiers: frozenlist[FieldModifier]
    unann_type: UnannType
    variable_declarators: frozenlist[VariableDeclarator]

FIELD_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.FieldDeclaration")
FIELD_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
FIELD_DECLARATION__UNANN_TYPE__NAME = hydra.core.Name("unannType")
FIELD_DECLARATION__VARIABLE_DECLARATORS__NAME = hydra.core.Name("variableDeclarators")

class FieldModifierAnnotation(Node["Annotation"]):
...

class FieldModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierPublic)
    def __hash__(self):
        return hash("FieldModifierPublic")

class FieldModifierProtected:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierProtected)
    def __hash__(self):
        return hash("FieldModifierProtected")

class FieldModifierPrivate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierPrivate)
    def __hash__(self):
        return hash("FieldModifierPrivate")

class FieldModifierStatic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierStatic)
    def __hash__(self):
        return hash("FieldModifierStatic")

class FieldModifierFinal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierFinal)
    def __hash__(self):
        return hash("FieldModifierFinal")

class FieldModifierTransient:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierTransient)
    def __hash__(self):
        return hash("FieldModifierTransient")

class FieldModifierVolatile:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldModifierVolatile)
    def __hash__(self):
        return hash("FieldModifierVolatile")

class _FieldModifierMeta(type):
    def __getitem__(cls, item):
        return object

class FieldModifier(metaclass=_FieldModifierMeta):
    r"""FieldModifierAnnotation | FieldModifierPublic | FieldModifierProtected | FieldModifierPrivate | FieldModifierStatic | FieldModifierFinal | FieldModifierTransient | FieldModifierVolatile"""
    
    pass

FIELD_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.FieldModifier")
FIELD_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
FIELD_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
FIELD_MODIFIER__PROTECTED__NAME = hydra.core.Name("protected")
FIELD_MODIFIER__PRIVATE__NAME = hydra.core.Name("private")
FIELD_MODIFIER__STATIC__NAME = hydra.core.Name("static")
FIELD_MODIFIER__FINAL__NAME = hydra.core.Name("final")
FIELD_MODIFIER__TRANSIENT__NAME = hydra.core.Name("transient")
FIELD_MODIFIER__VOLATILE__NAME = hydra.core.Name("volatile")

@dataclass(frozen=True)
class VariableDeclarator:
    id: VariableDeclaratorId
    initializer: Maybe[VariableInitializer]

VARIABLE_DECLARATOR__NAME = hydra.core.Name("hydra.ext.java.syntax.VariableDeclarator")
VARIABLE_DECLARATOR__ID__NAME = hydra.core.Name("id")
VARIABLE_DECLARATOR__INITIALIZER__NAME = hydra.core.Name("initializer")

@dataclass(frozen=True)
class VariableDeclaratorId:
    identifier: Identifier
    dims: Maybe[Dims]

VARIABLE_DECLARATOR_ID__NAME = hydra.core.Name("hydra.ext.java.syntax.VariableDeclaratorId")
VARIABLE_DECLARATOR_ID__IDENTIFIER__NAME = hydra.core.Name("identifier")
VARIABLE_DECLARATOR_ID__DIMS__NAME = hydra.core.Name("dims")

class VariableInitializerExpression(Node["Expression"]):
...

class VariableInitializerArrayInitializer(Node["ArrayInitializer"]):
...

class _VariableInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class VariableInitializer(metaclass=_VariableInitializerMeta):
    r"""VariableInitializerExpression | VariableInitializerArrayInitializer"""
    
    pass

VARIABLE_INITIALIZER__NAME = hydra.core.Name("hydra.ext.java.syntax.VariableInitializer")
VARIABLE_INITIALIZER__EXPRESSION__NAME = hydra.core.Name("expression")
VARIABLE_INITIALIZER__ARRAY_INITIALIZER__NAME = hydra.core.Name("arrayInitializer")

class UnannType(Node["Type"]):
    r"""A Type which does not allow annotations."""

UNANN_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.UnannType")

class UnannClassType(Node["ClassType"]):
    r"""A ClassType which does not allow annotations."""

UNANN_CLASS_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.UnannClassType")

@dataclass(frozen=True)
class MethodDeclaration:
    annotations: Annotated[frozenlist[Annotation], "Note: simple methods cannot have annotations"]
    modifiers: frozenlist[MethodModifier]
    header: MethodHeader
    body: MethodBody

METHOD_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodDeclaration")
METHOD_DECLARATION__ANNOTATIONS__NAME = hydra.core.Name("annotations")
METHOD_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
METHOD_DECLARATION__HEADER__NAME = hydra.core.Name("header")
METHOD_DECLARATION__BODY__NAME = hydra.core.Name("body")

class MethodModifierAnnotation(Node["Annotation"]):
...

class MethodModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierPublic)
    def __hash__(self):
        return hash("MethodModifierPublic")

class MethodModifierProtected:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierProtected)
    def __hash__(self):
        return hash("MethodModifierProtected")

class MethodModifierPrivate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierPrivate)
    def __hash__(self):
        return hash("MethodModifierPrivate")

class MethodModifierAbstract:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierAbstract)
    def __hash__(self):
        return hash("MethodModifierAbstract")

class MethodModifierStatic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierStatic)
    def __hash__(self):
        return hash("MethodModifierStatic")

class MethodModifierFinal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierFinal)
    def __hash__(self):
        return hash("MethodModifierFinal")

class MethodModifierSynchronized:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierSynchronized)
    def __hash__(self):
        return hash("MethodModifierSynchronized")

class MethodModifierNative:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierNative)
    def __hash__(self):
        return hash("MethodModifierNative")

class MethodModifierStrictfb:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodModifierStrictfb)
    def __hash__(self):
        return hash("MethodModifierStrictfb")

class _MethodModifierMeta(type):
    def __getitem__(cls, item):
        return object

class MethodModifier(metaclass=_MethodModifierMeta):
    r"""MethodModifierAnnotation | MethodModifierPublic | MethodModifierProtected | MethodModifierPrivate | MethodModifierAbstract | MethodModifierStatic | MethodModifierFinal | MethodModifierSynchronized | MethodModifierNative | MethodModifierStrictfb"""
    
    pass

METHOD_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodModifier")
METHOD_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
METHOD_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
METHOD_MODIFIER__PROTECTED__NAME = hydra.core.Name("protected")
METHOD_MODIFIER__PRIVATE__NAME = hydra.core.Name("private")
METHOD_MODIFIER__ABSTRACT__NAME = hydra.core.Name("abstract")
METHOD_MODIFIER__STATIC__NAME = hydra.core.Name("static")
METHOD_MODIFIER__FINAL__NAME = hydra.core.Name("final")
METHOD_MODIFIER__SYNCHRONIZED__NAME = hydra.core.Name("synchronized")
METHOD_MODIFIER__NATIVE__NAME = hydra.core.Name("native")
METHOD_MODIFIER__STRICTFB__NAME = hydra.core.Name("strictfb")

@dataclass(frozen=True)
class MethodHeader:
    parameters: frozenlist[TypeParameter]
    result: Result
    declarator: MethodDeclarator
    throws: Maybe[Throws]

METHOD_HEADER__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodHeader")
METHOD_HEADER__PARAMETERS__NAME = hydra.core.Name("parameters")
METHOD_HEADER__RESULT__NAME = hydra.core.Name("result")
METHOD_HEADER__DECLARATOR__NAME = hydra.core.Name("declarator")
METHOD_HEADER__THROWS__NAME = hydra.core.Name("throws")

class ResultType(Node["UnannType"]):
...

class ResultVoid:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ResultVoid)
    def __hash__(self):
        return hash("ResultVoid")

class _ResultMeta(type):
    def __getitem__(cls, item):
        return object

class Result(metaclass=_ResultMeta):
    r"""ResultType | ResultVoid"""
    
    pass

RESULT__NAME = hydra.core.Name("hydra.ext.java.syntax.Result")
RESULT__TYPE__NAME = hydra.core.Name("type")
RESULT__VOID__NAME = hydra.core.Name("void")

@dataclass(frozen=True)
class MethodDeclarator:
    identifier: Identifier
    receiver_parameter: Maybe[ReceiverParameter]
    formal_parameters: frozenlist[FormalParameter]

METHOD_DECLARATOR__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodDeclarator")
METHOD_DECLARATOR__IDENTIFIER__NAME = hydra.core.Name("identifier")
METHOD_DECLARATOR__RECEIVER_PARAMETER__NAME = hydra.core.Name("receiverParameter")
METHOD_DECLARATOR__FORMAL_PARAMETERS__NAME = hydra.core.Name("formalParameters")

@dataclass(frozen=True)
class ReceiverParameter:
    annotations: frozenlist[Annotation]
    unann_type: UnannType
    identifier: Maybe[Identifier]

RECEIVER_PARAMETER__NAME = hydra.core.Name("hydra.ext.java.syntax.ReceiverParameter")
RECEIVER_PARAMETER__ANNOTATIONS__NAME = hydra.core.Name("annotations")
RECEIVER_PARAMETER__UNANN_TYPE__NAME = hydra.core.Name("unannType")
RECEIVER_PARAMETER__IDENTIFIER__NAME = hydra.core.Name("identifier")

class FormalParameterSimple(Node["FormalParameter_Simple"]):
...

class FormalParameterVariableArity(Node["VariableArityParameter"]):
...

class _FormalParameterMeta(type):
    def __getitem__(cls, item):
        return object

class FormalParameter(metaclass=_FormalParameterMeta):
    r"""FormalParameterSimple | FormalParameterVariableArity"""
    
    pass

FORMAL_PARAMETER__NAME = hydra.core.Name("hydra.ext.java.syntax.FormalParameter")
FORMAL_PARAMETER__SIMPLE__NAME = hydra.core.Name("simple")
FORMAL_PARAMETER__VARIABLE_ARITY__NAME = hydra.core.Name("variableArity")

@dataclass(frozen=True)
class FormalParameter_Simple:
    modifiers: frozenlist[VariableModifier]
    type: UnannType
    id: VariableDeclaratorId

FORMAL_PARAMETER__SIMPLE__NAME = hydra.core.Name("hydra.ext.java.syntax.FormalParameter_Simple")
FORMAL_PARAMETER__SIMPLE__MODIFIERS__NAME = hydra.core.Name("modifiers")
FORMAL_PARAMETER__SIMPLE__TYPE__NAME = hydra.core.Name("type")
FORMAL_PARAMETER__SIMPLE__ID__NAME = hydra.core.Name("id")

@dataclass(frozen=True)
class VariableArityParameter:
    modifiers: VariableModifier
    type: UnannType
    annotations: frozenlist[Annotation]
    identifier: Identifier

VARIABLE_ARITY_PARAMETER__NAME = hydra.core.Name("hydra.ext.java.syntax.VariableArityParameter")
VARIABLE_ARITY_PARAMETER__MODIFIERS__NAME = hydra.core.Name("modifiers")
VARIABLE_ARITY_PARAMETER__TYPE__NAME = hydra.core.Name("type")
VARIABLE_ARITY_PARAMETER__ANNOTATIONS__NAME = hydra.core.Name("annotations")
VARIABLE_ARITY_PARAMETER__IDENTIFIER__NAME = hydra.core.Name("identifier")

class VariableModifierAnnotation(Node["Annotation"]):
...

class VariableModifierFinal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, VariableModifierFinal)
    def __hash__(self):
        return hash("VariableModifierFinal")

class _VariableModifierMeta(type):
    def __getitem__(cls, item):
        return object

class VariableModifier(metaclass=_VariableModifierMeta):
    r"""VariableModifierAnnotation | VariableModifierFinal"""
    
    pass

VARIABLE_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.VariableModifier")
VARIABLE_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
VARIABLE_MODIFIER__FINAL__NAME = hydra.core.Name("final")

class Throws(Node["frozenlist[ExceptionType]"]):
...

THROWS__NAME = hydra.core.Name("hydra.ext.java.syntax.Throws")

class ExceptionTypeClass(Node["ClassType"]):
...

class ExceptionTypeVariable(Node["TypeVariable"]):
...

class _ExceptionTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ExceptionType(metaclass=_ExceptionTypeMeta):
    r"""ExceptionTypeClass | ExceptionTypeVariable"""
    
    pass

EXCEPTION_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.ExceptionType")
EXCEPTION_TYPE__CLASS__NAME = hydra.core.Name("class")
EXCEPTION_TYPE__VARIABLE__NAME = hydra.core.Name("variable")

class MethodBodyBlock(Node["Block"]):
...

class MethodBodyNone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodBodyNone)
    def __hash__(self):
        return hash("MethodBodyNone")

class _MethodBodyMeta(type):
    def __getitem__(cls, item):
        return object

class MethodBody(metaclass=_MethodBodyMeta):
    r"""MethodBodyBlock | MethodBodyNone"""
    
    pass

METHOD_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodBody")
METHOD_BODY__BLOCK__NAME = hydra.core.Name("block")
METHOD_BODY__NONE__NAME = hydra.core.Name("none")

class InstanceInitializer(Node["Block"]):
...

INSTANCE_INITIALIZER__NAME = hydra.core.Name("hydra.ext.java.syntax.InstanceInitializer")

class StaticInitializer(Node["Block"]):
...

STATIC_INITIALIZER__NAME = hydra.core.Name("hydra.ext.java.syntax.StaticInitializer")

@dataclass(frozen=True)
class ConstructorDeclaration:
    modifiers: frozenlist[ConstructorModifier]
    constructor: ConstructorDeclarator
    throws: Maybe[Throws]
    body: ConstructorBody

CONSTRUCTOR_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclaration")
CONSTRUCTOR_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
CONSTRUCTOR_DECLARATION__CONSTRUCTOR__NAME = hydra.core.Name("constructor")
CONSTRUCTOR_DECLARATION__THROWS__NAME = hydra.core.Name("throws")
CONSTRUCTOR_DECLARATION__BODY__NAME = hydra.core.Name("body")

class ConstructorModifierAnnotation(Node["Annotation"]):
...

class ConstructorModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstructorModifierPublic)
    def __hash__(self):
        return hash("ConstructorModifierPublic")

class ConstructorModifierProtected:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstructorModifierProtected)
    def __hash__(self):
        return hash("ConstructorModifierProtected")

class ConstructorModifierPrivate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstructorModifierPrivate)
    def __hash__(self):
        return hash("ConstructorModifierPrivate")

class _ConstructorModifierMeta(type):
    def __getitem__(cls, item):
        return object

class ConstructorModifier(metaclass=_ConstructorModifierMeta):
    r"""ConstructorModifierAnnotation | ConstructorModifierPublic | ConstructorModifierProtected | ConstructorModifierPrivate"""
    
    pass

CONSTRUCTOR_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstructorModifier")
CONSTRUCTOR_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
CONSTRUCTOR_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
CONSTRUCTOR_MODIFIER__PROTECTED__NAME = hydra.core.Name("protected")
CONSTRUCTOR_MODIFIER__PRIVATE__NAME = hydra.core.Name("private")

@dataclass(frozen=True)
class ConstructorDeclarator:
    parameters: frozenlist[TypeParameter]
    name: SimpleTypeName
    receiver_parameter: Maybe[ReceiverParameter]
    formal_parameters: frozenlist[FormalParameter]

CONSTRUCTOR_DECLARATOR__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclarator")
CONSTRUCTOR_DECLARATOR__PARAMETERS__NAME = hydra.core.Name("parameters")
CONSTRUCTOR_DECLARATOR__NAME__NAME = hydra.core.Name("name")
CONSTRUCTOR_DECLARATOR__RECEIVER_PARAMETER__NAME = hydra.core.Name("receiverParameter")
CONSTRUCTOR_DECLARATOR__FORMAL_PARAMETERS__NAME = hydra.core.Name("formalParameters")

class SimpleTypeName(Node["TypeIdentifier"]):
...

SIMPLE_TYPE_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.SimpleTypeName")

@dataclass(frozen=True)
class ConstructorBody:
    invocation: Maybe[ExplicitConstructorInvocation]
    statements: frozenlist[BlockStatement]

CONSTRUCTOR_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstructorBody")
CONSTRUCTOR_BODY__INVOCATION__NAME = hydra.core.Name("invocation")
CONSTRUCTOR_BODY__STATEMENTS__NAME = hydra.core.Name("statements")

@dataclass(frozen=True)
class ExplicitConstructorInvocation:
    type_arguments: frozenlist[TypeArgument]
    arguments: frozenlist[Expression]
    variant: ExplicitConstructorInvocation_Variant

EXPLICIT_CONSTRUCTOR_INVOCATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ExplicitConstructorInvocation")
EXPLICIT_CONSTRUCTOR_INVOCATION__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
EXPLICIT_CONSTRUCTOR_INVOCATION__ARGUMENTS__NAME = hydra.core.Name("arguments")
EXPLICIT_CONSTRUCTOR_INVOCATION__VARIANT__NAME = hydra.core.Name("variant")

class ExplicitConstructorInvocation_VariantThis:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExplicitConstructorInvocation_VariantThis)
    def __hash__(self):
        return hash("ExplicitConstructorInvocation_VariantThis")

class ExplicitConstructorInvocation_VariantSuper(Node["Maybe[ExpressionName]"]):
...

class ExplicitConstructorInvocation_VariantPrimary(Node["Primary"]):
...

class _ExplicitConstructorInvocation_VariantMeta(type):
    def __getitem__(cls, item):
        return object

class ExplicitConstructorInvocation_Variant(metaclass=_ExplicitConstructorInvocation_VariantMeta):
    r"""ExplicitConstructorInvocation_VariantThis | ExplicitConstructorInvocation_VariantSuper | ExplicitConstructorInvocation_VariantPrimary"""
    
    pass

EXPLICIT_CONSTRUCTOR_INVOCATION__VARIANT__NAME = hydra.core.Name("hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant")
EXPLICIT_CONSTRUCTOR_INVOCATION__VARIANT__THIS__NAME = hydra.core.Name("this")
EXPLICIT_CONSTRUCTOR_INVOCATION__VARIANT__SUPER__NAME = hydra.core.Name("super")
EXPLICIT_CONSTRUCTOR_INVOCATION__VARIANT__PRIMARY__NAME = hydra.core.Name("primary")

@dataclass(frozen=True)
class EnumDeclaration:
    modifiers: frozenlist[ClassModifier]
    identifier: TypeIdentifier
    implements: frozenlist[InterfaceType]
    body: EnumBody

ENUM_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.EnumDeclaration")
ENUM_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
ENUM_DECLARATION__IDENTIFIER__NAME = hydra.core.Name("identifier")
ENUM_DECLARATION__IMPLEMENTS__NAME = hydra.core.Name("implements")
ENUM_DECLARATION__BODY__NAME = hydra.core.Name("body")

class EnumBody(Node["frozenlist[EnumBody_Element]"]):
...

ENUM_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.EnumBody")

@dataclass(frozen=True)
class EnumBody_Element:
    constants: frozenlist[EnumConstant]
    body_declarations: frozenlist[ClassBodyDeclaration]

ENUM_BODY__ELEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.EnumBody_Element")
ENUM_BODY__ELEMENT__CONSTANTS__NAME = hydra.core.Name("constants")
ENUM_BODY__ELEMENT__BODY_DECLARATIONS__NAME = hydra.core.Name("bodyDeclarations")

@dataclass(frozen=True)
class EnumConstant:
    modifiers: frozenlist[EnumConstantModifier]
    identifier: Identifier
    arguments: frozenlist[frozenlist[Expression]]
    body: Maybe[ClassBody]

ENUM_CONSTANT__NAME = hydra.core.Name("hydra.ext.java.syntax.EnumConstant")
ENUM_CONSTANT__MODIFIERS__NAME = hydra.core.Name("modifiers")
ENUM_CONSTANT__IDENTIFIER__NAME = hydra.core.Name("identifier")
ENUM_CONSTANT__ARGUMENTS__NAME = hydra.core.Name("arguments")
ENUM_CONSTANT__BODY__NAME = hydra.core.Name("body")

class EnumConstantModifier(Node["Annotation"]):
...

ENUM_CONSTANT_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.EnumConstantModifier")

class InterfaceDeclarationNormalInterface(Node["NormalInterfaceDeclaration"]):
...

class InterfaceDeclarationAnnotationType(Node["AnnotationTypeDeclaration"]):
...

class _InterfaceDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceDeclaration(metaclass=_InterfaceDeclarationMeta):
    r"""InterfaceDeclarationNormalInterface | InterfaceDeclarationAnnotationType"""
    
    pass

INTERFACE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceDeclaration")
INTERFACE_DECLARATION__NORMAL_INTERFACE__NAME = hydra.core.Name("normalInterface")
INTERFACE_DECLARATION__ANNOTATION_TYPE__NAME = hydra.core.Name("annotationType")

@dataclass(frozen=True)
class NormalInterfaceDeclaration:
    modifiers: frozenlist[InterfaceModifier]
    identifier: TypeIdentifier
    parameters: frozenlist[TypeParameter]
    extends: frozenlist[InterfaceType]
    body: InterfaceBody

NORMAL_INTERFACE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.NormalInterfaceDeclaration")
NORMAL_INTERFACE_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
NORMAL_INTERFACE_DECLARATION__IDENTIFIER__NAME = hydra.core.Name("identifier")
NORMAL_INTERFACE_DECLARATION__PARAMETERS__NAME = hydra.core.Name("parameters")
NORMAL_INTERFACE_DECLARATION__EXTENDS__NAME = hydra.core.Name("extends")
NORMAL_INTERFACE_DECLARATION__BODY__NAME = hydra.core.Name("body")

class InterfaceModifierAnnotation(Node["Annotation"]):
...

class InterfaceModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceModifierPublic)
    def __hash__(self):
        return hash("InterfaceModifierPublic")

class InterfaceModifierProtected:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceModifierProtected)
    def __hash__(self):
        return hash("InterfaceModifierProtected")

class InterfaceModifierPrivate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceModifierPrivate)
    def __hash__(self):
        return hash("InterfaceModifierPrivate")

class InterfaceModifierAbstract:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceModifierAbstract)
    def __hash__(self):
        return hash("InterfaceModifierAbstract")

class InterfaceModifierStatic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceModifierStatic)
    def __hash__(self):
        return hash("InterfaceModifierStatic")

class InterfaceModifierStrictfb:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceModifierStrictfb)
    def __hash__(self):
        return hash("InterfaceModifierStrictfb")

class _InterfaceModifierMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceModifier(metaclass=_InterfaceModifierMeta):
    r"""InterfaceModifierAnnotation | InterfaceModifierPublic | InterfaceModifierProtected | InterfaceModifierPrivate | InterfaceModifierAbstract | InterfaceModifierStatic | InterfaceModifierStrictfb"""
    
    pass

INTERFACE_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceModifier")
INTERFACE_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
INTERFACE_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
INTERFACE_MODIFIER__PROTECTED__NAME = hydra.core.Name("protected")
INTERFACE_MODIFIER__PRIVATE__NAME = hydra.core.Name("private")
INTERFACE_MODIFIER__ABSTRACT__NAME = hydra.core.Name("abstract")
INTERFACE_MODIFIER__STATIC__NAME = hydra.core.Name("static")
INTERFACE_MODIFIER__STRICTFB__NAME = hydra.core.Name("strictfb")

class InterfaceBody(Node["frozenlist[InterfaceMemberDeclaration]"]):
...

INTERFACE_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceBody")

class InterfaceMemberDeclarationConstant(Node["ConstantDeclaration"]):
...

class InterfaceMemberDeclarationInterfaceMethod(Node["InterfaceMethodDeclaration"]):
...

class InterfaceMemberDeclarationClass(Node["ClassDeclaration"]):
...

class InterfaceMemberDeclarationInterface(Node["InterfaceDeclaration"]):
...

class _InterfaceMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceMemberDeclaration(metaclass=_InterfaceMemberDeclarationMeta):
    r"""InterfaceMemberDeclarationConstant | InterfaceMemberDeclarationInterfaceMethod | InterfaceMemberDeclarationClass | InterfaceMemberDeclarationInterface"""
    
    pass

INTERFACE_MEMBER_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceMemberDeclaration")
INTERFACE_MEMBER_DECLARATION__CONSTANT__NAME = hydra.core.Name("constant")
INTERFACE_MEMBER_DECLARATION__INTERFACE_METHOD__NAME = hydra.core.Name("interfaceMethod")
INTERFACE_MEMBER_DECLARATION__CLASS__NAME = hydra.core.Name("class")
INTERFACE_MEMBER_DECLARATION__INTERFACE__NAME = hydra.core.Name("interface")

@dataclass(frozen=True)
class ConstantDeclaration:
    modifiers: frozenlist[ConstantModifier]
    type: UnannType
    variables: frozenlist[VariableDeclarator]

CONSTANT_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstantDeclaration")
CONSTANT_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
CONSTANT_DECLARATION__TYPE__NAME = hydra.core.Name("type")
CONSTANT_DECLARATION__VARIABLES__NAME = hydra.core.Name("variables")

class ConstantModifierAnnotation(Node["Annotation"]):
...

class ConstantModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstantModifierPublic)
    def __hash__(self):
        return hash("ConstantModifierPublic")

class ConstantModifierStatic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstantModifierStatic)
    def __hash__(self):
        return hash("ConstantModifierStatic")

class ConstantModifierFinal:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ConstantModifierFinal)
    def __hash__(self):
        return hash("ConstantModifierFinal")

class _ConstantModifierMeta(type):
    def __getitem__(cls, item):
        return object

class ConstantModifier(metaclass=_ConstantModifierMeta):
    r"""ConstantModifierAnnotation | ConstantModifierPublic | ConstantModifierStatic | ConstantModifierFinal"""
    
    pass

CONSTANT_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstantModifier")
CONSTANT_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
CONSTANT_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
CONSTANT_MODIFIER__STATIC__NAME = hydra.core.Name("static")
CONSTANT_MODIFIER__FINAL__NAME = hydra.core.Name("final")

@dataclass(frozen=True)
class InterfaceMethodDeclaration:
    modifiers: frozenlist[InterfaceMethodModifier]
    header: MethodHeader
    body: MethodBody

INTERFACE_METHOD_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceMethodDeclaration")
INTERFACE_METHOD_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
INTERFACE_METHOD_DECLARATION__HEADER__NAME = hydra.core.Name("header")
INTERFACE_METHOD_DECLARATION__BODY__NAME = hydra.core.Name("body")

class InterfaceMethodModifierAnnotation(Node["Annotation"]):
...

class InterfaceMethodModifierPublic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceMethodModifierPublic)
    def __hash__(self):
        return hash("InterfaceMethodModifierPublic")

class InterfaceMethodModifierPrivate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceMethodModifierPrivate)
    def __hash__(self):
        return hash("InterfaceMethodModifierPrivate")

class InterfaceMethodModifierAbstract:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceMethodModifierAbstract)
    def __hash__(self):
        return hash("InterfaceMethodModifierAbstract")

class InterfaceMethodModifierDefault:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceMethodModifierDefault)
    def __hash__(self):
        return hash("InterfaceMethodModifierDefault")

class InterfaceMethodModifierStatic:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceMethodModifierStatic)
    def __hash__(self):
        return hash("InterfaceMethodModifierStatic")

class InterfaceMethodModifierStrictfp:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, InterfaceMethodModifierStrictfp)
    def __hash__(self):
        return hash("InterfaceMethodModifierStrictfp")

class _InterfaceMethodModifierMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceMethodModifier(metaclass=_InterfaceMethodModifierMeta):
    r"""InterfaceMethodModifierAnnotation | InterfaceMethodModifierPublic | InterfaceMethodModifierPrivate | InterfaceMethodModifierAbstract | InterfaceMethodModifierDefault | InterfaceMethodModifierStatic | InterfaceMethodModifierStrictfp"""
    
    pass

INTERFACE_METHOD_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.InterfaceMethodModifier")
INTERFACE_METHOD_MODIFIER__ANNOTATION__NAME = hydra.core.Name("annotation")
INTERFACE_METHOD_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
INTERFACE_METHOD_MODIFIER__PRIVATE__NAME = hydra.core.Name("private")
INTERFACE_METHOD_MODIFIER__ABSTRACT__NAME = hydra.core.Name("abstract")
INTERFACE_METHOD_MODIFIER__DEFAULT__NAME = hydra.core.Name("default")
INTERFACE_METHOD_MODIFIER__STATIC__NAME = hydra.core.Name("static")
INTERFACE_METHOD_MODIFIER__STRICTFP__NAME = hydra.core.Name("strictfp")

@dataclass(frozen=True)
class AnnotationTypeDeclaration:
    modifiers: frozenlist[InterfaceModifier]
    identifier: TypeIdentifier
    body: AnnotationTypeBody

ANNOTATION_TYPE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeDeclaration")
ANNOTATION_TYPE_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
ANNOTATION_TYPE_DECLARATION__IDENTIFIER__NAME = hydra.core.Name("identifier")
ANNOTATION_TYPE_DECLARATION__BODY__NAME = hydra.core.Name("body")

class AnnotationTypeBody(Node["frozenlist[frozenlist[AnnotationTypeMemberDeclaration]]"]):
...

ANNOTATION_TYPE_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeBody")

class AnnotationTypeMemberDeclarationAnnotationType(Node["AnnotationTypeElementDeclaration"]):
...

class AnnotationTypeMemberDeclarationConstant(Node["ConstantDeclaration"]):
...

class AnnotationTypeMemberDeclarationClass(Node["ClassDeclaration"]):
...

class AnnotationTypeMemberDeclarationInterface(Node["InterfaceDeclaration"]):
...

class _AnnotationTypeMemberDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotationTypeMemberDeclaration(metaclass=_AnnotationTypeMemberDeclarationMeta):
    r"""AnnotationTypeMemberDeclarationAnnotationType | AnnotationTypeMemberDeclarationConstant | AnnotationTypeMemberDeclarationClass | AnnotationTypeMemberDeclarationInterface"""
    
    pass

ANNOTATION_TYPE_MEMBER_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeMemberDeclaration")
ANNOTATION_TYPE_MEMBER_DECLARATION__ANNOTATION_TYPE__NAME = hydra.core.Name("annotationType")
ANNOTATION_TYPE_MEMBER_DECLARATION__CONSTANT__NAME = hydra.core.Name("constant")
ANNOTATION_TYPE_MEMBER_DECLARATION__CLASS__NAME = hydra.core.Name("class")
ANNOTATION_TYPE_MEMBER_DECLARATION__INTERFACE__NAME = hydra.core.Name("interface")

@dataclass(frozen=True)
class AnnotationTypeElementDeclaration:
    modifiers: frozenlist[AnnotationTypeElementModifier]
    type: UnannType
    identifier: Identifier
    dims: Maybe[Dims]
    default: Maybe[DefaultValue]

ANNOTATION_TYPE_ELEMENT_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeElementDeclaration")
ANNOTATION_TYPE_ELEMENT_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
ANNOTATION_TYPE_ELEMENT_DECLARATION__TYPE__NAME = hydra.core.Name("type")
ANNOTATION_TYPE_ELEMENT_DECLARATION__IDENTIFIER__NAME = hydra.core.Name("identifier")
ANNOTATION_TYPE_ELEMENT_DECLARATION__DIMS__NAME = hydra.core.Name("dims")
ANNOTATION_TYPE_ELEMENT_DECLARATION__DEFAULT__NAME = hydra.core.Name("default")

class AnnotationTypeElementModifierPublic(Node["Annotation"]):
...

class AnnotationTypeElementModifierAbstract:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AnnotationTypeElementModifierAbstract)
    def __hash__(self):
        return hash("AnnotationTypeElementModifierAbstract")

class _AnnotationTypeElementModifierMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotationTypeElementModifier(metaclass=_AnnotationTypeElementModifierMeta):
    r"""AnnotationTypeElementModifierPublic | AnnotationTypeElementModifierAbstract"""
    
    pass

ANNOTATION_TYPE_ELEMENT_MODIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeElementModifier")
ANNOTATION_TYPE_ELEMENT_MODIFIER__PUBLIC__NAME = hydra.core.Name("public")
ANNOTATION_TYPE_ELEMENT_MODIFIER__ABSTRACT__NAME = hydra.core.Name("abstract")

class DefaultValue(Node["ElementValue"]):
...

DEFAULT_VALUE__NAME = hydra.core.Name("hydra.ext.java.syntax.DefaultValue")

class AnnotationNormal(Node["NormalAnnotation"]):
...

class AnnotationMarker(Node["MarkerAnnotation"]):
...

class AnnotationSingleElement(Node["SingleElementAnnotation"]):
...

class _AnnotationMeta(type):
    def __getitem__(cls, item):
        return object

class Annotation(metaclass=_AnnotationMeta):
    r"""AnnotationNormal | AnnotationMarker | AnnotationSingleElement"""
    
    pass

ANNOTATION__NAME = hydra.core.Name("hydra.ext.java.syntax.Annotation")
ANNOTATION__NORMAL__NAME = hydra.core.Name("normal")
ANNOTATION__MARKER__NAME = hydra.core.Name("marker")
ANNOTATION__SINGLE_ELEMENT__NAME = hydra.core.Name("singleElement")

@dataclass(frozen=True)
class NormalAnnotation:
    type_name: TypeName
    pairs: frozenlist[ElementValuePair]

NORMAL_ANNOTATION__NAME = hydra.core.Name("hydra.ext.java.syntax.NormalAnnotation")
NORMAL_ANNOTATION__TYPE_NAME__NAME = hydra.core.Name("typeName")
NORMAL_ANNOTATION__PAIRS__NAME = hydra.core.Name("pairs")

@dataclass(frozen=True)
class ElementValuePair:
    key: Identifier
    value: ElementValue

ELEMENT_VALUE_PAIR__NAME = hydra.core.Name("hydra.ext.java.syntax.ElementValuePair")
ELEMENT_VALUE_PAIR__KEY__NAME = hydra.core.Name("key")
ELEMENT_VALUE_PAIR__VALUE__NAME = hydra.core.Name("value")

class ElementValueConditionalExpression(Node["ConditionalExpression"]):
...

class ElementValueElementValueArrayInitializer(Node["ElementValueArrayInitializer"]):
...

class ElementValueAnnotation(Node["Annotation"]):
...

class _ElementValueMeta(type):
    def __getitem__(cls, item):
        return object

class ElementValue(metaclass=_ElementValueMeta):
    r"""ElementValueConditionalExpression | ElementValueElementValueArrayInitializer | ElementValueAnnotation"""
    
    pass

ELEMENT_VALUE__NAME = hydra.core.Name("hydra.ext.java.syntax.ElementValue")
ELEMENT_VALUE__CONDITIONAL_EXPRESSION__NAME = hydra.core.Name("conditionalExpression")
ELEMENT_VALUE__ELEMENT_VALUE_ARRAY_INITIALIZER__NAME = hydra.core.Name("elementValueArrayInitializer")
ELEMENT_VALUE__ANNOTATION__NAME = hydra.core.Name("annotation")

class ElementValueArrayInitializer(Node["frozenlist[ElementValue]"]):
...

ELEMENT_VALUE_ARRAY_INITIALIZER__NAME = hydra.core.Name("hydra.ext.java.syntax.ElementValueArrayInitializer")

class MarkerAnnotation(Node["TypeName"]):
...

MARKER_ANNOTATION__NAME = hydra.core.Name("hydra.ext.java.syntax.MarkerAnnotation")

@dataclass(frozen=True)
class SingleElementAnnotation:
    name: TypeName
    value: Maybe[ElementValue]

SINGLE_ELEMENT_ANNOTATION__NAME = hydra.core.Name("hydra.ext.java.syntax.SingleElementAnnotation")
SINGLE_ELEMENT_ANNOTATION__NAME__NAME = hydra.core.Name("name")
SINGLE_ELEMENT_ANNOTATION__VALUE__NAME = hydra.core.Name("value")

class ArrayInitializer(Node["frozenlist[frozenlist[VariableInitializer]]"]):
...

ARRAY_INITIALIZER__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayInitializer")

class Block(Node["frozenlist[BlockStatement]"]):
...

BLOCK__NAME = hydra.core.Name("hydra.ext.java.syntax.Block")

class BlockStatementLocalVariableDeclaration(Node["LocalVariableDeclarationStatement"]):
...

class BlockStatementClass(Node["ClassDeclaration"]):
...

class BlockStatementStatement(Node["Statement"]):
...

class _BlockStatementMeta(type):
    def __getitem__(cls, item):
        return object

class BlockStatement(metaclass=_BlockStatementMeta):
    r"""BlockStatementLocalVariableDeclaration | BlockStatementClass | BlockStatementStatement"""
    
    pass

BLOCK_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.BlockStatement")
BLOCK_STATEMENT__LOCAL_VARIABLE_DECLARATION__NAME = hydra.core.Name("localVariableDeclaration")
BLOCK_STATEMENT__CLASS__NAME = hydra.core.Name("class")
BLOCK_STATEMENT__STATEMENT__NAME = hydra.core.Name("statement")

class LocalVariableDeclarationStatement(Node["LocalVariableDeclaration"]):
...

LOCAL_VARIABLE_DECLARATION_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.LocalVariableDeclarationStatement")

@dataclass(frozen=True)
class LocalVariableDeclaration:
    modifiers: frozenlist[VariableModifier]
    type: LocalVariableType
    declarators: frozenlist[VariableDeclarator]

LOCAL_VARIABLE_DECLARATION__NAME = hydra.core.Name("hydra.ext.java.syntax.LocalVariableDeclaration")
LOCAL_VARIABLE_DECLARATION__MODIFIERS__NAME = hydra.core.Name("modifiers")
LOCAL_VARIABLE_DECLARATION__TYPE__NAME = hydra.core.Name("type")
LOCAL_VARIABLE_DECLARATION__DECLARATORS__NAME = hydra.core.Name("declarators")

class LocalVariableTypeType(Node["UnannType"]):
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
    
    pass

LOCAL_VARIABLE_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.LocalVariableType")
LOCAL_VARIABLE_TYPE__TYPE__NAME = hydra.core.Name("type")
LOCAL_VARIABLE_TYPE__VAR__NAME = hydra.core.Name("var")

class StatementWithoutTrailing(Node["StatementWithoutTrailingSubstatement"]):
...

class StatementLabeled(Node["LabeledStatement"]):
...

class StatementIfThen(Node["IfThenStatement"]):
...

class StatementIfThenElse(Node["IfThenElseStatement"]):
...

class StatementWhile(Node["WhileStatement"]):
...

class StatementFor(Node["ForStatement"]):
...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementWithoutTrailing | StatementLabeled | StatementIfThen | StatementIfThenElse | StatementWhile | StatementFor"""
    
    pass

STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.Statement")
STATEMENT__WITHOUT_TRAILING__NAME = hydra.core.Name("withoutTrailing")
STATEMENT__LABELED__NAME = hydra.core.Name("labeled")
STATEMENT__IF_THEN__NAME = hydra.core.Name("ifThen")
STATEMENT__IF_THEN_ELSE__NAME = hydra.core.Name("ifThenElse")
STATEMENT__WHILE__NAME = hydra.core.Name("while")
STATEMENT__FOR__NAME = hydra.core.Name("for")

class StatementNoShortIfWithoutTrailing(Node["StatementWithoutTrailingSubstatement"]):
...

class StatementNoShortIfLabeled(Node["LabeledStatementNoShortIf"]):
...

class StatementNoShortIfIfThenElse(Node["IfThenElseStatementNoShortIf"]):
...

class StatementNoShortIfWhile(Node["WhileStatementNoShortIf"]):
...

class StatementNoShortIfFor(Node["ForStatementNoShortIf"]):
...

class _StatementNoShortIfMeta(type):
    def __getitem__(cls, item):
        return object

class StatementNoShortIf(metaclass=_StatementNoShortIfMeta):
    r"""StatementNoShortIfWithoutTrailing | StatementNoShortIfLabeled | StatementNoShortIfIfThenElse | StatementNoShortIfWhile | StatementNoShortIfFor"""
    
    pass

STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.StatementNoShortIf")
STATEMENT_NO_SHORT_IF__WITHOUT_TRAILING__NAME = hydra.core.Name("withoutTrailing")
STATEMENT_NO_SHORT_IF__LABELED__NAME = hydra.core.Name("labeled")
STATEMENT_NO_SHORT_IF__IF_THEN_ELSE__NAME = hydra.core.Name("ifThenElse")
STATEMENT_NO_SHORT_IF__WHILE__NAME = hydra.core.Name("while")
STATEMENT_NO_SHORT_IF__FOR__NAME = hydra.core.Name("for")

class StatementWithoutTrailingSubstatementBlock(Node["Block"]):
...

class StatementWithoutTrailingSubstatementEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StatementWithoutTrailingSubstatementEmpty)
    def __hash__(self):
        return hash("StatementWithoutTrailingSubstatementEmpty")

class StatementWithoutTrailingSubstatementExpression(Node["ExpressionStatement"]):
...

class StatementWithoutTrailingSubstatementAssert(Node["AssertStatement"]):
...

class StatementWithoutTrailingSubstatementSwitch(Node["SwitchStatement"]):
...

class StatementWithoutTrailingSubstatementDo(Node["DoStatement"]):
...

class StatementWithoutTrailingSubstatementBreak(Node["BreakStatement"]):
...

class StatementWithoutTrailingSubstatementContinue(Node["ContinueStatement"]):
...

class StatementWithoutTrailingSubstatementReturn(Node["ReturnStatement"]):
...

class StatementWithoutTrailingSubstatementSynchronized(Node["SynchronizedStatement"]):
...

class StatementWithoutTrailingSubstatementThrow(Node["ThrowStatement"]):
...

class StatementWithoutTrailingSubstatementTry(Node["TryStatement"]):
...

class _StatementWithoutTrailingSubstatementMeta(type):
    def __getitem__(cls, item):
        return object

class StatementWithoutTrailingSubstatement(metaclass=_StatementWithoutTrailingSubstatementMeta):
    r"""StatementWithoutTrailingSubstatementBlock | StatementWithoutTrailingSubstatementEmpty | StatementWithoutTrailingSubstatementExpression | StatementWithoutTrailingSubstatementAssert | StatementWithoutTrailingSubstatementSwitch | StatementWithoutTrailingSubstatementDo | StatementWithoutTrailingSubstatementBreak | StatementWithoutTrailingSubstatementContinue | StatementWithoutTrailingSubstatementReturn | StatementWithoutTrailingSubstatementSynchronized | StatementWithoutTrailingSubstatementThrow | StatementWithoutTrailingSubstatementTry"""
    
    pass

STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.StatementWithoutTrailingSubstatement")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__BLOCK__NAME = hydra.core.Name("block")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__EMPTY__NAME = hydra.core.Name("empty")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__EXPRESSION__NAME = hydra.core.Name("expression")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__ASSERT__NAME = hydra.core.Name("assert")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__SWITCH__NAME = hydra.core.Name("switch")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__DO__NAME = hydra.core.Name("do")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__BREAK__NAME = hydra.core.Name("break")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__CONTINUE__NAME = hydra.core.Name("continue")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__RETURN__NAME = hydra.core.Name("return")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__SYNCHRONIZED__NAME = hydra.core.Name("synchronized")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__THROW__NAME = hydra.core.Name("throw")
STATEMENT_WITHOUT_TRAILING_SUBSTATEMENT__TRY__NAME = hydra.core.Name("try")

@dataclass(frozen=True)
class LabeledStatement:
    identifier: Identifier
    statement: Statement

LABELED_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.LabeledStatement")
LABELED_STATEMENT__IDENTIFIER__NAME = hydra.core.Name("identifier")
LABELED_STATEMENT__STATEMENT__NAME = hydra.core.Name("statement")

@dataclass(frozen=True)
class LabeledStatementNoShortIf:
    identifier: Identifier
    statement: StatementNoShortIf

LABELED_STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.LabeledStatementNoShortIf")
LABELED_STATEMENT_NO_SHORT_IF__IDENTIFIER__NAME = hydra.core.Name("identifier")
LABELED_STATEMENT_NO_SHORT_IF__STATEMENT__NAME = hydra.core.Name("statement")

class ExpressionStatement(Node["StatementExpression"]):
...

EXPRESSION_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.ExpressionStatement")

class StatementExpressionAssignment(Node["Assignment"]):
...

class StatementExpressionPreIncrement(Node["PreIncrementExpression"]):
...

class StatementExpressionPreDecrement(Node["PreDecrementExpression"]):
...

class StatementExpressionPostIncrement(Node["PostIncrementExpression"]):
...

class StatementExpressionPostDecrement(Node["PostDecrementExpression"]):
...

class StatementExpressionMethodInvocation(Node["MethodInvocation"]):
...

class StatementExpressionClassInstanceCreation(Node["ClassInstanceCreationExpression"]):
...

class _StatementExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class StatementExpression(metaclass=_StatementExpressionMeta):
    r"""StatementExpressionAssignment | StatementExpressionPreIncrement | StatementExpressionPreDecrement | StatementExpressionPostIncrement | StatementExpressionPostDecrement | StatementExpressionMethodInvocation | StatementExpressionClassInstanceCreation"""
    
    pass

STATEMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.StatementExpression")
STATEMENT_EXPRESSION__ASSIGNMENT__NAME = hydra.core.Name("assignment")
STATEMENT_EXPRESSION__PRE_INCREMENT__NAME = hydra.core.Name("preIncrement")
STATEMENT_EXPRESSION__PRE_DECREMENT__NAME = hydra.core.Name("preDecrement")
STATEMENT_EXPRESSION__POST_INCREMENT__NAME = hydra.core.Name("postIncrement")
STATEMENT_EXPRESSION__POST_DECREMENT__NAME = hydra.core.Name("postDecrement")
STATEMENT_EXPRESSION__METHOD_INVOCATION__NAME = hydra.core.Name("methodInvocation")
STATEMENT_EXPRESSION__CLASS_INSTANCE_CREATION__NAME = hydra.core.Name("classInstanceCreation")

@dataclass(frozen=True)
class IfThenStatement:
    expression: Expression
    statement: Statement

IF_THEN_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.IfThenStatement")
IF_THEN_STATEMENT__EXPRESSION__NAME = hydra.core.Name("expression")
IF_THEN_STATEMENT__STATEMENT__NAME = hydra.core.Name("statement")

@dataclass(frozen=True)
class IfThenElseStatement:
    cond: Maybe[Expression]
    then: StatementNoShortIf
    else_: Statement

IF_THEN_ELSE_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatement")
IF_THEN_ELSE_STATEMENT__COND__NAME = hydra.core.Name("cond")
IF_THEN_ELSE_STATEMENT__THEN__NAME = hydra.core.Name("then")
IF_THEN_ELSE_STATEMENT__ELSE__NAME = hydra.core.Name("else")

@dataclass(frozen=True)
class IfThenElseStatementNoShortIf:
    cond: Maybe[Expression]
    then: StatementNoShortIf
    else_: StatementNoShortIf

IF_THEN_ELSE_STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatementNoShortIf")
IF_THEN_ELSE_STATEMENT_NO_SHORT_IF__COND__NAME = hydra.core.Name("cond")
IF_THEN_ELSE_STATEMENT_NO_SHORT_IF__THEN__NAME = hydra.core.Name("then")
IF_THEN_ELSE_STATEMENT_NO_SHORT_IF__ELSE__NAME = hydra.core.Name("else")

class AssertStatementSingle(Node["Expression"]):
...

class AssertStatementPair(Node["AssertStatement_Pair"]):
...

class _AssertStatementMeta(type):
    def __getitem__(cls, item):
        return object

class AssertStatement(metaclass=_AssertStatementMeta):
    r"""AssertStatementSingle | AssertStatementPair"""
    
    pass

ASSERT_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.AssertStatement")
ASSERT_STATEMENT__SINGLE__NAME = hydra.core.Name("single")
ASSERT_STATEMENT__PAIR__NAME = hydra.core.Name("pair")

@dataclass(frozen=True)
class AssertStatement_Pair:
    first: Expression
    second: Expression

ASSERT_STATEMENT__PAIR__NAME = hydra.core.Name("hydra.ext.java.syntax.AssertStatement_Pair")
ASSERT_STATEMENT__PAIR__FIRST__NAME = hydra.core.Name("first")
ASSERT_STATEMENT__PAIR__SECOND__NAME = hydra.core.Name("second")

@dataclass(frozen=True)
class SwitchStatement:
    cond: Expression
    block: SwitchBlock

SWITCH_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.SwitchStatement")
SWITCH_STATEMENT__COND__NAME = hydra.core.Name("cond")
SWITCH_STATEMENT__BLOCK__NAME = hydra.core.Name("block")

class SwitchBlock(Node["frozenlist[SwitchBlock_Pair]"]):
...

SWITCH_BLOCK__NAME = hydra.core.Name("hydra.ext.java.syntax.SwitchBlock")

@dataclass(frozen=True)
class SwitchBlock_Pair:
    statements: frozenlist[SwitchBlockStatementGroup]
    labels: frozenlist[SwitchLabel]

SWITCH_BLOCK__PAIR__NAME = hydra.core.Name("hydra.ext.java.syntax.SwitchBlock_Pair")
SWITCH_BLOCK__PAIR__STATEMENTS__NAME = hydra.core.Name("statements")
SWITCH_BLOCK__PAIR__LABELS__NAME = hydra.core.Name("labels")

@dataclass(frozen=True)
class SwitchBlockStatementGroup:
    labels: frozenlist[SwitchLabel]
    statements: frozenlist[BlockStatement]

SWITCH_BLOCK_STATEMENT_GROUP__NAME = hydra.core.Name("hydra.ext.java.syntax.SwitchBlockStatementGroup")
SWITCH_BLOCK_STATEMENT_GROUP__LABELS__NAME = hydra.core.Name("labels")
SWITCH_BLOCK_STATEMENT_GROUP__STATEMENTS__NAME = hydra.core.Name("statements")

class SwitchLabelConstant(Node["ConstantExpression"]):
...

class SwitchLabelEnumConstant(Node["EnumConstantName"]):
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
    r"""SwitchLabelConstant | SwitchLabelEnumConstant | SwitchLabelDefault"""
    
    pass

SWITCH_LABEL__NAME = hydra.core.Name("hydra.ext.java.syntax.SwitchLabel")
SWITCH_LABEL__CONSTANT__NAME = hydra.core.Name("constant")
SWITCH_LABEL__ENUM_CONSTANT__NAME = hydra.core.Name("enumConstant")
SWITCH_LABEL__DEFAULT__NAME = hydra.core.Name("default")

class EnumConstantName(Node["Identifier"]):
...

ENUM_CONSTANT_NAME__NAME = hydra.core.Name("hydra.ext.java.syntax.EnumConstantName")

@dataclass(frozen=True)
class WhileStatement:
    cond: Maybe[Expression]
    body: Statement

WHILE_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.WhileStatement")
WHILE_STATEMENT__COND__NAME = hydra.core.Name("cond")
WHILE_STATEMENT__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class WhileStatementNoShortIf:
    cond: Maybe[Expression]
    body: StatementNoShortIf

WHILE_STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.WhileStatementNoShortIf")
WHILE_STATEMENT_NO_SHORT_IF__COND__NAME = hydra.core.Name("cond")
WHILE_STATEMENT_NO_SHORT_IF__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class DoStatement:
    body: Statement
    conde: Maybe[Expression]

DO_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.DoStatement")
DO_STATEMENT__BODY__NAME = hydra.core.Name("body")
DO_STATEMENT__CONDE__NAME = hydra.core.Name("conde")

class ForStatementBasic(Node["BasicForStatement"]):
...

class ForStatementEnhanced(Node["EnhancedForStatement"]):
...

class _ForStatementMeta(type):
    def __getitem__(cls, item):
        return object

class ForStatement(metaclass=_ForStatementMeta):
    r"""ForStatementBasic | ForStatementEnhanced"""
    
    pass

FOR_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.ForStatement")
FOR_STATEMENT__BASIC__NAME = hydra.core.Name("basic")
FOR_STATEMENT__ENHANCED__NAME = hydra.core.Name("enhanced")

class ForStatementNoShortIfBasic(Node["BasicForStatementNoShortIf"]):
...

class ForStatementNoShortIfEnhanced(Node["EnhancedForStatementNoShortIf"]):
...

class _ForStatementNoShortIfMeta(type):
    def __getitem__(cls, item):
        return object

class ForStatementNoShortIf(metaclass=_ForStatementNoShortIfMeta):
    r"""ForStatementNoShortIfBasic | ForStatementNoShortIfEnhanced"""
    
    pass

FOR_STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.ForStatementNoShortIf")
FOR_STATEMENT_NO_SHORT_IF__BASIC__NAME = hydra.core.Name("basic")
FOR_STATEMENT_NO_SHORT_IF__ENHANCED__NAME = hydra.core.Name("enhanced")

@dataclass(frozen=True)
class BasicForStatement:
    cond: ForCond
    body: Statement

BASIC_FOR_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.BasicForStatement")
BASIC_FOR_STATEMENT__COND__NAME = hydra.core.Name("cond")
BASIC_FOR_STATEMENT__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class ForCond:
    init: Maybe[ForInit]
    cond: Maybe[Expression]
    update: Maybe[ForUpdate]

FOR_COND__NAME = hydra.core.Name("hydra.ext.java.syntax.ForCond")
FOR_COND__INIT__NAME = hydra.core.Name("init")
FOR_COND__COND__NAME = hydra.core.Name("cond")
FOR_COND__UPDATE__NAME = hydra.core.Name("update")

@dataclass(frozen=True)
class BasicForStatementNoShortIf:
    cond: ForCond
    body: StatementNoShortIf

BASIC_FOR_STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.BasicForStatementNoShortIf")
BASIC_FOR_STATEMENT_NO_SHORT_IF__COND__NAME = hydra.core.Name("cond")
BASIC_FOR_STATEMENT_NO_SHORT_IF__BODY__NAME = hydra.core.Name("body")

class ForInitStatements(Node["frozenlist[StatementExpression]"]):
...

class ForInitLocalVariable(Node["LocalVariableDeclaration"]):
...

class _ForInitMeta(type):
    def __getitem__(cls, item):
        return object

class ForInit(metaclass=_ForInitMeta):
    r"""ForInitStatements | ForInitLocalVariable"""
    
    pass

FOR_INIT__NAME = hydra.core.Name("hydra.ext.java.syntax.ForInit")
FOR_INIT__STATEMENTS__NAME = hydra.core.Name("statements")
FOR_INIT__LOCAL_VARIABLE__NAME = hydra.core.Name("localVariable")

class ForUpdate(Node["frozenlist[StatementExpression]"]):
...

FOR_UPDATE__NAME = hydra.core.Name("hydra.ext.java.syntax.ForUpdate")

@dataclass(frozen=True)
class EnhancedForStatement:
    cond: EnhancedForCond
    body: Statement

ENHANCED_FOR_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.EnhancedForStatement")
ENHANCED_FOR_STATEMENT__COND__NAME = hydra.core.Name("cond")
ENHANCED_FOR_STATEMENT__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class EnhancedForCond:
    modifiers: frozenlist[VariableModifier]
    type: LocalVariableType
    id: VariableDeclaratorId
    expression: Expression

ENHANCED_FOR_COND__NAME = hydra.core.Name("hydra.ext.java.syntax.EnhancedForCond")
ENHANCED_FOR_COND__MODIFIERS__NAME = hydra.core.Name("modifiers")
ENHANCED_FOR_COND__TYPE__NAME = hydra.core.Name("type")
ENHANCED_FOR_COND__ID__NAME = hydra.core.Name("id")
ENHANCED_FOR_COND__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class EnhancedForStatementNoShortIf:
    cond: EnhancedForCond
    body: StatementNoShortIf

ENHANCED_FOR_STATEMENT_NO_SHORT_IF__NAME = hydra.core.Name("hydra.ext.java.syntax.EnhancedForStatementNoShortIf")
ENHANCED_FOR_STATEMENT_NO_SHORT_IF__COND__NAME = hydra.core.Name("cond")
ENHANCED_FOR_STATEMENT_NO_SHORT_IF__BODY__NAME = hydra.core.Name("body")

class BreakStatement(Node["Maybe[Identifier]"]):
...

BREAK_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.BreakStatement")

class ContinueStatement(Node["Maybe[Identifier]"]):
...

CONTINUE_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.ContinueStatement")

class ReturnStatement(Node["Maybe[Expression]"]):
...

RETURN_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.ReturnStatement")

class ThrowStatement(Node["Expression"]):
...

THROW_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.ThrowStatement")

@dataclass(frozen=True)
class SynchronizedStatement:
    expression: Expression
    block: Block

SYNCHRONIZED_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.SynchronizedStatement")
SYNCHRONIZED_STATEMENT__EXPRESSION__NAME = hydra.core.Name("expression")
SYNCHRONIZED_STATEMENT__BLOCK__NAME = hydra.core.Name("block")

class TryStatementSimple(Node["TryStatement_Simple"]):
...

class TryStatementWithFinally(Node["TryStatement_WithFinally"]):
...

class TryStatementWithResources(Node["TryWithResourcesStatement"]):
...

class _TryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class TryStatement(metaclass=_TryStatementMeta):
    r"""TryStatementSimple | TryStatementWithFinally | TryStatementWithResources"""
    
    pass

TRY_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.TryStatement")
TRY_STATEMENT__SIMPLE__NAME = hydra.core.Name("simple")
TRY_STATEMENT__WITH_FINALLY__NAME = hydra.core.Name("withFinally")
TRY_STATEMENT__WITH_RESOURCES__NAME = hydra.core.Name("withResources")

@dataclass(frozen=True)
class TryStatement_Simple:
    block: Block
    catches: Catches

TRY_STATEMENT__SIMPLE__NAME = hydra.core.Name("hydra.ext.java.syntax.TryStatement_Simple")
TRY_STATEMENT__SIMPLE__BLOCK__NAME = hydra.core.Name("block")
TRY_STATEMENT__SIMPLE__CATCHES__NAME = hydra.core.Name("catches")

@dataclass(frozen=True)
class TryStatement_WithFinally:
    block: Block
    catches: Maybe[Catches]
    finally_: Finally

TRY_STATEMENT__WITH_FINALLY__NAME = hydra.core.Name("hydra.ext.java.syntax.TryStatement_WithFinally")
TRY_STATEMENT__WITH_FINALLY__BLOCK__NAME = hydra.core.Name("block")
TRY_STATEMENT__WITH_FINALLY__CATCHES__NAME = hydra.core.Name("catches")
TRY_STATEMENT__WITH_FINALLY__FINALLY__NAME = hydra.core.Name("finally")

class Catches(Node["frozenlist[CatchClause]"]):
...

CATCHES__NAME = hydra.core.Name("hydra.ext.java.syntax.Catches")

@dataclass(frozen=True)
class CatchClause:
    parameter: Maybe[CatchFormalParameter]
    block: Block

CATCH_CLAUSE__NAME = hydra.core.Name("hydra.ext.java.syntax.CatchClause")
CATCH_CLAUSE__PARAMETER__NAME = hydra.core.Name("parameter")
CATCH_CLAUSE__BLOCK__NAME = hydra.core.Name("block")

@dataclass(frozen=True)
class CatchFormalParameter:
    modifiers: frozenlist[VariableModifier]
    type: CatchType
    id: VariableDeclaratorId

CATCH_FORMAL_PARAMETER__NAME = hydra.core.Name("hydra.ext.java.syntax.CatchFormalParameter")
CATCH_FORMAL_PARAMETER__MODIFIERS__NAME = hydra.core.Name("modifiers")
CATCH_FORMAL_PARAMETER__TYPE__NAME = hydra.core.Name("type")
CATCH_FORMAL_PARAMETER__ID__NAME = hydra.core.Name("id")

@dataclass(frozen=True)
class CatchType:
    type: UnannClassType
    types: frozenlist[ClassType]

CATCH_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.CatchType")
CATCH_TYPE__TYPE__NAME = hydra.core.Name("type")
CATCH_TYPE__TYPES__NAME = hydra.core.Name("types")

class Finally(Node["Block"]):
...

FINALLY__NAME = hydra.core.Name("hydra.ext.java.syntax.Finally")

@dataclass(frozen=True)
class TryWithResourcesStatement:
    resource_specification: ResourceSpecification
    block: Block
    catches: Maybe[Catches]
    finally_: Maybe[Finally]

TRY_WITH_RESOURCES_STATEMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.TryWithResourcesStatement")
TRY_WITH_RESOURCES_STATEMENT__RESOURCE_SPECIFICATION__NAME = hydra.core.Name("resourceSpecification")
TRY_WITH_RESOURCES_STATEMENT__BLOCK__NAME = hydra.core.Name("block")
TRY_WITH_RESOURCES_STATEMENT__CATCHES__NAME = hydra.core.Name("catches")
TRY_WITH_RESOURCES_STATEMENT__FINALLY__NAME = hydra.core.Name("finally")

class ResourceSpecification(Node["frozenlist[Resource]"]):
...

RESOURCE_SPECIFICATION__NAME = hydra.core.Name("hydra.ext.java.syntax.ResourceSpecification")

class ResourceLocal(Node["Resource_Local"]):
...

class ResourceVariable(Node["VariableAccess"]):
...

class _ResourceMeta(type):
    def __getitem__(cls, item):
        return object

class Resource(metaclass=_ResourceMeta):
    r"""ResourceLocal | ResourceVariable"""
    
    pass

RESOURCE__NAME = hydra.core.Name("hydra.ext.java.syntax.Resource")
RESOURCE__LOCAL__NAME = hydra.core.Name("local")
RESOURCE__VARIABLE__NAME = hydra.core.Name("variable")

@dataclass(frozen=True)
class Resource_Local:
    modifiers: frozenlist[VariableModifier]
    type: LocalVariableType
    identifier: Identifier
    expression: Expression

RESOURCE__LOCAL__NAME = hydra.core.Name("hydra.ext.java.syntax.Resource_Local")
RESOURCE__LOCAL__MODIFIERS__NAME = hydra.core.Name("modifiers")
RESOURCE__LOCAL__TYPE__NAME = hydra.core.Name("type")
RESOURCE__LOCAL__IDENTIFIER__NAME = hydra.core.Name("identifier")
RESOURCE__LOCAL__EXPRESSION__NAME = hydra.core.Name("expression")

class VariableAccessExpressionName(Node["ExpressionName"]):
...

class VariableAccessFieldAccess(Node["FieldAccess"]):
...

class _VariableAccessMeta(type):
    def __getitem__(cls, item):
        return object

class VariableAccess(metaclass=_VariableAccessMeta):
    r"""VariableAccessExpressionName | VariableAccessFieldAccess"""
    
    pass

VARIABLE_ACCESS__NAME = hydra.core.Name("hydra.ext.java.syntax.VariableAccess")
VARIABLE_ACCESS__EXPRESSION_NAME__NAME = hydra.core.Name("expressionName")
VARIABLE_ACCESS__FIELD_ACCESS__NAME = hydra.core.Name("fieldAccess")

class PrimaryNoNewArray(Node["PrimaryNoNewArray"]):
...

class PrimaryArrayCreation(Node["ArrayCreationExpression"]):
...

class _PrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class Primary(metaclass=_PrimaryMeta):
    r"""PrimaryNoNewArray | PrimaryArrayCreation"""
    
    pass

PRIMARY__NAME = hydra.core.Name("hydra.ext.java.syntax.Primary")
PRIMARY__NO_NEW_ARRAY__NAME = hydra.core.Name("noNewArray")
PRIMARY__ARRAY_CREATION__NAME = hydra.core.Name("arrayCreation")

class PrimaryNoNewArrayLiteral(Node["Literal"]):
...

class PrimaryNoNewArrayClassLiteral(Node["ClassLiteral"]):
...

class PrimaryNoNewArrayThis:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimaryNoNewArrayThis)
    def __hash__(self):
        return hash("PrimaryNoNewArrayThis")

class PrimaryNoNewArrayDotThis(Node["TypeName"]):
...

class PrimaryNoNewArrayParens(Node["Expression"]):
...

class PrimaryNoNewArrayClassInstance(Node["ClassInstanceCreationExpression"]):
...

class PrimaryNoNewArrayFieldAccess(Node["FieldAccess"]):
...

class PrimaryNoNewArrayArrayAccess(Node["ArrayAccess"]):
...

class PrimaryNoNewArrayMethodInvocation(Node["MethodInvocation"]):
...

class PrimaryNoNewArrayMethodReference(Node["MethodReference"]):
...

class _PrimaryNoNewArrayMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryNoNewArray(metaclass=_PrimaryNoNewArrayMeta):
    r"""PrimaryNoNewArrayLiteral | PrimaryNoNewArrayClassLiteral | PrimaryNoNewArrayThis | PrimaryNoNewArrayDotThis | PrimaryNoNewArrayParens | PrimaryNoNewArrayClassInstance | PrimaryNoNewArrayFieldAccess | PrimaryNoNewArrayArrayAccess | PrimaryNoNewArrayMethodInvocation | PrimaryNoNewArrayMethodReference"""
    
    pass

PRIMARY_NO_NEW_ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.PrimaryNoNewArray")
PRIMARY_NO_NEW_ARRAY__LITERAL__NAME = hydra.core.Name("literal")
PRIMARY_NO_NEW_ARRAY__CLASS_LITERAL__NAME = hydra.core.Name("classLiteral")
PRIMARY_NO_NEW_ARRAY__THIS__NAME = hydra.core.Name("this")
PRIMARY_NO_NEW_ARRAY__DOT_THIS__NAME = hydra.core.Name("dotThis")
PRIMARY_NO_NEW_ARRAY__PARENS__NAME = hydra.core.Name("parens")
PRIMARY_NO_NEW_ARRAY__CLASS_INSTANCE__NAME = hydra.core.Name("classInstance")
PRIMARY_NO_NEW_ARRAY__FIELD_ACCESS__NAME = hydra.core.Name("fieldAccess")
PRIMARY_NO_NEW_ARRAY__ARRAY_ACCESS__NAME = hydra.core.Name("arrayAccess")
PRIMARY_NO_NEW_ARRAY__METHOD_INVOCATION__NAME = hydra.core.Name("methodInvocation")
PRIMARY_NO_NEW_ARRAY__METHOD_REFERENCE__NAME = hydra.core.Name("methodReference")

class ClassLiteralType(Node["TypeNameArray"]):
...

class ClassLiteralNumericType(Node["NumericTypeArray"]):
...

class ClassLiteralBoolean(Node["BooleanArray"]):
...

class ClassLiteralVoid:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClassLiteralVoid)
    def __hash__(self):
        return hash("ClassLiteralVoid")

class _ClassLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class ClassLiteral(metaclass=_ClassLiteralMeta):
    r"""ClassLiteralType | ClassLiteralNumericType | ClassLiteralBoolean | ClassLiteralVoid"""
    
    pass

CLASS_LITERAL__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassLiteral")
CLASS_LITERAL__TYPE__NAME = hydra.core.Name("type")
CLASS_LITERAL__NUMERIC_TYPE__NAME = hydra.core.Name("numericType")
CLASS_LITERAL__BOOLEAN__NAME = hydra.core.Name("boolean")
CLASS_LITERAL__VOID__NAME = hydra.core.Name("void")

class TypeNameArraySimple(Node["TypeName"]):
...

class TypeNameArrayArray(Node["TypeNameArray"]):
...

class _TypeNameArrayMeta(type):
    def __getitem__(cls, item):
        return object

class TypeNameArray(metaclass=_TypeNameArrayMeta):
    r"""TypeNameArraySimple | TypeNameArrayArray"""
    
    pass

TYPE_NAME_ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeNameArray")
TYPE_NAME_ARRAY__SIMPLE__NAME = hydra.core.Name("simple")
TYPE_NAME_ARRAY__ARRAY__NAME = hydra.core.Name("array")

class NumericTypeArraySimple(Node["NumericType"]):
...

class NumericTypeArrayArray(Node["NumericTypeArray"]):
...

class _NumericTypeArrayMeta(type):
    def __getitem__(cls, item):
        return object

class NumericTypeArray(metaclass=_NumericTypeArrayMeta):
    r"""NumericTypeArraySimple | NumericTypeArrayArray"""
    
    pass

NUMERIC_TYPE_ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.NumericTypeArray")
NUMERIC_TYPE_ARRAY__SIMPLE__NAME = hydra.core.Name("simple")
NUMERIC_TYPE_ARRAY__ARRAY__NAME = hydra.core.Name("array")

class BooleanArraySimple:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BooleanArraySimple)
    def __hash__(self):
        return hash("BooleanArraySimple")

class BooleanArrayArray(Node["BooleanArray"]):
...

class _BooleanArrayMeta(type):
    def __getitem__(cls, item):
        return object

class BooleanArray(metaclass=_BooleanArrayMeta):
    r"""BooleanArraySimple | BooleanArrayArray"""
    
    pass

BOOLEAN_ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.BooleanArray")
BOOLEAN_ARRAY__SIMPLE__NAME = hydra.core.Name("simple")
BOOLEAN_ARRAY__ARRAY__NAME = hydra.core.Name("array")

@dataclass(frozen=True)
class ClassInstanceCreationExpression:
    qualifier: Maybe[ClassInstanceCreationExpression_Qualifier]
    expression: UnqualifiedClassInstanceCreationExpression

CLASS_INSTANCE_CREATION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassInstanceCreationExpression")
CLASS_INSTANCE_CREATION_EXPRESSION__QUALIFIER__NAME = hydra.core.Name("qualifier")
CLASS_INSTANCE_CREATION_EXPRESSION__EXPRESSION__NAME = hydra.core.Name("expression")

class ClassInstanceCreationExpression_QualifierExpression(Node["ExpressionName"]):
...

class ClassInstanceCreationExpression_QualifierPrimary(Node["Primary"]):
...

class _ClassInstanceCreationExpression_QualifierMeta(type):
    def __getitem__(cls, item):
        return object

class ClassInstanceCreationExpression_Qualifier(metaclass=_ClassInstanceCreationExpression_QualifierMeta):
    r"""ClassInstanceCreationExpression_QualifierExpression | ClassInstanceCreationExpression_QualifierPrimary"""
    
    pass

CLASS_INSTANCE_CREATION_EXPRESSION__QUALIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier")
CLASS_INSTANCE_CREATION_EXPRESSION__QUALIFIER__EXPRESSION__NAME = hydra.core.Name("expression")
CLASS_INSTANCE_CREATION_EXPRESSION__QUALIFIER__PRIMARY__NAME = hydra.core.Name("primary")

@dataclass(frozen=True)
class UnqualifiedClassInstanceCreationExpression:
    type_arguments: frozenlist[TypeArgument]
    class_or_interface: ClassOrInterfaceTypeToInstantiate
    arguments: frozenlist[Expression]
    body: Maybe[ClassBody]

UNQUALIFIED_CLASS_INSTANCE_CREATION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression")
UNQUALIFIED_CLASS_INSTANCE_CREATION_EXPRESSION__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
UNQUALIFIED_CLASS_INSTANCE_CREATION_EXPRESSION__CLASS_OR_INTERFACE__NAME = hydra.core.Name("classOrInterface")
UNQUALIFIED_CLASS_INSTANCE_CREATION_EXPRESSION__ARGUMENTS__NAME = hydra.core.Name("arguments")
UNQUALIFIED_CLASS_INSTANCE_CREATION_EXPRESSION__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class ClassOrInterfaceTypeToInstantiate:
    identifiers: frozenlist[AnnotatedIdentifier]
    type_arguments: Maybe[TypeArgumentsOrDiamond]

CLASS_OR_INTERFACE_TYPE_TO_INSTANTIATE__NAME = hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate")
CLASS_OR_INTERFACE_TYPE_TO_INSTANTIATE__IDENTIFIERS__NAME = hydra.core.Name("identifiers")
CLASS_OR_INTERFACE_TYPE_TO_INSTANTIATE__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class AnnotatedIdentifier:
    annotations: frozenlist[Annotation]
    identifier: Identifier

ANNOTATED_IDENTIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.AnnotatedIdentifier")
ANNOTATED_IDENTIFIER__ANNOTATIONS__NAME = hydra.core.Name("annotations")
ANNOTATED_IDENTIFIER__IDENTIFIER__NAME = hydra.core.Name("identifier")

class TypeArgumentsOrDiamondArguments(Node["frozenlist[TypeArgument]"]):
...

class TypeArgumentsOrDiamondDiamond:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeArgumentsOrDiamondDiamond)
    def __hash__(self):
        return hash("TypeArgumentsOrDiamondDiamond")

class _TypeArgumentsOrDiamondMeta(type):
    def __getitem__(cls, item):
        return object

class TypeArgumentsOrDiamond(metaclass=_TypeArgumentsOrDiamondMeta):
    r"""TypeArgumentsOrDiamondArguments | TypeArgumentsOrDiamondDiamond"""
    
    pass

TYPE_ARGUMENTS_OR_DIAMOND__NAME = hydra.core.Name("hydra.ext.java.syntax.TypeArgumentsOrDiamond")
TYPE_ARGUMENTS_OR_DIAMOND__ARGUMENTS__NAME = hydra.core.Name("arguments")
TYPE_ARGUMENTS_OR_DIAMOND__DIAMOND__NAME = hydra.core.Name("diamond")

@dataclass(frozen=True)
class FieldAccess:
    qualifier: FieldAccess_Qualifier
    identifier: Identifier

FIELD_ACCESS__NAME = hydra.core.Name("hydra.ext.java.syntax.FieldAccess")
FIELD_ACCESS__QUALIFIER__NAME = hydra.core.Name("qualifier")
FIELD_ACCESS__IDENTIFIER__NAME = hydra.core.Name("identifier")

class FieldAccess_QualifierPrimary(Node["Primary"]):
...

class FieldAccess_QualifierSuper:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, FieldAccess_QualifierSuper)
    def __hash__(self):
        return hash("FieldAccess_QualifierSuper")

class FieldAccess_QualifierTyped(Node["TypeName"]):
...

class _FieldAccess_QualifierMeta(type):
    def __getitem__(cls, item):
        return object

class FieldAccess_Qualifier(metaclass=_FieldAccess_QualifierMeta):
    r"""FieldAccess_QualifierPrimary | FieldAccess_QualifierSuper | FieldAccess_QualifierTyped"""
    
    pass

FIELD_ACCESS__QUALIFIER__NAME = hydra.core.Name("hydra.ext.java.syntax.FieldAccess_Qualifier")
FIELD_ACCESS__QUALIFIER__PRIMARY__NAME = hydra.core.Name("primary")
FIELD_ACCESS__QUALIFIER__SUPER__NAME = hydra.core.Name("super")
FIELD_ACCESS__QUALIFIER__TYPED__NAME = hydra.core.Name("typed")

@dataclass(frozen=True)
class ArrayAccess:
    expression: Maybe[Expression]
    variant: ArrayAccess_Variant

ARRAY_ACCESS__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayAccess")
ARRAY_ACCESS__EXPRESSION__NAME = hydra.core.Name("expression")
ARRAY_ACCESS__VARIANT__NAME = hydra.core.Name("variant")

class ArrayAccess_VariantName(Node["ExpressionName"]):
...

class ArrayAccess_VariantPrimary(Node["PrimaryNoNewArray"]):
...

class _ArrayAccess_VariantMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayAccess_Variant(metaclass=_ArrayAccess_VariantMeta):
    r"""ArrayAccess_VariantName | ArrayAccess_VariantPrimary"""
    
    pass

ARRAY_ACCESS__VARIANT__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayAccess_Variant")
ARRAY_ACCESS__VARIANT__NAME__NAME = hydra.core.Name("name")
ARRAY_ACCESS__VARIANT__PRIMARY__NAME = hydra.core.Name("primary")

@dataclass(frozen=True)
class MethodInvocation:
    header: MethodInvocation_Header
    arguments: frozenlist[Expression]

METHOD_INVOCATION__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation")
METHOD_INVOCATION__HEADER__NAME = hydra.core.Name("header")
METHOD_INVOCATION__ARGUMENTS__NAME = hydra.core.Name("arguments")

class MethodInvocation_HeaderSimple(Node["MethodName"]):
...

class MethodInvocation_HeaderComplex(Node["MethodInvocation_Complex"]):
...

class _MethodInvocation_HeaderMeta(type):
    def __getitem__(cls, item):
        return object

class MethodInvocation_Header(metaclass=_MethodInvocation_HeaderMeta):
    r"""MethodInvocation_HeaderSimple | MethodInvocation_HeaderComplex"""
    
    pass

METHOD_INVOCATION__HEADER__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Header")
METHOD_INVOCATION__HEADER__SIMPLE__NAME = hydra.core.Name("simple")
METHOD_INVOCATION__HEADER__COMPLEX__NAME = hydra.core.Name("complex")

@dataclass(frozen=True)
class MethodInvocation_Complex:
    variant: MethodInvocation_Variant
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

METHOD_INVOCATION__COMPLEX__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Complex")
METHOD_INVOCATION__COMPLEX__VARIANT__NAME = hydra.core.Name("variant")
METHOD_INVOCATION__COMPLEX__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
METHOD_INVOCATION__COMPLEX__IDENTIFIER__NAME = hydra.core.Name("identifier")

class MethodInvocation_VariantType(Node["TypeName"]):
...

class MethodInvocation_VariantExpression(Node["ExpressionName"]):
...

class MethodInvocation_VariantPrimary(Node["Primary"]):
...

class MethodInvocation_VariantSuper:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, MethodInvocation_VariantSuper)
    def __hash__(self):
        return hash("MethodInvocation_VariantSuper")

class MethodInvocation_VariantTypeSuper(Node["TypeName"]):
...

class _MethodInvocation_VariantMeta(type):
    def __getitem__(cls, item):
        return object

class MethodInvocation_Variant(metaclass=_MethodInvocation_VariantMeta):
    r"""MethodInvocation_VariantType | MethodInvocation_VariantExpression | MethodInvocation_VariantPrimary | MethodInvocation_VariantSuper | MethodInvocation_VariantTypeSuper"""
    
    pass

METHOD_INVOCATION__VARIANT__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Variant")
METHOD_INVOCATION__VARIANT__TYPE__NAME = hydra.core.Name("type")
METHOD_INVOCATION__VARIANT__EXPRESSION__NAME = hydra.core.Name("expression")
METHOD_INVOCATION__VARIANT__PRIMARY__NAME = hydra.core.Name("primary")
METHOD_INVOCATION__VARIANT__SUPER__NAME = hydra.core.Name("super")
METHOD_INVOCATION__VARIANT__TYPE_SUPER__NAME = hydra.core.Name("typeSuper")

class MethodReferenceExpression(Node["MethodReference_Expression"]):
...

class MethodReferencePrimary(Node["MethodReference_Primary"]):
...

class MethodReferenceReferenceType(Node["MethodReference_ReferenceType"]):
...

class MethodReferenceSuper(Node["MethodReference_Super"]):
...

class MethodReferenceNew(Node["MethodReference_New"]):
...

class MethodReferenceArray(Node["MethodReference_Array"]):
...

class _MethodReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class MethodReference(metaclass=_MethodReferenceMeta):
    r"""MethodReferenceExpression | MethodReferencePrimary | MethodReferenceReferenceType | MethodReferenceSuper | MethodReferenceNew | MethodReferenceArray"""
    
    pass

METHOD_REFERENCE__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference")
METHOD_REFERENCE__EXPRESSION__NAME = hydra.core.Name("expression")
METHOD_REFERENCE__PRIMARY__NAME = hydra.core.Name("primary")
METHOD_REFERENCE__REFERENCE_TYPE__NAME = hydra.core.Name("referenceType")
METHOD_REFERENCE__SUPER__NAME = hydra.core.Name("super")
METHOD_REFERENCE__NEW__NAME = hydra.core.Name("new")
METHOD_REFERENCE__ARRAY__NAME = hydra.core.Name("array")

@dataclass(frozen=True)
class MethodReference_Expression:
    name: ExpressionName
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

METHOD_REFERENCE__EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Expression")
METHOD_REFERENCE__EXPRESSION__NAME__NAME = hydra.core.Name("name")
METHOD_REFERENCE__EXPRESSION__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
METHOD_REFERENCE__EXPRESSION__IDENTIFIER__NAME = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MethodReference_Primary:
    primary: Primary
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

METHOD_REFERENCE__PRIMARY__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Primary")
METHOD_REFERENCE__PRIMARY__PRIMARY__NAME = hydra.core.Name("primary")
METHOD_REFERENCE__PRIMARY__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
METHOD_REFERENCE__PRIMARY__IDENTIFIER__NAME = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MethodReference_ReferenceType:
    reference_type: ReferenceType
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

METHOD_REFERENCE__REFERENCE_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference_ReferenceType")
METHOD_REFERENCE__REFERENCE_TYPE__REFERENCE_TYPE__NAME = hydra.core.Name("referenceType")
METHOD_REFERENCE__REFERENCE_TYPE__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
METHOD_REFERENCE__REFERENCE_TYPE__IDENTIFIER__NAME = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MethodReference_Super:
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier
    super: bool

METHOD_REFERENCE__SUPER__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Super")
METHOD_REFERENCE__SUPER__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")
METHOD_REFERENCE__SUPER__IDENTIFIER__NAME = hydra.core.Name("identifier")
METHOD_REFERENCE__SUPER__SUPER__NAME = hydra.core.Name("super")

@dataclass(frozen=True)
class MethodReference_New:
    class_type: ClassType
    type_arguments: frozenlist[TypeArgument]

METHOD_REFERENCE__NEW__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference_New")
METHOD_REFERENCE__NEW__CLASS_TYPE__NAME = hydra.core.Name("classType")
METHOD_REFERENCE__NEW__TYPE_ARGUMENTS__NAME = hydra.core.Name("typeArguments")

class MethodReference_Array(Node["ArrayType"]):
...

METHOD_REFERENCE__ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Array")

class ArrayCreationExpressionPrimitive(Node["ArrayCreationExpression_Primitive"]):
...

class ArrayCreationExpressionClassOrInterface(Node["ArrayCreationExpression_ClassOrInterface"]):
...

class ArrayCreationExpressionPrimitiveArray(Node["ArrayCreationExpression_PrimitiveArray"]):
...

class ArrayCreationExpressionClassOrInterfaceArray(Node["ArrayCreationExpression_ClassOrInterfaceArray"]):
...

class _ArrayCreationExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayCreationExpression(metaclass=_ArrayCreationExpressionMeta):
    r"""ArrayCreationExpressionPrimitive | ArrayCreationExpressionClassOrInterface | ArrayCreationExpressionPrimitiveArray | ArrayCreationExpressionClassOrInterfaceArray"""
    
    pass

ARRAY_CREATION_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression")
ARRAY_CREATION_EXPRESSION__PRIMITIVE__NAME = hydra.core.Name("primitive")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE__NAME = hydra.core.Name("classOrInterface")
ARRAY_CREATION_EXPRESSION__PRIMITIVE_ARRAY__NAME = hydra.core.Name("primitiveArray")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE_ARRAY__NAME = hydra.core.Name("classOrInterfaceArray")

@dataclass(frozen=True)
class ArrayCreationExpression_Primitive:
    type: PrimitiveTypeWithAnnotations
    dim_exprs: frozenlist[DimExpr]
    dims: Maybe[Dims]

ARRAY_CREATION_EXPRESSION__PRIMITIVE__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_Primitive")
ARRAY_CREATION_EXPRESSION__PRIMITIVE__TYPE__NAME = hydra.core.Name("type")
ARRAY_CREATION_EXPRESSION__PRIMITIVE__DIM_EXPRS__NAME = hydra.core.Name("dimExprs")
ARRAY_CREATION_EXPRESSION__PRIMITIVE__DIMS__NAME = hydra.core.Name("dims")

@dataclass(frozen=True)
class ArrayCreationExpression_ClassOrInterface:
    type: ClassOrInterfaceType
    dim_exprs: frozenlist[DimExpr]
    dims: Maybe[Dims]

ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE__TYPE__NAME = hydra.core.Name("type")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE__DIM_EXPRS__NAME = hydra.core.Name("dimExprs")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE__DIMS__NAME = hydra.core.Name("dims")

@dataclass(frozen=True)
class ArrayCreationExpression_PrimitiveArray:
    type: PrimitiveTypeWithAnnotations
    dims: frozenlist[Dims]
    array: ArrayInitializer

ARRAY_CREATION_EXPRESSION__PRIMITIVE_ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray")
ARRAY_CREATION_EXPRESSION__PRIMITIVE_ARRAY__TYPE__NAME = hydra.core.Name("type")
ARRAY_CREATION_EXPRESSION__PRIMITIVE_ARRAY__DIMS__NAME = hydra.core.Name("dims")
ARRAY_CREATION_EXPRESSION__PRIMITIVE_ARRAY__ARRAY__NAME = hydra.core.Name("array")

@dataclass(frozen=True)
class ArrayCreationExpression_ClassOrInterfaceArray:
    type: ClassOrInterfaceType
    dims: frozenlist[Dims]
    array: ArrayInitializer

ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE_ARRAY__NAME = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE_ARRAY__TYPE__NAME = hydra.core.Name("type")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE_ARRAY__DIMS__NAME = hydra.core.Name("dims")
ARRAY_CREATION_EXPRESSION__CLASS_OR_INTERFACE_ARRAY__ARRAY__NAME = hydra.core.Name("array")

@dataclass(frozen=True)
class DimExpr:
    annotations: frozenlist[Annotation]
    expression: Maybe[Expression]

DIM_EXPR__NAME = hydra.core.Name("hydra.ext.java.syntax.DimExpr")
DIM_EXPR__ANNOTATIONS__NAME = hydra.core.Name("annotations")
DIM_EXPR__EXPRESSION__NAME = hydra.core.Name("expression")

class ExpressionLambda(Node["LambdaExpression"]):
...

class ExpressionAssignment(Node["AssignmentExpression"]):
...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionLambda | ExpressionAssignment"""
    
    pass

EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.Expression")
EXPRESSION__LAMBDA__NAME = hydra.core.Name("lambda")
EXPRESSION__ASSIGNMENT__NAME = hydra.core.Name("assignment")

@dataclass(frozen=True)
class LambdaExpression:
    parameters: LambdaParameters
    body: LambdaBody

LAMBDA_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.LambdaExpression")
LAMBDA_EXPRESSION__PARAMETERS__NAME = hydra.core.Name("parameters")
LAMBDA_EXPRESSION__BODY__NAME = hydra.core.Name("body")

class LambdaParametersTuple(Node["frozenlist[LambdaParameters]"]):
...

class LambdaParametersSingle(Node["Identifier"]):
...

class _LambdaParametersMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaParameters(metaclass=_LambdaParametersMeta):
    r"""LambdaParametersTuple | LambdaParametersSingle"""
    
    pass

LAMBDA_PARAMETERS__NAME = hydra.core.Name("hydra.ext.java.syntax.LambdaParameters")
LAMBDA_PARAMETERS__TUPLE__NAME = hydra.core.Name("tuple")
LAMBDA_PARAMETERS__SINGLE__NAME = hydra.core.Name("single")

class LambdaParameterNormal(Node["LambdaParameter_Normal"]):
...

class LambdaParameterVariableArity(Node["VariableArityParameter"]):
...

class _LambdaParameterMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaParameter(metaclass=_LambdaParameterMeta):
    r"""LambdaParameterNormal | LambdaParameterVariableArity"""
    
    pass

LAMBDA_PARAMETER__NAME = hydra.core.Name("hydra.ext.java.syntax.LambdaParameter")
LAMBDA_PARAMETER__NORMAL__NAME = hydra.core.Name("normal")
LAMBDA_PARAMETER__VARIABLE_ARITY__NAME = hydra.core.Name("variableArity")

@dataclass(frozen=True)
class LambdaParameter_Normal:
    modifiers: frozenlist[VariableModifier]
    type: LambdaParameterType
    id: VariableDeclaratorId

LAMBDA_PARAMETER__NORMAL__NAME = hydra.core.Name("hydra.ext.java.syntax.LambdaParameter_Normal")
LAMBDA_PARAMETER__NORMAL__MODIFIERS__NAME = hydra.core.Name("modifiers")
LAMBDA_PARAMETER__NORMAL__TYPE__NAME = hydra.core.Name("type")
LAMBDA_PARAMETER__NORMAL__ID__NAME = hydra.core.Name("id")

class LambdaParameterTypeType(Node["UnannType"]):
...

class LambdaParameterTypeVar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LambdaParameterTypeVar)
    def __hash__(self):
        return hash("LambdaParameterTypeVar")

class _LambdaParameterTypeMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaParameterType(metaclass=_LambdaParameterTypeMeta):
    r"""LambdaParameterTypeType | LambdaParameterTypeVar"""
    
    pass

LAMBDA_PARAMETER_TYPE__NAME = hydra.core.Name("hydra.ext.java.syntax.LambdaParameterType")
LAMBDA_PARAMETER_TYPE__TYPE__NAME = hydra.core.Name("type")
LAMBDA_PARAMETER_TYPE__VAR__NAME = hydra.core.Name("var")

class LambdaBodyExpression(Node["Expression"]):
...

class LambdaBodyBlock(Node["Block"]):
...

class _LambdaBodyMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaBody(metaclass=_LambdaBodyMeta):
    r"""LambdaBodyExpression | LambdaBodyBlock"""
    
    pass

LAMBDA_BODY__NAME = hydra.core.Name("hydra.ext.java.syntax.LambdaBody")
LAMBDA_BODY__EXPRESSION__NAME = hydra.core.Name("expression")
LAMBDA_BODY__BLOCK__NAME = hydra.core.Name("block")

class AssignmentExpressionConditional(Node["ConditionalExpression"]):
...

class AssignmentExpressionAssignment(Node["Assignment"]):
...

class _AssignmentExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AssignmentExpression(metaclass=_AssignmentExpressionMeta):
    r"""AssignmentExpressionConditional | AssignmentExpressionAssignment"""
    
    pass

ASSIGNMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.AssignmentExpression")
ASSIGNMENT_EXPRESSION__CONDITIONAL__NAME = hydra.core.Name("conditional")
ASSIGNMENT_EXPRESSION__ASSIGNMENT__NAME = hydra.core.Name("assignment")

@dataclass(frozen=True)
class Assignment:
    lhs: LeftHandSide
    op: AssignmentOperator
    expression: Expression

ASSIGNMENT__NAME = hydra.core.Name("hydra.ext.java.syntax.Assignment")
ASSIGNMENT__LHS__NAME = hydra.core.Name("lhs")
ASSIGNMENT__OP__NAME = hydra.core.Name("op")
ASSIGNMENT__EXPRESSION__NAME = hydra.core.Name("expression")

class LeftHandSideExpressionName(Node["ExpressionName"]):
...

class LeftHandSideFieldAccess(Node["FieldAccess"]):
...

class LeftHandSideArrayAccess(Node["ArrayAccess"]):
...

class _LeftHandSideMeta(type):
    def __getitem__(cls, item):
        return object

class LeftHandSide(metaclass=_LeftHandSideMeta):
    r"""LeftHandSideExpressionName | LeftHandSideFieldAccess | LeftHandSideArrayAccess"""
    
    pass

LEFT_HAND_SIDE__NAME = hydra.core.Name("hydra.ext.java.syntax.LeftHandSide")
LEFT_HAND_SIDE__EXPRESSION_NAME__NAME = hydra.core.Name("expressionName")
LEFT_HAND_SIDE__FIELD_ACCESS__NAME = hydra.core.Name("fieldAccess")
LEFT_HAND_SIDE__ARRAY_ACCESS__NAME = hydra.core.Name("arrayAccess")

class AssignmentOperator(Enum):
    SIMPLE = "simple"
    
    TIMES = "times"
    
    DIV = "div"
    
    MOD = "mod"
    
    PLUS = "plus"
    
    MINUS = "minus"
    
    SHIFT_LEFT = "shiftLeft"
    
    SHIFT_RIGHT = "shiftRight"
    
    SHIFT_RIGHT_ZERO_FILL = "shiftRightZeroFill"
    
    AND = "and"
    
    XOR = "xor"
    
    OR = "or"

ASSIGNMENT_OPERATOR__NAME = hydra.core.Name("hydra.ext.java.syntax.AssignmentOperator")
ASSIGNMENT_OPERATOR__SIMPLE__NAME = hydra.core.Name("simple")
ASSIGNMENT_OPERATOR__TIMES__NAME = hydra.core.Name("times")
ASSIGNMENT_OPERATOR__DIV__NAME = hydra.core.Name("div")
ASSIGNMENT_OPERATOR__MOD__NAME = hydra.core.Name("mod")
ASSIGNMENT_OPERATOR__PLUS__NAME = hydra.core.Name("plus")
ASSIGNMENT_OPERATOR__MINUS__NAME = hydra.core.Name("minus")
ASSIGNMENT_OPERATOR__SHIFT_LEFT__NAME = hydra.core.Name("shiftLeft")
ASSIGNMENT_OPERATOR__SHIFT_RIGHT__NAME = hydra.core.Name("shiftRight")
ASSIGNMENT_OPERATOR__SHIFT_RIGHT_ZERO_FILL__NAME = hydra.core.Name("shiftRightZeroFill")
ASSIGNMENT_OPERATOR__AND__NAME = hydra.core.Name("and")
ASSIGNMENT_OPERATOR__XOR__NAME = hydra.core.Name("xor")
ASSIGNMENT_OPERATOR__OR__NAME = hydra.core.Name("or")

class ConditionalExpressionSimple(Node["ConditionalOrExpression"]):
...

class ConditionalExpressionTernaryCond(Node["ConditionalExpression_TernaryCond"]):
...

class ConditionalExpressionTernaryLambda(Node["ConditionalExpression_TernaryLambda"]):
...

class _ConditionalExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ConditionalExpression(metaclass=_ConditionalExpressionMeta):
    r"""ConditionalExpressionSimple | ConditionalExpressionTernaryCond | ConditionalExpressionTernaryLambda"""
    
    pass

CONDITIONAL_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression")
CONDITIONAL_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")
CONDITIONAL_EXPRESSION__TERNARY_COND__NAME = hydra.core.Name("ternaryCond")
CONDITIONAL_EXPRESSION__TERNARY_LAMBDA__NAME = hydra.core.Name("ternaryLambda")

@dataclass(frozen=True)
class ConditionalExpression_TernaryCond:
    cond: ConditionalOrExpression
    if_true: Expression
    if_false: ConditionalExpression

CONDITIONAL_EXPRESSION__TERNARY_COND__NAME = hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression_TernaryCond")
CONDITIONAL_EXPRESSION__TERNARY_COND__COND__NAME = hydra.core.Name("cond")
CONDITIONAL_EXPRESSION__TERNARY_COND__IF_TRUE__NAME = hydra.core.Name("ifTrue")
CONDITIONAL_EXPRESSION__TERNARY_COND__IF_FALSE__NAME = hydra.core.Name("ifFalse")

@dataclass(frozen=True)
class ConditionalExpression_TernaryLambda:
    cond: ConditionalOrExpression
    if_true: Expression
    if_false: LambdaExpression

CONDITIONAL_EXPRESSION__TERNARY_LAMBDA__NAME = hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression_TernaryLambda")
CONDITIONAL_EXPRESSION__TERNARY_LAMBDA__COND__NAME = hydra.core.Name("cond")
CONDITIONAL_EXPRESSION__TERNARY_LAMBDA__IF_TRUE__NAME = hydra.core.Name("ifTrue")
CONDITIONAL_EXPRESSION__TERNARY_LAMBDA__IF_FALSE__NAME = hydra.core.Name("ifFalse")

class ConditionalOrExpression(Node["frozenlist[ConditionalAndExpression]"]):
...

CONDITIONAL_OR_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ConditionalOrExpression")

class ConditionalAndExpression(Node["frozenlist[InclusiveOrExpression]"]):
...

CONDITIONAL_AND_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ConditionalAndExpression")

class InclusiveOrExpression(Node["frozenlist[ExclusiveOrExpression]"]):
...

INCLUSIVE_OR_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.InclusiveOrExpression")

class ExclusiveOrExpression(Node["frozenlist[AndExpression]"]):
...

EXCLUSIVE_OR_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ExclusiveOrExpression")

class AndExpression(Node["frozenlist[EqualityExpression]"]):
...

AND_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.AndExpression")

class EqualityExpressionUnary(Node["RelationalExpression"]):
...

class EqualityExpressionEqual(Node["EqualityExpression_Binary"]):
...

class EqualityExpressionNotEqual(Node["EqualityExpression_Binary"]):
...

class _EqualityExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class EqualityExpression(metaclass=_EqualityExpressionMeta):
    r"""EqualityExpressionUnary | EqualityExpressionEqual | EqualityExpressionNotEqual"""
    
    pass

EQUALITY_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.EqualityExpression")
EQUALITY_EXPRESSION__UNARY__NAME = hydra.core.Name("unary")
EQUALITY_EXPRESSION__EQUAL__NAME = hydra.core.Name("equal")
EQUALITY_EXPRESSION__NOT_EQUAL__NAME = hydra.core.Name("notEqual")

@dataclass(frozen=True)
class EqualityExpression_Binary:
    lhs: EqualityExpression
    rhs: RelationalExpression

EQUALITY_EXPRESSION__BINARY__NAME = hydra.core.Name("hydra.ext.java.syntax.EqualityExpression_Binary")
EQUALITY_EXPRESSION__BINARY__LHS__NAME = hydra.core.Name("lhs")
EQUALITY_EXPRESSION__BINARY__RHS__NAME = hydra.core.Name("rhs")

class RelationalExpressionSimple(Node["ShiftExpression"]):
...

class RelationalExpressionLessThan(Node["RelationalExpression_LessThan"]):
...

class RelationalExpressionGreaterThan(Node["RelationalExpression_GreaterThan"]):
...

class RelationalExpressionLessThanEqual(Node["RelationalExpression_LessThanEqual"]):
...

class RelationalExpressionGreaterThanEqual(Node["RelationalExpression_GreaterThanEqual"]):
...

class RelationalExpressionInstanceof(Node["RelationalExpression_InstanceOf"]):
...

class _RelationalExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class RelationalExpression(metaclass=_RelationalExpressionMeta):
    r"""RelationalExpressionSimple | RelationalExpressionLessThan | RelationalExpressionGreaterThan | RelationalExpressionLessThanEqual | RelationalExpressionGreaterThanEqual | RelationalExpressionInstanceof"""
    
    pass

RELATIONAL_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression")
RELATIONAL_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")
RELATIONAL_EXPRESSION__LESS_THAN__NAME = hydra.core.Name("lessThan")
RELATIONAL_EXPRESSION__GREATER_THAN__NAME = hydra.core.Name("greaterThan")
RELATIONAL_EXPRESSION__LESS_THAN_EQUAL__NAME = hydra.core.Name("lessThanEqual")
RELATIONAL_EXPRESSION__GREATER_THAN_EQUAL__NAME = hydra.core.Name("greaterThanEqual")
RELATIONAL_EXPRESSION__INSTANCEOF__NAME = hydra.core.Name("instanceof")

@dataclass(frozen=True)
class RelationalExpression_LessThan:
    lhs: RelationalExpression
    rhs: ShiftExpression

RELATIONAL_EXPRESSION__LESS_THAN__NAME = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_LessThan")
RELATIONAL_EXPRESSION__LESS_THAN__LHS__NAME = hydra.core.Name("lhs")
RELATIONAL_EXPRESSION__LESS_THAN__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_GreaterThan:
    lhs: RelationalExpression
    rhs: ShiftExpression

RELATIONAL_EXPRESSION__GREATER_THAN__NAME = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_GreaterThan")
RELATIONAL_EXPRESSION__GREATER_THAN__LHS__NAME = hydra.core.Name("lhs")
RELATIONAL_EXPRESSION__GREATER_THAN__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_LessThanEqual:
    lhs: RelationalExpression
    rhs: ShiftExpression

RELATIONAL_EXPRESSION__LESS_THAN_EQUAL__NAME = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_LessThanEqual")
RELATIONAL_EXPRESSION__LESS_THAN_EQUAL__LHS__NAME = hydra.core.Name("lhs")
RELATIONAL_EXPRESSION__LESS_THAN_EQUAL__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_GreaterThanEqual:
    lhs: RelationalExpression
    rhs: ShiftExpression

RELATIONAL_EXPRESSION__GREATER_THAN_EQUAL__NAME = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual")
RELATIONAL_EXPRESSION__GREATER_THAN_EQUAL__LHS__NAME = hydra.core.Name("lhs")
RELATIONAL_EXPRESSION__GREATER_THAN_EQUAL__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_InstanceOf:
    lhs: RelationalExpression
    rhs: ReferenceType

RELATIONAL_EXPRESSION__INSTANCE_OF__NAME = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_InstanceOf")
RELATIONAL_EXPRESSION__INSTANCE_OF__LHS__NAME = hydra.core.Name("lhs")
RELATIONAL_EXPRESSION__INSTANCE_OF__RHS__NAME = hydra.core.Name("rhs")

class ShiftExpressionUnary(Node["AdditiveExpression"]):
...

class ShiftExpressionShiftLeft(Node["ShiftExpression_Binary"]):
...

class ShiftExpressionShiftRight(Node["ShiftExpression_Binary"]):
...

class ShiftExpressionShiftRightZeroFill(Node["ShiftExpression_Binary"]):
...

class _ShiftExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ShiftExpression(metaclass=_ShiftExpressionMeta):
    r"""ShiftExpressionUnary | ShiftExpressionShiftLeft | ShiftExpressionShiftRight | ShiftExpressionShiftRightZeroFill"""
    
    pass

SHIFT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ShiftExpression")
SHIFT_EXPRESSION__UNARY__NAME = hydra.core.Name("unary")
SHIFT_EXPRESSION__SHIFT_LEFT__NAME = hydra.core.Name("shiftLeft")
SHIFT_EXPRESSION__SHIFT_RIGHT__NAME = hydra.core.Name("shiftRight")
SHIFT_EXPRESSION__SHIFT_RIGHT_ZERO_FILL__NAME = hydra.core.Name("shiftRightZeroFill")

@dataclass(frozen=True)
class ShiftExpression_Binary:
    lhs: ShiftExpression
    rhs: AdditiveExpression

SHIFT_EXPRESSION__BINARY__NAME = hydra.core.Name("hydra.ext.java.syntax.ShiftExpression_Binary")
SHIFT_EXPRESSION__BINARY__LHS__NAME = hydra.core.Name("lhs")
SHIFT_EXPRESSION__BINARY__RHS__NAME = hydra.core.Name("rhs")

class AdditiveExpressionUnary(Node["MultiplicativeExpression"]):
...

class AdditiveExpressionPlus(Node["AdditiveExpression_Binary"]):
...

class AdditiveExpressionMinus(Node["AdditiveExpression_Binary"]):
...

class _AdditiveExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AdditiveExpression(metaclass=_AdditiveExpressionMeta):
    r"""AdditiveExpressionUnary | AdditiveExpressionPlus | AdditiveExpressionMinus"""
    
    pass

ADDITIVE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.AdditiveExpression")
ADDITIVE_EXPRESSION__UNARY__NAME = hydra.core.Name("unary")
ADDITIVE_EXPRESSION__PLUS__NAME = hydra.core.Name("plus")
ADDITIVE_EXPRESSION__MINUS__NAME = hydra.core.Name("minus")

@dataclass(frozen=True)
class AdditiveExpression_Binary:
    lhs: AdditiveExpression
    rhs: MultiplicativeExpression

ADDITIVE_EXPRESSION__BINARY__NAME = hydra.core.Name("hydra.ext.java.syntax.AdditiveExpression_Binary")
ADDITIVE_EXPRESSION__BINARY__LHS__NAME = hydra.core.Name("lhs")
ADDITIVE_EXPRESSION__BINARY__RHS__NAME = hydra.core.Name("rhs")

class MultiplicativeExpressionUnary(Node["UnaryExpression"]):
...

class MultiplicativeExpressionTimes(Node["MultiplicativeExpression_Binary"]):
...

class MultiplicativeExpressionDivide(Node["MultiplicativeExpression_Binary"]):
...

class MultiplicativeExpressionMod(Node["MultiplicativeExpression_Binary"]):
...

class _MultiplicativeExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class MultiplicativeExpression(metaclass=_MultiplicativeExpressionMeta):
    r"""MultiplicativeExpressionUnary | MultiplicativeExpressionTimes | MultiplicativeExpressionDivide | MultiplicativeExpressionMod"""
    
    pass

MULTIPLICATIVE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.MultiplicativeExpression")
MULTIPLICATIVE_EXPRESSION__UNARY__NAME = hydra.core.Name("unary")
MULTIPLICATIVE_EXPRESSION__TIMES__NAME = hydra.core.Name("times")
MULTIPLICATIVE_EXPRESSION__DIVIDE__NAME = hydra.core.Name("divide")
MULTIPLICATIVE_EXPRESSION__MOD__NAME = hydra.core.Name("mod")

@dataclass(frozen=True)
class MultiplicativeExpression_Binary:
    lhs: MultiplicativeExpression
    rhs: UnaryExpression

MULTIPLICATIVE_EXPRESSION__BINARY__NAME = hydra.core.Name("hydra.ext.java.syntax.MultiplicativeExpression_Binary")
MULTIPLICATIVE_EXPRESSION__BINARY__LHS__NAME = hydra.core.Name("lhs")
MULTIPLICATIVE_EXPRESSION__BINARY__RHS__NAME = hydra.core.Name("rhs")

class UnaryExpressionPreIncrement(Node["PreIncrementExpression"]):
...

class UnaryExpressionPreDecrement(Node["PreDecrementExpression"]):
...

class UnaryExpressionPlus(Node["UnaryExpression"]):
...

class UnaryExpressionMinus(Node["UnaryExpression"]):
...

class UnaryExpressionOther(Node["UnaryExpressionNotPlusMinus"]):
...

class _UnaryExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryExpression(metaclass=_UnaryExpressionMeta):
    r"""UnaryExpressionPreIncrement | UnaryExpressionPreDecrement | UnaryExpressionPlus | UnaryExpressionMinus | UnaryExpressionOther"""
    
    pass

UNARY_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.UnaryExpression")
UNARY_EXPRESSION__PRE_INCREMENT__NAME = hydra.core.Name("preIncrement")
UNARY_EXPRESSION__PRE_DECREMENT__NAME = hydra.core.Name("preDecrement")
UNARY_EXPRESSION__PLUS__NAME = hydra.core.Name("plus")
UNARY_EXPRESSION__MINUS__NAME = hydra.core.Name("minus")
UNARY_EXPRESSION__OTHER__NAME = hydra.core.Name("other")

class PreIncrementExpression(Node["UnaryExpression"]):
...

PRE_INCREMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.PreIncrementExpression")

class PreDecrementExpression(Node["UnaryExpression"]):
...

PRE_DECREMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.PreDecrementExpression")

class UnaryExpressionNotPlusMinusPostfix(Node["PostfixExpression"]):
...

class UnaryExpressionNotPlusMinusTilde(Node["UnaryExpression"]):
...

class UnaryExpressionNotPlusMinusNot(Node["UnaryExpression"]):
...

class UnaryExpressionNotPlusMinusCast(Node["CastExpression"]):
...

class _UnaryExpressionNotPlusMinusMeta(type):
    def __getitem__(cls, item):
        return object

class UnaryExpressionNotPlusMinus(metaclass=_UnaryExpressionNotPlusMinusMeta):
    r"""UnaryExpressionNotPlusMinusPostfix | UnaryExpressionNotPlusMinusTilde | UnaryExpressionNotPlusMinusNot | UnaryExpressionNotPlusMinusCast"""
    
    pass

UNARY_EXPRESSION_NOT_PLUS_MINUS__NAME = hydra.core.Name("hydra.ext.java.syntax.UnaryExpressionNotPlusMinus")
UNARY_EXPRESSION_NOT_PLUS_MINUS__POSTFIX__NAME = hydra.core.Name("postfix")
UNARY_EXPRESSION_NOT_PLUS_MINUS__TILDE__NAME = hydra.core.Name("tilde")
UNARY_EXPRESSION_NOT_PLUS_MINUS__NOT__NAME = hydra.core.Name("not")
UNARY_EXPRESSION_NOT_PLUS_MINUS__CAST__NAME = hydra.core.Name("cast")

class PostfixExpressionPrimary(Node["Primary"]):
...

class PostfixExpressionName(Node["ExpressionName"]):
...

class PostfixExpressionPostIncrement(Node["PostIncrementExpression"]):
...

class PostfixExpressionPostDecrement(Node["PostDecrementExpression"]):
...

class _PostfixExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PostfixExpression(metaclass=_PostfixExpressionMeta):
    r"""PostfixExpressionPrimary | PostfixExpressionName | PostfixExpressionPostIncrement | PostfixExpressionPostDecrement"""
    
    pass

POSTFIX_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.PostfixExpression")
POSTFIX_EXPRESSION__PRIMARY__NAME = hydra.core.Name("primary")
POSTFIX_EXPRESSION__NAME__NAME = hydra.core.Name("name")
POSTFIX_EXPRESSION__POST_INCREMENT__NAME = hydra.core.Name("postIncrement")
POSTFIX_EXPRESSION__POST_DECREMENT__NAME = hydra.core.Name("postDecrement")

class PostIncrementExpression(Node["PostfixExpression"]):
...

POST_INCREMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.PostIncrementExpression")

class PostDecrementExpression(Node["PostfixExpression"]):
...

POST_DECREMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.PostDecrementExpression")

class CastExpressionPrimitive(Node["CastExpression_Primitive"]):
...

class CastExpressionNotPlusMinus(Node["CastExpression_NotPlusMinus"]):
...

class CastExpressionLambda(Node["CastExpression_Lambda"]):
...

class _CastExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class CastExpression(metaclass=_CastExpressionMeta):
    r"""CastExpressionPrimitive | CastExpressionNotPlusMinus | CastExpressionLambda"""
    
    pass

CAST_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.CastExpression")
CAST_EXPRESSION__PRIMITIVE__NAME = hydra.core.Name("primitive")
CAST_EXPRESSION__NOT_PLUS_MINUS__NAME = hydra.core.Name("notPlusMinus")
CAST_EXPRESSION__LAMBDA__NAME = hydra.core.Name("lambda")

@dataclass(frozen=True)
class CastExpression_Primitive:
    type: PrimitiveTypeWithAnnotations
    expression: UnaryExpression

CAST_EXPRESSION__PRIMITIVE__NAME = hydra.core.Name("hydra.ext.java.syntax.CastExpression_Primitive")
CAST_EXPRESSION__PRIMITIVE__TYPE__NAME = hydra.core.Name("type")
CAST_EXPRESSION__PRIMITIVE__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class CastExpression_NotPlusMinus:
    ref_and_bounds: CastExpression_RefAndBounds
    expression: UnaryExpression

CAST_EXPRESSION__NOT_PLUS_MINUS__NAME = hydra.core.Name("hydra.ext.java.syntax.CastExpression_NotPlusMinus")
CAST_EXPRESSION__NOT_PLUS_MINUS__REF_AND_BOUNDS__NAME = hydra.core.Name("refAndBounds")
CAST_EXPRESSION__NOT_PLUS_MINUS__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class CastExpression_Lambda:
    ref_and_bounds: CastExpression_RefAndBounds
    expression: LambdaExpression

CAST_EXPRESSION__LAMBDA__NAME = hydra.core.Name("hydra.ext.java.syntax.CastExpression_Lambda")
CAST_EXPRESSION__LAMBDA__REF_AND_BOUNDS__NAME = hydra.core.Name("refAndBounds")
CAST_EXPRESSION__LAMBDA__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class CastExpression_RefAndBounds:
    type: ReferenceType
    bounds: frozenlist[AdditionalBound]

CAST_EXPRESSION__REF_AND_BOUNDS__NAME = hydra.core.Name("hydra.ext.java.syntax.CastExpression_RefAndBounds")
CAST_EXPRESSION__REF_AND_BOUNDS__TYPE__NAME = hydra.core.Name("type")
CAST_EXPRESSION__REF_AND_BOUNDS__BOUNDS__NAME = hydra.core.Name("bounds")

class ConstantExpression(Node["Expression"]):
...

CONSTANT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.java.syntax.ConstantExpression")
