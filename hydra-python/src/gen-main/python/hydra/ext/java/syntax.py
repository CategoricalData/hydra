# Note: this is an automatically generated file. Do not edit.

r"""A Java syntax module. Based on the Oracle Java SE 12 BNF:
  https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html
Note: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class Identifier(Node[str]):
    ...

Identifier.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Identifier")

class TypeIdentifier(Node["Identifier"]):
    ...

TypeIdentifier.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeIdentifier")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Literal")
    NULL = hydra.core.Name("null")
    INTEGER = hydra.core.Name("integer")
    FLOATING_POINT = hydra.core.Name("floatingPoint")
    BOOLEAN = hydra.core.Name("boolean")
    CHARACTER = hydra.core.Name("character")
    STRING = hydra.core.Name("string")

class IntegerLiteral(Node[int]):
    r"""Note: this is an approximation which ignores encoding."""

IntegerLiteral.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.IntegerLiteral")

class FloatingPointLiteral(Node[Decimal]):
    r"""Note: this is an approximation which ignores encoding."""

FloatingPointLiteral.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FloatingPointLiteral")

class StringLiteral(Node[str]):
    r"""Note: this is an approximation which ignores encoding."""

StringLiteral.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.StringLiteral")

class TypePrimitive(Node["PrimitiveTypeWithAnnotations"]):
    ...

class TypeReference(Node["ReferenceType"]):
    ...

class _TypeMeta(type):
    def __getitem__(cls, item):
        return object

class Type(metaclass=_TypeMeta):
    r"""TypePrimitive | TypeReference"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Type")
    PRIMITIVE = hydra.core.Name("primitive")
    REFERENCE = hydra.core.Name("reference")

@dataclass(frozen=True)
class PrimitiveTypeWithAnnotations:
    type: PrimitiveType
    annotations: frozenlist[Annotation]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PrimitiveTypeWithAnnotations")
    TYPE = hydra.core.Name("type")
    ANNOTATIONS = hydra.core.Name("annotations")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PrimitiveType")
    NUMERIC = hydra.core.Name("numeric")
    BOOLEAN = hydra.core.Name("boolean")

class NumericTypeIntegral(Node["IntegralType"]):
    ...

class NumericTypeFloatingPoint(Node["FloatingPointType"]):
    ...

class _NumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NumericType(metaclass=_NumericTypeMeta):
    r"""NumericTypeIntegral | NumericTypeFloatingPoint"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.NumericType")
    INTEGRAL = hydra.core.Name("integral")
    FLOATING_POINT = hydra.core.Name("floatingPoint")

class IntegralType(Enum):
    BYTE = hydra.core.Name("byte")

    SHORT = hydra.core.Name("short")

    INT = hydra.core.Name("int")

    LONG = hydra.core.Name("long")

    CHAR = hydra.core.Name("char")

IntegralType.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.IntegralType")

class FloatingPointType(Enum):
    FLOAT = hydra.core.Name("float")

    DOUBLE = hydra.core.Name("double")

FloatingPointType.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FloatingPointType")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ReferenceType")
    CLASS_OR_INTERFACE = hydra.core.Name("classOrInterface")
    VARIABLE = hydra.core.Name("variable")
    ARRAY = hydra.core.Name("array")

class ClassOrInterfaceTypeClass(Node["ClassType"]):
    ...

class ClassOrInterfaceTypeInterface(Node["InterfaceType"]):
    ...

class _ClassOrInterfaceTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ClassOrInterfaceType(metaclass=_ClassOrInterfaceTypeMeta):
    r"""ClassOrInterfaceTypeClass | ClassOrInterfaceTypeInterface"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceType")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")

@dataclass(frozen=True)
class ClassType:
    annotations: frozenlist[Annotation]
    qualifier: ClassTypeQualifier
    identifier: TypeIdentifier
    arguments: frozenlist[TypeArgument]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassType")
    ANNOTATIONS = hydra.core.Name("annotations")
    QUALIFIER = hydra.core.Name("qualifier")
    IDENTIFIER = hydra.core.Name("identifier")
    ARGUMENTS = hydra.core.Name("arguments")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassTypeQualifier")
    NONE = hydra.core.Name("none")
    PACKAGE = hydra.core.Name("package")
    PARENT = hydra.core.Name("parent")

class InterfaceType(Node["ClassType"]):
    ...

InterfaceType.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceType")

@dataclass(frozen=True)
class TypeVariable:
    annotations: frozenlist[Annotation]
    identifier: TypeIdentifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeVariable")
    ANNOTATIONS = hydra.core.Name("annotations")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class ArrayType:
    dims: Dims
    variant: ArrayType_Variant

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayType")
    DIMS = hydra.core.Name("dims")
    VARIANT = hydra.core.Name("variant")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayType_Variant")
    PRIMITIVE = hydra.core.Name("primitive")
    CLASS_OR_INTERFACE = hydra.core.Name("classOrInterface")
    VARIABLE = hydra.core.Name("variable")

class Dims(Node["frozenlist[frozenlist[Annotation]]"]):
    ...

Dims.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Dims")

@dataclass(frozen=True)
class TypeParameter:
    modifiers: frozenlist[TypeParameterModifier]
    identifier: TypeIdentifier
    bound: Maybe[TypeBound]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeParameter")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIER = hydra.core.Name("identifier")
    BOUND = hydra.core.Name("bound")

class TypeParameterModifier(Node["Annotation"]):
    ...

TypeParameterModifier.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeParameterModifier")

class TypeBoundVariable(Node["TypeVariable"]):
    ...

class TypeBoundClassOrInterface(Node["TypeBound_ClassOrInterface"]):
    ...

class _TypeBoundMeta(type):
    def __getitem__(cls, item):
        return object

class TypeBound(metaclass=_TypeBoundMeta):
    r"""TypeBoundVariable | TypeBoundClassOrInterface"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeBound")
    VARIABLE = hydra.core.Name("variable")
    CLASS_OR_INTERFACE = hydra.core.Name("classOrInterface")

@dataclass(frozen=True)
class TypeBound_ClassOrInterface:
    type: ClassOrInterfaceType
    additional: frozenlist[AdditionalBound]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeBound_ClassOrInterface")
    TYPE = hydra.core.Name("type")
    ADDITIONAL = hydra.core.Name("additional")

class AdditionalBound(Node["InterfaceType"]):
    ...

AdditionalBound.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AdditionalBound")

class TypeArgumentReference(Node["ReferenceType"]):
    ...

class TypeArgumentWildcard(Node["Wildcard"]):
    ...

class _TypeArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TypeArgument(metaclass=_TypeArgumentMeta):
    r"""TypeArgumentReference | TypeArgumentWildcard"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeArgument")
    REFERENCE = hydra.core.Name("reference")
    WILDCARD = hydra.core.Name("wildcard")

@dataclass(frozen=True)
class Wildcard:
    annotations: frozenlist[Annotation]
    wildcard: Maybe[WildcardBounds]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Wildcard")
    ANNOTATIONS = hydra.core.Name("annotations")
    WILDCARD = hydra.core.Name("wildcard")

class WildcardBoundsExtends(Node["ReferenceType"]):
    ...

class WildcardBoundsSuper(Node["ReferenceType"]):
    ...

class _WildcardBoundsMeta(type):
    def __getitem__(cls, item):
        return object

class WildcardBounds(metaclass=_WildcardBoundsMeta):
    r"""WildcardBoundsExtends | WildcardBoundsSuper"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.WildcardBounds")
    EXTENDS = hydra.core.Name("extends")
    SUPER = hydra.core.Name("super")

@dataclass(frozen=True)
class ModuleName:
    identifier: Identifier
    name: Maybe[ModuleName]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModuleName")
    IDENTIFIER = hydra.core.Name("identifier")
    NAME = hydra.core.Name("name")

class PackageName(Node["frozenlist[Identifier]"]):
    ...

PackageName.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PackageName")

@dataclass(frozen=True)
class TypeName:
    identifier: TypeIdentifier
    qualifier: Maybe[PackageOrTypeName]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeName")
    IDENTIFIER = hydra.core.Name("identifier")
    QUALIFIER = hydra.core.Name("qualifier")

@dataclass(frozen=True)
class ExpressionName:
    qualifier: Maybe[AmbiguousName]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ExpressionName")
    QUALIFIER = hydra.core.Name("qualifier")
    IDENTIFIER = hydra.core.Name("identifier")

class MethodName(Node["Identifier"]):
    ...

MethodName.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodName")

class PackageOrTypeName(Node["frozenlist[Identifier]"]):
    ...

PackageOrTypeName.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PackageOrTypeName")

class AmbiguousName(Node["frozenlist[Identifier]"]):
    ...

AmbiguousName.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AmbiguousName")

class CompilationUnitOrdinary(Node["OrdinaryCompilationUnit"]):
    ...

class CompilationUnitModular(Node["ModularCompilationUnit"]):
    ...

class _CompilationUnitMeta(type):
    def __getitem__(cls, item):
        return object

class CompilationUnit(metaclass=_CompilationUnitMeta):
    r"""CompilationUnitOrdinary | CompilationUnitModular"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CompilationUnit")
    ORDINARY = hydra.core.Name("ordinary")
    MODULAR = hydra.core.Name("modular")

@dataclass(frozen=True)
class OrdinaryCompilationUnit:
    package: Maybe[PackageDeclaration]
    imports: frozenlist[ImportDeclaration]
    types: frozenlist[TypeDeclarationWithComments]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.OrdinaryCompilationUnit")
    PACKAGE = hydra.core.Name("package")
    IMPORTS = hydra.core.Name("imports")
    TYPES = hydra.core.Name("types")

@dataclass(frozen=True)
class ModularCompilationUnit:
    imports: frozenlist[ImportDeclaration]
    module: ModuleDeclaration

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModularCompilationUnit")
    IMPORTS = hydra.core.Name("imports")
    MODULE = hydra.core.Name("module")

@dataclass(frozen=True)
class PackageDeclaration:
    modifiers: frozenlist[PackageModifier]
    identifiers: frozenlist[Identifier]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PackageDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIERS = hydra.core.Name("identifiers")

class PackageModifier(Node["Annotation"]):
    ...

PackageModifier.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PackageModifier")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ImportDeclaration")
    SINGLE_TYPE = hydra.core.Name("singleType")
    TYPE_IMPORT_ON_DEMAND = hydra.core.Name("typeImportOnDemand")
    SINGLE_STATIC_IMPORT = hydra.core.Name("singleStaticImport")
    STATIC_IMPORT_ON_DEMAND = hydra.core.Name("staticImportOnDemand")

class SingleTypeImportDeclaration(Node["TypeName"]):
    ...

SingleTypeImportDeclaration.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SingleTypeImportDeclaration")

class TypeImportOnDemandDeclaration(Node["PackageOrTypeName"]):
    ...

TypeImportOnDemandDeclaration.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeImportOnDemandDeclaration")

@dataclass(frozen=True)
class SingleStaticImportDeclaration:
    type_name: TypeName
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SingleStaticImportDeclaration")
    TYPE_NAME = hydra.core.Name("typeName")
    IDENTIFIER = hydra.core.Name("identifier")

class StaticImportOnDemandDeclaration(Node["TypeName"]):
    ...

StaticImportOnDemandDeclaration.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.StaticImportOnDemandDeclaration")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeDeclaration")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")
    NONE = hydra.core.Name("none")

@dataclass(frozen=True)
class TypeDeclarationWithComments:
    value: TypeDeclaration
    comments: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeDeclarationWithComments")
    VALUE = hydra.core.Name("value")
    COMMENTS = hydra.core.Name("comments")

@dataclass(frozen=True)
class ModuleDeclaration:
    annotations: frozenlist[Annotation]
    open: bool
    identifiers: frozenlist[Identifier]
    directives: frozenlist[frozenlist[ModuleDirective]]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModuleDeclaration")
    ANNOTATIONS = hydra.core.Name("annotations")
    OPEN = hydra.core.Name("open")
    IDENTIFIERS = hydra.core.Name("identifiers")
    DIRECTIVES = hydra.core.Name("directives")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective")
    REQUIRES = hydra.core.Name("requires")
    EXPORTS = hydra.core.Name("exports")
    OPENS = hydra.core.Name("opens")
    USES = hydra.core.Name("uses")
    PROVIDES = hydra.core.Name("provides")

@dataclass(frozen=True)
class ModuleDirective_Requires:
    modifiers: frozenlist[RequiresModifier]
    module: ModuleName

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_Requires")
    MODIFIERS = hydra.core.Name("modifiers")
    MODULE = hydra.core.Name("module")

@dataclass(frozen=True)
class ModuleDirective_ExportsOrOpens:
    package: PackageName
    modules: Annotated[frozenlist[ModuleName], "At least one module"]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens")
    PACKAGE = hydra.core.Name("package")
    MODULES = hydra.core.Name("modules")

@dataclass(frozen=True)
class ModuleDirective_Provides:
    to: TypeName
    with_: Annotated[frozenlist[TypeName], "At least one type"]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ModuleDirective_Provides")
    TO = hydra.core.Name("to")
    WITH = hydra.core.Name("with")

class RequiresModifier(Enum):
    TRANSITIVE = hydra.core.Name("transitive")

    STATIC = hydra.core.Name("static")

RequiresModifier.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RequiresModifier")

class ClassDeclarationNormal(Node["NormalClassDeclaration"]):
    ...

class ClassDeclarationEnum(Node["EnumDeclaration"]):
    ...

class _ClassDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class ClassDeclaration(metaclass=_ClassDeclarationMeta):
    r"""ClassDeclarationNormal | ClassDeclarationEnum"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassDeclaration")
    NORMAL = hydra.core.Name("normal")
    ENUM = hydra.core.Name("enum")

@dataclass(frozen=True)
class NormalClassDeclaration:
    modifiers: frozenlist[ClassModifier]
    identifier: TypeIdentifier
    parameters: frozenlist[TypeParameter]
    extends: Maybe[ClassType]
    implements: frozenlist[InterfaceType]
    body: ClassBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.NormalClassDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIER = hydra.core.Name("identifier")
    PARAMETERS = hydra.core.Name("parameters")
    EXTENDS = hydra.core.Name("extends")
    IMPLEMENTS = hydra.core.Name("implements")
    BODY = hydra.core.Name("body")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    PROTECTED = hydra.core.Name("protected")
    PRIVATE = hydra.core.Name("private")
    ABSTRACT = hydra.core.Name("abstract")
    STATIC = hydra.core.Name("static")
    FINAL = hydra.core.Name("final")
    STRICTFP = hydra.core.Name("strictfp")

class ClassBody(Node["frozenlist[ClassBodyDeclarationWithComments]"]):
    ...

ClassBody.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassBody")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassBodyDeclaration")
    CLASS_MEMBER = hydra.core.Name("classMember")
    INSTANCE_INITIALIZER = hydra.core.Name("instanceInitializer")
    STATIC_INITIALIZER = hydra.core.Name("staticInitializer")
    CONSTRUCTOR_DECLARATION = hydra.core.Name("constructorDeclaration")

@dataclass(frozen=True)
class ClassBodyDeclarationWithComments:
    value: ClassBodyDeclaration
    comments: Maybe[str]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassBodyDeclarationWithComments")
    VALUE = hydra.core.Name("value")
    COMMENTS = hydra.core.Name("comments")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassMemberDeclaration")
    FIELD = hydra.core.Name("field")
    METHOD = hydra.core.Name("method")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")
    NONE = hydra.core.Name("none")

@dataclass(frozen=True)
class FieldDeclaration:
    modifiers: frozenlist[FieldModifier]
    unann_type: UnannType
    variable_declarators: frozenlist[VariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FieldDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    UNANN_TYPE = hydra.core.Name("unannType")
    VARIABLE_DECLARATORS = hydra.core.Name("variableDeclarators")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FieldModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    PROTECTED = hydra.core.Name("protected")
    PRIVATE = hydra.core.Name("private")
    STATIC = hydra.core.Name("static")
    FINAL = hydra.core.Name("final")
    TRANSIENT = hydra.core.Name("transient")
    VOLATILE = hydra.core.Name("volatile")

@dataclass(frozen=True)
class VariableDeclarator:
    id: VariableDeclaratorId
    initializer: Maybe[VariableInitializer]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.VariableDeclarator")
    ID = hydra.core.Name("id")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class VariableDeclaratorId:
    identifier: Identifier
    dims: Maybe[Dims]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.VariableDeclaratorId")
    IDENTIFIER = hydra.core.Name("identifier")
    DIMS = hydra.core.Name("dims")

class VariableInitializerExpression(Node["Expression"]):
    ...

class VariableInitializerArrayInitializer(Node["ArrayInitializer"]):
    ...

class _VariableInitializerMeta(type):
    def __getitem__(cls, item):
        return object

class VariableInitializer(metaclass=_VariableInitializerMeta):
    r"""VariableInitializerExpression | VariableInitializerArrayInitializer"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.VariableInitializer")
    EXPRESSION = hydra.core.Name("expression")
    ARRAY_INITIALIZER = hydra.core.Name("arrayInitializer")

class UnannType(Node["Type"]):
    r"""A Type which does not allow annotations."""

UnannType.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.UnannType")

class UnannClassType(Node["ClassType"]):
    r"""A ClassType which does not allow annotations."""

UnannClassType.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.UnannClassType")

@dataclass(frozen=True)
class MethodDeclaration:
    annotations: Annotated[frozenlist[Annotation], "Note: simple methods cannot have annotations"]
    modifiers: frozenlist[MethodModifier]
    header: MethodHeader
    body: MethodBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodDeclaration")
    ANNOTATIONS = hydra.core.Name("annotations")
    MODIFIERS = hydra.core.Name("modifiers")
    HEADER = hydra.core.Name("header")
    BODY = hydra.core.Name("body")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    PROTECTED = hydra.core.Name("protected")
    PRIVATE = hydra.core.Name("private")
    ABSTRACT = hydra.core.Name("abstract")
    STATIC = hydra.core.Name("static")
    FINAL = hydra.core.Name("final")
    SYNCHRONIZED = hydra.core.Name("synchronized")
    NATIVE = hydra.core.Name("native")
    STRICTFB = hydra.core.Name("strictfb")

@dataclass(frozen=True)
class MethodHeader:
    parameters: frozenlist[TypeParameter]
    result: Result
    declarator: MethodDeclarator
    throws: Maybe[Throws]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodHeader")
    PARAMETERS = hydra.core.Name("parameters")
    RESULT = hydra.core.Name("result")
    DECLARATOR = hydra.core.Name("declarator")
    THROWS = hydra.core.Name("throws")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Result")
    TYPE = hydra.core.Name("type")
    VOID = hydra.core.Name("void")

@dataclass(frozen=True)
class MethodDeclarator:
    identifier: Identifier
    receiver_parameter: Maybe[ReceiverParameter]
    formal_parameters: frozenlist[FormalParameter]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodDeclarator")
    IDENTIFIER = hydra.core.Name("identifier")
    RECEIVER_PARAMETER = hydra.core.Name("receiverParameter")
    FORMAL_PARAMETERS = hydra.core.Name("formalParameters")

@dataclass(frozen=True)
class ReceiverParameter:
    annotations: frozenlist[Annotation]
    unann_type: UnannType
    identifier: Maybe[Identifier]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ReceiverParameter")
    ANNOTATIONS = hydra.core.Name("annotations")
    UNANN_TYPE = hydra.core.Name("unannType")
    IDENTIFIER = hydra.core.Name("identifier")

class FormalParameterSimple(Node["FormalParameter_Simple"]):
    ...

class FormalParameterVariableArity(Node["VariableArityParameter"]):
    ...

class _FormalParameterMeta(type):
    def __getitem__(cls, item):
        return object

class FormalParameter(metaclass=_FormalParameterMeta):
    r"""FormalParameterSimple | FormalParameterVariableArity"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FormalParameter")
    SIMPLE = hydra.core.Name("simple")
    VARIABLE_ARITY = hydra.core.Name("variableArity")

@dataclass(frozen=True)
class FormalParameter_Simple:
    modifiers: frozenlist[VariableModifier]
    type: UnannType
    id: VariableDeclaratorId

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FormalParameter_Simple")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    ID = hydra.core.Name("id")

@dataclass(frozen=True)
class VariableArityParameter:
    modifiers: VariableModifier
    type: UnannType
    annotations: frozenlist[Annotation]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.VariableArityParameter")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    ANNOTATIONS = hydra.core.Name("annotations")
    IDENTIFIER = hydra.core.Name("identifier")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.VariableModifier")
    ANNOTATION = hydra.core.Name("annotation")
    FINAL = hydra.core.Name("final")

class Throws(Node["frozenlist[ExceptionType]"]):
    ...

Throws.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Throws")

class ExceptionTypeClass(Node["ClassType"]):
    ...

class ExceptionTypeVariable(Node["TypeVariable"]):
    ...

class _ExceptionTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ExceptionType(metaclass=_ExceptionTypeMeta):
    r"""ExceptionTypeClass | ExceptionTypeVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ExceptionType")
    CLASS = hydra.core.Name("class")
    VARIABLE = hydra.core.Name("variable")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodBody")
    BLOCK = hydra.core.Name("block")
    NONE = hydra.core.Name("none")

class InstanceInitializer(Node["Block"]):
    ...

InstanceInitializer.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InstanceInitializer")

class StaticInitializer(Node["Block"]):
    ...

StaticInitializer.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.StaticInitializer")

@dataclass(frozen=True)
class ConstructorDeclaration:
    modifiers: frozenlist[ConstructorModifier]
    constructor: ConstructorDeclarator
    throws: Maybe[Throws]
    body: ConstructorBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    CONSTRUCTOR = hydra.core.Name("constructor")
    THROWS = hydra.core.Name("throws")
    BODY = hydra.core.Name("body")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstructorModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    PROTECTED = hydra.core.Name("protected")
    PRIVATE = hydra.core.Name("private")

@dataclass(frozen=True)
class ConstructorDeclarator:
    parameters: frozenlist[TypeParameter]
    name: SimpleTypeName
    receiver_parameter: Maybe[ReceiverParameter]
    formal_parameters: frozenlist[FormalParameter]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstructorDeclarator")
    PARAMETERS = hydra.core.Name("parameters")
    NAME = hydra.core.Name("name")
    RECEIVER_PARAMETER = hydra.core.Name("receiverParameter")
    FORMAL_PARAMETERS = hydra.core.Name("formalParameters")

class SimpleTypeName(Node["TypeIdentifier"]):
    ...

SimpleTypeName.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SimpleTypeName")

@dataclass(frozen=True)
class ConstructorBody:
    invocation: Maybe[ExplicitConstructorInvocation]
    statements: frozenlist[BlockStatement]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstructorBody")
    INVOCATION = hydra.core.Name("invocation")
    STATEMENTS = hydra.core.Name("statements")

@dataclass(frozen=True)
class ExplicitConstructorInvocation:
    type_arguments: frozenlist[TypeArgument]
    arguments: frozenlist[Expression]
    variant: ExplicitConstructorInvocation_Variant

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ExplicitConstructorInvocation")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    ARGUMENTS = hydra.core.Name("arguments")
    VARIANT = hydra.core.Name("variant")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ExplicitConstructorInvocation_Variant")
    THIS = hydra.core.Name("this")
    SUPER = hydra.core.Name("super")
    PRIMARY = hydra.core.Name("primary")

@dataclass(frozen=True)
class EnumDeclaration:
    modifiers: frozenlist[ClassModifier]
    identifier: TypeIdentifier
    implements: frozenlist[InterfaceType]
    body: EnumBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnumDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIER = hydra.core.Name("identifier")
    IMPLEMENTS = hydra.core.Name("implements")
    BODY = hydra.core.Name("body")

class EnumBody(Node["frozenlist[EnumBody_Element]"]):
    ...

EnumBody.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnumBody")

@dataclass(frozen=True)
class EnumBody_Element:
    constants: frozenlist[EnumConstant]
    body_declarations: frozenlist[ClassBodyDeclaration]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnumBody_Element")
    CONSTANTS = hydra.core.Name("constants")
    BODY_DECLARATIONS = hydra.core.Name("bodyDeclarations")

@dataclass(frozen=True)
class EnumConstant:
    modifiers: frozenlist[EnumConstantModifier]
    identifier: Identifier
    arguments: frozenlist[frozenlist[Expression]]
    body: Maybe[ClassBody]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnumConstant")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIER = hydra.core.Name("identifier")
    ARGUMENTS = hydra.core.Name("arguments")
    BODY = hydra.core.Name("body")

class EnumConstantModifier(Node["Annotation"]):
    ...

EnumConstantModifier.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnumConstantModifier")

class InterfaceDeclarationNormalInterface(Node["NormalInterfaceDeclaration"]):
    ...

class InterfaceDeclarationAnnotationType(Node["AnnotationTypeDeclaration"]):
    ...

class _InterfaceDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

class InterfaceDeclaration(metaclass=_InterfaceDeclarationMeta):
    r"""InterfaceDeclarationNormalInterface | InterfaceDeclarationAnnotationType"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceDeclaration")
    NORMAL_INTERFACE = hydra.core.Name("normalInterface")
    ANNOTATION_TYPE = hydra.core.Name("annotationType")

@dataclass(frozen=True)
class NormalInterfaceDeclaration:
    modifiers: frozenlist[InterfaceModifier]
    identifier: TypeIdentifier
    parameters: frozenlist[TypeParameter]
    extends: frozenlist[InterfaceType]
    body: InterfaceBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.NormalInterfaceDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIER = hydra.core.Name("identifier")
    PARAMETERS = hydra.core.Name("parameters")
    EXTENDS = hydra.core.Name("extends")
    BODY = hydra.core.Name("body")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    PROTECTED = hydra.core.Name("protected")
    PRIVATE = hydra.core.Name("private")
    ABSTRACT = hydra.core.Name("abstract")
    STATIC = hydra.core.Name("static")
    STRICTFB = hydra.core.Name("strictfb")

class InterfaceBody(Node["frozenlist[InterfaceMemberDeclaration]"]):
    ...

InterfaceBody.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceBody")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceMemberDeclaration")
    CONSTANT = hydra.core.Name("constant")
    INTERFACE_METHOD = hydra.core.Name("interfaceMethod")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")

@dataclass(frozen=True)
class ConstantDeclaration:
    modifiers: frozenlist[ConstantModifier]
    type: UnannType
    variables: frozenlist[VariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstantDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    VARIABLES = hydra.core.Name("variables")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstantModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    STATIC = hydra.core.Name("static")
    FINAL = hydra.core.Name("final")

@dataclass(frozen=True)
class InterfaceMethodDeclaration:
    modifiers: frozenlist[InterfaceMethodModifier]
    header: MethodHeader
    body: MethodBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceMethodDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    HEADER = hydra.core.Name("header")
    BODY = hydra.core.Name("body")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InterfaceMethodModifier")
    ANNOTATION = hydra.core.Name("annotation")
    PUBLIC = hydra.core.Name("public")
    PRIVATE = hydra.core.Name("private")
    ABSTRACT = hydra.core.Name("abstract")
    DEFAULT = hydra.core.Name("default")
    STATIC = hydra.core.Name("static")
    STRICTFP = hydra.core.Name("strictfp")

@dataclass(frozen=True)
class AnnotationTypeDeclaration:
    modifiers: frozenlist[InterfaceModifier]
    identifier: TypeIdentifier
    body: AnnotationTypeBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    IDENTIFIER = hydra.core.Name("identifier")
    BODY = hydra.core.Name("body")

class AnnotationTypeBody(Node["frozenlist[frozenlist[AnnotationTypeMemberDeclaration]]"]):
    ...

AnnotationTypeBody.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeBody")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeMemberDeclaration")
    ANNOTATION_TYPE = hydra.core.Name("annotationType")
    CONSTANT = hydra.core.Name("constant")
    CLASS = hydra.core.Name("class")
    INTERFACE = hydra.core.Name("interface")

@dataclass(frozen=True)
class AnnotationTypeElementDeclaration:
    modifiers: frozenlist[AnnotationTypeElementModifier]
    type: UnannType
    identifier: Identifier
    dims: Maybe[Dims]
    default: Maybe[DefaultValue]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeElementDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")
    DIMS = hydra.core.Name("dims")
    DEFAULT = hydra.core.Name("default")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AnnotationTypeElementModifier")
    PUBLIC = hydra.core.Name("public")
    ABSTRACT = hydra.core.Name("abstract")

class DefaultValue(Node["ElementValue"]):
    ...

DefaultValue.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.DefaultValue")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Annotation")
    NORMAL = hydra.core.Name("normal")
    MARKER = hydra.core.Name("marker")
    SINGLE_ELEMENT = hydra.core.Name("singleElement")

@dataclass(frozen=True)
class NormalAnnotation:
    type_name: TypeName
    pairs: frozenlist[ElementValuePair]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.NormalAnnotation")
    TYPE_NAME = hydra.core.Name("typeName")
    PAIRS = hydra.core.Name("pairs")

@dataclass(frozen=True)
class ElementValuePair:
    key: Identifier
    value: ElementValue

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ElementValuePair")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ElementValue")
    CONDITIONAL_EXPRESSION = hydra.core.Name("conditionalExpression")
    ELEMENT_VALUE_ARRAY_INITIALIZER = hydra.core.Name("elementValueArrayInitializer")
    ANNOTATION = hydra.core.Name("annotation")

class ElementValueArrayInitializer(Node["frozenlist[ElementValue]"]):
    ...

ElementValueArrayInitializer.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ElementValueArrayInitializer")

class MarkerAnnotation(Node["TypeName"]):
    ...

MarkerAnnotation.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MarkerAnnotation")

@dataclass(frozen=True)
class SingleElementAnnotation:
    name: TypeName
    value: Maybe[ElementValue]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SingleElementAnnotation")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class ArrayInitializer(Node["frozenlist[frozenlist[VariableInitializer]]"]):
    ...

ArrayInitializer.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayInitializer")

class Block(Node["frozenlist[BlockStatement]"]):
    ...

Block.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Block")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.BlockStatement")
    LOCAL_VARIABLE_DECLARATION = hydra.core.Name("localVariableDeclaration")
    CLASS = hydra.core.Name("class")
    STATEMENT = hydra.core.Name("statement")

class LocalVariableDeclarationStatement(Node["LocalVariableDeclaration"]):
    ...

LocalVariableDeclarationStatement.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LocalVariableDeclarationStatement")

@dataclass(frozen=True)
class LocalVariableDeclaration:
    modifiers: frozenlist[VariableModifier]
    type: LocalVariableType
    declarators: frozenlist[VariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LocalVariableDeclaration")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    DECLARATORS = hydra.core.Name("declarators")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LocalVariableType")
    TYPE = hydra.core.Name("type")
    VAR = hydra.core.Name("var")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Statement")
    WITHOUT_TRAILING = hydra.core.Name("withoutTrailing")
    LABELED = hydra.core.Name("labeled")
    IF_THEN = hydra.core.Name("ifThen")
    IF_THEN_ELSE = hydra.core.Name("ifThenElse")
    WHILE = hydra.core.Name("while")
    FOR = hydra.core.Name("for")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.StatementNoShortIf")
    WITHOUT_TRAILING = hydra.core.Name("withoutTrailing")
    LABELED = hydra.core.Name("labeled")
    IF_THEN_ELSE = hydra.core.Name("ifThenElse")
    WHILE = hydra.core.Name("while")
    FOR = hydra.core.Name("for")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.StatementWithoutTrailingSubstatement")
    BLOCK = hydra.core.Name("block")
    EMPTY = hydra.core.Name("empty")
    EXPRESSION = hydra.core.Name("expression")
    ASSERT = hydra.core.Name("assert")
    SWITCH = hydra.core.Name("switch")
    DO = hydra.core.Name("do")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    RETURN = hydra.core.Name("return")
    SYNCHRONIZED = hydra.core.Name("synchronized")
    THROW = hydra.core.Name("throw")
    TRY = hydra.core.Name("try")

@dataclass(frozen=True)
class LabeledStatement:
    identifier: Identifier
    statement: Statement

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LabeledStatement")
    IDENTIFIER = hydra.core.Name("identifier")
    STATEMENT = hydra.core.Name("statement")

@dataclass(frozen=True)
class LabeledStatementNoShortIf:
    identifier: Identifier
    statement: StatementNoShortIf

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LabeledStatementNoShortIf")
    IDENTIFIER = hydra.core.Name("identifier")
    STATEMENT = hydra.core.Name("statement")

class ExpressionStatement(Node["StatementExpression"]):
    ...

ExpressionStatement.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ExpressionStatement")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.StatementExpression")
    ASSIGNMENT = hydra.core.Name("assignment")
    PRE_INCREMENT = hydra.core.Name("preIncrement")
    PRE_DECREMENT = hydra.core.Name("preDecrement")
    POST_INCREMENT = hydra.core.Name("postIncrement")
    POST_DECREMENT = hydra.core.Name("postDecrement")
    METHOD_INVOCATION = hydra.core.Name("methodInvocation")
    CLASS_INSTANCE_CREATION = hydra.core.Name("classInstanceCreation")

@dataclass(frozen=True)
class IfThenStatement:
    expression: Expression
    statement: Statement

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.IfThenStatement")
    EXPRESSION = hydra.core.Name("expression")
    STATEMENT = hydra.core.Name("statement")

@dataclass(frozen=True)
class IfThenElseStatement:
    cond: Maybe[Expression]
    then: StatementNoShortIf
    else_: Statement

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatement")
    COND = hydra.core.Name("cond")
    THEN = hydra.core.Name("then")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class IfThenElseStatementNoShortIf:
    cond: Maybe[Expression]
    then: StatementNoShortIf
    else_: StatementNoShortIf

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.IfThenElseStatementNoShortIf")
    COND = hydra.core.Name("cond")
    THEN = hydra.core.Name("then")
    ELSE = hydra.core.Name("else")

class AssertStatementSingle(Node["Expression"]):
    ...

class AssertStatementPair(Node["AssertStatement_Pair"]):
    ...

class _AssertStatementMeta(type):
    def __getitem__(cls, item):
        return object

class AssertStatement(metaclass=_AssertStatementMeta):
    r"""AssertStatementSingle | AssertStatementPair"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AssertStatement")
    SINGLE = hydra.core.Name("single")
    PAIR = hydra.core.Name("pair")

@dataclass(frozen=True)
class AssertStatement_Pair:
    first: Expression
    second: Expression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AssertStatement_Pair")
    FIRST = hydra.core.Name("first")
    SECOND = hydra.core.Name("second")

@dataclass(frozen=True)
class SwitchStatement:
    cond: Expression
    block: SwitchBlock

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SwitchStatement")
    COND = hydra.core.Name("cond")
    BLOCK = hydra.core.Name("block")

class SwitchBlock(Node["frozenlist[SwitchBlock_Pair]"]):
    ...

SwitchBlock.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SwitchBlock")

@dataclass(frozen=True)
class SwitchBlock_Pair:
    statements: frozenlist[SwitchBlockStatementGroup]
    labels: frozenlist[SwitchLabel]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SwitchBlock_Pair")
    STATEMENTS = hydra.core.Name("statements")
    LABELS = hydra.core.Name("labels")

@dataclass(frozen=True)
class SwitchBlockStatementGroup:
    labels: frozenlist[SwitchLabel]
    statements: frozenlist[BlockStatement]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SwitchBlockStatementGroup")
    LABELS = hydra.core.Name("labels")
    STATEMENTS = hydra.core.Name("statements")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SwitchLabel")
    CONSTANT = hydra.core.Name("constant")
    ENUM_CONSTANT = hydra.core.Name("enumConstant")
    DEFAULT = hydra.core.Name("default")

class EnumConstantName(Node["Identifier"]):
    ...

EnumConstantName.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnumConstantName")

@dataclass(frozen=True)
class WhileStatement:
    cond: Maybe[Expression]
    body: Statement

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.WhileStatement")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class WhileStatementNoShortIf:
    cond: Maybe[Expression]
    body: StatementNoShortIf

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.WhileStatementNoShortIf")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class DoStatement:
    body: Statement
    conde: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.DoStatement")
    BODY = hydra.core.Name("body")
    CONDE = hydra.core.Name("conde")

class ForStatementBasic(Node["BasicForStatement"]):
    ...

class ForStatementEnhanced(Node["EnhancedForStatement"]):
    ...

class _ForStatementMeta(type):
    def __getitem__(cls, item):
        return object

class ForStatement(metaclass=_ForStatementMeta):
    r"""ForStatementBasic | ForStatementEnhanced"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ForStatement")
    BASIC = hydra.core.Name("basic")
    ENHANCED = hydra.core.Name("enhanced")

class ForStatementNoShortIfBasic(Node["BasicForStatementNoShortIf"]):
    ...

class ForStatementNoShortIfEnhanced(Node["EnhancedForStatementNoShortIf"]):
    ...

class _ForStatementNoShortIfMeta(type):
    def __getitem__(cls, item):
        return object

class ForStatementNoShortIf(metaclass=_ForStatementNoShortIfMeta):
    r"""ForStatementNoShortIfBasic | ForStatementNoShortIfEnhanced"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ForStatementNoShortIf")
    BASIC = hydra.core.Name("basic")
    ENHANCED = hydra.core.Name("enhanced")

@dataclass(frozen=True)
class BasicForStatement:
    cond: ForCond
    body: Statement

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.BasicForStatement")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class ForCond:
    init: Maybe[ForInit]
    cond: Maybe[Expression]
    update: Maybe[ForUpdate]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ForCond")
    INIT = hydra.core.Name("init")
    COND = hydra.core.Name("cond")
    UPDATE = hydra.core.Name("update")

@dataclass(frozen=True)
class BasicForStatementNoShortIf:
    cond: ForCond
    body: StatementNoShortIf

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.BasicForStatementNoShortIf")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

class ForInitStatements(Node["frozenlist[StatementExpression]"]):
    ...

class ForInitLocalVariable(Node["LocalVariableDeclaration"]):
    ...

class _ForInitMeta(type):
    def __getitem__(cls, item):
        return object

class ForInit(metaclass=_ForInitMeta):
    r"""ForInitStatements | ForInitLocalVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ForInit")
    STATEMENTS = hydra.core.Name("statements")
    LOCAL_VARIABLE = hydra.core.Name("localVariable")

class ForUpdate(Node["frozenlist[StatementExpression]"]):
    ...

ForUpdate.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ForUpdate")

@dataclass(frozen=True)
class EnhancedForStatement:
    cond: EnhancedForCond
    body: Statement

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnhancedForStatement")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class EnhancedForCond:
    modifiers: frozenlist[VariableModifier]
    type: LocalVariableType
    id: VariableDeclaratorId
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnhancedForCond")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    ID = hydra.core.Name("id")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class EnhancedForStatementNoShortIf:
    cond: EnhancedForCond
    body: StatementNoShortIf

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EnhancedForStatementNoShortIf")
    COND = hydra.core.Name("cond")
    BODY = hydra.core.Name("body")

class BreakStatement(Node["Maybe[Identifier]"]):
    ...

BreakStatement.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.BreakStatement")

class ContinueStatement(Node["Maybe[Identifier]"]):
    ...

ContinueStatement.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ContinueStatement")

class ReturnStatement(Node["Maybe[Expression]"]):
    ...

ReturnStatement.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ReturnStatement")

class ThrowStatement(Node["Expression"]):
    ...

ThrowStatement.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ThrowStatement")

@dataclass(frozen=True)
class SynchronizedStatement:
    expression: Expression
    block: Block

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.SynchronizedStatement")
    EXPRESSION = hydra.core.Name("expression")
    BLOCK = hydra.core.Name("block")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TryStatement")
    SIMPLE = hydra.core.Name("simple")
    WITH_FINALLY = hydra.core.Name("withFinally")
    WITH_RESOURCES = hydra.core.Name("withResources")

@dataclass(frozen=True)
class TryStatement_Simple:
    block: Block
    catches: Catches

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TryStatement_Simple")
    BLOCK = hydra.core.Name("block")
    CATCHES = hydra.core.Name("catches")

@dataclass(frozen=True)
class TryStatement_WithFinally:
    block: Block
    catches: Maybe[Catches]
    finally_: Finally

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TryStatement_WithFinally")
    BLOCK = hydra.core.Name("block")
    CATCHES = hydra.core.Name("catches")
    FINALLY = hydra.core.Name("finally")

class Catches(Node["frozenlist[CatchClause]"]):
    ...

Catches.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Catches")

@dataclass(frozen=True)
class CatchClause:
    parameter: Maybe[CatchFormalParameter]
    block: Block

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CatchClause")
    PARAMETER = hydra.core.Name("parameter")
    BLOCK = hydra.core.Name("block")

@dataclass(frozen=True)
class CatchFormalParameter:
    modifiers: frozenlist[VariableModifier]
    type: CatchType
    id: VariableDeclaratorId

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CatchFormalParameter")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    ID = hydra.core.Name("id")

@dataclass(frozen=True)
class CatchType:
    type: UnannClassType
    types: frozenlist[ClassType]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CatchType")
    TYPE = hydra.core.Name("type")
    TYPES = hydra.core.Name("types")

class Finally(Node["Block"]):
    ...

Finally.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Finally")

@dataclass(frozen=True)
class TryWithResourcesStatement:
    resource_specification: ResourceSpecification
    block: Block
    catches: Maybe[Catches]
    finally_: Maybe[Finally]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TryWithResourcesStatement")
    RESOURCE_SPECIFICATION = hydra.core.Name("resourceSpecification")
    BLOCK = hydra.core.Name("block")
    CATCHES = hydra.core.Name("catches")
    FINALLY = hydra.core.Name("finally")

class ResourceSpecification(Node["frozenlist[Resource]"]):
    ...

ResourceSpecification.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ResourceSpecification")

class ResourceLocal(Node["Resource_Local"]):
    ...

class ResourceVariable(Node["VariableAccess"]):
    ...

class _ResourceMeta(type):
    def __getitem__(cls, item):
        return object

class Resource(metaclass=_ResourceMeta):
    r"""ResourceLocal | ResourceVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Resource")
    LOCAL = hydra.core.Name("local")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class Resource_Local:
    modifiers: frozenlist[VariableModifier]
    type: LocalVariableType
    identifier: Identifier
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Resource_Local")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    IDENTIFIER = hydra.core.Name("identifier")
    EXPRESSION = hydra.core.Name("expression")

class VariableAccessExpressionName(Node["ExpressionName"]):
    ...

class VariableAccessFieldAccess(Node["FieldAccess"]):
    ...

class _VariableAccessMeta(type):
    def __getitem__(cls, item):
        return object

class VariableAccess(metaclass=_VariableAccessMeta):
    r"""VariableAccessExpressionName | VariableAccessFieldAccess"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.VariableAccess")
    EXPRESSION_NAME = hydra.core.Name("expressionName")
    FIELD_ACCESS = hydra.core.Name("fieldAccess")

class PrimaryNoNewArray(Node["PrimaryNoNewArrayExpression"]):
    ...

class PrimaryArrayCreation(Node["ArrayCreationExpression"]):
    ...

class _PrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class Primary(metaclass=_PrimaryMeta):
    r"""PrimaryNoNewArray | PrimaryArrayCreation"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Primary")
    NO_NEW_ARRAY = hydra.core.Name("noNewArray")
    ARRAY_CREATION = hydra.core.Name("arrayCreation")

class PrimaryNoNewArrayExpressionLiteral(Node["Literal"]):
    ...

class PrimaryNoNewArrayExpressionClassLiteral(Node["ClassLiteral"]):
    ...

class PrimaryNoNewArrayExpressionThis:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimaryNoNewArrayExpressionThis)
    def __hash__(self):
        return hash("PrimaryNoNewArrayExpressionThis")

class PrimaryNoNewArrayExpressionDotThis(Node["TypeName"]):
    ...

class PrimaryNoNewArrayExpressionParens(Node["Expression"]):
    ...

class PrimaryNoNewArrayExpressionClassInstance(Node["ClassInstanceCreationExpression"]):
    ...

class PrimaryNoNewArrayExpressionFieldAccess(Node["FieldAccess"]):
    ...

class PrimaryNoNewArrayExpressionArrayAccess(Node["ArrayAccess"]):
    ...

class PrimaryNoNewArrayExpressionMethodInvocation(Node["MethodInvocation"]):
    ...

class PrimaryNoNewArrayExpressionMethodReference(Node["MethodReference"]):
    ...

class _PrimaryNoNewArrayExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryNoNewArrayExpression(metaclass=_PrimaryNoNewArrayExpressionMeta):
    r"""PrimaryNoNewArrayExpressionLiteral | PrimaryNoNewArrayExpressionClassLiteral | PrimaryNoNewArrayExpressionThis | PrimaryNoNewArrayExpressionDotThis | PrimaryNoNewArrayExpressionParens | PrimaryNoNewArrayExpressionClassInstance | PrimaryNoNewArrayExpressionFieldAccess | PrimaryNoNewArrayExpressionArrayAccess | PrimaryNoNewArrayExpressionMethodInvocation | PrimaryNoNewArrayExpressionMethodReference"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PrimaryNoNewArrayExpression")
    LITERAL = hydra.core.Name("literal")
    CLASS_LITERAL = hydra.core.Name("classLiteral")
    THIS = hydra.core.Name("this")
    DOT_THIS = hydra.core.Name("dotThis")
    PARENS = hydra.core.Name("parens")
    CLASS_INSTANCE = hydra.core.Name("classInstance")
    FIELD_ACCESS = hydra.core.Name("fieldAccess")
    ARRAY_ACCESS = hydra.core.Name("arrayAccess")
    METHOD_INVOCATION = hydra.core.Name("methodInvocation")
    METHOD_REFERENCE = hydra.core.Name("methodReference")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassLiteral")
    TYPE = hydra.core.Name("type")
    NUMERIC_TYPE = hydra.core.Name("numericType")
    BOOLEAN = hydra.core.Name("boolean")
    VOID = hydra.core.Name("void")

class TypeNameArraySimple(Node["TypeName"]):
    ...

class TypeNameArrayArray(Node["TypeNameArray"]):
    ...

class _TypeNameArrayMeta(type):
    def __getitem__(cls, item):
        return object

class TypeNameArray(metaclass=_TypeNameArrayMeta):
    r"""TypeNameArraySimple | TypeNameArrayArray"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeNameArray")
    SIMPLE = hydra.core.Name("simple")
    ARRAY = hydra.core.Name("array")

class NumericTypeArraySimple(Node["NumericType"]):
    ...

class NumericTypeArrayArray(Node["NumericTypeArray"]):
    ...

class _NumericTypeArrayMeta(type):
    def __getitem__(cls, item):
        return object

class NumericTypeArray(metaclass=_NumericTypeArrayMeta):
    r"""NumericTypeArraySimple | NumericTypeArrayArray"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.NumericTypeArray")
    SIMPLE = hydra.core.Name("simple")
    ARRAY = hydra.core.Name("array")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.BooleanArray")
    SIMPLE = hydra.core.Name("simple")
    ARRAY = hydra.core.Name("array")

@dataclass(frozen=True)
class ClassInstanceCreationExpression:
    qualifier: Maybe[ClassInstanceCreationExpression_Qualifier]
    expression: UnqualifiedClassInstanceCreationExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassInstanceCreationExpression")
    QUALIFIER = hydra.core.Name("qualifier")
    EXPRESSION = hydra.core.Name("expression")

class ClassInstanceCreationExpression_QualifierExpression(Node["ExpressionName"]):
    ...

class ClassInstanceCreationExpression_QualifierPrimary(Node["Primary"]):
    ...

class _ClassInstanceCreationExpression_QualifierMeta(type):
    def __getitem__(cls, item):
        return object

class ClassInstanceCreationExpression_Qualifier(metaclass=_ClassInstanceCreationExpression_QualifierMeta):
    r"""ClassInstanceCreationExpression_QualifierExpression | ClassInstanceCreationExpression_QualifierPrimary"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassInstanceCreationExpression_Qualifier")
    EXPRESSION = hydra.core.Name("expression")
    PRIMARY = hydra.core.Name("primary")

@dataclass(frozen=True)
class UnqualifiedClassInstanceCreationExpression:
    type_arguments: frozenlist[TypeArgument]
    class_or_interface: ClassOrInterfaceTypeToInstantiate
    arguments: frozenlist[Expression]
    body: Maybe[ClassBody]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.UnqualifiedClassInstanceCreationExpression")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    CLASS_OR_INTERFACE = hydra.core.Name("classOrInterface")
    ARGUMENTS = hydra.core.Name("arguments")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class ClassOrInterfaceTypeToInstantiate:
    identifiers: frozenlist[AnnotatedIdentifier]
    type_arguments: Maybe[TypeArgumentsOrDiamond]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ClassOrInterfaceTypeToInstantiate")
    IDENTIFIERS = hydra.core.Name("identifiers")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

@dataclass(frozen=True)
class AnnotatedIdentifier:
    annotations: frozenlist[Annotation]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AnnotatedIdentifier")
    ANNOTATIONS = hydra.core.Name("annotations")
    IDENTIFIER = hydra.core.Name("identifier")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.TypeArgumentsOrDiamond")
    ARGUMENTS = hydra.core.Name("arguments")
    DIAMOND = hydra.core.Name("diamond")

@dataclass(frozen=True)
class FieldAccess:
    qualifier: FieldAccess_Qualifier
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FieldAccess")
    QUALIFIER = hydra.core.Name("qualifier")
    IDENTIFIER = hydra.core.Name("identifier")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.FieldAccess_Qualifier")
    PRIMARY = hydra.core.Name("primary")
    SUPER = hydra.core.Name("super")
    TYPED = hydra.core.Name("typed")

@dataclass(frozen=True)
class ArrayAccess:
    expression: Maybe[Expression]
    variant: ArrayAccess_Variant

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayAccess")
    EXPRESSION = hydra.core.Name("expression")
    VARIANT = hydra.core.Name("variant")

class ArrayAccess_VariantName(Node["ExpressionName"]):
    ...

class ArrayAccess_VariantPrimary(Node["PrimaryNoNewArrayExpression"]):
    ...

class _ArrayAccess_VariantMeta(type):
    def __getitem__(cls, item):
        return object

class ArrayAccess_Variant(metaclass=_ArrayAccess_VariantMeta):
    r"""ArrayAccess_VariantName | ArrayAccess_VariantPrimary"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayAccess_Variant")
    NAME = hydra.core.Name("name")
    PRIMARY = hydra.core.Name("primary")

@dataclass(frozen=True)
class MethodInvocation:
    header: MethodInvocation_Header
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation")
    HEADER = hydra.core.Name("header")
    ARGUMENTS = hydra.core.Name("arguments")

class MethodInvocation_HeaderSimple(Node["MethodName"]):
    ...

class MethodInvocation_HeaderComplex(Node["MethodInvocation_Complex"]):
    ...

class _MethodInvocation_HeaderMeta(type):
    def __getitem__(cls, item):
        return object

class MethodInvocation_Header(metaclass=_MethodInvocation_HeaderMeta):
    r"""MethodInvocation_HeaderSimple | MethodInvocation_HeaderComplex"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Header")
    SIMPLE = hydra.core.Name("simple")
    COMPLEX = hydra.core.Name("complex")

@dataclass(frozen=True)
class MethodInvocation_Complex:
    variant: MethodInvocation_Variant
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Complex")
    VARIANT = hydra.core.Name("variant")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    IDENTIFIER = hydra.core.Name("identifier")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodInvocation_Variant")
    TYPE = hydra.core.Name("type")
    EXPRESSION = hydra.core.Name("expression")
    PRIMARY = hydra.core.Name("primary")
    SUPER = hydra.core.Name("super")
    TYPE_SUPER = hydra.core.Name("typeSuper")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference")
    EXPRESSION = hydra.core.Name("expression")
    PRIMARY = hydra.core.Name("primary")
    REFERENCE_TYPE = hydra.core.Name("referenceType")
    SUPER = hydra.core.Name("super")
    NEW = hydra.core.Name("new")
    ARRAY = hydra.core.Name("array")

@dataclass(frozen=True)
class MethodReference_Expression:
    name: ExpressionName
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Expression")
    NAME = hydra.core.Name("name")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MethodReference_Primary:
    primary: Primary
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Primary")
    PRIMARY = hydra.core.Name("primary")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MethodReference_ReferenceType:
    reference_type: ReferenceType
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference_ReferenceType")
    REFERENCE_TYPE = hydra.core.Name("referenceType")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    IDENTIFIER = hydra.core.Name("identifier")

@dataclass(frozen=True)
class MethodReference_Super:
    type_arguments: frozenlist[TypeArgument]
    identifier: Identifier
    super: bool

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Super")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")
    IDENTIFIER = hydra.core.Name("identifier")
    SUPER = hydra.core.Name("super")

@dataclass(frozen=True)
class MethodReference_New:
    class_type: ClassType
    type_arguments: frozenlist[TypeArgument]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference_New")
    CLASS_TYPE = hydra.core.Name("classType")
    TYPE_ARGUMENTS = hydra.core.Name("typeArguments")

class MethodReference_Array(Node["ArrayType"]):
    ...

MethodReference_Array.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MethodReference_Array")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression")
    PRIMITIVE = hydra.core.Name("primitive")
    CLASS_OR_INTERFACE = hydra.core.Name("classOrInterface")
    PRIMITIVE_ARRAY = hydra.core.Name("primitiveArray")
    CLASS_OR_INTERFACE_ARRAY = hydra.core.Name("classOrInterfaceArray")

@dataclass(frozen=True)
class ArrayCreationExpression_Primitive:
    type: PrimitiveTypeWithAnnotations
    dim_exprs: frozenlist[DimExpr]
    dims: Maybe[Dims]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_Primitive")
    TYPE = hydra.core.Name("type")
    DIM_EXPRS = hydra.core.Name("dimExprs")
    DIMS = hydra.core.Name("dims")

@dataclass(frozen=True)
class ArrayCreationExpression_ClassOrInterface:
    type: ClassOrInterfaceType
    dim_exprs: frozenlist[DimExpr]
    dims: Maybe[Dims]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterface")
    TYPE = hydra.core.Name("type")
    DIM_EXPRS = hydra.core.Name("dimExprs")
    DIMS = hydra.core.Name("dims")

@dataclass(frozen=True)
class ArrayCreationExpression_PrimitiveArray:
    type: PrimitiveTypeWithAnnotations
    dims: frozenlist[Dims]
    array: ArrayInitializer

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_PrimitiveArray")
    TYPE = hydra.core.Name("type")
    DIMS = hydra.core.Name("dims")
    ARRAY = hydra.core.Name("array")

@dataclass(frozen=True)
class ArrayCreationExpression_ClassOrInterfaceArray:
    type: ClassOrInterfaceType
    dims: frozenlist[Dims]
    array: ArrayInitializer

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ArrayCreationExpression_ClassOrInterfaceArray")
    TYPE = hydra.core.Name("type")
    DIMS = hydra.core.Name("dims")
    ARRAY = hydra.core.Name("array")

@dataclass(frozen=True)
class DimExpr:
    annotations: frozenlist[Annotation]
    expression: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.DimExpr")
    ANNOTATIONS = hydra.core.Name("annotations")
    EXPRESSION = hydra.core.Name("expression")

class ExpressionLambda(Node["LambdaExpression"]):
    ...

class ExpressionAssignment(Node["AssignmentExpression"]):
    ...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionLambda | ExpressionAssignment"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Expression")
    LAMBDA = hydra.core.Name("lambda")
    ASSIGNMENT = hydra.core.Name("assignment")

@dataclass(frozen=True)
class LambdaExpression:
    parameters: LambdaParameters
    body: LambdaBody

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LambdaExpression")
    PARAMETERS = hydra.core.Name("parameters")
    BODY = hydra.core.Name("body")

class LambdaParametersTuple(Node["frozenlist[LambdaParameters]"]):
    ...

class LambdaParametersSingle(Node["Identifier"]):
    ...

class _LambdaParametersMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaParameters(metaclass=_LambdaParametersMeta):
    r"""LambdaParametersTuple | LambdaParametersSingle"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LambdaParameters")
    TUPLE = hydra.core.Name("tuple")
    SINGLE = hydra.core.Name("single")

class LambdaParameterNormal(Node["LambdaParameter_Normal"]):
    ...

class LambdaParameterVariableArity(Node["VariableArityParameter"]):
    ...

class _LambdaParameterMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaParameter(metaclass=_LambdaParameterMeta):
    r"""LambdaParameterNormal | LambdaParameterVariableArity"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LambdaParameter")
    NORMAL = hydra.core.Name("normal")
    VARIABLE_ARITY = hydra.core.Name("variableArity")

@dataclass(frozen=True)
class LambdaParameter_Normal:
    modifiers: frozenlist[VariableModifier]
    type: LambdaParameterType
    id: VariableDeclaratorId

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LambdaParameter_Normal")
    MODIFIERS = hydra.core.Name("modifiers")
    TYPE = hydra.core.Name("type")
    ID = hydra.core.Name("id")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LambdaParameterType")
    TYPE = hydra.core.Name("type")
    VAR = hydra.core.Name("var")

class LambdaBodyExpression(Node["Expression"]):
    ...

class LambdaBodyBlock(Node["Block"]):
    ...

class _LambdaBodyMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaBody(metaclass=_LambdaBodyMeta):
    r"""LambdaBodyExpression | LambdaBodyBlock"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LambdaBody")
    EXPRESSION = hydra.core.Name("expression")
    BLOCK = hydra.core.Name("block")

class AssignmentExpressionConditional(Node["ConditionalExpression"]):
    ...

class AssignmentExpressionAssignment(Node["Assignment"]):
    ...

class _AssignmentExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class AssignmentExpression(metaclass=_AssignmentExpressionMeta):
    r"""AssignmentExpressionConditional | AssignmentExpressionAssignment"""

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AssignmentExpression")
    CONDITIONAL = hydra.core.Name("conditional")
    ASSIGNMENT = hydra.core.Name("assignment")

@dataclass(frozen=True)
class Assignment:
    lhs: LeftHandSide
    op: AssignmentOperator
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.Assignment")
    LHS = hydra.core.Name("lhs")
    OP = hydra.core.Name("op")
    EXPRESSION = hydra.core.Name("expression")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.LeftHandSide")
    EXPRESSION_NAME = hydra.core.Name("expressionName")
    FIELD_ACCESS = hydra.core.Name("fieldAccess")
    ARRAY_ACCESS = hydra.core.Name("arrayAccess")

class AssignmentOperator(Enum):
    SIMPLE = hydra.core.Name("simple")

    TIMES = hydra.core.Name("times")

    DIV = hydra.core.Name("div")

    MOD = hydra.core.Name("mod")

    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

    SHIFT_LEFT = hydra.core.Name("shiftLeft")

    SHIFT_RIGHT = hydra.core.Name("shiftRight")

    SHIFT_RIGHT_ZERO_FILL = hydra.core.Name("shiftRightZeroFill")

    AND = hydra.core.Name("and")

    XOR = hydra.core.Name("xor")

    OR = hydra.core.Name("or")

AssignmentOperator.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AssignmentOperator")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression")
    SIMPLE = hydra.core.Name("simple")
    TERNARY_COND = hydra.core.Name("ternaryCond")
    TERNARY_LAMBDA = hydra.core.Name("ternaryLambda")

@dataclass(frozen=True)
class ConditionalExpression_TernaryCond:
    cond: ConditionalOrExpression
    if_true: Expression
    if_false: ConditionalExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression_TernaryCond")
    COND = hydra.core.Name("cond")
    IF_TRUE = hydra.core.Name("ifTrue")
    IF_FALSE = hydra.core.Name("ifFalse")

@dataclass(frozen=True)
class ConditionalExpression_TernaryLambda:
    cond: ConditionalOrExpression
    if_true: Expression
    if_false: LambdaExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConditionalExpression_TernaryLambda")
    COND = hydra.core.Name("cond")
    IF_TRUE = hydra.core.Name("ifTrue")
    IF_FALSE = hydra.core.Name("ifFalse")

class ConditionalOrExpression(Node["frozenlist[ConditionalAndExpression]"]):
    ...

ConditionalOrExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConditionalOrExpression")

class ConditionalAndExpression(Node["frozenlist[InclusiveOrExpression]"]):
    ...

ConditionalAndExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConditionalAndExpression")

class InclusiveOrExpression(Node["frozenlist[ExclusiveOrExpression]"]):
    ...

InclusiveOrExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.InclusiveOrExpression")

class ExclusiveOrExpression(Node["frozenlist[AndExpression]"]):
    ...

ExclusiveOrExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ExclusiveOrExpression")

class AndExpression(Node["frozenlist[EqualityExpression]"]):
    ...

AndExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AndExpression")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EqualityExpression")
    UNARY = hydra.core.Name("unary")
    EQUAL = hydra.core.Name("equal")
    NOT_EQUAL = hydra.core.Name("notEqual")

@dataclass(frozen=True)
class EqualityExpression_Binary:
    lhs: EqualityExpression
    rhs: RelationalExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.EqualityExpression_Binary")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression")
    SIMPLE = hydra.core.Name("simple")
    LESS_THAN = hydra.core.Name("lessThan")
    GREATER_THAN = hydra.core.Name("greaterThan")
    LESS_THAN_EQUAL = hydra.core.Name("lessThanEqual")
    GREATER_THAN_EQUAL = hydra.core.Name("greaterThanEqual")
    INSTANCEOF = hydra.core.Name("instanceof")

@dataclass(frozen=True)
class RelationalExpression_LessThan:
    lhs: RelationalExpression
    rhs: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_LessThan")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_GreaterThan:
    lhs: RelationalExpression
    rhs: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_GreaterThan")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_LessThanEqual:
    lhs: RelationalExpression
    rhs: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_LessThanEqual")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_GreaterThanEqual:
    lhs: RelationalExpression
    rhs: ShiftExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_GreaterThanEqual")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class RelationalExpression_InstanceOf:
    lhs: RelationalExpression
    rhs: ReferenceType

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.RelationalExpression_InstanceOf")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ShiftExpression")
    UNARY = hydra.core.Name("unary")
    SHIFT_LEFT = hydra.core.Name("shiftLeft")
    SHIFT_RIGHT = hydra.core.Name("shiftRight")
    SHIFT_RIGHT_ZERO_FILL = hydra.core.Name("shiftRightZeroFill")

@dataclass(frozen=True)
class ShiftExpression_Binary:
    lhs: ShiftExpression
    rhs: AdditiveExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ShiftExpression_Binary")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AdditiveExpression")
    UNARY = hydra.core.Name("unary")
    PLUS = hydra.core.Name("plus")
    MINUS = hydra.core.Name("minus")

@dataclass(frozen=True)
class AdditiveExpression_Binary:
    lhs: AdditiveExpression
    rhs: MultiplicativeExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.AdditiveExpression_Binary")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MultiplicativeExpression")
    UNARY = hydra.core.Name("unary")
    TIMES = hydra.core.Name("times")
    DIVIDE = hydra.core.Name("divide")
    MOD = hydra.core.Name("mod")

@dataclass(frozen=True)
class MultiplicativeExpression_Binary:
    lhs: MultiplicativeExpression
    rhs: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.MultiplicativeExpression_Binary")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.UnaryExpression")
    PRE_INCREMENT = hydra.core.Name("preIncrement")
    PRE_DECREMENT = hydra.core.Name("preDecrement")
    PLUS = hydra.core.Name("plus")
    MINUS = hydra.core.Name("minus")
    OTHER = hydra.core.Name("other")

class PreIncrementExpression(Node["UnaryExpression"]):
    ...

PreIncrementExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PreIncrementExpression")

class PreDecrementExpression(Node["UnaryExpression"]):
    ...

PreDecrementExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PreDecrementExpression")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.UnaryExpressionNotPlusMinus")
    POSTFIX = hydra.core.Name("postfix")
    TILDE = hydra.core.Name("tilde")
    NOT = hydra.core.Name("not")
    CAST = hydra.core.Name("cast")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PostfixExpression")
    PRIMARY = hydra.core.Name("primary")
    NAME = hydra.core.Name("name")
    POST_INCREMENT = hydra.core.Name("postIncrement")
    POST_DECREMENT = hydra.core.Name("postDecrement")

class PostIncrementExpression(Node["PostfixExpression"]):
    ...

PostIncrementExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PostIncrementExpression")

class PostDecrementExpression(Node["PostfixExpression"]):
    ...

PostDecrementExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.PostDecrementExpression")

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

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CastExpression")
    PRIMITIVE = hydra.core.Name("primitive")
    NOT_PLUS_MINUS = hydra.core.Name("notPlusMinus")
    LAMBDA = hydra.core.Name("lambda")

@dataclass(frozen=True)
class CastExpression_Primitive:
    type: PrimitiveTypeWithAnnotations
    expression: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CastExpression_Primitive")
    TYPE = hydra.core.Name("type")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class CastExpression_NotPlusMinus:
    ref_and_bounds: CastExpression_RefAndBounds
    expression: UnaryExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CastExpression_NotPlusMinus")
    REF_AND_BOUNDS = hydra.core.Name("refAndBounds")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class CastExpression_Lambda:
    ref_and_bounds: CastExpression_RefAndBounds
    expression: LambdaExpression

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CastExpression_Lambda")
    REF_AND_BOUNDS = hydra.core.Name("refAndBounds")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class CastExpression_RefAndBounds:
    type: ReferenceType
    bounds: frozenlist[AdditionalBound]

    TYPE_ = hydra.core.Name("hydra.ext.java.syntax.CastExpression_RefAndBounds")
    TYPE = hydra.core.Name("type")
    BOUNDS = hydra.core.Name("bounds")

class ConstantExpression(Node["Expression"]):
    ...

ConstantExpression.TYPE_ = hydra.core.Name("hydra.ext.java.syntax.ConstantExpression")
