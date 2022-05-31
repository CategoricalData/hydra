-- Based on the Oracle Java SE 12 BNF.
-- See https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html

module Hydra.Impl.Haskell.Sources.Ext.Java.Syntax where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


javaSyntaxModule :: Module Meta
javaSyntaxModule = Module javaSyntax []

javaSyntaxName :: GraphName
javaSyntaxName = GraphName "hydra/ext/java/syntax"

javaSyntax :: Graph Meta
javaSyntax = Graph javaSyntaxName elements (const True) hydraCoreName
  where
    def = datatype javaSyntaxName
    java = nominal . qualify javaSyntaxName . Name

    elements = [

--Productions from §3 (Lexical Structure)

--Identifier:
--  IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
      def "Identifier" string,
--IdentifierChars:
--  JavaLetter {JavaLetterOrDigit}
--
--JavaLetter:
--  any Unicode character that is a "Java letter"
--
--JavaLetterOrDigit:
--  any Unicode character that is a "Java letter-or-digit"

--TypeIdentifier:
--  Identifier but not var
      def "TypeIdentifier" $ java "Identifier",

--Literal:
      def "Literal" $
        union [
--  NullLiteral
          field "null" unit,
--  IntegerLiteral
          field "integer" $ java "IntegerLiteral",
--  FloatingPointLiteral
          field "floatingPoint" $ java "FloatingPointLiteral",
--  BooleanLiteral
          field "boolean" boolean,
--  CharacterLiteral
          field "character" uint16,
--  StringLiteral
          field "string" $ java "StringLiteral"],
      def "IntegerLiteral" $
        doc "Note: this is an approximation which ignores encoding"
        bigint,
      def "FloatingPointLiteral" $
        doc "Note: this is an approximation which ignores encoding"
        bigfloat,
      def "StringLiteral" $
        doc "Note: this is an approximation which ignores encoding"
        string,

--Productions from §4 (Types, Values, and Variables)

--Type:
      def "Type" $ union [
--  PrimitiveType
          field "primitive" $ java "PrimitiveTypeWithAnnotations",
--  ReferenceType
          field "reference" $ java "ReferenceType"],

--PrimitiveType:
      def "PrimitiveTypeWithAnnotations" $ record [
        field "type" $ java "PrimitiveType",
        field "annotations" $ list $ java "Annotation"],
      def "PrimitiveType" $ union [
--  {Annotation} NumericType
        field "numeric" $ java "NumericType",
--  {Annotation} boolean
        field "boolean" unit],

--NumericType:
      def "NumericType" $ union [
--  IntegralType
        field "integral" $ java "IntegralType",
--  FloatingPointType
        field "floatingPoint" $ java "FloatingPointType"],

--IntegralType:
      def "IntegralType" $ enum [
--  (one of)
--  byte short int long char
        "byte", "short", "int", "long", "char"],

--FloatingPointType:
      def "FloatingPointType" $ enum [
--  (one of)
--  float double
        "float", "double"],

--ReferenceType:
      def "ReferenceType" $ union [
--  ClassOrInterfaceType
        field "classOrInterface" $ java "ClassOrInterfaceType",
--  TypeVariable
        field "variable" $ java "TypeVariable",
--  ArrayType
        field "array" $ java "ArrayType"],

--ClassOrInterfaceType:
      def "ClassOrInterfaceType" $ union [
--  ClassType
        field "class" $ java "ClassType",
--  InterfaceType
        field "interface" $ java "InterfaceType"],

--ClassType:
      def "ClassType" $ record [
        field "annotations" $ list $ java "Annotation",
        field "qualifier" $ java "ClassTypeQualifier",
        field "identifier" $ java "TypeIdentifier",
        field "arguments" $ list $ java "TypeArgument"],
      def "ClassTypeQualifier" $ union [
--  {Annotation} TypeIdentifier [TypeArguments]
        field "none" unit,
--  PackageName . {Annotation} TypeIdentifier [TypeArguments]
        field "package" $ java "PackageName",
--  ClassOrInterfaceType . {Annotation} TypeIdentifier [TypeArguments]
        field "parent" $ java "ClassOrInterfaceType"],

--InterfaceType:
--  ClassType
      def "InterfaceType" $ java "ClassType",

--TypeVariable:
--  {Annotation} TypeIdentifier
      def "TypeVariable" $ record [
        field "annotations" $ list $ java "Annotation",
        field "identifier" $ java "TypeIdentifier"],

--ArrayType:
      def "ArrayType" $ record [
        field "dims" $ java "Dims",
        field "variant" $ java "ArrayType.Variant"],
      def "ArrayType.Variant" $ union [
--  PrimitiveType Dims
        field "primitive" $ java "PrimitiveTypeWithAnnotations",
--  ClassOrInterfaceType Dims
        field "classOrInterface" $ java "ClassOrInterfaceType",
--  TypeVariable Dims
        field "variable" $ java "TypeVariable"],

--Dims:
--  {Annotation} [ ] {{Annotation} [ ]}
      def "Dims" $ list $ list $ java "Annotation",

--TypeParameter:
--  {TypeParameterModifier} TypeIdentifier [TypeBound]
      def "TypeParameter" $ record [
        field "modifiers" $ list $ java "TypeParameterModifier",
        field "identifier" $ java "TypeIdentifier",
        field "bound" $ optional $ java "TypeBound"],

--TypeParameterModifier:
--  Annotation
      def "TypeParameterModifier" $ java "Annotation",

--TypeBound:
      def "TypeBound" $ union [
--  extends TypeVariable
        field "variable" $ java "TypeVariable",
--  extends ClassOrInterfaceType {AdditionalBound}
        field "classOrInterface" $ java "TypeBound.ClassOrInterface"],
      def "TypeBound.ClassOrInterface" $ record [
        field "type" $ java "ClassOrInterfaceType",
        field "additional" $ list $ java "AdditionalBound"],

--AdditionalBound:
--  & InterfaceType
      def "AdditionalBound" $ java "InterfaceType",

--TypeArguments:
--  < TypeArgumentList >
--TypeArgumentList:
--  TypeArgument {, TypeArgument}

--TypeArgument:
      def "TypeArgument" $ union [
--  ReferenceType
        field "reference" $ java "ReferenceType",
--  Wildcard
        field "wildcard" $ java "Wildcard"],

--Wildcard:
--  {Annotation} ? [WildcardBounds]
      def "Wildcard" $ record [
        field "annotations" $ list $ java "Annotation",
        field "wildcard" $ optional $ java "WildcardBounds"],

--WildcardBounds:
      def "WildcardBounds" $ union [
--  extends ReferenceType
        field "extends" $ java "ReferenceType",
--  super ReferenceType
        field "super" $ java "ReferenceType"],

--Productions from §6 (Names)

--ModuleName:
      def "ModuleName" $ record [
--  Identifier
        field "identifier" $ java "Identifier",
--  ModuleName . Identifier
        field "name" $ optional $ java "ModuleName"],

--PackageName:
--  Identifier
--  PackageName . Identifier
      def "PackageName" $ list $ java "Identifier",

--TypeName:
      def "TypeName" $ record [
--  TypeIdentifier
        field "identifier" $ java "TypeIdentifier",
--  PackageOrTypeName . TypeIdentifier
        field "qualifier" $ optional $ java "PackageOrTypeName"],

--ExpressionName:
      def "ExpressionName" $ record [
--  Identifier
        field "identifier" $ java "Identifier",
--  AmbiguousName . Identifier
        field "name" $ optional $ java "AmbiguousName"],

--MethodName:
--  Identifier
      def "MethodName" $ java "Identifier",

--PackageOrTypeName:
--  Identifier
--  PackageOrTypeName . Identifier
      def "PackageOrTypeName" $ list $ java "Identifier",

--AmbiguousName:
--  Identifier
--  AmbiguousName . Identifier
      def "AmbiguousName" $ list $ java "Identifier",

--Productions from §7 (Packages and Modules)

--CompilationUnit:
      def "CompilationUnit" $ union [
--  OrdinaryCompilationUnit
        field "ordinary" $ java "OrdinaryCompilationUnit",
--  ModularCompilationUnit
        field "modular" $ java "ModularCompilationUnit"],

--OrdinaryCompilationUnit:
--  [PackageDeclaration] {ImportDeclaration} {TypeDeclaration}
      def "OrdinaryCompilationUnit" $ record [
        field "package" $ optional $ java "PackageDeclaration",
        field "imports" $ list $ java "ImportDeclaration",
        field "types" $ list $ java "TypeDeclaration"],

--ModularCompilationUnit:
--  {ImportDeclaration} ModuleDeclaration
      def "ModularCompilationUnit" $ record [
        field "imports" $ list $ java "ImportDeclaration",
        field "module" $ java "ModuleDeclaration"],

--PackageDeclaration:
--  {PackageModifier} package Identifier {. Identifier} ;
      def "PackageDeclaration" $ record [
        field "modifiers" $ list $ java "PackageModifier",
        field "identifiers" $ list $ java "Identifier"],

--PackageModifier:
--  Annotation
      def "PackageModifier" $ java "Annotation",

--ImportDeclaration:
      def "ImportDeclaration" $ union [
--  SingleTypeImportDeclaration
        field "singleType" $ java "SingleTypeImportDeclaration",
--  TypeImportOnDemandDeclaration
        field "typeImportOnDemand" $ java "TypeImportOnDemandDeclaration",
--  SingleStaticImportDeclaration
        field "singleStaticImport" $ java "SingleStaticImportDeclaration",
--  StaticImportOnDemandDeclaration
        field "staticImportOnDemand" $ java "StaticImportOnDemandDeclaration"],

--SingleTypeImportDeclaration:
--  import TypeName ;
      def "SingleTypeImportDeclaration" $ java "TypeName",

--TypeImportOnDemandDeclaration:
--  import PackageOrTypeName . * ;
      def "TypeImportOnDemandDeclaration" $ java "PackageOrTypeName",

--SingleStaticImportDeclaration:
--  import static TypeName . Identifier ;
      def "SingleStaticImportDeclaration" $ record [
        field "typeName" $ java "TypeName",
        field "identifier" $ java "Identifier"],

--StaticImportOnDemandDeclaration:
--  import static TypeName . * ;
      def "StaticImportOnDemandDeclaration" $ java "TypeName",

--TypeDeclaration:
      def "TypeDeclaration" $ union [
--  ClassDeclaration
        field "class" $ java "ClassDeclaration",
--  InterfaceDeclaration
        field "interface" $ java "InterfaceDeclaration"],
--  ;

--ModuleDeclaration:
--  {Annotation} [open] module Identifier {. Identifier} { {ModuleDirective} }
      def "ModuleDeclaration" $ record [
        field "annotations" $ list $ java "Annotation",
        field "open" boolean,
        field "identifiers" $ list $ java "Identifier",
        field "directives" $ list $ list $ java "ModuleDirective"],

--ModuleDirective:
      def "ModuleDirective" $ union [
--  requires {RequiresModifier} ModuleName ;
        field "requires" $ java "ModuleDirective.Requires",
--  exports PackageName [to ModuleName {, ModuleName}] ;
        field "exports" $ java "ModuleDirective.ExportsOrOpens",
--  opens PackageName [to ModuleName {, ModuleName}] ;
        field "opens" $ java "ModuleDirective.ExportsOrOpens",
--  uses TypeName ;
        field "uses" $ java "TypeName",
--  provides TypeName with TypeName {, TypeName} ;
        field "provides" $ java "ModuleDirective.Provides"],
      def "ModuleDirective.Requires" $ record [
        field "modifiers" $ list $ java "RequiresModifier",
        field "module" $ java "ModuleName"],
      def "ModuleDirective.ExportsOrOpens" $ record [
        field "package" $ java "PackageName",
        field "modules" $
          doc "At least one module" $
          list $ java "ModuleName"],
      def "ModuleDirective.Provides" $ record [
        field "to" $ java "TypeName",
        field "with" $
          doc "At least one type" $
          list $ java "TypeName"],

--RequiresModifier:
      def "RequiresModifier" $ enum [
--  (one of)
--  transitive static
        "transitive", "static"],

--Productions from §8 (Classes)

--ClassDeclaration:
      def "ClassDeclaration" $ union [
--  NormalClassDeclaration
        field "normal" $ java "NormalClassDeclaration",
--  EnumDeclaration
        field "enum" $ java "EnumDeclaration"],

--NormalClassDeclaration:
--  {ClassModifier} class TypeIdentifier [TypeParameters] [Superclass] [Superinterfaces] ClassBody
      def "NormalClassDeclaration" $ record [
        field "modifiers" $ list $ java "ClassModifier",
        field "identifier" $ java "TypeIdentifier",
        field "parameters" $ list $ java "TypeParameter",
        field "extends" $ optional $ java "ClassType",
        field "implements" $ list $ java "InterfaceType",
        field "body" $ java "ClassBody"],

--ClassModifier:
      def "ClassModifier" $ union [
--  (one of)
--  Annotation public protected private
--  abstract static final strictfp
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "protected" unit,
        field "private" unit,
        field "abstract" unit,
        field "static" unit,
        field "final" unit,
        field "strictfp" unit],

--TypeParameters:
--  < TypeParameterList >
--TypeParameterList:
--  TypeParameter {, TypeParameter}
--Superclass:
--  extends ClassType
--Superinterfaces:
--  implements InterfaceTypeList
--InterfaceTypeList:
--  InterfaceType {, InterfaceType}

--ClassBody:
--  { {ClassBodyDeclaration} }
      def "ClassBody" $ list $ java "ClassBodyDeclaration",

--ClassBodyDeclaration:
      def "ClassBodyDeclaration" $ union [
--  ClassMemberDeclaration
        field "classMember" $ java "ClassMemberDeclaration",
--  InstanceInitializer
        field "instanceInitializer" $ java "InstanceInitializer",
--  StaticInitializer
        field "staticInitializer" $ java "StaticInitializer",
--  ConstructorDeclaration
        field "constructorDeclaration" $ java "ConstructorDeclaration"],

--ClassMemberDeclaration:
      def "ClassMemberDeclaration" $ union [
--  FieldDeclaration
        field "field" $ java "FieldDeclaration",
--  MethodDeclaration
        field "method" $ java "MethodDeclaration",
--  ClassDeclaration
        field "class" $ java "ClassDeclaration",
--  InterfaceDeclaration
        field "interface" $ java "InterfaceDeclaration"],
--  ;

--FieldDeclaration:
--  {FieldModifier} UnannType VariableDeclaratorList ;
      def "FieldDeclaration" $ record [
        field "modifiers" $ list $ java "FieldModifier",
        field "unannType" $ java "UnannType",
        field "variableDeclarators" $ nonemptyList $ java "VariableDeclarator"],

--FieldModifier:
--  (one of)
      def "FieldModifier" $ union [
--  Annotation public protected private
--  static final transient volatile
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "protected" unit,
        field "private" unit,
        field "static" unit,
        field "final" unit,
        field "transient" unit,
        field "volatile" unit],

--VariableDeclaratorList:
--  VariableDeclarator {, VariableDeclarator}
--VariableDeclarator:
--  VariableDeclaratorId [= VariableInitializer]
      def "VariableDeclarator" $ record [
        field "id" $ java "VariableDeclaratorId",
        field "initializer" $ optional $ java "VariableInitializer"],

--VariableDeclaratorId:
--  Identifier [Dims]
      def "VariableDeclaratorId" $ record [
        field "identifier" $ java "Identifier",
        field "dims" $ optional $ java "Dims"],

--VariableInitializer:
      def "VariableInitializer" $ union [
--  Expression
        field "expression" $ java "Expression",
--  ArrayInitializer
        field "arrayInitializer" $ java "ArrayInitializer"],

--UnannType:
--  UnannPrimitiveType
--  UnannReferenceType
      def "UnannType" $
        doc "A Type which does not allow annotations" $
        java "Type",
--UnannPrimitiveType:
--  NumericType
--  boolean
--UnannReferenceType:
--  UnannClassOrInterfaceType
--  UnannTypeVariable
--  UnannArrayType
--UnannClassOrInterfaceType:
--  UnannClassType
--  UnannInterfaceType
--UnannClassType:
--  TypeIdentifier [TypeArguments]
--  PackageName . {Annotation} TypeIdentifier [TypeArguments]
--  UnannClassOrInterfaceType . {Annotation} TypeIdentifier [TypeArguments]
      def "UnannClassType" $
        doc "A ClassType which does not allow annotations" $
        java "ClassType",
--UnannInterfaceType:
--  UnannClassType
--UnannTypeVariable:
--  TypeIdentifier
--UnannArrayType:
--  UnannPrimitiveType Dims
--  UnannClassOrInterfaceType Dims
--  UnannTypeVariable Dims

--MethodDeclaration:
--  {MethodModifier} MethodHeader MethodBody
      def "MethodDeclaration" $ record [
        field "modifiers" $ list $ java "MethodModifier",
        field "header" $ java "MethodHeader",
        field "body" $ java "MethodBody"],

--MethodModifier:
--  (one of)
      def "MethodModifier" $ union [
--  Annotation public protected private
--  abstract static final synchronized native strictfp
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "protected" unit,
        field "private" unit,
        field "abstract" unit,
        field "final" unit,
        field "synchronized" unit,
        field "native" unit,
        field "strictfb" unit],

--MethodHeader:
--  Result MethodDeclarator [Throws]
--  TypeParameters {Annotation} Result MethodDeclarator [Throws]
      def "MethodHeader" $ record [
        field "parameters" $ list $ java "TypeParameter",
        field "annotations" $
          doc "Note: simple method headers cannot have annotations" $
          list $ java "Annotation",
        field "result" $ java "Result",
        field "declarator" $ java "MethodDeclarator",
        field "throws" $ optional $ java "Throws"],

--Result:
      def "Result" $ union [
--  UnannType
        field "type" $ java "UnannType",
--  void
        field "void" unit],

--MethodDeclarator:
--  Identifier ( [ReceiverParameter ,] [FormalParameterList] ) [Dims]
      def "MethodDeclarator" $ record [
        field "identifier" $ java "Identifier",
        field "receiverParameter" $ optional $ java "ReceiverParameter",
        field "formalParameters" $ nonemptyList $ java "FormalParameter"],

--ReceiverParameter:
--  {Annotation} UnannType [Identifier .] this
      def "ReceiverParameter" $ record [
        field "annotations" $ list $ java "Annotation",
        field "unannType" $ java "UnannType",
        field "identifier" $ optional $ java "Identifier"],

--FormalParameterList:
--  FormalParameter {, FormalParameter}
--FormalParameter:
      def "FormalParameter" $ union [
--  {VariableModifier} UnannType VariableDeclaratorId
        field "simple" $ java "FormalParameter.Simple",
--  VariableArityParameter
        field "variableArity" $ java "VariableArityParameter"],
      def "FormalParameter.Simple" $ record [
        field "modifiers" $ list $ java "VariableModifier",
        field "type" $ java "UnannType",
        field "id" $ java "VariableDeclaratorId"],

--VariableArityParameter:
--  {VariableModifier} UnannType {Annotation} ... Identifier
      def "VariableArityParameter" $ record [
        field "modifiers" $ java "VariableModifier",
        field "type" $ java "UnannType",
        field "annotations" $ list $ java "Annotation",
        field "identifier" $ java "Identifier"],

--VariableModifier:
      def "VariableModifier" $ union [
--  Annotation
        field "annotation" $ java "Annotation",
--  final
        field "final" unit],

--Throws:
--  throws ExceptionTypeList
      def "Throws" $ nonemptyList $ java "ExceptionType",

--ExceptionTypeList:
--  ExceptionType {, ExceptionType}
--ExceptionType:
      def "ExceptionType" $ union [
--  ClassType
        field "class" $ java "ClassType",
--  TypeVariable
        field "variable" $ java "TypeVariable"],

--MethodBody:
--  Block
      def "MethodBody" $ java "Block",
--  ;

--InstanceInitializer:
--  Block
      def "InstanceInitializer" $ java "Block",

--StaticInitializer:
--  static Block
      def "StaticInitializer" $ java "Block",

--ConstructorDeclaration:
--  {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
      def "ConstructorDeclaration" $ record [
        field "modifiers" $ list $ java "ConstructorModifier",
        field "constructor" $ java "ConstructorDeclarator",
        field "throws" $ optional $ java "Throws",
        field "body" $ java "ConstructorBody"],

--ConstructorModifier:
--  (one of)
      def "ConstructorModifier" $ union [
--  Annotation public protected private
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "protected" unit,
        field "private" unit],

--ConstructorDeclarator:
--  [TypeParameters] SimpleTypeName ( [ReceiverParameter ,] [FormalParameterList] )
      def "ConstructorDeclarator" $ record [
        field "parameters" $ list $ java "TypeParameter",
        field "name" $ java "SimpleTypeName",
        field "receiverParameter" $ optional $ java "ReceiverParameter",
        field "formalParameters" $ nonemptyList $ java "FormalParameter"],

--SimpleTypeName:
--  TypeIdentifier
      def "SimpleTypeName" $ java "TypeIdentifier",

--ConstructorBody:
--  { [ExplicitConstructorInvocation] [BlockStatements] }
      def "ConstructorBody" $ list $ java "ConstructorBody.Element",
      def "ConstructorBody.Element" $ record [
        field "invocation" $ optional $ java "ExplicitConstructorInvocation",
        field "statements" $ list $ java "BlockStatement"],

--ExplicitConstructorInvocation:
      def "ExplicitConstructorInvocation" $ record [
        field "typeArguments" $ list $ java "TypeArgument",
        field "arguments" $ list $ java "Expression",
        field "variant" $ java "ExplicitConstructorInvocation.Variant"],
      def "ExplicitConstructorInvocation.Variant" $ union [
--  [TypeArguments] this ( [ArgumentList] ) ;
        field "this" unit,
--  [TypeArguments] super ( [ArgumentList] ) ;
--  ExpressionName . [TypeArguments] super ( [ArgumentList] ) ;
        field "super" $ optional $ java "ExpressionName",
--  Primary . [TypeArguments] super ( [ArgumentList] ) ;
        field "primary" $ java "Primary"],

--EnumDeclaration:
--  {ClassModifier} enum TypeIdentifier [Superinterfaces] EnumBody
      def "EnumDeclaration" $ record [
        field "modifiers" $ list $ java "ClassModifier",
        field "identifier" $ java "TypeIdentifier",
        field "implements" $ list $ java "InterfaceType",
        field "body" $ java "EnumBody"],

--EnumBody:
--  { [EnumConstantList] [,] [EnumBodyDeclarations] }
      def "EnumBody" $ list $ java "EnumBody.Element",
      def "EnumBody.Element" $ record [
        field "constants" $ list $ java "EnumConstant",
        field "bodyDeclarations" $ list $ java "ClassBodyDeclaration"],

--EnumConstantList:
--  EnumConstant {, EnumConstant}
--EnumConstant:
--  {EnumConstantModifier} Identifier [( [ArgumentList] )] [ClassBody]
      def "EnumConstant" $ record [
        field "modifiers" $ list $ java "EnumConstantModifier",
        field "identifier" $ java "Identifier",
        field "arguments" $ list $ list $ java "Expression",
        field "body" $ optional $ java "ClassBody"],

--EnumConstantModifier:
--  Annotation
      def "EnumConstantModifier" $ java "Annotation",

--EnumBodyDeclarations:
--  ; {ClassBodyDeclaration}

--Productions from §9 (Interfaces)

--InterfaceDeclaration:
      def "InterfaceDeclaration" $ union [
--  NormalInterfaceDeclaration
        field "normalInterface" $ java "NormalInterfaceDeclaration",
--  AnnotationTypeDeclaration
        field "annotationType" $ java "AnnotationTypeDeclaration"],

--NormalInterfaceDeclaration:
--  {InterfaceModifier} interface TypeIdentifier [TypeParameters] [ExtendsInterfaces] InterfaceBody
      def "NormalInterfaceDeclaration" $ record [
        field "modifiers" $ list $ java "InterfaceModifier",
        field "identifier" $ java "TypeIdentifier",
        field "parameters" $ list $ java "TypeParameter",
        field "extends" $ list $ java "InterfaceType",
        field "body" $ java "InterfaceBody"],

--InterfaceModifier:
--  (one of)
      def "InterfaceModifier" $ union [
--  Annotation public protected private
--  abstract static strictfp
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "protected" unit,
        field "private" unit,
        field "abstract" unit,
        field "static" unit,
        field "strictfb" unit],

--ExtendsInterfaces:
--  extends InterfaceTypeList

--InterfaceBody:
--  { {InterfaceMemberDeclaration} }
      def "InterfaceBody" $ list $ list $ java "InterfaceMemberDeclaration",

--InterfaceMemberDeclaration:
      def "InterfaceMemberDeclaration" $ union [
--  ConstantDeclaration
        field "constant" $ java "ConstantDeclaration",
--  InterfaceMethodDeclaration
        field "interfaceMethod" $ java "InterfaceMethodDeclaration",
--  ClassDeclaration
        field "class" $ java "ClassDeclaration",
--  InterfaceDeclaration
        field "interface" $ java "InterfaceDeclaration"],
--  ;

--ConstantDeclaration:
--  {ConstantModifier} UnannType VariableDeclaratorList ;
      def "ConstantDeclaration" $ record [
        field "modifiers" $ list $ java "ConstantModifier",
        field "type" $ java "UnannType",
        field "variables" $ nonemptyList $ java "VariableDeclarator"],

--ConstantModifier:
--  (one of)
      def "ConstantModifier" $ union [
--  Annotation public
--  static final
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "static" unit,
        field "final" unit],

--InterfaceMethodDeclaration:
--  {InterfaceMethodModifier} MethodHeader MethodBody
      def "InterfaceMethodDeclaration" $ record [
        field "modifiers" $ list $ java "InterfaceMethodModifier",
        field "header" $ java "MethodHeader",
        field "body" $ java "MethodBody"],

--InterfaceMethodModifier:
--  (one of)
      def "InterfaceMethodModifier" $ union [
--  Annotation public private
--  abstract default static strictfp
        field "annotation" $ java "Annotation",
        field "public" unit,
        field "private" unit,
        field "abstract" unit,
        field "default" unit,
        field "static" unit,
        field "strictfp" unit],

--AnnotationTypeDeclaration:
--  {InterfaceModifier} @ interface TypeIdentifier AnnotationTypeBody
      def "AnnotationTypeDeclaration" $ record [
        field "modifiers" $ list $ java "InterfaceModifier",
        field "identifier" $ java "TypeIdentifier",
        field "body" $ java "AnnotationTypeBody"],

--AnnotationTypeBody:
--  { {AnnotationTypeMemberDeclaration} }
      def "AnnotationTypeBody" $ list $ list $ java "AnnotationTypeMemberDeclaration",

--AnnotationTypeMemberDeclaration:
      def "AnnotationTypeMemberDeclaration" $ union [
--  AnnotationTypeElementDeclaration
        field "annotationType" $ java "AnnotationTypeElementDeclaration",
--  ConstantDeclaration
        field "constant" $ java "ConstantDeclaration",
--  ClassDeclaration
        field "class" $ java "ClassDeclaration",
--  InterfaceDeclaration
        field "interface" $ java "InterfaceDeclaration"],
--  ;

--AnnotationTypeElementDeclaration:
--  {AnnotationTypeElementModifier} UnannType Identifier ( ) [Dims] [DefaultValue] ;
      def "AnnotationTypeElementDeclaration" $ record [
        field "modifiers" $ list $ java "AnnotationTypeElementModifier",
        field "type" $ java "UnannType",
        field "identifier" $ java "Identifier",
        field "dims" $ optional $ java "Dims",
        field "default" $ optional $ java "DefaultValue"],

--AnnotationTypeElementModifier:
--  (one of)
      def "AnnotationTypeElementModifier" $ union [
--  Annotation public
        field "public" $ java "Annotation",
--  abstract
        field "abstract" unit],

--DefaultValue:
--  default ElementValue
      def "DefaultValue" $ java "ElementValue",

--Annotation:
      def "Annotation" $ union [
--  NormalAnnotation
        field "normal" $ java "NormalAnnotation",
--  MarkerAnnotation
        field "marker" $ java "MarkerAnnotation",
--  SingleElementAnnotation
        field "singleElement" $ java "SingleElementAnnotation"],

--NormalAnnotation:
--  @ TypeName ( [ElementValuePairList] )
      def "NormalAnnotation" $ record [
        field "typeName" $ java "TypeName",
        field "pairs" $ list $ java "ElementValuePair"],

--ElementValuePairList:
--  ElementValuePair {, ElementValuePair}
--ElementValuePair:
--  Identifier = ElementValue
      def "ElementValuePair" $ record [
        field "key" $ java "Identifier",
        field "value" $ java "ElementValue"],

--ElementValue:
      def "ElementValue" $ union [
--  ConditionalExpression
        field "conditionalExpression" $ java "ConditionalExpression",
--  ElementValueArrayInitializer
        field "elementValueArrayInitializer" $ java "ElementValueArrayInitializer",
--  Annotation
        field "annotation" $ java "Annotation"],

--ElementValueArrayInitializer:
--  { [ElementValueList] [,] }
      def "ElementValueArrayInitializer" $ list $ java "ElementValue",
--ElementValueList:
--  ElementValue {, ElementValue}

--MarkerAnnotation:
--  @ TypeName
      def "MarkerAnnotation" $ java "TypeName",

--SingleElementAnnotation:
      def "SingleElementAnnotation" $ record [
--  @ TypeName ( ElementValue )
        field "name" $ java "TypeName",
        field "value" $ optional $ java "ElementValue"],

--  Productions from §10 (Arrays)

--ArrayInitializer:
--  { [VariableInitializerList] [,] }
      def "ArrayInitializer" $ list $ list $ java "VariableInitializer",
--VariableInitializerList:
--  VariableInitializer {, VariableInitializer}

--Productions from §14 (Blocks and Statements)

--Block:
--  { [BlockStatements] }
      def "Block" $ list $ java "BlockStatement",

--BlockStatements:
--  BlockStatement {BlockStatement}
--BlockStatement:
      def "BlockStatement" $ union [
--  LocalVariableDeclarationStatement
        field "localVariableDeclaration" $ java "LocalVariableDeclarationStatement",
--  ClassDeclaration
        field "class" $ java "ClassDeclaration",
--  Statement
        field "statement" $ java "Statement"],

--LocalVariableDeclarationStatement:
--  LocalVariableDeclaration ;
      def "LocalVariableDeclarationStatement" $ java "LocalVariableDeclaration",

--LocalVariableDeclaration:
--  {VariableModifier} LocalVariableType VariableDeclaratorList
      def "LocalVariableDeclaration" $ record [
        field "modifiers" $ list $ java "VariableModifier",
        field "type" $ java "LocalVariableType",
        field "declarators" $ nonemptyList $ java "VariableDeclarator"],

--LocalVariableType:
      def "LocalVariableType" $ union [
--  UnannType
        field "type" $ java "UnannType",
--  var
        field "var" unit],

--Statement:
      def "Statement" $ union [
--  StatementWithoutTrailingSubstatement
        field "withoutTrailing" $ java "StatementWithoutTrailingSubstatement",
--  LabeledStatement
        field "labeled" $ java "LabeledStatement",
--  IfThenStatement
        field "ifThen" $ java "IfThenStatement",
--  IfThenElseStatement
        field "ifThenElse" $ java "IfThenElseStatement",
--  WhileStatement
        field "while" $ java "WhileStatement",
--  ForStatement
        field "for" $ java "ForStatement"],

--StatementNoShortIf:
      def "StatementNoShortIf" $ union [
--  StatementWithoutTrailingSubstatement
        field "withoutTrailing" $ java "StatementWithoutTrailingSubstatement",
--  LabeledStatementNoShortIf
        field "labeled" $ java "LabeledStatementNoShortIf",
--  IfThenElseStatementNoShortIf
        field "ifThenElse" $ java "IfThenElseStatementNoShortIf",
--  WhileStatementNoShortIf
        field "while" $ java "WhileStatementNoShortIf",
--  ForStatementNoShortIf
        field "for" $ java "ForStatementNoShortIf"],

--StatementWithoutTrailingSubstatement:
      def "StatementWithoutTrailingSubstatement" $ union [
--  Block
        field "block" $ java "Block",
--  EmptyStatement
        field "empty" $ java "EmptyStatement",
--  ExpressionStatement
        field "expression" $ java "ExpressionStatement",
--  AssertStatement
        field "assert" $ java "AssertStatement",
--  SwitchStatement
        field "switch" $ java "SwitchStatement",
--  DoStatement
        field "do" $ java "DoStatement",
--  BreakStatement
        field "break" $ java "BreakStatement",
--  ContinueStatement
        field "continue" $ java "ContinueStatement",
--  ReturnStatement
        field "return" $ java "ReturnStatement",
--  SynchronizedStatement
        field "synchronized" $ java "SynchronizedStatement",
--  ThrowStatement
        field "throw" $ java "ThrowStatement",
--  TryStatement
        field "try" $ java "TryStatement"],

--EmptyStatement:
--  ;
      def "EmptyStatement" unit,

--LabeledStatement:
--  Identifier : Statement
      def "LabeledStatement" $ record [
        field "identifier" $ java "Identifier",
        field "statement" $ java "Statement"],

--LabeledStatementNoShortIf:
--  Identifier : StatementNoShortIf
      def "LabeledStatementNoShortIf" $ record [
        field "identifier" $ java "Identifier",
        field "statement" $ java "StatementNoShortIf"],

--ExpressionStatement:
--  StatementExpression ;
      def "ExpressionStatement" $ java "StatementExpression",

--StatementExpression:
      def "StatementExpression" $ union [
--  Assignment
        field "assignment" $ java "Assignment",
--  PreIncrementExpression
        field "preIncrement" $ java "PreIncrementExpression",
--  PreDecrementExpression
        field "preDecrement" $ java "PreDecrementExpression",
--  PostIncrementExpression
        field "postIncrement" $ java "PostIncrementExpression",
--  PostDecrementExpression
        field "postDecrement" $ java "PostDecrementExpression",
--  MethodInvocation
        field "methodInvocation" $ java "MethodInvocation",
--  ClassInstanceCreationExpression
        field "classInstanceCreation" $ java "ClassInstanceCreationExpression"],

--IfThenStatement:
--  if ( Expression ) Statement
      def "IfThenStatement" $ record [
        field "expression" $ optional $ java "Expression",
        field "statement" $ java "Statement"],

--IfThenElseStatement:
--  if ( Expression ) StatementNoShortIf else Statement
      def "IfThenElseStatement" $ record [
        field "cond" $ optional $ java "Expression",
        field "then" $ java "StatementNoShortIf",
        field "else" $ java "Statement"],

--IfThenElseStatementNoShortIf:
--  if ( Expression ) StatementNoShortIf else StatementNoShortIf
      def "IfThenElseStatementNoShortIf" $ record [
        field "cond" $ optional $ java "Expression",
        field "then" $ java "StatementNoShortIf",
        field "else" $ java "StatementNoShortIf"],

--AssertStatement:
      def "AssertStatement" $ union [
--  assert Expression ;
        field "single" $ java "Expression",
--  assert Expression : Expression ;
        field "pair" $ java "AssertStatement.Pair"],
      def "AssertStatement.Pair" $ record [
        field "first" $ java "Expression",
        field "second" $ java "Expression"],

--SwitchStatement:
--  switch ( Expression ) SwitchBlock
      def "SwitchStatement" $ record [
        field "cond" $ java "Expression",
        field "block" $ java "SwitchBlock"],

--SwitchBlock:
--  { {SwitchBlockStatementGroup} {SwitchLabel} }
      def "SwitchBlock" $ list $ java "SwitchBlock.Pair",
      def "SwitchBlock.Pair" $ record [
        field "statements" $ list $ java "SwitchBlockStatementGroup",
        field "labels" $ list $ java "SwitchLabel"],

--SwitchBlockStatementGroup:
--  SwitchLabels BlockStatements
      def "SwitchBlockStatementGroup" $ record [
        field "labels" $ nonemptyList $ java "SwitchLabel",
        field "statements" $ nonemptyList $ java "BlockStatement"],

--SwitchLabels:
--  SwitchLabel {SwitchLabel}
--SwitchLabel:
      def "SwitchLabel" $ union [
--  case ConstantExpression :
        field "constant" $ java "ConstantExpression",
--  case EnumConstantName :
        field "enumConstant" $ java "EnumConstantName",
--  default :
        field "default" unit],

--EnumConstantName:
--  Identifier
      def "EnumConstantName" $ java "Identifier",

--WhileStatement:
--  while ( Expression ) Statement
      def "WhileStatement" $ record [
        field "cond" $ optional $ java "Expression",
        field "body" $ java "Statement"],

--WhileStatementNoShortIf:
--  while ( Expression ) StatementNoShortIf
      def "WhileStatementNoShortIf" $ record [
        field "cond" $ optional $ java "Expression",
        field "body" $ java "StatementNoShortIf"],

--DoStatement:
--  do Statement while ( Expression ) ;
      def "DoStatement" $ record [
        field "body" $ java "Statement",
        field "conde" $ optional $ java "Expression"],

--ForStatement:
      def "ForStatement" $ union [
--  BasicForStatement
        field "basic" $ java "BasicForStatement",
--  EnhancedForStatement
        field "enhanced" $ java "EnhancedForStatement"],

--ForStatementNoShortIf:
      def "ForStatementNoShortIf" $ union [
--  BasicForStatementNoShortIf
        field "basic" $ java "BasicForStatementNoShortIf",
--  EnhancedForStatementNoShortIf
        field "enhanced" $ java "EnhancedForStatementNoShortIf"],

--BasicForStatement:
--  for ( [ForInit] ; [Expression] ; [ForUpdate] ) Statement
      def "BasicForStatement" $ record [
        field "cond" $ java "ForCond",
        field "body" $ java "Statement"],
      def "ForCond" $ record [
        field "init" $ optional $ java "ForInit",
        field "cond" $ optional $ java "Expression",
        field "update" $ optional $ java "ForUpdate"],
--BasicForStatementNoShortIf:
--  for ( [ForInit] ; [Expression] ; [ForUpdate] ) StatementNoShortIf
      def "BasicForStatementNoShortIf" $ record [
        field "cond" $ java "ForCond",
        field "body" $ java "StatementNoShortIf"],

--ForInit:
      def "ForInit" $ union [
--  StatementExpressionList
        field "statements" $ nonemptyList $ java "StatementExpression",
--  LocalVariableDeclaration
        field "localVariable" $ java "LocalVariableDeclaration"],

--ForUpdate:
--  StatementExpressionList
      def "ForUpdate" $ nonemptyList $ java "StatementExpression",
--  StatementExpressionList:
--  StatementExpression {, StatementExpression}

--EnhancedForStatement:
      def "EnhancedForStatement" $ record [
--  for ( {VariableModifier} LocalVariableType VariableDeclaratorId : Expression ) Statement
        field "cond" $ java "EnhancedForCond",
        field "body" $ java "Statement"],
      def "EnhancedForCond" $ record [
        field "modifiers" $ list $ java "VariableModifier",
        field "type" $ java "LocalVariableType",
        field "id" $ java "VariableDeclaratorId",
        field "expression" $ java "Expression"],
--EnhancedForStatementNoShortIf:
--  for ( {VariableModifier} LocalVariableType VariableDeclaratorId : Expression ) StatementNoShortIf
      def "EnhancedForStatementNoShortIf" $ record [
        field "cond" $ java "EnhancedForCond",
        field "body" $ java "StatementNoShortIf"],

--BreakStatement:
--  break [Identifier] ;
      def "BreakStatement" $ optional $ java "Identifier",

--ContinueStatement:
--  continue [Identifier] ;
      def "ContinueStatement" $ optional $ java "Identifier",

--ReturnStatement:
--  return [Expression] ;
      def "ReturnStatement" $ optional $ java "Expression",

--ThrowStatement:
--  throw Expression ;
      def "ThrowStatement" $ java "Expression",

--SynchronizedStatement:
--  synchronized ( Expression ) Block
      def "SynchronizedStatement" $ record [
        field "expression" $ java "Expression",
        field "block" $ java "Block"],

--TryStatement:
      def "TryStatement" $ union [
--  try Block Catches
        field "simple" $ java "TryStatement.Simple",
--  try Block [Catches] Finally
        field "withFinally" $ java "TryStatement.WithFinally",
--  TryWithResourcesStatement
        field "withResources" $ java "TryWithResourcesStatement"],
      def "TryStatement.Simple" $ record [
        field "block" $ java "Block",
        field "catches" $ java "Catches"],
      def "TryStatement.WithFinally" $ record [
        field "block" $ java "Block",
        field "catches" $ optional $ java "Catches",
        field "finally" $ java "Finally"],

--Catches:
--  CatchClause {CatchClause}
      def "Catches" $ list $ java "CatchClause",

--CatchClause:
--  catch ( CatchFormalParameter ) Block
      def "CatchClause" $ record [
        field "parameter" $ optional $ java "CatchFormalParameter",
        field "block" $ java "Block"],

--CatchFormalParameter:
--  {VariableModifier} CatchType VariableDeclaratorId
      def "CatchFormalParameter" $ record [
        field "modifiers" $ list $ java "VariableModifier",
        field "type" $ java "CatchType",
        field "id" $ java "VariableDeclaratorId"],

--CatchType:
--  UnannClassType {| ClassType}
      def "CatchType" $ record [
        field "type" $ java "UnannClassType",
        field "types" $ list $ java "ClassType"],

--Finally:
--  finally Block
      def "Finally" $ java "Block",

--TryWithResourcesStatement:
--  try ResourceSpecification Block [Catches] [Finally]
      def "TryWithResourcesStatement" $ record [
        field "resourceSpecification" $ java "ResourceSpecification",
        field "block" $ java "Block",
        field "catches" $ optional $ java "Catches",
        field "finally" $ optional $ java "Finally"],

--ResourceSpecification:
--  ( ResourceList [;] )
      def "ResourceSpecification" $ list $ java "Resource",

--ResourceList:
--  Resource {; Resource}
--Resource:
      def "Resource" $ union [
--  {VariableModifier} LocalVariableType Identifier = Expression
        field "local" $ java "Resource.Local",
--  VariableAccess
        field "variable" $ java "VariableAccess"],
      def "Resource.Local" $ record [
        field "modifiers" $ list $ java "VariableModifier",
        field "type" $ java "LocalVariableType",
        field "identifier" $ java "Identifier",
        field "expression" $ java "Expression"],

--VariableAccess:
      def "VariableAccess" $ union [
--  ExpressionName
        field "expressionName" $ java "ExpressionName",
--  FieldAccess
        field "fieldAccess" $ java "FieldAccess"],

--Productions from §15 (Expressions)

--Primary:
      def "Primary" $ union [
--  PrimaryNoNewArray
        field "noNewArray" $ java "PrimaryNoNewArray",
--  ArrayCreationExpression
        field "arrayCreation" $ java "ArrayCreationExpression"],

--PrimaryNoNewArray:
      def "PrimaryNoNewArray" $ union [
--  Literal
        field "literal" $ java "Literal",
--  ClassLiteral
        field "classLiteral" $ java "ClassLiteral",
--  this
        field "this" unit,
--  TypeName . this
        field "dotThis" $ java "TypeName",
--  ( Expression )
        field "parens" $ java "Expression",
--  ClassInstanceCreationExpression
        field "classInstance" $ java "ClassInstanceCreationExpression",
--  FieldAccess
        field "fieldAccess" $ java "FieldAccess",
--  ArrayAccess
        field "arrayAccess" $ java "ArrayAccess",
--  MethodInvocation
        field "methodInvocation" $ java "MethodInvocation",
--  MethodReference
        field "methodReference" $ java "MethodReference"],

--ClassLiteral:
      def "ClassLiteral" $ union [
--  TypeName {[ ]} . class
        field "type" $ java "TypeNameArray",
--  NumericType {[ ]} . class
        field "numericType" $ java "NumericTypeArray",
--  boolean {[ ]} . class
        field "boolean" $ java "BooleanArray",
--  void . class
        field "void" unit],
      def "TypeNameArray" $ union [
        field "simple" $ java "TypeName",
        field "array" $ java "TypeNameArray"],
      def "NumericTypeArray" $ union [
        field "simple" $ java "NumericType",
        field "array" $ java "NumericTypeArray"],
      def "BooleanArray" $ union [
        field "simple" unit,
        field "array" $ java "BooleanArray"],

--ClassInstanceCreationExpression:
--  UnqualifiedClassInstanceCreationExpression
--  ExpressionName . UnqualifiedClassInstanceCreationExpression
--  Primary . UnqualifiedClassInstanceCreationExpression
      def "ClassInstanceCreationExpression" $ record [
        field "qualifier" $ optional $ java "ClassInstanceCreationExpression.Qualifier",
        field "expression" $ java "UnqualifiedClassInstanceCreationExpression"],
      def "ClassInstanceCreationExpression.Qualifier" $ union [
        field "expression" $ java "ExpressionName",
        field "primary" $ java "Primary"],

--UnqualifiedClassInstanceCreationExpression:
--  new [TypeArguments] ClassOrInterfaceTypeToInstantiate ( [ArgumentList] ) [ClassBody]
      def "UnqualifiedClassInstanceCreationExpression" $ record [
        field "typeArguments" $ list $ java "TypeArgument",
        field "classOrInterface" $ java "ClassOrInterfaceTypeToInstantiate",
        field "arguments" $ list $ java "Expression",
        field "body" $ optional $ java "ClassBody"],

--ClassOrInterfaceTypeToInstantiate:
--  {Annotation} Identifier {. {Annotation} Identifier} [TypeArgumentsOrDiamond]
      def "ClassOrInterfaceTypeToInstantiate" $ record [
        field "identifiers" $ nonemptyList $ java "AnnotatedIdentifier",
        field "typeArguments" $ optional $ java "TypeArgumentsOrDiamond"],
      def "AnnotatedIdentifier" $ record [
        field "annotations" $ list $ java "Annotation",
        field "identifier" $ java "Identifier"],

--TypeArgumentsOrDiamond:
      def "TypeArgumentsOrDiamond" $ union [
--  TypeArguments
        field "arguments" $ nonemptyList $ java "TypeArgument",
--  <>
        field "diamond" unit],

--FieldAccess:
      def "FieldAccess" $ record [
        field "identifier" $ java "Identifier",
        field "variant" $ java "FieldAccess.Variant"],
      def "FieldAccess.Variant" $ union [
--  Primary . Identifier
        field "primary" $ java "Primary",
--  super . Identifier
        field "super" unit,
--  TypeName . super . Identifier
        field "typed" $ java "TypeName"],

--ArrayAccess:
      def "ArrayAccess" $ record [
        field "expression" $ optional $ java "Expression",
        field "variant" $ java "ArrayAccess.Variant"],
      def "ArrayAccess.Variant" $ union [
--  ExpressionName [ Expression ]
        field "name" $ java "ExpressionName",
--  PrimaryNoNewArray [ Expression ]
        field "primary" $ java "PrimaryNoNewArray"],

--MethodInvocation:
      def "MethodInvocation" $ record [
        field "id" $
          doc "Note: no id or type arguments for an invocation by method name" $
          list $ java "MethodInvocation.Id",
        field "arguments" $ list $ java "Expression",
        field "variant" $ java "MethodInvocation.Variant"],
      def "MethodInvocation.Variant" $ union [
--  MethodName ( [ArgumentList] )
        field "method" $ java "MethodName",
--  TypeName . [TypeArguments] Identifier ( [ArgumentList] )
        field "type" $ java "TypeName",
--  ExpressionName . [TypeArguments] Identifier ( [ArgumentList] )
        field "expression" $ java "ExpressionName",
--  Primary . [TypeArguments] Identifier ( [ArgumentList] )
        field "primary" $ java "Primary",
--  super . [TypeArguments] Identifier ( [ArgumentList] )
        field "super" unit,
--  TypeName . super . [TypeArguments] Identifier ( [ArgumentList] )
        field "typeSuper" $ java "TypeName"],
      def "MethodInvocation.Id" $ record [
        field "typeArguments" $ list $ java "TypeArgument",
        field "identifier" $ java "Identifier"],

--ArgumentList:
--  Expression {, Expression}

--MethodReference:
      def "MethodReference" $ union [
--  ExpressionName :: [TypeArguments] Identifier
        field "expression" $ java "MethodReference.Expression",
--  Primary :: [TypeArguments] Identifier
        field "primary" $ java "MethodReference.Primary",
--  ReferenceType :: [TypeArguments] Identifier
        field "referenceType" $ java"MethodReference.ReferenceType",
--  super :: [TypeArguments] Identifier
--  TypeName . super :: [TypeArguments] Identifier
        field "super" $ java "MethodReference.Super",
--  ClassType :: [TypeArguments] new
        field "new" $ java "MethodReference.New",
--  ArrayType :: new
        field "array" $ java "MethodReference.Array"],
      def "MethodReference.Expression" $ record [
        field "name" $ java "ExpressionName",
        field "typeArguments" $ list $ java "TypeArgument",
        field "identifier" $ java "Identifier"],
      def "MethodReference.Primary" $ record [
        field "primary" $ java "Primary",
        field "typeArguments" $ list $ java "TypeArgument",
        field "identifier" $ java "Identifier"],
      def "MethodReference.ReferenceType" $ record [
        field "referenceType" $ java "ReferenceType",
        field "typeArguments" $ list $ java "TypeArgument",
        field "identifier" $ java "Identifier"],
      def "MethodReference.Super" $ record [
        field "typeArguments" $ list $ java "TypeArgument",
        field "identifier" $ java "Identifier",
        field "super" boolean],
      def "MethodReference.New" $ record [
        field "classType" $ java "ClassType",
        field "typeArguments" $ list $ java "TypeArgument"],
      def "MethodReference.Array" $ java "ArrayType",

--ArrayCreationExpression:
      def "ArrayCreationExpression" $ union [
--  new PrimitiveType DimExprs [Dims]
        field "primitive" $ java "ArrayCreationExpression.Primitive",
--  new ClassOrInterfaceType DimExprs [Dims]
        field "classOrInterface" $ java "ArrayCreationExpression.ClassOrInterface",
--  new PrimitiveType Dims ArrayInitializer
        field "primitiveArray" $ java "ArrayCreationExpression.PrimitiveArray",
--  new ClassOrInterfaceType Dims ArrayInitializer
        field "classOrInterfaceArray" $ java "ArrayCreationExpression.ClassOrInterfaceArray"],
      def "ArrayCreationExpression.Primitive" $ record [
        field "type" $ java "PrimitiveTypeWithAnnotations",
        field "dimExprs" $ nonemptyList $ java "DimExpr",
        field "dims" $ optional $ java "Dims"],
      def "ArrayCreationExpression.ClassOrInterface" $ record [
        field "type" $ java "ClassOrInterfaceType",
        field "dimExprs" $ nonemptyList $ java "DimExpr",
        field "dims" $ optional $ java "Dims"],
      def "ArrayCreationExpression.PrimitiveArray" $ record [
        field "type" $ java "PrimitiveTypeWithAnnotations",
        field "dims" $ nonemptyList $ java "Dims",
        field "array" $ java "ArrayInitializer"],
      def "ArrayCreationExpression.ClassOrInterfaceArray" $ record [
        field "type" $ java "ClassOrInterfaceType",
        field "dims" $ nonemptyList $ java "Dims",
        field "array" $ java "ArrayInitializer"],

--DimExprs:
--  DimExpr {DimExpr}
--DimExpr:
--  {Annotation} [ Expression ]
      def "DimExpr" $ record [
        field "annotations" $ list $ java "Annotation",
        field "expression" $ optional $ java "Expression"],

--Expression:
      def "Expression" $ union [
--  LambdaExpression
        field "lambda" $ java "LambdaExpression",
--  AssignmentExpression
        field "assignment" $ java "AssignmentExpression"],

--LambdaExpression:
--  LambdaParameters -> LambdaBody
      def "LambdaExpression" $ record [
        field "parameters" $ java "LambdaParameters",
        field "body" $ java "LambdaBody"],

--LambdaParameters:
--  ( [LambdaParameterList] )
--  Identifier
      def "LambdaParameters" $ union [
        field "tuple" $ list $ java "LambdaParameters",
        field "single" $ java "Identifier"],

--LambdaParameterList:
--  LambdaParameter {, LambdaParameter}
--  Identifier {, Identifier}
--LambdaParameter:
      def "LambdaParameter" $ union [
--  {VariableModifier} LambdaParameterType VariableDeclaratorId
        field "normal" $ java "LambdaParameter.Normal",
--  VariableArityParameter
        field "variableArity" $ java "VariableArityParameter"],
      def "LambdaParameter.Normal" $ record [
        field "modifiers" $ list $ java "VariableModifier",
        field "type" $ java "LambdaParameterType",
        field "id" $ java "VariableDeclaratorId"],

--LambdaParameterType:
      def "LambdaParameterType" $ union [
--  UnannType
        field "type" $ java "UnannType",
--  var
        field "var" unit],

--LambdaBody:
      def "LambdaBody" $ union [
--  Expression
        field "expression" $ java "Expression",
--  Block
        field "block" $ java "Block"],

--AssignmentExpression:
      def "AssignmentExpression" $ union [
--  ConditionalExpression
        field "conditional" $ java "ConditionalExpression",
--  Assignment
        field "assignment" $ java "Assignment"],

--Assignment:
--  LeftHandSide AssignmentOperator Expression
      def "Assignment" $ record [
        field "lhs" $ java "LeftHandSide",
        field "op" $ java "AssignmentOperator",
        field "expression" $ java "Expression"],

--LeftHandSide:
      def "LeftHandSide" $ union [
--  ExpressionName
        field "expressionName" $ java "ExpressionName",
--  FieldAccess
        field "fieldAccess" $ java "FieldAccess",
--  ArrayAccess
        field "arrayAccess" $ java "ArrayAccess"],

--AssignmentOperator:
--  (one of)
      def "AssignmentOperator" $ enum [
--  =  *=  /=  %=  +=  -=  <<=  >>=  >>>=  &=  ^=  |=
        "simple", "times", "div", "mod", "plus", "minus",
        "shiftLeft", "shiftRight", "shiftRightZeroFill", "and", "xor", "or"],

--ConditionalExpression:
      def "ConditionalExpression" $ union [
--  ConditionalOrExpression
        field "simple" $ java "ConditionalOrExpression",
--  ConditionalOrExpression ? Expression : ConditionalExpression
        field "ternaryCond" $ java "ConditionalExpression.TernaryCond",
--  ConditionalOrExpression ? Expression : LambdaExpression
        field "ternaryLambda" $ java "ConditionalExpression.TernaryLambda"],
      def "ConditionalExpression.TernaryCond" $ record [
        field "cond" $ java "ConditionalOrExpression",
        field "ifTrue" $ java "Expression",
        field "ifFalse" $ java "ConditionalExpression"],
      def "ConditionalExpression.TernaryLambda" $ record [
        field "cond" $ java "ConditionalOrExpression",
        field "ifTrue" $ java "Expression",
        field "ifFalse" $ java "LambdaExpression"],

--ConditionalOrExpression:
--  ConditionalAndExpression
--  ConditionalOrExpression || ConditionalAndExpression
      def "ConditionalOrExpression" $ nonemptyList $ java "ConditionalAndExpression",

--ConditionalAndExpression:
--  InclusiveOrExpression
--  ConditionalAndExpression && InclusiveOrExpression
      def "ConditionalAndExpression" $ nonemptyList $ java "InclusiveOrExpression",

--InclusiveOrExpression:
--  ExclusiveOrExpression
--  InclusiveOrExpression | ExclusiveOrExpression
      def "InclusiveOrExpression" $ nonemptyList $ java "ExclusiveOrExpression",

--ExclusiveOrExpression:
--  AndExpression
--  ExclusiveOrExpression ^ AndExpression
      def "ExclusiveOrExpression" $ nonemptyList $ java "AndExpression",

--AndExpression:
--  EqualityExpression
--  AndExpression & EqualityExpression
      def "AndExpression" $ nonemptyList $ java "EqualityExpression",

--EqualityExpression:
      def "EqualityExpression" $ union [
--  RelationalExpression
        field "unary" $ java "RelationalExpression",
--  EqualityExpression == RelationalExpression
        field "equal" $ java "EqualityExpression.Binary",
--  EqualityExpression != RelationalExpression
        field "notEqual" $ java "EqualityExpression.Binary"],
      def "EqualityExpression.Binary" $ record [
        field "lhs" $ java "EqualityExpression",
        field "rhs" $ java "RelationalExpression"],

--RelationalExpression:
      def "RelationalExpression" $ union [
--  ShiftExpression
        field "simple" $ java "ShiftExpression",
--  RelationalExpression < ShiftExpression
        field "lessThan" $ java "RelationalExpression.LessThan",
--  RelationalExpression > ShiftExpression
        field "greaterThan" $ java "RelationalExpression.GreaterThan",
--  RelationalExpression <= ShiftExpression
        field "lessThanEqual" $ java "RelationalExpression.LessThanEqual",
--  RelationalExpression >= ShiftExpression
        field "greaterThanEqual" $ java "RelationalExpression.GreaterThanEqual",
--  RelationalExpression instanceof ReferenceType
        field "instanceof" $ java "RelationalExpression.InstanceOf"],
      def "RelationalExpression.LessThan" $ record [
        field "lhs" $ java "RelationalExpression",
        field "rhs" $ java "ShiftExpression"],
      def "RelationalExpression.GreaterThan" $ record [
        field "lhs" $ java "RelationalExpression",
        field "rhs" $ java "ShiftExpression"],
      def "RelationalExpression.LessThanEqual" $ record [
        field "lhs" $ java "RelationalExpression",
        field "rhs" $ java "ShiftExpression"],
      def "RelationalExpression.GreaterThanEqual" $ record [
        field "lhs" $ java "RelationalExpression",
        field "rhs" $ java "ShiftExpression"],
      def "RelationalExpression.InstanceOf" $ record [
        field "lhs" $ java "RelationalExpression",
        field "rhs" $ java "ReferenceType"],

--ShiftExpression:
      def "ShiftExpression" $ union [
--  AdditiveExpression
        field "unary" $ java "AdditiveExpression",
--  ShiftExpression << AdditiveExpression
        field "shiftLeft" $ java "ShiftExpression.Binary",
--  ShiftExpression >> AdditiveExpression
        field "shiftRight" $ java "ShiftExpression.Binary",
--  ShiftExpression >>> AdditiveExpression
        field "shiftRightZeroFill" $ java "ShiftExpression.Binary"],
      def "ShiftExpression.Binary" $ record [
        field "lhs" $ java "ShiftExpression",
        field "rhs" $ java "AdditiveExpression"],

--AdditiveExpression:
      def "AdditiveExpression" $ union [
--  MultiplicativeExpression
        field "unary" $ java "MultiplicativeExpression",
--  AdditiveExpression + MultiplicativeExpression
        field "plus" $ java "AdditiveExpression.Binary",
--  AdditiveExpression - MultiplicativeExpression
        field "minus" $ java "AdditiveExpression.Binary"],
      def "AdditiveExpression.Binary" $ record [
        field "lhs" $ java "AdditiveExpression",
        field "rhs" $ java "MultiplicativeExpression"],

--MultiplicativeExpression:
      def "MultiplicativeExpression" $ union [
--  UnaryExpression
        field "unary" $ java "UnaryExpression",
--  MultiplicativeExpression * UnaryExpression
        field "times" $ java "MultiplicativeExpression.Binary",
--  MultiplicativeExpression / UnaryExpression
        field "divide" $ java "MultiplicativeExpression.Binary",
--  MultiplicativeExpression % UnaryExpression
        field "mod" $ java "MultiplicativeExpression.Binary"],
      def "MultiplicativeExpression.Binary" $ record [
        field "lhs" $ java "MultiplicativeExpression",
        field "rhs" $ java "UnaryExpression"],

--UnaryExpression:
      def "UnaryExpression" $ union [
--  PreIncrementExpression
        field "preIncrement" $ java "PreIncrementExpression",
--  PreDecrementExpression
        field "preDecrement" $ java "PreDecrementExpression",
--  + UnaryExpression
        field "plus" $ java "UnaryExpression",
--  - UnaryExpression
        field "minus" $ java "UnaryExpression",
--  UnaryExpressionNotPlusMinus
        field "other" $ java "UnaryExpressionNotPlusMinus"],

--PreIncrementExpression:
--  ++ UnaryExpression
      def "PreIncrementExpression" $ java "UnaryExpression",

--PreDecrementExpression:
--  -- UnaryExpression
      def "PreDecrementExpression" $ java "UnaryExpression",

--UnaryExpressionNotPlusMinus:
      def "UnaryExpressionNotPlusMinus" $ union [
--  PostfixExpression
        field "postfix" $ java "PostfixExpression",
--  ~ UnaryExpression
        field "tilde" $ java "UnaryExpression",
--  ! UnaryExpression
        field "not" $ java "UnaryExpression",
--  CastExpression
        field "cast" $ java "CastExpression"],

--PostfixExpression:
      def "PostfixExpression" $ union [
--  Primary
        field "primary" $ java "Primary",
--  ExpressionName
        field "name" $ java "ExpressionName",
--  PostIncrementExpression
        field "postIncrement" $ java "PostIncrementExpression",
--  PostDecrementExpression
        field "postDecrement" $ java "PostDecrementExpression"],

--PostIncrementExpression:
--  PostfixExpression ++
      def "PostIncrementExpression" $ java "PostfixExpression",

--PostDecrementExpression:
--  PostfixExpression --
      def "PostDecrementExpression" $ java "PostfixExpression",

--CastExpression:
      def "CastExpression" $ union [
--  ( PrimitiveType ) UnaryExpression
        field "primitive" $ java "CastExpression.Primitive",
--  ( ReferenceType {AdditionalBound} ) UnaryExpressionNotPlusMinus
        field "notPlusMinus" $ java "CastExpression.NotPlusMinus",
--  ( ReferenceType {AdditionalBound} ) LambdaExpression
        field "lambda" $ java "CastExpression.Lambda"],
      def "CastExpression.Primitive" $ record [
        field "type" $ optional $ java "PrimitiveTypeWithAnnotations",
        field "expression" $ java "UnaryExpression"],
      def "CastExpression.NotPlusMinus" $ record [
        field "refAndBounds" $ optional $ java "CastExpression.RefAndBounds",
        field "expression" $ java "UnaryExpression"],
      def "CastExpression.Lambda" $ record [
        field "refAndBounds" $ optional $ java "CastExpression.RefAndBounds",
        field "expression" $ java "LambdaExpression"],
      def "CastExpression.RefAndBounds" $ record [
        field "type" $ java "ReferenceType",
        field "bounds" $ list $ java "AdditionalBound"],

--ConstantExpression:
--  Expression
      def "ConstantExpression" $ java "Expression"]
