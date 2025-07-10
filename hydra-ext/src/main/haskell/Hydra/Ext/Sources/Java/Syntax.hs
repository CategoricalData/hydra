module Hydra.Ext.Sources.Java.Syntax where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap

import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Mantle      as Mantle
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow


javaSyntaxModule :: Module
javaSyntaxModule = Module ns elements [Core.module_] [Core.module_] $
    Just ("A Java syntax module. Based on the Oracle Java SE 12 BNF:\n" ++
      "  https://docs.oracle.com/javase/specs/jls/se12/html/jls-19.html\n" ++
      "Note: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments.")
  where
    ns = Namespace "hydra.ext.java.syntax"
    def = datatype ns
    java = typeref ns

    elements = [

--Productions from §3 (Lexical Structure)

--Identifier:
--  IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
      def "Identifier" $ wrap string,
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
      def "TypeIdentifier" $ wrap $ java "Identifier",

--Literal:
      def "Literal" $
        union [
--  NullLiteral
          "null">: unit,
--  IntegerLiteral
          "integer">: java "IntegerLiteral",
--  FloatingPointLiteral
          "floatingPoint">: java "FloatingPointLiteral",
--  BooleanLiteral
          "boolean">: boolean,
--  CharacterLiteral
          "character">: uint16,
--  StringLiteral
          "string">: java "StringLiteral"],
      def "IntegerLiteral" $
        doc "Note: this is an approximation which ignores encoding" $
        wrap bigint,
      def "FloatingPointLiteral" $
        doc "Note: this is an approximation which ignores encoding" $
        wrap bigfloat,
      def "StringLiteral" $
        doc "Note: this is an approximation which ignores encoding" $
        wrap string,

--Productions from §4 (Types, Values, and Variables)

--Type:
      def "Type" $ union [
--  PrimitiveType
          "primitive">: java "PrimitiveTypeWithAnnotations",
--  ReferenceType
          "reference">: java "ReferenceType"],

--PrimitiveType:
      def "PrimitiveTypeWithAnnotations" $ record [
        "type">: java "PrimitiveType",
        "annotations">: list $ java "Annotation"],
      def "PrimitiveType" $ union [
--  {Annotation} NumericType
        "numeric">: java "NumericType",
--  {Annotation} boolean
        "boolean">: unit],

--NumericType:
      def "NumericType" $ union [
--  IntegralType
        "integral">: java "IntegralType",
--  FloatingPointType
        "floatingPoint">: java "FloatingPointType"],

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
        "classOrInterface">: java "ClassOrInterfaceType",
--  TypeVariable
        "variable">: java "TypeVariable",
--  ArrayType
        "array">: java "ArrayType"],

--ClassOrInterfaceType:
      def "ClassOrInterfaceType" $ union [
--  ClassType
        "class">: java "ClassType",
--  InterfaceType
        "interface">: java "InterfaceType"],

--ClassType:
      def "ClassType" $ record [
        "annotations">: list $ java "Annotation",
        "qualifier">: java "ClassTypeQualifier",
        "identifier">: java "TypeIdentifier",
        "arguments">: list $ java "TypeArgument"],
      def "ClassTypeQualifier" $ union [
--  {Annotation} TypeIdentifier [TypeArguments]
        "none">: unit,
--  PackageName . {Annotation} TypeIdentifier [TypeArguments]
        "package">: java "PackageName",
--  ClassOrInterfaceType . {Annotation} TypeIdentifier [TypeArguments]
        "parent">: java "ClassOrInterfaceType"],

--InterfaceType:
--  ClassType
      def "InterfaceType" $ wrap $ java "ClassType",

--TypeVariable:
--  {Annotation} TypeIdentifier
      def "TypeVariable" $ record [
        "annotations">: list $ java "Annotation",
        "identifier">: java "TypeIdentifier"],

--ArrayType:
      def "ArrayType" $ record [
        "dims">: java "Dims",
        "variant">: java "ArrayType_Variant"],
      def "ArrayType_Variant" $ union [
--  PrimitiveType Dims
        "primitive">: java "PrimitiveTypeWithAnnotations",
--  ClassOrInterfaceType Dims
        "classOrInterface">: java "ClassOrInterfaceType",
--  TypeVariable Dims
        "variable">: java "TypeVariable"],

--Dims:
--  {Annotation} [ ] {{Annotation} [ ]}
      def "Dims" $ wrap $ list $ list $ java "Annotation",

--TypeParameter:
--  {TypeParameterModifier} TypeIdentifier [TypeBound]
      def "TypeParameter" $ record [
        "modifiers">: list $ java "TypeParameterModifier",
        "identifier">: java "TypeIdentifier",
        "bound">: optional $ java "TypeBound"],

--TypeParameterModifier:
--  Annotation
      def "TypeParameterModifier" $ wrap $ java "Annotation",

--TypeBound:
      def "TypeBound" $ union [
--  extends TypeVariable
        "variable">: java "TypeVariable",
--  extends ClassOrInterfaceType {AdditionalBound}
        "classOrInterface">: java "TypeBound_ClassOrInterface"],
      def "TypeBound_ClassOrInterface" $ record [
        "type">: java "ClassOrInterfaceType",
        "additional">: list $ java "AdditionalBound"],

--AdditionalBound:
--  & InterfaceType
      def "AdditionalBound" $ wrap $ java "InterfaceType",

--TypeArguments:
--  < TypeArgumentList >
--TypeArgumentList:
--  TypeArgument {, TypeArgument}

--TypeArgument:
      def "TypeArgument" $ union [
--  ReferenceType
        "reference">: java "ReferenceType",
--  Wildcard
        "wildcard">: java "Wildcard"],

--Wildcard:
--  {Annotation} ? [WildcardBounds]
      def "Wildcard" $ record [
        "annotations">: list $ java "Annotation",
        "wildcard">: optional $ java "WildcardBounds"],

--WildcardBounds:
      def "WildcardBounds" $ union [
--  extends ReferenceType
        "extends">: java "ReferenceType",
--  super ReferenceType
        "super">: java "ReferenceType"],

--Productions from §6 (Names)

--ModuleName:
      def "ModuleName" $ record [
--  Identifier
        "identifier">: java "Identifier",
--  ModuleName . Identifier
        "name">: optional $ java "ModuleName"],

--PackageName:
--  Identifier
--  PackageName . Identifier
      def "PackageName" $ wrap $ list $ java "Identifier",

--TypeName:
      def "TypeName" $ record [
--  TypeIdentifier
        "identifier">: java "TypeIdentifier",
--  PackageOrTypeName . TypeIdentifier
        "qualifier">: optional $ java "PackageOrTypeName"],

--ExpressionName:
--  Identifier
--  AmbiguousName . Identifier
      def "ExpressionName" $ record [
        "qualifier">: optional $ java "AmbiguousName",
        "identifier">: java "Identifier"],

--MethodName:
--  Identifier
      def "MethodName" $ wrap $ java "Identifier",

--PackageOrTypeName:
--  Identifier
--  PackageOrTypeName . Identifier
      def "PackageOrTypeName" $ wrap $ list $ java "Identifier",

--AmbiguousName:
--  Identifier
--  AmbiguousName . Identifier
      def "AmbiguousName" $ wrap $ list $ java "Identifier",

--Productions from §7 (Packages and Modules)

--CompilationUnit:
      def "CompilationUnit" $ union [
--  OrdinaryCompilationUnit
        "ordinary">: java "OrdinaryCompilationUnit",
--  ModularCompilationUnit
        "modular">: java "ModularCompilationUnit"],

--OrdinaryCompilationUnit:
--  [PackageDeclaration] {ImportDeclaration} {TypeDeclaration}
      def "OrdinaryCompilationUnit" $ record [
        "package">: optional $ java "PackageDeclaration",
        "imports">: list $ java "ImportDeclaration",
        "types">: list $ java "TypeDeclarationWithComments"],

--ModularCompilationUnit:
--  {ImportDeclaration} ModuleDeclaration
      def "ModularCompilationUnit" $ record [
        "imports">: list $ java "ImportDeclaration",
        "module">: java "ModuleDeclaration"],

--PackageDeclaration:
--  {PackageModifier} package Identifier {. Identifier} ;
      def "PackageDeclaration" $ record [
        "modifiers">: list $ java "PackageModifier",
        "identifiers">: list $ java "Identifier"],

--PackageModifier:
--  Annotation
      def "PackageModifier" $ wrap $ java "Annotation",

--ImportDeclaration:
      def "ImportDeclaration" $ union [
--  SingleTypeImportDeclaration
        "singleType">: java "SingleTypeImportDeclaration",
--  TypeImportOnDemandDeclaration
        "typeImportOnDemand">: java "TypeImportOnDemandDeclaration",
--  SingleStaticImportDeclaration
        "singleStaticImport">: java "SingleStaticImportDeclaration",
--  StaticImportOnDemandDeclaration
        "staticImportOnDemand">: java "StaticImportOnDemandDeclaration"],

--SingleTypeImportDeclaration:
--  import TypeName ;
      def "SingleTypeImportDeclaration" $ wrap $ java "TypeName",

--TypeImportOnDemandDeclaration:
--  import PackageOrTypeName . * ;
      def "TypeImportOnDemandDeclaration" $ wrap $ java "PackageOrTypeName",

--SingleStaticImportDeclaration:
--  import static TypeName . Identifier ;
      def "SingleStaticImportDeclaration" $ record [
        "typeName">: java "TypeName",
        "identifier">: java "Identifier"],

--StaticImportOnDemandDeclaration:
--  import static TypeName . * ;
      def "StaticImportOnDemandDeclaration" $ wrap $ java "TypeName",

--TypeDeclaration:
      def "TypeDeclaration" $ union [
--  ClassDeclaration
        "class">: java "ClassDeclaration",
--  InterfaceDeclaration
        "interface">: java "InterfaceDeclaration",
--  ;
        "none">: unit],
      def "TypeDeclarationWithComments" $
        record [
          "value">: java "TypeDeclaration",
          "comments">: optional string],

--ModuleDeclaration:
--  {Annotation} [open] module Identifier {. Identifier} { {ModuleDirective} }
      def "ModuleDeclaration" $ record [
        "annotations">: list $ java "Annotation",
        "open">: boolean,
        "identifiers">: list $ java "Identifier",
        "directives">: list $ list $ java "ModuleDirective"],

--ModuleDirective:
      def "ModuleDirective" $ union [
--  requires {RequiresModifier} ModuleName ;
        "requires">: java "ModuleDirective_Requires",
--  exports PackageName [to ModuleName {, ModuleName}] ;
        "exports">: java "ModuleDirective_ExportsOrOpens",
--  opens PackageName [to ModuleName {, ModuleName}] ;
        "opens">: java "ModuleDirective_ExportsOrOpens",
--  uses TypeName ;
        "uses">: java "TypeName",
--  provides TypeName with TypeName {, TypeName} ;
        "provides">: java "ModuleDirective_Provides"],
      def "ModuleDirective_Requires" $ record [
        "modifiers">: list $ java "RequiresModifier",
        "module">: java "ModuleName"],
      def "ModuleDirective_ExportsOrOpens" $ record [
        "package">: java "PackageName",
        "modules">:
          doc "At least one module" $
          list $ java "ModuleName"],
      def "ModuleDirective_Provides" $ record [
        "to">: java "TypeName",
        "with">:
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
        "normal">: java "NormalClassDeclaration",
--  EnumDeclaration
        "enum">: java "EnumDeclaration"],

--NormalClassDeclaration:
--  {ClassModifier} class TypeIdentifier [TypeParameters] [Superclass] [Superinterfaces] ClassBody
      def "NormalClassDeclaration" $ record [
        "modifiers">: list $ java "ClassModifier",
        "identifier">: java "TypeIdentifier",
        "parameters">: list $ java "TypeParameter",
        "extends">: optional $ java "ClassType",
        "implements">: list $ java "InterfaceType",
        "body">: java "ClassBody"],

--ClassModifier:
      def "ClassModifier" $ union [
--  (one of)
--  Annotation public protected private
--  abstract static final strictfp
        "annotation">: java "Annotation",
        "public">: unit,
        "protected">: unit,
        "private">: unit,
        "abstract">: unit,
        "static">: unit,
        "final">: unit,
        "strictfp">: unit],

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
      def "ClassBody" $ wrap $ list $ java "ClassBodyDeclarationWithComments",

--ClassBodyDeclaration:
      def "ClassBodyDeclaration" $ union [
--  ClassMemberDeclaration
        "classMember">: java "ClassMemberDeclaration",
--  InstanceInitializer
        "instanceInitializer">: java "InstanceInitializer",
--  StaticInitializer
        "staticInitializer">: java "StaticInitializer",
--  ConstructorDeclaration
        "constructorDeclaration">: java "ConstructorDeclaration"],
      def "ClassBodyDeclarationWithComments" $
        record [
          "value">: java "ClassBodyDeclaration",
          "comments">: optional string],

--ClassMemberDeclaration:
      def "ClassMemberDeclaration" $ union [
--  FieldDeclaration
        "field">: java "FieldDeclaration",
--  MethodDeclaration
        "method">: java "MethodDeclaration",
--  ClassDeclaration
        "class">: java "ClassDeclaration",
--  InterfaceDeclaration
        "interface">: java "InterfaceDeclaration",
--  ;
        "none">: unit],

--FieldDeclaration:
--  {FieldModifier} UnannType VariableDeclaratorList ;
      def "FieldDeclaration" $ record [
        "modifiers">: list $ java "FieldModifier",
        "unannType">: java "UnannType",
        "variableDeclarators">: nonemptyList $ java "VariableDeclarator"],

--FieldModifier:
--  (one of)
      def "FieldModifier" $ union [
--  Annotation public protected private
--  static final transient volatile
        "annotation">: java "Annotation",
        "public">: unit,
        "protected">: unit,
        "private">: unit,
        "static">: unit,
        "final">: unit,
        "transient">: unit,
        "volatile">: unit],

--VariableDeclaratorList:
--  VariableDeclarator {, VariableDeclarator}
--VariableDeclarator:
--  VariableDeclaratorId [= VariableInitializer]
      def "VariableDeclarator" $ record [
        "id">: java "VariableDeclaratorId",
        "initializer">: optional $ java "VariableInitializer"],

--VariableDeclaratorId:
--  Identifier [Dims]
      def "VariableDeclaratorId" $ record [
        "identifier">: java "Identifier",
        "dims">: optional $ java "Dims"],

--VariableInitializer:
      def "VariableInitializer" $ union [
--  Expression
        "expression">: java "Expression",
--  ArrayInitializer
        "arrayInitializer">: java "ArrayInitializer"],

--UnannType:
--  UnannPrimitiveType
--  UnannReferenceType
      def "UnannType" $
        doc "A Type which does not allow annotations" $
        wrap $ java "Type",
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
        wrap $ java "ClassType",
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
        "annotations">:
          doc "Note: simple methods cannot have annotations" $
          list $ java "Annotation",
        "modifiers">: list $ java "MethodModifier",
        "header">: java "MethodHeader",
        "body">: java "MethodBody"],

--MethodModifier:
--  (one of)
      def "MethodModifier" $ union [
--  Annotation public protected private
--  abstract static final synchronized native strictfp
        "annotation">: java "Annotation",
        "public">: unit,
        "protected">: unit,
        "private">: unit,
        "abstract">: unit,
        "static">: unit,
        "final">: unit,
        "synchronized">: unit,
        "native">: unit,
        "strictfb">: unit],

--MethodHeader:
--  Result MethodDeclarator [Throws]
--  TypeParameters {Annotation} Result MethodDeclarator [Throws]
      def "MethodHeader" $ record [
        "parameters">: list $ java "TypeParameter",
        "result">: java "Result",
        "declarator">: java "MethodDeclarator",
        "throws">: optional $ java "Throws"],

--Result:
      def "Result" $ union [
--  UnannType
        "type">: java "UnannType",
--  void
        "void">: unit],

--MethodDeclarator:
--  Identifier ( [ReceiverParameter ,] [FormalParameterList] ) [Dims]
      def "MethodDeclarator" $ record [
        "identifier">: java "Identifier",
        "receiverParameter">: optional $ java "ReceiverParameter",
        "formalParameters">: nonemptyList $ java "FormalParameter"],

--ReceiverParameter:
--  {Annotation} UnannType [Identifier .] this
      def "ReceiverParameter" $ record [
        "annotations">: list $ java "Annotation",
        "unannType">: java "UnannType",
        "identifier">: optional $ java "Identifier"],

--FormalParameterList:
--  FormalParameter {, FormalParameter}
--FormalParameter:
      def "FormalParameter" $ union [
--  {VariableModifier} UnannType VariableDeclaratorId
        "simple">: java "FormalParameter_Simple",
--  VariableArityParameter
        "variableArity">: java "VariableArityParameter"],
      def "FormalParameter_Simple" $ record [
        "modifiers">: list $ java "VariableModifier",
        "type">: java "UnannType",
        "id">: java "VariableDeclaratorId"],

--VariableArityParameter:
--  {VariableModifier} UnannType {Annotation} ... Identifier
      def "VariableArityParameter" $ record [
        "modifiers">: java "VariableModifier",
        "type">: java "UnannType",
        "annotations">: list $ java "Annotation",
        "identifier">: java "Identifier"],

--VariableModifier:
      def "VariableModifier" $ union [
--  Annotation
        "annotation">: java "Annotation",
--  final
        "final">: unit],

--Throws:
--  throws ExceptionTypeList
      def "Throws" $ wrap $ nonemptyList $ java "ExceptionType",

--ExceptionTypeList:
--  ExceptionType {, ExceptionType}
--ExceptionType:
      def "ExceptionType" $ union [
--  ClassType
        "class">: java "ClassType",
--  TypeVariable
        "variable">: java "TypeVariable"],

--MethodBody:
      def "MethodBody" $ union [
--  Block
        "block">: java "Block",
--  ;
        "none">: unit],

--InstanceInitializer:
--  Block
      def "InstanceInitializer" $ wrap $ java "Block",

--StaticInitializer:
--  static Block
      def "StaticInitializer" $ wrap $ java "Block",

--ConstructorDeclaration:
--  {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
      def "ConstructorDeclaration" $ record [
        "modifiers">: list $ java "ConstructorModifier",
        "constructor">: java "ConstructorDeclarator",
        "throws">: optional $ java "Throws",
        "body">: java "ConstructorBody"],

--ConstructorModifier:
--  (one of)
      def "ConstructorModifier" $ union [
--  Annotation public protected private
        "annotation">: java "Annotation",
        "public">: unit,
        "protected">: unit,
        "private">: unit],

--ConstructorDeclarator:
--  [TypeParameters] SimpleTypeName ( [ReceiverParameter ,] [FormalParameterList] )
      def "ConstructorDeclarator" $ record [
        "parameters">: list $ java "TypeParameter",
        "name">: java "SimpleTypeName",
        "receiverParameter">: optional $ java "ReceiverParameter",
        "formalParameters">: nonemptyList $ java "FormalParameter"],

--SimpleTypeName:
--  TypeIdentifier
      def "SimpleTypeName" $ wrap $ java "TypeIdentifier",

--ConstructorBody:
--  { [ExplicitConstructorInvocation] [BlockStatements] }
      def "ConstructorBody" $ record [
        "invocation">: optional $ java "ExplicitConstructorInvocation",
        "statements">: list $ java "BlockStatement"],

--ExplicitConstructorInvocation:
      def "ExplicitConstructorInvocation" $ record [
        "typeArguments">: list $ java "TypeArgument",
        "arguments">: list $ java "Expression",
        "variant">: java "ExplicitConstructorInvocation_Variant"],
      def "ExplicitConstructorInvocation_Variant" $ union [
--  [TypeArguments] this ( [ArgumentList] ) ;
        "this">: unit,
--  [TypeArguments] super ( [ArgumentList] ) ;
--  ExpressionName . [TypeArguments] super ( [ArgumentList] ) ;
        "super">: optional $ java "ExpressionName",
--  Primary . [TypeArguments] super ( [ArgumentList] ) ;
        "primary">: java "Primary"],

--EnumDeclaration:
--  {ClassModifier} enum TypeIdentifier [Superinterfaces] EnumBody
      def "EnumDeclaration" $ record [
        "modifiers">: list $ java "ClassModifier",
        "identifier">: java "TypeIdentifier",
        "implements">: list $ java "InterfaceType",
        "body">: java "EnumBody"],

--EnumBody:
--  { [EnumConstantList] [,] [EnumBodyDeclarations] }
      def "EnumBody" $ wrap $ list $ java "EnumBody_Element",
      def "EnumBody_Element" $ record [
        "constants">: list $ java "EnumConstant",
        "bodyDeclarations">: list $ java "ClassBodyDeclaration"],

--EnumConstantList:
--  EnumConstant {, EnumConstant}
--EnumConstant:
--  {EnumConstantModifier} Identifier [( [ArgumentList] )] [ClassBody]
      def "EnumConstant" $ record [
        "modifiers">: list $ java "EnumConstantModifier",
        "identifier">: java "Identifier",
        "arguments">: list $ list $ java "Expression",
        "body">: optional $ java "ClassBody"],

--EnumConstantModifier:
--  Annotation
      def "EnumConstantModifier" $ wrap $ java "Annotation",

--EnumBodyDeclarations:
--  ; {ClassBodyDeclaration}

--Productions from §9 (Interfaces)

--InterfaceDeclaration:
      def "InterfaceDeclaration" $ union [
--  NormalInterfaceDeclaration
        "normalInterface">: java "NormalInterfaceDeclaration",
--  AnnotationTypeDeclaration
        "annotationType">: java "AnnotationTypeDeclaration"],

--NormalInterfaceDeclaration:
--  {InterfaceModifier} interface TypeIdentifier [TypeParameters] [ExtendsInterfaces] InterfaceBody
      def "NormalInterfaceDeclaration" $ record [
        "modifiers">: list $ java "InterfaceModifier",
        "identifier">: java "TypeIdentifier",
        "parameters">: list $ java "TypeParameter",
        "extends">: list $ java "InterfaceType",
        "body">: java "InterfaceBody"],

--InterfaceModifier:
--  (one of)
      def "InterfaceModifier" $ union [
--  Annotation public protected private
--  abstract static strictfp
        "annotation">: java "Annotation",
        "public">: unit,
        "protected">: unit,
        "private">: unit,
        "abstract">: unit,
        "static">: unit,
        "strictfb">: unit],

--ExtendsInterfaces:
--  extends InterfaceTypeList

--InterfaceBody:
--  { {InterfaceMemberDeclaration} }
      def "InterfaceBody" $ wrap $ list $ java "InterfaceMemberDeclaration",

--InterfaceMemberDeclaration:
      def "InterfaceMemberDeclaration" $ union [
--  ConstantDeclaration
        "constant">: java "ConstantDeclaration",
--  InterfaceMethodDeclaration
        "interfaceMethod">: java "InterfaceMethodDeclaration",
--  ClassDeclaration
        "class">: java "ClassDeclaration",
--  InterfaceDeclaration
        "interface">: java "InterfaceDeclaration"],
--  ;

--ConstantDeclaration:
--  {ConstantModifier} UnannType VariableDeclaratorList ;
      def "ConstantDeclaration" $ record [
        "modifiers">: list $ java "ConstantModifier",
        "type">: java "UnannType",
        "variables">: nonemptyList $ java "VariableDeclarator"],

--ConstantModifier:
--  (one of)
      def "ConstantModifier" $ union [
--  Annotation public
--  static final
        "annotation">: java "Annotation",
        "public">: unit,
        "static">: unit,
        "final">: unit],

--InterfaceMethodDeclaration:
--  {InterfaceMethodModifier} MethodHeader MethodBody
      def "InterfaceMethodDeclaration" $ record [
        "modifiers">: list $ java "InterfaceMethodModifier",
        "header">: java "MethodHeader",
        "body">: java "MethodBody"],

--InterfaceMethodModifier:
--  (one of)
      def "InterfaceMethodModifier" $ union [
--  Annotation public private
--  abstract default static strictfp
        "annotation">: java "Annotation",
        "public">: unit,
        "private">: unit,
        "abstract">: unit,
        "default">: unit,
        "static">: unit,
        "strictfp">: unit],

--AnnotationTypeDeclaration:
--  {InterfaceModifier} @ interface TypeIdentifier AnnotationTypeBody
      def "AnnotationTypeDeclaration" $ record [
        "modifiers">: list $ java "InterfaceModifier",
        "identifier">: java "TypeIdentifier",
        "body">: java "AnnotationTypeBody"],

--AnnotationTypeBody:
--  { {AnnotationTypeMemberDeclaration} }
      def "AnnotationTypeBody" $ wrap $ list $ list $ java "AnnotationTypeMemberDeclaration",

--AnnotationTypeMemberDeclaration:
      def "AnnotationTypeMemberDeclaration" $ union [
--  AnnotationTypeElementDeclaration
        "annotationType">: java "AnnotationTypeElementDeclaration",
--  ConstantDeclaration
        "constant">: java "ConstantDeclaration",
--  ClassDeclaration
        "class">: java "ClassDeclaration",
--  InterfaceDeclaration
        "interface">: java "InterfaceDeclaration"],
--  ;

--AnnotationTypeElementDeclaration:
--  {AnnotationTypeElementModifier} UnannType Identifier ( ) [Dims] [DefaultValue] ;
      def "AnnotationTypeElementDeclaration" $ record [
        "modifiers">: list $ java "AnnotationTypeElementModifier",
        "type">: java "UnannType",
        "identifier">: java "Identifier",
        "dims">: optional $ java "Dims",
        "default">: optional $ java "DefaultValue"],

--AnnotationTypeElementModifier:
--  (one of)
      def "AnnotationTypeElementModifier" $ union [
--  Annotation public
        "public">: java "Annotation",
--  abstract
        "abstract">: unit],

--DefaultValue:
--  default ElementValue
      def "DefaultValue" $ wrap $ java "ElementValue",

--Annotation:
      def "Annotation" $ union [
--  NormalAnnotation
        "normal">: java "NormalAnnotation",
--  MarkerAnnotation
        "marker">: java "MarkerAnnotation",
--  SingleElementAnnotation
        "singleElement">: java "SingleElementAnnotation"],

--NormalAnnotation:
--  @ TypeName ( [ElementValuePairList] )
      def "NormalAnnotation" $ record [
        "typeName">: java "TypeName",
        "pairs">: list $ java "ElementValuePair"],

--ElementValuePairList:
--  ElementValuePair {, ElementValuePair}
--ElementValuePair:
--  Identifier = ElementValue
      def "ElementValuePair" $ record [
        "key">: java "Identifier",
        "value">: java "ElementValue"],

--ElementValue:
      def "ElementValue" $ union [
--  ConditionalExpression
        "conditionalExpression">: java "ConditionalExpression",
--  ElementValueArrayInitializer
        "elementValueArrayInitializer">: java "ElementValueArrayInitializer",
--  Annotation
        "annotation">: java "Annotation"],

--ElementValueArrayInitializer:
--  { [ElementValueList] [,] }
      def "ElementValueArrayInitializer" $ wrap $ list $ java "ElementValue",
--ElementValueList:
--  ElementValue {, ElementValue}

--MarkerAnnotation:
--  @ TypeName
      def "MarkerAnnotation" $ wrap $ java "TypeName",

--SingleElementAnnotation:
      def "SingleElementAnnotation" $ record [
--  @ TypeName ( ElementValue )
        "name">: java "TypeName",
        "value">: optional $ java "ElementValue"],

--  Productions from §10 (Arrays)

--ArrayInitializer:
--  { [VariableInitializerList] [,] }
      def "ArrayInitializer" $ wrap $ list $ list $ java "VariableInitializer",
--VariableInitializerList:
--  VariableInitializer {, VariableInitializer}

--Productions from §14 (Blocks and Statements)

--Block:
--  { [BlockStatements] }
      def "Block" $ wrap $ list $ java "BlockStatement",

--BlockStatements:
--  BlockStatement {BlockStatement}
--BlockStatement:
      def "BlockStatement" $ union [
--  LocalVariableDeclarationStatement
        "localVariableDeclaration">: java "LocalVariableDeclarationStatement",
--  ClassDeclaration
        "class">: java "ClassDeclaration",
--  Statement
        "statement">: java "Statement"],

--LocalVariableDeclarationStatement:
--  LocalVariableDeclaration ;
      def "LocalVariableDeclarationStatement" $ wrap $ java "LocalVariableDeclaration",

--LocalVariableDeclaration:
--  {VariableModifier} LocalVariableType VariableDeclaratorList
      def "LocalVariableDeclaration" $ record [
        "modifiers">: list $ java "VariableModifier",
        "type">: java "LocalVariableType",
        "declarators">: nonemptyList $ java "VariableDeclarator"],

--LocalVariableType:
      def "LocalVariableType" $ union [
--  UnannType
        "type">: java "UnannType",
--  var
        "var">: unit],

--Statement:
      def "Statement" $ union [
--  StatementWithoutTrailingSubstatement
        "withoutTrailing">: java "StatementWithoutTrailingSubstatement",
--  LabeledStatement
        "labeled">: java "LabeledStatement",
--  IfThenStatement
        "ifThen">: java "IfThenStatement",
--  IfThenElseStatement
        "ifThenElse">: java "IfThenElseStatement",
--  WhileStatement
        "while">: java "WhileStatement",
--  ForStatement
        "for">: java "ForStatement"],

--StatementNoShortIf:
      def "StatementNoShortIf" $ union [
--  StatementWithoutTrailingSubstatement
        "withoutTrailing">: java "StatementWithoutTrailingSubstatement",
--  LabeledStatementNoShortIf
        "labeled">: java "LabeledStatementNoShortIf",
--  IfThenElseStatementNoShortIf
        "ifThenElse">: java "IfThenElseStatementNoShortIf",
--  WhileStatementNoShortIf
        "while">: java "WhileStatementNoShortIf",
--  ForStatementNoShortIf
        "for">: java "ForStatementNoShortIf"],

--StatementWithoutTrailingSubstatement:
      def "StatementWithoutTrailingSubstatement" $ union [
--  Block
        "block">: java "Block",
--  EmptyStatement
        "empty">: unit,
--  ExpressionStatement
        "expression">: java "ExpressionStatement",
--  AssertStatement
        "assert">: java "AssertStatement",
--  SwitchStatement
        "switch">: java "SwitchStatement",
--  DoStatement
        "do">: java "DoStatement",
--  BreakStatement
        "break">: java "BreakStatement",
--  ContinueStatement
        "continue">: java "ContinueStatement",
--  ReturnStatement
        "return">: java "ReturnStatement",
--  SynchronizedStatement
        "synchronized">: java "SynchronizedStatement",
--  ThrowStatement
        "throw">: java "ThrowStatement",
--  TryStatement
        "try">: java "TryStatement"],

--EmptyStatement:
--  ;
--LabeledStatement:
--  Identifier : Statement
      def "LabeledStatement" $ record [
        "identifier">: java "Identifier",
        "statement">: java "Statement"],

--LabeledStatementNoShortIf:
--  Identifier : StatementNoShortIf
      def "LabeledStatementNoShortIf" $ record [
        "identifier">: java "Identifier",
        "statement">: java "StatementNoShortIf"],

--ExpressionStatement:
--  StatementExpression ;
      def "ExpressionStatement" $ wrap $ java "StatementExpression",

--StatementExpression:
      def "StatementExpression" $ union [
--  Assignment
        "assignment">: java "Assignment",
--  PreIncrementExpression
        "preIncrement">: java "PreIncrementExpression",
--  PreDecrementExpression
        "preDecrement">: java "PreDecrementExpression",
--  PostIncrementExpression
        "postIncrement">: java "PostIncrementExpression",
--  PostDecrementExpression
        "postDecrement">: java "PostDecrementExpression",
--  MethodInvocation
        "methodInvocation">: java "MethodInvocation",
--  ClassInstanceCreationExpression
        "classInstanceCreation">: java "ClassInstanceCreationExpression"],

--IfThenStatement:
--  if ( Expression ) Statement
      def "IfThenStatement" $ record [
        "expression">: java "Expression",
        "statement">: java "Statement"],

--IfThenElseStatement:
--  if ( Expression ) StatementNoShortIf else Statement
      def "IfThenElseStatement" $ record [
        "cond">: optional $ java "Expression",
        "then">: java "StatementNoShortIf",
        "else">: java "Statement"],

--IfThenElseStatementNoShortIf:
--  if ( Expression ) StatementNoShortIf else StatementNoShortIf
      def "IfThenElseStatementNoShortIf" $ record [
        "cond">: optional $ java "Expression",
        "then">: java "StatementNoShortIf",
        "else">: java "StatementNoShortIf"],

--AssertStatement:
      def "AssertStatement" $ union [
--  assert Expression ;
        "single">: java "Expression",
--  assert Expression : Expression ;
        "pair">: java "AssertStatement_Pair"],
      def "AssertStatement_Pair" $ record [
        "first">: java "Expression",
        "second">: java "Expression"],

--SwitchStatement:
--  switch ( Expression ) SwitchBlock
      def "SwitchStatement" $ record [
        "cond">: java "Expression",
        "block">: java "SwitchBlock"],

--SwitchBlock:
--  { {SwitchBlockStatementGroup} {SwitchLabel} }
      def "SwitchBlock" $ wrap $ list $ java "SwitchBlock_Pair",
      def "SwitchBlock_Pair" $ record [
        "statements">: list $ java "SwitchBlockStatementGroup",
        "labels">: list $ java "SwitchLabel"],

--SwitchBlockStatementGroup:
--  SwitchLabels BlockStatements
      def "SwitchBlockStatementGroup" $ record [
        "labels">: nonemptyList $ java "SwitchLabel",
        "statements">: nonemptyList $ java "BlockStatement"],

--SwitchLabels:
--  SwitchLabel {SwitchLabel}
--SwitchLabel:
      def "SwitchLabel" $ union [
--  case ConstantExpression :
        "constant">: java "ConstantExpression",
--  case EnumConstantName :
        "enumConstant">: java "EnumConstantName",
--  default :
        "default">: unit],

--EnumConstantName:
--  Identifier
      def "EnumConstantName" $ wrap $ java "Identifier",

--WhileStatement:
--  while ( Expression ) Statement
      def "WhileStatement" $ record [
        "cond">: optional $ java "Expression",
        "body">: java "Statement"],

--WhileStatementNoShortIf:
--  while ( Expression ) StatementNoShortIf
      def "WhileStatementNoShortIf" $ record [
        "cond">: optional $ java "Expression",
        "body">: java "StatementNoShortIf"],

--DoStatement:
--  do Statement while ( Expression ) ;
      def "DoStatement" $ record [
        "body">: java "Statement",
        "conde">: optional $ java "Expression"],

--ForStatement:
      def "ForStatement" $ union [
--  BasicForStatement
        "basic">: java "BasicForStatement",
--  EnhancedForStatement
        "enhanced">: java "EnhancedForStatement"],

--ForStatementNoShortIf:
      def "ForStatementNoShortIf" $ union [
--  BasicForStatementNoShortIf
        "basic">: java "BasicForStatementNoShortIf",
--  EnhancedForStatementNoShortIf
        "enhanced">: java "EnhancedForStatementNoShortIf"],

--BasicForStatement:
--  for ( [ForInit] ; [Expression] ; [ForUpdate] ) Statement
      def "BasicForStatement" $ record [
        "cond">: java "ForCond",
        "body">: java "Statement"],
      def "ForCond" $ record [
        "init">: optional $ java "ForInit",
        "cond">: optional $ java "Expression",
        "update">: optional $ java "ForUpdate"],
--BasicForStatementNoShortIf:
--  for ( [ForInit] ; [Expression] ; [ForUpdate] ) StatementNoShortIf
      def "BasicForStatementNoShortIf" $ record [
        "cond">: java "ForCond",
        "body">: java "StatementNoShortIf"],

--ForInit:
      def "ForInit" $ union [
--  StatementExpressionList
        "statements">: nonemptyList $ java "StatementExpression",
--  LocalVariableDeclaration
        "localVariable">: java "LocalVariableDeclaration"],

--ForUpdate:
--  StatementExpressionList
      def "ForUpdate" $ wrap $ nonemptyList $ java "StatementExpression",
--  StatementExpressionList:
--  StatementExpression {, StatementExpression}

--EnhancedForStatement:
      def "EnhancedForStatement" $ record [
--  for ( {VariableModifier} LocalVariableType VariableDeclaratorId : Expression ) Statement
        "cond">: java "EnhancedForCond",
        "body">: java "Statement"],
      def "EnhancedForCond" $ record [
        "modifiers">: list $ java "VariableModifier",
        "type">: java "LocalVariableType",
        "id">: java "VariableDeclaratorId",
        "expression">: java "Expression"],
--EnhancedForStatementNoShortIf:
--  for ( {VariableModifier} LocalVariableType VariableDeclaratorId : Expression ) StatementNoShortIf
      def "EnhancedForStatementNoShortIf" $ record [
        "cond">: java "EnhancedForCond",
        "body">: java "StatementNoShortIf"],

--BreakStatement:
--  break [Identifier] ;
      def "BreakStatement" $ wrap $ optional $ java "Identifier",

--ContinueStatement:
--  continue [Identifier] ;
      def "ContinueStatement" $ wrap $ optional $ java "Identifier",

--ReturnStatement:
--  return [Expression] ;
      def "ReturnStatement" $ wrap $ optional $ java "Expression",

--ThrowStatement:
--  throw Expression ;
      def "ThrowStatement" $ wrap $ java "Expression",

--SynchronizedStatement:
--  synchronized ( Expression ) Block
      def "SynchronizedStatement" $ record [
        "expression">: java "Expression",
        "block">: java "Block"],

--TryStatement:
      def "TryStatement" $ union [
--  try Block Catches
        "simple">: java "TryStatement_Simple",
--  try Block [Catches] Finally
        "withFinally">: java "TryStatement_WithFinally",
--  TryWithResourcesStatement
        "withResources">: java "TryWithResourcesStatement"],
      def "TryStatement_Simple" $ record [
        "block">: java "Block",
        "catches">: java "Catches"],
      def "TryStatement_WithFinally" $ record [
        "block">: java "Block",
        "catches">: optional $ java "Catches",
        "finally">: java "Finally"],

--Catches:
--  CatchClause {CatchClause}
      def "Catches" $ wrap $ list $ java "CatchClause",

--CatchClause:
--  catch ( CatchFormalParameter ) Block
      def "CatchClause" $ record [
        "parameter">: optional $ java "CatchFormalParameter",
        "block">: java "Block"],

--CatchFormalParameter:
--  {VariableModifier} CatchType VariableDeclaratorId
      def "CatchFormalParameter" $ record [
        "modifiers">: list $ java "VariableModifier",
        "type">: java "CatchType",
        "id">: java "VariableDeclaratorId"],

--CatchType:
--  UnannClassType {| ClassType}
      def "CatchType" $ record [
        "type">: java "UnannClassType",
        "types">: list $ java "ClassType"],

--Finally:
--  finally Block
      def "Finally" $ wrap $ java "Block",

--TryWithResourcesStatement:
--  try ResourceSpecification Block [Catches] [Finally]
      def "TryWithResourcesStatement" $ record [
        "resourceSpecification">: java "ResourceSpecification",
        "block">: java "Block",
        "catches">: optional $ java "Catches",
        "finally">: optional $ java "Finally"],

--ResourceSpecification:
--  ( ResourceList [;] )
      def "ResourceSpecification" $ wrap $ list $ java "Resource",

--ResourceList:
--  Resource {; Resource}
--Resource:
      def "Resource" $ union [
--  {VariableModifier} LocalVariableType Identifier = Expression
        "local">: java "Resource_Local",
--  VariableAccess
        "variable">: java "VariableAccess"],
      def "Resource_Local" $ record [
        "modifiers">: list $ java "VariableModifier",
        "type">: java "LocalVariableType",
        "identifier">: java "Identifier",
        "expression">: java "Expression"],

--VariableAccess:
      def "VariableAccess" $ union [
--  ExpressionName
        "expressionName">: java "ExpressionName",
--  FieldAccess
        "fieldAccess">: java "FieldAccess"],

--Productions from §15 (Expressions)

--Primary:
      def "Primary" $ union [
--  PrimaryNoNewArray
        "noNewArray">: java "PrimaryNoNewArray",
--  ArrayCreationExpression
        "arrayCreation">: java "ArrayCreationExpression"],

--PrimaryNoNewArray:
      def "PrimaryNoNewArray" $ union [
--  Literal
        "literal">: java "Literal",
--  ClassLiteral
        "classLiteral">: java "ClassLiteral",
--  this
        "this">: unit,
--  TypeName . this
        "dotThis">: java "TypeName",
--  ( Expression )
        "parens">: java "Expression",
--  ClassInstanceCreationExpression
        "classInstance">: java "ClassInstanceCreationExpression",
--  FieldAccess
        "fieldAccess">: java "FieldAccess",
--  ArrayAccess
        "arrayAccess">: java "ArrayAccess",
--  MethodInvocation
        "methodInvocation">: java "MethodInvocation",
--  MethodReference
        "methodReference">: java "MethodReference"],

--ClassLiteral:
      def "ClassLiteral" $ union [
--  TypeName {[ ]} . class
        "type">: java "TypeNameArray",
--  NumericType {[ ]} . class
        "numericType">: java "NumericTypeArray",
--  boolean {[ ]} . class
        "boolean">: java "BooleanArray",
--  void . class
        "void">: unit],
      def "TypeNameArray" $ union [
        "simple">: java "TypeName",
        "array">: java "TypeNameArray"],
      def "NumericTypeArray" $ union [
        "simple">: java "NumericType",
        "array">: java "NumericTypeArray"],
      def "BooleanArray" $ union [
        "simple">: unit,
        "array">: java "BooleanArray"],

--ClassInstanceCreationExpression:
--  UnqualifiedClassInstanceCreationExpression
--  ExpressionName . UnqualifiedClassInstanceCreationExpression
--  Primary . UnqualifiedClassInstanceCreationExpression
      def "ClassInstanceCreationExpression" $ record [
        "qualifier">: optional $ java "ClassInstanceCreationExpression_Qualifier",
        "expression">: java "UnqualifiedClassInstanceCreationExpression"],
      def "ClassInstanceCreationExpression_Qualifier" $ union [
        "expression">: java "ExpressionName",
        "primary">: java "Primary"],

--UnqualifiedClassInstanceCreationExpression:
--  new [TypeArguments] ClassOrInterfaceTypeToInstantiate ( [ArgumentList] ) [ClassBody]
      def "UnqualifiedClassInstanceCreationExpression" $ record [
        "typeArguments">: list $ java "TypeArgument",
        "classOrInterface">: java "ClassOrInterfaceTypeToInstantiate",
        "arguments">: list $ java "Expression",
        "body">: optional $ java "ClassBody"],

--ClassOrInterfaceTypeToInstantiate:
--  {Annotation} Identifier {. {Annotation} Identifier} [TypeArgumentsOrDiamond]
      def "ClassOrInterfaceTypeToInstantiate" $ record [
        "identifiers">: nonemptyList $ java "AnnotatedIdentifier",
        "typeArguments">: optional $ java "TypeArgumentsOrDiamond"],
      def "AnnotatedIdentifier" $ record [
        "annotations">: list $ java "Annotation",
        "identifier">: java "Identifier"],

--TypeArgumentsOrDiamond:
      def "TypeArgumentsOrDiamond" $ union [
--  TypeArguments
        "arguments">: nonemptyList $ java "TypeArgument",
--  <>
        "diamond">: unit],

--FieldAccess:
      def "FieldAccess" $ record [
        "qualifier">: java "FieldAccess_Qualifier",
        "identifier">: java "Identifier"],
      def "FieldAccess_Qualifier" $ union [
--  Primary . Identifier
        "primary">: java "Primary",
--  super . Identifier
        "super">: unit,
--  TypeName . super . Identifier
        "typed">: java "TypeName"],

--ArrayAccess:
      def "ArrayAccess" $ record [
        "expression">: optional $ java "Expression",
        "variant">: java "ArrayAccess_Variant"],
      def "ArrayAccess_Variant" $ union [
--  ExpressionName [ Expression ]
        "name">: java "ExpressionName",
--  PrimaryNoNewArray [ Expression ]
        "primary">: java "PrimaryNoNewArray"],

--MethodInvocation:
      def "MethodInvocation" $ record [
        "header">: java "MethodInvocation_Header",
        "arguments">: list $ java "Expression"],
      def "MethodInvocation_Header" $ union [
--  MethodName ( [ArgumentList] )
        "simple">: java "MethodName",
        "complex">: java "MethodInvocation_Complex"],
      def "MethodInvocation_Complex" $ record [
        "variant">: java "MethodInvocation_Variant",
        "typeArguments">: list $ java "TypeArgument",
        "identifier">: java "Identifier"],
      def "MethodInvocation_Variant" $ union [
--  TypeName . [TypeArguments] Identifier ( [ArgumentList] )
        "type">: java "TypeName",
--  ExpressionName . [TypeArguments] Identifier ( [ArgumentList] )
        "expression">: java "ExpressionName",
--  Primary . [TypeArguments] Identifier ( [ArgumentList] )
        "primary">: java "Primary",
--  super . [TypeArguments] Identifier ( [ArgumentList] )
        "super">: unit,
--  TypeName . super . [TypeArguments] Identifier ( [ArgumentList] )
        "typeSuper">: java "TypeName"],

--ArgumentList:
--  Expression {, Expression}

--MethodReference:
      def "MethodReference" $ union [
--  ExpressionName :: [TypeArguments] Identifier
        "expression">: java "MethodReference_Expression",
--  Primary :: [TypeArguments] Identifier
        "primary">: java "MethodReference_Primary",
--  ReferenceType :: [TypeArguments] Identifier
        "referenceType">: java"MethodReference_ReferenceType",
--  super :: [TypeArguments] Identifier
--  TypeName . super :: [TypeArguments] Identifier
        "super">: java "MethodReference_Super",
--  ClassType :: [TypeArguments] new
        "new">: java "MethodReference_New",
--  ArrayType :: new
        "array">: java "MethodReference_Array"],
      def "MethodReference_Expression" $ record [
        "name">: java "ExpressionName",
        "typeArguments">: list $ java "TypeArgument",
        "identifier">: java "Identifier"],
      def "MethodReference_Primary" $ record [
        "primary">: java "Primary",
        "typeArguments">: list $ java "TypeArgument",
        "identifier">: java "Identifier"],
      def "MethodReference_ReferenceType" $ record [
        "referenceType">: java "ReferenceType",
        "typeArguments">: list $ java "TypeArgument",
        "identifier">: java "Identifier"],
      def "MethodReference_Super" $ record [
        "typeArguments">: list $ java "TypeArgument",
        "identifier">: java "Identifier",
        "super">: boolean],
      def "MethodReference_New" $ record [
        "classType">: java "ClassType",
        "typeArguments">: list $ java "TypeArgument"],
      def "MethodReference_Array" $ wrap $ java "ArrayType",

--ArrayCreationExpression:
      def "ArrayCreationExpression" $ union [
--  new PrimitiveType DimExprs [Dims]
        "primitive">: java "ArrayCreationExpression_Primitive",
--  new ClassOrInterfaceType DimExprs [Dims]
        "classOrInterface">: java "ArrayCreationExpression_ClassOrInterface",
--  new PrimitiveType Dims ArrayInitializer
        "primitiveArray">: java "ArrayCreationExpression_PrimitiveArray",
--  new ClassOrInterfaceType Dims ArrayInitializer
        "classOrInterfaceArray">: java "ArrayCreationExpression_ClassOrInterfaceArray"],
      def "ArrayCreationExpression_Primitive" $ record [
        "type">: java "PrimitiveTypeWithAnnotations",
        "dimExprs">: nonemptyList $ java "DimExpr",
        "dims">: optional $ java "Dims"],
      def "ArrayCreationExpression_ClassOrInterface" $ record [
        "type">: java "ClassOrInterfaceType",
        "dimExprs">: nonemptyList $ java "DimExpr",
        "dims">: optional $ java "Dims"],
      def "ArrayCreationExpression_PrimitiveArray" $ record [
        "type">: java "PrimitiveTypeWithAnnotations",
        "dims">: nonemptyList $ java "Dims",
        "array">: java "ArrayInitializer"],
      def "ArrayCreationExpression_ClassOrInterfaceArray" $ record [
        "type">: java "ClassOrInterfaceType",
        "dims">: nonemptyList $ java "Dims",
        "array">: java "ArrayInitializer"],

--DimExprs:
--  DimExpr {DimExpr}
--DimExpr:
--  {Annotation} [ Expression ]
      def "DimExpr" $ record [
        "annotations">: list $ java "Annotation",
        "expression">: optional $ java "Expression"],

--Expression:
      def "Expression" $ union [
--  LambdaExpression
        "lambda">: java "LambdaExpression",
--  AssignmentExpression
        "assignment">: java "AssignmentExpression"],

--LambdaExpression:
--  LambdaParameters -> LambdaBody
      def "LambdaExpression" $ record [
        "parameters">: java "LambdaParameters",
        "body">: java "LambdaBody"],

--LambdaParameters:
--  ( [LambdaParameterList] )
--  Identifier
      def "LambdaParameters" $ union [
        "tuple">: list $ java "LambdaParameters",
        "single">: java "Identifier"],

--LambdaParameterList:
--  LambdaParameter {, LambdaParameter}
--  Identifier {, Identifier}
--LambdaParameter:
      def "LambdaParameter" $ union [
--  {VariableModifier} LambdaParameterType VariableDeclaratorId
        "normal">: java "LambdaParameter_Normal",
--  VariableArityParameter
        "variableArity">: java "VariableArityParameter"],
      def "LambdaParameter_Normal" $ record [
        "modifiers">: list $ java "VariableModifier",
        "type">: java "LambdaParameterType",
        "id">: java "VariableDeclaratorId"],

--LambdaParameterType:
      def "LambdaParameterType" $ union [
--  UnannType
        "type">: java "UnannType",
--  var
        "var">: unit],

--LambdaBody:
      def "LambdaBody" $ union [
--  Expression
        "expression">: java "Expression",
--  Block
        "block">: java "Block"],

--AssignmentExpression:
      def "AssignmentExpression" $ union [
--  ConditionalExpression
        "conditional">: java "ConditionalExpression",
--  Assignment
        "assignment">: java "Assignment"],

--Assignment:
--  LeftHandSide AssignmentOperator Expression
      def "Assignment" $ record [
        "lhs">: java "LeftHandSide",
        "op">: java "AssignmentOperator",
        "expression">: java "Expression"],

--LeftHandSide:
      def "LeftHandSide" $ union [
--  ExpressionName
        "expressionName">: java "ExpressionName",
--  FieldAccess
        "fieldAccess">: java "FieldAccess",
--  ArrayAccess
        "arrayAccess">: java "ArrayAccess"],

--AssignmentOperator:
--  (one of)
      def "AssignmentOperator" $ enum [
--  =  *=  /=  %=  +=  -=  <<=  >>=  >>>=  &=  ^=  |=
        "simple", "times", "div", "mod", "plus", "minus",
        "shiftLeft", "shiftRight", "shiftRightZeroFill", "and", "xor", "or"],

--ConditionalExpression:
      def "ConditionalExpression" $ union [
--  ConditionalOrExpression
        "simple">: java "ConditionalOrExpression",
--  ConditionalOrExpression ? Expression : ConditionalExpression
        "ternaryCond">: java "ConditionalExpression_TernaryCond",
--  ConditionalOrExpression ? Expression : LambdaExpression
        "ternaryLambda">: java "ConditionalExpression_TernaryLambda"],
      def "ConditionalExpression_TernaryCond" $ record [
        "cond">: java "ConditionalOrExpression",
        "ifTrue">: java "Expression",
        "ifFalse">: java "ConditionalExpression"],
      def "ConditionalExpression_TernaryLambda" $ record [
        "cond">: java "ConditionalOrExpression",
        "ifTrue">: java "Expression",
        "ifFalse">: java "LambdaExpression"],

--ConditionalOrExpression:
--  ConditionalAndExpression
--  ConditionalOrExpression || ConditionalAndExpression
      def "ConditionalOrExpression" $ wrap $ nonemptyList $ java "ConditionalAndExpression",

--ConditionalAndExpression:
--  InclusiveOrExpression
--  ConditionalAndExpression && InclusiveOrExpression
      def "ConditionalAndExpression" $ wrap $ nonemptyList $ java "InclusiveOrExpression",

--InclusiveOrExpression:
--  ExclusiveOrExpression
--  InclusiveOrExpression | ExclusiveOrExpression
      def "InclusiveOrExpression" $ wrap $ nonemptyList $ java "ExclusiveOrExpression",

--ExclusiveOrExpression:
--  AndExpression
--  ExclusiveOrExpression ^ AndExpression
      def "ExclusiveOrExpression" $ wrap $ nonemptyList $ java "AndExpression",

--AndExpression:
--  EqualityExpression
--  AndExpression & EqualityExpression
      def "AndExpression" $ wrap $ nonemptyList $ java "EqualityExpression",

--EqualityExpression:
      def "EqualityExpression" $ union [
--  RelationalExpression
        "unary">: java "RelationalExpression",
--  EqualityExpression == RelationalExpression
        "equal">: java "EqualityExpression_Binary",
--  EqualityExpression != RelationalExpression
        "notEqual">: java "EqualityExpression_Binary"],
      def "EqualityExpression_Binary" $ record [
        "lhs">: java "EqualityExpression",
        "rhs">: java "RelationalExpression"],

--RelationalExpression:
      def "RelationalExpression" $ union [
--  ShiftExpression
        "simple">: java "ShiftExpression",
--  RelationalExpression < ShiftExpression
        "lessThan">: java "RelationalExpression_LessThan",
--  RelationalExpression > ShiftExpression
        "greaterThan">: java "RelationalExpression_GreaterThan",
--  RelationalExpression <= ShiftExpression
        "lessThanEqual">: java "RelationalExpression_LessThanEqual",
--  RelationalExpression >= ShiftExpression
        "greaterThanEqual">: java "RelationalExpression_GreaterThanEqual",
--  RelationalExpression instanceof ReferenceType
        "instanceof">: java "RelationalExpression_InstanceOf"],
      def "RelationalExpression_LessThan" $ record [
        "lhs">: java "RelationalExpression",
        "rhs">: java "ShiftExpression"],
      def "RelationalExpression_GreaterThan" $ record [
        "lhs">: java "RelationalExpression",
        "rhs">: java "ShiftExpression"],
      def "RelationalExpression_LessThanEqual" $ record [
        "lhs">: java "RelationalExpression",
        "rhs">: java "ShiftExpression"],
      def "RelationalExpression_GreaterThanEqual" $ record [
        "lhs">: java "RelationalExpression",
        "rhs">: java "ShiftExpression"],
      def "RelationalExpression_InstanceOf" $ record [
        "lhs">: java "RelationalExpression",
        "rhs">: java "ReferenceType"],

--ShiftExpression:
      def "ShiftExpression" $ union [
--  AdditiveExpression
        "unary">: java "AdditiveExpression",
--  ShiftExpression << AdditiveExpression
        "shiftLeft">: java "ShiftExpression_Binary",
--  ShiftExpression >> AdditiveExpression
        "shiftRight">: java "ShiftExpression_Binary",
--  ShiftExpression >>> AdditiveExpression
        "shiftRightZeroFill">: java "ShiftExpression_Binary"],
      def "ShiftExpression_Binary" $ record [
        "lhs">: java "ShiftExpression",
        "rhs">: java "AdditiveExpression"],

--AdditiveExpression:
      def "AdditiveExpression" $ union [
--  MultiplicativeExpression
        "unary">: java "MultiplicativeExpression",
--  AdditiveExpression + MultiplicativeExpression
        "plus">: java "AdditiveExpression_Binary",
--  AdditiveExpression - MultiplicativeExpression
        "minus">: java "AdditiveExpression_Binary"],
      def "AdditiveExpression_Binary" $ record [
        "lhs">: java "AdditiveExpression",
        "rhs">: java "MultiplicativeExpression"],

--MultiplicativeExpression:
      def "MultiplicativeExpression" $ union [
--  UnaryExpression
        "unary">: java "UnaryExpression",
--  MultiplicativeExpression * UnaryExpression
        "times">: java "MultiplicativeExpression_Binary",
--  MultiplicativeExpression / UnaryExpression
        "divide">: java "MultiplicativeExpression_Binary",
--  MultiplicativeExpression % UnaryExpression
        "mod">: java "MultiplicativeExpression_Binary"],
      def "MultiplicativeExpression_Binary" $ record [
        "lhs">: java "MultiplicativeExpression",
        "rhs">: java "UnaryExpression"],

--UnaryExpression:
      def "UnaryExpression" $ union [
--  PreIncrementExpression
        "preIncrement">: java "PreIncrementExpression",
--  PreDecrementExpression
        "preDecrement">: java "PreDecrementExpression",
--  + UnaryExpression
        "plus">: java "UnaryExpression",
--  - UnaryExpression
        "minus">: java "UnaryExpression",
--  UnaryExpressionNotPlusMinus
        "other">: java "UnaryExpressionNotPlusMinus"],

--PreIncrementExpression:
--  ++ UnaryExpression
      def "PreIncrementExpression" $ wrap $ java "UnaryExpression",

--PreDecrementExpression:
--  -- UnaryExpression
      def "PreDecrementExpression" $ wrap $ java "UnaryExpression",

--UnaryExpressionNotPlusMinus:
      def "UnaryExpressionNotPlusMinus" $ union [
--  PostfixExpression
        "postfix">: java "PostfixExpression",
--  ~ UnaryExpression
        "tilde">: java "UnaryExpression",
--  ! UnaryExpression
        "not">: java "UnaryExpression",
--  CastExpression
        "cast">: java "CastExpression"],

--PostfixExpression:
      def "PostfixExpression" $ union [
--  Primary
        "primary">: java "Primary",
--  ExpressionName
        "name">: java "ExpressionName",
--  PostIncrementExpression
        "postIncrement">: java "PostIncrementExpression",
--  PostDecrementExpression
        "postDecrement">: java "PostDecrementExpression"],

--PostIncrementExpression:
--  PostfixExpression ++
      def "PostIncrementExpression" $ wrap $ java "PostfixExpression",

--PostDecrementExpression:
--  PostfixExpression --
      def "PostDecrementExpression" $ wrap $ java "PostfixExpression",

--CastExpression:
      def "CastExpression" $ union [
--  ( PrimitiveType ) UnaryExpression
        "primitive">: java "CastExpression_Primitive",
--  ( ReferenceType {AdditionalBound} ) UnaryExpressionNotPlusMinus
        "notPlusMinus">: java "CastExpression_NotPlusMinus",
--  ( ReferenceType {AdditionalBound} ) LambdaExpression
        "lambda">: java "CastExpression_Lambda"],
      def "CastExpression_Primitive" $ record [
        "type">: java "PrimitiveTypeWithAnnotations",
        "expression">: java "UnaryExpression"],
      def "CastExpression_NotPlusMinus" $ record [
        "refAndBounds">: java "CastExpression_RefAndBounds",
        "expression">: java "UnaryExpression"],
      def "CastExpression_Lambda" $ record [
        "refAndBounds">: java "CastExpression_RefAndBounds",
        "expression">: java "LambdaExpression"],
      def "CastExpression_RefAndBounds" $ record [
        "type">: java "ReferenceType",
        "bounds">: list $ java "AdditionalBound"],

--ConstantExpression:
--  Expression
      def "ConstantExpression" $ wrap $ java "Expression"]
