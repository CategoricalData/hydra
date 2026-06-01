module Hydra.Sources.Java.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel hiding (packageName)
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y


ns :: ModuleName
ns = ModuleName "hydra.java.syntax"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A Java syntax module. Tracks the Oracle Java SE 21 BNF:\n" ++
      "  https://docs.oracle.com/javase/specs/jls/se21/html/jls-19.html\n" ++
      "Note: all *WithComments types were added manually, rather than derived from the BNF, which does not allow for comments."))}
  where
    definitions = [
      identifier,
      typeIdentifier,
      literal,
      integerLiteral,
      floatingPointLiteral,
      stringLiteral,
      textBlock,
      type_,
      primitiveTypeWithAnnotations,
      primitiveType_,
      numericType,
      integralType,
      floatingPointType,
      referenceType,
      classOrInterfaceType,
      classType,
      classTypeQualifier,
      interfaceType,
      typeVariable,
      arrayType,
      arrayType_Variant,
      dims,
      typeParameter,
      typeParameterModifier,
      typeBound,
      typeBound_ClassOrInterface,
      additionalBound,
      typeArgument,
      wildcard,
      wildcardBounds,
      moduleNameDef,
      packageName,
      typeName,
      expressionName,
      methodName,
      packageOrTypeName,
      ambiguousName,
      compilationUnit,
      ordinaryCompilationUnit,
      modularCompilationUnit,
      packageDeclaration,
      packageModifier,
      importDeclaration,
      singleTypeImportDeclaration,
      typeImportOnDemandDeclaration,
      singleStaticImportDeclaration,
      staticImportOnDemandDeclaration,
      topLevelClassOrInterfaceDeclaration,
      topLevelClassOrInterfaceDeclarationWithComments,
      moduleDeclaration,
      moduleDirective,
      moduleDirective_Requires,
      moduleDirective_ExportsOrOpens,
      moduleDirective_Provides,
      requiresModifier,
      classDeclaration,
      normalClassDeclaration,
      classModifier,
      classBody,
      classBodyDeclaration,
      classBodyDeclarationWithComments,
      classMemberDeclaration,
      fieldDeclaration,
      fieldModifier,
      variableDeclarator,
      variableDeclaratorId,
      variableInitializer,
      unannType,
      unannClassType,
      methodDeclaration,
      methodModifier,
      methodHeader,
      result,
      methodDeclarator,
      receiverParameter,
      formalParameter,
      formalParameter_Simple,
      variableArityParameter,
      variableModifier,
      throws,
      exceptionType,
      methodBody,
      instanceInitializer,
      staticInitializer,
      constructorDeclaration,
      constructorModifier,
      constructorDeclarator,
      simpleTypeName,
      constructorBody,
      explicitConstructorInvocation,
      explicitConstructorInvocation_Variant,
      enumDeclaration,
      enumBody,
      enumBody_Element,
      enumConstant,
      enumConstantModifier,
      recordDeclaration,
      recordHeader,
      recordComponent,
      recordComponent_Simple,
      variableArityRecordComponent,
      recordComponentModifier,
      recordBody,
      recordBodyDeclaration,
      compactConstructorDeclaration,
      interfaceDeclaration,
      normalInterfaceDeclaration,
      interfaceModifier,
      interfaceBody,
      interfaceMemberDeclaration,
      interfaceMemberDeclarationWithComments,
      constantDeclaration,
      constantModifier,
      interfaceMethodDeclaration,
      interfaceMethodModifier,
      annotationInterfaceDeclaration,
      annotationInterfaceBody,
      annotationInterfaceMemberDeclaration,
      annotationInterfaceElementDeclaration,
      annotationInterfaceElementModifier,
      defaultValue,
      annotation,
      normalAnnotation,
      elementValuePair,
      elementValue,
      elementValueArrayInitializer,
      markerAnnotation,
      singleElementAnnotation,
      arrayInitializer,
      block,
      blockStatement,
      localClassOrInterfaceDeclaration,
      localVariableDeclarationStatement,
      localVariableDeclaration,
      localVariableType,
      statement,
      statementNoShortIf,
      statementWithoutTrailingSubstatement,
      labeledStatement,
      labeledStatementNoShortIf,
      expressionStatement,
      statementExpression,
      ifThenStatement,
      ifThenElseStatement,
      ifThenElseStatementNoShortIf,
      assertStatement,
      assertStatement_Pair,
      switchStatement,
      switchBlock,
      switchBlock_Legacy,
      switchRule,
      switchRule_Body,
      switchBlockStatementGroup,
      switchLabel,
      caseConstant,
      casePattern,
      guard,
      pattern_,
      typePattern,
      recordPattern,
      whileStatement,
      whileStatementNoShortIf,
      doStatement,
      forStatement,
      forStatementNoShortIf,
      basicForStatement,
      forCond,
      basicForStatementNoShortIf,
      forInit,
      forUpdate,
      enhancedForStatement,
      enhancedForCond,
      enhancedForStatementNoShortIf,
      breakStatement,
      yieldStatement,
      continueStatement,
      returnStatement,
      throwStatement,
      synchronizedStatement,
      tryStatement,
      tryStatement_Simple,
      tryStatement_WithFinally,
      catches,
      catchClause,
      catchFormalParameter,
      catchType,
      finally_,
      tryWithResourcesStatement,
      resourceSpecification,
      resource,
      resource_Local,
      variableAccess,
      primary,
      primaryNoNewArrayExpression,
      classLiteral,
      typeNameArray,
      numericTypeArray,
      booleanArray,
      classInstanceCreationExpression,
      classInstanceCreationExpression_Qualifier,
      unqualifiedClassInstanceCreationExpression,
      classOrInterfaceTypeToInstantiate,
      annotatedIdentifier,
      typeArgumentsOrDiamond,
      fieldAccess,
      fieldAccess_Qualifier,
      arrayAccess,
      arrayAccess_Variant,
      methodInvocation,
      methodInvocation_Header,
      methodInvocation_Complex,
      methodInvocation_Variant,
      methodReference,
      methodReference_Expression,
      methodReference_Primary,
      methodReference_ReferenceType,
      methodReference_Super,
      methodReference_New,
      methodReference_Array,
      arrayCreationExpression,
      arrayCreationExpressionWithoutInitializer,
      arrayCreationExpressionWithoutInitializer_Primitive,
      arrayCreationExpressionWithoutInitializer_ClassOrInterface,
      arrayCreationExpressionWithInitializer,
      arrayCreationExpressionWithInitializer_Primitive,
      arrayCreationExpressionWithInitializer_ClassOrInterface,
      dimExpr,
      expression,
      lambdaExpression,
      lambdaParameters,
      lambdaParameter_,
      lambdaParameter_Normal,
      lambdaParameterType,
      lambdaBody_,
      assignmentExpression,
      assignment,
      leftHandSide,
      assignmentOperator,
      conditionalExpression,
      conditionalExpression_TernaryCond,
      conditionalExpression_TernaryLambda,
      conditionalOrExpression,
      conditionalAndExpression,
      inclusiveOrExpression,
      exclusiveOrExpression,
      andExpression,
      equalityExpression,
      equalityExpression_Binary,
      relationalExpression,
      relationalExpression_LessThan,
      relationalExpression_GreaterThan,
      relationalExpression_LessThanEqual,
      relationalExpression_GreaterThanEqual,
      instanceofExpression,
      instanceofExpression_Rhs,
      shiftExpression,
      shiftExpression_Binary,
      additiveExpression,
      additiveExpression_Binary,
      multiplicativeExpression,
      multiplicativeExpression_Binary,
      unaryExpression,
      preIncrementExpression,
      preDecrementExpression,
      unaryExpressionNotPlusMinus,
      postfixExpression,
      postIncrementExpression,
      postDecrementExpression,
      castExpression,
      castExpression_Primitive,
      castExpression_NotPlusMinus,
      castExpression_Lambda,
      castExpression_RefAndBounds,
      switchExpression,
      constantExpression]

def :: String -> Type -> TypeDefinition
def = datatype ns

java :: String -> Type
java = typeref ns

--Productions from §3 (Lexical Structure)

floatingPointLiteral :: TypeDefinition
floatingPointLiteral = def "FloatingPointLiteral" $
  doc "Note: this is an approximation which ignores encoding" $
  T.wrap T.float64

--Identifier:
--  IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
identifier :: TypeDefinition
identifier = def "Identifier" $ T.wrap T.string

--IdentifierChars:
--  JavaLetter {JavaLetterOrDigit}
--
--JavaLetter:
--  any Unicode character that is a "Java letter"
--
--JavaLetterOrDigit:
--  any Unicode character that is a "Java letter-or-digit"

integerLiteral :: TypeDefinition
integerLiteral = def "IntegerLiteral" $
  doc "Note: this is an approximation which ignores encoding" $
  T.wrap T.bigint

--Literal:
literal :: TypeDefinition
literal = def "Literal" $
  T.union [
--  NullLiteral
    "null">: T.unit,
--  IntegerLiteral
    "integer">: java "IntegerLiteral",
--  FloatingPointLiteral
    "floatingPoint">: java "FloatingPointLiteral",
--  BooleanLiteral
    "boolean">: T.boolean,
--  CharacterLiteral
    "character">: T.uint16,
--  StringLiteral
    "string">: java "StringLiteral",
--  TextBlock (JEP 378, Java SE 15)
    "textBlock">: java "TextBlock"]

stringLiteral :: TypeDefinition
stringLiteral = def "StringLiteral" $
  doc "Note: this is an approximation which ignores encoding" $
  T.wrap T.string

--TextBlock (JEP 378, Java SE 15):
--  A multi-line string literal delimited by triple double-quotes (\"\"\"...\"\"\").
textBlock :: TypeDefinition
textBlock = def "TextBlock" $
  doc "Note: this is an approximation which ignores encoding and incidental whitespace stripping" $
  T.wrap T.string

--TypeIdentifier:
--  Identifier but not var
typeIdentifier :: TypeDefinition
typeIdentifier = def "TypeIdentifier" $ T.wrap $ java "Identifier"

--Productions from §4 (Types, Values, and Variables)

--AdditionalBound:
--  & InterfaceType
additionalBound :: TypeDefinition
additionalBound = def "AdditionalBound" $ T.wrap $ java "InterfaceType"

--TypeArguments:
--  < TypeArgumentList >
--TypeArgumentList:
--  TypeArgument {, TypeArgument}

--ArrayType:
arrayType :: TypeDefinition
arrayType = def "ArrayType" $ T.record [
  "dims">: java "Dims",
  "variant">: java "ArrayType_Variant"]

arrayType_Variant :: TypeDefinition
arrayType_Variant = def "ArrayType_Variant" $ T.union [
--  PrimitiveType Dims
  "primitive">: java "PrimitiveTypeWithAnnotations",
--  ClassOrInterfaceType Dims
  "classOrInterface">: java "ClassOrInterfaceType",
--  TypeVariable Dims
  "variable">: java "TypeVariable"]

--ClassOrInterfaceType:
classOrInterfaceType :: TypeDefinition
classOrInterfaceType = def "ClassOrInterfaceType" $ T.union [
--  ClassType
  "class">: java "ClassType",
--  InterfaceType
  "interface">: java "InterfaceType"]

--ClassType:
classType :: TypeDefinition
classType = def "ClassType" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "qualifier">: java "ClassTypeQualifier",
  "identifier">: java "TypeIdentifier",
  "arguments">: T.list $ java "TypeArgument"]

classTypeQualifier :: TypeDefinition
classTypeQualifier = def "ClassTypeQualifier" $ T.union [
--  {Annotation} TypeIdentifier [TypeArguments]
  "none">: T.unit,
--  PackageName . {Annotation} TypeIdentifier [TypeArguments]
  "package">: java "PackageName",
--  ClassOrInterfaceType . {Annotation} TypeIdentifier [TypeArguments]
  "parent">: java "ClassOrInterfaceType"]

--Dims:
--  {Annotation} [ ] {{Annotation} [ ]}
dims :: TypeDefinition
dims = def "Dims" $ T.wrap $ T.list $ T.list $ java "Annotation"

--FloatingPointType:
floatingPointType :: TypeDefinition
floatingPointType = def "FloatingPointType" $ T.enum [
--  (one of)
--  float double
  "float", "double"]

--IntegralType:
integralType :: TypeDefinition
integralType = def "IntegralType" $ T.enum [
--  (one of)
--  byte short int long char
  "byte", "short", "int", "long", "char"]

--InterfaceType:
--  ClassType
interfaceType :: TypeDefinition
interfaceType = def "InterfaceType" $ T.wrap $ java "ClassType"

--NumericType:
numericType :: TypeDefinition
numericType = def "NumericType" $ T.union [
--  IntegralType
  "integral">: java "IntegralType",
--  FloatingPointType
  "floatingPoint">: java "FloatingPointType"]

--PrimitiveType:
primitiveTypeWithAnnotations :: TypeDefinition
primitiveTypeWithAnnotations = def "PrimitiveTypeWithAnnotations" $ T.record [
  "type">: java "PrimitiveType",
  "annotations">: T.list $ java "Annotation"]

primitiveType_ :: TypeDefinition
primitiveType_ = def "PrimitiveType" $ T.union [
--  {Annotation} NumericType
  "numeric">: java "NumericType",
--  {Annotation} boolean
  "boolean">: T.unit]

--ReferenceType:
referenceType :: TypeDefinition
referenceType = def "ReferenceType" $ T.union [
--  ClassOrInterfaceType
  "classOrInterface">: java "ClassOrInterfaceType",
--  TypeVariable
  "variable">: java "TypeVariable",
--  ArrayType
  "array">: java "ArrayType"]

--TypeArgument:
typeArgument :: TypeDefinition
typeArgument = def "TypeArgument" $ T.union [
--  ReferenceType
  "reference">: java "ReferenceType",
--  Wildcard
  "wildcard">: java "Wildcard"]

--TypeBound:
typeBound :: TypeDefinition
typeBound = def "TypeBound" $ T.union [
--  extends TypeVariable
  "variable">: java "TypeVariable",
--  extends ClassOrInterfaceType {AdditionalBound}
  "classOrInterface">: java "TypeBound_ClassOrInterface"]

typeBound_ClassOrInterface :: TypeDefinition
typeBound_ClassOrInterface = def "TypeBound_ClassOrInterface" $ T.record [
  "type">: java "ClassOrInterfaceType",
  "additional">: T.list $ java "AdditionalBound"]

--TypeParameter:
--  {TypeParameterModifier} TypeIdentifier [TypeBound]
typeParameter :: TypeDefinition
typeParameter = def "TypeParameter" $ T.record [
  "modifiers">: T.list $ java "TypeParameterModifier",
  "identifier">: java "TypeIdentifier",
  "bound">: T.maybe $ java "TypeBound"]

--TypeParameterModifier:
--  Annotation
typeParameterModifier :: TypeDefinition
typeParameterModifier = def "TypeParameterModifier" $ T.wrap $ java "Annotation"

--TypeVariable:
--  {Annotation} TypeIdentifier
typeVariable :: TypeDefinition
typeVariable = def "TypeVariable" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "identifier">: java "TypeIdentifier"]

--Type:
type_ :: TypeDefinition
type_ = def "Type" $ T.union [
--  PrimitiveType
    "primitive">: java "PrimitiveTypeWithAnnotations",
--  ReferenceType
    "reference">: java "ReferenceType"]

--Wildcard:
--  {Annotation} ? [WildcardBounds]
wildcard :: TypeDefinition
wildcard = def "Wildcard" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "wildcard">: T.maybe $ java "WildcardBounds"]

--WildcardBounds:
wildcardBounds :: TypeDefinition
wildcardBounds = def "WildcardBounds" $ T.union [
--  extends ReferenceType
  "extends">: java "ReferenceType",
--  super ReferenceType
  "super">: java "ReferenceType"]

--Productions from §6 (Names)

--AmbiguousName:
--  Identifier
--  AmbiguousName . Identifier
ambiguousName :: TypeDefinition
ambiguousName = def "AmbiguousName" $ T.wrap $ T.list $ java "Identifier"

--ExpressionName:
--  Identifier
--  AmbiguousName . Identifier
expressionName :: TypeDefinition
expressionName = def "ExpressionName" $ T.record [
  "qualifier">: T.maybe $ java "AmbiguousName",
  "identifier">: java "Identifier"]

--MethodName:
--  Identifier
methodName :: TypeDefinition
methodName = def "MethodName" $ T.wrap $ java "Identifier"

--ModuleName:
moduleNameDef :: TypeDefinition
moduleNameDef = def "ModuleName" $ T.record [
--  Identifier
  "identifier">: java "Identifier",
--  ModuleName . Identifier
  "name">: T.maybe $ java "ModuleName"]

--PackageName:
--  Identifier
--  PackageName . Identifier
packageName :: TypeDefinition
packageName = def "PackageName" $ T.wrap $ T.list $ java "Identifier"

--PackageOrTypeName:
--  Identifier
--  PackageOrTypeName . Identifier
packageOrTypeName :: TypeDefinition
packageOrTypeName = def "PackageOrTypeName" $ T.wrap $ T.list $ java "Identifier"

--TypeName:
typeName :: TypeDefinition
typeName = def "TypeName" $ T.record [
--  TypeIdentifier
  "identifier">: java "TypeIdentifier",
--  PackageOrTypeName . TypeIdentifier
  "qualifier">: T.maybe $ java "PackageOrTypeName"]

--Productions from §7 (Packages and Modules)

--CompilationUnit:
compilationUnit :: TypeDefinition
compilationUnit = def "CompilationUnit" $ T.union [
--  OrdinaryCompilationUnit
  "ordinary">: java "OrdinaryCompilationUnit",
--  ModularCompilationUnit
  "modular">: java "ModularCompilationUnit"]

--ImportDeclaration:
importDeclaration :: TypeDefinition
importDeclaration = def "ImportDeclaration" $ T.union [
--  SingleTypeImportDeclaration
  "singleType">: java "SingleTypeImportDeclaration",
--  TypeImportOnDemandDeclaration
  "typeImportOnDemand">: java "TypeImportOnDemandDeclaration",
--  SingleStaticImportDeclaration
  "singleStaticImport">: java "SingleStaticImportDeclaration",
--  StaticImportOnDemandDeclaration
  "staticImportOnDemand">: java "StaticImportOnDemandDeclaration"]

--ModularCompilationUnit:
--  {ImportDeclaration} ModuleDeclaration
modularCompilationUnit :: TypeDefinition
modularCompilationUnit = def "ModularCompilationUnit" $ T.record [
  "imports">: T.list $ java "ImportDeclaration",
  "module">: java "ModuleDeclaration"]

--ModuleDeclaration:
--  {Annotation} [open] module Identifier {. Identifier} { {ModuleDirective} }
moduleDeclaration :: TypeDefinition
moduleDeclaration = def "ModuleDeclaration" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "open">: T.boolean,
  "identifiers">: T.list $ java "Identifier",
  "directives">: T.list $ java "ModuleDirective"]

--ModuleDirective:
moduleDirective :: TypeDefinition
moduleDirective = def "ModuleDirective" $ T.union [
--  requires {RequiresModifier} ModuleName ;
  "requires">: java "ModuleDirective_Requires",
--  exports PackageName [to ModuleName {, ModuleName}] ;
  "exports">: java "ModuleDirective_ExportsOrOpens",
--  opens PackageName [to ModuleName {, ModuleName}] ;
  "opens">: java "ModuleDirective_ExportsOrOpens",
--  uses TypeName ;
  "uses">: java "TypeName",
--  provides TypeName with TypeName {, TypeName} ;
  "provides">: java "ModuleDirective_Provides"]

moduleDirective_ExportsOrOpens :: TypeDefinition
moduleDirective_ExportsOrOpens = def "ModuleDirective_ExportsOrOpens" $ T.record [
  "package">: java "PackageName",
  "modules">:
    doc "At least one module" $
    T.list $ java "ModuleName"]

moduleDirective_Provides :: TypeDefinition
moduleDirective_Provides = def "ModuleDirective_Provides" $ T.record [
  "to">: java "TypeName",
  "with">:
    doc "At least one type" $
    T.list $ java "TypeName"]

moduleDirective_Requires :: TypeDefinition
moduleDirective_Requires = def "ModuleDirective_Requires" $ T.record [
  "modifiers">: T.list $ java "RequiresModifier",
  "module">: java "ModuleName"]

--OrdinaryCompilationUnit:
--  [PackageDeclaration] {ImportDeclaration} {TopLevelClassOrInterfaceDeclaration}
ordinaryCompilationUnit :: TypeDefinition
ordinaryCompilationUnit = def "OrdinaryCompilationUnit" $ T.record [
  "package">: T.maybe $ java "PackageDeclaration",
  "imports">: T.list $ java "ImportDeclaration",
  "types">: T.list $ java "TopLevelClassOrInterfaceDeclarationWithComments"]

--PackageDeclaration:
--  {PackageModifier} package Identifier {. Identifier} ;
packageDeclaration :: TypeDefinition
packageDeclaration = def "PackageDeclaration" $ T.record [
  "modifiers">: T.list $ java "PackageModifier",
  "identifiers">: T.list $ java "Identifier"]

--PackageModifier:
--  Annotation
packageModifier :: TypeDefinition
packageModifier = def "PackageModifier" $ T.wrap $ java "Annotation"

--RequiresModifier:
requiresModifier :: TypeDefinition
requiresModifier = def "RequiresModifier" $ T.enum [
--  (one of)
--  transitive static
  "transitive", "static"]

--SingleStaticImportDeclaration:
--  import static TypeName . Identifier ;
singleStaticImportDeclaration :: TypeDefinition
singleStaticImportDeclaration = def "SingleStaticImportDeclaration" $ T.record [
  "typeName">: java "TypeName",
  "identifier">: java "Identifier"]

--SingleTypeImportDeclaration:
--  import TypeName ;
singleTypeImportDeclaration :: TypeDefinition
singleTypeImportDeclaration = def "SingleTypeImportDeclaration" $ T.wrap $ java "TypeName"

--StaticImportOnDemandDeclaration:
--  import static TypeName . * ;
staticImportOnDemandDeclaration :: TypeDefinition
staticImportOnDemandDeclaration = def "StaticImportOnDemandDeclaration" $ T.wrap $ java "TypeName"

--TopLevelClassOrInterfaceDeclaration:
--  (renamed from TypeDeclaration in SE 16)
topLevelClassOrInterfaceDeclaration :: TypeDefinition
topLevelClassOrInterfaceDeclaration = def "TopLevelClassOrInterfaceDeclaration" $ T.union [
--  ClassDeclaration
  "class">: java "ClassDeclaration",
--  InterfaceDeclaration
  "interface">: java "InterfaceDeclaration",
--  ;
  "none">: T.unit]

topLevelClassOrInterfaceDeclarationWithComments :: TypeDefinition
topLevelClassOrInterfaceDeclarationWithComments = def "TopLevelClassOrInterfaceDeclarationWithComments" $
  T.record [
    "value">: java "TopLevelClassOrInterfaceDeclaration",
    "comments">: T.maybe T.string]

--TypeImportOnDemandDeclaration:
--  import PackageOrTypeName . * ;
typeImportOnDemandDeclaration :: TypeDefinition
typeImportOnDemandDeclaration = def "TypeImportOnDemandDeclaration" $ T.wrap $ java "PackageOrTypeName"

--Productions from §8 (Classes)

--ClassBody:
--  { {ClassBodyDeclaration} }
classBody :: TypeDefinition
classBody = def "ClassBody" $ T.wrap $ T.list $ java "ClassBodyDeclarationWithComments"

--ClassBodyDeclaration:
classBodyDeclaration :: TypeDefinition
classBodyDeclaration = def "ClassBodyDeclaration" $ T.union [
--  ClassMemberDeclaration
  "classMember">: java "ClassMemberDeclaration",
--  InstanceInitializer
  "instanceInitializer">: java "InstanceInitializer",
--  StaticInitializer
  "staticInitializer">: java "StaticInitializer",
--  ConstructorDeclaration
  "constructorDeclaration">: java "ConstructorDeclaration"]

classBodyDeclarationWithComments :: TypeDefinition
classBodyDeclarationWithComments = def "ClassBodyDeclarationWithComments" $
  T.record [
    "value">: java "ClassBodyDeclaration",
    "comments">: T.maybe T.string]

--ClassDeclaration:
classDeclaration :: TypeDefinition
classDeclaration = def "ClassDeclaration" $ T.union [
--  NormalClassDeclaration
  "normal">: java "NormalClassDeclaration",
--  EnumDeclaration
  "enum">: java "EnumDeclaration",
--  RecordDeclaration (JEP 395, Java SE 16)
  "record">: java "RecordDeclaration"]

--ClassMemberDeclaration:
classMemberDeclaration :: TypeDefinition
classMemberDeclaration = def "ClassMemberDeclaration" $ T.union [
--  FieldDeclaration
  "field">: java "FieldDeclaration",
--  MethodDeclaration
  "method">: java "MethodDeclaration",
--  ClassDeclaration
  "class">: java "ClassDeclaration",
--  InterfaceDeclaration
  "interface">: java "InterfaceDeclaration",
--  ;
  "none">: T.unit]

--ClassModifier:
classModifier :: TypeDefinition
classModifier = def "ClassModifier" $ T.union [
--  (one of)
--  Annotation public protected private
--  abstract static final sealed non-sealed strictfp
  "annotation">: java "Annotation",
  "public">: T.unit,
  "protected">: T.unit,
  "private">: T.unit,
  "abstract">: T.unit,
  "static">: T.unit,
  "final">: T.unit,
--  sealed and non-sealed: JEP 409, Java SE 17
  "sealed">: T.unit,
  "nonSealed">: T.unit,
  "strictfp">: T.unit]

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

--ConstructorBody:
--  { [ExplicitConstructorInvocation] [BlockStatements] }
constructorBody :: TypeDefinition
constructorBody = def "ConstructorBody" $ T.record [
  "invocation">: T.maybe $ java "ExplicitConstructorInvocation",
  "statements">: T.list $ java "BlockStatement"]

--ConstructorDeclaration:
--  {ConstructorModifier} ConstructorDeclarator [Throws] ConstructorBody
constructorDeclaration :: TypeDefinition
constructorDeclaration = def "ConstructorDeclaration" $ T.record [
  "modifiers">: T.list $ java "ConstructorModifier",
  "constructor">: java "ConstructorDeclarator",
  "throws">: T.maybe $ java "Throws",
  "body">: java "ConstructorBody"]

--ConstructorDeclarator:
--  [TypeParameters] SimpleTypeName ( [ReceiverParameter ,] [FormalParameterList] )
constructorDeclarator :: TypeDefinition
constructorDeclarator = def "ConstructorDeclarator" $ T.record [
  "parameters">: T.list $ java "TypeParameter",
  "name">: java "SimpleTypeName",
  "receiverParameter">: T.maybe $ java "ReceiverParameter",
  "formalParameters">: nonemptyList $ java "FormalParameter"]

--ConstructorModifier:
--  (one of)
constructorModifier :: TypeDefinition
constructorModifier = def "ConstructorModifier" $ T.union [
--  Annotation public protected private
  "annotation">: java "Annotation",
  "public">: T.unit,
  "protected">: T.unit,
  "private">: T.unit]

--EnumBody:
--  { [EnumConstantList] [,] [EnumBodyDeclarations] }
enumBody :: TypeDefinition
enumBody = def "EnumBody" $ T.wrap $ T.list $ java "EnumBody_Element"

enumBody_Element :: TypeDefinition
enumBody_Element = def "EnumBody_Element" $ T.record [
  "constants">: T.list $ java "EnumConstant",
  "bodyDeclarations">: T.list $ java "ClassBodyDeclaration"]

--EnumConstantList:
--  EnumConstant {, EnumConstant}
--EnumConstant:
--  {EnumConstantModifier} Identifier [( [ArgumentList] )] [ClassBody]
enumConstant :: TypeDefinition
enumConstant = def "EnumConstant" $ T.record [
  "modifiers">: T.list $ java "EnumConstantModifier",
  "identifier">: java "Identifier",
  "arguments">: T.maybe $ T.list $ java "Expression",
  "body">: T.maybe $ java "ClassBody"]

--EnumConstantModifier:
--  Annotation
enumConstantModifier :: TypeDefinition
enumConstantModifier = def "EnumConstantModifier" $ T.wrap $ java "Annotation"

--EnumBodyDeclarations:
--  ; {ClassBodyDeclaration}

--EnumDeclaration:
--  {ClassModifier} enum TypeIdentifier [Superinterfaces] EnumBody
enumDeclaration :: TypeDefinition
enumDeclaration = def "EnumDeclaration" $ T.record [
  "modifiers">: T.list $ java "ClassModifier",
  "identifier">: java "TypeIdentifier",
  "implements">: T.list $ java "InterfaceType",
  "body">: java "EnumBody"]

--ExceptionTypeList:
--  ExceptionType {, ExceptionType}
--ExceptionType:
exceptionType :: TypeDefinition
exceptionType = def "ExceptionType" $ T.union [
--  ClassType
  "class">: java "ClassType",
--  TypeVariable
  "variable">: java "TypeVariable"]

--ExplicitConstructorInvocation:
explicitConstructorInvocation :: TypeDefinition
explicitConstructorInvocation = def "ExplicitConstructorInvocation" $ T.record [
  "typeArguments">: T.list $ java "TypeArgument",
  "arguments">: T.list $ java "Expression",
  "variant">: java "ExplicitConstructorInvocation_Variant"]

explicitConstructorInvocation_Variant :: TypeDefinition
explicitConstructorInvocation_Variant = def "ExplicitConstructorInvocation_Variant" $ T.union [
--  [TypeArguments] this ( [ArgumentList] ) ;
  "this">: T.unit,
--  [TypeArguments] super ( [ArgumentList] ) ;
--  ExpressionName . [TypeArguments] super ( [ArgumentList] ) ;
  "super">: T.maybe $ java "ExpressionName",
--  Primary . [TypeArguments] super ( [ArgumentList] ) ;
  "primary">: java "Primary"]

--FieldDeclaration:
--  {FieldModifier} UnannType VariableDeclaratorList ;
fieldDeclaration :: TypeDefinition
fieldDeclaration = def "FieldDeclaration" $ T.record [
  "modifiers">: T.list $ java "FieldModifier",
  "unannType">: java "UnannType",
  "variableDeclarators">: nonemptyList $ java "VariableDeclarator"]

--FieldModifier:
--  (one of)
fieldModifier :: TypeDefinition
fieldModifier = def "FieldModifier" $ T.union [
--  Annotation public protected private
--  static final transient volatile
  "annotation">: java "Annotation",
  "public">: T.unit,
  "protected">: T.unit,
  "private">: T.unit,
  "static">: T.unit,
  "final">: T.unit,
  "transient">: T.unit,
  "volatile">: T.unit]

--FormalParameterList:
--  FormalParameter {, FormalParameter}
--FormalParameter:
formalParameter :: TypeDefinition
formalParameter = def "FormalParameter" $ T.union [
--  {VariableModifier} UnannType VariableDeclaratorId
  "simple">: java "FormalParameter_Simple",
--  VariableArityParameter
  "variableArity">: java "VariableArityParameter"]

formalParameter_Simple :: TypeDefinition
formalParameter_Simple = def "FormalParameter_Simple" $ T.record [
  "modifiers">: T.list $ java "VariableModifier",
  "type">: java "UnannType",
  "id">: java "VariableDeclaratorId"]

--InstanceInitializer:
--  Block
instanceInitializer :: TypeDefinition
instanceInitializer = def "InstanceInitializer" $ T.wrap $ java "Block"

--MethodBody:
methodBody :: TypeDefinition
methodBody = def "MethodBody" $ T.union [
--  Block
  "block">: java "Block",
--  ;
  "none">: T.unit]

--MethodDeclaration:
--  {MethodModifier} MethodHeader MethodBody
methodDeclaration :: TypeDefinition
methodDeclaration = def "MethodDeclaration" $ T.record [
  "annotations">:
    doc "Note: simple methods cannot have annotations" $
    T.list $ java "Annotation",
  "modifiers">: T.list $ java "MethodModifier",
  "header">: java "MethodHeader",
  "body">: java "MethodBody"]

--MethodDeclarator:
--  Identifier ( [ReceiverParameter ,] [FormalParameterList] ) [Dims]
methodDeclarator :: TypeDefinition
methodDeclarator = def "MethodDeclarator" $ T.record [
  "identifier">: java "Identifier",
  "receiverParameter">: T.maybe $ java "ReceiverParameter",
  "formalParameters">: nonemptyList $ java "FormalParameter"]

--MethodHeader:
--  Result MethodDeclarator [Throws]
--  TypeParameters {Annotation} Result MethodDeclarator [Throws]
methodHeader :: TypeDefinition
methodHeader = def "MethodHeader" $ T.record [
  "parameters">: T.list $ java "TypeParameter",
  "result">: java "Result",
  "declarator">: java "MethodDeclarator",
  "throws">: T.maybe $ java "Throws"]

--MethodModifier:
--  (one of)
methodModifier :: TypeDefinition
methodModifier = def "MethodModifier" $ T.union [
--  Annotation public protected private
--  abstract static final synchronized native strictfp
  "annotation">: java "Annotation",
  "public">: T.unit,
  "protected">: T.unit,
  "private">: T.unit,
  "abstract">: T.unit,
  "static">: T.unit,
  "final">: T.unit,
  "synchronized">: T.unit,
  "native">: T.unit,
  "strictfp">: T.unit]

--NormalClassDeclaration:
--  {ClassModifier} class TypeIdentifier [TypeParameters] [ClassExtends] [ClassImplements] [ClassPermits] ClassBody
normalClassDeclaration :: TypeDefinition
normalClassDeclaration = def "NormalClassDeclaration" $ T.record [
  "modifiers">: T.list $ java "ClassModifier",
  "identifier">: java "TypeIdentifier",
  "parameters">: T.list $ java "TypeParameter",
  "extends">: T.maybe $ java "ClassType",
  "implements">: T.list $ java "InterfaceType",
--  ClassPermits:
--    permits TypeName {, TypeName}
--  (JEP 409, Java SE 17. An empty list means absent.)
  "permits">: T.list $ java "TypeName",
  "body">: java "ClassBody"]

--ReceiverParameter:
--  {Annotation} UnannType [Identifier .] this
receiverParameter :: TypeDefinition
receiverParameter = def "ReceiverParameter" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "unannType">: java "UnannType",
  "identifier">: T.maybe $ java "Identifier"]

--Result:
result :: TypeDefinition
result = def "Result" $ T.union [
--  UnannType
  "type">: java "UnannType",
--  void
  "void">: T.unit]

--SimpleTypeName:
--  TypeIdentifier
simpleTypeName :: TypeDefinition
simpleTypeName = def "SimpleTypeName" $ T.wrap $ java "TypeIdentifier"

--StaticInitializer:
--  static Block
staticInitializer :: TypeDefinition
staticInitializer = def "StaticInitializer" $ T.wrap $ java "Block"

--Throws:
--  throws ExceptionTypeList
throws :: TypeDefinition
throws = def "Throws" $ T.wrap $ nonemptyList $ java "ExceptionType"

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
unannClassType :: TypeDefinition
unannClassType = def "UnannClassType" $
  doc "A ClassType which does not allow annotations" $
  T.wrap $ java "ClassType"

--UnannInterfaceType:
--  UnannClassType
--UnannTypeVariable:
--  TypeIdentifier
--UnannArrayType:
--  UnannPrimitiveType Dims
--  UnannClassOrInterfaceType Dims
--  UnannTypeVariable Dims

--UnannType:
--  UnannPrimitiveType
--  UnannReferenceType
unannType :: TypeDefinition
unannType = def "UnannType" $
  doc "A Type which does not allow annotations" $
  T.wrap $ java "Type"

--VariableArityParameter:
--  {VariableModifier} UnannType {Annotation} ... Identifier
variableArityParameter :: TypeDefinition
variableArityParameter = def "VariableArityParameter" $ T.record [
  "modifiers">: T.list $ java "VariableModifier",
  "type">: java "UnannType",
  "annotations">: T.list $ java "Annotation",
  "identifier">: java "Identifier"]

--VariableDeclaratorList:
--  VariableDeclarator {, VariableDeclarator}
--VariableDeclarator:
--  VariableDeclaratorId [= VariableInitializer]
variableDeclarator :: TypeDefinition
variableDeclarator = def "VariableDeclarator" $ T.record [
  "id">: java "VariableDeclaratorId",
  "initializer">: T.maybe $ java "VariableInitializer"]

--VariableDeclaratorId:
--  Identifier [Dims]
variableDeclaratorId :: TypeDefinition
variableDeclaratorId = def "VariableDeclaratorId" $ T.record [
  "identifier">: java "Identifier",
  "dims">: T.maybe $ java "Dims"]

--VariableInitializer:
variableInitializer :: TypeDefinition
variableInitializer = def "VariableInitializer" $ T.union [
--  Expression
  "expression">: java "Expression",
--  ArrayInitializer
  "arrayInitializer">: java "ArrayInitializer"]

--VariableModifier:
variableModifier :: TypeDefinition
variableModifier = def "VariableModifier" $ T.union [
--  Annotation
  "annotation">: java "Annotation",
--  final
  "final">: T.unit]

-- Records (JEP 395, Java SE 16)

--CompactConstructorDeclaration:
--  {ConstructorModifier} SimpleTypeName ConstructorBody
compactConstructorDeclaration :: TypeDefinition
compactConstructorDeclaration = def "CompactConstructorDeclaration" $ T.record [
  "modifiers">: T.list $ java "ConstructorModifier",
  "name">: java "SimpleTypeName",
  "body">: java "ConstructorBody"]

--RecordBody:
--  { {RecordBodyDeclaration} }
recordBody :: TypeDefinition
recordBody = def "RecordBody" $ T.wrap $ T.list $ java "RecordBodyDeclaration"

--RecordBodyDeclaration:
recordBodyDeclaration :: TypeDefinition
recordBodyDeclaration = def "RecordBodyDeclaration" $ T.union [
--  ClassBodyDeclaration
  "classBody">: java "ClassBodyDeclaration",
--  CompactConstructorDeclaration
  "compactConstructor">: java "CompactConstructorDeclaration"]

--RecordComponent:
recordComponent :: TypeDefinition
recordComponent = def "RecordComponent" $ T.union [
--  {RecordComponentModifier} UnannType Identifier
  "simple">: java "RecordComponent_Simple",
--  VariableArityRecordComponent
  "variableArity">: java "VariableArityRecordComponent"]

--RecordComponentModifier:
--  Annotation
recordComponentModifier :: TypeDefinition
recordComponentModifier = def "RecordComponentModifier" $ T.wrap $ java "Annotation"

recordComponent_Simple :: TypeDefinition
recordComponent_Simple = def "RecordComponent_Simple" $ T.record [
  "modifiers">: T.list $ java "RecordComponentModifier",
  "type">: java "UnannType",
  "identifier">: java "Identifier"]

--RecordDeclaration:
--  {ClassModifier} record TypeIdentifier [TypeParameters] RecordHeader [ClassImplements] RecordBody
recordDeclaration :: TypeDefinition
recordDeclaration = def "RecordDeclaration" $ T.record [
  "modifiers">: T.list $ java "ClassModifier",
  "identifier">: java "TypeIdentifier",
  "parameters">: T.list $ java "TypeParameter",
  "header">: java "RecordHeader",
  "implements">: T.list $ java "InterfaceType",
  "body">: java "RecordBody"]

--RecordHeader:
--  ( [RecordComponentList] )
--RecordComponentList:
--  RecordComponent {, RecordComponent}
recordHeader :: TypeDefinition
recordHeader = def "RecordHeader" $ T.wrap $ T.list $ java "RecordComponent"

--VariableArityRecordComponent:
--  {RecordComponentModifier} UnannType {Annotation} ... Identifier
variableArityRecordComponent :: TypeDefinition
variableArityRecordComponent = def "VariableArityRecordComponent" $ T.record [
  "modifiers">: T.list $ java "RecordComponentModifier",
  "type">: java "UnannType",
  "annotations">: T.list $ java "Annotation",
  "identifier">: java "Identifier"]

--Productions from §9 (Interfaces)

--Annotation:
annotation :: TypeDefinition
annotation = def "Annotation" $ T.union [
--  NormalAnnotation
  "normal">: java "NormalAnnotation",
--  MarkerAnnotation
  "marker">: java "MarkerAnnotation",
--  SingleElementAnnotation
  "singleElement">: java "SingleElementAnnotation"]

--AnnotationInterfaceBody:
--  { {AnnotationInterfaceMemberDeclaration} }
annotationInterfaceBody :: TypeDefinition
annotationInterfaceBody = def "AnnotationInterfaceBody" $ T.wrap $ T.list $ java "AnnotationInterfaceMemberDeclaration"

--AnnotationInterfaceDeclaration:
--  {InterfaceModifier} @ interface TypeIdentifier AnnotationInterfaceBody
annotationInterfaceDeclaration :: TypeDefinition
annotationInterfaceDeclaration = def "AnnotationInterfaceDeclaration" $ T.record [
  "modifiers">: T.list $ java "InterfaceModifier",
  "identifier">: java "TypeIdentifier",
  "body">: java "AnnotationInterfaceBody"]

--AnnotationInterfaceElementDeclaration:
--  {AnnotationInterfaceElementModifier} UnannType Identifier ( ) [Dims] [DefaultValue] ;
annotationInterfaceElementDeclaration :: TypeDefinition
annotationInterfaceElementDeclaration = def "AnnotationInterfaceElementDeclaration" $ T.record [
  "modifiers">: T.list $ java "AnnotationInterfaceElementModifier",
  "type">: java "UnannType",
  "identifier">: java "Identifier",
  "dims">: T.maybe $ java "Dims",
  "default">: T.maybe $ java "DefaultValue"]

--AnnotationInterfaceElementModifier:
--  (one of)
annotationInterfaceElementModifier :: TypeDefinition
annotationInterfaceElementModifier = def "AnnotationInterfaceElementModifier" $ T.union [
--  Annotation
  "annotation">: java "Annotation",
--  public
  "public">: T.unit,
--  abstract
  "abstract">: T.unit]

--AnnotationInterfaceMemberDeclaration:
annotationInterfaceMemberDeclaration :: TypeDefinition
annotationInterfaceMemberDeclaration = def "AnnotationInterfaceMemberDeclaration" $ T.union [
--  AnnotationInterfaceElementDeclaration
  "annotationInterface">: java "AnnotationInterfaceElementDeclaration",
--  ConstantDeclaration
  "constant">: java "ConstantDeclaration",
--  ClassDeclaration
  "class">: java "ClassDeclaration",
--  InterfaceDeclaration
  "interface">: java "InterfaceDeclaration"]
--  ;

--ConstantDeclaration:
--  {ConstantModifier} UnannType VariableDeclaratorList ;
constantDeclaration :: TypeDefinition
constantDeclaration = def "ConstantDeclaration" $ T.record [
  "modifiers">: T.list $ java "ConstantModifier",
  "type">: java "UnannType",
  "variables">: nonemptyList $ java "VariableDeclarator"]

--ConstantModifier:
--  (one of)
constantModifier :: TypeDefinition
constantModifier = def "ConstantModifier" $ T.union [
--  Annotation public
--  static final
  "annotation">: java "Annotation",
  "public">: T.unit,
  "static">: T.unit,
  "final">: T.unit]

--DefaultValue:
--  default ElementValue
defaultValue :: TypeDefinition
defaultValue = def "DefaultValue" $ T.wrap $ java "ElementValue"

--ElementValue:
elementValue :: TypeDefinition
elementValue = def "ElementValue" $ T.union [
--  ConditionalExpression
  "conditionalExpression">: java "ConditionalExpression",
--  ElementValueArrayInitializer
  "elementValueArrayInitializer">: java "ElementValueArrayInitializer",
--  Annotation
  "annotation">: java "Annotation"]

--ElementValueArrayInitializer:
--  { [ElementValueList] [,] }
elementValueArrayInitializer :: TypeDefinition
elementValueArrayInitializer = def "ElementValueArrayInitializer" $ T.wrap $ T.list $ java "ElementValue"

--ElementValueList:
--  ElementValue {, ElementValue}

--ElementValuePairList:
--  ElementValuePair {, ElementValuePair}
--ElementValuePair:
--  Identifier = ElementValue
elementValuePair :: TypeDefinition
elementValuePair = def "ElementValuePair" $ T.record [
  "key">: java "Identifier",
  "value">: java "ElementValue"]

--InterfaceBody:
--  { {InterfaceMemberDeclaration} }
interfaceBody :: TypeDefinition
interfaceBody = def "InterfaceBody" $ T.wrap $ T.list $ java "InterfaceMemberDeclarationWithComments"

--InterfaceDeclaration:
interfaceDeclaration :: TypeDefinition
interfaceDeclaration = def "InterfaceDeclaration" $ T.union [
--  NormalInterfaceDeclaration
  "normalInterface">: java "NormalInterfaceDeclaration",
--  AnnotationInterfaceDeclaration (renamed from AnnotationInterfaceDeclaration in SE 16)
  "annotationInterface">: java "AnnotationInterfaceDeclaration"]

--InterfaceMemberDeclaration:
interfaceMemberDeclaration :: TypeDefinition
interfaceMemberDeclaration = def "InterfaceMemberDeclaration" $ T.union [
--  ConstantDeclaration
  "constant">: java "ConstantDeclaration",
--  InterfaceMethodDeclaration
  "interfaceMethod">: java "InterfaceMethodDeclaration",
--  ClassDeclaration
  "class">: java "ClassDeclaration",
--  InterfaceDeclaration
  "interface">: java "InterfaceDeclaration"]
--  ;

interfaceMemberDeclarationWithComments :: TypeDefinition
interfaceMemberDeclarationWithComments = def "InterfaceMemberDeclarationWithComments" $
  T.record [
    "value">: java "InterfaceMemberDeclaration",
    "comments">: T.maybe T.string]

--InterfaceMethodDeclaration:
--  {InterfaceMethodModifier} MethodHeader MethodBody
interfaceMethodDeclaration :: TypeDefinition
interfaceMethodDeclaration = def "InterfaceMethodDeclaration" $ T.record [
  "modifiers">: T.list $ java "InterfaceMethodModifier",
  "header">: java "MethodHeader",
  "body">: java "MethodBody"]

--InterfaceMethodModifier:
--  (one of)
interfaceMethodModifier :: TypeDefinition
interfaceMethodModifier = def "InterfaceMethodModifier" $ T.union [
--  Annotation public private
--  abstract default static strictfp
  "annotation">: java "Annotation",
  "public">: T.unit,
  "private">: T.unit,
  "abstract">: T.unit,
  "default">: T.unit,
  "static">: T.unit,
  "strictfp">: T.unit]

--InterfaceModifier:
--  (one of)
interfaceModifier :: TypeDefinition
interfaceModifier = def "InterfaceModifier" $ T.union [
--  Annotation public protected private
--  abstract static sealed non-sealed strictfp
  "annotation">: java "Annotation",
  "public">: T.unit,
  "protected">: T.unit,
  "private">: T.unit,
  "abstract">: T.unit,
  "static">: T.unit,
--  sealed and non-sealed: JEP 409, Java SE 17
  "sealed">: T.unit,
  "nonSealed">: T.unit,
  "strictfp">: T.unit]

--ExtendsInterfaces:
--  extends InterfaceTypeList

--MarkerAnnotation:
--  @ TypeName
markerAnnotation :: TypeDefinition
markerAnnotation = def "MarkerAnnotation" $ T.wrap $ java "TypeName"

--NormalAnnotation:
--  @ TypeName ( [ElementValuePairList] )
normalAnnotation :: TypeDefinition
normalAnnotation = def "NormalAnnotation" $ T.record [
  "typeName">: java "TypeName",
  "pairs">: T.list $ java "ElementValuePair"]

--NormalInterfaceDeclaration:
--  {InterfaceModifier} interface TypeIdentifier [TypeParameters] [InterfaceExtends] [InterfacePermits] InterfaceBody
normalInterfaceDeclaration :: TypeDefinition
normalInterfaceDeclaration = def "NormalInterfaceDeclaration" $ T.record [
  "modifiers">: T.list $ java "InterfaceModifier",
  "identifier">: java "TypeIdentifier",
  "parameters">: T.list $ java "TypeParameter",
  "extends">: T.list $ java "InterfaceType",
--  InterfacePermits:
--    permits TypeName {, TypeName}
--  (JEP 409, Java SE 17. An empty list means absent.)
  "permits">: T.list $ java "TypeName",
  "body">: java "InterfaceBody"]

--SingleElementAnnotation:
singleElementAnnotation :: TypeDefinition
singleElementAnnotation = def "SingleElementAnnotation" $ T.record [
--  @ TypeName ( ElementValue )
  "name">: java "TypeName",
  "value">: T.maybe $ java "ElementValue"]

--  Productions from §10 (Arrays)

--ArrayInitializer:
--  { [VariableInitializerList] [,] }
arrayInitializer :: TypeDefinition
arrayInitializer = def "ArrayInitializer" $ T.wrap $ T.list $ T.list $ java "VariableInitializer"

--VariableInitializerList:
--  VariableInitializer {, VariableInitializer}

--Productions from §14 (Blocks and Statements)

--AssertStatement:
assertStatement :: TypeDefinition
assertStatement = def "AssertStatement" $ T.union [
--  assert Expression ;
  "single">: java "Expression",
--  assert Expression : Expression ;
  "pair">: java "AssertStatement_Pair"]

assertStatement_Pair :: TypeDefinition
assertStatement_Pair = def "AssertStatement_Pair" $ T.record [
  "first">: java "Expression",
  "second">: java "Expression"]

--Block:
--  { [BlockStatements] }
block :: TypeDefinition
block = def "Block" $ T.wrap $ T.list $ java "BlockStatement"

--BlockStatements:
--  BlockStatement {BlockStatement}
--BlockStatement:
blockStatement :: TypeDefinition
blockStatement = def "BlockStatement" $ T.union [
--  LocalVariableDeclarationStatement
  "localVariableDeclaration">: java "LocalVariableDeclarationStatement",
--  LocalClassOrInterfaceDeclaration (broadened from ClassDeclaration in SE 16)
  "localClassOrInterface">: java "LocalClassOrInterfaceDeclaration",
--  Statement
  "statement">: java "Statement"]

--CaseConstant:
--  ConditionalExpression
caseConstant :: TypeDefinition
caseConstant = def "CaseConstant" $ T.wrap $ java "ConditionalExpression"

--ExpressionStatement:
--  StatementExpression ;
expressionStatement :: TypeDefinition
expressionStatement = def "ExpressionStatement" $ T.wrap $ java "StatementExpression"

--IfThenElseStatement:
--  if ( Expression ) StatementNoShortIf else Statement
ifThenElseStatement :: TypeDefinition
ifThenElseStatement = def "IfThenElseStatement" $ T.record [
  "cond">: T.maybe $ java "Expression",
  "then">: java "StatementNoShortIf",
  "else">: java "Statement"]

--IfThenElseStatementNoShortIf:
--  if ( Expression ) StatementNoShortIf else StatementNoShortIf
ifThenElseStatementNoShortIf :: TypeDefinition
ifThenElseStatementNoShortIf = def "IfThenElseStatementNoShortIf" $ T.record [
  "cond">: T.maybe $ java "Expression",
  "then">: java "StatementNoShortIf",
  "else">: java "StatementNoShortIf"]

--IfThenStatement:
--  if ( Expression ) Statement
ifThenStatement :: TypeDefinition
ifThenStatement = def "IfThenStatement" $ T.record [
  "expression">: java "Expression",
  "statement">: java "Statement"]

--EmptyStatement:
--  ;
--LabeledStatement:
--  Identifier : Statement
labeledStatement :: TypeDefinition
labeledStatement = def "LabeledStatement" $ T.record [
  "identifier">: java "Identifier",
  "statement">: java "Statement"]

--LabeledStatementNoShortIf:
--  Identifier : StatementNoShortIf
labeledStatementNoShortIf :: TypeDefinition
labeledStatementNoShortIf = def "LabeledStatementNoShortIf" $ T.record [
  "identifier">: java "Identifier",
  "statement">: java "StatementNoShortIf"]

--LocalClassOrInterfaceDeclaration:
localClassOrInterfaceDeclaration :: TypeDefinition
localClassOrInterfaceDeclaration = def "LocalClassOrInterfaceDeclaration" $ T.union [
--  ClassDeclaration
  "class">: java "ClassDeclaration",
--  NormalInterfaceDeclaration
  "normalInterface">: java "NormalInterfaceDeclaration"]

--LocalVariableDeclaration:
--  {VariableModifier} LocalVariableType VariableDeclaratorList
localVariableDeclaration :: TypeDefinition
localVariableDeclaration = def "LocalVariableDeclaration" $ T.record [
  "modifiers">: T.list $ java "VariableModifier",
  "type">: java "LocalVariableType",
  "declarators">: nonemptyList $ java "VariableDeclarator"]

--LocalVariableDeclarationStatement:
--  LocalVariableDeclaration ;
localVariableDeclarationStatement :: TypeDefinition
localVariableDeclarationStatement = def "LocalVariableDeclarationStatement" $ T.wrap $ java "LocalVariableDeclaration"

--LocalVariableType:
localVariableType :: TypeDefinition
localVariableType = def "LocalVariableType" $ T.union [
--  UnannType
  "type">: java "UnannType",
--  var
  "var">: T.unit]

--Statement:
statement :: TypeDefinition
statement = def "Statement" $ T.union [
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
  "for">: java "ForStatement"]

--StatementExpression:
statementExpression :: TypeDefinition
statementExpression = def "StatementExpression" $ T.union [
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
  "classInstanceCreation">: java "ClassInstanceCreationExpression"]

--StatementNoShortIf:
statementNoShortIf :: TypeDefinition
statementNoShortIf = def "StatementNoShortIf" $ T.union [
--  StatementWithoutTrailingSubstatement
  "withoutTrailing">: java "StatementWithoutTrailingSubstatement",
--  LabeledStatementNoShortIf
  "labeled">: java "LabeledStatementNoShortIf",
--  IfThenElseStatementNoShortIf
  "ifThenElse">: java "IfThenElseStatementNoShortIf",
--  WhileStatementNoShortIf
  "while">: java "WhileStatementNoShortIf",
--  ForStatementNoShortIf
  "for">: java "ForStatementNoShortIf"]

--StatementWithoutTrailingSubstatement:
statementWithoutTrailingSubstatement :: TypeDefinition
statementWithoutTrailingSubstatement = def "StatementWithoutTrailingSubstatement" $ T.union [
--  Block
  "block">: java "Block",
--  EmptyStatement
  "empty">: T.unit,
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
  "try">: java "TryStatement",
--  YieldStatement (JEP 361, Java SE 14)
  "yield">: java "YieldStatement"]

--SwitchBlock:
--  { SwitchRule {SwitchRule} }
--  { {SwitchBlockStatementGroup} {SwitchLabel :} }
-- (Reshape: JEP 361, Java SE 14 introduced arrow-style switch rules.)
switchBlock :: TypeDefinition
switchBlock = def "SwitchBlock" $ T.union [
--  Arrow-style: nonempty list of SwitchRule
  "rules">: nonemptyList $ java "SwitchRule",
--  Legacy colon-style
  "legacy">: java "SwitchBlock_Legacy"]

--SwitchBlockStatementGroup:
--  SwitchLabel : {SwitchLabel :} BlockStatements
switchBlockStatementGroup :: TypeDefinition
switchBlockStatementGroup = def "SwitchBlockStatementGroup" $ T.record [
  "labels">: nonemptyList $ java "SwitchLabel",
  "statements">: nonemptyList $ java "BlockStatement"]

switchBlock_Legacy :: TypeDefinition
switchBlock_Legacy = def "SwitchBlock_Legacy" $ T.record [
  "groups">: T.list $ java "SwitchBlockStatementGroup",
  "trailingLabels">: T.list $ java "SwitchLabel"]

--SwitchLabel:
--  case CaseConstant {, CaseConstant}
--  case null [, default]
--  case CasePattern [Guard]
--  default
switchLabel :: TypeDefinition
switchLabel = def "SwitchLabel" $ T.union [
--  case CaseConstant {, CaseConstant}
  "case">: nonemptyList $ java "CaseConstant",
--  case null [, default]   -- the boolean indicates whether ", default" is present
  "null">: T.boolean,
--  case CasePattern (JEP 441, SE 21)
  "casePattern">: java "CasePattern",
--  default
  "default">: T.unit]

--SwitchRule (JEP 361, Java SE 14):
--  SwitchLabel -> Expression ;
--  SwitchLabel -> Block
--  SwitchLabel -> ThrowStatement
switchRule :: TypeDefinition
switchRule = def "SwitchRule" $ T.record [
  "label">: java "SwitchLabel",
  "body">: java "SwitchRule_Body"]

switchRule_Body :: TypeDefinition
switchRule_Body = def "SwitchRule_Body" $ T.union [
  "expression">: java "Expression",
  "block">: java "Block",
  "throw">: java "ThrowStatement"]

--SwitchStatement:
--  switch ( Expression ) SwitchBlock
switchStatement :: TypeDefinition
switchStatement = def "SwitchStatement" $ T.record [
  "cond">: java "Expression",
  "block">: java "SwitchBlock"]

-- Pattern matching (JEPs 440 record patterns, 441 sealed switch / SE 21)

--BasicForStatement:
--  for ( [ForInit] ; [Expression] ; [ForUpdate] ) Statement
basicForStatement :: TypeDefinition
basicForStatement = def "BasicForStatement" $ T.record [
  "cond">: java "ForCond",
  "body">: java "Statement"]

--BasicForStatementNoShortIf:
--  for ( [ForInit] ; [Expression] ; [ForUpdate] ) StatementNoShortIf
basicForStatementNoShortIf :: TypeDefinition
basicForStatementNoShortIf = def "BasicForStatementNoShortIf" $ T.record [
  "cond">: java "ForCond",
  "body">: java "StatementNoShortIf"]

--BreakStatement:
--  break [Identifier] ;
breakStatement :: TypeDefinition
breakStatement = def "BreakStatement" $ T.wrap $ T.maybe $ java "Identifier"

--CasePattern:
--  Pattern [Guard]
casePattern :: TypeDefinition
casePattern = def "CasePattern" $ T.record [
  "pattern">: java "Pattern",
  "guard">: T.maybe $ java "Guard"]

--CatchClause:
--  catch ( CatchFormalParameter ) Block
catchClause :: TypeDefinition
catchClause = def "CatchClause" $ T.record [
  "parameter">: T.maybe $ java "CatchFormalParameter",
  "block">: java "Block"]

--CatchFormalParameter:
--  {VariableModifier} CatchType VariableDeclaratorId
catchFormalParameter :: TypeDefinition
catchFormalParameter = def "CatchFormalParameter" $ T.record [
  "modifiers">: T.list $ java "VariableModifier",
  "type">: java "CatchType",
  "id">: java "VariableDeclaratorId"]

--CatchType:
--  UnannClassType {| ClassType}
catchType :: TypeDefinition
catchType = def "CatchType" $ T.record [
  "type">: java "UnannClassType",
  "types">: T.list $ java "ClassType"]

--Catches:
--  CatchClause {CatchClause}
catches :: TypeDefinition
catches = def "Catches" $ T.wrap $ T.list $ java "CatchClause"

--ContinueStatement:
--  continue [Identifier] ;
continueStatement :: TypeDefinition
continueStatement = def "ContinueStatement" $ T.wrap $ T.maybe $ java "Identifier"

--DoStatement:
--  do Statement while ( Expression ) ;
doStatement :: TypeDefinition
doStatement = def "DoStatement" $ T.record [
  "body">: java "Statement",
  "cond">: java "Expression"]

enhancedForCond :: TypeDefinition
enhancedForCond = def "EnhancedForCond" $ T.record [
  "declaration">: java "LocalVariableDeclaration",
  "expression">: java "Expression"]

--EnhancedForStatement:
--  for ( LocalVariableDeclaration : Expression ) Statement
-- (Reshape: SE 16 simplified the production by reusing LocalVariableDeclaration.)
enhancedForStatement :: TypeDefinition
enhancedForStatement = def "EnhancedForStatement" $ T.record [
  "cond">: java "EnhancedForCond",
  "body">: java "Statement"]

--EnhancedForStatementNoShortIf:
--  for ( LocalVariableDeclaration : Expression ) StatementNoShortIf
enhancedForStatementNoShortIf :: TypeDefinition
enhancedForStatementNoShortIf = def "EnhancedForStatementNoShortIf" $ T.record [
  "cond">: java "EnhancedForCond",
  "body">: java "StatementNoShortIf"]

--Finally:
--  finally Block
finally_ :: TypeDefinition
finally_ = def "Finally" $ T.wrap $ java "Block"

forCond :: TypeDefinition
forCond = def "ForCond" $ T.record [
  "init">: T.maybe $ java "ForInit",
  "cond">: T.maybe $ java "Expression",
  "update">: T.maybe $ java "ForUpdate"]

--ForInit:
forInit :: TypeDefinition
forInit = def "ForInit" $ T.union [
--  StatementExpressionList
  "statements">: nonemptyList $ java "StatementExpression",
--  LocalVariableDeclaration
  "localVariable">: java "LocalVariableDeclaration"]

--ForStatement:
forStatement :: TypeDefinition
forStatement = def "ForStatement" $ T.union [
--  BasicForStatement
  "basic">: java "BasicForStatement",
--  EnhancedForStatement
  "enhanced">: java "EnhancedForStatement"]

--ForStatementNoShortIf:
forStatementNoShortIf :: TypeDefinition
forStatementNoShortIf = def "ForStatementNoShortIf" $ T.union [
--  BasicForStatementNoShortIf
  "basic">: java "BasicForStatementNoShortIf",
--  EnhancedForStatementNoShortIf
  "enhanced">: java "EnhancedForStatementNoShortIf"]

--ForUpdate:
--  StatementExpressionList
forUpdate :: TypeDefinition
forUpdate = def "ForUpdate" $ T.wrap $ nonemptyList $ java "StatementExpression"

--  StatementExpressionList:
--  StatementExpression {, StatementExpression}

--Guard:
--  when Expression
guard :: TypeDefinition
guard = def "Guard" $ T.wrap $ java "Expression"

--Pattern:
pattern_ :: TypeDefinition
pattern_ = def "Pattern" $ T.union [
--  TypePattern
  "type">: java "TypePattern",
--  RecordPattern
  "record">: java "RecordPattern"]

--RecordPattern:
--  ReferenceType ( [PatternList] )
--PatternList:
--  Pattern {, Pattern}
recordPattern :: TypeDefinition
recordPattern = def "RecordPattern" $ T.record [
  "type">: java "ReferenceType",
  "patterns">: T.list $ java "Pattern"]

--ResourceList:
--  Resource {; Resource}
--Resource:
resource :: TypeDefinition
resource = def "Resource" $ T.union [
--  {VariableModifier} LocalVariableType Identifier = Expression
  "local">: java "Resource_Local",
--  VariableAccess
  "variable">: java "VariableAccess"]

--ResourceSpecification:
--  ( ResourceList [;] )
resourceSpecification :: TypeDefinition
resourceSpecification = def "ResourceSpecification" $ T.wrap $ T.list $ java "Resource"

resource_Local :: TypeDefinition
resource_Local = def "Resource_Local" $ T.record [
  "modifiers">: T.list $ java "VariableModifier",
  "type">: java "LocalVariableType",
  "identifier">: java "Identifier",
  "expression">: java "Expression"]

--ReturnStatement:
--  return [Expression] ;
returnStatement :: TypeDefinition
returnStatement = def "ReturnStatement" $ T.wrap $ T.maybe $ java "Expression"

--SynchronizedStatement:
--  synchronized ( Expression ) Block
synchronizedStatement :: TypeDefinition
synchronizedStatement = def "SynchronizedStatement" $ T.record [
  "expression">: java "Expression",
  "block">: java "Block"]

--ThrowStatement:
--  throw Expression ;
throwStatement :: TypeDefinition
throwStatement = def "ThrowStatement" $ T.wrap $ java "Expression"

--TryStatement:
tryStatement :: TypeDefinition
tryStatement = def "TryStatement" $ T.union [
--  try Block Catches
  "simple">: java "TryStatement_Simple",
--  try Block [Catches] Finally
  "withFinally">: java "TryStatement_WithFinally",
--  TryWithResourcesStatement
  "withResources">: java "TryWithResourcesStatement"]

tryStatement_Simple :: TypeDefinition
tryStatement_Simple = def "TryStatement_Simple" $ T.record [
  "block">: java "Block",
  "catches">: java "Catches"]

tryStatement_WithFinally :: TypeDefinition
tryStatement_WithFinally = def "TryStatement_WithFinally" $ T.record [
  "block">: java "Block",
  "catches">: T.maybe $ java "Catches",
  "finally">: java "Finally"]

--TryWithResourcesStatement:
--  try ResourceSpecification Block [Catches] [Finally]
tryWithResourcesStatement :: TypeDefinition
tryWithResourcesStatement = def "TryWithResourcesStatement" $ T.record [
  "resourceSpecification">: java "ResourceSpecification",
  "block">: java "Block",
  "catches">: T.maybe $ java "Catches",
  "finally">: T.maybe $ java "Finally"]

--TypePattern:
--  LocalVariableDeclaration
typePattern :: TypeDefinition
typePattern = def "TypePattern" $ T.wrap $ java "LocalVariableDeclaration"

--VariableAccess:
variableAccess :: TypeDefinition
variableAccess = def "VariableAccess" $ T.union [
--  ExpressionName
  "expressionName">: java "ExpressionName",
--  FieldAccess
  "fieldAccess">: java "FieldAccess"]

--WhileStatement:
--  while ( Expression ) Statement
whileStatement :: TypeDefinition
whileStatement = def "WhileStatement" $ T.record [
  "cond">: T.maybe $ java "Expression",
  "body">: java "Statement"]

--WhileStatementNoShortIf:
--  while ( Expression ) StatementNoShortIf
whileStatementNoShortIf :: TypeDefinition
whileStatementNoShortIf = def "WhileStatementNoShortIf" $ T.record [
  "cond">: T.maybe $ java "Expression",
  "body">: java "StatementNoShortIf"]

--YieldStatement (JEP 361, Java SE 14):
--  yield Expression ;
yieldStatement :: TypeDefinition
yieldStatement = def "YieldStatement" $ T.wrap $ java "Expression"

--Productions from §15 (Expressions)

--AdditiveExpression:
additiveExpression :: TypeDefinition
additiveExpression = def "AdditiveExpression" $ T.union [
--  MultiplicativeExpression
  "unary">: java "MultiplicativeExpression",
--  AdditiveExpression + MultiplicativeExpression
  "plus">: java "AdditiveExpression_Binary",
--  AdditiveExpression - MultiplicativeExpression
  "minus">: java "AdditiveExpression_Binary"]

additiveExpression_Binary :: TypeDefinition
additiveExpression_Binary = def "AdditiveExpression_Binary" $ T.record [
  "lhs">: java "AdditiveExpression",
  "rhs">: java "MultiplicativeExpression"]

--AndExpression:
--  EqualityExpression
--  AndExpression & EqualityExpression
andExpression :: TypeDefinition
andExpression = def "AndExpression" $ T.wrap $ nonemptyList $ java "EqualityExpression"

annotatedIdentifier :: TypeDefinition
annotatedIdentifier = def "AnnotatedIdentifier" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "identifier">: java "Identifier"]

--ArrayAccess:
arrayAccess :: TypeDefinition
arrayAccess = def "ArrayAccess" $ T.record [
  "expression">: T.maybe $ java "Expression",
  "variant">: java "ArrayAccess_Variant"]

arrayAccess_Variant :: TypeDefinition
arrayAccess_Variant = def "ArrayAccess_Variant" $ T.union [
--  ExpressionName [ Expression ]
  "name">: java "ExpressionName",
--  PrimaryNoNewArray [ Expression ]
  "primary">: java "PrimaryNoNewArrayExpression",
--  ArrayCreationExpressionWithInitializer [ Expression ]  (added in SE 21)
  "arrayCreationWithInitializer">: java "ArrayCreationExpressionWithInitializer"]

--ArrayCreationExpression:
--  (SE 21 splits this into two intermediate productions)
arrayCreationExpression :: TypeDefinition
arrayCreationExpression = def "ArrayCreationExpression" $ T.union [
--  ArrayCreationExpressionWithoutInitializer
  "withoutInitializer">: java "ArrayCreationExpressionWithoutInitializer",
--  ArrayCreationExpressionWithInitializer
  "withInitializer">: java "ArrayCreationExpressionWithInitializer"]

--ArrayCreationExpressionWithInitializer:
arrayCreationExpressionWithInitializer :: TypeDefinition
arrayCreationExpressionWithInitializer = def "ArrayCreationExpressionWithInitializer" $ T.union [
--  new PrimitiveType Dims ArrayInitializer
  "primitive">: java "ArrayCreationExpressionWithInitializer_Primitive",
--  new ClassOrInterfaceType Dims ArrayInitializer
  "classOrInterface">: java "ArrayCreationExpressionWithInitializer_ClassOrInterface"]

arrayCreationExpressionWithInitializer_ClassOrInterface :: TypeDefinition
arrayCreationExpressionWithInitializer_ClassOrInterface = def "ArrayCreationExpressionWithInitializer_ClassOrInterface" $ T.record [
  "type">: java "ClassOrInterfaceType",
  "dims">: nonemptyList $ java "Dims",
  "array">: java "ArrayInitializer"]

arrayCreationExpressionWithInitializer_Primitive :: TypeDefinition
arrayCreationExpressionWithInitializer_Primitive = def "ArrayCreationExpressionWithInitializer_Primitive" $ T.record [
  "type">: java "PrimitiveTypeWithAnnotations",
  "dims">: nonemptyList $ java "Dims",
  "array">: java "ArrayInitializer"]

--ArrayCreationExpressionWithoutInitializer:
arrayCreationExpressionWithoutInitializer :: TypeDefinition
arrayCreationExpressionWithoutInitializer = def "ArrayCreationExpressionWithoutInitializer" $ T.union [
--  new PrimitiveType DimExprs [Dims]
  "primitive">: java "ArrayCreationExpressionWithoutInitializer_Primitive",
--  new ClassOrInterfaceType DimExprs [Dims]
  "classOrInterface">: java "ArrayCreationExpressionWithoutInitializer_ClassOrInterface"]

arrayCreationExpressionWithoutInitializer_ClassOrInterface :: TypeDefinition
arrayCreationExpressionWithoutInitializer_ClassOrInterface = def "ArrayCreationExpressionWithoutInitializer_ClassOrInterface" $ T.record [
  "type">: java "ClassOrInterfaceType",
  "dimExprs">: nonemptyList $ java "DimExpr",
  "dims">: T.maybe $ java "Dims"]

arrayCreationExpressionWithoutInitializer_Primitive :: TypeDefinition
arrayCreationExpressionWithoutInitializer_Primitive = def "ArrayCreationExpressionWithoutInitializer_Primitive" $ T.record [
  "type">: java "PrimitiveTypeWithAnnotations",
  "dimExprs">: nonemptyList $ java "DimExpr",
  "dims">: T.maybe $ java "Dims"]

--Assignment:
--  LeftHandSide AssignmentOperator Expression
assignment :: TypeDefinition
assignment = def "Assignment" $ T.record [
  "lhs">: java "LeftHandSide",
  "op">: java "AssignmentOperator",
  "expression">: java "Expression"]

--AssignmentExpression:
assignmentExpression :: TypeDefinition
assignmentExpression = def "AssignmentExpression" $ T.union [
--  ConditionalExpression
  "conditional">: java "ConditionalExpression",
--  Assignment
  "assignment">: java "Assignment"]

--AssignmentOperator:
--  (one of)
assignmentOperator :: TypeDefinition
assignmentOperator = def "AssignmentOperator" $ T.enum [
--  =  *=  /=  %=  +=  -=  <<~  >>=  >>>=  &=  ^=  |=
  "simple", "times", "div", "mod", "plus", "minus",
  "shiftLeft", "shiftRight", "shiftRightZeroFill", "and", "xor", "or"]

booleanArray :: TypeDefinition
booleanArray = def "BooleanArray" $ T.union [
  "simple">: T.unit,
  "array">: java "BooleanArray"]

--CastExpression:
castExpression :: TypeDefinition
castExpression = def "CastExpression" $ T.union [
--  ( PrimitiveType ) UnaryExpression
  "primitive">: java "CastExpression_Primitive",
--  ( ReferenceType {AdditionalBound} ) UnaryExpressionNotPlusMinus
  "notPlusMinus">: java "CastExpression_NotPlusMinus",
--  ( ReferenceType {AdditionalBound} ) LambdaExpression
  "lambda">: java "CastExpression_Lambda"]

castExpression_Lambda :: TypeDefinition
castExpression_Lambda = def "CastExpression_Lambda" $ T.record [
  "refAndBounds">: java "CastExpression_RefAndBounds",
  "expression">: java "LambdaExpression"]

castExpression_NotPlusMinus :: TypeDefinition
castExpression_NotPlusMinus = def "CastExpression_NotPlusMinus" $ T.record [
  "refAndBounds">: java "CastExpression_RefAndBounds",
  "expression">: java "UnaryExpression"]

castExpression_Primitive :: TypeDefinition
castExpression_Primitive = def "CastExpression_Primitive" $ T.record [
  "type">: java "PrimitiveTypeWithAnnotations",
  "expression">: java "UnaryExpression"]

castExpression_RefAndBounds :: TypeDefinition
castExpression_RefAndBounds = def "CastExpression_RefAndBounds" $ T.record [
  "type">: java "ReferenceType",
  "bounds">: T.list $ java "AdditionalBound"]

--ClassInstanceCreationExpression:
--  UnqualifiedClassInstanceCreationExpression
--  ExpressionName . UnqualifiedClassInstanceCreationExpression
--  Primary . UnqualifiedClassInstanceCreationExpression
classInstanceCreationExpression :: TypeDefinition
classInstanceCreationExpression = def "ClassInstanceCreationExpression" $ T.record [
  "qualifier">: T.maybe $ java "ClassInstanceCreationExpression_Qualifier",
  "expression">: java "UnqualifiedClassInstanceCreationExpression"]

classInstanceCreationExpression_Qualifier :: TypeDefinition
classInstanceCreationExpression_Qualifier = def "ClassInstanceCreationExpression_Qualifier" $ T.union [
  "expression">: java "ExpressionName",
  "primary">: java "Primary"]

--ClassLiteral:
classLiteral :: TypeDefinition
classLiteral = def "ClassLiteral" $ T.union [
--  TypeName {[ ]} . class
  "type">: java "TypeNameArray",
--  NumericType {[ ]} . class
  "numericType">: java "NumericTypeArray",
--  boolean {[ ]} . class
  "boolean">: java "BooleanArray",
--  void . class
  "void">: T.unit]

--ClassOrInterfaceTypeToInstantiate:
--  {Annotation} Identifier {. {Annotation} Identifier} [TypeArgumentsOrDiamond]
classOrInterfaceTypeToInstantiate :: TypeDefinition
classOrInterfaceTypeToInstantiate = def "ClassOrInterfaceTypeToInstantiate" $ T.record [
  "identifiers">: nonemptyList $ java "AnnotatedIdentifier",
  "typeArguments">: T.maybe $ java "TypeArgumentsOrDiamond"]

--ConditionalAndExpression:
--  InclusiveOrExpression
--  ConditionalAndExpression && InclusiveOrExpression
conditionalAndExpression :: TypeDefinition
conditionalAndExpression = def "ConditionalAndExpression" $ T.wrap $ nonemptyList $ java "InclusiveOrExpression"

--ConditionalExpression:
conditionalExpression :: TypeDefinition
conditionalExpression = def "ConditionalExpression" $ T.union [
--  ConditionalOrExpression
  "simple">: java "ConditionalOrExpression",
--  ConditionalOrExpression ? Expression : ConditionalExpression
  "ternaryCond">: java "ConditionalExpression_TernaryCond",
--  ConditionalOrExpression ? Expression : LambdaExpression
  "ternaryLambda">: java "ConditionalExpression_TernaryLambda"]

conditionalExpression_TernaryCond :: TypeDefinition
conditionalExpression_TernaryCond = def "ConditionalExpression_TernaryCond" $ T.record [
  "cond">: java "ConditionalOrExpression",
  "ifTrue">: java "Expression",
  "ifFalse">: java "ConditionalExpression"]

conditionalExpression_TernaryLambda :: TypeDefinition
conditionalExpression_TernaryLambda = def "ConditionalExpression_TernaryLambda" $ T.record [
  "cond">: java "ConditionalOrExpression",
  "ifTrue">: java "Expression",
  "ifFalse">: java "LambdaExpression"]

--ConditionalOrExpression:
--  ConditionalAndExpression
--  ConditionalOrExpression || ConditionalAndExpression
conditionalOrExpression :: TypeDefinition
conditionalOrExpression = def "ConditionalOrExpression" $ T.wrap $ nonemptyList $ java "ConditionalAndExpression"

--ConstantExpression:
--  Expression
constantExpression :: TypeDefinition
constantExpression = def "ConstantExpression" $ T.wrap $ java "Expression"

--DimExprs:
--  DimExpr {DimExpr}
--DimExpr:
--  {Annotation} [ Expression ]
dimExpr :: TypeDefinition
dimExpr = def "DimExpr" $ T.record [
  "annotations">: T.list $ java "Annotation",
  "expression">: T.maybe $ java "Expression"]

--EqualityExpression:
equalityExpression :: TypeDefinition
equalityExpression = def "EqualityExpression" $ T.union [
--  RelationalExpression
  "unary">: java "RelationalExpression",
--  EqualityExpression == RelationalExpression
  "equal">: java "EqualityExpression_Binary",
--  EqualityExpression != RelationalExpression
  "notEqual">: java "EqualityExpression_Binary"]

equalityExpression_Binary :: TypeDefinition
equalityExpression_Binary = def "EqualityExpression_Binary" $ T.record [
  "lhs">: java "EqualityExpression",
  "rhs">: java "RelationalExpression"]

--ExclusiveOrExpression:
--  AndExpression
--  ExclusiveOrExpression ^ AndExpression
exclusiveOrExpression :: TypeDefinition
exclusiveOrExpression = def "ExclusiveOrExpression" $ T.wrap $ nonemptyList $ java "AndExpression"

--Expression:
expression :: TypeDefinition
expression = def "Expression" $ T.union [
--  LambdaExpression
  "lambda">: java "LambdaExpression",
--  AssignmentExpression
  "assignment">: java "AssignmentExpression"]

--FieldAccess:
fieldAccess :: TypeDefinition
fieldAccess = def "FieldAccess" $ T.record [
  "qualifier">: java "FieldAccess_Qualifier",
  "identifier">: java "Identifier"]

fieldAccess_Qualifier :: TypeDefinition
fieldAccess_Qualifier = def "FieldAccess_Qualifier" $ T.union [
--  Primary . Identifier
  "primary">: java "Primary",
--  super . Identifier
  "super">: T.unit,
--  TypeName . super . Identifier
  "typed">: java "TypeName"]

--InclusiveOrExpression:
--  ExclusiveOrExpression
--  InclusiveOrExpression | ExclusiveOrExpression
inclusiveOrExpression :: TypeDefinition
inclusiveOrExpression = def "InclusiveOrExpression" $ T.wrap $ nonemptyList $ java "ExclusiveOrExpression"

--InstanceofExpression (extracted from RelationalExpression in SE 16; pattern arm SE 21):
--  RelationalExpression instanceof ReferenceType
--  RelationalExpression instanceof Pattern
instanceofExpression :: TypeDefinition
instanceofExpression = def "InstanceofExpression" $ T.record [
  "lhs">: java "RelationalExpression",
  "rhs">: java "InstanceofExpression_Rhs"]

instanceofExpression_Rhs :: TypeDefinition
instanceofExpression_Rhs = def "InstanceofExpression_Rhs" $ T.union [
  "referenceType">: java "ReferenceType",
--  Pattern (JEP 441, Java SE 21)
  "pattern">: java "Pattern"]

--LambdaBody:
lambdaBody_ :: TypeDefinition
lambdaBody_ = def "LambdaBody" $ T.union [
--  Expression
  "expression">: java "Expression",
--  Block
  "block">: java "Block"]

--LambdaExpression:
--  LambdaParameters -> LambdaBody
lambdaExpression :: TypeDefinition
lambdaExpression = def "LambdaExpression" $ T.record [
  "parameters">: java "LambdaParameters",
  "body">: java "LambdaBody"]

--LambdaParameterType:
lambdaParameterType :: TypeDefinition
lambdaParameterType = def "LambdaParameterType" $ T.union [
--  UnannType
  "type">: java "UnannType",
--  var
  "var">: T.unit]

--LambdaParameterList:
--  LambdaParameter {, LambdaParameter}
--  Identifier {, Identifier}
--LambdaParameter:
lambdaParameter_ :: TypeDefinition
lambdaParameter_ = def "LambdaParameter" $ T.union [
--  {VariableModifier} LambdaParameterType VariableDeclaratorId
  "normal">: java "LambdaParameter_Normal",
--  VariableArityParameter
  "variableArity">: java "VariableArityParameter"]

lambdaParameter_Normal :: TypeDefinition
lambdaParameter_Normal = def "LambdaParameter_Normal" $ T.record [
  "modifiers">: T.list $ java "VariableModifier",
  "type">: java "LambdaParameterType",
  "id">: java "VariableDeclaratorId"]

--LambdaParameters:
--  ( [LambdaParameterList] )
--  Identifier
lambdaParameters :: TypeDefinition
lambdaParameters = def "LambdaParameters" $ T.union [
  "tuple">: T.list $ java "LambdaParameters",
  "single">: java "Identifier"]

--LeftHandSide:
leftHandSide :: TypeDefinition
leftHandSide = def "LeftHandSide" $ T.union [
--  ExpressionName
  "expressionName">: java "ExpressionName",
--  FieldAccess
  "fieldAccess">: java "FieldAccess",
--  ArrayAccess
  "arrayAccess">: java "ArrayAccess"]

--MethodInvocation:
methodInvocation :: TypeDefinition
methodInvocation = def "MethodInvocation" $ T.record [
  "header">: java "MethodInvocation_Header",
  "arguments">: T.list $ java "Expression"]

methodInvocation_Complex :: TypeDefinition
methodInvocation_Complex = def "MethodInvocation_Complex" $ T.record [
  "variant">: java "MethodInvocation_Variant",
  "typeArguments">: T.list $ java "TypeArgument",
  "identifier">: java "Identifier"]

methodInvocation_Header :: TypeDefinition
methodInvocation_Header = def "MethodInvocation_Header" $ T.union [
--  MethodName ( [ArgumentList] )
  "simple">: java "MethodName",
  "complex">: java "MethodInvocation_Complex"]

methodInvocation_Variant :: TypeDefinition
methodInvocation_Variant = def "MethodInvocation_Variant" $ T.union [
--  TypeName . [TypeArguments] Identifier ( [ArgumentList] )
  "type">: java "TypeName",
--  ExpressionName . [TypeArguments] Identifier ( [ArgumentList] )
  "expression">: java "ExpressionName",
--  Primary . [TypeArguments] Identifier ( [ArgumentList] )
  "primary">: java "Primary",
--  super . [TypeArguments] Identifier ( [ArgumentList] )
  "super">: T.unit,
--  TypeName . super . [TypeArguments] Identifier ( [ArgumentList] )
  "typeSuper">: java "TypeName"]

--ArgumentList:
--  Expression {, Expression}

--MethodReference:
methodReference :: TypeDefinition
methodReference = def "MethodReference" $ T.union [
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
  "array">: java "MethodReference_Array"]

methodReference_Array :: TypeDefinition
methodReference_Array = def "MethodReference_Array" $ T.wrap $ java "ArrayType"

methodReference_Expression :: TypeDefinition
methodReference_Expression = def "MethodReference_Expression" $ T.record [
  "name">: java "ExpressionName",
  "typeArguments">: T.list $ java "TypeArgument",
  "identifier">: java "Identifier"]

methodReference_New :: TypeDefinition
methodReference_New = def "MethodReference_New" $ T.record [
  "classType">: java "ClassType",
  "typeArguments">: T.list $ java "TypeArgument"]

methodReference_Primary :: TypeDefinition
methodReference_Primary = def "MethodReference_Primary" $ T.record [
  "primary">: java "Primary",
  "typeArguments">: T.list $ java "TypeArgument",
  "identifier">: java "Identifier"]

methodReference_ReferenceType :: TypeDefinition
methodReference_ReferenceType = def "MethodReference_ReferenceType" $ T.record [
  "referenceType">: java "ReferenceType",
  "typeArguments">: T.list $ java "TypeArgument",
  "identifier">: java "Identifier"]

methodReference_Super :: TypeDefinition
methodReference_Super = def "MethodReference_Super" $ T.record [
  "typeArguments">: T.list $ java "TypeArgument",
  "identifier">: java "Identifier",
  "super">: T.boolean]

--MultiplicativeExpression:
multiplicativeExpression :: TypeDefinition
multiplicativeExpression = def "MultiplicativeExpression" $ T.union [
--  UnaryExpression
  "unary">: java "UnaryExpression",
--  MultiplicativeExpression * UnaryExpression
  "times">: java "MultiplicativeExpression_Binary",
--  MultiplicativeExpression / UnaryExpression
  "divide">: java "MultiplicativeExpression_Binary",
--  MultiplicativeExpression % UnaryExpression
  "mod">: java "MultiplicativeExpression_Binary"]

multiplicativeExpression_Binary :: TypeDefinition
multiplicativeExpression_Binary = def "MultiplicativeExpression_Binary" $ T.record [
  "lhs">: java "MultiplicativeExpression",
  "rhs">: java "UnaryExpression"]

numericTypeArray :: TypeDefinition
numericTypeArray = def "NumericTypeArray" $ T.union [
  "simple">: java "NumericType",
  "array">: java "NumericTypeArray"]

--PostDecrementExpression:
--  PostfixExpression --
postDecrementExpression :: TypeDefinition
postDecrementExpression = def "PostDecrementExpression" $ T.wrap $ java "PostfixExpression"

--PostIncrementExpression:
--  PostfixExpression ++
postIncrementExpression :: TypeDefinition
postIncrementExpression = def "PostIncrementExpression" $ T.wrap $ java "PostfixExpression"

--PostfixExpression:
postfixExpression :: TypeDefinition
postfixExpression = def "PostfixExpression" $ T.union [
--  Primary
  "primary">: java "Primary",
--  ExpressionName
  "name">: java "ExpressionName",
--  PostIncrementExpression
  "postIncrement">: java "PostIncrementExpression",
--  PostDecrementExpression
  "postDecrement">: java "PostDecrementExpression"]

--PreDecrementExpression:
--  -- UnaryExpression
preDecrementExpression :: TypeDefinition
preDecrementExpression = def "PreDecrementExpression" $ T.wrap $ java "UnaryExpression"

--PreIncrementExpression:
--  ++ UnaryExpression
preIncrementExpression :: TypeDefinition
preIncrementExpression = def "PreIncrementExpression" $ T.wrap $ java "UnaryExpression"

--Primary:
primary :: TypeDefinition
primary = def "Primary" $ T.union [
--  PrimaryNoNewArray
  "noNewArray">: java "PrimaryNoNewArrayExpression",
--  ArrayCreationExpression
  "arrayCreation">: java "ArrayCreationExpression"]

--PrimaryNoNewArrayExpression:
primaryNoNewArrayExpression :: TypeDefinition
primaryNoNewArrayExpression = def "PrimaryNoNewArrayExpression" $ T.union [
--  Literal
  "literal">: java "Literal",
--  ClassLiteral
  "classLiteral">: java "ClassLiteral",
--  this
  "this">: T.unit,
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
  "methodReference">: java "MethodReference"]

--RelationalExpression:
relationalExpression :: TypeDefinition
relationalExpression = def "RelationalExpression" $ T.union [
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
--  InstanceofExpression  (extracted from RelationalExpression in SE 16)
  "instanceofExpression">: java "InstanceofExpression"]

relationalExpression_GreaterThan :: TypeDefinition
relationalExpression_GreaterThan = def "RelationalExpression_GreaterThan" $ T.record [
  "lhs">: java "RelationalExpression",
  "rhs">: java "ShiftExpression"]

relationalExpression_GreaterThanEqual :: TypeDefinition
relationalExpression_GreaterThanEqual = def "RelationalExpression_GreaterThanEqual" $ T.record [
  "lhs">: java "RelationalExpression",
  "rhs">: java "ShiftExpression"]

relationalExpression_LessThan :: TypeDefinition
relationalExpression_LessThan = def "RelationalExpression_LessThan" $ T.record [
  "lhs">: java "RelationalExpression",
  "rhs">: java "ShiftExpression"]

relationalExpression_LessThanEqual :: TypeDefinition
relationalExpression_LessThanEqual = def "RelationalExpression_LessThanEqual" $ T.record [
  "lhs">: java "RelationalExpression",
  "rhs">: java "ShiftExpression"]

--ShiftExpression:
shiftExpression :: TypeDefinition
shiftExpression = def "ShiftExpression" $ T.union [
--  AdditiveExpression
  "unary">: java "AdditiveExpression",
--  ShiftExpression << AdditiveExpression
  "shiftLeft">: java "ShiftExpression_Binary",
--  ShiftExpression >> AdditiveExpression
  "shiftRight">: java "ShiftExpression_Binary",
--  ShiftExpression >>> AdditiveExpression
  "shiftRightZeroFill">: java "ShiftExpression_Binary"]

shiftExpression_Binary :: TypeDefinition
shiftExpression_Binary = def "ShiftExpression_Binary" $ T.record [
  "lhs">: java "ShiftExpression",
  "rhs">: java "AdditiveExpression"]

--SwitchExpression (JEP 361, Java SE 14):
--  switch ( Expression ) SwitchBlock
switchExpression :: TypeDefinition
switchExpression = def "SwitchExpression" $ T.record [
  "cond">: java "Expression",
  "block">: java "SwitchBlock"]

--TypeArgumentsOrDiamond:
typeArgumentsOrDiamond :: TypeDefinition
typeArgumentsOrDiamond = def "TypeArgumentsOrDiamond" $ T.union [
--  TypeArguments
  "arguments">: nonemptyList $ java "TypeArgument",
--  <>
  "diamond">: T.unit]

typeNameArray :: TypeDefinition
typeNameArray = def "TypeNameArray" $ T.union [
  "simple">: java "TypeName",
  "array">: java "TypeNameArray"]

--UnaryExpression:
unaryExpression :: TypeDefinition
unaryExpression = def "UnaryExpression" $ T.union [
--  PreIncrementExpression
  "preIncrement">: java "PreIncrementExpression",
--  PreDecrementExpression
  "preDecrement">: java "PreDecrementExpression",
--  + UnaryExpression
  "plus">: java "UnaryExpression",
--  - UnaryExpression
  "minus">: java "UnaryExpression",
--  UnaryExpressionNotPlusMinus
  "other">: java "UnaryExpressionNotPlusMinus"]

--UnaryExpressionNotPlusMinus:
unaryExpressionNotPlusMinus :: TypeDefinition
unaryExpressionNotPlusMinus = def "UnaryExpressionNotPlusMinus" $ T.union [
--  PostfixExpression
  "postfix">: java "PostfixExpression",
--  ~ UnaryExpression
  "tilde">: java "UnaryExpression",
--  ! UnaryExpression
  "not">: java "UnaryExpression",
--  CastExpression
  "cast">: java "CastExpression",
--  SwitchExpression (JEP 361, Java SE 14)
  "switchExpression">: java "SwitchExpression"]

--UnqualifiedClassInstanceCreationExpression:
--  new [TypeArguments] ClassOrInterfaceTypeToInstantiate ( [ArgumentList] ) [ClassBody]
unqualifiedClassInstanceCreationExpression :: TypeDefinition
unqualifiedClassInstanceCreationExpression = def "UnqualifiedClassInstanceCreationExpression" $ T.record [
  "typeArguments">: T.list $ java "TypeArgument",
  "classOrInterface">: java "ClassOrInterfaceTypeToInstantiate",
  "arguments">: T.list $ java "Expression",
  "body">: T.maybe $ java "ClassBody"]
