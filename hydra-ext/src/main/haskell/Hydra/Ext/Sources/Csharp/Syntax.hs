module Hydra.Ext.Sources.Csharp.Syntax where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.csharp.syntax"

def :: String -> Type -> Binding
def = datatype ns

csharp :: String -> Type
csharp = typeref ns


module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just ("A C# syntax module based on the ANTLR grammar dated 02/07/2024 and available at:\n"
      ++ "  https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar")
  where
    elements = lexicalElements ++ syntacticElements ++ unsafeElements

    lexicalElements = [
      identifier,
      keyword,
      literal,
      integerLiteral]

    syntacticElements = [
      namespaceName,
      typeName,
      namespaceOrTypeName,
      identifierNamespaceOrTypeName,
      qualifiedNamespaceOrTypeName,
      type_,
      referenceType,
      classType,
      interfaceType,
      arrayType,
      nonArrayType,
      rankSpecifier,
      delegateType,
      valueType,
      structOrEnumType,
      structType,
      simpleType,
      numericType,
      integralType,
      floatingPointType,
      tupleType,
      tupleTypeElement,
      enumType,
      typeArgumentList,
      typeParameter,
      unmanagedType,
      variableReference,
      pattern_,
      declarationPattern,
      designation,
      argumentList,
      argument,
      argumentValue,
      primaryExpression,
      primaryNoArrayCreationExpression,
      interpolatedStringExpression,
      interpolatedRegularStringExpression,
      regularInterpolation,
      interpolatedVerbatimStringExpression,
      verbatimInterpolation,
      simpleName,
      tupleExpression,
      tupleElement,
      deconstructionTuple,
      deconstructionElement,
      memberAccess,
      memberAccessHead,
      predefinedType,
      nullConditionalMemberAccess,
      dependentAccess,
      dependentAccessForMember,
      nullConditionalProjectionInitializer,
      invocationExpression,
      nullConditionalInvocationExpression,
      nullConditionalInvocationExpressionHead,
      elementAccess,
      nullConditionalElementAccess,
      baseAccess,
      baseAccessWithIdentifier,
      objectCreationExpression,
      objectOrCollectionInitializer,
      memberInitializer,
      initializerTarget,
      initializerValue,
      elementInitializer,
      expressionList,
      arrayCreationExpression,
      nonArrayTypeArrayCreationExpression,
      arrayTypeArrayCreationExpression,
      rankSpecifierArrayCreationExpression,
      delegateCreationExpression,
      memberDeclaratorList,
      memberDeclarator,
      assignmentMemberDeclarator,
      typeofExpression,
      unboundTypeName,
      unboundTypeNamePart,
      defaultValueExpression,
      stackallocExpression,
      namedEntity,
      namedEntityPart,
      namedEntityTarget,
      unaryExpression,
      castExpression,
      multiplicativeExpression,
      binaryMultiplicativeExpression,
      multiplicativeOperator,
      additiveExpression,
      binaryAdditiveExpression,
      additiveOperator,
      shiftExpression,
      binaryShiftExpression,
      shiftOperator,
      relationalExpression,
      binaryRelationalExpression,
      relationalOperator,
      isTypeExpression,
      isPatternExpression,
      asTypeExpression,
      equalityExpression,
      binaryEqualityExpression,
      equalityOperator,
      andExpression,
      binaryAndExpression,
      exclusiveOrExpression,
      binaryExclusiveOrExpression,
      inclusiveOrExpression,
      binaryInclusiveOrExpression,
      conditionalAndExpression,
      binaryConditionalAndExpression,
      conditionalOrExpression,
      binaryConditionalOrExpression,
      nullCoalescingExpression,
      binaryNullCoalescingExpression,
      declarationExpression,
      localVariableType,
      conditionalExpression,
      simpleConditionalExpression,
      refConditionalExpression,
      lambdaExpression,
      anonymousMethodExpression,
      anonymousFunctionSignature,
      explicitAnonymousFunctionParameter,
      anonymousFunctionParameterModifier,
      anonymousFunctionBody,
      queryExpression,
      fromClause,
      queryBody,
      queryBodyClause,
      letClause,
      joinClause,
      ordering,
      orderingDirection,
      selectOrGroupClause,
      groupClause,
      queryContinuation,
      assignment,
      assignmentOperator,
      expression,
      nonAssignmentExpression,
      constantExpression,
      booleanExpression,
      statement,
      embeddedStatement,
      block,
      labeledStatement,
      declarationStatement,
      localVariableDeclaration,
      implicitlyTypedLocalVariableDeclaration,
      refVarImplicitlyTypedLocalVariableDeclaration,
      implicitlyTypedLocalVariableDeclarator,
      explicitlyTypedLocalVariableDeclaration,
      explicitlyTypedLocalVariableDeclarator,
      localVariableInitializer,
      refLocalVariableDeclaration,
      refLocalVariableDeclarator,
      localConstantDeclaration,
      constantDeclarator,
      localFunctionDeclaration,
      standardLocalFunctionDeclaration,
      refLocalFunctionDeclaration,
      localFunctionHeader,
      localFunctionModifier,
      refLocalFunctionModifier,
      localFunctionBody,
      refLocalFunctionBody,
      statementExpression,
      selectionStatement,
      ifStatement,
      switchStatement,
      switchSection,
      switchLabel,
      switchBranch,
      iterationStatement,
      whileStatement,
      doStatement,
      forStatement,
      forInitializer,
      statementExpressionList,
      foreachStatement,
      jumpStatement,
      gotoStatement,
      returnStatement,
      tryStatement,
      catchClauses,
      specificCatchClause,
      exceptionSpecifier,
      lockStatement,
      usingStatement,
      resourceAcquisition,
      yieldStatement,
      compilationUnit,
      namespaceDeclaration,
      qualifiedIdentifier,
      namespaceBody,
      externAliasDirective,
      usingDirective,
      usingAliasDirective,
      namespaceMemberDeclaration,
      typeDeclaration,
      qualifiedAliasMember,
      classDeclaration,
      classModifier,
      typeParameterList,
      typeParameterPart,
      classBase,
      typeParameterConstraintsClause,
      typeParameterConstraints,
      primaryConstraint,
      secondaryConstraints,
      secondaryConstraint,
      classBody,
      classMemberDeclaration,
      constantDeclaration,
      constantModifier,
      fieldDeclaration,
      fieldModifier,
      variableDeclarators,
      variableDeclarator,
      methodDeclaration,
      standardMethodDeclaration,
      refReturnMethodDeclaration,
      methodModifiers,
      refKind,
      methodHeader,
      methodModifier,
      refMethodModifier,
      returnType,
      memberName,
      methodBody,
      refMethodBody,
      formalParameterList,
      fixedParameter,
      parameterModifier,
      parameterModeModifier,
      parameterArray,
      propertyDeclaration,
      standardPropertyDeclaration,
      refReturnPropertyDeclaration,
      propertyModifier,
      propertyBody,
      blockPropertyBody,
      refPropertyBody,
      accessorDeclarations,
      accessorDeclaration,
      accessorModifier,
      accessorBody,
      refGetAccessorDeclaration,
      refAccessorBody,
      eventDeclaration,
      standardEventDeclaration,
      accessorsEventDeclaration,
      eventModifier,
      eventAccessorDeclarations,
      addRemoveAccessorDeclaration,
      indexerDeclaration,
      standardIndexerDeclaration,
      refIndexerDeclaration,
      indexerModifier,
      indexerDeclarator,
      indexerBody,
      refIndexerBody,
      operatorDeclaration,
      operatorModifier,
      operatorDeclarator,
      unaryOperatorDeclarator,
      overloadableUnaryOperator,
      binaryOperatorDeclarator,
      overloadableBinaryOperator,
      conversionOperatorDeclarator,
      conversionKind,
      operatorBody,
      constructorDeclaration,
      constructorModifier,
      constructorDeclarator,
      constructorInitializer,
      constructorBody,
      staticConstructorDeclaration,
      staticConstructorModifiers,
      staticConstructorBody,
      finalizerDeclaration,
      finalizerBody,
      structDeclaration,
      structModifier,
      structMemberDeclaration,
      arrayInitializer,
      variableInitializer,
      interfaceDeclaration,
      interfaceModifier,
      variantTypeParameters,
      variantTypeParameter,
      varianceAnnotation,
      interfaceMemberDeclaration,
      interfaceMethodDeclaration,
      interfaceMethodHeader,
      interfacePropertyDeclaration,
      interfaceAccessors,
      interfaceEventDeclaration,
      interfaceIndexerDeclaration,
      enumDeclaration,
      enumBase,
      enumBody,
      enumModifier,
      enumMemberDeclaration,
      delegateDeclaration,
      delegateHeader,
      delegateModifier,
      globalAttributeSection,
      attributes,
      attributeSection,
      attributeTarget,
      attributeList,
      attribute,
      attributeName,
      attributeArguments,
      positionalArgumentList,
      positionalArgument,
      namedArgumentList,
      namedArgument,
      attributeArgumentExpression]

    unsafeElements = [
      pointerType,
      pointerMemberAccess,
      pointerElementAccess,
      fixedStatement,
      fixedPointerDeclarator,
      fixedSizeBufferDeclaration,
      fixedSizeBufferModifier,
      fixedSizeBufferDeclarator]


-- Lexical Elements

identifier :: Binding
identifier = def "Identifier" $ T.wrap T.string

keyword :: Binding
keyword = def "Keyword" $ T.wrap T.string

literal :: Binding
literal = def "Literal" $ T.union [
  "boolean">: T.boolean,
  "integer">: csharp "IntegerLiteral",
  "real">: T.bigfloat,
  "character">: T.string,
  "string">: T.string,
  "null">: T.unit]

integerLiteral :: Binding
integerLiteral = def "IntegerLiteral" $ T.union [
  "decimal">: T.string,
  "hexadecimal">: T.string,
  "binary">: T.bigint]


-- Syntactic Elements

namespaceName :: Binding
namespaceName = def "NamespaceName" $ T.wrap $
  csharp "NamespaceOrTypeName"

typeName :: Binding
typeName = def "TypeName" $ T.wrap $
  csharp "NamespaceOrTypeName"

namespaceOrTypeName :: Binding
namespaceOrTypeName = def "NamespaceOrTypeName" $ T.union [
  "identifier">: csharp "IdentifierNamespaceOrTypeName",
  "qualified">: csharp "QualifiedNamespaceOrTypeName",
  "alias">: csharp "QualifiedAliasMember"]

identifierNamespaceOrTypeName :: Binding
identifierNamespaceOrTypeName = def "IdentifierNamespaceOrTypeName" $ T.record [
  "identifier">: csharp "Identifier",
  "arguments">: T.maybe $ csharp "TypeArgumentList"]

qualifiedNamespaceOrTypeName :: Binding
qualifiedNamespaceOrTypeName = def "QualifiedNamespaceOrTypeName" $ T.record [
  "namespaceOrType">: csharp "NamespaceOrTypeName",
  "identifier">: csharp "Identifier",
  "arguments">: T.maybe $ csharp "TypeArgumentList"]

type_ :: Binding
type_ = def "Type" $ T.union [
  "reference">: csharp "ReferenceType",
  "value">: csharp "ValueType",
  "param">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

referenceType :: Binding
referenceType = def "ReferenceType" $ T.union [
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "array">: csharp "ArrayType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit]

classType :: Binding
classType = def "ClassType" $ T.union [
  "typeName">: csharp "TypeName",
  "object">: T.unit,
  "string">: T.unit]

interfaceType :: Binding
interfaceType = def "InterfaceType" $ T.wrap $
  csharp "TypeName"

arrayType :: Binding
arrayType = def "ArrayType" $ T.record [
  "type">: csharp "NonArrayType",
  "rank">: T.list $ csharp "RankSpecifier"]

nonArrayType :: Binding
nonArrayType = def "NonArrayType" $ T.union [
  "value">: csharp "ValueType",
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit,
  "parameter">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

rankSpecifier :: Binding
rankSpecifier = def "RankSpecifier" $ T.wrap T.int32

delegateType :: Binding
delegateType = def "DelegateType" $ T.wrap $
  csharp "TypeName"

valueType :: Binding
valueType = def "ValueType" $ T.union [
  "nonNullable">: csharp "StructOrEnumType",
  "nullable">: csharp "StructOrEnumType"]

structOrEnumType :: Binding
structOrEnumType = def "StructOrEnumType" $ T.union [
  "struct">: csharp "StructType",
  "enum">: csharp "EnumType"]

structType :: Binding
structType = def "StructType" $ T.union [
  "typeName">: csharp "TypeName",
  "simple">: csharp "SimpleType",
  "tuple">: csharp "TupleType"]

simpleType :: Binding
simpleType = def "SimpleType" $ T.union [
  "numeric">: csharp "NumericType",
  "bool">: T.unit]

numericType :: Binding
numericType = def "NumericType" $ T.union [
  "integral">: csharp "IntegralType",
  "floatingPoint">: csharp "FloatingPointType",
  "decimal">: T.unit]

integralType :: Binding
integralType = def "IntegralType" $ T.union [
  "sbyte">: T.unit,
  "byte">: T.unit,
  "short">: T.unit,
  "ushort">: T.unit,
  "int">: T.unit,
  "uint">: T.unit,
  "long">: T.unit,
  "ulong">: T.unit,
  "char">: T.unit]

floatingPointType :: Binding
floatingPointType = def "FloatingPointType" $ T.union [
  "float">: T.unit,
  "double">: T.unit]

tupleType :: Binding
tupleType = def "TupleType" $ T.wrap $
  nonemptyList $ csharp "TupleTypeElement"

tupleTypeElement :: Binding
tupleTypeElement = def "TupleTypeElement" $ T.record [
  "type">: csharp "Type",
  "identifier">: T.maybe $ csharp "Identifier"]

enumType :: Binding
enumType = def "EnumType" $ T.wrap $
  csharp "TypeName"

typeArgumentList :: Binding
typeArgumentList = def "TypeArgumentList" $ T.wrap $ T.list $ csharp "Type"

typeParameter :: Binding
typeParameter = def "TypeParameter" $ T.wrap $
  csharp "Identifier"

unmanagedType :: Binding
unmanagedType = def "UnmanagedType" $ T.union [
  "value">: csharp "ValueType",
  "pointer">: csharp "PointerType"]

variableReference :: Binding
variableReference = def "VariableReference" $ T.wrap $
  csharp "Expression"

pattern_ :: Binding
pattern_ = def "Pattern" $ T.union [
  "declaration">: csharp "DeclarationPattern",
  "constant">: csharp "Expression",
  "var">: csharp "Designation"]

declarationPattern :: Binding
declarationPattern = def "DeclarationPattern" $ T.record [
  "type">: csharp "Type",
  "designation">: csharp "Designation"]

designation :: Binding
designation = def "Designation" $ T.wrap $
  csharp "Identifier"

argumentList :: Binding
argumentList = def "ArgumentList" $ T.wrap $ nonemptyList $ csharp "Argument"

argument :: Binding
argument = def "Argument" $ T.record [
  "name">: T.maybe $ csharp "Identifier",
  "value">: csharp "ArgumentValue"]

argumentValue :: Binding
argumentValue = def "ArgumentValue" $ T.union [
  "expression">: csharp "Expression",
  "in">: csharp "VariableReference",
  "ref">: csharp "VariableReference",
  "out">: csharp "VariableReference"]

primaryExpression :: Binding
primaryExpression = def "PrimaryExpression" $ T.union [
  "noArray">: csharp "PrimaryNoArrayCreationExpression",
  "array">: csharp "ArrayCreationExpression"]

primaryNoArrayCreationExpression :: Binding
primaryNoArrayCreationExpression = def "PrimaryNoArrayCreationExpression" $ T.union [
  "literal">: csharp "Literal",
  "interpolatedString">: csharp "InterpolatedStringExpression",
  "simpleName">: csharp "SimpleName",
  "parenthesized">: csharp "Expression",
  "tuple">: csharp "TupleExpression",
  "memberAccess">: csharp "MemberAccess",
  "nullConditionalMemberAccess">: csharp "NullConditionalMemberAccess",
  "invocation">: csharp "InvocationExpression",
  "elementAccess">: csharp "ElementAccess",
  "nullConditionalElementAccess">: csharp "NullConditionalElementAccess",
  "thisAccess">: T.unit,
  "baseAccess">: csharp "BaseAccess",
  "postIncrement">: csharp "PrimaryExpression",
  "postDecrement">: csharp "PrimaryExpression",
  "objectCreation">: csharp "ObjectCreationExpression",
  "delegateCreation">: csharp "DelegateCreationExpression",
  "anonymousObjectCreation">: T.maybe $ csharp "MemberDeclaratorList",
  "typeof">: csharp "TypeofExpression",
  "sizeof">: csharp "UnmanagedType",
  "checked">: csharp "Expression",
  "unchecked">: csharp "Expression",
  "defaultValue">: csharp "DefaultValueExpression",
  "nameof">: csharp "NamedEntity",
  "anonymousMethod">: csharp "AnonymousMethodExpression",
  "pointerMemberAccess">: csharp "PointerMemberAccess",
  "pointerElementAccess">: csharp "PointerElementAccess",
  "stackalloc">: csharp "StackallocExpression"]

interpolatedStringExpression :: Binding
interpolatedStringExpression = def "InterpolatedStringExpression" $ T.union [
  "regular">: csharp "InterpolatedRegularStringExpression",
  "verbatim">: csharp "InterpolatedVerbatimStringExpression"]

interpolatedRegularStringExpression :: Binding
interpolatedRegularStringExpression = def "InterpolatedRegularStringExpression" $ T.wrap T.string

regularInterpolation :: Binding
regularInterpolation = def "RegularInterpolation" $ T.record [
  "expression">: csharp "Expression",
  "width">: T.maybe $ csharp "Expression",
  "format">: T.maybe T.string]

interpolatedVerbatimStringExpression :: Binding
interpolatedVerbatimStringExpression = def "InterpolatedVerbatimStringExpression" $ T.wrap T.string

verbatimInterpolation :: Binding
verbatimInterpolation = def "VerbatimInterpolation" $ T.record [
  "expression">: csharp "Expression",
  "width">: T.maybe $ csharp "ConstantExpression",
  "format">: T.maybe T.string]

simpleName :: Binding
simpleName = def "SimpleName" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

tupleExpression :: Binding
tupleExpression = def "TupleExpression" $ T.union [
  "elements">: nonemptyList $ csharp "TupleElement",
  "deconstruction">: csharp "DeconstructionTuple"]

tupleElement :: Binding
tupleElement = def "TupleElement" $ T.record [
  "name">: T.maybe $ csharp "Identifier",
  "expression">: csharp "Expression"]

deconstructionTuple :: Binding
deconstructionTuple = def "DeconstructionTuple" $ T.wrap $
  nonemptyList $ csharp "DeconstructionElement"

deconstructionElement :: Binding
deconstructionElement = def "DeconstructionElement" $ T.union [
  "tuple">: csharp "DeconstructionTuple",
  "identifier">: csharp "Identifier"]

memberAccess :: Binding
memberAccess = def "MemberAccess" $ T.record [
  "head">: csharp "MemberAccessHead",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

memberAccessHead :: Binding
memberAccessHead = def "MemberAccessHead" $ T.union [
  "primary">: csharp "PrimaryExpression",
  "predefined">: csharp "PredefinedType",
  "qualifiedAlias">: csharp "QualifiedAliasMember"]

predefinedType :: Binding
predefinedType = def "PredefinedType" $ T.enum [
  "bool", "byte", "char", "decimal", "double", "float", "int", "long", "object", "sbyte", "short", "string",
  "uint", "ulong", "ushort"]

nullConditionalMemberAccess :: Binding
nullConditionalMemberAccess = def "NullConditionalMemberAccess" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

dependentAccess :: Binding
dependentAccess = def "DependentAccess" $ T.union [
  "memberAccess">: csharp "DependentAccessForMember",
  "elementAccess">: csharp "ArgumentList",
  "invocation">: T.maybe $ csharp "ArgumentList"]

dependentAccessForMember :: Binding
dependentAccessForMember = def "DependentAccessForMember" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

nullConditionalProjectionInitializer :: Binding
nullConditionalProjectionInitializer = def "NullConditionalProjectionInitializer" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

invocationExpression :: Binding
invocationExpression = def "InvocationExpression" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "arguments">: T.maybe $ csharp "ArgumentList"]

nullConditionalInvocationExpression :: Binding
nullConditionalInvocationExpression = def "NullConditionalInvocationExpression" $ T.record [
  "head">: csharp "NullConditionalInvocationExpressionHead",
  "arguments">: T.maybe $ csharp "ArgumentList"]

nullConditionalInvocationExpressionHead :: Binding
nullConditionalInvocationExpressionHead = def "NullConditionalInvocationExpressionHead" $ T.union [
  "member">: csharp "NullConditionalMemberAccess",
  "element">: csharp "NullConditionalElementAccess"]

elementAccess :: Binding
elementAccess = def "ElementAccess" $ T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList"]

nullConditionalElementAccess :: Binding
nullConditionalElementAccess = def "NullConditionalElementAccess" $ T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

baseAccess :: Binding
baseAccess = def "BaseAccess" $ T.union [
  "identifier">: csharp "BaseAccessWithIdentifier",
  "arguments">: csharp "ArgumentList"]

baseAccessWithIdentifier :: Binding
baseAccessWithIdentifier = def "BaseAccessWithIdentifier" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

objectCreationExpression :: Binding
objectCreationExpression = def "ObjectCreationExpression" $ T.record [
  "type">: csharp "Type",
  "arguments">: T.maybe $ csharp "ArgumentList",
  "initializer">: T.maybe $ csharp "ObjectOrCollectionInitializer"]

objectOrCollectionInitializer :: Binding
objectOrCollectionInitializer = def "ObjectOrCollectionInitializer" $ T.union [
  "object">: T.list $ csharp "MemberInitializer",
  "collection">: T.list $ csharp "ElementInitializer"]

memberInitializer :: Binding
memberInitializer = def "MemberInitializer" $ T.record [
  "target">: csharp "InitializerTarget",
  "value">: csharp "InitializerValue"]

initializerTarget :: Binding
initializerTarget = def "InitializerTarget" $ T.union [
  "identifier">: csharp "Identifier",
  "arguments">: csharp "ArgumentList"]

initializerValue :: Binding
initializerValue = def "InitializerValue" $ T.union [
  "expression">: csharp "Expression",
  "objectOrCollection">: csharp "ObjectOrCollectionInitializer"]

elementInitializer :: Binding
elementInitializer = def "ElementInitializer" $ T.union [
  "single">: csharp "NonAssignmentExpression",
  "list">: csharp "ExpressionList"]

expressionList :: Binding
expressionList = def "ExpressionList" $ T.wrap $ nonemptyList $ csharp "Expression"

arrayCreationExpression :: Binding
arrayCreationExpression = def "ArrayCreationExpression" $ T.union [
  "nonArrayType">: csharp "NonArrayTypeArrayCreationExpression",
  "arrayType">: csharp "ArrayTypeArrayCreationExpression",
  "rankSpecifier">: csharp "RankSpecifierArrayCreationExpression"]

nonArrayTypeArrayCreationExpression :: Binding
nonArrayTypeArrayCreationExpression = def "NonArrayTypeArrayCreationExpression" $ T.record [
  "type">: csharp "NonArrayType",
  "expressions">: csharp "ExpressionList",
  "rankSpecifiers">: T.list $ csharp "RankSpecifier",
  "initializer">: T.maybe $ csharp "ArrayInitializer"]

arrayTypeArrayCreationExpression :: Binding
arrayTypeArrayCreationExpression = def "ArrayTypeArrayCreationExpression" $ T.record [
  "type">: csharp "ArrayType",
  "initializer">: csharp "ArrayInitializer"]

rankSpecifierArrayCreationExpression :: Binding
rankSpecifierArrayCreationExpression = def "RankSpecifierArrayCreationExpression" $ T.record [
  "rankSpecifier">: csharp "RankSpecifier",
  "initializer">: csharp "ArrayInitializer"]

delegateCreationExpression :: Binding
delegateCreationExpression = def "DelegateCreationExpression" $ T.record [
  "type">: csharp "DelegateType",
  "expression">: csharp "Expression"]

memberDeclaratorList :: Binding
memberDeclaratorList = def "MemberDeclaratorList" $ T.wrap $ nonemptyList $ csharp "MemberDeclarator"

memberDeclarator :: Binding
memberDeclarator = def "MemberDeclarator" $ T.union [
  "name">: csharp "SimpleName",
  "memberAccess">: csharp "MemberAccess",
  "nullConditionalProjectionInitializer">: csharp "NullConditionalProjectionInitializer",
  "baseAccess">: csharp "BaseAccess",
  "assignment">: csharp "AssignmentMemberDeclarator"]

assignmentMemberDeclarator :: Binding
assignmentMemberDeclarator = def "AssignmentMemberDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

typeofExpression :: Binding
typeofExpression = def "TypeofExpression" $ T.union [
  "type">: csharp "Type",
  "unboundTypeName">: csharp "UnboundTypeName",
  "void">: T.unit]

unboundTypeName :: Binding
unboundTypeName = def "UnboundTypeName" $ T.wrap $
  nonemptyList $ csharp "UnboundTypeNamePart"

unboundTypeNamePart :: Binding
unboundTypeNamePart = def "UnboundTypeNamePart" $ T.record [
  "identifier">: csharp "Identifier",
  "aliased">: T.boolean,
  "dimension">: T.maybe T.int32]

defaultValueExpression :: Binding
defaultValueExpression = def "DefaultValueExpression" $ T.union [
  "explicitlyTyped">: csharp "Type",
  "defaultLiteral">: T.unit]

stackallocExpression :: Binding
stackallocExpression = def "StackallocExpression" $ T.record [
  "type">: T.maybe $ csharp "UnmanagedType",
  "expression">: T.maybe $ csharp "ConstantExpression",
  "initializer">: T.list $ csharp "Expression"]

namedEntity :: Binding
namedEntity = def "NamedEntity" $ T.record [
  "target">: csharp "NamedEntityTarget",
  "parts">: T.list $ csharp "NamedEntityPart"]

namedEntityPart :: Binding
namedEntityPart = def "NamedEntityPart" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

namedEntityTarget :: Binding
namedEntityTarget = def "NamedEntityTarget" $ T.union [
  "name">: csharp "SimpleName",
  "this">: T.unit,
  "base">: T.unit,
  "predefinedType">: csharp "PredefinedType",
  "qualifiedAliasMember">: csharp "QualifiedAliasMember"]

unaryExpression :: Binding
unaryExpression = def "UnaryExpression" $ T.union [
  "primary">: csharp "PrimaryExpression",
  "plus">: csharp "UnaryExpression",
  "minus">: csharp "UnaryExpression",
  "not">: csharp "UnaryExpression",
  "bitwiseComplement">: csharp "UnaryExpression",
  "preIncrement">: csharp "UnaryExpression",
  "preDecrement">: csharp "UnaryExpression",
  "cast">: csharp "CastExpression",
  "await">: csharp "UnaryExpression",
  "pointerIndirection">: csharp "UnaryExpression",
  "addressOf">: csharp "UnaryExpression"]

castExpression :: Binding
castExpression = def "CastExpression" $ T.record [
  "type">: csharp "Type",
  "expression">: csharp "UnaryExpression"]

multiplicativeExpression :: Binding
multiplicativeExpression = def "MultiplicativeExpression" $ T.union [
  "simple">: csharp "UnaryExpression",
  "binary">: csharp "BinaryMultiplicativeExpression"]

binaryMultiplicativeExpression :: Binding
binaryMultiplicativeExpression = def "BinaryMultiplicativeExpression" $ T.record [
  "left">: csharp "MultiplicativeExpression",
  "operator">: csharp "MultiplicativeOperator",
  "right">: csharp "UnaryExpression"]

multiplicativeOperator :: Binding
multiplicativeOperator = def "MultiplicativeOperator" $ T.enum [
  "times",
  "divide",
  "modulo"]

additiveExpression :: Binding
additiveExpression = def "AdditiveExpression" $ T.union [
  "simple">: csharp "MultiplicativeExpression",
  "binary">: csharp "BinaryAdditiveExpression"]

binaryAdditiveExpression :: Binding
binaryAdditiveExpression = def "BinaryAdditiveExpression" $ T.record [
  "left">: csharp "AdditiveExpression",
  "operator">: csharp "AdditiveOperator",
  "right">: csharp "MultiplicativeExpression"]

additiveOperator :: Binding
additiveOperator = def "AdditiveOperator" $ T.enum [
  "plus",
  "minus"]

shiftExpression :: Binding
shiftExpression = def "ShiftExpression" $ T.union [
  "simple">: csharp "AdditiveExpression",
  "binary">: csharp "BinaryShiftExpression"]

binaryShiftExpression :: Binding
binaryShiftExpression = def "BinaryShiftExpression" $ T.record [
  "left">: csharp "ShiftExpression",
  "operator">: csharp "ShiftOperator",
  "right">: csharp "AdditiveExpression"]

shiftOperator :: Binding
shiftOperator = def "ShiftOperator" $ T.enum [
  "left",
  "right"]

relationalExpression :: Binding
relationalExpression = def "RelationalExpression" $ T.union [
  "simple">: csharp "ShiftExpression",
  "binary">: csharp "BinaryRelationalExpression",
  "isType">: csharp "IsTypeExpression",
  "isPattern">: csharp "IsPatternExpression",
  "asType">: csharp "AsTypeExpression"]

binaryRelationalExpression :: Binding
binaryRelationalExpression = def "BinaryRelationalExpression" $ T.record [
  "left">: csharp "RelationalExpression",
  "operator">: csharp "RelationalOperator",
  "right">: csharp "ShiftExpression"]

relationalOperator :: Binding
relationalOperator = def "RelationalOperator" $ T.enum [
  "lessThan", "greaterThan", "lessThanOrEqual", "greaterThanOrEqual"]

isTypeExpression :: Binding
isTypeExpression = def "IsTypeExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

isPatternExpression :: Binding
isPatternExpression = def "IsPatternExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "pattern">: csharp "Pattern"]

asTypeExpression :: Binding
asTypeExpression = def "AsTypeExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

equalityExpression :: Binding
equalityExpression = def "EqualityExpression" $ T.union [
  "simple">: csharp "RelationalExpression",
  "binary">: csharp "BinaryEqualityExpression"]

binaryEqualityExpression :: Binding
binaryEqualityExpression = def "BinaryEqualityExpression" $ T.record [
  "left">: csharp "EqualityExpression",
  "operator">: csharp "EqualityOperator",
  "right">: csharp "RelationalExpression"]

equalityOperator :: Binding
equalityOperator = def "EqualityOperator" $ T.enum [
  "equal",
  "notEqual"]

andExpression :: Binding
andExpression = def "AndExpression" $ T.union [
  "simple">: csharp "EqualityExpression",
  "binary">: csharp "BinaryAndExpression"]

binaryAndExpression :: Binding
binaryAndExpression = def "BinaryAndExpression" $ T.record [
  "left">: csharp "AndExpression",
  "right">: csharp "EqualityExpression"]

exclusiveOrExpression :: Binding
exclusiveOrExpression = def "ExclusiveOrExpression" $ T.union [
  "simple">: csharp "AndExpression",
  "binary">: csharp "BinaryExclusiveOrExpression"]

binaryExclusiveOrExpression :: Binding
binaryExclusiveOrExpression = def "BinaryExclusiveOrExpression" $ T.record [
  "left">: csharp "ExclusiveOrExpression",
  "right">: csharp "AndExpression"]

inclusiveOrExpression :: Binding
inclusiveOrExpression = def "InclusiveOrExpression" $ T.union [
  "simple">: csharp "ExclusiveOrExpression",
  "binary">: csharp "BinaryInclusiveOrExpression"]

binaryInclusiveOrExpression :: Binding
binaryInclusiveOrExpression = def "BinaryInclusiveOrExpression" $ T.record [
  "left">: csharp "InclusiveOrExpression",
  "right">: csharp "ExclusiveOrExpression"]

conditionalAndExpression :: Binding
conditionalAndExpression = def "ConditionalAndExpression" $ T.union [
  "simple">: csharp "InclusiveOrExpression",
  "binary">: csharp "BinaryConditionalAndExpression"]

binaryConditionalAndExpression :: Binding
binaryConditionalAndExpression = def "BinaryConditionalAndExpression" $ T.record [
  "left">: csharp "ConditionalAndExpression",
  "right">: csharp "InclusiveOrExpression"]

conditionalOrExpression :: Binding
conditionalOrExpression = def "ConditionalOrExpression" $ T.union [
  "simple">: csharp "ConditionalAndExpression",
  "binary">: csharp "BinaryConditionalOrExpression"]

binaryConditionalOrExpression :: Binding
binaryConditionalOrExpression = def "BinaryConditionalOrExpression" $ T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "ConditionalAndExpression"]

nullCoalescingExpression :: Binding
nullCoalescingExpression = def "NullCoalescingExpression" $ T.union [
  "simple">: csharp "ConditionalOrExpression",
  "binary">: csharp "BinaryNullCoalescingExpression",
  "throw">: csharp "NullCoalescingExpression"]

binaryNullCoalescingExpression :: Binding
binaryNullCoalescingExpression = def "BinaryNullCoalescingExpression" $ T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "NullCoalescingExpression"]

declarationExpression :: Binding
declarationExpression = def "DeclarationExpression" $ T.record [
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier"]

localVariableType :: Binding
localVariableType = def "LocalVariableType" $ T.union [
  "type">: csharp "Type",
  "var">: T.unit]

conditionalExpression :: Binding
conditionalExpression = def "ConditionalExpression" $ T.union [
  "simple">: csharp "NullCoalescingExpression",
  "simpleConditional">: csharp "SimpleConditionalExpression",
  "refConditional">: csharp "RefConditionalExpression"]

simpleConditionalExpression :: Binding
simpleConditionalExpression = def "SimpleConditionalExpression" $ T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "Expression",
  "false">: csharp "Expression"]

refConditionalExpression :: Binding
refConditionalExpression = def "RefConditionalExpression" $ T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "VariableReference",
  "false">: csharp "VariableReference"]

lambdaExpression :: Binding
lambdaExpression = def "LambdaExpression" $ T.record [
  "async">: T.boolean,
  "signature">: csharp "AnonymousFunctionSignature",
  "body">: csharp "AnonymousFunctionBody"]

anonymousMethodExpression :: Binding
anonymousMethodExpression = def "AnonymousMethodExpression" $ T.record [
  "async">: T.boolean,
  "signature">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "body">: csharp "Block"]

anonymousFunctionSignature :: Binding
anonymousFunctionSignature = def "AnonymousFunctionSignature" $ T.union [
  "explicit">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "implicit">: T.list $ csharp "Identifier"]

explicitAnonymousFunctionParameter :: Binding
explicitAnonymousFunctionParameter = def "ExplicitAnonymousFunctionParameter" $ T.record [
  "modifier">: T.maybe $ csharp "AnonymousFunctionParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier"]

anonymousFunctionParameterModifier :: Binding
anonymousFunctionParameterModifier = def "AnonymousFunctionParameterModifier" $ T.enum [
  "ref",
  "out",
  "in"]

anonymousFunctionBody :: Binding
anonymousFunctionBody = def "AnonymousFunctionBody" $ T.union [
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "ref">: csharp "VariableReference",
  "block">: csharp "Block"]

queryExpression :: Binding
queryExpression = def "QueryExpression" $ T.record [
  "from">: csharp "FromClause",
  "body">: csharp "QueryBody"]

fromClause :: Binding
fromClause = def "FromClause" $ T.record [
  "type">: T.maybe $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression"]

queryBody :: Binding
queryBody = def "QueryBody" $ T.record [
  "clauses">: T.list $ csharp "QueryBodyClause",
  "selectOrGroup">: csharp "SelectOrGroupClause",
  "continuation">: T.maybe $ csharp "QueryContinuation"]

queryBodyClause :: Binding
queryBodyClause = def "QueryBodyClause" $ T.union [
  "from">: csharp "FromClause",
  "let">: csharp "LetClause",
  "where">: csharp "BooleanExpression",
  "join">: csharp "JoinClause",
  "orderby">: nonemptyList $ csharp "Ordering"]

letClause :: Binding
letClause = def "LetClause" $ T.record [
  "left">: csharp "Identifier",
  "right">: csharp "Expression"]

joinClause :: Binding
joinClause = def "JoinClause" $ T.record [
  "type">: T.maybe $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression",
  "on">: csharp "Expression",
  "equals">: csharp "Expression",
  "into">: T.maybe $ csharp "Identifier"]

ordering :: Binding
ordering = def "Ordering" $ T.record [
  "expression">: csharp "Expression",
  "direction">: T.maybe $ csharp "OrderingDirection"]

orderingDirection :: Binding
orderingDirection = def "OrderingDirection" $ T.enum [
  "ascending",
  "descending"]

selectOrGroupClause :: Binding
selectOrGroupClause = def "SelectOrGroupClause" $ T.union [
  "select">: csharp "Expression",
  "group">: csharp "GroupClause"]

groupClause :: Binding
groupClause = def "GroupClause" $ T.record [
  "grouped">: csharp "Expression",
  "by">: csharp "Expression"]

queryContinuation :: Binding
queryContinuation = def "QueryContinuation" $ T.record [
  "into">: csharp "Identifier",
  "body">: csharp "QueryBody"]

assignment :: Binding
assignment = def "Assignment" $ T.record [
  "left">: csharp "UnaryExpression",
  "operator">: csharp "AssignmentOperator",
  "right">: csharp "Expression"]

assignmentOperator :: Binding
assignmentOperator = def "AssignmentOperator" $ T.union [
  "simple">: T.boolean,
  "plusEquals">: T.unit,
  "minusEquals">: T.unit,
  "timesEquals">: T.unit,
  "divideEquals">: T.unit,
  "modEquals">: T.unit,
  "andEquals">: T.unit,
  "orEquals">: T.unit,
  "xorEquals">: T.unit,
  "leftShiftEquals">: T.unit,
  "rightShiftEquals">: T.unit]

expression :: Binding
expression = def "Expression" $ T.union [
  "nonAssignment">: csharp "NonAssignmentExpression",
  "assignment">: csharp "Assignment"]

nonAssignmentExpression :: Binding
nonAssignmentExpression = def "NonAssignmentExpression" $ T.union [
  "declaration">: csharp "DeclarationExpression",
  "conditional">: csharp "ConditionalExpression",
  "lambda">: csharp "LambdaExpression",
  "query">: csharp "QueryExpression"]

constantExpression :: Binding
constantExpression = def "ConstantExpression" $ T.wrap $
  csharp "Expression"

booleanExpression :: Binding
booleanExpression = def "BooleanExpression" $ T.wrap $
  csharp "Expression"

statement :: Binding
statement = def "Statement" $ T.union [
  "labeled">: csharp "LabeledStatement",
  "declaration">: csharp "DeclarationStatement",
  "embedded">: csharp "EmbeddedStatement"]

embeddedStatement :: Binding
embeddedStatement = def "EmbeddedStatement" $ T.union [
  "block">: csharp "Block",
  "empty">: T.unit,
  "expression">: csharp "StatementExpression",
  "selection">: csharp "SelectionStatement",
  "iteration">: csharp "IterationStatement",
  "jump">: csharp "JumpStatement",
  "try">: csharp "TryStatement",
  "checked">: csharp "Block",
  "unchecked">: csharp "Block",
  "lock">: csharp "LockStatement",
  "using">: csharp "UsingStatement",
  "yield">: csharp "YieldStatement",
  "unsafe">: csharp "Block",
  "fixed">: csharp "FixedStatement"]

block :: Binding
block = def "Block" $ T.wrap $
  T.list $ csharp "Statement"

labeledStatement :: Binding
labeledStatement = def "LabeledStatement" $ T.record [
  "label">: csharp "Identifier",
  "statement">: csharp "Statement"]

declarationStatement :: Binding
declarationStatement = def "DeclarationStatement" $ T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "constant">: csharp "LocalConstantDeclaration",
  "function">: csharp "LocalFunctionDeclaration"]

localVariableDeclaration :: Binding
localVariableDeclaration = def "LocalVariableDeclaration" $ T.union [
  "implicitlyTyped">: csharp "ImplicitlyTypedLocalVariableDeclaration",
  "explicitlyTyped">: csharp "ExplicitlyTypedLocalVariableDeclaration",
  "ref">: csharp "RefLocalVariableDeclaration"]

implicitlyTypedLocalVariableDeclaration :: Binding
implicitlyTypedLocalVariableDeclaration = def "ImplicitlyTypedLocalVariableDeclaration" $ T.union [
  "var">: csharp "ImplicitlyTypedLocalVariableDeclarator",
  "refVar">: csharp "RefVarImplicitlyTypedLocalVariableDeclaration"]

refVarImplicitlyTypedLocalVariableDeclaration :: Binding
refVarImplicitlyTypedLocalVariableDeclaration = def "RefVarImplicitlyTypedLocalVariableDeclaration" $ T.record [
  "refKind">: csharp "RefKind",
  "declarator">: csharp "RefLocalVariableDeclarator"]

implicitlyTypedLocalVariableDeclarator :: Binding
implicitlyTypedLocalVariableDeclarator = def "ImplicitlyTypedLocalVariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

explicitlyTypedLocalVariableDeclaration :: Binding
explicitlyTypedLocalVariableDeclaration = def "ExplicitlyTypedLocalVariableDeclaration" $ T.record [
  "type">: csharp "Type",
  "declarators">: T.list $ csharp "ExplicitlyTypedLocalVariableDeclarator"]

explicitlyTypedLocalVariableDeclarator :: Binding
explicitlyTypedLocalVariableDeclarator = def "ExplicitlyTypedLocalVariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.maybe $ csharp "LocalVariableInitializer"]

localVariableInitializer :: Binding
localVariableInitializer = def "LocalVariableInitializer" $ T.union [
  "expression">: csharp "Expression",
  "initializer">: csharp "ArrayInitializer"]

refLocalVariableDeclaration :: Binding
refLocalVariableDeclaration = def "RefLocalVariableDeclaration" $ T.record [
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "RefLocalVariableDeclarator"]

refLocalVariableDeclarator :: Binding
refLocalVariableDeclarator = def "RefLocalVariableDeclarator" $ T.record [
  "left">: csharp "Identifier",
  "right">: csharp "VariableReference"]

localConstantDeclaration :: Binding
localConstantDeclaration = def "LocalConstantDeclaration" $ T.record [
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

constantDeclarator :: Binding
constantDeclarator = def "ConstantDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "ConstantExpression"]

localFunctionDeclaration :: Binding
localFunctionDeclaration = def "LocalFunctionDeclaration" $ T.union [
  "standard">: csharp "StandardLocalFunctionDeclaration",
  "ref">: csharp "RefLocalFunctionDeclaration"]

standardLocalFunctionDeclaration :: Binding
standardLocalFunctionDeclaration = def "StandardLocalFunctionDeclaration" $ T.record [
  "modifiers">: T.list $ csharp "LocalFunctionModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "LocalFunctionBody"]

refLocalFunctionDeclaration :: Binding
refLocalFunctionDeclaration = def "RefLocalFunctionDeclaration" $ T.record [
  "modifiers">: T.list $ csharp "RefLocalFunctionModifier",
  "refKind">: csharp "RefKind",
  "returnType">: csharp "Type",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "RefLocalFunctionBody"]

localFunctionHeader :: Binding
localFunctionHeader = def "LocalFunctionHeader" $ T.record [
  "identifier">: csharp "Identifier",
  "typeParameters">: T.maybe $ csharp "TypeParameterList",
  "parameters">: csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

localFunctionModifier :: Binding
localFunctionModifier = def "LocalFunctionModifier" $ T.union [
  "ref">: csharp "RefLocalFunctionModifier",
  "async">: T.unit]

refLocalFunctionModifier :: Binding
refLocalFunctionModifier = def "RefLocalFunctionModifier" $ T.enum [
  "static",
  "unsafe"]

localFunctionBody :: Binding
localFunctionBody = def "LocalFunctionBody" $ T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression"]

refLocalFunctionBody :: Binding
refLocalFunctionBody = def "RefLocalFunctionBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference"]

statementExpression :: Binding
statementExpression = def "StatementExpression" $ T.union [
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "invocation">: csharp "InvocationExpression",
  "objectCreation">: csharp "ObjectCreationExpression",
  "assignment">: csharp "Assignment",
  "postIncrement">: csharp "PrimaryExpression",
  "postDecrement">: csharp "PrimaryExpression",
  "preIncrement">: csharp "UnaryExpression",
  "preDecrement">: csharp "UnaryExpression",
  "await">: csharp "UnaryExpression"]

selectionStatement :: Binding
selectionStatement = def "SelectionStatement" $ T.union [
  "if">: csharp "IfStatement",
  "switch">: csharp "SwitchStatement"]

ifStatement :: Binding
ifStatement = def "IfStatement" $ T.record [
  "condition">: csharp "BooleanExpression",
  "ifBranch">: csharp "EmbeddedStatement",
  "elseBranch">: csharp "EmbeddedStatement"]

switchStatement :: Binding
switchStatement = def "SwitchStatement" $ T.record [
  "expression">: csharp "Expression",
  "branches">: T.list $ csharp "SwitchSection"]

switchSection :: Binding
switchSection = def "SwitchSection" $ T.record [
  "labels">: nonemptyList $ csharp "SwitchLabel",
  "statements">: T.list $ csharp "Statement"]

switchLabel :: Binding
switchLabel = def "SwitchLabel" $ T.union [
  "branch">: csharp "SwitchBranch",
  "default">: T.unit]

switchBranch :: Binding
switchBranch = def "SwitchBranch" $ T.record [
  "pattern">: csharp "Pattern",
  "guard">: T.maybe $ csharp "Expression"]

iterationStatement :: Binding
iterationStatement = def "IterationStatement" $ T.union [
  "while">: csharp "WhileStatement",
  "do">: csharp "DoStatement",
  "for">: csharp "ForStatement",
  "foreach">: csharp "ForeachStatement"]

whileStatement :: Binding
whileStatement = def "WhileStatement" $ T.record [
  "condition">: csharp "BooleanExpression",
  "body">: csharp "EmbeddedStatement"]

doStatement :: Binding
doStatement = def "DoStatement" $ T.record [
  "body">: csharp "EmbeddedStatement",
  "while">: csharp "BooleanExpression"]

forStatement :: Binding
forStatement = def "ForStatement" $ T.record [
  "initializer">: T.maybe $ csharp "ForInitializer",
  "condition">: T.maybe $ csharp "BooleanExpression",
  "iterator">: T.maybe $ csharp "StatementExpressionList",
  "body">: csharp "EmbeddedStatement"]

forInitializer :: Binding
forInitializer = def "ForInitializer" $ T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "statements">: csharp "StatementExpressionList"]

statementExpressionList :: Binding
statementExpressionList = def "StatementExpressionList" $ T.wrap $ nonemptyList $ csharp "StatementExpression"

foreachStatement :: Binding
foreachStatement = def "ForeachStatement" $ T.record [
  "kind">: T.maybe $ csharp "RefKind",
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

jumpStatement :: Binding
jumpStatement = def "JumpStatement" $ T.union [
  "break">: T.unit,
  "continue">: T.unit,
  "goto">: csharp "GotoStatement",
  "return">: csharp "ReturnStatement",
  "throw">: T.maybe $ csharp "Expression"]

gotoStatement :: Binding
gotoStatement = def "GotoStatement" $ T.union [
  "identifier">: csharp "Identifier",
  "case">: csharp "ConstantExpression",
  "default">: T.unit]

returnStatement :: Binding
returnStatement = def "ReturnStatement" $ T.union [
  "simple">: T.unit,
  "value">: csharp "Expression",
  "ref">: csharp "VariableReference"]

tryStatement :: Binding
tryStatement = def "TryStatement" $ T.record [
  "body">: csharp "Block",
  "catches">: csharp "CatchClauses",
  "finally">: T.maybe $ csharp "Block"]

catchClauses :: Binding
catchClauses = def "CatchClauses" $ T.union [
  "specific">: T.list $ csharp "SpecificCatchClause",
  "general">: csharp "Block"]

specificCatchClause :: Binding
specificCatchClause = def "SpecificCatchClause" $ T.record [
  "specifier">: T.maybe $ csharp "ExceptionSpecifier",
  "filter">: T.maybe $ csharp "BooleanExpression",
  "body">: csharp "Block"]

exceptionSpecifier :: Binding
exceptionSpecifier = def "ExceptionSpecifier" $ T.record [
  "type">: csharp "Type",
  "identifier">: T.maybe $ csharp "Identifier"]

lockStatement :: Binding
lockStatement = def "LockStatement" $ T.record [
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

usingStatement :: Binding
usingStatement = def "UsingStatement" $ T.record [
  "acquisition">: csharp "ResourceAcquisition",
  "body">: csharp "EmbeddedStatement"]

resourceAcquisition :: Binding
resourceAcquisition = def "ResourceAcquisition" $ T.union [
  "local">: csharp "LocalVariableDeclaration",
  "expression">: csharp "Expression"]

yieldStatement :: Binding
yieldStatement = def "YieldStatement" $ T.union [
  "return">: csharp "Expression",
  "break">: T.unit]

compilationUnit :: Binding
compilationUnit = def "CompilationUnit" $ T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "attributes">: T.list $ csharp "GlobalAttributeSection",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

namespaceDeclaration :: Binding
namespaceDeclaration = def "NamespaceDeclaration" $ T.record [
  "name">: csharp "QualifiedIdentifier",
  "body">: csharp "NamespaceBody"]

qualifiedIdentifier :: Binding
qualifiedIdentifier = def "QualifiedIdentifier" $ T.wrap $
  nonemptyList $ csharp "Identifier"

namespaceBody :: Binding
namespaceBody = def "NamespaceBody" $ T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

externAliasDirective :: Binding
externAliasDirective = def "ExternAliasDirective" $ T.wrap $
  csharp "Identifier"

usingDirective :: Binding
usingDirective = def "UsingDirective" $ T.union [
  "alias">: csharp "UsingAliasDirective",
  "namespace">: csharp "NamespaceName",
  "static">: csharp "TypeName"]

usingAliasDirective :: Binding
usingAliasDirective = def "UsingAliasDirective" $ T.record [
  "alias">: csharp "Identifier",
  "name">: csharp "NamespaceOrTypeName"]

namespaceMemberDeclaration :: Binding
namespaceMemberDeclaration = def "NamespaceMemberDeclaration" $ T.union [
  "namespace">: csharp "NamespaceDeclaration",
  "type">: csharp "TypeDeclaration"]

typeDeclaration :: Binding
typeDeclaration = def "TypeDeclaration" $ T.union [
  "class">: csharp "ClassDeclaration",
  "struct">: csharp "StructDeclaration",
  "interface">: csharp "InterfaceDeclaration",
  "enum">: csharp "EnumDeclaration",
  "delegate">: csharp "DelegateDeclaration"]

qualifiedAliasMember :: Binding
qualifiedAliasMember = def "QualifiedAliasMember" $ T.record [
  "alias">: csharp "Identifier",
  "member">: csharp "Identifier",
  "arguments">: T.maybe $ csharp "TypeArgumentList"]

classDeclaration :: Binding
classDeclaration = def "ClassDeclaration" $ T.record [
  "attributes">: T.list $ csharp "AttributeSection",
  "modifiers">: T.list $ csharp "ClassModifier",
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.maybe $ csharp "TypeParameterList",
  "base">: T.maybe $ csharp "ClassBase",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: csharp "ClassBody"]

classModifier :: Binding
classModifier = def "ClassModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "abstract",
  "sealed",
  "static",
  "unsafe"]

typeParameterList :: Binding
typeParameterList = def "TypeParameterList" $ T.wrap $ nonemptyList $ csharp "TypeParameterPart"

typeParameterPart :: Binding
typeParameterPart = def "TypeParameterPart" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "name">: csharp "TypeParameter"]

classBase :: Binding
classBase = def "ClassBase" $ T.union [
  "class">: T.maybe $ csharp "ClassType",
  "interfaces">: T.list $ csharp "InterfaceType"]

typeParameterConstraintsClause :: Binding
typeParameterConstraintsClause = def "TypeParameterConstraintsClause" $ T.record [
  "parameter">: csharp "TypeParameter",
  "constraints">: T.list $ csharp "TypeParameterConstraints"]

typeParameterConstraints :: Binding
typeParameterConstraints = def "TypeParameterConstraints" $ T.record [
  "primary">: T.maybe $ csharp "PrimaryConstraint",
  "secondary">: T.maybe $ csharp "SecondaryConstraints",
  "constructor">: T.boolean]

primaryConstraint :: Binding
primaryConstraint = def "PrimaryConstraint" $ T.union [
  "classType">: csharp "ClassType",
  "class">: T.unit,
  "struct">: T.unit,
  "unmanaged">: T.unit]

secondaryConstraints :: Binding
secondaryConstraints = def "SecondaryConstraints" $ T.wrap $ nonemptyList $ csharp "SecondaryConstraint"

secondaryConstraint :: Binding
secondaryConstraint = def "SecondaryConstraint" $ T.union [
  "interface">: csharp "InterfaceType",
  "parameter">: csharp "TypeParameter"]

classBody :: Binding
classBody = def "ClassBody" $ T.wrap $
  T.list $ csharp "ClassMemberDeclaration"

classMemberDeclaration :: Binding
classMemberDeclaration = def "ClassMemberDeclaration" $ T.union [
  "constant">: csharp "ConstantDeclaration",
  "field">: csharp "FieldDeclaration",
  "method">: csharp "MethodDeclaration",
  "property">: csharp "PropertyDeclaration",
  "event">: csharp "EventDeclaration",
  "indexer">: csharp "IndexerDeclaration",
  "operator">: csharp "OperatorDeclaration",
  "constructor">: csharp "ConstructorDeclaration",
  "finalizer">: csharp "FinalizerDeclaration",
  "staticConstructor">: csharp "StaticConstructorDeclaration",
  "type">: csharp "TypeDeclaration"]

constantDeclaration :: Binding
constantDeclaration = def "ConstantDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstantModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

constantModifier :: Binding
constantModifier = def "ConstantModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

fieldDeclaration :: Binding
fieldDeclaration = def "FieldDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "FieldModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "VariableDeclarator"]

fieldModifier :: Binding
fieldModifier = def "FieldModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "static",
  "readonly",
  "volatile",
  "unsafe"]

variableDeclarators :: Binding
variableDeclarators = def "VariableDeclarators" $ T.wrap $ nonemptyList $ csharp "VariableDeclarator"

variableDeclarator :: Binding
variableDeclarator = def "VariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.maybe $ csharp "VariableInitializer"]

methodDeclaration :: Binding
methodDeclaration = def "MethodDeclaration" $ T.union [
  "standard">: csharp "StandardMethodDeclaration",
  "refReturn">: csharp "RefReturnMethodDeclaration"]

standardMethodDeclaration :: Binding
standardMethodDeclaration = def "StandardMethodDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "MethodModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "MethodBody"]

refReturnMethodDeclaration :: Binding
refReturnMethodDeclaration = def "RefReturnMethodDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "RefMethodModifier",
  "kind">: csharp "RefKind",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "RefMethodBody"]

methodModifiers :: Binding
methodModifiers = def "MethodModifiers" $ T.record [
  "modifiers">: T.list $ csharp "MethodModifier",
  "partial">: T.boolean]

refKind :: Binding
refKind = def "RefKind" $ T.enum [
  "ref",
  "refReadonly"]

methodHeader :: Binding
methodHeader = def "MethodHeader" $ T.record [
  "name">: csharp "MemberName",
  "typeParameters">: T.maybe $ csharp "TypeParameterList",
  "parameters">: T.maybe $ csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

methodModifier :: Binding
methodModifier = def "MethodModifier" $ T.union [
  "ref">: csharp "RefMethodModifier",
  "async">: T.unit]

refMethodModifier :: Binding
refMethodModifier = def "RefMethodModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "static",
  "virtual",
  "sealed",
  "override",
  "abstract",
  "extern",
  "unsafe"]

returnType :: Binding
returnType = def "ReturnType" $ T.union [
  "ref">: csharp "Type",
  "void">: T.unit]

memberName :: Binding
memberName = def "MemberName" $ T.record [
  "interfaceType">: T.maybe $ csharp "TypeName",
  "identifier">: csharp "Identifier"]

methodBody :: Binding
methodBody = def "MethodBody" $ T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "empty">: T.unit]

refMethodBody :: Binding
refMethodBody = def "RefMethodBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

formalParameterList :: Binding
formalParameterList = def "FormalParameterList" $ T.record [
  "fixed">: T.list $ csharp "FixedParameter",
  "array">: T.maybe $ csharp "ParameterArray"]

fixedParameter :: Binding
fixedParameter = def "FixedParameter" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifier">: T.maybe $ csharp "ParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier",
  "defaultArgument">: T.maybe $ csharp "Expression"]

parameterModifier :: Binding
parameterModifier = def "ParameterModifier" $ T.union [
  "mode">: csharp "ParameterModeModifier",
  "this">: T.unit]

parameterModeModifier :: Binding
parameterModeModifier = def "ParameterModeModifier" $ T.enum [
  "ref",
  "out",
  "in"]

parameterArray :: Binding
parameterArray = def "ParameterArray" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "type">: csharp "ArrayType",
  "identifier">: csharp "Identifier"]

propertyDeclaration :: Binding
propertyDeclaration = def "PropertyDeclaration" $ T.union [
  "standard">: csharp "StandardPropertyDeclaration",
  "refReturn">: csharp "RefReturnPropertyDeclaration"]

standardPropertyDeclaration :: Binding
standardPropertyDeclaration = def "StandardPropertyDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "PropertyBody"]

refReturnPropertyDeclaration :: Binding
refReturnPropertyDeclaration = def "RefReturnPropertyDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "RefPropertyBody"]

propertyModifier :: Binding
propertyModifier = def "PropertyModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "static",
  "virtual",
  "sealed",
  "override",
  "abstract",
  "extern",
  "unsafe"]

propertyBody :: Binding
propertyBody = def "PropertyBody" $ T.union [
  "block">: csharp "BlockPropertyBody",
  "expression">: csharp "Expression"]

blockPropertyBody :: Binding
blockPropertyBody = def "BlockPropertyBody" $ T.record [
  "accessors">: csharp "AccessorDeclarations",
  "initializer">: T.maybe $ csharp "VariableInitializer"]

refPropertyBody :: Binding
refPropertyBody = def "RefPropertyBody" $ T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

accessorDeclarations :: Binding
accessorDeclarations = def "AccessorDeclarations" $ T.union [
  "get">: T.maybe $ csharp "AccessorDeclaration",
  "set">: T.maybe $ csharp "AccessorDeclaration"]

accessorDeclaration :: Binding
accessorDeclaration = def "AccessorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifier">: T.maybe $ csharp "AccessorModifier",
  "body">: csharp "AccessorBody"]

accessorModifier :: Binding
accessorModifier = def "AccessorModifier" $ T.enum [
  "protected",
  "internal",
  "private",
  "protectedInternal",
  "internalProtected",
  "protectedPrivate",
  "privateProtected"]

accessorBody :: Binding
accessorBody = def "AccessorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

refGetAccessorDeclaration :: Binding
refGetAccessorDeclaration = def "RefGetAccessorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifier">: T.maybe $ csharp "AccessorModifier",
  "body">: csharp "RefAccessorBody"]

refAccessorBody :: Binding
refAccessorBody = def "RefAccessorBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

eventDeclaration :: Binding
eventDeclaration = def "EventDeclaration" $ T.union [
  "standard">: csharp "StandardEventDeclaration",
  "accessors">: csharp "AccessorsEventDeclaration"]

standardEventDeclaration :: Binding
standardEventDeclaration = def "StandardEventDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "declarators">: csharp "VariableDeclarators"]

accessorsEventDeclaration :: Binding
accessorsEventDeclaration = def "AccessorsEventDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "accessors">: csharp "EventAccessorDeclarations"]

eventModifier :: Binding
eventModifier = def "EventModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "static",
  "virtual",
  "sealed",
  "override",
  "abstract",
  "extern",
  "unsafe"]

eventAccessorDeclarations :: Binding
eventAccessorDeclarations = def "EventAccessorDeclarations" $ T.union [
  "add">: csharp "AddRemoveAccessorDeclaration",
  "remove">: csharp "AddRemoveAccessorDeclaration"]

addRemoveAccessorDeclaration :: Binding
addRemoveAccessorDeclaration = def "AddRemoveAccessorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "body">: csharp "Block"]

indexerDeclaration :: Binding
indexerDeclaration = def "IndexerDeclaration" $ T.union [
  "standard">: csharp "StandardIndexerDeclaration",
  "ref">: csharp "RefIndexerDeclaration"]

standardIndexerDeclaration :: Binding
standardIndexerDeclaration = def "StandardIndexerDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "IndexerBody"]

refIndexerDeclaration :: Binding
refIndexerDeclaration = def "RefIndexerDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "refKind">: csharp "RefKind",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "RefIndexerBody"]

indexerModifier :: Binding
indexerModifier = def "IndexerModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "virtual",
  "sealed",
  "override",
  "abstract",
  "extern",
  "unsafe"]

indexerDeclarator :: Binding
indexerDeclarator = def "IndexerDeclarator" $ T.record [
  "type">: csharp "Type",
  "interface">: T.maybe $ csharp "InterfaceType",
  "parameters">: csharp "FormalParameterList"]

indexerBody :: Binding
indexerBody = def "IndexerBody" $ T.union [
  "block">: csharp "AccessorDeclarations",
  "expression">: csharp "Expression"]

refIndexerBody :: Binding
refIndexerBody = def "RefIndexerBody" $ T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

operatorDeclaration :: Binding
operatorDeclaration = def "OperatorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "OperatorModifier",
  "declarator">: csharp "OperatorDeclarator",
  "body">: csharp "OperatorBody"]

operatorModifier :: Binding
operatorModifier = def "OperatorModifier" $ T.enum [
  "public",
  "static",
  "extern",
  "unsafe"]

operatorDeclarator :: Binding
operatorDeclarator = def "OperatorDeclarator" $ T.union [
  "unary">: csharp "UnaryOperatorDeclarator",
  "binary">: csharp "BinaryOperatorDeclarator",
  "conversion">: csharp "ConversionOperatorDeclarator"]

unaryOperatorDeclarator :: Binding
unaryOperatorDeclarator = def "UnaryOperatorDeclarator" $ T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableUnaryOperator",
  "parameter">: csharp "FixedParameter"]

overloadableUnaryOperator :: Binding
overloadableUnaryOperator = def "OverloadableUnaryOperator" $ T.enum [
  "plus",
  "minus",
  "not",
  "complement",
  "increment",
  "decrement",
  "true",
  "false"]

binaryOperatorDeclarator :: Binding
binaryOperatorDeclarator = def "BinaryOperatorDeclarator" $ T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableBinaryOperator",
  "left">: csharp "FixedParameter",
  "right">: csharp "FixedParameter"]

overloadableBinaryOperator :: Binding
overloadableBinaryOperator = def "OverloadableBinaryOperator" $ T.enum [
  "add",
  "subtract",
  "multiply",
  "divide",
  "modulus",
  "and",
  "or",
  "xor",
  "leftShift",
  "rightShift",
  "equal",
  "notEqual",
  "greaterThan",
  "lessThan",
  "greaterThanOrEqual",
  "lessThanOrEqual"]

conversionOperatorDeclarator :: Binding
conversionOperatorDeclarator = def "ConversionOperatorDeclarator" $ T.record [
  "kind">: csharp "ConversionKind",
  "type">: csharp "Type",
  "parameter">: csharp "FixedParameter"]

conversionKind :: Binding
conversionKind = def "ConversionKind" $ T.enum [
  "implicit",
  "explicit"]

operatorBody :: Binding
operatorBody = def "OperatorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

constructorDeclaration :: Binding
constructorDeclaration = def "ConstructorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstructorModifier",
  "declarator">: csharp "ConstructorDeclarator",
  "body">: csharp "ConstructorBody"]

constructorModifier :: Binding
constructorModifier = def "ConstructorModifier" $ T.enum [
  "public",
  "protected",
  "internal",
  "private",
  "extern",
  "unsafe"]

constructorDeclarator :: Binding
constructorDeclarator = def "ConstructorDeclarator" $ T.record [
  "name">: csharp "Identifier",
  "parameters">: T.maybe $ csharp "FormalParameterList",
  "initializer">: T.maybe $ csharp "ConstructorInitializer"]

constructorInitializer :: Binding
constructorInitializer = def "ConstructorInitializer" $ T.union [
  "base">: T.maybe $ csharp "ArgumentList",
  "this">: T.maybe $ csharp "ArgumentList"]

constructorBody :: Binding
constructorBody = def "ConstructorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

staticConstructorDeclaration :: Binding
staticConstructorDeclaration = def "StaticConstructorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: csharp "StaticConstructorModifiers",
  "name">: csharp "Identifier",
  "body">: csharp "StaticConstructorBody"]

staticConstructorModifiers :: Binding
staticConstructorModifiers = def "StaticConstructorModifiers" $ T.record [
  "extern">: T.boolean,
  "unsafe">: T.boolean]

staticConstructorBody :: Binding
staticConstructorBody = def "StaticConstructorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

finalizerDeclaration :: Binding
finalizerDeclaration = def "FinalizerDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "extern">: T.boolean,
  "unsafe">: T.boolean,
  "name">: csharp "Identifier",
  "body">: csharp "FinalizerBody"]

finalizerBody :: Binding
finalizerBody = def "FinalizerBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

structDeclaration :: Binding
structDeclaration = def "StructDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "StructModifier",
  "ref">: T.boolean,
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.maybe $ csharp "TypeParameterList",
  "interfaces">: T.list $ csharp "InterfaceType",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: T.list $ csharp "StructMemberDeclaration"]

structModifier :: Binding
structModifier = def "StructModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "readonly",
  "unsafe"]

structMemberDeclaration :: Binding
structMemberDeclaration = def "StructMemberDeclaration" $ T.union [
  "constant">: csharp "ConstantDeclaration",
  "field">: csharp "FieldDeclaration",
  "method">: csharp "MethodDeclaration",
  "property">: csharp "PropertyDeclaration",
  "event">: csharp "EventDeclaration",
  "indexer">: csharp "IndexerDeclaration",
  "operator">: csharp "OperatorDeclaration",
  "constructor">: csharp "ConstructorDeclaration",
  "staticConstructor">: csharp "StaticConstructorDeclaration",
  "type">: csharp "TypeDeclaration",
  "fixedSizeBuffer">: csharp "FixedSizeBufferDeclaration"]

arrayInitializer :: Binding
arrayInitializer = def "ArrayInitializer" $ T.wrap $
  T.list $ csharp "VariableInitializer"

variableInitializer :: Binding
variableInitializer = def "VariableInitializer" $ T.union [
  "expression">: csharp "Expression",
  "array">: csharp "ArrayInitializer"]

interfaceDeclaration :: Binding
interfaceDeclaration = def "InterfaceDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "InterfaceModifier",
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.maybe $ csharp "VariantTypeParameters",
  "base">: T.list $ csharp "InterfaceType",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: T.list $ csharp "InterfaceMemberDeclaration"]

interfaceModifier :: Binding
interfaceModifier = def "InterfaceModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

variantTypeParameters :: Binding
variantTypeParameters = def "VariantTypeParameters" $ T.wrap $ T.list $ csharp "VariantTypeParameter"

variantTypeParameter :: Binding
variantTypeParameter = def "VariantTypeParameter" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "variance">: T.maybe $ csharp "VarianceAnnotation",
  "parameter">: csharp "TypeParameter"]

varianceAnnotation :: Binding
varianceAnnotation = def "VarianceAnnotation" $ T.enum [
  "in",
  "out"]

interfaceMemberDeclaration :: Binding
interfaceMemberDeclaration = def "InterfaceMemberDeclaration" $ T.union [
  "method">: csharp "InterfaceMethodDeclaration",
  "property">: csharp "InterfacePropertyDeclaration",
  "event">: csharp "InterfaceEventDeclaration",
  "indexer">: csharp "InterfaceIndexerDeclaration"]

interfaceMethodDeclaration :: Binding
interfaceMethodDeclaration = def "InterfaceMethodDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "new">: T.boolean,
  "returnType">: csharp "ReturnType",
  "refKind">: T.maybe $ csharp "RefKind",
  "header">: csharp "InterfaceMethodHeader"]

interfaceMethodHeader :: Binding
interfaceMethodHeader = def "InterfaceMethodHeader" $ T.record [
  "name">: csharp "Identifier",
  "parameters">: T.maybe $ csharp "FormalParameterList",
  "typeParameters">: T.maybe $ csharp "TypeParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

interfacePropertyDeclaration :: Binding
interfacePropertyDeclaration = def "InterfacePropertyDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.maybe $ csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "Identifier",
  "accessors">: csharp "InterfaceAccessors"]

interfaceAccessors :: Binding
interfaceAccessors = def "InterfaceAccessors" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "get">: T.maybe $ csharp "Attributes",
  "set">: T.maybe $ csharp "Attributes"]

interfaceEventDeclaration :: Binding
interfaceEventDeclaration = def "InterfaceEventDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "new">: T.boolean,
  "type">: csharp "Type",
  "name">: csharp "Identifier"]

interfaceIndexerDeclaration :: Binding
interfaceIndexerDeclaration = def "InterfaceIndexerDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.maybe $ csharp "RefKind",
  "type">: csharp "Type",
  "parameters">: csharp "FormalParameterList",
  "accessors">: csharp "InterfaceAccessors"]

enumDeclaration :: Binding
enumDeclaration = def "EnumDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EnumModifier",
  "name">: csharp "Identifier",
  "base">: T.maybe $ csharp "EnumBase",
  "body">: T.maybe $ csharp "EnumBody"]

enumBase :: Binding
enumBase = def "EnumBase" $ T.union [
  "type">: csharp "IntegralType",
  "name">: csharp "TypeName"]

enumBody :: Binding
enumBody = def "EnumBody" $ T.wrap $
  T.list $ csharp "EnumMemberDeclaration"

enumModifier :: Binding
enumModifier = def "EnumModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

enumMemberDeclaration :: Binding
enumMemberDeclaration = def "EnumMemberDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "name">: csharp "Identifier",
  "value">: T.maybe $ csharp "ConstantExpression"]

delegateDeclaration :: Binding
delegateDeclaration = def "DelegateDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "DelegateModifier",
  "returnType">: csharp "ReturnType",
  "refKind">: T.maybe $ csharp "RefKind",
  "refReturnType">: T.maybe $ csharp "Type",
  "header">: csharp "DelegateHeader"]

delegateHeader :: Binding
delegateHeader = def "DelegateHeader" $ T.record [
  "name">: csharp "Identifier",
  "typeParameters">: T.maybe $ csharp "VariantTypeParameters",
  "parameters">: T.maybe $ csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

delegateModifier :: Binding
delegateModifier = def "DelegateModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

globalAttributeSection :: Binding
globalAttributeSection = def "GlobalAttributeSection" $ T.record [
  "target">: csharp "Identifier",
  "attributes">: csharp "AttributeList"]

attributes :: Binding
attributes = def "Attributes" $ T.wrap $ nonemptyList $ csharp "AttributeSection"

attributeSection :: Binding
attributeSection = def "AttributeSection" $ T.record [
  "target">: T.maybe $ csharp "AttributeTarget",
  "attributes">: csharp "AttributeList"]

attributeTarget :: Binding
attributeTarget = def "AttributeTarget" $ T.union [
  "identifier">: csharp "Identifier",
  "keyword">: csharp "Keyword"]

attributeList :: Binding
attributeList = def "AttributeList" $ T.wrap $ nonemptyList $ csharp "Attribute"

attribute :: Binding
attribute = def "Attribute" $ T.record [
  "name">: csharp "AttributeName",
  "arguments">: T.maybe $ csharp "AttributeArguments"]

attributeName :: Binding
attributeName = def "AttributeName" $ T.wrap $
  csharp "TypeName"

attributeArguments :: Binding
attributeArguments = def "AttributeArguments" $ T.record [
  "positonal">: T.maybe $ csharp "PositionalArgumentList",
  "named">: T.maybe $ csharp "NamedArgumentList"]

positionalArgumentList :: Binding
positionalArgumentList = def "PositionalArgumentList" $ T.wrap $ nonemptyList $ csharp "PositionalArgument"

positionalArgument :: Binding
positionalArgument = def "PositionalArgument" $ T.record [
  "name">: T.maybe $ csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

namedArgumentList :: Binding
namedArgumentList = def "NamedArgumentList" $ T.wrap $ nonemptyList $ csharp "NamedArgument"

namedArgument :: Binding
namedArgument = def "NamedArgument" $ T.record [
  "name">: csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

attributeArgumentExpression :: Binding
attributeArgumentExpression = def "AttributeArgumentExpression" $ T.wrap $
  csharp "NonAssignmentExpression"


-- Unsafe Elements

pointerType :: Binding
pointerType = def "PointerType" $ T.union [
  "valueType">: T.maybe $ csharp "ValueType",
  "pointerDepth">: T.int32]

pointerMemberAccess :: Binding
pointerMemberAccess = def "PointerMemberAccess" $ T.record [
  "pointer">: csharp "PrimaryExpression",
  "member">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

pointerElementAccess :: Binding
pointerElementAccess = def "PointerElementAccess" $ T.record [
  "pointer">: csharp "PrimaryNoArrayCreationExpression",
  "index">: csharp "Expression"]

fixedStatement :: Binding
fixedStatement = def "FixedStatement" $ T.record [
  "pointerType">: csharp "PointerType",
  "declarators">: nonemptyList $ csharp "FixedPointerDeclarator",
  "statement">: csharp "EmbeddedStatement"]

fixedPointerDeclarator :: Binding
fixedPointerDeclarator = def "FixedPointerDeclarator" $ T.union [
  "reference">: csharp "VariableReference",
  "expression">: csharp "Expression"]

fixedSizeBufferDeclaration :: Binding
fixedSizeBufferDeclaration = def "FixedSizeBufferDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: nonemptyList $ csharp "FixedSizeBufferModifier",
  "elementType">: csharp "Type",
  "declarators">: nonemptyList $ csharp "FixedSizeBufferDeclarator"]

fixedSizeBufferModifier :: Binding
fixedSizeBufferModifier = def "FixedSizeBufferModifier" $ T.enum [
  "new",
  "public",
  "internal",
  "private",
  "unsafe"]

fixedSizeBufferDeclarator :: Binding
fixedSizeBufferDeclarator = def "FixedSizeBufferDeclarator" $ T.record [
  "name">: csharp "Identifier",
  "size">: csharp "ConstantExpression"]
