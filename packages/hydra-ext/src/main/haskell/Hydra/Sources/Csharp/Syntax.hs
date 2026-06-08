module Hydra.Sources.Csharp.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel hiding (typeParameterConstraints)
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
ns = ModuleName "hydra.csharp.syntax"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just ("A C# syntax module based on the ANTLR grammar dated 02/07/2024 and available at:\n"
      ++ "  https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar"))}
  where
    definitions = lexicalElements ++ syntacticElements ++ unsafeElements

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


csharp :: String -> Type
csharp = typeref ns


def :: String -> Type -> TypeDefinition
def = datatype ns

-- Lexical Elements

identifier :: TypeDefinition
identifier = def "Identifier" $ T.wrap T.string

integerLiteral :: TypeDefinition
integerLiteral = def "IntegerLiteral" $ T.union [
  "decimal">: T.string,
  "hexadecimal">: T.string,
  "binary">: T.bigint]


keyword :: TypeDefinition
keyword = def "Keyword" $ T.wrap T.string

literal :: TypeDefinition
literal = def "Literal" $ T.union [
  "boolean">: T.boolean,
  "integer">: csharp "IntegerLiteral",
  "real">: T.float64,
  "character">: T.string,
  "string">: T.string,
  "null">: T.unit]

-- Syntactic Elements

accessorBody :: TypeDefinition
accessorBody = def "AccessorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

accessorDeclaration :: TypeDefinition
accessorDeclaration = def "AccessorDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifier">: T.optional $ csharp "AccessorModifier",
  "body">: csharp "AccessorBody"]

accessorDeclarations :: TypeDefinition
accessorDeclarations = def "AccessorDeclarations" $ T.union [
  "get">: T.optional $ csharp "AccessorDeclaration",
  "set">: T.optional $ csharp "AccessorDeclaration"]

accessorModifier :: TypeDefinition
accessorModifier = def "AccessorModifier" $ T.enum [
  "protected",
  "internal",
  "private",
  "protectedInternal",
  "internalProtected",
  "protectedPrivate",
  "privateProtected"]

accessorsEventDeclaration :: TypeDefinition
accessorsEventDeclaration = def "AccessorsEventDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "accessors">: csharp "EventAccessorDeclarations"]

addRemoveAccessorDeclaration :: TypeDefinition
addRemoveAccessorDeclaration = def "AddRemoveAccessorDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "body">: csharp "Block"]

additiveExpression :: TypeDefinition
additiveExpression = def "AdditiveExpression" $ T.union [
  "simple">: csharp "MultiplicativeExpression",
  "binary">: csharp "BinaryAdditiveExpression"]

additiveOperator :: TypeDefinition
additiveOperator = def "AdditiveOperator" $ T.enum [
  "plus",
  "minus"]

andExpression :: TypeDefinition
andExpression = def "AndExpression" $ T.union [
  "simple">: csharp "EqualityExpression",
  "binary">: csharp "BinaryAndExpression"]

anonymousFunctionBody :: TypeDefinition
anonymousFunctionBody = def "AnonymousFunctionBody" $ T.union [
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "ref">: csharp "VariableReference",
  "block">: csharp "Block"]

anonymousFunctionParameterModifier :: TypeDefinition
anonymousFunctionParameterModifier = def "AnonymousFunctionParameterModifier" $ T.enum [
  "ref",
  "out",
  "in"]

anonymousFunctionSignature :: TypeDefinition
anonymousFunctionSignature = def "AnonymousFunctionSignature" $ T.union [
  "explicit">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "implicit">: T.list $ csharp "Identifier"]

anonymousMethodExpression :: TypeDefinition
anonymousMethodExpression = def "AnonymousMethodExpression" $ T.record [
  "async">: T.boolean,
  "signature">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "body">: csharp "Block"]

argument :: TypeDefinition
argument = def "Argument" $ T.record [
  "name">: T.optional $ csharp "Identifier",
  "value">: csharp "ArgumentValue"]

argumentList :: TypeDefinition
argumentList = def "ArgumentList" $ T.wrap $ nonemptyList $ csharp "Argument"

argumentValue :: TypeDefinition
argumentValue = def "ArgumentValue" $ T.union [
  "expression">: csharp "Expression",
  "in">: csharp "VariableReference",
  "ref">: csharp "VariableReference",
  "out">: csharp "VariableReference"]

arrayCreationExpression :: TypeDefinition
arrayCreationExpression = def "ArrayCreationExpression" $ T.union [
  "nonArrayType">: csharp "NonArrayTypeArrayCreationExpression",
  "arrayType">: csharp "ArrayTypeArrayCreationExpression",
  "rankSpecifier">: csharp "RankSpecifierArrayCreationExpression"]

arrayInitializer :: TypeDefinition
arrayInitializer = def "ArrayInitializer" $ T.wrap $
  T.list $ csharp "VariableInitializer"

arrayType :: TypeDefinition
arrayType = def "ArrayType" $ T.record [
  "type">: csharp "NonArrayType",
  "rank">: T.list $ csharp "RankSpecifier"]

arrayTypeArrayCreationExpression :: TypeDefinition
arrayTypeArrayCreationExpression = def "ArrayTypeArrayCreationExpression" $ T.record [
  "type">: csharp "ArrayType",
  "initializer">: csharp "ArrayInitializer"]

asTypeExpression :: TypeDefinition
asTypeExpression = def "AsTypeExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

assignment :: TypeDefinition
assignment = def "Assignment" $ T.record [
  "left">: csharp "UnaryExpression",
  "operator">: csharp "AssignmentOperator",
  "right">: csharp "Expression"]

assignmentMemberDeclarator :: TypeDefinition
assignmentMemberDeclarator = def "AssignmentMemberDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

assignmentOperator :: TypeDefinition
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

attribute :: TypeDefinition
attribute = def "Attribute" $ T.record [
  "name">: csharp "AttributeName",
  "arguments">: T.optional $ csharp "AttributeArguments"]

attributeArgumentExpression :: TypeDefinition
attributeArgumentExpression = def "AttributeArgumentExpression" $ T.wrap $
  csharp "NonAssignmentExpression"


attributeArguments :: TypeDefinition
attributeArguments = def "AttributeArguments" $ T.record [
  "positonal">: T.optional $ csharp "PositionalArgumentList",
  "named">: T.optional $ csharp "NamedArgumentList"]

attributeList :: TypeDefinition
attributeList = def "AttributeList" $ T.wrap $ nonemptyList $ csharp "Attribute"

attributeName :: TypeDefinition
attributeName = def "AttributeName" $ T.wrap $
  csharp "TypeName"

attributeSection :: TypeDefinition
attributeSection = def "AttributeSection" $ T.record [
  "target">: T.optional $ csharp "AttributeTarget",
  "attributes">: csharp "AttributeList"]

attributeTarget :: TypeDefinition
attributeTarget = def "AttributeTarget" $ T.union [
  "identifier">: csharp "Identifier",
  "keyword">: csharp "Keyword"]

attributes :: TypeDefinition
attributes = def "Attributes" $ T.wrap $ nonemptyList $ csharp "AttributeSection"

baseAccess :: TypeDefinition
baseAccess = def "BaseAccess" $ T.union [
  "identifier">: csharp "BaseAccessWithIdentifier",
  "arguments">: csharp "ArgumentList"]

baseAccessWithIdentifier :: TypeDefinition
baseAccessWithIdentifier = def "BaseAccessWithIdentifier" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

binaryAdditiveExpression :: TypeDefinition
binaryAdditiveExpression = def "BinaryAdditiveExpression" $ T.record [
  "left">: csharp "AdditiveExpression",
  "operator">: csharp "AdditiveOperator",
  "right">: csharp "MultiplicativeExpression"]

binaryAndExpression :: TypeDefinition
binaryAndExpression = def "BinaryAndExpression" $ T.record [
  "left">: csharp "AndExpression",
  "right">: csharp "EqualityExpression"]

binaryConditionalAndExpression :: TypeDefinition
binaryConditionalAndExpression = def "BinaryConditionalAndExpression" $ T.record [
  "left">: csharp "ConditionalAndExpression",
  "right">: csharp "InclusiveOrExpression"]

binaryConditionalOrExpression :: TypeDefinition
binaryConditionalOrExpression = def "BinaryConditionalOrExpression" $ T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "ConditionalAndExpression"]

binaryEqualityExpression :: TypeDefinition
binaryEqualityExpression = def "BinaryEqualityExpression" $ T.record [
  "left">: csharp "EqualityExpression",
  "operator">: csharp "EqualityOperator",
  "right">: csharp "RelationalExpression"]

binaryExclusiveOrExpression :: TypeDefinition
binaryExclusiveOrExpression = def "BinaryExclusiveOrExpression" $ T.record [
  "left">: csharp "ExclusiveOrExpression",
  "right">: csharp "AndExpression"]

binaryInclusiveOrExpression :: TypeDefinition
binaryInclusiveOrExpression = def "BinaryInclusiveOrExpression" $ T.record [
  "left">: csharp "InclusiveOrExpression",
  "right">: csharp "ExclusiveOrExpression"]

binaryMultiplicativeExpression :: TypeDefinition
binaryMultiplicativeExpression = def "BinaryMultiplicativeExpression" $ T.record [
  "left">: csharp "MultiplicativeExpression",
  "operator">: csharp "MultiplicativeOperator",
  "right">: csharp "UnaryExpression"]

binaryNullCoalescingExpression :: TypeDefinition
binaryNullCoalescingExpression = def "BinaryNullCoalescingExpression" $ T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "NullCoalescingExpression"]

binaryOperatorDeclarator :: TypeDefinition
binaryOperatorDeclarator = def "BinaryOperatorDeclarator" $ T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableBinaryOperator",
  "left">: csharp "FixedParameter",
  "right">: csharp "FixedParameter"]

binaryRelationalExpression :: TypeDefinition
binaryRelationalExpression = def "BinaryRelationalExpression" $ T.record [
  "left">: csharp "RelationalExpression",
  "operator">: csharp "RelationalOperator",
  "right">: csharp "ShiftExpression"]

binaryShiftExpression :: TypeDefinition
binaryShiftExpression = def "BinaryShiftExpression" $ T.record [
  "left">: csharp "ShiftExpression",
  "operator">: csharp "ShiftOperator",
  "right">: csharp "AdditiveExpression"]

block :: TypeDefinition
block = def "Block" $ T.wrap $
  T.list $ csharp "Statement"

blockPropertyBody :: TypeDefinition
blockPropertyBody = def "BlockPropertyBody" $ T.record [
  "accessors">: csharp "AccessorDeclarations",
  "initializer">: T.optional $ csharp "VariableInitializer"]

booleanExpression :: TypeDefinition
booleanExpression = def "BooleanExpression" $ T.wrap $
  csharp "Expression"

castExpression :: TypeDefinition
castExpression = def "CastExpression" $ T.record [
  "type">: csharp "Type",
  "expression">: csharp "UnaryExpression"]

catchClauses :: TypeDefinition
catchClauses = def "CatchClauses" $ T.union [
  "specific">: T.list $ csharp "SpecificCatchClause",
  "general">: csharp "Block"]

classBase :: TypeDefinition
classBase = def "ClassBase" $ T.union [
  "class">: T.optional $ csharp "ClassType",
  "interfaces">: T.list $ csharp "InterfaceType"]

classBody :: TypeDefinition
classBody = def "ClassBody" $ T.wrap $
  T.list $ csharp "ClassMemberDeclaration"

classDeclaration :: TypeDefinition
classDeclaration = def "ClassDeclaration" $ T.record [
  "attributes">: T.list $ csharp "AttributeSection",
  "modifiers">: T.list $ csharp "ClassModifier",
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "TypeParameterList",
  "base">: T.optional $ csharp "ClassBase",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: csharp "ClassBody"]

classMemberDeclaration :: TypeDefinition
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

classModifier :: TypeDefinition
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

classType :: TypeDefinition
classType = def "ClassType" $ T.union [
  "typeName">: csharp "TypeName",
  "object">: T.unit,
  "string">: T.unit]

compilationUnit :: TypeDefinition
compilationUnit = def "CompilationUnit" $ T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "attributes">: T.list $ csharp "GlobalAttributeSection",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

conditionalAndExpression :: TypeDefinition
conditionalAndExpression = def "ConditionalAndExpression" $ T.union [
  "simple">: csharp "InclusiveOrExpression",
  "binary">: csharp "BinaryConditionalAndExpression"]

conditionalExpression :: TypeDefinition
conditionalExpression = def "ConditionalExpression" $ T.union [
  "simple">: csharp "NullCoalescingExpression",
  "simpleConditional">: csharp "SimpleConditionalExpression",
  "refConditional">: csharp "RefConditionalExpression"]

conditionalOrExpression :: TypeDefinition
conditionalOrExpression = def "ConditionalOrExpression" $ T.union [
  "simple">: csharp "ConditionalAndExpression",
  "binary">: csharp "BinaryConditionalOrExpression"]

constantDeclaration :: TypeDefinition
constantDeclaration = def "ConstantDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstantModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

constantDeclarator :: TypeDefinition
constantDeclarator = def "ConstantDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "ConstantExpression"]

constantExpression :: TypeDefinition
constantExpression = def "ConstantExpression" $ T.wrap $
  csharp "Expression"

constantModifier :: TypeDefinition
constantModifier = def "ConstantModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

constructorBody :: TypeDefinition
constructorBody = def "ConstructorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

constructorDeclaration :: TypeDefinition
constructorDeclaration = def "ConstructorDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstructorModifier",
  "declarator">: csharp "ConstructorDeclarator",
  "body">: csharp "ConstructorBody"]

constructorDeclarator :: TypeDefinition
constructorDeclarator = def "ConstructorDeclarator" $ T.record [
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "initializer">: T.optional $ csharp "ConstructorInitializer"]

constructorInitializer :: TypeDefinition
constructorInitializer = def "ConstructorInitializer" $ T.union [
  "base">: T.optional $ csharp "ArgumentList",
  "this">: T.optional $ csharp "ArgumentList"]

constructorModifier :: TypeDefinition
constructorModifier = def "ConstructorModifier" $ T.enum [
  "public",
  "protected",
  "internal",
  "private",
  "extern",
  "unsafe"]

conversionKind :: TypeDefinition
conversionKind = def "ConversionKind" $ T.enum [
  "implicit",
  "explicit"]

conversionOperatorDeclarator :: TypeDefinition
conversionOperatorDeclarator = def "ConversionOperatorDeclarator" $ T.record [
  "kind">: csharp "ConversionKind",
  "type">: csharp "Type",
  "parameter">: csharp "FixedParameter"]

declarationExpression :: TypeDefinition
declarationExpression = def "DeclarationExpression" $ T.record [
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier"]

declarationPattern :: TypeDefinition
declarationPattern = def "DeclarationPattern" $ T.record [
  "type">: csharp "Type",
  "designation">: csharp "Designation"]

declarationStatement :: TypeDefinition
declarationStatement = def "DeclarationStatement" $ T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "constant">: csharp "LocalConstantDeclaration",
  "function">: csharp "LocalFunctionDeclaration"]

deconstructionElement :: TypeDefinition
deconstructionElement = def "DeconstructionElement" $ T.union [
  "tuple">: csharp "DeconstructionTuple",
  "identifier">: csharp "Identifier"]

deconstructionTuple :: TypeDefinition
deconstructionTuple = def "DeconstructionTuple" $ T.wrap $
  nonemptyList $ csharp "DeconstructionElement"

defaultValueExpression :: TypeDefinition
defaultValueExpression = def "DefaultValueExpression" $ T.union [
  "explicitlyTyped">: csharp "Type",
  "defaultLiteral">: T.unit]

delegateCreationExpression :: TypeDefinition
delegateCreationExpression = def "DelegateCreationExpression" $ T.record [
  "type">: csharp "DelegateType",
  "expression">: csharp "Expression"]

delegateDeclaration :: TypeDefinition
delegateDeclaration = def "DelegateDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "DelegateModifier",
  "returnType">: csharp "ReturnType",
  "refKind">: T.optional $ csharp "RefKind",
  "refReturnType">: T.optional $ csharp "Type",
  "header">: csharp "DelegateHeader"]

delegateHeader :: TypeDefinition
delegateHeader = def "DelegateHeader" $ T.record [
  "name">: csharp "Identifier",
  "typeParameters">: T.optional $ csharp "VariantTypeParameters",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

delegateModifier :: TypeDefinition
delegateModifier = def "DelegateModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

delegateType :: TypeDefinition
delegateType = def "DelegateType" $ T.wrap $
  csharp "TypeName"

dependentAccess :: TypeDefinition
dependentAccess = def "DependentAccess" $ T.union [
  "memberAccess">: csharp "DependentAccessForMember",
  "elementAccess">: csharp "ArgumentList",
  "invocation">: T.optional $ csharp "ArgumentList"]

dependentAccessForMember :: TypeDefinition
dependentAccessForMember = def "DependentAccessForMember" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

designation :: TypeDefinition
designation = def "Designation" $ T.wrap $
  csharp "Identifier"

doStatement :: TypeDefinition
doStatement = def "DoStatement" $ T.record [
  "body">: csharp "EmbeddedStatement",
  "while">: csharp "BooleanExpression"]

elementAccess :: TypeDefinition
elementAccess = def "ElementAccess" $ T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList"]

elementInitializer :: TypeDefinition
elementInitializer = def "ElementInitializer" $ T.union [
  "single">: csharp "NonAssignmentExpression",
  "list">: csharp "ExpressionList"]

embeddedStatement :: TypeDefinition
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

enumBase :: TypeDefinition
enumBase = def "EnumBase" $ T.union [
  "type">: csharp "IntegralType",
  "name">: csharp "TypeName"]

enumBody :: TypeDefinition
enumBody = def "EnumBody" $ T.wrap $
  T.list $ csharp "EnumMemberDeclaration"

enumDeclaration :: TypeDefinition
enumDeclaration = def "EnumDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EnumModifier",
  "name">: csharp "Identifier",
  "base">: T.optional $ csharp "EnumBase",
  "body">: T.optional $ csharp "EnumBody"]

enumMemberDeclaration :: TypeDefinition
enumMemberDeclaration = def "EnumMemberDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "name">: csharp "Identifier",
  "value">: T.optional $ csharp "ConstantExpression"]

enumModifier :: TypeDefinition
enumModifier = def "EnumModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

enumType :: TypeDefinition
enumType = def "EnumType" $ T.wrap $
  csharp "TypeName"

equalityExpression :: TypeDefinition
equalityExpression = def "EqualityExpression" $ T.union [
  "simple">: csharp "RelationalExpression",
  "binary">: csharp "BinaryEqualityExpression"]

equalityOperator :: TypeDefinition
equalityOperator = def "EqualityOperator" $ T.enum [
  "equal",
  "notEqual"]

eventAccessorDeclarations :: TypeDefinition
eventAccessorDeclarations = def "EventAccessorDeclarations" $ T.union [
  "add">: csharp "AddRemoveAccessorDeclaration",
  "remove">: csharp "AddRemoveAccessorDeclaration"]

eventDeclaration :: TypeDefinition
eventDeclaration = def "EventDeclaration" $ T.union [
  "standard">: csharp "StandardEventDeclaration",
  "accessors">: csharp "AccessorsEventDeclaration"]

eventModifier :: TypeDefinition
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

exceptionSpecifier :: TypeDefinition
exceptionSpecifier = def "ExceptionSpecifier" $ T.record [
  "type">: csharp "Type",
  "identifier">: T.optional $ csharp "Identifier"]

exclusiveOrExpression :: TypeDefinition
exclusiveOrExpression = def "ExclusiveOrExpression" $ T.union [
  "simple">: csharp "AndExpression",
  "binary">: csharp "BinaryExclusiveOrExpression"]

explicitAnonymousFunctionParameter :: TypeDefinition
explicitAnonymousFunctionParameter = def "ExplicitAnonymousFunctionParameter" $ T.record [
  "modifier">: T.optional $ csharp "AnonymousFunctionParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier"]

explicitlyTypedLocalVariableDeclaration :: TypeDefinition
explicitlyTypedLocalVariableDeclaration = def "ExplicitlyTypedLocalVariableDeclaration" $ T.record [
  "type">: csharp "Type",
  "declarators">: T.list $ csharp "ExplicitlyTypedLocalVariableDeclarator"]

explicitlyTypedLocalVariableDeclarator :: TypeDefinition
explicitlyTypedLocalVariableDeclarator = def "ExplicitlyTypedLocalVariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.optional $ csharp "LocalVariableInitializer"]

expression :: TypeDefinition
expression = def "Expression" $ T.union [
  "nonAssignment">: csharp "NonAssignmentExpression",
  "assignment">: csharp "Assignment"]

expressionList :: TypeDefinition
expressionList = def "ExpressionList" $ T.wrap $ nonemptyList $ csharp "Expression"

externAliasDirective :: TypeDefinition
externAliasDirective = def "ExternAliasDirective" $ T.wrap $
  csharp "Identifier"

fieldDeclaration :: TypeDefinition
fieldDeclaration = def "FieldDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "FieldModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "VariableDeclarator"]

fieldModifier :: TypeDefinition
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

finalizerBody :: TypeDefinition
finalizerBody = def "FinalizerBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

finalizerDeclaration :: TypeDefinition
finalizerDeclaration = def "FinalizerDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "extern">: T.boolean,
  "unsafe">: T.boolean,
  "name">: csharp "Identifier",
  "body">: csharp "FinalizerBody"]

fixedParameter :: TypeDefinition
fixedParameter = def "FixedParameter" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifier">: T.optional $ csharp "ParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier",
  "defaultArgument">: T.optional $ csharp "Expression"]

floatingPointType :: TypeDefinition
floatingPointType = def "FloatingPointType" $ T.union [
  "float">: T.unit,
  "double">: T.unit]

forInitializer :: TypeDefinition
forInitializer = def "ForInitializer" $ T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "statements">: csharp "StatementExpressionList"]

forStatement :: TypeDefinition
forStatement = def "ForStatement" $ T.record [
  "initializer">: T.optional $ csharp "ForInitializer",
  "condition">: T.optional $ csharp "BooleanExpression",
  "iterator">: T.optional $ csharp "StatementExpressionList",
  "body">: csharp "EmbeddedStatement"]

foreachStatement :: TypeDefinition
foreachStatement = def "ForeachStatement" $ T.record [
  "kind">: T.optional $ csharp "RefKind",
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

formalParameterList :: TypeDefinition
formalParameterList = def "FormalParameterList" $ T.record [
  "fixed">: T.list $ csharp "FixedParameter",
  "array">: T.optional $ csharp "ParameterArray"]

fromClause :: TypeDefinition
fromClause = def "FromClause" $ T.record [
  "type">: T.optional $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression"]

globalAttributeSection :: TypeDefinition
globalAttributeSection = def "GlobalAttributeSection" $ T.record [
  "target">: csharp "Identifier",
  "attributes">: csharp "AttributeList"]

gotoStatement :: TypeDefinition
gotoStatement = def "GotoStatement" $ T.union [
  "identifier">: csharp "Identifier",
  "case">: csharp "ConstantExpression",
  "default">: T.unit]

groupClause :: TypeDefinition
groupClause = def "GroupClause" $ T.record [
  "grouped">: csharp "Expression",
  "by">: csharp "Expression"]

identifierNamespaceOrTypeName :: TypeDefinition
identifierNamespaceOrTypeName = def "IdentifierNamespaceOrTypeName" $ T.record [
  "identifier">: csharp "Identifier",
  "arguments">: T.optional $ csharp "TypeArgumentList"]

ifStatement :: TypeDefinition
ifStatement = def "IfStatement" $ T.record [
  "condition">: csharp "BooleanExpression",
  "ifBranch">: csharp "EmbeddedStatement",
  "elseBranch">: csharp "EmbeddedStatement"]

implicitlyTypedLocalVariableDeclaration :: TypeDefinition
implicitlyTypedLocalVariableDeclaration = def "ImplicitlyTypedLocalVariableDeclaration" $ T.union [
  "var">: csharp "ImplicitlyTypedLocalVariableDeclarator",
  "refVar">: csharp "RefVarImplicitlyTypedLocalVariableDeclaration"]

implicitlyTypedLocalVariableDeclarator :: TypeDefinition
implicitlyTypedLocalVariableDeclarator = def "ImplicitlyTypedLocalVariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

inclusiveOrExpression :: TypeDefinition
inclusiveOrExpression = def "InclusiveOrExpression" $ T.union [
  "simple">: csharp "ExclusiveOrExpression",
  "binary">: csharp "BinaryInclusiveOrExpression"]

indexerBody :: TypeDefinition
indexerBody = def "IndexerBody" $ T.union [
  "block">: csharp "AccessorDeclarations",
  "expression">: csharp "Expression"]

indexerDeclaration :: TypeDefinition
indexerDeclaration = def "IndexerDeclaration" $ T.union [
  "standard">: csharp "StandardIndexerDeclaration",
  "ref">: csharp "RefIndexerDeclaration"]

indexerDeclarator :: TypeDefinition
indexerDeclarator = def "IndexerDeclarator" $ T.record [
  "type">: csharp "Type",
  "interface">: T.optional $ csharp "InterfaceType",
  "parameters">: csharp "FormalParameterList"]

indexerModifier :: TypeDefinition
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

initializerTarget :: TypeDefinition
initializerTarget = def "InitializerTarget" $ T.union [
  "identifier">: csharp "Identifier",
  "arguments">: csharp "ArgumentList"]

initializerValue :: TypeDefinition
initializerValue = def "InitializerValue" $ T.union [
  "expression">: csharp "Expression",
  "objectOrCollection">: csharp "ObjectOrCollectionInitializer"]

integralType :: TypeDefinition
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

interfaceAccessors :: TypeDefinition
interfaceAccessors = def "InterfaceAccessors" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "get">: T.optional $ csharp "Attributes",
  "set">: T.optional $ csharp "Attributes"]

interfaceDeclaration :: TypeDefinition
interfaceDeclaration = def "InterfaceDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "InterfaceModifier",
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "VariantTypeParameters",
  "base">: T.list $ csharp "InterfaceType",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: T.list $ csharp "InterfaceMemberDeclaration"]

interfaceEventDeclaration :: TypeDefinition
interfaceEventDeclaration = def "InterfaceEventDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "type">: csharp "Type",
  "name">: csharp "Identifier"]

interfaceIndexerDeclaration :: TypeDefinition
interfaceIndexerDeclaration = def "InterfaceIndexerDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.optional $ csharp "RefKind",
  "type">: csharp "Type",
  "parameters">: csharp "FormalParameterList",
  "accessors">: csharp "InterfaceAccessors"]

interfaceMemberDeclaration :: TypeDefinition
interfaceMemberDeclaration = def "InterfaceMemberDeclaration" $ T.union [
  "method">: csharp "InterfaceMethodDeclaration",
  "property">: csharp "InterfacePropertyDeclaration",
  "event">: csharp "InterfaceEventDeclaration",
  "indexer">: csharp "InterfaceIndexerDeclaration"]

interfaceMethodDeclaration :: TypeDefinition
interfaceMethodDeclaration = def "InterfaceMethodDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "returnType">: csharp "ReturnType",
  "refKind">: T.optional $ csharp "RefKind",
  "header">: csharp "InterfaceMethodHeader"]

interfaceMethodHeader :: TypeDefinition
interfaceMethodHeader = def "InterfaceMethodHeader" $ T.record [
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "typeParameters">: T.optional $ csharp "TypeParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

interfaceModifier :: TypeDefinition
interfaceModifier = def "InterfaceModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

interfacePropertyDeclaration :: TypeDefinition
interfacePropertyDeclaration = def "InterfacePropertyDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.optional $ csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "Identifier",
  "accessors">: csharp "InterfaceAccessors"]

interfaceType :: TypeDefinition
interfaceType = def "InterfaceType" $ T.wrap $
  csharp "TypeName"

interpolatedRegularStringExpression :: TypeDefinition
interpolatedRegularStringExpression = def "InterpolatedRegularStringExpression" $ T.wrap T.string

interpolatedStringExpression :: TypeDefinition
interpolatedStringExpression = def "InterpolatedStringExpression" $ T.union [
  "regular">: csharp "InterpolatedRegularStringExpression",
  "verbatim">: csharp "InterpolatedVerbatimStringExpression"]

interpolatedVerbatimStringExpression :: TypeDefinition
interpolatedVerbatimStringExpression = def "InterpolatedVerbatimStringExpression" $ T.wrap T.string

invocationExpression :: TypeDefinition
invocationExpression = def "InvocationExpression" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "arguments">: T.optional $ csharp "ArgumentList"]

isPatternExpression :: TypeDefinition
isPatternExpression = def "IsPatternExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "pattern">: csharp "Pattern"]

isTypeExpression :: TypeDefinition
isTypeExpression = def "IsTypeExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

iterationStatement :: TypeDefinition
iterationStatement = def "IterationStatement" $ T.union [
  "while">: csharp "WhileStatement",
  "do">: csharp "DoStatement",
  "for">: csharp "ForStatement",
  "foreach">: csharp "ForeachStatement"]

joinClause :: TypeDefinition
joinClause = def "JoinClause" $ T.record [
  "type">: T.optional $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression",
  "on">: csharp "Expression",
  "equals">: csharp "Expression",
  "into">: T.optional $ csharp "Identifier"]

jumpStatement :: TypeDefinition
jumpStatement = def "JumpStatement" $ T.union [
  "break">: T.unit,
  "continue">: T.unit,
  "goto">: csharp "GotoStatement",
  "return">: csharp "ReturnStatement",
  "throw">: T.optional $ csharp "Expression"]

labeledStatement :: TypeDefinition
labeledStatement = def "LabeledStatement" $ T.record [
  "label">: csharp "Identifier",
  "statement">: csharp "Statement"]

lambdaExpression :: TypeDefinition
lambdaExpression = def "LambdaExpression" $ T.record [
  "async">: T.boolean,
  "signature">: csharp "AnonymousFunctionSignature",
  "body">: csharp "AnonymousFunctionBody"]

letClause :: TypeDefinition
letClause = def "LetClause" $ T.record [
  "left">: csharp "Identifier",
  "right">: csharp "Expression"]

localConstantDeclaration :: TypeDefinition
localConstantDeclaration = def "LocalConstantDeclaration" $ T.record [
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

localFunctionBody :: TypeDefinition
localFunctionBody = def "LocalFunctionBody" $ T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression"]

localFunctionDeclaration :: TypeDefinition
localFunctionDeclaration = def "LocalFunctionDeclaration" $ T.union [
  "standard">: csharp "StandardLocalFunctionDeclaration",
  "ref">: csharp "RefLocalFunctionDeclaration"]

localFunctionHeader :: TypeDefinition
localFunctionHeader = def "LocalFunctionHeader" $ T.record [
  "identifier">: csharp "Identifier",
  "typeParameters">: T.optional $ csharp "TypeParameterList",
  "parameters">: csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

localFunctionModifier :: TypeDefinition
localFunctionModifier = def "LocalFunctionModifier" $ T.union [
  "ref">: csharp "RefLocalFunctionModifier",
  "async">: T.unit]

localVariableDeclaration :: TypeDefinition
localVariableDeclaration = def "LocalVariableDeclaration" $ T.union [
  "implicitlyTyped">: csharp "ImplicitlyTypedLocalVariableDeclaration",
  "explicitlyTyped">: csharp "ExplicitlyTypedLocalVariableDeclaration",
  "ref">: csharp "RefLocalVariableDeclaration"]

localVariableInitializer :: TypeDefinition
localVariableInitializer = def "LocalVariableInitializer" $ T.union [
  "expression">: csharp "Expression",
  "initializer">: csharp "ArrayInitializer"]

localVariableType :: TypeDefinition
localVariableType = def "LocalVariableType" $ T.union [
  "type">: csharp "Type",
  "var">: T.unit]

lockStatement :: TypeDefinition
lockStatement = def "LockStatement" $ T.record [
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

memberAccess :: TypeDefinition
memberAccess = def "MemberAccess" $ T.record [
  "head">: csharp "MemberAccessHead",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

memberAccessHead :: TypeDefinition
memberAccessHead = def "MemberAccessHead" $ T.union [
  "primary">: csharp "PrimaryExpression",
  "predefined">: csharp "PredefinedType",
  "qualifiedAlias">: csharp "QualifiedAliasMember"]

memberDeclarator :: TypeDefinition
memberDeclarator = def "MemberDeclarator" $ T.union [
  "name">: csharp "SimpleName",
  "memberAccess">: csharp "MemberAccess",
  "nullConditionalProjectionInitializer">: csharp "NullConditionalProjectionInitializer",
  "baseAccess">: csharp "BaseAccess",
  "assignment">: csharp "AssignmentMemberDeclarator"]

memberDeclaratorList :: TypeDefinition
memberDeclaratorList = def "MemberDeclaratorList" $ T.wrap $ nonemptyList $ csharp "MemberDeclarator"

memberInitializer :: TypeDefinition
memberInitializer = def "MemberInitializer" $ T.record [
  "target">: csharp "InitializerTarget",
  "value">: csharp "InitializerValue"]

memberName :: TypeDefinition
memberName = def "MemberName" $ T.record [
  "interfaceType">: T.optional $ csharp "TypeName",
  "identifier">: csharp "Identifier"]

methodBody :: TypeDefinition
methodBody = def "MethodBody" $ T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "empty">: T.unit]

methodDeclaration :: TypeDefinition
methodDeclaration = def "MethodDeclaration" $ T.union [
  "standard">: csharp "StandardMethodDeclaration",
  "refReturn">: csharp "RefReturnMethodDeclaration"]

methodHeader :: TypeDefinition
methodHeader = def "MethodHeader" $ T.record [
  "name">: csharp "MemberName",
  "typeParameters">: T.optional $ csharp "TypeParameterList",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

methodModifier :: TypeDefinition
methodModifier = def "MethodModifier" $ T.union [
  "ref">: csharp "RefMethodModifier",
  "async">: T.unit]

methodModifiers :: TypeDefinition
methodModifiers = def "MethodModifiers" $ T.record [
  "modifiers">: T.list $ csharp "MethodModifier",
  "partial">: T.boolean]

multiplicativeExpression :: TypeDefinition
multiplicativeExpression = def "MultiplicativeExpression" $ T.union [
  "simple">: csharp "UnaryExpression",
  "binary">: csharp "BinaryMultiplicativeExpression"]

multiplicativeOperator :: TypeDefinition
multiplicativeOperator = def "MultiplicativeOperator" $ T.enum [
  "times",
  "divide",
  "modulo"]

namedArgument :: TypeDefinition
namedArgument = def "NamedArgument" $ T.record [
  "name">: csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

namedArgumentList :: TypeDefinition
namedArgumentList = def "NamedArgumentList" $ T.wrap $ nonemptyList $ csharp "NamedArgument"

namedEntity :: TypeDefinition
namedEntity = def "NamedEntity" $ T.record [
  "target">: csharp "NamedEntityTarget",
  "parts">: T.list $ csharp "NamedEntityPart"]

namedEntityPart :: TypeDefinition
namedEntityPart = def "NamedEntityPart" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

namedEntityTarget :: TypeDefinition
namedEntityTarget = def "NamedEntityTarget" $ T.union [
  "name">: csharp "SimpleName",
  "this">: T.unit,
  "base">: T.unit,
  "predefinedType">: csharp "PredefinedType",
  "qualifiedAliasMember">: csharp "QualifiedAliasMember"]

namespaceBody :: TypeDefinition
namespaceBody = def "NamespaceBody" $ T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

namespaceDeclaration :: TypeDefinition
namespaceDeclaration = def "NamespaceDeclaration" $ T.record [
  "name">: csharp "QualifiedIdentifier",
  "body">: csharp "NamespaceBody"]

namespaceMemberDeclaration :: TypeDefinition
namespaceMemberDeclaration = def "NamespaceMemberDeclaration" $ T.union [
  "namespace">: csharp "NamespaceDeclaration",
  "type">: csharp "TypeDeclaration"]

namespaceName :: TypeDefinition
namespaceName = def "NamespaceName" $ T.wrap $
  csharp "NamespaceOrTypeName"

namespaceOrTypeName :: TypeDefinition
namespaceOrTypeName = def "NamespaceOrTypeName" $ T.union [
  "identifier">: csharp "IdentifierNamespaceOrTypeName",
  "qualified">: csharp "QualifiedNamespaceOrTypeName",
  "alias">: csharp "QualifiedAliasMember"]

nonArrayType :: TypeDefinition
nonArrayType = def "NonArrayType" $ T.union [
  "value">: csharp "ValueType",
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit,
  "parameter">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

nonArrayTypeArrayCreationExpression :: TypeDefinition
nonArrayTypeArrayCreationExpression = def "NonArrayTypeArrayCreationExpression" $ T.record [
  "type">: csharp "NonArrayType",
  "expressions">: csharp "ExpressionList",
  "rankSpecifiers">: T.list $ csharp "RankSpecifier",
  "initializer">: T.optional $ csharp "ArrayInitializer"]

nonAssignmentExpression :: TypeDefinition
nonAssignmentExpression = def "NonAssignmentExpression" $ T.union [
  "declaration">: csharp "DeclarationExpression",
  "conditional">: csharp "ConditionalExpression",
  "lambda">: csharp "LambdaExpression",
  "query">: csharp "QueryExpression"]

nullCoalescingExpression :: TypeDefinition
nullCoalescingExpression = def "NullCoalescingExpression" $ T.union [
  "simple">: csharp "ConditionalOrExpression",
  "binary">: csharp "BinaryNullCoalescingExpression",
  "throw">: csharp "NullCoalescingExpression"]

nullConditionalElementAccess :: TypeDefinition
nullConditionalElementAccess = def "NullConditionalElementAccess" $ T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

nullConditionalInvocationExpression :: TypeDefinition
nullConditionalInvocationExpression = def "NullConditionalInvocationExpression" $ T.record [
  "head">: csharp "NullConditionalInvocationExpressionHead",
  "arguments">: T.optional $ csharp "ArgumentList"]

nullConditionalInvocationExpressionHead :: TypeDefinition
nullConditionalInvocationExpressionHead = def "NullConditionalInvocationExpressionHead" $ T.union [
  "member">: csharp "NullConditionalMemberAccess",
  "element">: csharp "NullConditionalElementAccess"]

nullConditionalMemberAccess :: TypeDefinition
nullConditionalMemberAccess = def "NullConditionalMemberAccess" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

nullConditionalProjectionInitializer :: TypeDefinition
nullConditionalProjectionInitializer = def "NullConditionalProjectionInitializer" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

numericType :: TypeDefinition
numericType = def "NumericType" $ T.union [
  "integral">: csharp "IntegralType",
  "floatingPoint">: csharp "FloatingPointType",
  "decimal">: T.unit]

objectCreationExpression :: TypeDefinition
objectCreationExpression = def "ObjectCreationExpression" $ T.record [
  "type">: csharp "Type",
  "arguments">: T.optional $ csharp "ArgumentList",
  "initializer">: T.optional $ csharp "ObjectOrCollectionInitializer"]

objectOrCollectionInitializer :: TypeDefinition
objectOrCollectionInitializer = def "ObjectOrCollectionInitializer" $ T.union [
  "object">: T.list $ csharp "MemberInitializer",
  "collection">: T.list $ csharp "ElementInitializer"]

operatorBody :: TypeDefinition
operatorBody = def "OperatorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

operatorDeclaration :: TypeDefinition
operatorDeclaration = def "OperatorDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "OperatorModifier",
  "declarator">: csharp "OperatorDeclarator",
  "body">: csharp "OperatorBody"]

operatorDeclarator :: TypeDefinition
operatorDeclarator = def "OperatorDeclarator" $ T.union [
  "unary">: csharp "UnaryOperatorDeclarator",
  "binary">: csharp "BinaryOperatorDeclarator",
  "conversion">: csharp "ConversionOperatorDeclarator"]

operatorModifier :: TypeDefinition
operatorModifier = def "OperatorModifier" $ T.enum [
  "public",
  "static",
  "extern",
  "unsafe"]

ordering :: TypeDefinition
ordering = def "Ordering" $ T.record [
  "expression">: csharp "Expression",
  "direction">: T.optional $ csharp "OrderingDirection"]

orderingDirection :: TypeDefinition
orderingDirection = def "OrderingDirection" $ T.enum [
  "ascending",
  "descending"]

overloadableBinaryOperator :: TypeDefinition
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

overloadableUnaryOperator :: TypeDefinition
overloadableUnaryOperator = def "OverloadableUnaryOperator" $ T.enum [
  "plus",
  "minus",
  "not",
  "complement",
  "increment",
  "decrement",
  "true",
  "false"]

parameterArray :: TypeDefinition
parameterArray = def "ParameterArray" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "type">: csharp "ArrayType",
  "identifier">: csharp "Identifier"]

parameterModeModifier :: TypeDefinition
parameterModeModifier = def "ParameterModeModifier" $ T.enum [
  "ref",
  "out",
  "in"]

parameterModifier :: TypeDefinition
parameterModifier = def "ParameterModifier" $ T.union [
  "mode">: csharp "ParameterModeModifier",
  "this">: T.unit]

pattern_ :: TypeDefinition
pattern_ = def "Pattern" $ T.union [
  "declaration">: csharp "DeclarationPattern",
  "constant">: csharp "Expression",
  "var">: csharp "Designation"]

positionalArgument :: TypeDefinition
positionalArgument = def "PositionalArgument" $ T.record [
  "name">: T.optional $ csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

positionalArgumentList :: TypeDefinition
positionalArgumentList = def "PositionalArgumentList" $ T.wrap $ nonemptyList $ csharp "PositionalArgument"

predefinedType :: TypeDefinition
predefinedType = def "PredefinedType" $ T.enum [
  "bool", "byte", "char", "decimal", "double", "float", "int", "long", "object", "sbyte", "short", "string",
  "uint", "ulong", "ushort"]

primaryConstraint :: TypeDefinition
primaryConstraint = def "PrimaryConstraint" $ T.union [
  "classType">: csharp "ClassType",
  "class">: T.unit,
  "struct">: T.unit,
  "unmanaged">: T.unit]

primaryExpression :: TypeDefinition
primaryExpression = def "PrimaryExpression" $ T.union [
  "noArray">: csharp "PrimaryNoArrayCreationExpression",
  "array">: csharp "ArrayCreationExpression"]

primaryNoArrayCreationExpression :: TypeDefinition
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
  "anonymousObjectCreation">: T.optional $ csharp "MemberDeclaratorList",
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

propertyBody :: TypeDefinition
propertyBody = def "PropertyBody" $ T.union [
  "block">: csharp "BlockPropertyBody",
  "expression">: csharp "Expression"]

propertyDeclaration :: TypeDefinition
propertyDeclaration = def "PropertyDeclaration" $ T.union [
  "standard">: csharp "StandardPropertyDeclaration",
  "refReturn">: csharp "RefReturnPropertyDeclaration"]

propertyModifier :: TypeDefinition
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

qualifiedAliasMember :: TypeDefinition
qualifiedAliasMember = def "QualifiedAliasMember" $ T.record [
  "alias">: csharp "Identifier",
  "member">: csharp "Identifier",
  "arguments">: T.optional $ csharp "TypeArgumentList"]

qualifiedIdentifier :: TypeDefinition
qualifiedIdentifier = def "QualifiedIdentifier" $ T.wrap $
  nonemptyList $ csharp "Identifier"

qualifiedNamespaceOrTypeName :: TypeDefinition
qualifiedNamespaceOrTypeName = def "QualifiedNamespaceOrTypeName" $ T.record [
  "namespaceOrType">: csharp "NamespaceOrTypeName",
  "identifier">: csharp "Identifier",
  "arguments">: T.optional $ csharp "TypeArgumentList"]

queryBody :: TypeDefinition
queryBody = def "QueryBody" $ T.record [
  "clauses">: T.list $ csharp "QueryBodyClause",
  "selectOrGroup">: csharp "SelectOrGroupClause",
  "continuation">: T.optional $ csharp "QueryContinuation"]

queryBodyClause :: TypeDefinition
queryBodyClause = def "QueryBodyClause" $ T.union [
  "from">: csharp "FromClause",
  "let">: csharp "LetClause",
  "where">: csharp "BooleanExpression",
  "join">: csharp "JoinClause",
  "orderby">: nonemptyList $ csharp "Ordering"]

queryContinuation :: TypeDefinition
queryContinuation = def "QueryContinuation" $ T.record [
  "into">: csharp "Identifier",
  "body">: csharp "QueryBody"]

queryExpression :: TypeDefinition
queryExpression = def "QueryExpression" $ T.record [
  "from">: csharp "FromClause",
  "body">: csharp "QueryBody"]

rankSpecifier :: TypeDefinition
rankSpecifier = def "RankSpecifier" $ T.wrap T.int32

rankSpecifierArrayCreationExpression :: TypeDefinition
rankSpecifierArrayCreationExpression = def "RankSpecifierArrayCreationExpression" $ T.record [
  "rankSpecifier">: csharp "RankSpecifier",
  "initializer">: csharp "ArrayInitializer"]

refAccessorBody :: TypeDefinition
refAccessorBody = def "RefAccessorBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

refConditionalExpression :: TypeDefinition
refConditionalExpression = def "RefConditionalExpression" $ T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "VariableReference",
  "false">: csharp "VariableReference"]

refGetAccessorDeclaration :: TypeDefinition
refGetAccessorDeclaration = def "RefGetAccessorDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifier">: T.optional $ csharp "AccessorModifier",
  "body">: csharp "RefAccessorBody"]

refIndexerBody :: TypeDefinition
refIndexerBody = def "RefIndexerBody" $ T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

refIndexerDeclaration :: TypeDefinition
refIndexerDeclaration = def "RefIndexerDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "refKind">: csharp "RefKind",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "RefIndexerBody"]

refKind :: TypeDefinition
refKind = def "RefKind" $ T.enum [
  "ref",
  "refReadonly"]

refLocalFunctionBody :: TypeDefinition
refLocalFunctionBody = def "RefLocalFunctionBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference"]

refLocalFunctionDeclaration :: TypeDefinition
refLocalFunctionDeclaration = def "RefLocalFunctionDeclaration" $ T.record [
  "modifiers">: T.list $ csharp "RefLocalFunctionModifier",
  "refKind">: csharp "RefKind",
  "returnType">: csharp "Type",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "RefLocalFunctionBody"]

refLocalFunctionModifier :: TypeDefinition
refLocalFunctionModifier = def "RefLocalFunctionModifier" $ T.enum [
  "static",
  "unsafe"]

refLocalVariableDeclaration :: TypeDefinition
refLocalVariableDeclaration = def "RefLocalVariableDeclaration" $ T.record [
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "RefLocalVariableDeclarator"]

refLocalVariableDeclarator :: TypeDefinition
refLocalVariableDeclarator = def "RefLocalVariableDeclarator" $ T.record [
  "left">: csharp "Identifier",
  "right">: csharp "VariableReference"]

refMethodBody :: TypeDefinition
refMethodBody = def "RefMethodBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

refMethodModifier :: TypeDefinition
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

refPropertyBody :: TypeDefinition
refPropertyBody = def "RefPropertyBody" $ T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

refReturnMethodDeclaration :: TypeDefinition
refReturnMethodDeclaration = def "RefReturnMethodDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "RefMethodModifier",
  "kind">: csharp "RefKind",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "RefMethodBody"]

refReturnPropertyDeclaration :: TypeDefinition
refReturnPropertyDeclaration = def "RefReturnPropertyDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "RefPropertyBody"]

refVarImplicitlyTypedLocalVariableDeclaration :: TypeDefinition
refVarImplicitlyTypedLocalVariableDeclaration = def "RefVarImplicitlyTypedLocalVariableDeclaration" $ T.record [
  "refKind">: csharp "RefKind",
  "declarator">: csharp "RefLocalVariableDeclarator"]

referenceType :: TypeDefinition
referenceType = def "ReferenceType" $ T.union [
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "array">: csharp "ArrayType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit]

regularInterpolation :: TypeDefinition
regularInterpolation = def "RegularInterpolation" $ T.record [
  "expression">: csharp "Expression",
  "width">: T.optional $ csharp "Expression",
  "format">: T.optional T.string]

relationalExpression :: TypeDefinition
relationalExpression = def "RelationalExpression" $ T.union [
  "simple">: csharp "ShiftExpression",
  "binary">: csharp "BinaryRelationalExpression",
  "isType">: csharp "IsTypeExpression",
  "isPattern">: csharp "IsPatternExpression",
  "asType">: csharp "AsTypeExpression"]

relationalOperator :: TypeDefinition
relationalOperator = def "RelationalOperator" $ T.enum [
  "lessThan", "greaterThan", "lessThanOrEqual", "greaterThanOrEqual"]

resourceAcquisition :: TypeDefinition
resourceAcquisition = def "ResourceAcquisition" $ T.union [
  "local">: csharp "LocalVariableDeclaration",
  "expression">: csharp "Expression"]

returnStatement :: TypeDefinition
returnStatement = def "ReturnStatement" $ T.union [
  "simple">: T.unit,
  "value">: csharp "Expression",
  "ref">: csharp "VariableReference"]

returnType :: TypeDefinition
returnType = def "ReturnType" $ T.union [
  "ref">: csharp "Type",
  "void">: T.unit]

secondaryConstraint :: TypeDefinition
secondaryConstraint = def "SecondaryConstraint" $ T.union [
  "interface">: csharp "InterfaceType",
  "parameter">: csharp "TypeParameter"]

secondaryConstraints :: TypeDefinition
secondaryConstraints = def "SecondaryConstraints" $ T.wrap $ nonemptyList $ csharp "SecondaryConstraint"

selectOrGroupClause :: TypeDefinition
selectOrGroupClause = def "SelectOrGroupClause" $ T.union [
  "select">: csharp "Expression",
  "group">: csharp "GroupClause"]

selectionStatement :: TypeDefinition
selectionStatement = def "SelectionStatement" $ T.union [
  "if">: csharp "IfStatement",
  "switch">: csharp "SwitchStatement"]

shiftExpression :: TypeDefinition
shiftExpression = def "ShiftExpression" $ T.union [
  "simple">: csharp "AdditiveExpression",
  "binary">: csharp "BinaryShiftExpression"]

shiftOperator :: TypeDefinition
shiftOperator = def "ShiftOperator" $ T.enum [
  "left",
  "right"]

simpleConditionalExpression :: TypeDefinition
simpleConditionalExpression = def "SimpleConditionalExpression" $ T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "Expression",
  "false">: csharp "Expression"]

simpleName :: TypeDefinition
simpleName = def "SimpleName" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

simpleType :: TypeDefinition
simpleType = def "SimpleType" $ T.union [
  "numeric">: csharp "NumericType",
  "bool">: T.unit]

specificCatchClause :: TypeDefinition
specificCatchClause = def "SpecificCatchClause" $ T.record [
  "specifier">: T.optional $ csharp "ExceptionSpecifier",
  "filter">: T.optional $ csharp "BooleanExpression",
  "body">: csharp "Block"]

stackallocExpression :: TypeDefinition
stackallocExpression = def "StackallocExpression" $ T.record [
  "type">: T.optional $ csharp "UnmanagedType",
  "expression">: T.optional $ csharp "ConstantExpression",
  "initializer">: T.list $ csharp "Expression"]

standardEventDeclaration :: TypeDefinition
standardEventDeclaration = def "StandardEventDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "declarators">: csharp "VariableDeclarators"]

standardIndexerDeclaration :: TypeDefinition
standardIndexerDeclaration = def "StandardIndexerDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "IndexerBody"]

standardLocalFunctionDeclaration :: TypeDefinition
standardLocalFunctionDeclaration = def "StandardLocalFunctionDeclaration" $ T.record [
  "modifiers">: T.list $ csharp "LocalFunctionModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "LocalFunctionBody"]

standardMethodDeclaration :: TypeDefinition
standardMethodDeclaration = def "StandardMethodDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "MethodModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "MethodBody"]

standardPropertyDeclaration :: TypeDefinition
standardPropertyDeclaration = def "StandardPropertyDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "PropertyBody"]

statement :: TypeDefinition
statement = def "Statement" $ T.union [
  "labeled">: csharp "LabeledStatement",
  "declaration">: csharp "DeclarationStatement",
  "embedded">: csharp "EmbeddedStatement"]

statementExpression :: TypeDefinition
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

statementExpressionList :: TypeDefinition
statementExpressionList = def "StatementExpressionList" $ T.wrap $ nonemptyList $ csharp "StatementExpression"

staticConstructorBody :: TypeDefinition
staticConstructorBody = def "StaticConstructorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

staticConstructorDeclaration :: TypeDefinition
staticConstructorDeclaration = def "StaticConstructorDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: csharp "StaticConstructorModifiers",
  "name">: csharp "Identifier",
  "body">: csharp "StaticConstructorBody"]

staticConstructorModifiers :: TypeDefinition
staticConstructorModifiers = def "StaticConstructorModifiers" $ T.record [
  "extern">: T.boolean,
  "unsafe">: T.boolean]

structDeclaration :: TypeDefinition
structDeclaration = def "StructDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "StructModifier",
  "ref">: T.boolean,
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "TypeParameterList",
  "interfaces">: T.list $ csharp "InterfaceType",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: T.list $ csharp "StructMemberDeclaration"]

structMemberDeclaration :: TypeDefinition
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

structModifier :: TypeDefinition
structModifier = def "StructModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "readonly",
  "unsafe"]

structOrEnumType :: TypeDefinition
structOrEnumType = def "StructOrEnumType" $ T.union [
  "struct">: csharp "StructType",
  "enum">: csharp "EnumType"]

structType :: TypeDefinition
structType = def "StructType" $ T.union [
  "typeName">: csharp "TypeName",
  "simple">: csharp "SimpleType",
  "tuple">: csharp "TupleType"]

switchBranch :: TypeDefinition
switchBranch = def "SwitchBranch" $ T.record [
  "pattern">: csharp "Pattern",
  "guard">: T.optional $ csharp "Expression"]

switchLabel :: TypeDefinition
switchLabel = def "SwitchLabel" $ T.union [
  "branch">: csharp "SwitchBranch",
  "default">: T.unit]

switchSection :: TypeDefinition
switchSection = def "SwitchSection" $ T.record [
  "labels">: nonemptyList $ csharp "SwitchLabel",
  "statements">: T.list $ csharp "Statement"]

switchStatement :: TypeDefinition
switchStatement = def "SwitchStatement" $ T.record [
  "expression">: csharp "Expression",
  "branches">: T.list $ csharp "SwitchSection"]

tryStatement :: TypeDefinition
tryStatement = def "TryStatement" $ T.record [
  "body">: csharp "Block",
  "catches">: csharp "CatchClauses",
  "finally">: T.optional $ csharp "Block"]

tupleElement :: TypeDefinition
tupleElement = def "TupleElement" $ T.record [
  "name">: T.optional $ csharp "Identifier",
  "expression">: csharp "Expression"]

tupleExpression :: TypeDefinition
tupleExpression = def "TupleExpression" $ T.union [
  "elements">: nonemptyList $ csharp "TupleElement",
  "deconstruction">: csharp "DeconstructionTuple"]

tupleType :: TypeDefinition
tupleType = def "TupleType" $ T.wrap $
  nonemptyList $ csharp "TupleTypeElement"

tupleTypeElement :: TypeDefinition
tupleTypeElement = def "TupleTypeElement" $ T.record [
  "type">: csharp "Type",
  "identifier">: T.optional $ csharp "Identifier"]

typeArgumentList :: TypeDefinition
typeArgumentList = def "TypeArgumentList" $ T.wrap $ T.list $ csharp "Type"

typeDeclaration :: TypeDefinition
typeDeclaration = def "TypeDeclaration" $ T.union [
  "class">: csharp "ClassDeclaration",
  "struct">: csharp "StructDeclaration",
  "interface">: csharp "InterfaceDeclaration",
  "enum">: csharp "EnumDeclaration",
  "delegate">: csharp "DelegateDeclaration"]

typeName :: TypeDefinition
typeName = def "TypeName" $ T.wrap $
  csharp "NamespaceOrTypeName"

typeParameter :: TypeDefinition
typeParameter = def "TypeParameter" $ T.wrap $
  csharp "Identifier"

typeParameterConstraints :: TypeDefinition
typeParameterConstraints = def "TypeParameterConstraints" $ T.record [
  "primary">: T.optional $ csharp "PrimaryConstraint",
  "secondary">: T.optional $ csharp "SecondaryConstraints",
  "constructor">: T.boolean]

typeParameterConstraintsClause :: TypeDefinition
typeParameterConstraintsClause = def "TypeParameterConstraintsClause" $ T.record [
  "parameter">: csharp "TypeParameter",
  "constraints">: T.list $ csharp "TypeParameterConstraints"]

typeParameterList :: TypeDefinition
typeParameterList = def "TypeParameterList" $ T.wrap $ nonemptyList $ csharp "TypeParameterPart"

typeParameterPart :: TypeDefinition
typeParameterPart = def "TypeParameterPart" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "name">: csharp "TypeParameter"]

type_ :: TypeDefinition
type_ = def "Type" $ T.union [
  "reference">: csharp "ReferenceType",
  "value">: csharp "ValueType",
  "param">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

typeofExpression :: TypeDefinition
typeofExpression = def "TypeofExpression" $ T.union [
  "type">: csharp "Type",
  "unboundTypeName">: csharp "UnboundTypeName",
  "void">: T.unit]

unaryExpression :: TypeDefinition
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

unaryOperatorDeclarator :: TypeDefinition
unaryOperatorDeclarator = def "UnaryOperatorDeclarator" $ T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableUnaryOperator",
  "parameter">: csharp "FixedParameter"]

unboundTypeName :: TypeDefinition
unboundTypeName = def "UnboundTypeName" $ T.wrap $
  nonemptyList $ csharp "UnboundTypeNamePart"

unboundTypeNamePart :: TypeDefinition
unboundTypeNamePart = def "UnboundTypeNamePart" $ T.record [
  "identifier">: csharp "Identifier",
  "aliased">: T.boolean,
  "dimension">: T.optional T.int32]

unmanagedType :: TypeDefinition
unmanagedType = def "UnmanagedType" $ T.union [
  "value">: csharp "ValueType",
  "pointer">: csharp "PointerType"]

usingAliasDirective :: TypeDefinition
usingAliasDirective = def "UsingAliasDirective" $ T.record [
  "alias">: csharp "Identifier",
  "name">: csharp "NamespaceOrTypeName"]

usingDirective :: TypeDefinition
usingDirective = def "UsingDirective" $ T.union [
  "alias">: csharp "UsingAliasDirective",
  "namespace">: csharp "NamespaceName",
  "static">: csharp "TypeName"]

usingStatement :: TypeDefinition
usingStatement = def "UsingStatement" $ T.record [
  "acquisition">: csharp "ResourceAcquisition",
  "body">: csharp "EmbeddedStatement"]

valueType :: TypeDefinition
valueType = def "ValueType" $ T.union [
  "nonNullable">: csharp "StructOrEnumType",
  "nullable">: csharp "StructOrEnumType"]

variableDeclarator :: TypeDefinition
variableDeclarator = def "VariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.optional $ csharp "VariableInitializer"]

variableDeclarators :: TypeDefinition
variableDeclarators = def "VariableDeclarators" $ T.wrap $ nonemptyList $ csharp "VariableDeclarator"

variableInitializer :: TypeDefinition
variableInitializer = def "VariableInitializer" $ T.union [
  "expression">: csharp "Expression",
  "array">: csharp "ArrayInitializer"]

variableReference :: TypeDefinition
variableReference = def "VariableReference" $ T.wrap $
  csharp "Expression"

varianceAnnotation :: TypeDefinition
varianceAnnotation = def "VarianceAnnotation" $ T.enum [
  "in",
  "out"]

variantTypeParameter :: TypeDefinition
variantTypeParameter = def "VariantTypeParameter" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "variance">: T.optional $ csharp "VarianceAnnotation",
  "parameter">: csharp "TypeParameter"]

variantTypeParameters :: TypeDefinition
variantTypeParameters = def "VariantTypeParameters" $ T.wrap $ T.list $ csharp "VariantTypeParameter"

verbatimInterpolation :: TypeDefinition
verbatimInterpolation = def "VerbatimInterpolation" $ T.record [
  "expression">: csharp "Expression",
  "width">: T.optional $ csharp "ConstantExpression",
  "format">: T.optional T.string]

whileStatement :: TypeDefinition
whileStatement = def "WhileStatement" $ T.record [
  "condition">: csharp "BooleanExpression",
  "body">: csharp "EmbeddedStatement"]

yieldStatement :: TypeDefinition
yieldStatement = def "YieldStatement" $ T.union [
  "return">: csharp "Expression",
  "break">: T.unit]

-- Unsafe Elements

fixedPointerDeclarator :: TypeDefinition
fixedPointerDeclarator = def "FixedPointerDeclarator" $ T.union [
  "reference">: csharp "VariableReference",
  "expression">: csharp "Expression"]

fixedSizeBufferDeclaration :: TypeDefinition
fixedSizeBufferDeclaration = def "FixedSizeBufferDeclaration" $ T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: nonemptyList $ csharp "FixedSizeBufferModifier",
  "elementType">: csharp "Type",
  "declarators">: nonemptyList $ csharp "FixedSizeBufferDeclarator"]

fixedSizeBufferDeclarator :: TypeDefinition
fixedSizeBufferDeclarator = def "FixedSizeBufferDeclarator" $ T.record [
  "name">: csharp "Identifier",
  "size">: csharp "ConstantExpression"]

fixedSizeBufferModifier :: TypeDefinition
fixedSizeBufferModifier = def "FixedSizeBufferModifier" $ T.enum [
  "new",
  "public",
  "internal",
  "private",
  "unsafe"]

fixedStatement :: TypeDefinition
fixedStatement = def "FixedStatement" $ T.record [
  "pointerType">: csharp "PointerType",
  "declarators">: nonemptyList $ csharp "FixedPointerDeclarator",
  "statement">: csharp "EmbeddedStatement"]

pointerElementAccess :: TypeDefinition
pointerElementAccess = def "PointerElementAccess" $ T.record [
  "pointer">: csharp "PrimaryNoArrayCreationExpression",
  "index">: csharp "Expression"]

pointerMemberAccess :: TypeDefinition
pointerMemberAccess = def "PointerMemberAccess" $ T.record [
  "pointer">: csharp "PrimaryExpression",
  "member">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

pointerType :: TypeDefinition
pointerType = def "PointerType" $ T.union [
  "valueType">: T.optional $ csharp "ValueType",
  "pointerDepth">: T.int32]
