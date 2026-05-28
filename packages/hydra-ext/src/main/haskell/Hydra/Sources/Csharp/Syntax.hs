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
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just ("A C# syntax module based on the ANTLR grammar dated 02/07/2024 and available at:\n"
      ++ "  https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/grammar")}
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


def :: String -> Type -> Binding
def = datatype ns

-- Lexical Elements

identifier :: Binding
identifier = def "Identifier" $ T.wrap T.string

integerLiteral :: Binding
integerLiteral = def "IntegerLiteral" $ T.union [
  "decimal">: T.string,
  "hexadecimal">: T.string,
  "binary">: T.bigint]


keyword :: Binding
keyword = def "Keyword" $ T.wrap T.string

literal :: Binding
literal = def "Literal" $ T.union [
  "boolean">: T.boolean,
  "integer">: csharp "IntegerLiteral",
  "real">: T.float64,
  "character">: T.string,
  "string">: T.string,
  "null">: T.unit]

-- Syntactic Elements

accessorBody :: Binding
accessorBody = def "AccessorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

accessorDeclaration :: Binding
accessorDeclaration = def "AccessorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifier">: T.maybe $ csharp "AccessorModifier",
  "body">: csharp "AccessorBody"]

accessorDeclarations :: Binding
accessorDeclarations = def "AccessorDeclarations" $ T.union [
  "get">: T.maybe $ csharp "AccessorDeclaration",
  "set">: T.maybe $ csharp "AccessorDeclaration"]

accessorModifier :: Binding
accessorModifier = def "AccessorModifier" $ T.enum [
  "protected",
  "internal",
  "private",
  "protectedInternal",
  "internalProtected",
  "protectedPrivate",
  "privateProtected"]

accessorsEventDeclaration :: Binding
accessorsEventDeclaration = def "AccessorsEventDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "accessors">: csharp "EventAccessorDeclarations"]

addRemoveAccessorDeclaration :: Binding
addRemoveAccessorDeclaration = def "AddRemoveAccessorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "body">: csharp "Block"]

additiveExpression :: Binding
additiveExpression = def "AdditiveExpression" $ T.union [
  "simple">: csharp "MultiplicativeExpression",
  "binary">: csharp "BinaryAdditiveExpression"]

additiveOperator :: Binding
additiveOperator = def "AdditiveOperator" $ T.enum [
  "plus",
  "minus"]

andExpression :: Binding
andExpression = def "AndExpression" $ T.union [
  "simple">: csharp "EqualityExpression",
  "binary">: csharp "BinaryAndExpression"]

anonymousFunctionBody :: Binding
anonymousFunctionBody = def "AnonymousFunctionBody" $ T.union [
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "ref">: csharp "VariableReference",
  "block">: csharp "Block"]

anonymousFunctionParameterModifier :: Binding
anonymousFunctionParameterModifier = def "AnonymousFunctionParameterModifier" $ T.enum [
  "ref",
  "out",
  "in"]

anonymousFunctionSignature :: Binding
anonymousFunctionSignature = def "AnonymousFunctionSignature" $ T.union [
  "explicit">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "implicit">: T.list $ csharp "Identifier"]

anonymousMethodExpression :: Binding
anonymousMethodExpression = def "AnonymousMethodExpression" $ T.record [
  "async">: T.boolean,
  "signature">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "body">: csharp "Block"]

argument :: Binding
argument = def "Argument" $ T.record [
  "name">: T.maybe $ csharp "Identifier",
  "value">: csharp "ArgumentValue"]

argumentList :: Binding
argumentList = def "ArgumentList" $ T.wrap $ nonemptyList $ csharp "Argument"

argumentValue :: Binding
argumentValue = def "ArgumentValue" $ T.union [
  "expression">: csharp "Expression",
  "in">: csharp "VariableReference",
  "ref">: csharp "VariableReference",
  "out">: csharp "VariableReference"]

arrayCreationExpression :: Binding
arrayCreationExpression = def "ArrayCreationExpression" $ T.union [
  "nonArrayType">: csharp "NonArrayTypeArrayCreationExpression",
  "arrayType">: csharp "ArrayTypeArrayCreationExpression",
  "rankSpecifier">: csharp "RankSpecifierArrayCreationExpression"]

arrayInitializer :: Binding
arrayInitializer = def "ArrayInitializer" $ T.wrap $
  T.list $ csharp "VariableInitializer"

arrayType :: Binding
arrayType = def "ArrayType" $ T.record [
  "type">: csharp "NonArrayType",
  "rank">: T.list $ csharp "RankSpecifier"]

arrayTypeArrayCreationExpression :: Binding
arrayTypeArrayCreationExpression = def "ArrayTypeArrayCreationExpression" $ T.record [
  "type">: csharp "ArrayType",
  "initializer">: csharp "ArrayInitializer"]

asTypeExpression :: Binding
asTypeExpression = def "AsTypeExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

assignment :: Binding
assignment = def "Assignment" $ T.record [
  "left">: csharp "UnaryExpression",
  "operator">: csharp "AssignmentOperator",
  "right">: csharp "Expression"]

assignmentMemberDeclarator :: Binding
assignmentMemberDeclarator = def "AssignmentMemberDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

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

attribute :: Binding
attribute = def "Attribute" $ T.record [
  "name">: csharp "AttributeName",
  "arguments">: T.maybe $ csharp "AttributeArguments"]

attributeArgumentExpression :: Binding
attributeArgumentExpression = def "AttributeArgumentExpression" $ T.wrap $
  csharp "NonAssignmentExpression"


attributeArguments :: Binding
attributeArguments = def "AttributeArguments" $ T.record [
  "positonal">: T.maybe $ csharp "PositionalArgumentList",
  "named">: T.maybe $ csharp "NamedArgumentList"]

attributeList :: Binding
attributeList = def "AttributeList" $ T.wrap $ nonemptyList $ csharp "Attribute"

attributeName :: Binding
attributeName = def "AttributeName" $ T.wrap $
  csharp "TypeName"

attributeSection :: Binding
attributeSection = def "AttributeSection" $ T.record [
  "target">: T.maybe $ csharp "AttributeTarget",
  "attributes">: csharp "AttributeList"]

attributeTarget :: Binding
attributeTarget = def "AttributeTarget" $ T.union [
  "identifier">: csharp "Identifier",
  "keyword">: csharp "Keyword"]

attributes :: Binding
attributes = def "Attributes" $ T.wrap $ nonemptyList $ csharp "AttributeSection"

baseAccess :: Binding
baseAccess = def "BaseAccess" $ T.union [
  "identifier">: csharp "BaseAccessWithIdentifier",
  "arguments">: csharp "ArgumentList"]

baseAccessWithIdentifier :: Binding
baseAccessWithIdentifier = def "BaseAccessWithIdentifier" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

binaryAdditiveExpression :: Binding
binaryAdditiveExpression = def "BinaryAdditiveExpression" $ T.record [
  "left">: csharp "AdditiveExpression",
  "operator">: csharp "AdditiveOperator",
  "right">: csharp "MultiplicativeExpression"]

binaryAndExpression :: Binding
binaryAndExpression = def "BinaryAndExpression" $ T.record [
  "left">: csharp "AndExpression",
  "right">: csharp "EqualityExpression"]

binaryConditionalAndExpression :: Binding
binaryConditionalAndExpression = def "BinaryConditionalAndExpression" $ T.record [
  "left">: csharp "ConditionalAndExpression",
  "right">: csharp "InclusiveOrExpression"]

binaryConditionalOrExpression :: Binding
binaryConditionalOrExpression = def "BinaryConditionalOrExpression" $ T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "ConditionalAndExpression"]

binaryEqualityExpression :: Binding
binaryEqualityExpression = def "BinaryEqualityExpression" $ T.record [
  "left">: csharp "EqualityExpression",
  "operator">: csharp "EqualityOperator",
  "right">: csharp "RelationalExpression"]

binaryExclusiveOrExpression :: Binding
binaryExclusiveOrExpression = def "BinaryExclusiveOrExpression" $ T.record [
  "left">: csharp "ExclusiveOrExpression",
  "right">: csharp "AndExpression"]

binaryInclusiveOrExpression :: Binding
binaryInclusiveOrExpression = def "BinaryInclusiveOrExpression" $ T.record [
  "left">: csharp "InclusiveOrExpression",
  "right">: csharp "ExclusiveOrExpression"]

binaryMultiplicativeExpression :: Binding
binaryMultiplicativeExpression = def "BinaryMultiplicativeExpression" $ T.record [
  "left">: csharp "MultiplicativeExpression",
  "operator">: csharp "MultiplicativeOperator",
  "right">: csharp "UnaryExpression"]

binaryNullCoalescingExpression :: Binding
binaryNullCoalescingExpression = def "BinaryNullCoalescingExpression" $ T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "NullCoalescingExpression"]

binaryOperatorDeclarator :: Binding
binaryOperatorDeclarator = def "BinaryOperatorDeclarator" $ T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableBinaryOperator",
  "left">: csharp "FixedParameter",
  "right">: csharp "FixedParameter"]

binaryRelationalExpression :: Binding
binaryRelationalExpression = def "BinaryRelationalExpression" $ T.record [
  "left">: csharp "RelationalExpression",
  "operator">: csharp "RelationalOperator",
  "right">: csharp "ShiftExpression"]

binaryShiftExpression :: Binding
binaryShiftExpression = def "BinaryShiftExpression" $ T.record [
  "left">: csharp "ShiftExpression",
  "operator">: csharp "ShiftOperator",
  "right">: csharp "AdditiveExpression"]

block :: Binding
block = def "Block" $ T.wrap $
  T.list $ csharp "Statement"

blockPropertyBody :: Binding
blockPropertyBody = def "BlockPropertyBody" $ T.record [
  "accessors">: csharp "AccessorDeclarations",
  "initializer">: T.maybe $ csharp "VariableInitializer"]

booleanExpression :: Binding
booleanExpression = def "BooleanExpression" $ T.wrap $
  csharp "Expression"

castExpression :: Binding
castExpression = def "CastExpression" $ T.record [
  "type">: csharp "Type",
  "expression">: csharp "UnaryExpression"]

catchClauses :: Binding
catchClauses = def "CatchClauses" $ T.union [
  "specific">: T.list $ csharp "SpecificCatchClause",
  "general">: csharp "Block"]

classBase :: Binding
classBase = def "ClassBase" $ T.union [
  "class">: T.maybe $ csharp "ClassType",
  "interfaces">: T.list $ csharp "InterfaceType"]

classBody :: Binding
classBody = def "ClassBody" $ T.wrap $
  T.list $ csharp "ClassMemberDeclaration"

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

classType :: Binding
classType = def "ClassType" $ T.union [
  "typeName">: csharp "TypeName",
  "object">: T.unit,
  "string">: T.unit]

compilationUnit :: Binding
compilationUnit = def "CompilationUnit" $ T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "attributes">: T.list $ csharp "GlobalAttributeSection",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

conditionalAndExpression :: Binding
conditionalAndExpression = def "ConditionalAndExpression" $ T.union [
  "simple">: csharp "InclusiveOrExpression",
  "binary">: csharp "BinaryConditionalAndExpression"]

conditionalExpression :: Binding
conditionalExpression = def "ConditionalExpression" $ T.union [
  "simple">: csharp "NullCoalescingExpression",
  "simpleConditional">: csharp "SimpleConditionalExpression",
  "refConditional">: csharp "RefConditionalExpression"]

conditionalOrExpression :: Binding
conditionalOrExpression = def "ConditionalOrExpression" $ T.union [
  "simple">: csharp "ConditionalAndExpression",
  "binary">: csharp "BinaryConditionalOrExpression"]

constantDeclaration :: Binding
constantDeclaration = def "ConstantDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstantModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

constantDeclarator :: Binding
constantDeclarator = def "ConstantDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "ConstantExpression"]

constantExpression :: Binding
constantExpression = def "ConstantExpression" $ T.wrap $
  csharp "Expression"

constantModifier :: Binding
constantModifier = def "ConstantModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

constructorBody :: Binding
constructorBody = def "ConstructorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

constructorDeclaration :: Binding
constructorDeclaration = def "ConstructorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstructorModifier",
  "declarator">: csharp "ConstructorDeclarator",
  "body">: csharp "ConstructorBody"]

constructorDeclarator :: Binding
constructorDeclarator = def "ConstructorDeclarator" $ T.record [
  "name">: csharp "Identifier",
  "parameters">: T.maybe $ csharp "FormalParameterList",
  "initializer">: T.maybe $ csharp "ConstructorInitializer"]

constructorInitializer :: Binding
constructorInitializer = def "ConstructorInitializer" $ T.union [
  "base">: T.maybe $ csharp "ArgumentList",
  "this">: T.maybe $ csharp "ArgumentList"]

constructorModifier :: Binding
constructorModifier = def "ConstructorModifier" $ T.enum [
  "public",
  "protected",
  "internal",
  "private",
  "extern",
  "unsafe"]

conversionKind :: Binding
conversionKind = def "ConversionKind" $ T.enum [
  "implicit",
  "explicit"]

conversionOperatorDeclarator :: Binding
conversionOperatorDeclarator = def "ConversionOperatorDeclarator" $ T.record [
  "kind">: csharp "ConversionKind",
  "type">: csharp "Type",
  "parameter">: csharp "FixedParameter"]

declarationExpression :: Binding
declarationExpression = def "DeclarationExpression" $ T.record [
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier"]

declarationPattern :: Binding
declarationPattern = def "DeclarationPattern" $ T.record [
  "type">: csharp "Type",
  "designation">: csharp "Designation"]

declarationStatement :: Binding
declarationStatement = def "DeclarationStatement" $ T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "constant">: csharp "LocalConstantDeclaration",
  "function">: csharp "LocalFunctionDeclaration"]

deconstructionElement :: Binding
deconstructionElement = def "DeconstructionElement" $ T.union [
  "tuple">: csharp "DeconstructionTuple",
  "identifier">: csharp "Identifier"]

deconstructionTuple :: Binding
deconstructionTuple = def "DeconstructionTuple" $ T.wrap $
  nonemptyList $ csharp "DeconstructionElement"

defaultValueExpression :: Binding
defaultValueExpression = def "DefaultValueExpression" $ T.union [
  "explicitlyTyped">: csharp "Type",
  "defaultLiteral">: T.unit]

delegateCreationExpression :: Binding
delegateCreationExpression = def "DelegateCreationExpression" $ T.record [
  "type">: csharp "DelegateType",
  "expression">: csharp "Expression"]

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

delegateType :: Binding
delegateType = def "DelegateType" $ T.wrap $
  csharp "TypeName"

dependentAccess :: Binding
dependentAccess = def "DependentAccess" $ T.union [
  "memberAccess">: csharp "DependentAccessForMember",
  "elementAccess">: csharp "ArgumentList",
  "invocation">: T.maybe $ csharp "ArgumentList"]

dependentAccessForMember :: Binding
dependentAccessForMember = def "DependentAccessForMember" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

designation :: Binding
designation = def "Designation" $ T.wrap $
  csharp "Identifier"

doStatement :: Binding
doStatement = def "DoStatement" $ T.record [
  "body">: csharp "EmbeddedStatement",
  "while">: csharp "BooleanExpression"]

elementAccess :: Binding
elementAccess = def "ElementAccess" $ T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList"]

elementInitializer :: Binding
elementInitializer = def "ElementInitializer" $ T.union [
  "single">: csharp "NonAssignmentExpression",
  "list">: csharp "ExpressionList"]

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

enumBase :: Binding
enumBase = def "EnumBase" $ T.union [
  "type">: csharp "IntegralType",
  "name">: csharp "TypeName"]

enumBody :: Binding
enumBody = def "EnumBody" $ T.wrap $
  T.list $ csharp "EnumMemberDeclaration"

enumDeclaration :: Binding
enumDeclaration = def "EnumDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EnumModifier",
  "name">: csharp "Identifier",
  "base">: T.maybe $ csharp "EnumBase",
  "body">: T.maybe $ csharp "EnumBody"]

enumMemberDeclaration :: Binding
enumMemberDeclaration = def "EnumMemberDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "name">: csharp "Identifier",
  "value">: T.maybe $ csharp "ConstantExpression"]

enumModifier :: Binding
enumModifier = def "EnumModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

enumType :: Binding
enumType = def "EnumType" $ T.wrap $
  csharp "TypeName"

equalityExpression :: Binding
equalityExpression = def "EqualityExpression" $ T.union [
  "simple">: csharp "RelationalExpression",
  "binary">: csharp "BinaryEqualityExpression"]

equalityOperator :: Binding
equalityOperator = def "EqualityOperator" $ T.enum [
  "equal",
  "notEqual"]

eventAccessorDeclarations :: Binding
eventAccessorDeclarations = def "EventAccessorDeclarations" $ T.union [
  "add">: csharp "AddRemoveAccessorDeclaration",
  "remove">: csharp "AddRemoveAccessorDeclaration"]

eventDeclaration :: Binding
eventDeclaration = def "EventDeclaration" $ T.union [
  "standard">: csharp "StandardEventDeclaration",
  "accessors">: csharp "AccessorsEventDeclaration"]

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

exceptionSpecifier :: Binding
exceptionSpecifier = def "ExceptionSpecifier" $ T.record [
  "type">: csharp "Type",
  "identifier">: T.maybe $ csharp "Identifier"]

exclusiveOrExpression :: Binding
exclusiveOrExpression = def "ExclusiveOrExpression" $ T.union [
  "simple">: csharp "AndExpression",
  "binary">: csharp "BinaryExclusiveOrExpression"]

explicitAnonymousFunctionParameter :: Binding
explicitAnonymousFunctionParameter = def "ExplicitAnonymousFunctionParameter" $ T.record [
  "modifier">: T.maybe $ csharp "AnonymousFunctionParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier"]

explicitlyTypedLocalVariableDeclaration :: Binding
explicitlyTypedLocalVariableDeclaration = def "ExplicitlyTypedLocalVariableDeclaration" $ T.record [
  "type">: csharp "Type",
  "declarators">: T.list $ csharp "ExplicitlyTypedLocalVariableDeclarator"]

explicitlyTypedLocalVariableDeclarator :: Binding
explicitlyTypedLocalVariableDeclarator = def "ExplicitlyTypedLocalVariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.maybe $ csharp "LocalVariableInitializer"]

expression :: Binding
expression = def "Expression" $ T.union [
  "nonAssignment">: csharp "NonAssignmentExpression",
  "assignment">: csharp "Assignment"]

expressionList :: Binding
expressionList = def "ExpressionList" $ T.wrap $ nonemptyList $ csharp "Expression"

externAliasDirective :: Binding
externAliasDirective = def "ExternAliasDirective" $ T.wrap $
  csharp "Identifier"

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

finalizerBody :: Binding
finalizerBody = def "FinalizerBody" $ T.union [
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

fixedParameter :: Binding
fixedParameter = def "FixedParameter" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifier">: T.maybe $ csharp "ParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier",
  "defaultArgument">: T.maybe $ csharp "Expression"]

floatingPointType :: Binding
floatingPointType = def "FloatingPointType" $ T.union [
  "float">: T.unit,
  "double">: T.unit]

forInitializer :: Binding
forInitializer = def "ForInitializer" $ T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "statements">: csharp "StatementExpressionList"]

forStatement :: Binding
forStatement = def "ForStatement" $ T.record [
  "initializer">: T.maybe $ csharp "ForInitializer",
  "condition">: T.maybe $ csharp "BooleanExpression",
  "iterator">: T.maybe $ csharp "StatementExpressionList",
  "body">: csharp "EmbeddedStatement"]

foreachStatement :: Binding
foreachStatement = def "ForeachStatement" $ T.record [
  "kind">: T.maybe $ csharp "RefKind",
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

formalParameterList :: Binding
formalParameterList = def "FormalParameterList" $ T.record [
  "fixed">: T.list $ csharp "FixedParameter",
  "array">: T.maybe $ csharp "ParameterArray"]

fromClause :: Binding
fromClause = def "FromClause" $ T.record [
  "type">: T.maybe $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression"]

globalAttributeSection :: Binding
globalAttributeSection = def "GlobalAttributeSection" $ T.record [
  "target">: csharp "Identifier",
  "attributes">: csharp "AttributeList"]

gotoStatement :: Binding
gotoStatement = def "GotoStatement" $ T.union [
  "identifier">: csharp "Identifier",
  "case">: csharp "ConstantExpression",
  "default">: T.unit]

groupClause :: Binding
groupClause = def "GroupClause" $ T.record [
  "grouped">: csharp "Expression",
  "by">: csharp "Expression"]

identifierNamespaceOrTypeName :: Binding
identifierNamespaceOrTypeName = def "IdentifierNamespaceOrTypeName" $ T.record [
  "identifier">: csharp "Identifier",
  "arguments">: T.maybe $ csharp "TypeArgumentList"]

ifStatement :: Binding
ifStatement = def "IfStatement" $ T.record [
  "condition">: csharp "BooleanExpression",
  "ifBranch">: csharp "EmbeddedStatement",
  "elseBranch">: csharp "EmbeddedStatement"]

implicitlyTypedLocalVariableDeclaration :: Binding
implicitlyTypedLocalVariableDeclaration = def "ImplicitlyTypedLocalVariableDeclaration" $ T.union [
  "var">: csharp "ImplicitlyTypedLocalVariableDeclarator",
  "refVar">: csharp "RefVarImplicitlyTypedLocalVariableDeclaration"]

implicitlyTypedLocalVariableDeclarator :: Binding
implicitlyTypedLocalVariableDeclarator = def "ImplicitlyTypedLocalVariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

inclusiveOrExpression :: Binding
inclusiveOrExpression = def "InclusiveOrExpression" $ T.union [
  "simple">: csharp "ExclusiveOrExpression",
  "binary">: csharp "BinaryInclusiveOrExpression"]

indexerBody :: Binding
indexerBody = def "IndexerBody" $ T.union [
  "block">: csharp "AccessorDeclarations",
  "expression">: csharp "Expression"]

indexerDeclaration :: Binding
indexerDeclaration = def "IndexerDeclaration" $ T.union [
  "standard">: csharp "StandardIndexerDeclaration",
  "ref">: csharp "RefIndexerDeclaration"]

indexerDeclarator :: Binding
indexerDeclarator = def "IndexerDeclarator" $ T.record [
  "type">: csharp "Type",
  "interface">: T.maybe $ csharp "InterfaceType",
  "parameters">: csharp "FormalParameterList"]

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

initializerTarget :: Binding
initializerTarget = def "InitializerTarget" $ T.union [
  "identifier">: csharp "Identifier",
  "arguments">: csharp "ArgumentList"]

initializerValue :: Binding
initializerValue = def "InitializerValue" $ T.union [
  "expression">: csharp "Expression",
  "objectOrCollection">: csharp "ObjectOrCollectionInitializer"]

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

interfaceAccessors :: Binding
interfaceAccessors = def "InterfaceAccessors" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "get">: T.maybe $ csharp "Attributes",
  "set">: T.maybe $ csharp "Attributes"]

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

interfaceModifier :: Binding
interfaceModifier = def "InterfaceModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

interfacePropertyDeclaration :: Binding
interfacePropertyDeclaration = def "InterfacePropertyDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.maybe $ csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "Identifier",
  "accessors">: csharp "InterfaceAccessors"]

interfaceType :: Binding
interfaceType = def "InterfaceType" $ T.wrap $
  csharp "TypeName"

interpolatedRegularStringExpression :: Binding
interpolatedRegularStringExpression = def "InterpolatedRegularStringExpression" $ T.wrap T.string

interpolatedStringExpression :: Binding
interpolatedStringExpression = def "InterpolatedStringExpression" $ T.union [
  "regular">: csharp "InterpolatedRegularStringExpression",
  "verbatim">: csharp "InterpolatedVerbatimStringExpression"]

interpolatedVerbatimStringExpression :: Binding
interpolatedVerbatimStringExpression = def "InterpolatedVerbatimStringExpression" $ T.wrap T.string

invocationExpression :: Binding
invocationExpression = def "InvocationExpression" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "arguments">: T.maybe $ csharp "ArgumentList"]

isPatternExpression :: Binding
isPatternExpression = def "IsPatternExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "pattern">: csharp "Pattern"]

isTypeExpression :: Binding
isTypeExpression = def "IsTypeExpression" $ T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

iterationStatement :: Binding
iterationStatement = def "IterationStatement" $ T.union [
  "while">: csharp "WhileStatement",
  "do">: csharp "DoStatement",
  "for">: csharp "ForStatement",
  "foreach">: csharp "ForeachStatement"]

joinClause :: Binding
joinClause = def "JoinClause" $ T.record [
  "type">: T.maybe $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression",
  "on">: csharp "Expression",
  "equals">: csharp "Expression",
  "into">: T.maybe $ csharp "Identifier"]

jumpStatement :: Binding
jumpStatement = def "JumpStatement" $ T.union [
  "break">: T.unit,
  "continue">: T.unit,
  "goto">: csharp "GotoStatement",
  "return">: csharp "ReturnStatement",
  "throw">: T.maybe $ csharp "Expression"]

labeledStatement :: Binding
labeledStatement = def "LabeledStatement" $ T.record [
  "label">: csharp "Identifier",
  "statement">: csharp "Statement"]

lambdaExpression :: Binding
lambdaExpression = def "LambdaExpression" $ T.record [
  "async">: T.boolean,
  "signature">: csharp "AnonymousFunctionSignature",
  "body">: csharp "AnonymousFunctionBody"]

letClause :: Binding
letClause = def "LetClause" $ T.record [
  "left">: csharp "Identifier",
  "right">: csharp "Expression"]

localConstantDeclaration :: Binding
localConstantDeclaration = def "LocalConstantDeclaration" $ T.record [
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

localFunctionBody :: Binding
localFunctionBody = def "LocalFunctionBody" $ T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression"]

localFunctionDeclaration :: Binding
localFunctionDeclaration = def "LocalFunctionDeclaration" $ T.union [
  "standard">: csharp "StandardLocalFunctionDeclaration",
  "ref">: csharp "RefLocalFunctionDeclaration"]

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

localVariableDeclaration :: Binding
localVariableDeclaration = def "LocalVariableDeclaration" $ T.union [
  "implicitlyTyped">: csharp "ImplicitlyTypedLocalVariableDeclaration",
  "explicitlyTyped">: csharp "ExplicitlyTypedLocalVariableDeclaration",
  "ref">: csharp "RefLocalVariableDeclaration"]

localVariableInitializer :: Binding
localVariableInitializer = def "LocalVariableInitializer" $ T.union [
  "expression">: csharp "Expression",
  "initializer">: csharp "ArrayInitializer"]

localVariableType :: Binding
localVariableType = def "LocalVariableType" $ T.union [
  "type">: csharp "Type",
  "var">: T.unit]

lockStatement :: Binding
lockStatement = def "LockStatement" $ T.record [
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

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

memberDeclarator :: Binding
memberDeclarator = def "MemberDeclarator" $ T.union [
  "name">: csharp "SimpleName",
  "memberAccess">: csharp "MemberAccess",
  "nullConditionalProjectionInitializer">: csharp "NullConditionalProjectionInitializer",
  "baseAccess">: csharp "BaseAccess",
  "assignment">: csharp "AssignmentMemberDeclarator"]

memberDeclaratorList :: Binding
memberDeclaratorList = def "MemberDeclaratorList" $ T.wrap $ nonemptyList $ csharp "MemberDeclarator"

memberInitializer :: Binding
memberInitializer = def "MemberInitializer" $ T.record [
  "target">: csharp "InitializerTarget",
  "value">: csharp "InitializerValue"]

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

methodDeclaration :: Binding
methodDeclaration = def "MethodDeclaration" $ T.union [
  "standard">: csharp "StandardMethodDeclaration",
  "refReturn">: csharp "RefReturnMethodDeclaration"]

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

methodModifiers :: Binding
methodModifiers = def "MethodModifiers" $ T.record [
  "modifiers">: T.list $ csharp "MethodModifier",
  "partial">: T.boolean]

multiplicativeExpression :: Binding
multiplicativeExpression = def "MultiplicativeExpression" $ T.union [
  "simple">: csharp "UnaryExpression",
  "binary">: csharp "BinaryMultiplicativeExpression"]

multiplicativeOperator :: Binding
multiplicativeOperator = def "MultiplicativeOperator" $ T.enum [
  "times",
  "divide",
  "modulo"]

namedArgument :: Binding
namedArgument = def "NamedArgument" $ T.record [
  "name">: csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

namedArgumentList :: Binding
namedArgumentList = def "NamedArgumentList" $ T.wrap $ nonemptyList $ csharp "NamedArgument"

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

namespaceBody :: Binding
namespaceBody = def "NamespaceBody" $ T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

namespaceDeclaration :: Binding
namespaceDeclaration = def "NamespaceDeclaration" $ T.record [
  "name">: csharp "QualifiedIdentifier",
  "body">: csharp "NamespaceBody"]

namespaceMemberDeclaration :: Binding
namespaceMemberDeclaration = def "NamespaceMemberDeclaration" $ T.union [
  "namespace">: csharp "NamespaceDeclaration",
  "type">: csharp "TypeDeclaration"]

namespaceName :: Binding
namespaceName = def "NamespaceName" $ T.wrap $
  csharp "NamespaceOrTypeName"

namespaceOrTypeName :: Binding
namespaceOrTypeName = def "NamespaceOrTypeName" $ T.union [
  "identifier">: csharp "IdentifierNamespaceOrTypeName",
  "qualified">: csharp "QualifiedNamespaceOrTypeName",
  "alias">: csharp "QualifiedAliasMember"]

nonArrayType :: Binding
nonArrayType = def "NonArrayType" $ T.union [
  "value">: csharp "ValueType",
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit,
  "parameter">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

nonArrayTypeArrayCreationExpression :: Binding
nonArrayTypeArrayCreationExpression = def "NonArrayTypeArrayCreationExpression" $ T.record [
  "type">: csharp "NonArrayType",
  "expressions">: csharp "ExpressionList",
  "rankSpecifiers">: T.list $ csharp "RankSpecifier",
  "initializer">: T.maybe $ csharp "ArrayInitializer"]

nonAssignmentExpression :: Binding
nonAssignmentExpression = def "NonAssignmentExpression" $ T.union [
  "declaration">: csharp "DeclarationExpression",
  "conditional">: csharp "ConditionalExpression",
  "lambda">: csharp "LambdaExpression",
  "query">: csharp "QueryExpression"]

nullCoalescingExpression :: Binding
nullCoalescingExpression = def "NullCoalescingExpression" $ T.union [
  "simple">: csharp "ConditionalOrExpression",
  "binary">: csharp "BinaryNullCoalescingExpression",
  "throw">: csharp "NullCoalescingExpression"]

nullConditionalElementAccess :: Binding
nullConditionalElementAccess = def "NullConditionalElementAccess" $ T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

nullConditionalInvocationExpression :: Binding
nullConditionalInvocationExpression = def "NullConditionalInvocationExpression" $ T.record [
  "head">: csharp "NullConditionalInvocationExpressionHead",
  "arguments">: T.maybe $ csharp "ArgumentList"]

nullConditionalInvocationExpressionHead :: Binding
nullConditionalInvocationExpressionHead = def "NullConditionalInvocationExpressionHead" $ T.union [
  "member">: csharp "NullConditionalMemberAccess",
  "element">: csharp "NullConditionalElementAccess"]

nullConditionalMemberAccess :: Binding
nullConditionalMemberAccess = def "NullConditionalMemberAccess" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

nullConditionalProjectionInitializer :: Binding
nullConditionalProjectionInitializer = def "NullConditionalProjectionInitializer" $ T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

numericType :: Binding
numericType = def "NumericType" $ T.union [
  "integral">: csharp "IntegralType",
  "floatingPoint">: csharp "FloatingPointType",
  "decimal">: T.unit]

objectCreationExpression :: Binding
objectCreationExpression = def "ObjectCreationExpression" $ T.record [
  "type">: csharp "Type",
  "arguments">: T.maybe $ csharp "ArgumentList",
  "initializer">: T.maybe $ csharp "ObjectOrCollectionInitializer"]

objectOrCollectionInitializer :: Binding
objectOrCollectionInitializer = def "ObjectOrCollectionInitializer" $ T.union [
  "object">: T.list $ csharp "MemberInitializer",
  "collection">: T.list $ csharp "ElementInitializer"]

operatorBody :: Binding
operatorBody = def "OperatorBody" $ T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

operatorDeclaration :: Binding
operatorDeclaration = def "OperatorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "OperatorModifier",
  "declarator">: csharp "OperatorDeclarator",
  "body">: csharp "OperatorBody"]

operatorDeclarator :: Binding
operatorDeclarator = def "OperatorDeclarator" $ T.union [
  "unary">: csharp "UnaryOperatorDeclarator",
  "binary">: csharp "BinaryOperatorDeclarator",
  "conversion">: csharp "ConversionOperatorDeclarator"]

operatorModifier :: Binding
operatorModifier = def "OperatorModifier" $ T.enum [
  "public",
  "static",
  "extern",
  "unsafe"]

ordering :: Binding
ordering = def "Ordering" $ T.record [
  "expression">: csharp "Expression",
  "direction">: T.maybe $ csharp "OrderingDirection"]

orderingDirection :: Binding
orderingDirection = def "OrderingDirection" $ T.enum [
  "ascending",
  "descending"]

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

parameterArray :: Binding
parameterArray = def "ParameterArray" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "type">: csharp "ArrayType",
  "identifier">: csharp "Identifier"]

parameterModeModifier :: Binding
parameterModeModifier = def "ParameterModeModifier" $ T.enum [
  "ref",
  "out",
  "in"]

parameterModifier :: Binding
parameterModifier = def "ParameterModifier" $ T.union [
  "mode">: csharp "ParameterModeModifier",
  "this">: T.unit]

pattern_ :: Binding
pattern_ = def "Pattern" $ T.union [
  "declaration">: csharp "DeclarationPattern",
  "constant">: csharp "Expression",
  "var">: csharp "Designation"]

positionalArgument :: Binding
positionalArgument = def "PositionalArgument" $ T.record [
  "name">: T.maybe $ csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

positionalArgumentList :: Binding
positionalArgumentList = def "PositionalArgumentList" $ T.wrap $ nonemptyList $ csharp "PositionalArgument"

predefinedType :: Binding
predefinedType = def "PredefinedType" $ T.enum [
  "bool", "byte", "char", "decimal", "double", "float", "int", "long", "object", "sbyte", "short", "string",
  "uint", "ulong", "ushort"]

primaryConstraint :: Binding
primaryConstraint = def "PrimaryConstraint" $ T.union [
  "classType">: csharp "ClassType",
  "class">: T.unit,
  "struct">: T.unit,
  "unmanaged">: T.unit]

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

propertyBody :: Binding
propertyBody = def "PropertyBody" $ T.union [
  "block">: csharp "BlockPropertyBody",
  "expression">: csharp "Expression"]

propertyDeclaration :: Binding
propertyDeclaration = def "PropertyDeclaration" $ T.union [
  "standard">: csharp "StandardPropertyDeclaration",
  "refReturn">: csharp "RefReturnPropertyDeclaration"]

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

qualifiedAliasMember :: Binding
qualifiedAliasMember = def "QualifiedAliasMember" $ T.record [
  "alias">: csharp "Identifier",
  "member">: csharp "Identifier",
  "arguments">: T.maybe $ csharp "TypeArgumentList"]

qualifiedIdentifier :: Binding
qualifiedIdentifier = def "QualifiedIdentifier" $ T.wrap $
  nonemptyList $ csharp "Identifier"

qualifiedNamespaceOrTypeName :: Binding
qualifiedNamespaceOrTypeName = def "QualifiedNamespaceOrTypeName" $ T.record [
  "namespaceOrType">: csharp "NamespaceOrTypeName",
  "identifier">: csharp "Identifier",
  "arguments">: T.maybe $ csharp "TypeArgumentList"]

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

queryContinuation :: Binding
queryContinuation = def "QueryContinuation" $ T.record [
  "into">: csharp "Identifier",
  "body">: csharp "QueryBody"]

queryExpression :: Binding
queryExpression = def "QueryExpression" $ T.record [
  "from">: csharp "FromClause",
  "body">: csharp "QueryBody"]

rankSpecifier :: Binding
rankSpecifier = def "RankSpecifier" $ T.wrap T.int32

rankSpecifierArrayCreationExpression :: Binding
rankSpecifierArrayCreationExpression = def "RankSpecifierArrayCreationExpression" $ T.record [
  "rankSpecifier">: csharp "RankSpecifier",
  "initializer">: csharp "ArrayInitializer"]

refAccessorBody :: Binding
refAccessorBody = def "RefAccessorBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

refConditionalExpression :: Binding
refConditionalExpression = def "RefConditionalExpression" $ T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "VariableReference",
  "false">: csharp "VariableReference"]

refGetAccessorDeclaration :: Binding
refGetAccessorDeclaration = def "RefGetAccessorDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifier">: T.maybe $ csharp "AccessorModifier",
  "body">: csharp "RefAccessorBody"]

refIndexerBody :: Binding
refIndexerBody = def "RefIndexerBody" $ T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

refIndexerDeclaration :: Binding
refIndexerDeclaration = def "RefIndexerDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "refKind">: csharp "RefKind",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "RefIndexerBody"]

refKind :: Binding
refKind = def "RefKind" $ T.enum [
  "ref",
  "refReadonly"]

refLocalFunctionBody :: Binding
refLocalFunctionBody = def "RefLocalFunctionBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference"]

refLocalFunctionDeclaration :: Binding
refLocalFunctionDeclaration = def "RefLocalFunctionDeclaration" $ T.record [
  "modifiers">: T.list $ csharp "RefLocalFunctionModifier",
  "refKind">: csharp "RefKind",
  "returnType">: csharp "Type",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "RefLocalFunctionBody"]

refLocalFunctionModifier :: Binding
refLocalFunctionModifier = def "RefLocalFunctionModifier" $ T.enum [
  "static",
  "unsafe"]

refLocalVariableDeclaration :: Binding
refLocalVariableDeclaration = def "RefLocalVariableDeclaration" $ T.record [
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "RefLocalVariableDeclarator"]

refLocalVariableDeclarator :: Binding
refLocalVariableDeclarator = def "RefLocalVariableDeclarator" $ T.record [
  "left">: csharp "Identifier",
  "right">: csharp "VariableReference"]

refMethodBody :: Binding
refMethodBody = def "RefMethodBody" $ T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

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

refPropertyBody :: Binding
refPropertyBody = def "RefPropertyBody" $ T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

refReturnMethodDeclaration :: Binding
refReturnMethodDeclaration = def "RefReturnMethodDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "RefMethodModifier",
  "kind">: csharp "RefKind",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "RefMethodBody"]

refReturnPropertyDeclaration :: Binding
refReturnPropertyDeclaration = def "RefReturnPropertyDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "RefPropertyBody"]

refVarImplicitlyTypedLocalVariableDeclaration :: Binding
refVarImplicitlyTypedLocalVariableDeclaration = def "RefVarImplicitlyTypedLocalVariableDeclaration" $ T.record [
  "refKind">: csharp "RefKind",
  "declarator">: csharp "RefLocalVariableDeclarator"]

referenceType :: Binding
referenceType = def "ReferenceType" $ T.union [
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "array">: csharp "ArrayType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit]

regularInterpolation :: Binding
regularInterpolation = def "RegularInterpolation" $ T.record [
  "expression">: csharp "Expression",
  "width">: T.maybe $ csharp "Expression",
  "format">: T.maybe T.string]

relationalExpression :: Binding
relationalExpression = def "RelationalExpression" $ T.union [
  "simple">: csharp "ShiftExpression",
  "binary">: csharp "BinaryRelationalExpression",
  "isType">: csharp "IsTypeExpression",
  "isPattern">: csharp "IsPatternExpression",
  "asType">: csharp "AsTypeExpression"]

relationalOperator :: Binding
relationalOperator = def "RelationalOperator" $ T.enum [
  "lessThan", "greaterThan", "lessThanOrEqual", "greaterThanOrEqual"]

resourceAcquisition :: Binding
resourceAcquisition = def "ResourceAcquisition" $ T.union [
  "local">: csharp "LocalVariableDeclaration",
  "expression">: csharp "Expression"]

returnStatement :: Binding
returnStatement = def "ReturnStatement" $ T.union [
  "simple">: T.unit,
  "value">: csharp "Expression",
  "ref">: csharp "VariableReference"]

returnType :: Binding
returnType = def "ReturnType" $ T.union [
  "ref">: csharp "Type",
  "void">: T.unit]

secondaryConstraint :: Binding
secondaryConstraint = def "SecondaryConstraint" $ T.union [
  "interface">: csharp "InterfaceType",
  "parameter">: csharp "TypeParameter"]

secondaryConstraints :: Binding
secondaryConstraints = def "SecondaryConstraints" $ T.wrap $ nonemptyList $ csharp "SecondaryConstraint"

selectOrGroupClause :: Binding
selectOrGroupClause = def "SelectOrGroupClause" $ T.union [
  "select">: csharp "Expression",
  "group">: csharp "GroupClause"]

selectionStatement :: Binding
selectionStatement = def "SelectionStatement" $ T.union [
  "if">: csharp "IfStatement",
  "switch">: csharp "SwitchStatement"]

shiftExpression :: Binding
shiftExpression = def "ShiftExpression" $ T.union [
  "simple">: csharp "AdditiveExpression",
  "binary">: csharp "BinaryShiftExpression"]

shiftOperator :: Binding
shiftOperator = def "ShiftOperator" $ T.enum [
  "left",
  "right"]

simpleConditionalExpression :: Binding
simpleConditionalExpression = def "SimpleConditionalExpression" $ T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "Expression",
  "false">: csharp "Expression"]

simpleName :: Binding
simpleName = def "SimpleName" $ T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

simpleType :: Binding
simpleType = def "SimpleType" $ T.union [
  "numeric">: csharp "NumericType",
  "bool">: T.unit]

specificCatchClause :: Binding
specificCatchClause = def "SpecificCatchClause" $ T.record [
  "specifier">: T.maybe $ csharp "ExceptionSpecifier",
  "filter">: T.maybe $ csharp "BooleanExpression",
  "body">: csharp "Block"]

stackallocExpression :: Binding
stackallocExpression = def "StackallocExpression" $ T.record [
  "type">: T.maybe $ csharp "UnmanagedType",
  "expression">: T.maybe $ csharp "ConstantExpression",
  "initializer">: T.list $ csharp "Expression"]

standardEventDeclaration :: Binding
standardEventDeclaration = def "StandardEventDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "declarators">: csharp "VariableDeclarators"]

standardIndexerDeclaration :: Binding
standardIndexerDeclaration = def "StandardIndexerDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "IndexerBody"]

standardLocalFunctionDeclaration :: Binding
standardLocalFunctionDeclaration = def "StandardLocalFunctionDeclaration" $ T.record [
  "modifiers">: T.list $ csharp "LocalFunctionModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "LocalFunctionBody"]

standardMethodDeclaration :: Binding
standardMethodDeclaration = def "StandardMethodDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "MethodModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "MethodBody"]

standardPropertyDeclaration :: Binding
standardPropertyDeclaration = def "StandardPropertyDeclaration" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "PropertyBody"]

statement :: Binding
statement = def "Statement" $ T.union [
  "labeled">: csharp "LabeledStatement",
  "declaration">: csharp "DeclarationStatement",
  "embedded">: csharp "EmbeddedStatement"]

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

statementExpressionList :: Binding
statementExpressionList = def "StatementExpressionList" $ T.wrap $ nonemptyList $ csharp "StatementExpression"

staticConstructorBody :: Binding
staticConstructorBody = def "StaticConstructorBody" $ T.union [
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

structModifier :: Binding
structModifier = def "StructModifier" $ T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "readonly",
  "unsafe"]

structOrEnumType :: Binding
structOrEnumType = def "StructOrEnumType" $ T.union [
  "struct">: csharp "StructType",
  "enum">: csharp "EnumType"]

structType :: Binding
structType = def "StructType" $ T.union [
  "typeName">: csharp "TypeName",
  "simple">: csharp "SimpleType",
  "tuple">: csharp "TupleType"]

switchBranch :: Binding
switchBranch = def "SwitchBranch" $ T.record [
  "pattern">: csharp "Pattern",
  "guard">: T.maybe $ csharp "Expression"]

switchLabel :: Binding
switchLabel = def "SwitchLabel" $ T.union [
  "branch">: csharp "SwitchBranch",
  "default">: T.unit]

switchSection :: Binding
switchSection = def "SwitchSection" $ T.record [
  "labels">: nonemptyList $ csharp "SwitchLabel",
  "statements">: T.list $ csharp "Statement"]

switchStatement :: Binding
switchStatement = def "SwitchStatement" $ T.record [
  "expression">: csharp "Expression",
  "branches">: T.list $ csharp "SwitchSection"]

tryStatement :: Binding
tryStatement = def "TryStatement" $ T.record [
  "body">: csharp "Block",
  "catches">: csharp "CatchClauses",
  "finally">: T.maybe $ csharp "Block"]

tupleElement :: Binding
tupleElement = def "TupleElement" $ T.record [
  "name">: T.maybe $ csharp "Identifier",
  "expression">: csharp "Expression"]

tupleExpression :: Binding
tupleExpression = def "TupleExpression" $ T.union [
  "elements">: nonemptyList $ csharp "TupleElement",
  "deconstruction">: csharp "DeconstructionTuple"]

tupleType :: Binding
tupleType = def "TupleType" $ T.wrap $
  nonemptyList $ csharp "TupleTypeElement"

tupleTypeElement :: Binding
tupleTypeElement = def "TupleTypeElement" $ T.record [
  "type">: csharp "Type",
  "identifier">: T.maybe $ csharp "Identifier"]

typeArgumentList :: Binding
typeArgumentList = def "TypeArgumentList" $ T.wrap $ T.list $ csharp "Type"

typeDeclaration :: Binding
typeDeclaration = def "TypeDeclaration" $ T.union [
  "class">: csharp "ClassDeclaration",
  "struct">: csharp "StructDeclaration",
  "interface">: csharp "InterfaceDeclaration",
  "enum">: csharp "EnumDeclaration",
  "delegate">: csharp "DelegateDeclaration"]

typeName :: Binding
typeName = def "TypeName" $ T.wrap $
  csharp "NamespaceOrTypeName"

typeParameter :: Binding
typeParameter = def "TypeParameter" $ T.wrap $
  csharp "Identifier"

typeParameterConstraints :: Binding
typeParameterConstraints = def "TypeParameterConstraints" $ T.record [
  "primary">: T.maybe $ csharp "PrimaryConstraint",
  "secondary">: T.maybe $ csharp "SecondaryConstraints",
  "constructor">: T.boolean]

typeParameterConstraintsClause :: Binding
typeParameterConstraintsClause = def "TypeParameterConstraintsClause" $ T.record [
  "parameter">: csharp "TypeParameter",
  "constraints">: T.list $ csharp "TypeParameterConstraints"]

typeParameterList :: Binding
typeParameterList = def "TypeParameterList" $ T.wrap $ nonemptyList $ csharp "TypeParameterPart"

typeParameterPart :: Binding
typeParameterPart = def "TypeParameterPart" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "name">: csharp "TypeParameter"]

type_ :: Binding
type_ = def "Type" $ T.union [
  "reference">: csharp "ReferenceType",
  "value">: csharp "ValueType",
  "param">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

typeofExpression :: Binding
typeofExpression = def "TypeofExpression" $ T.union [
  "type">: csharp "Type",
  "unboundTypeName">: csharp "UnboundTypeName",
  "void">: T.unit]

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

unaryOperatorDeclarator :: Binding
unaryOperatorDeclarator = def "UnaryOperatorDeclarator" $ T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableUnaryOperator",
  "parameter">: csharp "FixedParameter"]

unboundTypeName :: Binding
unboundTypeName = def "UnboundTypeName" $ T.wrap $
  nonemptyList $ csharp "UnboundTypeNamePart"

unboundTypeNamePart :: Binding
unboundTypeNamePart = def "UnboundTypeNamePart" $ T.record [
  "identifier">: csharp "Identifier",
  "aliased">: T.boolean,
  "dimension">: T.maybe T.int32]

unmanagedType :: Binding
unmanagedType = def "UnmanagedType" $ T.union [
  "value">: csharp "ValueType",
  "pointer">: csharp "PointerType"]

usingAliasDirective :: Binding
usingAliasDirective = def "UsingAliasDirective" $ T.record [
  "alias">: csharp "Identifier",
  "name">: csharp "NamespaceOrTypeName"]

usingDirective :: Binding
usingDirective = def "UsingDirective" $ T.union [
  "alias">: csharp "UsingAliasDirective",
  "namespace">: csharp "NamespaceName",
  "static">: csharp "TypeName"]

usingStatement :: Binding
usingStatement = def "UsingStatement" $ T.record [
  "acquisition">: csharp "ResourceAcquisition",
  "body">: csharp "EmbeddedStatement"]

valueType :: Binding
valueType = def "ValueType" $ T.union [
  "nonNullable">: csharp "StructOrEnumType",
  "nullable">: csharp "StructOrEnumType"]

variableDeclarator :: Binding
variableDeclarator = def "VariableDeclarator" $ T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.maybe $ csharp "VariableInitializer"]

variableDeclarators :: Binding
variableDeclarators = def "VariableDeclarators" $ T.wrap $ nonemptyList $ csharp "VariableDeclarator"

variableInitializer :: Binding
variableInitializer = def "VariableInitializer" $ T.union [
  "expression">: csharp "Expression",
  "array">: csharp "ArrayInitializer"]

variableReference :: Binding
variableReference = def "VariableReference" $ T.wrap $
  csharp "Expression"

varianceAnnotation :: Binding
varianceAnnotation = def "VarianceAnnotation" $ T.enum [
  "in",
  "out"]

variantTypeParameter :: Binding
variantTypeParameter = def "VariantTypeParameter" $ T.record [
  "attributes">: T.maybe $ csharp "Attributes",
  "variance">: T.maybe $ csharp "VarianceAnnotation",
  "parameter">: csharp "TypeParameter"]

variantTypeParameters :: Binding
variantTypeParameters = def "VariantTypeParameters" $ T.wrap $ T.list $ csharp "VariantTypeParameter"

verbatimInterpolation :: Binding
verbatimInterpolation = def "VerbatimInterpolation" $ T.record [
  "expression">: csharp "Expression",
  "width">: T.maybe $ csharp "ConstantExpression",
  "format">: T.maybe T.string]

whileStatement :: Binding
whileStatement = def "WhileStatement" $ T.record [
  "condition">: csharp "BooleanExpression",
  "body">: csharp "EmbeddedStatement"]

yieldStatement :: Binding
yieldStatement = def "YieldStatement" $ T.union [
  "return">: csharp "Expression",
  "break">: T.unit]

-- Unsafe Elements

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

fixedSizeBufferDeclarator :: Binding
fixedSizeBufferDeclarator = def "FixedSizeBufferDeclarator" $ T.record [
  "name">: csharp "Identifier",
  "size">: csharp "ConstantExpression"]

fixedSizeBufferModifier :: Binding
fixedSizeBufferModifier = def "FixedSizeBufferModifier" $ T.enum [
  "new",
  "public",
  "internal",
  "private",
  "unsafe"]

fixedStatement :: Binding
fixedStatement = def "FixedStatement" $ T.record [
  "pointerType">: csharp "PointerType",
  "declarators">: nonemptyList $ csharp "FixedPointerDeclarator",
  "statement">: csharp "EmbeddedStatement"]

pointerElementAccess :: Binding
pointerElementAccess = def "PointerElementAccess" $ T.record [
  "pointer">: csharp "PrimaryNoArrayCreationExpression",
  "index">: csharp "Expression"]

pointerMemberAccess :: Binding
pointerMemberAccess = def "PointerMemberAccess" $ T.record [
  "pointer">: csharp "PrimaryExpression",
  "member">: csharp "Identifier",
  "typeArguments">: T.maybe $ csharp "TypeArgumentList"]

pointerType :: Binding
pointerType = def "PointerType" $ T.union [
  "valueType">: T.maybe $ csharp "ValueType",
  "pointerDepth">: T.int32]
