module Hydra.Sources.Csharp.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel hiding (typeParameterConstraints)
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
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
    definitions = [
      accessorBody,
      accessorDeclaration,
      accessorDeclarations,
      accessorModifier,
      accessorsEventDeclaration,
      addRemoveAccessorDeclaration,
      additiveExpression,
      additiveOperator,
      andExpression,
      anonymousFunctionBody,
      anonymousFunctionParameterModifier,
      anonymousFunctionSignature,
      anonymousMethodExpression,
      argument,
      argumentList,
      argumentValue,
      arrayCreationExpression,
      arrayInitializer,
      arrayType,
      arrayTypeArrayCreationExpression,
      asTypeExpression,
      assignment,
      assignmentMemberDeclarator,
      assignmentOperator,
      attribute,
      attributeArgumentExpression,
      attributeArguments,
      attributeList,
      attributeName,
      attributeSection,
      attributeTarget,
      attributes,
      baseAccess,
      baseAccessWithIdentifier,
      binaryAdditiveExpression,
      binaryAndExpression,
      binaryConditionalAndExpression,
      binaryConditionalOrExpression,
      binaryEqualityExpression,
      binaryExclusiveOrExpression,
      binaryInclusiveOrExpression,
      binaryMultiplicativeExpression,
      binaryNullCoalescingExpression,
      binaryOperatorDeclarator,
      binaryRelationalExpression,
      binaryShiftExpression,
      block,
      blockPropertyBody,
      booleanExpression,
      castExpression,
      catchClauses,
      classBase,
      classBody,
      classDeclaration,
      classMemberDeclaration,
      classModifier,
      classType,
      compilationUnit,
      conditionalAndExpression,
      conditionalExpression,
      conditionalOrExpression,
      constantDeclaration,
      constantDeclarator,
      constantExpression,
      constantModifier,
      constructorBody,
      constructorDeclaration,
      constructorDeclarator,
      constructorInitializer,
      constructorModifier,
      conversionKind,
      conversionOperatorDeclarator,
      declarationExpression,
      declarationPattern,
      declarationStatement,
      deconstructionElement,
      deconstructionTuple,
      defaultValueExpression,
      delegateCreationExpression,
      delegateDeclaration,
      delegateHeader,
      delegateModifier,
      delegateType,
      dependentAccess,
      dependentAccessForMember,
      designation,
      doStatement,
      elementAccess,
      elementInitializer,
      embeddedStatement,
      enumBase,
      enumBody,
      enumDeclaration,
      enumMemberDeclaration,
      enumModifier,
      enumType,
      equalityExpression,
      equalityOperator,
      eventAccessorDeclarations,
      eventDeclaration,
      eventModifier,
      exceptionSpecifier,
      exclusiveOrExpression,
      explicitAnonymousFunctionParameter,
      explicitlyTypedLocalVariableDeclaration,
      explicitlyTypedLocalVariableDeclarator,
      expression,
      expressionList,
      externAliasDirective,
      fieldDeclaration,
      fieldModifier,
      finalizerBody,
      finalizerDeclaration,
      fixedParameter,
      fixedPointerDeclarator,
      fixedSizeBufferDeclaration,
      fixedSizeBufferDeclarator,
      fixedSizeBufferModifier,
      fixedStatement,
      floatingPointType,
      forInitializer,
      forStatement,
      foreachStatement,
      formalParameterList,
      fromClause,
      globalAttributeSection,
      gotoStatement,
      groupClause,
      identifier,
      identifierNamespaceOrTypeName,
      ifStatement,
      implicitlyTypedLocalVariableDeclaration,
      implicitlyTypedLocalVariableDeclarator,
      inclusiveOrExpression,
      indexerBody,
      indexerDeclaration,
      indexerDeclarator,
      indexerModifier,
      initializerTarget,
      initializerValue,
      integerLiteral,
      integralType,
      interfaceAccessors,
      interfaceDeclaration,
      interfaceEventDeclaration,
      interfaceIndexerDeclaration,
      interfaceMemberDeclaration,
      interfaceMethodDeclaration,
      interfaceMethodHeader,
      interfaceModifier,
      interfacePropertyDeclaration,
      interfaceType,
      interpolatedRegularStringExpression,
      interpolatedStringExpression,
      interpolatedVerbatimStringExpression,
      invocationExpression,
      isPatternExpression,
      isTypeExpression,
      iterationStatement,
      joinClause,
      jumpStatement,
      keyword,
      labeledStatement,
      lambdaExpression,
      letClause,
      literal,
      localConstantDeclaration,
      localFunctionBody,
      localFunctionDeclaration,
      localFunctionHeader,
      localFunctionModifier,
      localVariableDeclaration,
      localVariableInitializer,
      localVariableType,
      lockStatement,
      memberAccess,
      memberAccessHead,
      memberDeclarator,
      memberDeclaratorList,
      memberInitializer,
      memberName,
      methodBody,
      methodDeclaration,
      methodHeader,
      methodModifier,
      methodModifiers,
      multiplicativeExpression,
      multiplicativeOperator,
      namedArgument,
      namedArgumentList,
      namedEntity,
      namedEntityPart,
      namedEntityTarget,
      namespaceBody,
      namespaceDeclaration,
      namespaceMemberDeclaration,
      namespaceName,
      namespaceOrTypeName,
      nonArrayType,
      nonArrayTypeArrayCreationExpression,
      nonAssignmentExpression,
      nullCoalescingExpression,
      nullConditionalElementAccess,
      nullConditionalInvocationExpression,
      nullConditionalInvocationExpressionHead,
      nullConditionalMemberAccess,
      nullConditionalProjectionInitializer,
      numericType,
      objectCreationExpression,
      objectOrCollectionInitializer,
      operatorBody,
      operatorDeclaration,
      operatorDeclarator,
      operatorModifier,
      ordering,
      orderingDirection,
      overloadableBinaryOperator,
      overloadableUnaryOperator,
      parameterArray,
      parameterModeModifier,
      parameterModifier,
      pattern_,
      pointerElementAccess,
      pointerMemberAccess,
      pointerType,
      positionalArgument,
      positionalArgumentList,
      predefinedType,
      primaryConstraint,
      primaryExpression,
      primaryNoArrayCreationExpression,
      propertyBody,
      propertyDeclaration,
      propertyModifier,
      qualifiedAliasMember,
      qualifiedIdentifier,
      qualifiedNamespaceOrTypeName,
      queryBody,
      queryBodyClause,
      queryContinuation,
      queryExpression,
      rankSpecifier,
      rankSpecifierArrayCreationExpression,
      refAccessorBody,
      refConditionalExpression,
      refGetAccessorDeclaration,
      refIndexerBody,
      refIndexerDeclaration,
      refKind,
      refLocalFunctionBody,
      refLocalFunctionDeclaration,
      refLocalFunctionModifier,
      refLocalVariableDeclaration,
      refLocalVariableDeclarator,
      refMethodBody,
      refMethodModifier,
      refPropertyBody,
      refReturnMethodDeclaration,
      refReturnPropertyDeclaration,
      refVarImplicitlyTypedLocalVariableDeclaration,
      referenceType,
      regularInterpolation,
      relationalExpression,
      relationalOperator,
      resourceAcquisition,
      returnStatement,
      returnType,
      secondaryConstraint,
      secondaryConstraints,
      selectOrGroupClause,
      selectionStatement,
      shiftExpression,
      shiftOperator,
      simpleConditionalExpression,
      simpleName,
      simpleType,
      specificCatchClause,
      stackallocExpression,
      standardEventDeclaration,
      standardIndexerDeclaration,
      standardLocalFunctionDeclaration,
      standardMethodDeclaration,
      standardPropertyDeclaration,
      statement,
      statementExpression,
      statementExpressionList,
      staticConstructorBody,
      staticConstructorDeclaration,
      staticConstructorModifiers,
      structDeclaration,
      structMemberDeclaration,
      structModifier,
      structOrEnumType,
      structType,
      switchBranch,
      switchLabel,
      switchSection,
      switchStatement,
      tryStatement,
      tupleElement,
      tupleExpression,
      tupleType,
      tupleTypeElement,
      type_,
      typeArgumentList,
      typeDeclaration,
      typeName,
      typeParameter,
      typeParameterConstraints,
      typeParameterConstraintsClause,
      typeParameterList,
      typeParameterPart,
      typeofExpression,
      unaryExpression,
      unaryOperatorDeclarator,
      unboundTypeName,
      unboundTypeNamePart,
      unmanagedType,
      usingAliasDirective,
      usingDirective,
      usingStatement,
      valueType,
      variableDeclarator,
      variableDeclarators,
      variableInitializer,
      variableReference,
      varianceAnnotation,
      variantTypeParameter,
      variantTypeParameters,
      verbatimInterpolation,
      whileStatement,
      yieldStatement
      ]


csharp :: String -> Type
csharp = typeref ns


def :: String -> Type -> TypeDefinition
def = datatype ns

-- Lexical Elements

identifier :: TypeDefinition
identifier = def "Identifier" $
  doc "A C# identifier: a name for a variable, type, member, or other program element." $
  T.wrap T.string

integerLiteral :: TypeDefinition
integerLiteral = def "IntegerLiteral" $
  doc "An integer literal, expressed in decimal, hexadecimal, or binary notation." $
  T.union [
  "decimal">: T.string,
  "hexadecimal">: T.string,
  "binary">: T.bigint]


keyword :: TypeDefinition
keyword = def "Keyword" $
  doc "A reserved C# keyword." $
  T.wrap T.string

literal :: TypeDefinition
literal = def "Literal" $
  doc "A literal value: boolean, integer, real, character, string, or null." $
  T.union [
  "boolean">: T.boolean,
  "integer">: csharp "IntegerLiteral",
  "real">: T.float64,
  "character">: T.string,
  "string">: T.string,
  "null">: T.unit]

-- Syntactic Elements

accessorBody :: TypeDefinition
accessorBody = def "AccessorBody" $
  doc "The body of a property or indexer accessor: a block, an expression, or empty (abstract)." $
  T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

accessorDeclaration :: TypeDefinition
accessorDeclaration = def "AccessorDeclaration" $
  doc "A single get or set accessor, with optional attributes and an access modifier." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifier">: T.optional $ csharp "AccessorModifier",
  "body">: csharp "AccessorBody"]

accessorDeclarations :: TypeDefinition
accessorDeclarations = def "AccessorDeclarations" $
  doc "The get and/or set accessors of a standard (non-ref-returning) property." $
  T.union [
  "get">: T.optional $ csharp "AccessorDeclaration",
  "set">: T.optional $ csharp "AccessorDeclaration"]

accessorModifier :: TypeDefinition
accessorModifier = def "AccessorModifier" $
  doc "An access modifier restricting a get or set accessor beyond the property's own accessibility." $
  T.enum [
  "protected",
  "internal",
  "private",
  "protectedInternal",
  "internalProtected",
  "protectedPrivate",
  "privateProtected"]

accessorsEventDeclaration :: TypeDefinition
accessorsEventDeclaration = def "AccessorsEventDeclaration" $
  doc "An event declaration using explicit add/remove accessor syntax." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "accessors">: csharp "EventAccessorDeclarations"]

addRemoveAccessorDeclaration :: TypeDefinition
addRemoveAccessorDeclaration = def "AddRemoveAccessorDeclaration" $
  doc "The add or remove accessor body of an event declaration." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "body">: csharp "Block"]

additiveExpression :: TypeDefinition
additiveExpression = def "AdditiveExpression" $
  doc "A simple multiplicative expression or a binary addition/subtraction expression." $
  T.union [
  "simple">: csharp "MultiplicativeExpression",
  "binary">: csharp "BinaryAdditiveExpression"]

additiveOperator :: TypeDefinition
additiveOperator = def "AdditiveOperator" $
  doc "The plus or minus operator used in an additive expression." $
  T.enum [
  "plus",
  "minus"]

andExpression :: TypeDefinition
andExpression = def "AndExpression" $
  doc "A simple equality expression or a binary bitwise-AND expression." $
  T.union [
  "simple">: csharp "EqualityExpression",
  "binary">: csharp "BinaryAndExpression"]

anonymousFunctionBody :: TypeDefinition
anonymousFunctionBody = def "AnonymousFunctionBody" $
  doc "The body of an anonymous function: a null-conditional invocation, expression, ref, or block." $
  T.union [
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "ref">: csharp "VariableReference",
  "block">: csharp "Block"]

anonymousFunctionParameterModifier :: TypeDefinition
anonymousFunctionParameterModifier = def "AnonymousFunctionParameterModifier" $
  doc "A ref, out, or in modifier on an explicit anonymous function parameter." $
  T.enum [
  "ref",
  "out",
  "in"]

anonymousFunctionSignature :: TypeDefinition
anonymousFunctionSignature = def "AnonymousFunctionSignature" $
  doc "The parameter list of a lambda expression, either explicitly or implicitly typed." $
  T.union [
  "explicit">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "implicit">: T.list $ csharp "Identifier"]

anonymousMethodExpression :: TypeDefinition
anonymousMethodExpression = def "AnonymousMethodExpression" $
  doc "A delegate expression created with the anonymous method syntax (delegate(...) { ... })." $
  T.record [
  "async">: T.boolean,
  "signature">: T.list $ csharp "ExplicitAnonymousFunctionParameter",
  "body">: csharp "Block"]

argument :: TypeDefinition
argument = def "Argument" $
  doc "A single argument in an argument list, with an optional name for named arguments." $
  T.record [
  "name">: T.optional $ csharp "Identifier",
  "value">: csharp "ArgumentValue"]

argumentList :: TypeDefinition
argumentList = def "ArgumentList" $
  doc "A non-empty, comma-separated list of arguments passed to a member or delegate invocation." $
  T.wrap $ nonemptyList $ csharp "Argument"

argumentValue :: TypeDefinition
argumentValue = def "ArgumentValue" $
  doc "The value portion of an argument: an expression, or a ref/in/out variable reference." $
  T.union [
  "expression">: csharp "Expression",
  "in">: csharp "VariableReference",
  "ref">: csharp "VariableReference",
  "out">: csharp "VariableReference"]

arrayCreationExpression :: TypeDefinition
arrayCreationExpression = def "ArrayCreationExpression" $
  doc "An expression that creates a new array, in any of its three creation-syntax forms." $
  T.union [
  "nonArrayType">: csharp "NonArrayTypeArrayCreationExpression",
  "arrayType">: csharp "ArrayTypeArrayCreationExpression",
  "rankSpecifier">: csharp "RankSpecifierArrayCreationExpression"]

arrayInitializer :: TypeDefinition
arrayInitializer = def "ArrayInitializer" $
  doc "A brace-delimited list of variable initializers used to populate an array." $
  T.wrap $
  T.list $ csharp "VariableInitializer"

arrayType :: TypeDefinition
arrayType = def "ArrayType" $
  doc "An array type formed from a non-array element type and one or more rank specifiers." $
  T.record [
  "type">: csharp "NonArrayType",
  "rank">: T.list $ csharp "RankSpecifier"]

arrayTypeArrayCreationExpression :: TypeDefinition
arrayTypeArrayCreationExpression = def "ArrayTypeArrayCreationExpression" $
  doc "Array creation using an explicit array type together with an array initializer." $
  T.record [
  "type">: csharp "ArrayType",
  "initializer">: csharp "ArrayInitializer"]

asTypeExpression :: TypeDefinition
asTypeExpression = def "AsTypeExpression" $
  doc "An 'as' expression that attempts a type conversion, yielding null on failure." $
  T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

assignment :: TypeDefinition
assignment = def "Assignment" $
  doc "An assignment expression: a target, an assignment operator, and a right-hand expression." $
  T.record [
  "left">: csharp "UnaryExpression",
  "operator">: csharp "AssignmentOperator",
  "right">: csharp "Expression"]

assignmentMemberDeclarator :: TypeDefinition
assignmentMemberDeclarator = def "AssignmentMemberDeclarator" $
  doc "A member declarator of the form 'name = expression' in an anonymous object creation." $
  T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

assignmentOperator :: TypeDefinition
assignmentOperator = def "AssignmentOperator" $
  doc "The simple assignment operator or one of the compound assignment operators." $
  T.union [
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
attribute = def "Attribute" $
  doc "A single attribute, consisting of an attribute name and optional constructor/named arguments." $
  T.record [
  "name">: csharp "AttributeName",
  "arguments">: T.optional $ csharp "AttributeArguments"]

attributeArgumentExpression :: TypeDefinition
attributeArgumentExpression = def "AttributeArgumentExpression" $
  doc "An expression usable as an attribute argument (a compile-time constant expression)." $
  T.wrap $
  csharp "NonAssignmentExpression"


attributeArguments :: TypeDefinition
attributeArguments = def "AttributeArguments" $
  doc "The positional and named argument lists supplied to an attribute." $
  T.record [
  "positonal">: T.optional $ csharp "PositionalArgumentList",
  "named">: T.optional $ csharp "NamedArgumentList"]

attributeList :: TypeDefinition
attributeList = def "AttributeList" $
  doc "A non-empty, comma-separated list of attributes within a single attribute section." $
  T.wrap $ nonemptyList $ csharp "Attribute"

attributeName :: TypeDefinition
attributeName = def "AttributeName" $
  doc "The type name referenced by an attribute, resolved against types ending in the suffix Attribute." $
  T.wrap $
  csharp "TypeName"

attributeSection :: TypeDefinition
attributeSection = def "AttributeSection" $
  doc "A bracketed attribute section, with an optional target specifier, attached to a declaration." $
  T.record [
  "target">: T.optional $ csharp "AttributeTarget",
  "attributes">: csharp "AttributeList"]

attributeTarget :: TypeDefinition
attributeTarget = def "AttributeTarget" $
  doc "The target specifier of an attribute section (e.g. assembly, module, return, field)." $
  T.union [
  "identifier">: csharp "Identifier",
  "keyword">: csharp "Keyword"]

attributes :: TypeDefinition
attributes = def "Attributes" $
  doc "A non-empty sequence of attribute sections attached to a single declaration." $
  T.wrap $ nonemptyList $ csharp "AttributeSection"

baseAccess :: TypeDefinition
baseAccess = def "BaseAccess" $
  doc "A 'base' access expression, either invoking a base member or indexing via base[args]." $
  T.union [
  "identifier">: csharp "BaseAccessWithIdentifier",
  "arguments">: csharp "ArgumentList"]

baseAccessWithIdentifier :: TypeDefinition
baseAccessWithIdentifier = def "BaseAccessWithIdentifier" $
  doc "A 'base.member' access, with an optional list of type arguments." $
  T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

binaryAdditiveExpression :: TypeDefinition
binaryAdditiveExpression = def "BinaryAdditiveExpression" $
  doc "A binary addition or subtraction expression with a left operand, operator, and right operand." $
  T.record [
  "left">: csharp "AdditiveExpression",
  "operator">: csharp "AdditiveOperator",
  "right">: csharp "MultiplicativeExpression"]

binaryAndExpression :: TypeDefinition
binaryAndExpression = def "BinaryAndExpression" $
  doc "A binary bitwise-AND expression (&) over two operand expressions." $
  T.record [
  "left">: csharp "AndExpression",
  "right">: csharp "EqualityExpression"]

binaryConditionalAndExpression :: TypeDefinition
binaryConditionalAndExpression = def "BinaryConditionalAndExpression" $
  doc "A binary conditional-AND expression (&&) over two operand expressions." $
  T.record [
  "left">: csharp "ConditionalAndExpression",
  "right">: csharp "InclusiveOrExpression"]

binaryConditionalOrExpression :: TypeDefinition
binaryConditionalOrExpression = def "BinaryConditionalOrExpression" $
  doc "A binary conditional-OR expression (||) over two operand expressions." $
  T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "ConditionalAndExpression"]

binaryEqualityExpression :: TypeDefinition
binaryEqualityExpression = def "BinaryEqualityExpression" $
  doc "A binary equality or inequality comparison expression." $
  T.record [
  "left">: csharp "EqualityExpression",
  "operator">: csharp "EqualityOperator",
  "right">: csharp "RelationalExpression"]

binaryExclusiveOrExpression :: TypeDefinition
binaryExclusiveOrExpression = def "BinaryExclusiveOrExpression" $
  doc "A binary bitwise exclusive-OR expression (^) over two operand expressions." $
  T.record [
  "left">: csharp "ExclusiveOrExpression",
  "right">: csharp "AndExpression"]

binaryInclusiveOrExpression :: TypeDefinition
binaryInclusiveOrExpression = def "BinaryInclusiveOrExpression" $
  doc "A binary bitwise inclusive-OR expression (|) over two operand expressions." $
  T.record [
  "left">: csharp "InclusiveOrExpression",
  "right">: csharp "ExclusiveOrExpression"]

binaryMultiplicativeExpression :: TypeDefinition
binaryMultiplicativeExpression = def "BinaryMultiplicativeExpression" $
  doc "A binary multiplication, division, or remainder expression." $
  T.record [
  "left">: csharp "MultiplicativeExpression",
  "operator">: csharp "MultiplicativeOperator",
  "right">: csharp "UnaryExpression"]

binaryNullCoalescingExpression :: TypeDefinition
binaryNullCoalescingExpression = def "BinaryNullCoalescingExpression" $
  doc "A binary null-coalescing expression (??) over two operand expressions." $
  T.record [
  "left">: csharp "ConditionalOrExpression",
  "right">: csharp "NullCoalescingExpression"]

binaryOperatorDeclarator :: TypeDefinition
binaryOperatorDeclarator = def "BinaryOperatorDeclarator" $
  doc "The declarator for a user-defined binary operator overload, naming its two parameters." $
  T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableBinaryOperator",
  "left">: csharp "FixedParameter",
  "right">: csharp "FixedParameter"]

binaryRelationalExpression :: TypeDefinition
binaryRelationalExpression = def "BinaryRelationalExpression" $
  doc "A binary relational comparison expression (<, >, <=, or >=)." $
  T.record [
  "left">: csharp "RelationalExpression",
  "operator">: csharp "RelationalOperator",
  "right">: csharp "ShiftExpression"]

binaryShiftExpression :: TypeDefinition
binaryShiftExpression = def "BinaryShiftExpression" $
  doc "A binary left-shift or right-shift expression." $
  T.record [
  "left">: csharp "ShiftExpression",
  "operator">: csharp "ShiftOperator",
  "right">: csharp "AdditiveExpression"]

block :: TypeDefinition
block = def "Block" $
  doc "A brace-delimited sequence of statements forming a single compound statement." $
  T.wrap $
  T.list $ csharp "Statement"

blockPropertyBody :: TypeDefinition
blockPropertyBody = def "BlockPropertyBody" $
  doc "A property body given as get/set accessor declarations, with an optional initializer." $
  T.record [
  "accessors">: csharp "AccessorDeclarations",
  "initializer">: T.optional $ csharp "VariableInitializer"]

booleanExpression :: TypeDefinition
booleanExpression = def "BooleanExpression" $
  doc "An expression required to have (or be convertible to) type bool, such as an if condition." $
  T.wrap $
  csharp "Expression"

castExpression :: TypeDefinition
castExpression = def "CastExpression" $
  doc "An explicit type-cast expression applied to a unary expression." $
  T.record [
  "type">: csharp "Type",
  "expression">: csharp "UnaryExpression"]

catchClauses :: TypeDefinition
catchClauses = def "CatchClauses" $
  doc "The catch clauses of a try statement: one or more specific clauses, or a single general clause." $
  T.union [
  "specific">: T.list $ csharp "SpecificCatchClause",
  "general">: csharp "Block"]

classBase :: TypeDefinition
classBase = def "ClassBase" $
  doc "The base class and/or implemented interfaces listed in a class declaration's base-list." $
  T.union [
  "class">: T.optional $ csharp "ClassType",
  "interfaces">: T.list $ csharp "InterfaceType"]

classBody :: TypeDefinition
classBody = def "ClassBody" $
  doc "The brace-delimited sequence of member declarations that make up a class." $
  T.wrap $
  T.list $ csharp "ClassMemberDeclaration"

classDeclaration :: TypeDefinition
classDeclaration = def "ClassDeclaration" $
  doc "A C# class declaration, including its modifiers, type parameters, base list, and body." $
  T.record [
  "attributes">: T.list $ csharp "AttributeSection",
  "modifiers">: T.list $ csharp "ClassModifier",
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "TypeParameterList",
  "base">: T.optional $ csharp "ClassBase",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: csharp "ClassBody"]

classMemberDeclaration :: TypeDefinition
classMemberDeclaration = def "ClassMemberDeclaration" $
  doc "Any member that can appear in a class body: field, method, property, event, and so on." $
  T.union [
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
classModifier = def "ClassModifier" $
  doc "A modifier keyword on a class declaration, such as public, abstract, sealed, or static." $
  T.enum [
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
classType = def "ClassType" $
  doc "A reference to a class type: a named type, or the predefined 'object' or 'string' types." $
  T.union [
  "typeName">: csharp "TypeName",
  "object">: T.unit,
  "string">: T.unit]

compilationUnit :: TypeDefinition
compilationUnit = def "CompilationUnit" $
  doc "The root of a C# source file: extern aliases, using directives, attributes, and type members." $
  T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "attributes">: T.list $ csharp "GlobalAttributeSection",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

conditionalAndExpression :: TypeDefinition
conditionalAndExpression = def "ConditionalAndExpression" $
  doc "A simple inclusive-OR expression or a binary conditional-AND (&&) expression." $
  T.union [
  "simple">: csharp "InclusiveOrExpression",
  "binary">: csharp "BinaryConditionalAndExpression"]

conditionalExpression :: TypeDefinition
conditionalExpression = def "ConditionalExpression" $
  doc "The ternary conditional expression, in its simple, ref-conditional, or fallthrough forms." $
  T.union [
  "simple">: csharp "NullCoalescingExpression",
  "simpleConditional">: csharp "SimpleConditionalExpression",
  "refConditional">: csharp "RefConditionalExpression"]

conditionalOrExpression :: TypeDefinition
conditionalOrExpression = def "ConditionalOrExpression" $
  doc "A simple conditional-AND expression or a binary conditional-OR (||) expression." $
  T.union [
  "simple">: csharp "ConditionalAndExpression",
  "binary">: csharp "BinaryConditionalOrExpression"]

constantDeclaration :: TypeDefinition
constantDeclaration = def "ConstantDeclaration" $
  doc "A 'const' field declaration with its modifiers, type, and one or more declarators." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstantModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

constantDeclarator :: TypeDefinition
constantDeclarator = def "ConstantDeclarator" $
  doc "A single 'identifier = expression' pair within a constant declaration." $
  T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "ConstantExpression"]

constantExpression :: TypeDefinition
constantExpression = def "ConstantExpression" $
  doc "An expression that is required to be evaluable at compile time." $
  T.wrap $
  csharp "Expression"

constantModifier :: TypeDefinition
constantModifier = def "ConstantModifier" $
  doc "An access modifier (new, public, protected, internal, or private) on a constant declaration." $
  T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

constructorBody :: TypeDefinition
constructorBody = def "ConstructorBody" $
  doc "The body of an instance constructor: a block, expression body, or empty (extern) body." $
  T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

constructorDeclaration :: TypeDefinition
constructorDeclaration = def "ConstructorDeclaration" $
  doc "An instance constructor declaration, including its declarator and body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "ConstructorModifier",
  "declarator">: csharp "ConstructorDeclarator",
  "body">: csharp "ConstructorBody"]

constructorDeclarator :: TypeDefinition
constructorDeclarator = def "ConstructorDeclarator" $
  doc "A constructor's name, parameter list, and optional base/this initializer." $
  T.record [
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "initializer">: T.optional $ csharp "ConstructorInitializer"]

constructorInitializer :: TypeDefinition
constructorInitializer = def "ConstructorInitializer" $
  doc "A ': base(...)' or ': this(...)' initializer invoked before a constructor body runs." $
  T.union [
  "base">: T.optional $ csharp "ArgumentList",
  "this">: T.optional $ csharp "ArgumentList"]

constructorModifier :: TypeDefinition
constructorModifier = def "ConstructorModifier" $
  doc "An access or extern/unsafe modifier on an instance constructor declaration." $
  T.enum [
  "public",
  "protected",
  "internal",
  "private",
  "extern",
  "unsafe"]

conversionKind :: TypeDefinition
conversionKind = def "ConversionKind" $
  doc "Whether a user-defined conversion operator is implicit or explicit." $
  T.enum [
  "implicit",
  "explicit"]

conversionOperatorDeclarator :: TypeDefinition
conversionOperatorDeclarator = def "ConversionOperatorDeclarator" $
  doc "The declarator for a user-defined implicit or explicit conversion operator." $
  T.record [
  "kind">: csharp "ConversionKind",
  "type">: csharp "Type",
  "parameter">: csharp "FixedParameter"]

declarationExpression :: TypeDefinition
declarationExpression = def "DeclarationExpression" $
  doc "An inline variable declaration used as an expression, such as an out-variable declaration." $
  T.record [
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier"]

declarationPattern :: TypeDefinition
declarationPattern = def "DeclarationPattern" $
  doc "A pattern that tests an expression's type and, on success, binds it to a designation." $
  T.record [
  "type">: csharp "Type",
  "designation">: csharp "Designation"]

declarationStatement :: TypeDefinition
declarationStatement = def "DeclarationStatement" $
  doc "A local variable, local constant, or local function declaration statement." $
  T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "constant">: csharp "LocalConstantDeclaration",
  "function">: csharp "LocalFunctionDeclaration"]

deconstructionElement :: TypeDefinition
deconstructionElement = def "DeconstructionElement" $
  doc "One element of a deconstruction pattern: a nested tuple or a bound identifier." $
  T.union [
  "tuple">: csharp "DeconstructionTuple",
  "identifier">: csharp "Identifier"]

deconstructionTuple :: TypeDefinition
deconstructionTuple = def "DeconstructionTuple" $
  doc "A tuple deconstruction pattern used on the left-hand side of a deconstructing assignment." $
  T.wrap $
  nonemptyList $ csharp "DeconstructionElement"

defaultValueExpression :: TypeDefinition
defaultValueExpression = def "DefaultValueExpression" $
  doc "A 'default(T)' expression, or the type-inferred 'default' literal." $
  T.union [
  "explicitlyTyped">: csharp "Type",
  "defaultLiteral">: T.unit]

delegateCreationExpression :: TypeDefinition
delegateCreationExpression = def "DelegateCreationExpression" $
  doc "An expression that creates a delegate instance from a method group or lambda." $
  T.record [
  "type">: csharp "DelegateType",
  "expression">: csharp "Expression"]

delegateDeclaration :: TypeDefinition
delegateDeclaration = def "DelegateDeclaration" $
  doc "A delegate type declaration, naming its return type, ref kind, and parameter list." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "DelegateModifier",
  "returnType">: csharp "ReturnType",
  "refKind">: T.optional $ csharp "RefKind",
  "refReturnType">: T.optional $ csharp "Type",
  "header">: csharp "DelegateHeader"]

delegateHeader :: TypeDefinition
delegateHeader = def "DelegateHeader" $
  doc "The name, type parameters, formal parameters, and constraints of a delegate declaration." $
  T.record [
  "name">: csharp "Identifier",
  "typeParameters">: T.optional $ csharp "VariantTypeParameters",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

delegateModifier :: TypeDefinition
delegateModifier = def "DelegateModifier" $
  doc "An access or unsafe modifier on a delegate type declaration." $
  T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

delegateType :: TypeDefinition
delegateType = def "DelegateType" $
  doc "A reference to a delegate type by its type name." $
  T.wrap $
  csharp "TypeName"

dependentAccess :: TypeDefinition
dependentAccess = def "DependentAccess" $
  doc "A member access, element access, or invocation chained after a null-conditional operator." $
  T.union [
  "memberAccess">: csharp "DependentAccessForMember",
  "elementAccess">: csharp "ArgumentList",
  "invocation">: T.optional $ csharp "ArgumentList"]

dependentAccessForMember :: TypeDefinition
dependentAccessForMember = def "DependentAccessForMember" $
  doc "A member identifier and optional type arguments chained after a null-conditional access." $
  T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

designation :: TypeDefinition
designation = def "Designation" $
  doc "A simple identifier binding introduced by a pattern, such as in a declaration pattern." $
  T.wrap $
  csharp "Identifier"

doStatement :: TypeDefinition
doStatement = def "DoStatement" $
  doc "A do-while iteration statement that executes its body at least once." $
  T.record [
  "body">: csharp "EmbeddedStatement",
  "while">: csharp "BooleanExpression"]

elementAccess :: TypeDefinition
elementAccess = def "ElementAccess" $
  doc "An indexer access expression applying an argument list to a primary expression." $
  T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList"]

elementInitializer :: TypeDefinition
elementInitializer = def "ElementInitializer" $
  doc "A single element of a collection initializer: one expression, or a nested element list." $
  T.union [
  "single">: csharp "NonAssignmentExpression",
  "list">: csharp "ExpressionList"]

embeddedStatement :: TypeDefinition
embeddedStatement = def "EmbeddedStatement" $
  doc "A statement nested inside another statement, excluding labeled and declaration statements." $
  T.union [
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
enumBase = def "EnumBase" $
  doc "The optional explicit underlying integral type declared for an enum." $
  T.union [
  "type">: csharp "IntegralType",
  "name">: csharp "TypeName"]

enumBody :: TypeDefinition
enumBody = def "EnumBody" $
  doc "The brace-delimited list of member declarations that make up an enum." $
  T.wrap $
  T.list $ csharp "EnumMemberDeclaration"

enumDeclaration :: TypeDefinition
enumDeclaration = def "EnumDeclaration" $
  doc "An enum type declaration, with its modifiers, name, underlying type, and members." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EnumModifier",
  "name">: csharp "Identifier",
  "base">: T.optional $ csharp "EnumBase",
  "body">: T.optional $ csharp "EnumBody"]

enumMemberDeclaration :: TypeDefinition
enumMemberDeclaration = def "EnumMemberDeclaration" $
  doc "A single named enum member, with optional attributes and an explicit constant value." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "name">: csharp "Identifier",
  "value">: T.optional $ csharp "ConstantExpression"]

enumModifier :: TypeDefinition
enumModifier = def "EnumModifier" $
  doc "An access modifier (new, public, protected, internal, or private) on an enum declaration." $
  T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private"]

enumType :: TypeDefinition
enumType = def "EnumType" $
  doc "A reference to an enum type by its type name." $
  T.wrap $
  csharp "TypeName"

equalityExpression :: TypeDefinition
equalityExpression = def "EqualityExpression" $
  doc "A simple relational expression or a binary equality/inequality expression." $
  T.union [
  "simple">: csharp "RelationalExpression",
  "binary">: csharp "BinaryEqualityExpression"]

equalityOperator :: TypeDefinition
equalityOperator = def "EqualityOperator" $
  doc "The equality (==) or inequality (!=) operator." $
  T.enum [
  "equal",
  "notEqual"]

eventAccessorDeclarations :: TypeDefinition
eventAccessorDeclarations = def "EventAccessorDeclarations" $
  doc "The add and remove accessor declarations of an event with an explicit accessor body." $
  T.union [
  "add">: csharp "AddRemoveAccessorDeclaration",
  "remove">: csharp "AddRemoveAccessorDeclaration"]

eventDeclaration :: TypeDefinition
eventDeclaration = def "EventDeclaration" $
  doc "An event member declaration, in either its field-like or explicit-accessor form." $
  T.union [
  "standard">: csharp "StandardEventDeclaration",
  "accessors">: csharp "AccessorsEventDeclaration"]

eventModifier :: TypeDefinition
eventModifier = def "EventModifier" $
  doc "A modifier keyword on an event declaration, such as static, virtual, or override." $
  T.enum [
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
exceptionSpecifier = def "ExceptionSpecifier" $
  doc "The exception type and optional identifier caught by a specific catch clause." $
  T.record [
  "type">: csharp "Type",
  "identifier">: T.optional $ csharp "Identifier"]

exclusiveOrExpression :: TypeDefinition
exclusiveOrExpression = def "ExclusiveOrExpression" $
  doc "A simple AND expression or a binary bitwise exclusive-OR (^) expression." $
  T.union [
  "simple">: csharp "AndExpression",
  "binary">: csharp "BinaryExclusiveOrExpression"]

explicitAnonymousFunctionParameter :: TypeDefinition
explicitAnonymousFunctionParameter = def "ExplicitAnonymousFunctionParameter" $
  doc "An explicitly typed parameter of an anonymous function, with an optional modifier." $
  T.record [
  "modifier">: T.optional $ csharp "AnonymousFunctionParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier"]

explicitlyTypedLocalVariableDeclaration :: TypeDefinition
explicitlyTypedLocalVariableDeclaration = def "ExplicitlyTypedLocalVariableDeclaration" $
  doc "A local variable declaration whose type is written out explicitly." $
  T.record [
  "type">: csharp "Type",
  "declarators">: T.list $ csharp "ExplicitlyTypedLocalVariableDeclarator"]

explicitlyTypedLocalVariableDeclarator :: TypeDefinition
explicitlyTypedLocalVariableDeclarator = def "ExplicitlyTypedLocalVariableDeclarator" $
  doc "An identifier and optional initializer within an explicitly typed local declaration." $
  T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.optional $ csharp "LocalVariableInitializer"]

expression :: TypeDefinition
expression = def "Expression" $
  doc "A non-assignment expression or an assignment expression." $
  T.union [
  "nonAssignment">: csharp "NonAssignmentExpression",
  "assignment">: csharp "Assignment"]

expressionList :: TypeDefinition
expressionList = def "ExpressionList" $
  doc "A non-empty, comma-separated list of expressions, as used in an array creation expression." $
  T.wrap $ nonemptyList $ csharp "Expression"

externAliasDirective :: TypeDefinition
externAliasDirective = def "ExternAliasDirective" $
  doc "An 'extern alias' directive introducing an alias for an external assembly reference." $
  T.wrap $
  csharp "Identifier"

fieldDeclaration :: TypeDefinition
fieldDeclaration = def "FieldDeclaration" $
  doc "A field member declaration, with its modifiers, type, and one or more variable declarators." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "FieldModifier",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "VariableDeclarator"]

fieldModifier :: TypeDefinition
fieldModifier = def "FieldModifier" $
  doc "A modifier keyword on a field declaration, such as static, readonly, or volatile." $
  T.enum [
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
finalizerBody = def "FinalizerBody" $
  doc "The body of a finalizer: a block, expression body, or empty (extern) body." $
  T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

finalizerDeclaration :: TypeDefinition
finalizerDeclaration = def "FinalizerDeclaration" $
  doc "A finalizer (destructor) declaration, with its extern/unsafe flags, name, and body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "extern">: T.boolean,
  "unsafe">: T.boolean,
  "name">: csharp "Identifier",
  "body">: csharp "FinalizerBody"]

fixedParameter :: TypeDefinition
fixedParameter = def "FixedParameter" $
  doc "A single by-value or by-reference formal parameter, with optional attributes and a default value." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifier">: T.optional $ csharp "ParameterModifier",
  "type">: csharp "Type",
  "identifier">: csharp "Identifier",
  "defaultArgument">: T.optional $ csharp "Expression"]

floatingPointType :: TypeDefinition
floatingPointType = def "FloatingPointType" $
  doc "The float or double floating-point numeric type." $
  T.union [
  "float">: T.unit,
  "double">: T.unit]

forInitializer :: TypeDefinition
forInitializer = def "ForInitializer" $
  doc "The initializer clause of a for statement: a local variable declaration or expression list." $
  T.union [
  "variable">: csharp "LocalVariableDeclaration",
  "statements">: csharp "StatementExpressionList"]

forStatement :: TypeDefinition
forStatement = def "ForStatement" $
  doc "A C-style for loop, with optional initializer, condition, iterator, and a body statement." $
  T.record [
  "initializer">: T.optional $ csharp "ForInitializer",
  "condition">: T.optional $ csharp "BooleanExpression",
  "iterator">: T.optional $ csharp "StatementExpressionList",
  "body">: csharp "EmbeddedStatement"]

foreachStatement :: TypeDefinition
foreachStatement = def "ForeachStatement" $
  doc "A foreach iteration statement over an enumerable expression, binding each element to a variable." $
  T.record [
  "kind">: T.optional $ csharp "RefKind",
  "type">: csharp "LocalVariableType",
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

formalParameterList :: TypeDefinition
formalParameterList = def "FormalParameterList" $
  doc "The fixed parameters of a member, plus an optional trailing params array parameter." $
  T.record [
  "fixed">: T.list $ csharp "FixedParameter",
  "array">: T.optional $ csharp "ParameterArray"]

fromClause :: TypeDefinition
fromClause = def "FromClause" $
  doc "The initial or additional 'from' clause of a query expression, introducing a range variable." $
  T.record [
  "type">: T.optional $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression"]

globalAttributeSection :: TypeDefinition
globalAttributeSection = def "GlobalAttributeSection" $
  doc "An assembly- or module-level attribute section appearing at the top of a compilation unit." $
  T.record [
  "target">: csharp "Identifier",
  "attributes">: csharp "AttributeList"]

gotoStatement :: TypeDefinition
gotoStatement = def "GotoStatement" $
  doc "A goto statement targeting a label, a switch case, or the switch default section." $
  T.union [
  "identifier">: csharp "Identifier",
  "case">: csharp "ConstantExpression",
  "default">: T.unit]

groupClause :: TypeDefinition
groupClause = def "GroupClause" $
  doc "The 'group ... by ...' clause that ends a query expression, producing grouped results." $
  T.record [
  "grouped">: csharp "Expression",
  "by">: csharp "Expression"]

identifierNamespaceOrTypeName :: TypeDefinition
identifierNamespaceOrTypeName = def "IdentifierNamespaceOrTypeName" $
  doc "A simple namespace-or-type name consisting of an identifier and optional type arguments." $
  T.record [
  "identifier">: csharp "Identifier",
  "arguments">: T.optional $ csharp "TypeArgumentList"]

ifStatement :: TypeDefinition
ifStatement = def "IfStatement" $
  doc "An if statement with a boolean condition, a then-branch, and an else-branch." $
  T.record [
  "condition">: csharp "BooleanExpression",
  "ifBranch">: csharp "EmbeddedStatement",
  "elseBranch">: csharp "EmbeddedStatement"]

implicitlyTypedLocalVariableDeclaration :: TypeDefinition
implicitlyTypedLocalVariableDeclaration = def "ImplicitlyTypedLocalVariableDeclaration" $
  doc "A local variable declaration using 'var' with inferred type, plain or ref." $
  T.union [
  "var">: csharp "ImplicitlyTypedLocalVariableDeclarator",
  "refVar">: csharp "RefVarImplicitlyTypedLocalVariableDeclaration"]

implicitlyTypedLocalVariableDeclarator :: TypeDefinition
implicitlyTypedLocalVariableDeclarator = def "ImplicitlyTypedLocalVariableDeclarator" $
  doc "The identifier and initializer expression of a 'var'-typed local variable." $
  T.record [
  "identifier">: csharp "Identifier",
  "expression">: csharp "Expression"]

inclusiveOrExpression :: TypeDefinition
inclusiveOrExpression = def "InclusiveOrExpression" $
  doc "A simple exclusive-OR expression or a binary bitwise inclusive-OR (|) expression." $
  T.union [
  "simple">: csharp "ExclusiveOrExpression",
  "binary">: csharp "BinaryInclusiveOrExpression"]

indexerBody :: TypeDefinition
indexerBody = def "IndexerBody" $
  doc "The body of a standard indexer: block accessors or a single expression body." $
  T.union [
  "block">: csharp "AccessorDeclarations",
  "expression">: csharp "Expression"]

indexerDeclaration :: TypeDefinition
indexerDeclaration = def "IndexerDeclaration" $
  doc "An indexer member declaration, in its standard or ref-returning form." $
  T.union [
  "standard">: csharp "StandardIndexerDeclaration",
  "ref">: csharp "RefIndexerDeclaration"]

indexerDeclarator :: TypeDefinition
indexerDeclarator = def "IndexerDeclarator" $
  doc "An indexer's declared type, optional explicit interface, and formal parameter list." $
  T.record [
  "type">: csharp "Type",
  "interface">: T.optional $ csharp "InterfaceType",
  "parameters">: csharp "FormalParameterList"]

indexerModifier :: TypeDefinition
indexerModifier = def "IndexerModifier" $
  doc "A modifier keyword on an indexer declaration, such as virtual, override, or abstract." $
  T.enum [
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
initializerTarget = def "InitializerTarget" $
  doc "The left-hand side of a member initializer: an identifier or an indexer argument list." $
  T.union [
  "identifier">: csharp "Identifier",
  "arguments">: csharp "ArgumentList"]

initializerValue :: TypeDefinition
initializerValue = def "InitializerValue" $
  doc "The right-hand side of a member initializer: an expression or a nested collection initializer." $
  T.union [
  "expression">: csharp "Expression",
  "objectOrCollection">: csharp "ObjectOrCollectionInitializer"]

integralType :: TypeDefinition
integralType = def "IntegralType" $
  doc "One of the built-in integral numeric types (sbyte through char)." $
  T.union [
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
interfaceAccessors = def "InterfaceAccessors" $
  doc "The get and/or set accessor attribute lists declared by an interface property or indexer." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "get">: T.optional $ csharp "Attributes",
  "set">: T.optional $ csharp "Attributes"]

interfaceDeclaration :: TypeDefinition
interfaceDeclaration = def "InterfaceDeclaration" $
  doc "An interface type declaration, with its modifiers, type parameters, base interfaces, and members." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "InterfaceModifier",
  "partial">: T.boolean,
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "VariantTypeParameters",
  "base">: T.list $ csharp "InterfaceType",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause",
  "body">: T.list $ csharp "InterfaceMemberDeclaration"]

interfaceEventDeclaration :: TypeDefinition
interfaceEventDeclaration = def "InterfaceEventDeclaration" $
  doc "An event member declared within an interface body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "type">: csharp "Type",
  "name">: csharp "Identifier"]

interfaceIndexerDeclaration :: TypeDefinition
interfaceIndexerDeclaration = def "InterfaceIndexerDeclaration" $
  doc "An indexer member declared within an interface body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.optional $ csharp "RefKind",
  "type">: csharp "Type",
  "parameters">: csharp "FormalParameterList",
  "accessors">: csharp "InterfaceAccessors"]

interfaceMemberDeclaration :: TypeDefinition
interfaceMemberDeclaration = def "InterfaceMemberDeclaration" $
  doc "Any member that can appear in an interface body: method, property, event, or indexer." $
  T.union [
  "method">: csharp "InterfaceMethodDeclaration",
  "property">: csharp "InterfacePropertyDeclaration",
  "event">: csharp "InterfaceEventDeclaration",
  "indexer">: csharp "InterfaceIndexerDeclaration"]

interfaceMethodDeclaration :: TypeDefinition
interfaceMethodDeclaration = def "InterfaceMethodDeclaration" $
  doc "A method member declared within an interface body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "returnType">: csharp "ReturnType",
  "refKind">: T.optional $ csharp "RefKind",
  "header">: csharp "InterfaceMethodHeader"]

interfaceMethodHeader :: TypeDefinition
interfaceMethodHeader = def "InterfaceMethodHeader" $
  doc "The name, parameters, type parameters, and constraints of an interface method declaration." $
  T.record [
  "name">: csharp "Identifier",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "typeParameters">: T.optional $ csharp "TypeParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

interfaceModifier :: TypeDefinition
interfaceModifier = def "InterfaceModifier" $
  doc "An access or unsafe modifier on an interface declaration." $
  T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "unsafe"]

interfacePropertyDeclaration :: TypeDefinition
interfacePropertyDeclaration = def "InterfacePropertyDeclaration" $
  doc "A property member declared within an interface body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "new">: T.boolean,
  "refKind">: T.optional $ csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "Identifier",
  "accessors">: csharp "InterfaceAccessors"]

interfaceType :: TypeDefinition
interfaceType = def "InterfaceType" $
  doc "A reference to an interface type by its type name." $
  T.wrap $
  csharp "TypeName"

interpolatedRegularStringExpression :: TypeDefinition
interpolatedRegularStringExpression = def "InterpolatedRegularStringExpression" $
  doc "An interpolated string literal delimited by ordinary double quotes." $
  T.wrap T.string

interpolatedStringExpression :: TypeDefinition
interpolatedStringExpression = def "InterpolatedStringExpression" $
  doc "An interpolated string expression, in its regular or verbatim quoting form." $
  T.union [
  "regular">: csharp "InterpolatedRegularStringExpression",
  "verbatim">: csharp "InterpolatedVerbatimStringExpression"]

interpolatedVerbatimStringExpression :: TypeDefinition
interpolatedVerbatimStringExpression = def "InterpolatedVerbatimStringExpression" $
  doc "An interpolated string literal using verbatim, at-sign-prefixed quoting." $
  T.wrap T.string

invocationExpression :: TypeDefinition
invocationExpression = def "InvocationExpression" $
  doc "A method or delegate invocation: a primary expression applied to an argument list." $
  T.record [
  "expression">: csharp "PrimaryExpression",
  "arguments">: T.optional $ csharp "ArgumentList"]

isPatternExpression :: TypeDefinition
isPatternExpression = def "IsPatternExpression" $
  doc "An 'is' pattern-matching expression testing a relational expression against a pattern." $
  T.record [
  "expression">: csharp "RelationalExpression",
  "pattern">: csharp "Pattern"]

isTypeExpression :: TypeDefinition
isTypeExpression = def "IsTypeExpression" $
  doc "An 'is' type-testing expression checking whether an expression is of a given type." $
  T.record [
  "expression">: csharp "RelationalExpression",
  "type">: csharp "Type"]

iterationStatement :: TypeDefinition
iterationStatement = def "IterationStatement" $
  doc "A while, do, for, or foreach loop statement." $
  T.union [
  "while">: csharp "WhileStatement",
  "do">: csharp "DoStatement",
  "for">: csharp "ForStatement",
  "foreach">: csharp "ForeachStatement"]

joinClause :: TypeDefinition
joinClause = def "JoinClause" $
  doc "A 'join' clause in a query expression, correlating two sequences on matching keys." $
  T.record [
  "type">: T.optional $ csharp "Type",
  "identifier">: csharp "Identifier",
  "in">: csharp "Expression",
  "on">: csharp "Expression",
  "equals">: csharp "Expression",
  "into">: T.optional $ csharp "Identifier"]

jumpStatement :: TypeDefinition
jumpStatement = def "JumpStatement" $
  doc "A break, continue, goto, return, or throw statement that transfers control." $
  T.union [
  "break">: T.unit,
  "continue">: T.unit,
  "goto">: csharp "GotoStatement",
  "return">: csharp "ReturnStatement",
  "throw">: T.optional $ csharp "Expression"]

labeledStatement :: TypeDefinition
labeledStatement = def "LabeledStatement" $
  doc "A statement prefixed with a label, usable as the target of a goto statement." $
  T.record [
  "label">: csharp "Identifier",
  "statement">: csharp "Statement"]

lambdaExpression :: TypeDefinition
lambdaExpression = def "LambdaExpression" $
  doc "A lambda expression, with its async flag, parameter signature, and body." $
  T.record [
  "async">: T.boolean,
  "signature">: csharp "AnonymousFunctionSignature",
  "body">: csharp "AnonymousFunctionBody"]

letClause :: TypeDefinition
letClause = def "LetClause" $
  doc "A 'let' clause in a query expression that introduces a computed range variable." $
  T.record [
  "left">: csharp "Identifier",
  "right">: csharp "Expression"]

localConstantDeclaration :: TypeDefinition
localConstantDeclaration = def "LocalConstantDeclaration" $
  doc "A 'const' local declaration with its type and one or more constant declarators." $
  T.record [
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "ConstantDeclarator"]

localFunctionBody :: TypeDefinition
localFunctionBody = def "LocalFunctionBody" $
  doc "The body of a local function: a block, null-conditional invocation, or expression." $
  T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression"]

localFunctionDeclaration :: TypeDefinition
localFunctionDeclaration = def "LocalFunctionDeclaration" $
  doc "A local function declaration, in its standard or ref-returning form." $
  T.union [
  "standard">: csharp "StandardLocalFunctionDeclaration",
  "ref">: csharp "RefLocalFunctionDeclaration"]

localFunctionHeader :: TypeDefinition
localFunctionHeader = def "LocalFunctionHeader" $
  doc "The name, type parameters, formal parameters, and constraints of a local function." $
  T.record [
  "identifier">: csharp "Identifier",
  "typeParameters">: T.optional $ csharp "TypeParameterList",
  "parameters">: csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

localFunctionModifier :: TypeDefinition
localFunctionModifier = def "LocalFunctionModifier" $
  doc "A modifier on a local function declaration: a ref modifier, or 'async'." $
  T.union [
  "ref">: csharp "RefLocalFunctionModifier",
  "async">: T.unit]

localVariableDeclaration :: TypeDefinition
localVariableDeclaration = def "LocalVariableDeclaration" $
  doc "A local variable declaration, in its implicitly typed, explicitly typed, or ref form." $
  T.union [
  "implicitlyTyped">: csharp "ImplicitlyTypedLocalVariableDeclaration",
  "explicitlyTyped">: csharp "ExplicitlyTypedLocalVariableDeclaration",
  "ref">: csharp "RefLocalVariableDeclaration"]

localVariableInitializer :: TypeDefinition
localVariableInitializer = def "LocalVariableInitializer" $
  doc "The initializer of a local variable: an expression or an array initializer." $
  T.union [
  "expression">: csharp "Expression",
  "initializer">: csharp "ArrayInitializer"]

localVariableType :: TypeDefinition
localVariableType = def "LocalVariableType" $
  doc "The declared type of a local variable, or 'var' for implicit typing." $
  T.union [
  "type">: csharp "Type",
  "var">: T.unit]

lockStatement :: TypeDefinition
lockStatement = def "LockStatement" $
  doc "A 'lock' statement that acquires a mutual-exclusion lock for the duration of its body." $
  T.record [
  "expression">: csharp "Expression",
  "body">: csharp "EmbeddedStatement"]

memberAccess :: TypeDefinition
memberAccess = def "MemberAccess" $
  doc "A 'primary.identifier' member access, with optional explicit type arguments." $
  T.record [
  "head">: csharp "MemberAccessHead",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

memberAccessHead :: TypeDefinition
memberAccessHead = def "MemberAccessHead" $
  doc "The left-hand target of a member access: a primary expression, predefined type, or alias." $
  T.union [
  "primary">: csharp "PrimaryExpression",
  "predefined">: csharp "PredefinedType",
  "qualifiedAlias">: csharp "QualifiedAliasMember"]

memberDeclarator :: TypeDefinition
memberDeclarator = def "MemberDeclarator" $
  doc "A single member of an anonymous object initializer, naming or computing a member value." $
  T.union [
  "name">: csharp "SimpleName",
  "memberAccess">: csharp "MemberAccess",
  "nullConditionalProjectionInitializer">: csharp "NullConditionalProjectionInitializer",
  "baseAccess">: csharp "BaseAccess",
  "assignment">: csharp "AssignmentMemberDeclarator"]

memberDeclaratorList :: TypeDefinition
memberDeclaratorList = def "MemberDeclaratorList" $
  doc "A non-empty, comma-separated list of member declarators in an anonymous object creation." $
  T.wrap $ nonemptyList $ csharp "MemberDeclarator"

memberInitializer :: TypeDefinition
memberInitializer = def "MemberInitializer" $
  doc "A 'target = value' pair within an object initializer." $
  T.record [
  "target">: csharp "InitializerTarget",
  "value">: csharp "InitializerValue"]

memberName :: TypeDefinition
memberName = def "MemberName" $
  doc "A member's declared name, with an optional explicit interface type qualifier." $
  T.record [
  "interfaceType">: T.optional $ csharp "TypeName",
  "identifier">: csharp "Identifier"]

methodBody :: TypeDefinition
methodBody = def "MethodBody" $
  doc "The body of a standard method: a block, null-conditional invocation, expression, or empty." $
  T.union [
  "block">: csharp "Block",
  "nullConditionalInvocation">: csharp "NullConditionalInvocationExpression",
  "expression">: csharp "Expression",
  "empty">: T.unit]

methodDeclaration :: TypeDefinition
methodDeclaration = def "MethodDeclaration" $
  doc "A method member declaration, in its standard or ref-returning form." $
  T.union [
  "standard">: csharp "StandardMethodDeclaration",
  "refReturn">: csharp "RefReturnMethodDeclaration"]

methodHeader :: TypeDefinition
methodHeader = def "MethodHeader" $
  doc "The name, type parameters, formal parameters, and constraints of a method declaration." $
  T.record [
  "name">: csharp "MemberName",
  "typeParameters">: T.optional $ csharp "TypeParameterList",
  "parameters">: T.optional $ csharp "FormalParameterList",
  "constraints">: T.list $ csharp "TypeParameterConstraintsClause"]

methodModifier :: TypeDefinition
methodModifier = def "MethodModifier" $
  doc "A modifier on a method declaration: a ref modifier, or 'async'." $
  T.union [
  "ref">: csharp "RefMethodModifier",
  "async">: T.unit]

methodModifiers :: TypeDefinition
methodModifiers = def "MethodModifiers" $
  doc "The full modifier list of a method declaration, including whether it is 'partial'." $
  T.record [
  "modifiers">: T.list $ csharp "MethodModifier",
  "partial">: T.boolean]

multiplicativeExpression :: TypeDefinition
multiplicativeExpression = def "MultiplicativeExpression" $
  doc "A simple unary expression or a binary multiplication/division/remainder expression." $
  T.union [
  "simple">: csharp "UnaryExpression",
  "binary">: csharp "BinaryMultiplicativeExpression"]

multiplicativeOperator :: TypeDefinition
multiplicativeOperator = def "MultiplicativeOperator" $
  doc "The multiplication, division, or modulo operator." $
  T.enum [
  "times",
  "divide",
  "modulo"]

namedArgument :: TypeDefinition
namedArgument = def "NamedArgument" $
  doc "An argument of the form 'name: value' passed to an attribute constructor." $
  T.record [
  "name">: csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

namedArgumentList :: TypeDefinition
namedArgumentList = def "NamedArgumentList" $
  doc "A non-empty, comma-separated list of named arguments passed to an attribute." $
  T.wrap $ nonemptyList $ csharp "NamedArgument"

namedEntity :: TypeDefinition
namedEntity = def "NamedEntity" $
  doc "The target expression of a 'nameof' operator, with its chain of accessed parts." $
  T.record [
  "target">: csharp "NamedEntityTarget",
  "parts">: T.list $ csharp "NamedEntityPart"]

namedEntityPart :: TypeDefinition
namedEntityPart = def "NamedEntityPart" $
  doc "A single '.identifier<TypeArgs>' segment chained within a named entity." $
  T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

namedEntityTarget :: TypeDefinition
namedEntityTarget = def "NamedEntityTarget" $
  doc "The initial target of a named entity: a simple name, this, base, or a predefined/aliased type." $
  T.union [
  "name">: csharp "SimpleName",
  "this">: T.unit,
  "base">: T.unit,
  "predefinedType">: csharp "PredefinedType",
  "qualifiedAliasMember">: csharp "QualifiedAliasMember"]

namespaceBody :: TypeDefinition
namespaceBody = def "NamespaceBody" $
  doc "The extern aliases, using directives, and member declarations within a namespace." $
  T.record [
  "externs">: T.list $ csharp "ExternAliasDirective",
  "usings">: T.list $ csharp "UsingDirective",
  "members">: T.list $ csharp "NamespaceMemberDeclaration"]

namespaceDeclaration :: TypeDefinition
namespaceDeclaration = def "NamespaceDeclaration" $
  doc "A namespace declaration, naming the namespace and giving its body." $
  T.record [
  "name">: csharp "QualifiedIdentifier",
  "body">: csharp "NamespaceBody"]

namespaceMemberDeclaration :: TypeDefinition
namespaceMemberDeclaration = def "NamespaceMemberDeclaration" $
  doc "A member of a namespace: a nested namespace declaration or a type declaration." $
  T.union [
  "namespace">: csharp "NamespaceDeclaration",
  "type">: csharp "TypeDeclaration"]

namespaceName :: TypeDefinition
namespaceName = def "NamespaceName" $
  doc "A reference to a namespace, expressed as a namespace-or-type name." $
  T.wrap $
  csharp "NamespaceOrTypeName"

namespaceOrTypeName :: TypeDefinition
namespaceOrTypeName = def "NamespaceOrTypeName" $
  doc "A name that may refer to either a namespace or a type: identifier, qualified, or alias-qualified." $
  T.union [
  "identifier">: csharp "IdentifierNamespaceOrTypeName",
  "qualified">: csharp "QualifiedNamespaceOrTypeName",
  "alias">: csharp "QualifiedAliasMember"]

nonArrayType :: TypeDefinition
nonArrayType = def "NonArrayType" $
  doc "A type that is not itself an array type: value, class, interface, delegate, dynamic, or pointer." $
  T.union [
  "value">: csharp "ValueType",
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit,
  "parameter">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

nonArrayTypeArrayCreationExpression :: TypeDefinition
nonArrayTypeArrayCreationExpression = def "NonArrayTypeArrayCreationExpression" $
  doc "Array creation giving the element type, dimension expressions, and rank specifiers directly." $
  T.record [
  "type">: csharp "NonArrayType",
  "expressions">: csharp "ExpressionList",
  "rankSpecifiers">: T.list $ csharp "RankSpecifier",
  "initializer">: T.optional $ csharp "ArrayInitializer"]

nonAssignmentExpression :: TypeDefinition
nonAssignmentExpression = def "NonAssignmentExpression" $
  doc "An expression other than an assignment: declaration, conditional, lambda, or query expression." $
  T.union [
  "declaration">: csharp "DeclarationExpression",
  "conditional">: csharp "ConditionalExpression",
  "lambda">: csharp "LambdaExpression",
  "query">: csharp "QueryExpression"]

nullCoalescingExpression :: TypeDefinition
nullCoalescingExpression = def "NullCoalescingExpression" $
  doc "A simple, binary (??), or throw-expression form of the null-coalescing operator." $
  T.union [
  "simple">: csharp "ConditionalOrExpression",
  "binary">: csharp "BinaryNullCoalescingExpression",
  "throw">: csharp "NullCoalescingExpression"]

nullConditionalElementAccess :: TypeDefinition
nullConditionalElementAccess = def "NullConditionalElementAccess" $
  doc "A null-conditional element access (?[...]) with its chain of dependent accesses." $
  T.record [
  "expression">: csharp "PrimaryNoArrayCreationExpression",
  "arguments">: csharp "ArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

nullConditionalInvocationExpression :: TypeDefinition
nullConditionalInvocationExpression = def "NullConditionalInvocationExpression" $
  doc "A null-conditional invocation (?.method(...) or ?[...]) with optional arguments." $
  T.record [
  "head">: csharp "NullConditionalInvocationExpressionHead",
  "arguments">: T.optional $ csharp "ArgumentList"]

nullConditionalInvocationExpressionHead :: TypeDefinition
nullConditionalInvocationExpressionHead = def "NullConditionalInvocationExpressionHead" $
  doc "The member- or element-access head of a null-conditional invocation expression." $
  T.union [
  "member">: csharp "NullConditionalMemberAccess",
  "element">: csharp "NullConditionalElementAccess"]

nullConditionalMemberAccess :: TypeDefinition
nullConditionalMemberAccess = def "NullConditionalMemberAccess" $
  doc "A null-conditional member access (?.member) with its chain of dependent accesses." $
  T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList",
  "dependentAccess">: T.list $ csharp "DependentAccess"]

nullConditionalProjectionInitializer :: TypeDefinition
nullConditionalProjectionInitializer = def "NullConditionalProjectionInitializer" $
  doc "A null-conditional member projection used inside an anonymous object initializer." $
  T.record [
  "expression">: csharp "PrimaryExpression",
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

numericType :: TypeDefinition
numericType = def "NumericType" $
  doc "A numeric type: integral, floating-point, or decimal." $
  T.union [
  "integral">: csharp "IntegralType",
  "floatingPoint">: csharp "FloatingPointType",
  "decimal">: T.unit]

objectCreationExpression :: TypeDefinition
objectCreationExpression = def "ObjectCreationExpression" $
  doc "A 'new' expression that constructs an object, with optional arguments and initializer." $
  T.record [
  "type">: csharp "Type",
  "arguments">: T.optional $ csharp "ArgumentList",
  "initializer">: T.optional $ csharp "ObjectOrCollectionInitializer"]

objectOrCollectionInitializer :: TypeDefinition
objectOrCollectionInitializer = def "ObjectOrCollectionInitializer" $
  doc "An object initializer's member list, or a collection initializer's element list." $
  T.union [
  "object">: T.list $ csharp "MemberInitializer",
  "collection">: T.list $ csharp "ElementInitializer"]

operatorBody :: TypeDefinition
operatorBody = def "OperatorBody" $
  doc "The body of an operator overload declaration: a block, expression body, or empty (extern) body." $
  T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

operatorDeclaration :: TypeDefinition
operatorDeclaration = def "OperatorDeclaration" $
  doc "A user-defined operator overload declaration, with its modifiers, declarator, and body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "OperatorModifier",
  "declarator">: csharp "OperatorDeclarator",
  "body">: csharp "OperatorBody"]

operatorDeclarator :: TypeDefinition
operatorDeclarator = def "OperatorDeclarator" $
  doc "The declarator of an operator overload: unary, binary, or conversion form." $
  T.union [
  "unary">: csharp "UnaryOperatorDeclarator",
  "binary">: csharp "BinaryOperatorDeclarator",
  "conversion">: csharp "ConversionOperatorDeclarator"]

operatorModifier :: TypeDefinition
operatorModifier = def "OperatorModifier" $
  doc "A modifier keyword on an operator declaration: public, static, extern, or unsafe." $
  T.enum [
  "public",
  "static",
  "extern",
  "unsafe"]

ordering :: TypeDefinition
ordering = def "Ordering" $
  doc "A single ordering key expression, with an optional ascending/descending direction, in an orderby clause." $
  T.record [
  "expression">: csharp "Expression",
  "direction">: T.optional $ csharp "OrderingDirection"]

orderingDirection :: TypeDefinition
orderingDirection = def "OrderingDirection" $
  doc "The ascending or descending direction of an ordering key in an orderby clause." $
  T.enum [
  "ascending",
  "descending"]

overloadableBinaryOperator :: TypeDefinition
overloadableBinaryOperator = def "OverloadableBinaryOperator" $
  doc "A binary operator symbol that may be overloaded via a user-defined operator declaration." $
  T.enum [
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
overloadableUnaryOperator = def "OverloadableUnaryOperator" $
  doc "A unary operator symbol that may be overloaded via a user-defined operator declaration." $
  T.enum [
  "plus",
  "minus",
  "not",
  "complement",
  "increment",
  "decrement",
  "true",
  "false"]

parameterArray :: TypeDefinition
parameterArray = def "ParameterArray" $
  doc "A trailing 'params' array parameter of a formal parameter list." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "type">: csharp "ArrayType",
  "identifier">: csharp "Identifier"]

parameterModeModifier :: TypeDefinition
parameterModeModifier = def "ParameterModeModifier" $
  doc "The ref, out, or in passing-mode modifier on a fixed parameter." $
  T.enum [
  "ref",
  "out",
  "in"]

parameterModifier :: TypeDefinition
parameterModifier = def "ParameterModifier" $
  doc "A modifier on a fixed parameter: a passing-mode modifier, or 'this' for extension methods." $
  T.union [
  "mode">: csharp "ParameterModeModifier",
  "this">: T.unit]

pattern_ :: TypeDefinition
pattern_ = def "Pattern" $
  doc "A pattern used in pattern matching: a declaration, constant, or var pattern." $
  T.union [
  "declaration">: csharp "DeclarationPattern",
  "constant">: csharp "Expression",
  "var">: csharp "Designation"]

positionalArgument :: TypeDefinition
positionalArgument = def "PositionalArgument" $
  doc "A single positional (or optionally named) argument in an attribute's argument list." $
  T.record [
  "name">: T.optional $ csharp "Identifier",
  "value">: csharp "AttributeArgumentExpression"]

positionalArgumentList :: TypeDefinition
positionalArgumentList = def "PositionalArgumentList" $
  doc "A non-empty, comma-separated list of positional arguments passed to an attribute." $
  T.wrap $ nonemptyList $ csharp "PositionalArgument"

predefinedType :: TypeDefinition
predefinedType = def "PredefinedType" $
  doc "One of the built-in predefined type keywords, such as int, string, or bool." $
  T.enum [
  "bool", "byte", "char", "decimal", "double", "float", "int", "long", "object", "sbyte", "short", "string",
  "uint", "ulong", "ushort"]

primaryConstraint :: TypeDefinition
primaryConstraint = def "PrimaryConstraint" $
  doc "The primary type-parameter constraint: a class type, or the class/struct/unmanaged constraint." $
  T.union [
  "classType">: csharp "ClassType",
  "class">: T.unit,
  "struct">: T.unit,
  "unmanaged">: T.unit]

primaryExpression :: TypeDefinition
primaryExpression = def "PrimaryExpression" $
  doc "An expression without an array creation, or an array creation expression." $
  T.union [
  "noArray">: csharp "PrimaryNoArrayCreationExpression",
  "array">: csharp "ArrayCreationExpression"]

primaryNoArrayCreationExpression :: TypeDefinition
primaryNoArrayCreationExpression = def "PrimaryNoArrayCreationExpression" $
  doc "A primary expression other than array creation: literal, invocation, access, and so on." $
  T.union [
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
propertyBody = def "PropertyBody" $
  doc "The body of a standard property: block accessors or a single expression body." $
  T.union [
  "block">: csharp "BlockPropertyBody",
  "expression">: csharp "Expression"]

propertyDeclaration :: TypeDefinition
propertyDeclaration = def "PropertyDeclaration" $
  doc "A property member declaration, in its standard or ref-returning form." $
  T.union [
  "standard">: csharp "StandardPropertyDeclaration",
  "refReturn">: csharp "RefReturnPropertyDeclaration"]

propertyModifier :: TypeDefinition
propertyModifier = def "PropertyModifier" $
  doc "A modifier keyword on a property declaration, such as virtual, override, or abstract." $
  T.enum [
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
qualifiedAliasMember = def "QualifiedAliasMember" $
  doc "An 'alias::member' reference, with optional type arguments, using an extern or global alias." $
  T.record [
  "alias">: csharp "Identifier",
  "member">: csharp "Identifier",
  "arguments">: T.optional $ csharp "TypeArgumentList"]

qualifiedIdentifier :: TypeDefinition
qualifiedIdentifier = def "QualifiedIdentifier" $
  doc "A non-empty, dot-separated sequence of identifiers, as used in a namespace declaration." $
  T.wrap $
  nonemptyList $ csharp "Identifier"

qualifiedNamespaceOrTypeName :: TypeDefinition
qualifiedNamespaceOrTypeName = def "QualifiedNamespaceOrTypeName" $
  doc "A namespace-or-type name qualified with a preceding namespace-or-type and identifier." $
  T.record [
  "namespaceOrType">: csharp "NamespaceOrTypeName",
  "identifier">: csharp "Identifier",
  "arguments">: T.optional $ csharp "TypeArgumentList"]

queryBody :: TypeDefinition
queryBody = def "QueryBody" $
  doc "The clauses, select/group clause, and optional continuation of a query expression." $
  T.record [
  "clauses">: T.list $ csharp "QueryBodyClause",
  "selectOrGroup">: csharp "SelectOrGroupClause",
  "continuation">: T.optional $ csharp "QueryContinuation"]

queryBodyClause :: TypeDefinition
queryBodyClause = def "QueryBodyClause" $
  doc "A clause in a query expression body: from, let, where, join, or orderby." $
  T.union [
  "from">: csharp "FromClause",
  "let">: csharp "LetClause",
  "where">: csharp "BooleanExpression",
  "join">: csharp "JoinClause",
  "orderby">: nonemptyList $ csharp "Ordering"]

queryContinuation :: TypeDefinition
queryContinuation = def "QueryContinuation" $
  doc "An 'into' continuation that feeds a query's results into a further query body." $
  T.record [
  "into">: csharp "Identifier",
  "body">: csharp "QueryBody"]

queryExpression :: TypeDefinition
queryExpression = def "QueryExpression" $
  doc "A LINQ query expression, starting with a from clause and continuing with a query body." $
  T.record [
  "from">: csharp "FromClause",
  "body">: csharp "QueryBody"]

rankSpecifier :: TypeDefinition
rankSpecifier = def "RankSpecifier" $
  doc "The rank (number of dimensions) of an array type or array creation expression." $
  T.wrap T.int32

rankSpecifierArrayCreationExpression :: TypeDefinition
rankSpecifierArrayCreationExpression = def "RankSpecifierArrayCreationExpression" $
  doc "Array creation using a rank specifier together with an array initializer, with inferred element type." $
  T.record [
  "rankSpecifier">: csharp "RankSpecifier",
  "initializer">: csharp "ArrayInitializer"]

refAccessorBody :: TypeDefinition
refAccessorBody = def "RefAccessorBody" $
  doc "The body of a ref-returning get accessor: a block, ref expression, or empty (abstract) body." $
  T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

refConditionalExpression :: TypeDefinition
refConditionalExpression = def "RefConditionalExpression" $
  doc "A ref-returning conditional expression, selecting between two variable references." $
  T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "VariableReference",
  "false">: csharp "VariableReference"]

refGetAccessorDeclaration :: TypeDefinition
refGetAccessorDeclaration = def "RefGetAccessorDeclaration" $
  doc "A ref-returning property or indexer get accessor declaration." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifier">: T.optional $ csharp "AccessorModifier",
  "body">: csharp "RefAccessorBody"]

refIndexerBody :: TypeDefinition
refIndexerBody = def "RefIndexerBody" $
  doc "The body of a ref-returning indexer: a ref get accessor, or a single ref expression." $
  T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

refIndexerDeclaration :: TypeDefinition
refIndexerDeclaration = def "RefIndexerDeclaration" $
  doc "A ref-returning indexer member declaration." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "refKind">: csharp "RefKind",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "RefIndexerBody"]

refKind :: TypeDefinition
refKind = def "RefKind" $
  doc "Whether a ref-returning member returns by plain ref or by ref readonly." $
  T.enum [
  "ref",
  "refReadonly"]

refLocalFunctionBody :: TypeDefinition
refLocalFunctionBody = def "RefLocalFunctionBody" $
  doc "The body of a ref-returning local function: a block, or a single ref expression." $
  T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference"]

refLocalFunctionDeclaration :: TypeDefinition
refLocalFunctionDeclaration = def "RefLocalFunctionDeclaration" $
  doc "A ref-returning local function declaration." $
  T.record [
  "modifiers">: T.list $ csharp "RefLocalFunctionModifier",
  "refKind">: csharp "RefKind",
  "returnType">: csharp "Type",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "RefLocalFunctionBody"]

refLocalFunctionModifier :: TypeDefinition
refLocalFunctionModifier = def "RefLocalFunctionModifier" $
  doc "A modifier on a ref-returning local function declaration: static or unsafe." $
  T.enum [
  "static",
  "unsafe"]

refLocalVariableDeclaration :: TypeDefinition
refLocalVariableDeclaration = def "RefLocalVariableDeclaration" $
  doc "A local variable declaration that binds a variable by reference." $
  T.record [
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "declarators">: nonemptyList $ csharp "RefLocalVariableDeclarator"]

refLocalVariableDeclarator :: TypeDefinition
refLocalVariableDeclarator = def "RefLocalVariableDeclarator" $
  doc "The identifier and referenced variable in a single ref local variable declarator." $
  T.record [
  "left">: csharp "Identifier",
  "right">: csharp "VariableReference"]

refMethodBody :: TypeDefinition
refMethodBody = def "RefMethodBody" $
  doc "The body of a ref-returning method: a block, ref expression, or empty (extern) body." $
  T.union [
  "block">: csharp "Block",
  "ref">: csharp "VariableReference",
  "empty">: T.unit]

refMethodModifier :: TypeDefinition
refMethodModifier = def "RefMethodModifier" $
  doc "A modifier keyword on a ref-returning method declaration." $
  T.enum [
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
refPropertyBody = def "RefPropertyBody" $
  doc "The body of a ref-returning property: a ref get accessor, or a single ref expression." $
  T.union [
  "block">: csharp "RefGetAccessorDeclaration",
  "ref">: csharp "VariableReference"]

refReturnMethodDeclaration :: TypeDefinition
refReturnMethodDeclaration = def "RefReturnMethodDeclaration" $
  doc "A ref-returning method declaration, including its ref kind, header, and body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "RefMethodModifier",
  "kind">: csharp "RefKind",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "RefMethodBody"]

refReturnPropertyDeclaration :: TypeDefinition
refReturnPropertyDeclaration = def "RefReturnPropertyDeclaration" $
  doc "A ref-returning property declaration, including its ref kind and body." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "refKind">: csharp "RefKind",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "RefPropertyBody"]

refVarImplicitlyTypedLocalVariableDeclaration :: TypeDefinition
refVarImplicitlyTypedLocalVariableDeclaration = def "RefVarImplicitlyTypedLocalVariableDeclaration" $
  doc "An implicitly typed local variable declared by reference using 'ref var'." $
  T.record [
  "refKind">: csharp "RefKind",
  "declarator">: csharp "RefLocalVariableDeclarator"]

referenceType :: TypeDefinition
referenceType = def "ReferenceType" $
  doc "A reference type: class, interface, array, delegate, or the dynamic type." $
  T.union [
  "class">: csharp "ClassType",
  "interface">: csharp "InterfaceType",
  "array">: csharp "ArrayType",
  "delegate">: csharp "DelegateType",
  "dynamic">: T.unit]

regularInterpolation :: TypeDefinition
regularInterpolation = def "RegularInterpolation" $
  doc "A single '{expression[,width][:format]}' hole within a regular interpolated string." $
  T.record [
  "expression">: csharp "Expression",
  "width">: T.optional $ csharp "Expression",
  "format">: T.optional T.string]

relationalExpression :: TypeDefinition
relationalExpression = def "RelationalExpression" $
  doc "A simple, binary, is-type, is-pattern, or as-type relational expression." $
  T.union [
  "simple">: csharp "ShiftExpression",
  "binary">: csharp "BinaryRelationalExpression",
  "isType">: csharp "IsTypeExpression",
  "isPattern">: csharp "IsPatternExpression",
  "asType">: csharp "AsTypeExpression"]

relationalOperator :: TypeDefinition
relationalOperator = def "RelationalOperator" $
  doc "A relational comparison operator: less than, greater than, or their or-equal variants." $
  T.enum [
  "lessThan", "greaterThan", "lessThanOrEqual", "greaterThanOrEqual"]

resourceAcquisition :: TypeDefinition
resourceAcquisition = def "ResourceAcquisition" $
  doc "The resource acquired by a using statement: a local variable declaration or an expression." $
  T.union [
  "local">: csharp "LocalVariableDeclaration",
  "expression">: csharp "Expression"]

returnStatement :: TypeDefinition
returnStatement = def "ReturnStatement" $
  doc "A return statement, in its value-less, value-returning, or ref-returning form." $
  T.union [
  "simple">: T.unit,
  "value">: csharp "Expression",
  "ref">: csharp "VariableReference"]

returnType :: TypeDefinition
returnType = def "ReturnType" $
  doc "The declared return type of a member: a ref-qualified type, or void." $
  T.union [
  "ref">: csharp "Type",
  "void">: T.unit]

secondaryConstraint :: TypeDefinition
secondaryConstraint = def "SecondaryConstraint" $
  doc "A secondary type-parameter constraint: an interface type or another type parameter." $
  T.union [
  "interface">: csharp "InterfaceType",
  "parameter">: csharp "TypeParameter"]

secondaryConstraints :: TypeDefinition
secondaryConstraints = def "SecondaryConstraints" $
  doc "A non-empty list of secondary constraints on a type parameter." $
  T.wrap $ nonemptyList $ csharp "SecondaryConstraint"

selectOrGroupClause :: TypeDefinition
selectOrGroupClause = def "SelectOrGroupClause" $
  doc "The final clause of a query body: a 'select' projection or a 'group by' clause." $
  T.union [
  "select">: csharp "Expression",
  "group">: csharp "GroupClause"]

selectionStatement :: TypeDefinition
selectionStatement = def "SelectionStatement" $
  doc "An if or switch statement." $
  T.union [
  "if">: csharp "IfStatement",
  "switch">: csharp "SwitchStatement"]

shiftExpression :: TypeDefinition
shiftExpression = def "ShiftExpression" $
  doc "A simple additive expression or a binary left/right-shift expression." $
  T.union [
  "simple">: csharp "AdditiveExpression",
  "binary">: csharp "BinaryShiftExpression"]

shiftOperator :: TypeDefinition
shiftOperator = def "ShiftOperator" $
  doc "The left-shift (<<) or right-shift (>>) operator." $
  T.enum [
  "left",
  "right"]

simpleConditionalExpression :: TypeDefinition
simpleConditionalExpression = def "SimpleConditionalExpression" $
  doc "The ordinary ternary conditional expression: 'condition ? true : false'." $
  T.record [
  "condition">: csharp "NullCoalescingExpression",
  "true">: csharp "Expression",
  "false">: csharp "Expression"]

simpleName :: TypeDefinition
simpleName = def "SimpleName" $
  doc "A simple identifier reference, with an optional list of explicit type arguments." $
  T.record [
  "identifier">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

simpleType :: TypeDefinition
simpleType = def "SimpleType" $
  doc "A numeric type or the bool type." $
  T.union [
  "numeric">: csharp "NumericType",
  "bool">: T.unit]

specificCatchClause :: TypeDefinition
specificCatchClause = def "SpecificCatchClause" $
  doc "A catch clause naming an exception type and/or filter, with a handler block." $
  T.record [
  "specifier">: T.optional $ csharp "ExceptionSpecifier",
  "filter">: T.optional $ csharp "BooleanExpression",
  "body">: csharp "Block"]

stackallocExpression :: TypeDefinition
stackallocExpression = def "StackallocExpression" $
  doc "A 'stackalloc' expression that allocates a block of memory on the stack." $
  T.record [
  "type">: T.optional $ csharp "UnmanagedType",
  "expression">: T.optional $ csharp "ConstantExpression",
  "initializer">: T.list $ csharp "Expression"]

standardEventDeclaration :: TypeDefinition
standardEventDeclaration = def "StandardEventDeclaration" $
  doc "A field-like event declaration, with its type and one or more declarators." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "EventModifier",
  "type">: csharp "Type",
  "declarators">: csharp "VariableDeclarators"]

standardIndexerDeclaration :: TypeDefinition
standardIndexerDeclaration = def "StandardIndexerDeclaration" $
  doc "A standard (non-ref-returning) indexer declaration." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "IndexerModifier",
  "declarator">: csharp "IndexerDeclarator",
  "body">: csharp "IndexerBody"]

standardLocalFunctionDeclaration :: TypeDefinition
standardLocalFunctionDeclaration = def "StandardLocalFunctionDeclaration" $
  doc "A standard (non-ref-returning) local function declaration." $
  T.record [
  "modifiers">: T.list $ csharp "LocalFunctionModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "LocalFunctionHeader",
  "body">: csharp "LocalFunctionBody"]

standardMethodDeclaration :: TypeDefinition
standardMethodDeclaration = def "StandardMethodDeclaration" $
  doc "A standard (non-ref-returning) method declaration." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "MethodModifier",
  "returnType">: csharp "ReturnType",
  "header">: csharp "MethodHeader",
  "body">: csharp "MethodBody"]

standardPropertyDeclaration :: TypeDefinition
standardPropertyDeclaration = def "StandardPropertyDeclaration" $
  doc "A standard (non-ref-returning) property declaration." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: T.list $ csharp "PropertyModifier",
  "type">: csharp "Type",
  "name">: csharp "MemberName",
  "body">: csharp "PropertyBody"]

statement :: TypeDefinition
statement = def "Statement" $
  doc "A labeled statement, a declaration statement, or an embedded statement." $
  T.union [
  "labeled">: csharp "LabeledStatement",
  "declaration">: csharp "DeclarationStatement",
  "embedded">: csharp "EmbeddedStatement"]

statementExpression :: TypeDefinition
statementExpression = def "StatementExpression" $
  doc "An expression usable as a statement on its own, such as an invocation or assignment." $
  T.union [
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
statementExpressionList = def "StatementExpressionList" $
  doc "A non-empty, comma-separated list of statement expressions, as used in a for statement." $
  T.wrap $ nonemptyList $ csharp "StatementExpression"

staticConstructorBody :: TypeDefinition
staticConstructorBody = def "StaticConstructorBody" $
  doc "The body of a static constructor: a block, expression body, or empty (extern) body." $
  T.union [
  "block">: csharp "Block",
  "expression">: csharp "Expression",
  "empty">: T.unit]

staticConstructorDeclaration :: TypeDefinition
staticConstructorDeclaration = def "StaticConstructorDeclaration" $
  doc "A static constructor declaration for a type." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: csharp "StaticConstructorModifiers",
  "name">: csharp "Identifier",
  "body">: csharp "StaticConstructorBody"]

staticConstructorModifiers :: TypeDefinition
staticConstructorModifiers = def "StaticConstructorModifiers" $
  doc "The extern and unsafe flags of a static constructor declaration." $
  T.record [
  "extern">: T.boolean,
  "unsafe">: T.boolean]

structDeclaration :: TypeDefinition
structDeclaration = def "StructDeclaration" $
  doc "A struct type declaration, with its modifiers, type parameters, interfaces, and body." $
  T.record [
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
structMemberDeclaration = def "StructMemberDeclaration" $
  doc "Any member that can appear in a struct body, including fixed-size buffer declarations." $
  T.union [
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
structModifier = def "StructModifier" $
  doc "A modifier keyword on a struct declaration, such as readonly or unsafe." $
  T.enum [
  "new",
  "public",
  "protected",
  "internal",
  "private",
  "readonly",
  "unsafe"]

structOrEnumType :: TypeDefinition
structOrEnumType = def "StructOrEnumType" $
  doc "A struct type or an enum type, as used in the definition of value types." $
  T.union [
  "struct">: csharp "StructType",
  "enum">: csharp "EnumType"]

structType :: TypeDefinition
structType = def "StructType" $
  doc "A reference to a struct type: a named type, a simple type, or a tuple type." $
  T.union [
  "typeName">: csharp "TypeName",
  "simple">: csharp "SimpleType",
  "tuple">: csharp "TupleType"]

switchBranch :: TypeDefinition
switchBranch = def "SwitchBranch" $
  doc "A pattern and optional guard expression used in a pattern-matching switch section." $
  T.record [
  "pattern">: csharp "Pattern",
  "guard">: T.optional $ csharp "Expression"]

switchLabel :: TypeDefinition
switchLabel = def "SwitchLabel" $
  doc "A single label of a switch section: a pattern branch, or the default label." $
  T.union [
  "branch">: csharp "SwitchBranch",
  "default">: T.unit]

switchSection :: TypeDefinition
switchSection = def "SwitchSection" $
  doc "A group of switch labels sharing a common statement list within a switch statement." $
  T.record [
  "labels">: nonemptyList $ csharp "SwitchLabel",
  "statements">: T.list $ csharp "Statement"]

switchStatement :: TypeDefinition
switchStatement = def "SwitchStatement" $
  doc "A switch statement, dispatching on an expression across its switch sections." $
  T.record [
  "expression">: csharp "Expression",
  "branches">: T.list $ csharp "SwitchSection"]

tryStatement :: TypeDefinition
tryStatement = def "TryStatement" $
  doc "A try statement with its protected block, catch clauses, and optional finally block." $
  T.record [
  "body">: csharp "Block",
  "catches">: csharp "CatchClauses",
  "finally">: T.optional $ csharp "Block"]

tupleElement :: TypeDefinition
tupleElement = def "TupleElement" $
  doc "A single element of a tuple expression, with an optional name." $
  T.record [
  "name">: T.optional $ csharp "Identifier",
  "expression">: csharp "Expression"]

tupleExpression :: TypeDefinition
tupleExpression = def "TupleExpression" $
  doc "A tuple literal expression, either a list of named elements or a deconstruction tuple." $
  T.union [
  "elements">: nonemptyList $ csharp "TupleElement",
  "deconstruction">: csharp "DeconstructionTuple"]

tupleType :: TypeDefinition
tupleType = def "TupleType" $
  doc "A tuple type formed from a non-empty list of named or unnamed element types." $
  T.wrap $
  nonemptyList $ csharp "TupleTypeElement"

tupleTypeElement :: TypeDefinition
tupleTypeElement = def "TupleTypeElement" $
  doc "A single element type of a tuple type, with an optional element name." $
  T.record [
  "type">: csharp "Type",
  "identifier">: T.optional $ csharp "Identifier"]

typeArgumentList :: TypeDefinition
typeArgumentList = def "TypeArgumentList" $
  doc "A list of type arguments supplied to a generic type or method." $
  T.wrap $ T.list $ csharp "Type"

typeDeclaration :: TypeDefinition
typeDeclaration = def "TypeDeclaration" $
  doc "A top-level or nested type declaration: class, struct, interface, enum, or delegate." $
  T.union [
  "class">: csharp "ClassDeclaration",
  "struct">: csharp "StructDeclaration",
  "interface">: csharp "InterfaceDeclaration",
  "enum">: csharp "EnumDeclaration",
  "delegate">: csharp "DelegateDeclaration"]

typeName :: TypeDefinition
typeName = def "TypeName" $
  doc "A name that is known to refer to a type, expressed as a namespace-or-type name." $
  T.wrap $
  csharp "NamespaceOrTypeName"

typeParameter :: TypeDefinition
typeParameter = def "TypeParameter" $
  doc "A single generic type parameter, identified by its name." $
  T.wrap $
  csharp "Identifier"

typeParameterConstraints :: TypeDefinition
typeParameterConstraints = def "TypeParameterConstraints" $
  doc "The primary, secondary, and constructor constraints on a single type parameter." $
  T.record [
  "primary">: T.optional $ csharp "PrimaryConstraint",
  "secondary">: T.optional $ csharp "SecondaryConstraints",
  "constructor">: T.boolean]

typeParameterConstraintsClause :: TypeDefinition
typeParameterConstraintsClause = def "TypeParameterConstraintsClause" $
  doc "A 'where T : ...' clause constraining one type parameter of a generic declaration." $
  T.record [
  "parameter">: csharp "TypeParameter",
  "constraints">: T.list $ csharp "TypeParameterConstraints"]

typeParameterList :: TypeDefinition
typeParameterList = def "TypeParameterList" $
  doc "A non-empty, comma-separated list of type parameters in a generic declaration." $
  T.wrap $ nonemptyList $ csharp "TypeParameterPart"

typeParameterPart :: TypeDefinition
typeParameterPart = def "TypeParameterPart" $
  doc "A single type parameter within a type parameter list, with its optional attributes." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "name">: csharp "TypeParameter"]

type_ :: TypeDefinition
type_ = def "Type" $
  doc "A C# type: a reference type, value type, type parameter, or pointer type." $
  T.union [
  "reference">: csharp "ReferenceType",
  "value">: csharp "ValueType",
  "param">: csharp "TypeParameter",
  "pointer">: csharp "PointerType"]

typeofExpression :: TypeDefinition
typeofExpression = def "TypeofExpression" $
  doc "A 'typeof' expression yielding the System.Type of a type, unbound generic type, or void." $
  T.union [
  "type">: csharp "Type",
  "unboundTypeName">: csharp "UnboundTypeName",
  "void">: T.unit]

unaryExpression :: TypeDefinition
unaryExpression = def "UnaryExpression" $
  doc "A unary expression: a primary expression, or a prefixed unary/cast/await operation." $
  T.union [
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
unaryOperatorDeclarator = def "UnaryOperatorDeclarator" $
  doc "The declarator for a user-defined unary operator overload, naming its single parameter." $
  T.record [
  "type">: csharp "Type",
  "operator">: csharp "OverloadableUnaryOperator",
  "parameter">: csharp "FixedParameter"]

unboundTypeName :: TypeDefinition
unboundTypeName = def "UnboundTypeName" $
  doc "An unbound generic type name, as used in a typeof expression (e.g. Dictionary<,>)." $
  T.wrap $
  nonemptyList $ csharp "UnboundTypeNamePart"

unboundTypeNamePart :: TypeDefinition
unboundTypeNamePart = def "UnboundTypeNamePart" $
  doc "A single segment of an unbound type name, with its aliasing flag and generic dimension." $
  T.record [
  "identifier">: csharp "Identifier",
  "aliased">: T.boolean,
  "dimension">: T.optional T.int32]

unmanagedType :: TypeDefinition
unmanagedType = def "UnmanagedType" $
  doc "A type usable with the unsafe 'sizeof' operator: a value type or pointer type." $
  T.union [
  "value">: csharp "ValueType",
  "pointer">: csharp "PointerType"]

usingAliasDirective :: TypeDefinition
usingAliasDirective = def "UsingAliasDirective" $
  doc "A 'using alias = namespace-or-type;' directive introducing a using alias." $
  T.record [
  "alias">: csharp "Identifier",
  "name">: csharp "NamespaceOrTypeName"]

usingDirective :: TypeDefinition
usingDirective = def "UsingDirective" $
  doc "A using directive: an alias directive, a namespace import, or a using-static directive." $
  T.union [
  "alias">: csharp "UsingAliasDirective",
  "namespace">: csharp "NamespaceName",
  "static">: csharp "TypeName"]

usingStatement :: TypeDefinition
usingStatement = def "UsingStatement" $
  doc "A 'using' statement that deterministically disposes an acquired resource after its body." $
  T.record [
  "acquisition">: csharp "ResourceAcquisition",
  "body">: csharp "EmbeddedStatement"]

valueType :: TypeDefinition
valueType = def "ValueType" $
  doc "A struct or enum type, in its non-nullable or nullable ('?') form." $
  T.union [
  "nonNullable">: csharp "StructOrEnumType",
  "nullable">: csharp "StructOrEnumType"]

variableDeclarator :: TypeDefinition
variableDeclarator = def "VariableDeclarator" $
  doc "An identifier and optional initializer within a field or local variable declaration." $
  T.record [
  "identifier">: csharp "Identifier",
  "initializer">: T.optional $ csharp "VariableInitializer"]

variableDeclarators :: TypeDefinition
variableDeclarators = def "VariableDeclarators" $
  doc "A non-empty, comma-separated list of variable declarators, as used in an event declaration." $
  T.wrap $ nonemptyList $ csharp "VariableDeclarator"

variableInitializer :: TypeDefinition
variableInitializer = def "VariableInitializer" $
  doc "The initializer of a variable: an expression or a nested array initializer." $
  T.union [
  "expression">: csharp "Expression",
  "array">: csharp "ArrayInitializer"]

variableReference :: TypeDefinition
variableReference = def "VariableReference" $
  doc "A reference to a variable, expressed as an expression." $
  T.wrap $
  csharp "Expression"

varianceAnnotation :: TypeDefinition
varianceAnnotation = def "VarianceAnnotation" $
  doc "The in (contravariant) or out (covariant) variance annotation on a generic type parameter." $
  T.enum [
  "in",
  "out"]

variantTypeParameter :: TypeDefinition
variantTypeParameter = def "VariantTypeParameter" $
  doc "A single type parameter of an interface or delegate, with optional variance annotation." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "variance">: T.optional $ csharp "VarianceAnnotation",
  "parameter">: csharp "TypeParameter"]

variantTypeParameters :: TypeDefinition
variantTypeParameters = def "VariantTypeParameters" $
  doc "The list of variant type parameters declared by a generic interface or delegate." $
  T.wrap $ T.list $ csharp "VariantTypeParameter"

verbatimInterpolation :: TypeDefinition
verbatimInterpolation = def "VerbatimInterpolation" $
  doc "A single '{expression[,width][:format]}' hole within a verbatim interpolated string." $
  T.record [
  "expression">: csharp "Expression",
  "width">: T.optional $ csharp "ConstantExpression",
  "format">: T.optional T.string]

whileStatement :: TypeDefinition
whileStatement = def "WhileStatement" $
  doc "A while iteration statement that tests its condition before each execution of the body." $
  T.record [
  "condition">: csharp "BooleanExpression",
  "body">: csharp "EmbeddedStatement"]

yieldStatement :: TypeDefinition
yieldStatement = def "YieldStatement" $
  doc "A 'yield return' or 'yield break' statement used within an iterator." $
  T.union [
  "return">: csharp "Expression",
  "break">: T.unit]

-- Unsafe Elements

fixedPointerDeclarator :: TypeDefinition
fixedPointerDeclarator = def "FixedPointerDeclarator" $
  doc "A single declarator in a fixed statement, binding a pointer to a variable or expression." $
  T.union [
  "reference">: csharp "VariableReference",
  "expression">: csharp "Expression"]

fixedSizeBufferDeclaration :: TypeDefinition
fixedSizeBufferDeclaration = def "FixedSizeBufferDeclaration" $
  doc "An unsafe fixed-size buffer field declaration inside a struct." $
  T.record [
  "attributes">: T.optional $ csharp "Attributes",
  "modifiers">: nonemptyList $ csharp "FixedSizeBufferModifier",
  "elementType">: csharp "Type",
  "declarators">: nonemptyList $ csharp "FixedSizeBufferDeclarator"]

fixedSizeBufferDeclarator :: TypeDefinition
fixedSizeBufferDeclarator = def "FixedSizeBufferDeclarator" $
  doc "The name and constant size of a single fixed-size buffer member." $
  T.record [
  "name">: csharp "Identifier",
  "size">: csharp "ConstantExpression"]

fixedSizeBufferModifier :: TypeDefinition
fixedSizeBufferModifier = def "FixedSizeBufferModifier" $
  doc "An access or unsafe modifier on a fixed-size buffer declaration." $
  T.enum [
  "new",
  "public",
  "internal",
  "private",
  "unsafe"]

fixedStatement :: TypeDefinition
fixedStatement = def "FixedStatement" $
  doc "An unsafe 'fixed' statement that pins one or more variables for pointer access." $
  T.record [
  "pointerType">: csharp "PointerType",
  "declarators">: nonemptyList $ csharp "FixedPointerDeclarator",
  "statement">: csharp "EmbeddedStatement"]

pointerElementAccess :: TypeDefinition
pointerElementAccess = def "PointerElementAccess" $
  doc "An unsafe pointer indexing expression (pointer[index])." $
  T.record [
  "pointer">: csharp "PrimaryNoArrayCreationExpression",
  "index">: csharp "Expression"]

pointerMemberAccess :: TypeDefinition
pointerMemberAccess = def "PointerMemberAccess" $
  doc "An unsafe pointer member access expression (pointer->member)." $
  T.record [
  "pointer">: csharp "PrimaryExpression",
  "member">: csharp "Identifier",
  "typeArguments">: T.optional $ csharp "TypeArgumentList"]

pointerType :: TypeDefinition
pointerType = def "PointerType" $
  doc "An unsafe pointer type, formed from a pointee value type and a pointer depth." $
  T.union [
  "valueType">: T.optional $ csharp "ValueType",
  "pointerDepth">: T.int32]
