
-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Sources.Cpp.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
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
ns = ModuleName "hydra.cpp.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleMetadata = descriptionMetadata (Just "A C++ syntax model, focusing on features for representing algebraic data types and declarative computations")}
  where
    definitions = [
      accessSpecifier,
      addOperation,
      additiveExpression,
      andExpression,
      assignmentExpression,
      assignmentOperator,
      baseSpecifier,
      basicType,
      binaryOperator,
      bitwiseAndOperation,
      bitwiseOrOperation,
      bitwiseXorOperation,
      booleanLiteral,
      capture,
      captureList,
      caseStatement,
      caseValue,
      characterLiteral,
      classBody,
      classDeclaration,
      classKey,
      classSpecifier,
      commaExpression,
      comment,
      compoundStatement,
      conditionalExpression,
      constructorDeclaration,
      containerDeclaration,
      declaration,
      defineDirective,
      destructorDeclaration,
      divideOperation,
      doStatement,
      elifDirective,
      elseDirective,
      endifDirective,
      equalOperation,
      equalityExpression,
      errorDirective,
      exclusiveOrExpression,
      explicitAssignment,
      expression,
      expressionStatement,
      floatingLiteral,
      forInit,
      forStatement,
      functionApplication,
      functionBody,
      functionCallOperation,
      functionDeclaration,
      functionIdentifier,
      functionSpecifierPrefix,
      functionSpecifierSuffix,
      functionType_,
      greaterEqualOperation,
      greaterOperation,
      identifier_,
      ifDirective,
      ifdefDirective,
      ifndefDirective,
      includeDirective,
      inclusiveOrExpression,
      integerLiteral,
      iterationStatement,
      jumpStatement,
      labeledStatement,
      lambdaExpression,
      leftShiftOperation,
      lessEqualOperation,
      lessOperation,
      lineDirective,
      listDeclaration,
      literal,
      logicalAndExpression,
      logicalAndOperation,
      logicalOrExpression,
      logicalOrOperation,
      map_,
      mapDeclaration,
      mapEntry,
      memInitializer,
      memberAccessOperation,
      memberDeclaration,
      memberSpecification,
      moduloOperation,
      multiplicativeExpression,
      multiplyOperation,
      namespaceDeclaration,
      notEqualOperation,
      optional,
      optionalDeclaration,
      overloadedLambdas,
      parameter,
      patternMatch,
      pointerMemberAccessOperation,
      postfixExpression,
      pragmaDirective,
      preprocessorDirective,
      primaryExpression,
      productDeclaration,
      program,
      qualifiedIdentifier,
      qualifiedType,
      rangeForStatement,
      relationalExpression,
      rightShiftOperation,
      selectionStatement,
      set_,
      setDeclaration,
      shiftExpression,
      sizeofExpression,
      statement,
      stringLiteral,
      subscriptOperation,
      subtractOperation,
      switchStatement,
      templateArgument,
      templateDeclaration,
      templateFunctionCallOperation,
      templateType,
      ternaryExpression,
      typeExpression,
      typeQualifier,
      typedefDeclaration,
      unaryExpression,
      unaryOperation,
      unaryOperator,
      undefDirective,
      variableDeclaration,
      variantDeclaration,
      vector,
      visitor,
      warningDirective,
      whileStatement]

-- Access specifiers
accessSpecifier :: TypeDefinition
accessSpecifier = define "AccessSpecifier" $
  doc "A C++ member or base-class access specifier: public, protected, or private" $
  T.enum ["public", "protected", "private", "none"]

addOperation :: TypeDefinition
addOperation = define "AddOperation" $
  doc "A C++ binary addition expression (left + right)" $
  T.record [
  "left">: cpp "AdditiveExpression",
  "right">: cpp "MultiplicativeExpression"]

additiveExpression :: TypeDefinition
additiveExpression = define "AdditiveExpression" $
  doc "A C++ additive-expression: a multiplicative expression, an addition, or a subtraction" $
  T.union [
  "multiplicative">: cpp "MultiplicativeExpression",
  "add">: cpp "AddOperation",
  "subtract">: cpp "SubtractOperation"]

andExpression :: TypeDefinition
andExpression = define "AndExpression" $
  doc "A C++ and-expression: an equality expression or a bitwise AND operation" $
  T.union [
  "equality">: cpp "EqualityExpression",
  "bitwiseAnd">: cpp "BitwiseAndOperation"]

assignmentExpression :: TypeDefinition
assignmentExpression = define "AssignmentExpression" $
  doc "A C++ assignment-expression: a conditional expression or an explicit assignment" $
  T.union [
  "conditional">: cpp "ConditionalExpression",
  "assignment">: cpp "ExplicitAssignment"]

assignmentOperator :: TypeDefinition
assignmentOperator = define "AssignmentOperator" $
  doc "A C++ assignment operator, such as =, +=, or <<=" $
  T.enum [
  "assign", "plusAssign", "minusAssign", "multiplyAssign", "divideAssign",
  "moduloAssign", "leftShiftAssign", "rightShiftAssign", "bitwiseAndAssign",
  "bitwiseXorAssign", "bitwiseOrAssign"]

baseSpecifier :: TypeDefinition
baseSpecifier = define "BaseSpecifier" $
  doc "A C++ base class specifier within a class's inheritance list" $
  T.record [
  "access">: cpp "AccessSpecifier",
  "name">: T.string]

basicType :: TypeDefinition
basicType = define "BasicType" $
  doc "A C++ built-in or named basic type" $
  T.union [
  "void">: T.unit,
  "bool">: T.unit,
  "char">: T.unit,
  "int">: T.unit,
  "float">: T.unit,
  "double">: T.unit,
  "string">: T.unit,
  "auto">: T.unit,
  "named">: T.string]

-- Operator types
binaryOperator :: TypeDefinition
binaryOperator = define "BinaryOperator" $
  doc "A C++ binary operator" $
  T.enum [
  "plus", "minus", "multiply", "divide", "modulo",
  "bitwiseAnd", "bitwiseOr", "bitwiseXor",
  "logicalAnd", "logicalOr",
  "equal", "notEqual", "less", "greater", "lessEqual", "greaterEqual",
  "leftShift", "rightShift"]

bitwiseAndOperation :: TypeDefinition
bitwiseAndOperation = define "BitwiseAndOperation" $
  doc "A C++ bitwise AND expression (left & right)" $
  T.record [
  "left">: cpp "AndExpression",
  "right">: cpp "EqualityExpression"]

bitwiseOrOperation :: TypeDefinition
bitwiseOrOperation = define "BitwiseOrOperation" $
  doc "A C++ bitwise OR expression (left | right)" $
  T.record [
  "left">: cpp "InclusiveOrExpression",
  "right">: cpp "ExclusiveOrExpression"]

bitwiseXorOperation :: TypeDefinition
bitwiseXorOperation = define "BitwiseXorOperation" $
  doc "A C++ bitwise XOR expression (left ^ right)" $
  T.record [
  "left">: cpp "ExclusiveOrExpression",
  "right">: cpp "AndExpression"]

booleanLiteral :: TypeDefinition
booleanLiteral = define "BooleanLiteral" $
  doc "A C++ boolean literal (true or false)" $
  T.wrap T.boolean

capture :: TypeDefinition
capture = define "Capture" $
  doc "A single variable captured by a C++ lambda expression" $
  T.record [
  "name">: T.string,
  "byReference">: T.boolean]

captureList :: TypeDefinition
captureList = define "CaptureList" $
  doc "A C++ lambda capture list: capture-by-value ([=]) or an explicit list of captures" $
  T.union [
  "captureByValue">: T.unit,
  "captures">: T.list $ cpp "Capture"]

caseStatement :: TypeDefinition
caseStatement = define "CaseStatement" $
  doc "A single case or default branch of a C++ switch statement" $
  T.union [
  "case">: cpp "CaseValue",
  "default">: cpp "Statement"]

caseValue :: TypeDefinition
caseValue = define "CaseValue" $
  doc "A C++ switch case label together with its associated statement" $
  T.record [
  "value">: cpp "Expression",
  "statement">: cpp "Statement"]

characterLiteral :: TypeDefinition
characterLiteral = define "CharacterLiteral" $
  doc "A C++ character literal" $
  T.wrap T.string

classBody :: TypeDefinition
classBody = define "ClassBody" $
  doc "The body of a C++ class, struct, or enum declaration: a list of member specifications" $
  T.wrap $ T.list $ cpp "MemberSpecification"

classDeclaration :: TypeDefinition
classDeclaration = define "ClassDeclaration" $
  doc "A C++ class, struct, or enum declaration, with an optional body" $
  T.record [
  "specifier">: cpp "ClassSpecifier",
  "body">: T.optional $ cpp "ClassBody"]

classKey :: TypeDefinition
classKey = define "ClassKey" $
  doc "The keyword introducing a C++ class-like declaration: class, enum, enum class, or struct" $
  T.enum ["class", "enum", "enumClass", "struct"]

classSpecifier :: TypeDefinition
classSpecifier = define "ClassSpecifier" $
  doc "The head of a C++ class declaration: its key, name, and base class list" $
  T.record [
  "key">: cpp "ClassKey",
  "name">: T.string,
  "inheritance">: T.list $ cpp "BaseSpecifier"]

commaExpression :: TypeDefinition
commaExpression = define "CommaExpression" $
  doc "A C++ comma expression (left, right)" $
  T.record [
  "left">: cpp "Expression",
  "right">: cpp "AssignmentExpression"]

comment :: TypeDefinition
comment = define "Comment" $
  doc "A C++ line or block comment" $
  T.record [
  "text">: T.string,
  "isMultiline">: T.boolean]

compoundStatement :: TypeDefinition
compoundStatement = define "CompoundStatement" $
  doc "A C++ compound statement: a brace-enclosed block of statements" $
  T.wrap $ T.list $ cpp "Statement"

conditionalExpression :: TypeDefinition
conditionalExpression = define "ConditionalExpression" $
  doc "A C++ conditional-expression: a logical-or expression or a ternary expression" $
  T.union [
  "logicalOr">: cpp "LogicalOrExpression",
  "ternary">: cpp "TernaryExpression"]

constructorDeclaration :: TypeDefinition
constructorDeclaration = define "ConstructorDeclaration" $
  doc "A C++ constructor declaration, with parameters, member initializers, and a body" $
  T.record [
  "name">: T.string,
  "parameters">: T.list $ cpp "Parameter",
  "initializers">: T.list $ cpp "MemInitializer",
  "body">: cpp "FunctionBody"]

containerDeclaration :: TypeDefinition
containerDeclaration = define "ContainerDeclaration" $
  doc "A declaration of a standard-library container type alias: list, map, set, or optional" $
  T.union [
  "list">: cpp "ListDeclaration",
  "map">: cpp "MapDeclaration",
  "set">: cpp "SetDeclaration",
  "optional">: cpp "OptionalDeclaration"]

cpp :: String -> Type
cpp = typeref ns

declaration :: TypeDefinition
declaration = define "Declaration" $
  doc "A top-level C++ declaration" $
  T.union [
  "preprocessor">: cpp "PreprocessorDirective",
  "class">: cpp "ClassDeclaration",
  "function">: cpp "FunctionDeclaration",
  "variable">: cpp "VariableDeclaration",
  "typedef">: cpp "TypedefDeclaration",
  "namespace">: cpp "NamespaceDeclaration",
  "template">: cpp "TemplateDeclaration"]

defineDirective :: TypeDefinition
defineDirective = define "DefineDirective" $
  doc "A C preprocessor #define directive, optionally function-like" $
  T.record [
  "name">: T.string,
  "parameters">: T.optional $ T.list T.string,
  "replacement">: T.optional T.string]

destructorDeclaration :: TypeDefinition
destructorDeclaration = define "DestructorDeclaration" $
  doc "A C++ destructor declaration" $
  T.record [
  "prefixSpecifiers" >: T.list (cpp "FunctionSpecifierPrefix"),
  "name" >: T.string,
  "suffixSpecifiers" >: T.list (cpp "FunctionSpecifierSuffix"),
  "body" >: cpp "FunctionBody"]

divideOperation :: TypeDefinition
divideOperation = define "DivideOperation" $
  doc "A C++ division expression (left / right)" $
  T.record [
  "left">: cpp "MultiplicativeExpression",
  "right">: cpp "UnaryExpression"]

doStatement :: TypeDefinition
doStatement = define "DoStatement" $
  doc "A C++ do-while statement" $
  T.record [
  "body">: cpp "Statement",
  "condition">: cpp "Expression"]

elifDirective :: TypeDefinition
elifDirective = define "ElifDirective" $
  doc "A C preprocessor #elif directive" $
  T.record [
  "condition">: T.string]

elseDirective :: TypeDefinition
elseDirective = define "ElseDirective" $
  doc "A C preprocessor #else directive" $
  T.unit

endifDirective :: TypeDefinition
endifDirective = define "EndifDirective" $
  doc "A C preprocessor #endif directive" $
  T.unit

equalOperation :: TypeDefinition
equalOperation = define "EqualOperation" $
  doc "A C++ equality comparison expression (left == right)" $
  T.record [
  "left">: cpp "EqualityExpression",
  "right">: cpp "RelationalExpression"]

equalityExpression :: TypeDefinition
equalityExpression = define "EqualityExpression" $
  doc "A C++ equality-expression: a relational expression, an equality, or an inequality" $
  T.union [
  "relational">: cpp "RelationalExpression",
  "equal">: cpp "EqualOperation",
  "notEqual">: cpp "NotEqualOperation"]

errorDirective :: TypeDefinition
errorDirective = define "ErrorDirective" $
  doc "A C preprocessor #error directive" $
  T.record [
  "message">: T.string]

exclusiveOrExpression :: TypeDefinition
exclusiveOrExpression = define "ExclusiveOrExpression" $
  doc "A C++ exclusive-or-expression: an and expression or a bitwise XOR operation" $
  T.union [
  "and">: cpp "AndExpression",
  "bitwiseXor">: cpp "BitwiseXorOperation"]

explicitAssignment :: TypeDefinition
explicitAssignment = define "ExplicitAssignment" $
  doc "A C++ assignment expression with an explicit operator (e.g. left += right)" $
  T.record [
  "left">: cpp "LogicalOrExpression",
  "op">: cpp "AssignmentOperator",
  "right">: cpp "AssignmentExpression"]

-- Expression-related types
expression :: TypeDefinition
expression = define "Expression" $
  doc "A C++ expression: an assignment expression or a comma expression" $
  T.union [
  "assignment">: cpp "AssignmentExpression",
  "comma">: cpp "CommaExpression"]

expressionStatement :: TypeDefinition
expressionStatement = define "ExpressionStatement" $
  doc "A C++ expression statement: an expression followed by a semicolon" $
  T.wrap $ cpp "Expression"

floatingLiteral :: TypeDefinition
floatingLiteral = define "FloatingLiteral" $
  doc "A C++ floating-point literal" $
  T.wrap T.float64

forInit :: TypeDefinition
forInit = define "ForInit" $
  doc "The initializer clause of a C++ for statement: an expression, a declaration, or empty" $
  T.union [
  "expression">: cpp "Expression",
  "declaration">: cpp "VariableDeclaration",
  "empty">: T.unit]

forStatement :: TypeDefinition
forStatement = define "ForStatement" $
  doc "A C++ for statement" $
  T.record [
  "init">: cpp "ForInit",
  "condition">: cpp "Expression",
  "increment">: cpp "Expression",
  "body">: cpp "Statement"]

functionApplication :: TypeDefinition
functionApplication = define "FunctionApplication" $
  doc "A simplified function call: a function identifier applied to a list of arguments" $
  T.record [
  "function">: cpp "FunctionIdentifier",
  "arguments">: T.list $ cpp "Expression"]

functionBody :: TypeDefinition
functionBody = define "FunctionBody" $
  doc "The body of a C++ function: a compound statement, or a declaration-only/pure/default marker" $
  T.union [
  "compound">: cpp "CompoundStatement",
  "declaration">: T.unit,
  "pure">: T.unit,
  "default">: T.unit]

functionCallOperation :: TypeDefinition
functionCallOperation = define "FunctionCallOperation" $
  doc "A C++ function call expression: a postfix expression applied to arguments" $
  T.record [
  "function">: cpp "PostfixExpression",
  "arguments">: T.list $ cpp "Expression"]

functionDeclaration :: TypeDefinition
functionDeclaration = define "FunctionDeclaration" $
  doc "A C++ function declaration or definition" $
  T.record [
  "prefixSpecifiers" >: T.list (cpp "FunctionSpecifierPrefix"),
  "returnType" >: cpp "TypeExpression",
  "name" >: T.string,
  "parameters" >: T.list (cpp "Parameter"),
  "suffixSpecifiers" >: T.list (cpp "FunctionSpecifierSuffix"),
  "body" >: cpp "FunctionBody"]

functionIdentifier :: TypeDefinition
functionIdentifier = define "FunctionIdentifier" $
  doc "The identifier of a called function: a simple name or a qualified name" $
  T.union [
  "simple">: T.string,
  "qualified">: cpp "QualifiedIdentifier"]

functionSpecifierPrefix :: TypeDefinition
functionSpecifierPrefix = define "FunctionSpecifierPrefix" $
  doc "A C++ function specifier that appears before the declaration: inline, virtual, static, or explicit" $
  T.enum ["inline", "virtual", "static", "explicit"]

functionSpecifierSuffix :: TypeDefinition
functionSpecifierSuffix = define "FunctionSpecifierSuffix" $
  doc "A C++ function specifier that appears after the parameter list: const, noexcept, override, or final" $
  T.enum ["const", "noexcept", "override", "final"]

functionType_ :: TypeDefinition
functionType_ = define "FunctionType" $
  doc "A C++ function type: a return type together with parameter types" $
  T.record [
  "returnType">: cpp "TypeExpression",
  "parameters">: T.list $ cpp "Parameter"]

greaterEqualOperation :: TypeDefinition
greaterEqualOperation = define "GreaterEqualOperation" $
  doc "A C++ greater-than-or-equal comparison expression (left >= right)" $
  T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

greaterOperation :: TypeDefinition
greaterOperation = define "GreaterOperation" $
  doc "A C++ greater-than comparison expression (left > right)" $
  T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

-- Utility types
identifier_ :: TypeDefinition
identifier_ = define "Identifier" $
  doc "A C++ identifier" $
  T.string

ifDirective :: TypeDefinition
ifDirective = define "IfDirective" $
  doc "A C preprocessor #if directive" $
  T.record [
  "condition">: T.string]

ifdefDirective :: TypeDefinition
ifdefDirective = define "IfdefDirective" $
  doc "A C preprocessor #ifdef directive" $
  T.record [
  "identifier">: T.string]

ifndefDirective :: TypeDefinition
ifndefDirective = define "IfndefDirective" $
  doc "A C preprocessor #ifndef directive" $
  T.record [
  "identifier">: T.string]

includeDirective :: TypeDefinition
includeDirective = define "IncludeDirective" $
  doc "A C preprocessor #include directive, for a system or local header" $
  T.record [
  "name">: T.string,
  "isSystem">: T.boolean]

inclusiveOrExpression :: TypeDefinition
inclusiveOrExpression = define "InclusiveOrExpression" $
  doc "A C++ inclusive-or-expression: an exclusive-or expression or a bitwise OR operation" $
  T.union [
  "exclusiveOr">: cpp "ExclusiveOrExpression",
  "bitwiseOr">: cpp "BitwiseOrOperation"]

integerLiteral :: TypeDefinition
integerLiteral = define "IntegerLiteral" $
  doc "A C++ integer literal, in decimal, hexadecimal, octal, or binary form" $
  T.union [
  "decimal">: T.bigint,
  "hexadecimal">: T.string,
  "octal">: T.string,
  "binary">: T.string]

iterationStatement :: TypeDefinition
iterationStatement = define "IterationStatement" $
  doc "A C++ iteration statement: while, do-while, for, or range-based for" $
  T.union [
  "while">: cpp "WhileStatement",
  "do">: cpp "DoStatement",
  "for">: cpp "ForStatement",
  "rangeFor">: cpp "RangeForStatement"]

jumpStatement :: TypeDefinition
jumpStatement = define "JumpStatement" $
  doc "A C++ jump statement: break, continue, return (with or without a value), or throw" $
  T.union [
  "break">: T.unit,
  "continue">: T.unit,
  "returnValue">: cpp "Expression",
  "returnVoid">: T.unit,
  "throw">: cpp "Expression"]

labeledStatement :: TypeDefinition
labeledStatement = define "LabeledStatement" $
  doc "A C++ labeled statement" $
  T.record [
  "label">: T.string,
  "statement">: cpp "Statement"]

lambdaExpression :: TypeDefinition
lambdaExpression = define "LambdaExpression" $
  doc "A C++ lambda expression" $
  T.record [
  "captures">: cpp "CaptureList",
  "parameters">: T.list $ cpp "Parameter",
  "returnType">: T.optional $ cpp "TypeExpression",
  "body">: cpp "CompoundStatement"]

leftShiftOperation :: TypeDefinition
leftShiftOperation = define "LeftShiftOperation" $
  doc "A C++ left shift expression (left << right)" $
  T.record [
  "left">: cpp "ShiftExpression",
  "right">: cpp "AdditiveExpression"]

lessEqualOperation :: TypeDefinition
lessEqualOperation = define "LessEqualOperation" $
  doc "A C++ less-than-or-equal comparison expression (left <= right)" $
  T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

lessOperation :: TypeDefinition
lessOperation = define "LessOperation" $
  doc "A C++ less-than comparison expression (left < right)" $
  T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

lineDirective :: TypeDefinition
lineDirective = define "LineDirective" $
  doc "A C preprocessor #line directive" $
  T.record [
  "lineNumber">: T.int32,
  "filename">: T.optional T.string]

listDeclaration :: TypeDefinition
listDeclaration = define "ListDeclaration" $
  doc "A declaration of a named std::vector type alias" $
  T.record [
  "elementType">: cpp "TypeExpression",
  "name">: T.string]

-- Literal-related types
literal :: TypeDefinition
literal = define "Literal" $
  doc "A C++ literal value" $
  T.union [
  "integer">: cpp "IntegerLiteral",
  "floating">: cpp "FloatingLiteral",
  "character">: cpp "CharacterLiteral",
  "string">: cpp "StringLiteral",
  "boolean">: cpp "BooleanLiteral",
  "null">: T.unit]

logicalAndExpression :: TypeDefinition
logicalAndExpression = define "LogicalAndExpression" $
  doc "A C++ logical-and-expression: an inclusive-or expression or a logical AND operation" $
  T.union [
  "inclusiveOr">: cpp "InclusiveOrExpression",
  "logicalAnd">: cpp "LogicalAndOperation"]

logicalAndOperation :: TypeDefinition
logicalAndOperation = define "LogicalAndOperation" $
  doc "A C++ logical AND expression (left && right)" $
  T.record [
  "left">: cpp "LogicalAndExpression",
  "right">: cpp "InclusiveOrExpression"]

logicalOrExpression :: TypeDefinition
logicalOrExpression = define "LogicalOrExpression" $
  doc "A C++ logical-or-expression: a logical-and expression or a logical OR operation" $
  T.union [
  "logicalAnd">: cpp "LogicalAndExpression",
  "logicalOr">: cpp "LogicalOrOperation"]

logicalOrOperation :: TypeDefinition
logicalOrOperation = define "LogicalOrOperation" $
  doc "A C++ logical OR expression (left || right)" $
  T.record [
  "left">: cpp "LogicalOrExpression",
  "right">: cpp "LogicalAndExpression"]

mapDeclaration :: TypeDefinition
mapDeclaration = define "MapDeclaration" $
  doc "A declaration of a named std::map type alias" $
  T.record [
  "keyType">: cpp "TypeExpression",
  "valueType">: cpp "TypeExpression",
  "name">: T.string]

mapEntry :: TypeDefinition
mapEntry = define "MapEntry" $
  doc "A single key/value entry in a C++ map literal" $
  T.record [
  "key">: cpp "Expression",
  "value">: cpp "Expression"]

map_ :: TypeDefinition
map_ = define "Map" $
  doc "A C++ std::map literal, with a key type, a value type, and entries" $
  T.record [
  "keyType">: cpp "TypeExpression",
  "valueType">: cpp "TypeExpression",
  "entries">: T.list $ cpp "MapEntry"]

memInitializer :: TypeDefinition
memInitializer = define "MemInitializer" $
  doc "A C++ member initializer in a constructor's initializer list" $
  T.record [
  "name">: T.string,
  "arguments">: T.list $ cpp "Expression"]

memberAccessOperation :: TypeDefinition
memberAccessOperation = define "MemberAccessOperation" $
  doc "A C++ member access expression (object.member)" $
  T.record [
  "object">: cpp "PostfixExpression",
  "member">: T.string]

memberDeclaration :: TypeDefinition
memberDeclaration = define "MemberDeclaration" $
  doc "A single member declaration within a C++ class body" $
  T.union [
  "function">: cpp "FunctionDeclaration",
  "variable">: cpp "VariableDeclaration",
  "constructor">: cpp "ConstructorDeclaration",
  "destructor">: cpp "DestructorDeclaration",
  "nestedClass">: cpp "ClassDeclaration",
  "template">: cpp "TemplateDeclaration"]

memberSpecification :: TypeDefinition
memberSpecification = define "MemberSpecification" $
  doc "An entry in a C++ class body: an access-level label or a member declaration" $
  T.union [
  "accessLabel">: cpp "AccessSpecifier",
  "member">: cpp "MemberDeclaration"]

moduloOperation :: TypeDefinition
moduloOperation = define "ModuloOperation" $
  doc "A C++ modulo expression (left % right)" $
  T.record [
  "left">: cpp "MultiplicativeExpression",
  "right">: cpp "UnaryExpression"]

multiplicativeExpression :: TypeDefinition
multiplicativeExpression = define "MultiplicativeExpression" $
  doc "A C++ multiplicative-expression: a unary expression, a multiplication, a division, or a modulo" $
  T.union [
  "unary">: cpp "UnaryExpression",
  "multiply">: cpp "MultiplyOperation",
  "divide">: cpp "DivideOperation",
  "modulo">: cpp "ModuloOperation"]

multiplyOperation :: TypeDefinition
multiplyOperation = define "MultiplyOperation" $
  doc "A C++ multiplication expression (left * right)" $
  T.record [
  "left">: cpp "MultiplicativeExpression",
  "right">: cpp "UnaryExpression"]

namespaceDeclaration :: TypeDefinition
namespaceDeclaration = define "NamespaceDeclaration" $
  doc "A C++ namespace declaration enclosing a list of declarations" $
  T.record [
  "name">: T.string,
  "declarations">: T.list $ cpp "Declaration"]

notEqualOperation :: TypeDefinition
notEqualOperation = define "NotEqualOperation" $
  doc "A C++ inequality comparison expression (left != right)" $
  T.record [
  "left">: cpp "EqualityExpression",
  "right">: cpp "RelationalExpression"]

optional :: TypeDefinition
optional = define "Optional" $
  doc "A C++ std::optional literal, with a value type and an optional contained value" $
  T.record [
  "valueType">: cpp "TypeExpression",
  "value">: T.optional $ cpp "Expression"]

optionalDeclaration :: TypeDefinition
optionalDeclaration = define "OptionalDeclaration" $
  doc "A declaration of a named std::optional type alias" $
  T.record [
  "valueType">: cpp "TypeExpression",
  "name">: T.string]

overloadedLambdas :: TypeDefinition
overloadedLambdas = define "OverloadedLambdas" $
  doc "A set of lambda expressions combined via an overload pattern (e.g. a visitor helper)" $
  T.wrap $ T.list $ cpp "LambdaExpression"

parameter :: TypeDefinition
parameter = define "Parameter" $
  doc "A C++ function or lambda parameter" $
  T.record [
  "type">: cpp "TypeExpression",
  "name">: T.string,
  "unnamed">: T.boolean,
  "defaultValue">: T.optional $ cpp "Expression"]

patternMatch :: TypeDefinition
patternMatch = define "PatternMatch" $
  doc "A std::visit-style pattern match: a visitor applied to a variant expression" $
  T.record [
  "visitor">: cpp "Visitor",
  "variant">: cpp "Expression"]

pointerMemberAccessOperation :: TypeDefinition
pointerMemberAccessOperation = define "PointerMemberAccessOperation" $
  doc "A C++ pointer member access expression (pointer->member)" $
  T.record [
  "pointer">: cpp "PostfixExpression",
  "member">: T.string]

postfixExpression :: TypeDefinition
postfixExpression = define "PostfixExpression" $
  doc "A C++ postfix-expression: a primary expression optionally followed by a postfix operation" $
  T.union [
  "primary">: cpp "PrimaryExpression",
  "subscript">: cpp "SubscriptOperation",
  "functionCall">: cpp "FunctionCallOperation",
  "templateFunctionCall">: cpp "TemplateFunctionCallOperation",
  "memberAccess">: cpp "MemberAccessOperation",
  "pointerMemberAccess">: cpp "PointerMemberAccessOperation",
  "postIncrement">: cpp "PostfixExpression",
  "postDecrement">: cpp "PostfixExpression"]

pragmaDirective :: TypeDefinition
pragmaDirective = define "PragmaDirective" $
  doc "A C preprocessor #pragma directive" $
  T.record [
  "content">: T.string]

preprocessorDirective :: TypeDefinition
preprocessorDirective = define "PreprocessorDirective" $
  doc "A C preprocessor directive" $
  T.union [
  "include">: cpp "IncludeDirective",
  "pragma">: cpp "PragmaDirective",
  "define">: cpp "DefineDirective",
  "undef">: cpp "UndefDirective",
  "ifdef">: cpp "IfdefDirective",
  "ifndef">: cpp "IfndefDirective",
  "if">: cpp "IfDirective",
  "elif">: cpp "ElifDirective",
  "else">: cpp "ElseDirective",
  "endif">: cpp "EndifDirective",
  "line">: cpp "LineDirective",
  "error">: cpp "ErrorDirective",
  "warning">: cpp "WarningDirective"]

primaryExpression :: TypeDefinition
primaryExpression = define "PrimaryExpression" $
  doc "A C++ primary-expression: an identifier, a literal, a parenthesized expression, or a lambda" $
  T.union [
  "identifier">: T.string,
  "literal">: cpp "Literal",
  "parenthesized">: cpp "Expression",
  "lambda">: cpp "LambdaExpression"]

productDeclaration :: TypeDefinition
productDeclaration = define "ProductDeclaration" $
  doc "A declaration of a product type as a C++ struct with named fields" $
  T.record [
  "name">: T.string,
  "fields">: T.list $ cpp "VariableDeclaration"]

-- Declaration-related types
program :: TypeDefinition
program = define "Program" $
  doc "A complete C++ source or header file: preprocessor directives, includes, and declarations" $
  T.record [
  "preprocessorDirectives">: T.list $ cpp "PreprocessorDirective",
  "includes">: T.list $ cpp "IncludeDirective",
  "declarations">: T.list $ cpp "Declaration"]

qualifiedIdentifier :: TypeDefinition
qualifiedIdentifier = define "QualifiedIdentifier" $
  doc "A C++ namespace-qualified identifier (namespace::name)" $
  T.record [
  "namespace">: T.string,
  "name">: T.string]

qualifiedType :: TypeDefinition
qualifiedType = define "QualifiedType" $
  doc "A C++ type expression qualified by const, a reference, or a pointer" $
  T.record [
  "baseType">: cpp "TypeExpression",
  "qualifier">: cpp "TypeQualifier"]

rangeForStatement :: TypeDefinition
rangeForStatement = define "RangeForStatement" $
  doc "A C++ range-based for statement" $
  T.record [
  "type">: cpp "TypeExpression",
  "variable">: T.string,
  "range">: cpp "Expression",
  "body">: cpp "Statement"]

relationalExpression :: TypeDefinition
relationalExpression = define "RelationalExpression" $
  doc "A C++ relational-expression: a shift expression or a relational comparison" $
  T.union [
  "shift">: cpp "ShiftExpression",
  "less">: cpp "LessOperation",
  "greater">: cpp "GreaterOperation",
  "lessEqual">: cpp "LessEqualOperation",
  "greaterEqual">: cpp "GreaterEqualOperation"]

rightShiftOperation :: TypeDefinition
rightShiftOperation = define "RightShiftOperation" $
  doc "A C++ right shift expression (left >> right)" $
  T.record [
  "left">: cpp "ShiftExpression",
  "right">: cpp "AdditiveExpression"]

selectionStatement :: TypeDefinition
selectionStatement = define "SelectionStatement" $
  doc "A C++ if statement, with an optional else branch" $
  T.record [
  "condition">: cpp "Expression",
  "thenBranch">: cpp "Statement",
  "elseBranch">: T.optional $ cpp "Statement"]

setDeclaration :: TypeDefinition
setDeclaration = define "SetDeclaration" $
  doc "A declaration of a named std::set type alias" $
  T.record [
  "elementType">: cpp "TypeExpression",
  "name">: T.string]

set_ :: TypeDefinition
set_ = define "Set" $
  doc "A C++ std::set literal, with an element type and elements" $
  T.record [
  "elementType">: cpp "TypeExpression",
  "elements">: T.list $ cpp "Expression"]

shiftExpression :: TypeDefinition
shiftExpression = define "ShiftExpression" $
  doc "A C++ shift-expression: an additive expression, a left shift, or a right shift" $
  T.union [
  "additive">: cpp "AdditiveExpression",
  "leftShift">: cpp "LeftShiftOperation",
  "rightShift">: cpp "RightShiftOperation"]

sizeofExpression :: TypeDefinition
sizeofExpression = define "SizeofExpression" $
  doc "A C++ sizeof expression applied to a type" $
  T.wrap $ cpp "TypeExpression"

-- Statement-related types
statement :: TypeDefinition
statement = define "Statement" $
  doc "A C++ statement" $
  T.union [
  "labeled">: cpp "LabeledStatement",
  "compound">: cpp "CompoundStatement",
  "selection">: cpp "SelectionStatement",
  "switch">: cpp "SwitchStatement",
  "iteration">: cpp "IterationStatement",
  "jump">: cpp "JumpStatement",
  "declaration">: cpp "VariableDeclaration",
  "expression">: cpp "Expression"]

stringLiteral :: TypeDefinition
stringLiteral = define "StringLiteral" $
  doc "A C++ string literal" $
  T.wrap T.string

subscriptOperation :: TypeDefinition
subscriptOperation = define "SubscriptOperation" $
  doc "A C++ array subscript expression (array[index])" $
  T.record [
  "array">: cpp "PostfixExpression",
  "index">: cpp "Expression"]

subtractOperation :: TypeDefinition
subtractOperation = define "SubtractOperation" $
  doc "A C++ subtraction expression (left - right)" $
  T.record [
  "left">: cpp "AdditiveExpression",
  "right">: cpp "MultiplicativeExpression"]

switchStatement :: TypeDefinition
switchStatement = define "SwitchStatement" $
  doc "A C++ switch statement" $
  T.record [
  "value">: cpp "Expression",
  "cases">: T.list $ cpp "CaseStatement"]

templateArgument :: TypeDefinition
templateArgument = define "TemplateArgument" $
  doc "A single argument to a C++ template instantiation: a type or a value" $
  T.union [
  "type">: cpp "TypeExpression",
  "value">: cpp "Expression"]

templateDeclaration :: TypeDefinition
templateDeclaration = define "TemplateDeclaration" $
  doc "A C++ template declaration wrapping a class or function declaration" $
  T.record [
  "inline">: T.boolean,
  "parameters">: T.list T.string,
  "declaration">: cpp "Declaration"]

templateFunctionCallOperation :: TypeDefinition
templateFunctionCallOperation = define "TemplateFunctionCallOperation" $
  doc "A C++ explicit template function call (func<Args...>(arguments))" $
  T.record [
  "function">: cpp "PostfixExpression",
  "templateArguments">: T.list $ cpp "TemplateArgument",
  "arguments">: T.list $ cpp "Expression"]

templateType :: TypeDefinition
templateType = define "TemplateType" $
  doc "A C++ template instantiation type (e.g. std::vector<int>)" $
  T.record [
  "name">: T.string,
  "arguments">: T.list $ cpp "TemplateArgument"]

ternaryExpression :: TypeDefinition
ternaryExpression = define "TernaryExpression" $
  doc "A C++ ternary conditional expression (condition ? trueExpr : falseExpr)" $
  T.record [
  "condition">: cpp "LogicalOrExpression",
  "trueExpr">: cpp "Expression",
  "falseExpr">: cpp "ConditionalExpression"]

-- Type-related types
typeExpression :: TypeDefinition
typeExpression = define "TypeExpression" $
  doc "A C++ type expression" $
  T.union [
  "basic">: cpp "BasicType",
  "qualified">: cpp "QualifiedType",
  "template">: cpp "TemplateType",
  "function">: cpp "FunctionType",
  "auto">: T.unit]

typeQualifier :: TypeDefinition
typeQualifier = define "TypeQualifier" $
  doc "A C++ type qualifier: const, lvalue reference, rvalue reference, or pointer" $
  T.enum ["const", "lvalueRef", "rvalueRef", "pointer"]

typedefDeclaration :: TypeDefinition
typedefDeclaration = define "TypedefDeclaration" $
  doc "A C++ typedef or using type alias declaration" $
  T.record [
  "name">: T.string,
  "type">: cpp "TypeExpression",
  "isUsing">: T.boolean]

unaryExpression :: TypeDefinition
unaryExpression = define "UnaryExpression" $
  doc "A C++ unary-expression: a postfix expression, a unary operation, or a sizeof expression" $
  T.union [
  "postfix">: cpp "PostfixExpression",
  "unaryOp">: cpp "UnaryOperation",
  "sizeof">: cpp "SizeofExpression"]

unaryOperation :: TypeDefinition
unaryOperation = define "UnaryOperation" $
  doc "A C++ unary operation applied to an operand (e.g. -x, !x, *x)" $
  T.record [
  "operator">: cpp "UnaryOperator",
  "operand">: cpp "UnaryExpression"]

unaryOperator :: TypeDefinition
unaryOperator = define "UnaryOperator" $
  doc "A C++ unary operator" $
  T.enum [
  "plus", "minus", "logicalNot", "bitwiseNot", "dereference",
  "addressOf", "preIncrement", "preDecrement"]

undefDirective :: TypeDefinition
undefDirective = define "UndefDirective" $
  doc "A C preprocessor #undef directive" $
  T.record [
  "name">: T.string]

variableDeclaration :: TypeDefinition
variableDeclaration = define "VariableDeclaration" $
  doc "A C++ variable declaration, with an optional type (for auto) and initializer" $
  T.record [
  "type">: T.optional $ cpp "TypeExpression",
  "name">: T.string,
  "initializer">: T.optional $ cpp "Expression",
  "isAuto">: T.boolean]

variantDeclaration :: TypeDefinition
variantDeclaration = define "VariantDeclaration" $
  doc "A declaration of a named std::variant type alias over a list of member types" $
  T.record [
  "types">: T.list $ cpp "TypeExpression",
  "name">: T.string]

-- Container-related types
vector :: TypeDefinition
vector = define "Vector" $
  doc "A C++ std::vector literal, with an element type and elements" $
  T.record [
  "elementType">: cpp "TypeExpression",
  "elements">: T.list $ cpp "Expression"]

visitor :: TypeDefinition
visitor = define "Visitor" $
  doc "A C++ visitor used in a pattern match: a single lambda or a set of overloaded lambdas" $
  T.union [
  "lambda">: cpp "LambdaExpression",
  "overloaded">: cpp "OverloadedLambdas"]

warningDirective :: TypeDefinition
warningDirective = define "WarningDirective" $
  doc "A C preprocessor #warning directive" $
  T.record [
  "message">: T.string]

whileStatement :: TypeDefinition
whileStatement = define "WhileStatement" $
  doc "A C++ while statement" $
  T.record [
  "condition">: cpp "Expression",
  "body">: cpp "Statement"]
