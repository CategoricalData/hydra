
-- Note: this file was created with the help of a large language model. It requires further human review.

module Hydra.Sources.Cpp.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
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
ns = ModuleName "hydra.cpp.syntax"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just "A C++ syntax model, focusing on features for representing algebraic data types and declarative computations"}
  where
    definitions = accessSpecifiers ++ declarationTypes ++ expressionTypes ++ statementTypes
               ++ typeTypes ++ literalTypes ++ containers ++ utilities ++ operatorTypes

    accessSpecifiers = [accessSpecifier]

    declarationTypes = [
      program, includeDirective, declaration, namespaceDeclaration, typedefDeclaration,
      classDeclaration, templateDeclaration, preprocessorDirective, pragmaDirective,
      defineDirective, undefDirective, ifdefDirective, ifndefDirective, ifDirective,
      elifDirective, elseDirective, endifDirective, lineDirective, errorDirective,
      warningDirective, classSpecifier, classKey, baseSpecifier, classBody,
      memberSpecification, memberDeclaration, constructorDeclaration, memInitializer,
      destructorDeclaration, functionDeclaration, functionSpecifierPrefix, functionSpecifierSuffix,
      parameter, functionBody, variableDeclaration, variantDeclaration, productDeclaration,
      containerDeclaration, listDeclaration, mapDeclaration, setDeclaration, optionalDeclaration]

    expressionTypes = [
      expression, commaExpression, assignmentExpression, explicitAssignment, assignmentOperator,
      conditionalExpression, ternaryExpression, logicalOrExpression, logicalOrOperation,
      logicalAndExpression, logicalAndOperation, inclusiveOrExpression, bitwiseOrOperation,
      exclusiveOrExpression, bitwiseXorOperation, andExpression, bitwiseAndOperation,
      equalityExpression, equalOperation, notEqualOperation, relationalExpression,
      lessOperation, greaterOperation, lessEqualOperation, greaterEqualOperation,
      shiftExpression, leftShiftOperation, rightShiftOperation, additiveExpression,
      addOperation, subtractOperation, multiplicativeExpression, multiplyOperation,
      divideOperation, moduloOperation, unaryExpression, unaryOperation, unaryOperator,
      sizeofExpression, postfixExpression, subscriptOperation, functionCallOperation,
      memberAccessOperation, pointerMemberAccessOperation, templateFunctionCallOperation,
      primaryExpression, lambdaExpression, captureList, capture, patternMatch, visitor,
      overloadedLambdas, functionApplication, functionIdentifier, qualifiedIdentifier]

    statementTypes = [
      statement, labeledStatement, compoundStatement, selectionStatement, switchStatement,
      caseStatement, caseValue, iterationStatement, whileStatement, doStatement,
      forStatement, forInit, rangeForStatement, jumpStatement, expressionStatement]

    typeTypes = [
      typeExpression, basicType, qualifiedType, typeQualifier, templateType,
      templateArgument, functionType_]

    literalTypes = [
      literal, integerLiteral, floatingLiteral, characterLiteral, stringLiteral, booleanLiteral]

    containers = [vector, map_, mapEntry, set_, optional]

    operatorTypes = [binaryOperator]

    utilities = [identifier_, comment]

-- Access specifiers
accessSpecifier :: Binding
accessSpecifier = define "AccessSpecifier" $ T.enum ["public", "protected", "private", "none"]

addOperation :: Binding
addOperation = define "AddOperation" $ T.record [
  "left">: cpp "AdditiveExpression",
  "right">: cpp "MultiplicativeExpression"]

additiveExpression :: Binding
additiveExpression = define "AdditiveExpression" $ T.union [
  "multiplicative">: cpp "MultiplicativeExpression",
  "add">: cpp "AddOperation",
  "subtract">: cpp "SubtractOperation"]

andExpression :: Binding
andExpression = define "AndExpression" $ T.union [
  "equality">: cpp "EqualityExpression",
  "bitwiseAnd">: cpp "BitwiseAndOperation"]

assignmentExpression :: Binding
assignmentExpression = define "AssignmentExpression" $ T.union [
  "conditional">: cpp "ConditionalExpression",
  "assignment">: cpp "ExplicitAssignment"]

assignmentOperator :: Binding
assignmentOperator = define "AssignmentOperator" $ T.enum [
  "assign", "plusAssign", "minusAssign", "multiplyAssign", "divideAssign",
  "moduloAssign", "leftShiftAssign", "rightShiftAssign", "bitwiseAndAssign",
  "bitwiseXorAssign", "bitwiseOrAssign"]

baseSpecifier :: Binding
baseSpecifier = define "BaseSpecifier" $ T.record [
  "access">: cpp "AccessSpecifier",
  "name">: T.string]

basicType :: Binding
basicType = define "BasicType" $ T.union [
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
binaryOperator :: Binding
binaryOperator = define "BinaryOperator" $ T.enum [
  "plus", "minus", "multiply", "divide", "modulo",
  "bitwiseAnd", "bitwiseOr", "bitwiseXor",
  "logicalAnd", "logicalOr",
  "equal", "notEqual", "less", "greater", "lessEqual", "greaterEqual",
  "leftShift", "rightShift"]

bitwiseAndOperation :: Binding
bitwiseAndOperation = define "BitwiseAndOperation" $ T.record [
  "left">: cpp "AndExpression",
  "right">: cpp "EqualityExpression"]

bitwiseOrOperation :: Binding
bitwiseOrOperation = define "BitwiseOrOperation" $ T.record [
  "left">: cpp "InclusiveOrExpression",
  "right">: cpp "ExclusiveOrExpression"]

bitwiseXorOperation :: Binding
bitwiseXorOperation = define "BitwiseXorOperation" $ T.record [
  "left">: cpp "ExclusiveOrExpression",
  "right">: cpp "AndExpression"]

booleanLiteral :: Binding
booleanLiteral = define "BooleanLiteral" $ T.wrap T.boolean

capture :: Binding
capture = define "Capture" $ T.record [
  "name">: T.string,
  "byReference">: T.boolean]

captureList :: Binding
captureList = define "CaptureList" $ T.union [
  "captureByValue">: T.unit,
  "captures">: T.list $ cpp "Capture"]

caseStatement :: Binding
caseStatement = define "CaseStatement" $ T.union [
  "case">: cpp "CaseValue",
  "default">: cpp "Statement"]

caseValue :: Binding
caseValue = define "CaseValue" $ T.record [
  "value">: cpp "Expression",
  "statement">: cpp "Statement"]

characterLiteral :: Binding
characterLiteral = define "CharacterLiteral" $ T.wrap T.string

classBody :: Binding
classBody = define "ClassBody" $ T.wrap $ T.list $ cpp "MemberSpecification"

classDeclaration :: Binding
classDeclaration = define "ClassDeclaration" $ T.record [
  "specifier">: cpp "ClassSpecifier",
  "body">: T.optional $ cpp "ClassBody"]

classKey :: Binding
classKey = define "ClassKey" $ T.enum ["class", "enum", "enumClass", "struct"]

classSpecifier :: Binding
classSpecifier = define "ClassSpecifier" $ T.record [
  "key">: cpp "ClassKey",
  "name">: T.string,
  "inheritance">: T.list $ cpp "BaseSpecifier"]

commaExpression :: Binding
commaExpression = define "CommaExpression" $ T.record [
  "left">: cpp "Expression",
  "right">: cpp "AssignmentExpression"]

comment :: Binding
comment = define "Comment" $ T.record [
  "text">: T.string,
  "isMultiline">: T.boolean]

compoundStatement :: Binding
compoundStatement = define "CompoundStatement" $ T.wrap $ T.list $ cpp "Statement"

conditionalExpression :: Binding
conditionalExpression = define "ConditionalExpression" $ T.union [
  "logicalOr">: cpp "LogicalOrExpression",
  "ternary">: cpp "TernaryExpression"]

constructorDeclaration :: Binding
constructorDeclaration = define "ConstructorDeclaration" $ T.record [
  "name">: T.string,
  "parameters">: T.list $ cpp "Parameter",
  "initializers">: T.list $ cpp "MemInitializer",
  "body">: cpp "FunctionBody"]

containerDeclaration :: Binding
containerDeclaration = define "ContainerDeclaration" $ T.union [
  "list">: cpp "ListDeclaration",
  "map">: cpp "MapDeclaration",
  "set">: cpp "SetDeclaration",
  "optional">: cpp "OptionalDeclaration"]

cpp :: String -> Type
cpp = typeref ns

declaration :: Binding
declaration = define "Declaration" $ T.union [
  "preprocessor">: cpp "PreprocessorDirective",
  "class">: cpp "ClassDeclaration",
  "function">: cpp "FunctionDeclaration",
  "variable">: cpp "VariableDeclaration",
  "typedef">: cpp "TypedefDeclaration",
  "namespace">: cpp "NamespaceDeclaration",
  "template">: cpp "TemplateDeclaration"]

defineDirective :: Binding
defineDirective = define "DefineDirective" $ T.record [
  "name">: T.string,
  "parameters">: T.optional $ T.list T.string,
  "replacement">: T.optional T.string]

destructorDeclaration :: Binding
destructorDeclaration = define "DestructorDeclaration" $ T.record [
  "prefixSpecifiers" >: T.list (cpp "FunctionSpecifierPrefix"),
  "name" >: T.string,
  "suffixSpecifiers" >: T.list (cpp "FunctionSpecifierSuffix"),
  "body" >: cpp "FunctionBody"]

divideOperation :: Binding
divideOperation = define "DivideOperation" $ T.record [
  "left">: cpp "MultiplicativeExpression",
  "right">: cpp "UnaryExpression"]

doStatement :: Binding
doStatement = define "DoStatement" $ T.record [
  "body">: cpp "Statement",
  "condition">: cpp "Expression"]

elifDirective :: Binding
elifDirective = define "ElifDirective" $ T.record [
  "condition">: T.string]

elseDirective :: Binding
elseDirective = define "ElseDirective" T.unit

endifDirective :: Binding
endifDirective = define "EndifDirective" T.unit

equalOperation :: Binding
equalOperation = define "EqualOperation" $ T.record [
  "left">: cpp "EqualityExpression",
  "right">: cpp "RelationalExpression"]

equalityExpression :: Binding
equalityExpression = define "EqualityExpression" $ T.union [
  "relational">: cpp "RelationalExpression",
  "equal">: cpp "EqualOperation",
  "notEqual">: cpp "NotEqualOperation"]

errorDirective :: Binding
errorDirective = define "ErrorDirective" $ T.record [
  "message">: T.string]

exclusiveOrExpression :: Binding
exclusiveOrExpression = define "ExclusiveOrExpression" $ T.union [
  "and">: cpp "AndExpression",
  "bitwiseXor">: cpp "BitwiseXorOperation"]

explicitAssignment :: Binding
explicitAssignment = define "ExplicitAssignment" $ T.record [
  "left">: cpp "LogicalOrExpression",
  "op">: cpp "AssignmentOperator",
  "right">: cpp "AssignmentExpression"]

-- Expression-related types
expression :: Binding
expression = define "Expression" $ T.union [
  "assignment">: cpp "AssignmentExpression",
  "comma">: cpp "CommaExpression"]

expressionStatement :: Binding
expressionStatement = define "ExpressionStatement" $ T.wrap $ cpp "Expression"

floatingLiteral :: Binding
floatingLiteral = define "FloatingLiteral" $ T.wrap T.float64

forInit :: Binding
forInit = define "ForInit" $ T.union [
  "expression">: cpp "Expression",
  "declaration">: cpp "VariableDeclaration",
  "empty">: T.unit]

forStatement :: Binding
forStatement = define "ForStatement" $ T.record [
  "init">: cpp "ForInit",
  "condition">: cpp "Expression",
  "increment">: cpp "Expression",
  "body">: cpp "Statement"]

functionApplication :: Binding
functionApplication = define "FunctionApplication" $ T.record [
  "function">: cpp "FunctionIdentifier",
  "arguments">: T.list $ cpp "Expression"]

functionBody :: Binding
functionBody = define "FunctionBody" $ T.union [
  "compound">: cpp "CompoundStatement",
  "declaration">: T.unit,
  "pure">: T.unit,
  "default">: T.unit]

functionCallOperation :: Binding
functionCallOperation = define "FunctionCallOperation" $ T.record [
  "function">: cpp "PostfixExpression",
  "arguments">: T.list $ cpp "Expression"]

functionDeclaration :: Binding
functionDeclaration = define "FunctionDeclaration" $ T.record [
  "prefixSpecifiers" >: T.list (cpp "FunctionSpecifierPrefix"),
  "returnType" >: cpp "TypeExpression",
  "name" >: T.string,
  "parameters" >: T.list (cpp "Parameter"),
  "suffixSpecifiers" >: T.list (cpp "FunctionSpecifierSuffix"),
  "body" >: cpp "FunctionBody"]

functionIdentifier :: Binding
functionIdentifier = define "FunctionIdentifier" $ T.union [
  "simple">: T.string,
  "qualified">: cpp "QualifiedIdentifier"]

functionSpecifierPrefix :: Binding
functionSpecifierPrefix = define "FunctionSpecifierPrefix" $ T.enum ["inline", "virtual", "static", "explicit"]

functionSpecifierSuffix :: Binding
functionSpecifierSuffix = define "FunctionSpecifierSuffix" $ T.enum ["const", "noexcept", "override", "final"]

functionType_ :: Binding
functionType_ = define "FunctionType" $ T.record [
  "returnType">: cpp "TypeExpression",
  "parameters">: T.list $ cpp "Parameter"]

greaterEqualOperation :: Binding
greaterEqualOperation = define "GreaterEqualOperation" $ T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

greaterOperation :: Binding
greaterOperation = define "GreaterOperation" $ T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

-- Utility types
identifier_ :: Binding
identifier_ = define "Identifier" T.string

ifDirective :: Binding
ifDirective = define "IfDirective" $ T.record [
  "condition">: T.string]

ifdefDirective :: Binding
ifdefDirective = define "IfdefDirective" $ T.record [
  "identifier">: T.string]

ifndefDirective :: Binding
ifndefDirective = define "IfndefDirective" $ T.record [
  "identifier">: T.string]

includeDirective :: Binding
includeDirective = define "IncludeDirective" $ T.record [
  "name">: T.string,
  "isSystem">: T.boolean]

inclusiveOrExpression :: Binding
inclusiveOrExpression = define "InclusiveOrExpression" $ T.union [
  "exclusiveOr">: cpp "ExclusiveOrExpression",
  "bitwiseOr">: cpp "BitwiseOrOperation"]

integerLiteral :: Binding
integerLiteral = define "IntegerLiteral" $ T.union [
  "decimal">: T.bigint,
  "hexadecimal">: T.string,
  "octal">: T.string,
  "binary">: T.string]

iterationStatement :: Binding
iterationStatement = define "IterationStatement" $ T.union [
  "while">: cpp "WhileStatement",
  "do">: cpp "DoStatement",
  "for">: cpp "ForStatement",
  "rangeFor">: cpp "RangeForStatement"]

jumpStatement :: Binding
jumpStatement = define "JumpStatement" $ T.union [
  "break">: T.unit,
  "continue">: T.unit,
  "returnValue">: cpp "Expression",
  "returnVoid">: T.unit,
  "throw">: cpp "Expression"]

labeledStatement :: Binding
labeledStatement = define "LabeledStatement" $ T.record [
  "label">: T.string,
  "statement">: cpp "Statement"]

lambdaExpression :: Binding
lambdaExpression = define "LambdaExpression" $ T.record [
  "captures">: cpp "CaptureList",
  "parameters">: T.list $ cpp "Parameter",
  "returnType">: T.optional $ cpp "TypeExpression",
  "body">: cpp "CompoundStatement"]

leftShiftOperation :: Binding
leftShiftOperation = define "LeftShiftOperation" $ T.record [
  "left">: cpp "ShiftExpression",
  "right">: cpp "AdditiveExpression"]

lessEqualOperation :: Binding
lessEqualOperation = define "LessEqualOperation" $ T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

lessOperation :: Binding
lessOperation = define "LessOperation" $ T.record [
  "left">: cpp "RelationalExpression",
  "right">: cpp "ShiftExpression"]

lineDirective :: Binding
lineDirective = define "LineDirective" $ T.record [
  "lineNumber">: T.int32,
  "filename">: T.optional T.string]

listDeclaration :: Binding
listDeclaration = define "ListDeclaration" $ T.record [
  "elementType">: cpp "TypeExpression",
  "name">: T.string]

-- Literal-related types
literal :: Binding
literal = define "Literal" $ T.union [
  "integer">: cpp "IntegerLiteral",
  "floating">: cpp "FloatingLiteral",
  "character">: cpp "CharacterLiteral",
  "string">: cpp "StringLiteral",
  "boolean">: cpp "BooleanLiteral",
  "null">: T.unit]

logicalAndExpression :: Binding
logicalAndExpression = define "LogicalAndExpression" $ T.union [
  "inclusiveOr">: cpp "InclusiveOrExpression",
  "logicalAnd">: cpp "LogicalAndOperation"]

logicalAndOperation :: Binding
logicalAndOperation = define "LogicalAndOperation" $ T.record [
  "left">: cpp "LogicalAndExpression",
  "right">: cpp "InclusiveOrExpression"]

logicalOrExpression :: Binding
logicalOrExpression = define "LogicalOrExpression" $ T.union [
  "logicalAnd">: cpp "LogicalAndExpression",
  "logicalOr">: cpp "LogicalOrOperation"]

logicalOrOperation :: Binding
logicalOrOperation = define "LogicalOrOperation" $ T.record [
  "left">: cpp "LogicalOrExpression",
  "right">: cpp "LogicalAndExpression"]

mapDeclaration :: Binding
mapDeclaration = define "MapDeclaration" $ T.record [
  "keyType">: cpp "TypeExpression",
  "valueType">: cpp "TypeExpression",
  "name">: T.string]

mapEntry :: Binding
mapEntry = define "MapEntry" $ T.record [
  "key">: cpp "Expression",
  "value">: cpp "Expression"]

map_ :: Binding
map_ = define "Map" $ T.record [
  "keyType">: cpp "TypeExpression",
  "valueType">: cpp "TypeExpression",
  "entries">: T.list $ cpp "MapEntry"]

memInitializer :: Binding
memInitializer = define "MemInitializer" $ T.record [
  "name">: T.string,
  "arguments">: T.list $ cpp "Expression"]

memberAccessOperation :: Binding
memberAccessOperation = define "MemberAccessOperation" $ T.record [
  "object">: cpp "PostfixExpression",
  "member">: T.string]

memberDeclaration :: Binding
memberDeclaration = define "MemberDeclaration" $ T.union [
  "function">: cpp "FunctionDeclaration",
  "variable">: cpp "VariableDeclaration",
  "constructor">: cpp "ConstructorDeclaration",
  "destructor">: cpp "DestructorDeclaration",
  "nestedClass">: cpp "ClassDeclaration",
  "template">: cpp "TemplateDeclaration"]

memberSpecification :: Binding
memberSpecification = define "MemberSpecification" $ T.union [
  "accessLabel">: cpp "AccessSpecifier",
  "member">: cpp "MemberDeclaration"]

moduloOperation :: Binding
moduloOperation = define "ModuloOperation" $ T.record [
  "left">: cpp "MultiplicativeExpression",
  "right">: cpp "UnaryExpression"]

multiplicativeExpression :: Binding
multiplicativeExpression = define "MultiplicativeExpression" $ T.union [
  "unary">: cpp "UnaryExpression",
  "multiply">: cpp "MultiplyOperation",
  "divide">: cpp "DivideOperation",
  "modulo">: cpp "ModuloOperation"]

multiplyOperation :: Binding
multiplyOperation = define "MultiplyOperation" $ T.record [
  "left">: cpp "MultiplicativeExpression",
  "right">: cpp "UnaryExpression"]

namespaceDeclaration :: Binding
namespaceDeclaration = define "NamespaceDeclaration" $ T.record [
  "name">: T.string,
  "declarations">: T.list $ cpp "Declaration"]

notEqualOperation :: Binding
notEqualOperation = define "NotEqualOperation" $ T.record [
  "left">: cpp "EqualityExpression",
  "right">: cpp "RelationalExpression"]

optional :: Binding
optional = define "Optional" $ T.record [
  "valueType">: cpp "TypeExpression",
  "value">: T.optional $ cpp "Expression"]

optionalDeclaration :: Binding
optionalDeclaration = define "OptionalDeclaration" $ T.record [
  "valueType">: cpp "TypeExpression",
  "name">: T.string]

overloadedLambdas :: Binding
overloadedLambdas = define "OverloadedLambdas" $ T.wrap $ T.list $ cpp "LambdaExpression"

parameter :: Binding
parameter = define "Parameter" $ T.record [
  "type">: cpp "TypeExpression",
  "name">: T.string,
  "unnamed">: T.boolean,
  "defaultValue">: T.optional $ cpp "Expression"]

patternMatch :: Binding
patternMatch = define "PatternMatch" $ T.record [
  "visitor">: cpp "Visitor",
  "variant">: cpp "Expression"]

pointerMemberAccessOperation :: Binding
pointerMemberAccessOperation = define "PointerMemberAccessOperation" $ T.record [
  "pointer">: cpp "PostfixExpression",
  "member">: T.string]

postfixExpression :: Binding
postfixExpression = define "PostfixExpression" $ T.union [
  "primary">: cpp "PrimaryExpression",
  "subscript">: cpp "SubscriptOperation",
  "functionCall">: cpp "FunctionCallOperation",
  "templateFunctionCall">: cpp "TemplateFunctionCallOperation",
  "memberAccess">: cpp "MemberAccessOperation",
  "pointerMemberAccess">: cpp "PointerMemberAccessOperation",
  "postIncrement">: cpp "PostfixExpression",
  "postDecrement">: cpp "PostfixExpression"]

pragmaDirective :: Binding
pragmaDirective = define "PragmaDirective" $ T.record [
  "content">: T.string]

preprocessorDirective :: Binding
preprocessorDirective = define "PreprocessorDirective" $ T.union [
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

primaryExpression :: Binding
primaryExpression = define "PrimaryExpression" $ T.union [
  "identifier">: T.string,
  "literal">: cpp "Literal",
  "parenthesized">: cpp "Expression",
  "lambda">: cpp "LambdaExpression"]

productDeclaration :: Binding
productDeclaration = define "ProductDeclaration" $ T.record [
  "name">: T.string,
  "fields">: T.list $ cpp "VariableDeclaration"]

-- Declaration-related types
program :: Binding
program = define "Program" $ T.record [
  "preprocessorDirectives">: T.list $ cpp "PreprocessorDirective",
  "includes">: T.list $ cpp "IncludeDirective",
  "declarations">: T.list $ cpp "Declaration"]

qualifiedIdentifier :: Binding
qualifiedIdentifier = define "QualifiedIdentifier" $ T.record [
  "namespace">: T.string,
  "name">: T.string]

qualifiedType :: Binding
qualifiedType = define "QualifiedType" $ T.record [
  "baseType">: cpp "TypeExpression",
  "qualifier">: cpp "TypeQualifier"]

rangeForStatement :: Binding
rangeForStatement = define "RangeForStatement" $ T.record [
  "type">: cpp "TypeExpression",
  "variable">: T.string,
  "range">: cpp "Expression",
  "body">: cpp "Statement"]

relationalExpression :: Binding
relationalExpression = define "RelationalExpression" $ T.union [
  "shift">: cpp "ShiftExpression",
  "less">: cpp "LessOperation",
  "greater">: cpp "GreaterOperation",
  "lessEqual">: cpp "LessEqualOperation",
  "greaterEqual">: cpp "GreaterEqualOperation"]

rightShiftOperation :: Binding
rightShiftOperation = define "RightShiftOperation" $ T.record [
  "left">: cpp "ShiftExpression",
  "right">: cpp "AdditiveExpression"]

selectionStatement :: Binding
selectionStatement = define "SelectionStatement" $ T.record [
  "condition">: cpp "Expression",
  "thenBranch">: cpp "Statement",
  "elseBranch">: T.optional $ cpp "Statement"]

setDeclaration :: Binding
setDeclaration = define "SetDeclaration" $ T.record [
  "elementType">: cpp "TypeExpression",
  "name">: T.string]

set_ :: Binding
set_ = define "Set" $ T.record [
  "elementType">: cpp "TypeExpression",
  "elements">: T.list $ cpp "Expression"]

shiftExpression :: Binding
shiftExpression = define "ShiftExpression" $ T.union [
  "additive">: cpp "AdditiveExpression",
  "leftShift">: cpp "LeftShiftOperation",
  "rightShift">: cpp "RightShiftOperation"]

sizeofExpression :: Binding
sizeofExpression = define "SizeofExpression" $ T.wrap $ cpp "TypeExpression"

-- Statement-related types
statement :: Binding
statement = define "Statement" $ T.union [
  "labeled">: cpp "LabeledStatement",
  "compound">: cpp "CompoundStatement",
  "selection">: cpp "SelectionStatement",
  "switch">: cpp "SwitchStatement",
  "iteration">: cpp "IterationStatement",
  "jump">: cpp "JumpStatement",
  "declaration">: cpp "VariableDeclaration",
  "expression">: cpp "Expression"]

stringLiteral :: Binding
stringLiteral = define "StringLiteral" $ T.wrap T.string

subscriptOperation :: Binding
subscriptOperation = define "SubscriptOperation" $ T.record [
  "array">: cpp "PostfixExpression",
  "index">: cpp "Expression"]

subtractOperation :: Binding
subtractOperation = define "SubtractOperation" $ T.record [
  "left">: cpp "AdditiveExpression",
  "right">: cpp "MultiplicativeExpression"]

switchStatement :: Binding
switchStatement = define "SwitchStatement" $ T.record [
  "value">: cpp "Expression",
  "cases">: T.list $ cpp "CaseStatement"]

templateArgument :: Binding
templateArgument = define "TemplateArgument" $ T.union [
  "type">: cpp "TypeExpression",
  "value">: cpp "Expression"]

templateDeclaration :: Binding
templateDeclaration = define "TemplateDeclaration" $ T.record [
  "inline">: T.boolean,
  "parameters">: T.list T.string,
  "declaration">: cpp "Declaration"]

templateFunctionCallOperation :: Binding
templateFunctionCallOperation = define "TemplateFunctionCallOperation" $ T.record [
  "function">: cpp "PostfixExpression",
  "templateArguments">: T.list $ cpp "TemplateArgument",
  "arguments">: T.list $ cpp "Expression"]

templateType :: Binding
templateType = define "TemplateType" $ T.record [
  "name">: T.string,
  "arguments">: T.list $ cpp "TemplateArgument"]

ternaryExpression :: Binding
ternaryExpression = define "TernaryExpression" $ T.record [
  "condition">: cpp "LogicalOrExpression",
  "trueExpr">: cpp "Expression",
  "falseExpr">: cpp "ConditionalExpression"]

-- Type-related types
typeExpression :: Binding
typeExpression = define "TypeExpression" $ T.union [
  "basic">: cpp "BasicType",
  "qualified">: cpp "QualifiedType",
  "template">: cpp "TemplateType",
  "function">: cpp "FunctionType",
  "auto">: T.unit]

typeQualifier :: Binding
typeQualifier = define "TypeQualifier" $ T.enum ["const", "lvalueRef", "rvalueRef", "pointer"]

typedefDeclaration :: Binding
typedefDeclaration = define "TypedefDeclaration" $ T.record [
  "name">: T.string,
  "type">: cpp "TypeExpression",
  "isUsing">: T.boolean]

unaryExpression :: Binding
unaryExpression = define "UnaryExpression" $ T.union [
  "postfix">: cpp "PostfixExpression",
  "unaryOp">: cpp "UnaryOperation",
  "sizeof">: cpp "SizeofExpression"]

unaryOperation :: Binding
unaryOperation = define "UnaryOperation" $ T.record [
  "operator">: cpp "UnaryOperator",
  "operand">: cpp "UnaryExpression"]

unaryOperator :: Binding
unaryOperator = define "UnaryOperator" $ T.enum [
  "plus", "minus", "logicalNot", "bitwiseNot", "dereference",
  "addressOf", "preIncrement", "preDecrement"]

undefDirective :: Binding
undefDirective = define "UndefDirective" $ T.record [
  "name">: T.string]

variableDeclaration :: Binding
variableDeclaration = define "VariableDeclaration" $ T.record [
  "type">: T.optional $ cpp "TypeExpression",
  "name">: T.string,
  "initializer">: T.optional $ cpp "Expression",
  "isAuto">: T.boolean]

variantDeclaration :: Binding
variantDeclaration = define "VariantDeclaration" $ T.record [
  "types">: T.list $ cpp "TypeExpression",
  "name">: T.string]

-- Container-related types
vector :: Binding
vector = define "Vector" $ T.record [
  "elementType">: cpp "TypeExpression",
  "elements">: T.list $ cpp "Expression"]

visitor :: Binding
visitor = define "Visitor" $ T.union [
  "lambda">: cpp "LambdaExpression",
  "overloaded">: cpp "OverloadedLambdas"]

warningDirective :: Binding
warningDirective = define "WarningDirective" $ T.record [
  "message">: T.string]

whileStatement :: Binding
whileStatement = define "WhileStatement" $ T.record [
  "condition">: cpp "Expression",
  "body">: cpp "Statement"]
