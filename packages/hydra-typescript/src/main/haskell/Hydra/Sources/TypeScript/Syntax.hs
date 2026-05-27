-- | A TypeScript 5.x syntax model for code generation.
--
-- The expression and statement layers reuse the ECMAScript 2024 grammar
-- (TypeScript is a superset of modern JavaScript). The type-level grammar
-- — type aliases, interface declarations, generics, discriminated unions,
-- tuples, intersections — is TypeScript-only. The model focuses on the
-- subset of TypeScript needed to emit Hydra-shaped output:
--
--   * Hydra records       → readonly `interface` declarations
--   * Hydra unions        → readonly discriminated-union `type` aliases
--   * Hydra type params   → generic `<T extends …>` parameters
--   * Hydra `Maybe<T>`    → `{ tag: "just", value: T } | { tag: "nothing" }`
--   * Hydra `Pair<A, B>`  → `readonly [A, B]`
--
-- Anything not needed for Hydra emission (decorators, namespaces, `enum`,
-- mapped/conditional types, classes-as-data) is intentionally omitted.

module Hydra.Sources.TypeScript.Syntax where

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
ns = ModuleName "hydra.typeScript.syntax"

define :: String -> Type -> Binding
define = datatype ns

ts :: String -> Type
ts = typeref ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just "A TypeScript 5.x syntax model for Hydra code generation"}
  where
    definitions = [
      -- Identifiers and names
      identifier,
      qualifiedName,

      -- Literals
      literal,
      stringLiteral,
      templateLiteral,
      templateElement,
      numericLiteral,

      -- Types (TypeScript type-level grammar)
      typeAnnotation,
      typeExpression,
      functionTypeExpression,
      arrayTypeExpression,
      tupleTypeExpression,
      unionTypeExpression,
      intersectionTypeExpression,
      parameterizedTypeExpression,
      objectTypeExpression,
      propertySignature,
      typeParameter,
      interfaceDeclaration,
      typeAliasDeclaration,

      -- Expressions
      expression,
      asExpression,
      arrayExpression,
      objectExpression,
      functionExpression,
      arrowFunctionExpression,
      arrowFunctionBody,
      callExpression,
      memberExpression,
      conditionalExpression,
      binaryExpression,
      unaryExpression,
      assignmentExpression,
      spreadElement,

      -- Object and array components
      property,
      propertyKind,
      arrayElement,

      -- Patterns (for destructuring)
      pattern_,
      objectPattern,
      objectPatternProperty,
      arrayPattern,
      assignmentPattern,
      restElement,
      typedPattern,

      -- Statements
      statement,
      labeledStatement,
      blockStatement,
      variableDeclaration,
      variableDeclarator,
      variableKind,
      ifStatement,
      switchStatement,
      switchCase,
      forStatement,
      forInit,
      forInStatement,
      forInLeft,
      forOfStatement,
      whileStatement,
      doWhileStatement,
      tryStatement,
      catchClause,
      throwStatement,
      returnStatement,
      breakStatement,
      continueStatement,

      -- Declarations
      functionDeclaration,
      classDeclaration,
      classBody,
      methodDefinition,
      methodKind,

      -- Modules
      program,
      sourceType,
      moduleItem,
      importDeclaration,
      importClause,
      importSpecifier,
      importDefaultSpecifier,
      importNamespaceSpecifier,
      exportDeclaration,
      namedExport,
      exportAllDeclaration,
      exportSpecifier,

      -- Operators
      binaryOperator,
      unaryOperator,
      assignmentOperator,

      -- Comments (including documentation comments)
      comment,
      documentationComment,
      documentationTag,

      -- Declarations with comments (for attaching JSDoc)
      moduleItemWithComments,
      statementWithComments,
      functionDeclarationWithComments,
      classDeclarationWithComments]

-- ============================================================================
-- Identifiers and Names
-- ============================================================================

identifier :: Binding
identifier = define "Identifier" $
  doc "A TypeScript identifier (variable, function, class name, etc.)" $
  T.wrap T.string

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  doc "A qualified name like 'module.submodule.name'" $
  T.list $ ts "Identifier"

-- ============================================================================
-- Literals
-- ============================================================================

literal :: Binding
literal = define "Literal" $
  doc "A literal value" $
  T.union [
    "string">:
      doc "A string literal" $
      ts "StringLiteral",
    "number">:
      doc "A numeric literal" $
      ts "NumericLiteral",
    "boolean">:
      doc "A boolean literal (true or false)"
      T.boolean,
    "null">:
      doc "The null literal"
      T.unit,
    "undefined">:
      doc "The undefined literal"
      T.unit,
    "bigInt">:
      doc "A BigInt literal (e.g., 123n)"
      T.bigint,
    "template">:
      doc "A template literal" $
      ts "TemplateLiteral"]

stringLiteral :: Binding
stringLiteral = define "StringLiteral" $
  doc "A string literal with quote style" $
  T.record [
    "value">:
      doc "The string value"
      T.string,
    "singleQuote">:
      doc "Whether to use single quotes (true) or double quotes (false)"
      T.boolean]

templateLiteral :: Binding
templateLiteral = define "TemplateLiteral" $
  doc "A template literal (backtick string with interpolations)" $
  T.record [
    "quasis">:
      doc "The static string parts" $
      T.list $ ts "TemplateElement",
    "expressions">:
      doc "The interpolated expressions" $
      T.list $ ts "Expression"]

templateElement :: Binding
templateElement = define "TemplateElement" $
  doc "A static part of a template literal" $
  T.record [
    "value">:
      doc "The raw string value"
      T.string,
    "tail">:
      doc "Whether this is the last element"
      T.boolean]

numericLiteral :: Binding
numericLiteral = define "NumericLiteral" $
  doc "A numeric literal (integer or floating-point)" $
  T.union [
    "integer">:
      doc "An integer literal"
      T.int64,
    "float">:
      doc "A floating-point literal"
      T.float64]

-- ============================================================================
-- Type Annotations (JSDoc / TypeScript-compatible)
-- ============================================================================

typeAnnotation :: Binding
typeAnnotation = define "TypeAnnotation" $
  doc "A type annotation (for JSDoc comments or TypeScript)" $
  T.wrap $ ts "TypeExpression"

typeExpression :: Binding
typeExpression = define "TypeExpression" $
  doc "A type expression" $
  T.union [
    "identifier">:
      doc "A named type (e.g., 'string', 'number', 'MyClass')" $
      ts "Identifier",
    "literal">:
      doc "A literal type (e.g., 'hello', 42)" $
      ts "Literal",
    "array">:
      doc "An array type" $
      ts "ArrayTypeExpression",
    "function">:
      doc "A function type" $
      ts "FunctionTypeExpression",
    "object">:
      doc "An object type" $
      ts "ObjectTypeExpression",
    "tuple">:
      doc "A tuple type (readonly [A, B, C])" $
      ts "TupleTypeExpression",
    "union">:
      doc "A union type (A | B)" $
      ts "UnionTypeExpression",
    "intersection">:
      doc "An intersection type (A & B)" $
      ts "IntersectionTypeExpression",
    "parameterized">:
      doc "A parameterized type (e.g., Array<T>, Map<K, V>)" $
      ts "ParameterizedTypeExpression",
    "optional">:
      doc "An optional type (T | undefined or T?)" $
      ts "TypeExpression",
    "readonly">:
      doc "A readonly modifier (readonly T[], readonly [A, B])" $
      ts "TypeExpression",
    "any">:
      doc "The 'any' type"
      T.unit,
    "unknown">:
      doc "The 'unknown' type"
      T.unit,
    "void">:
      doc "The 'void' type"
      T.unit,
    "never">:
      doc "The 'never' type"
      T.unit]

functionTypeExpression :: Binding
functionTypeExpression = define "FunctionTypeExpression" $
  doc "A function type expression" $
  T.record [
    "typeParameters">:
      doc "Type parameters (generics)" $
      T.list $ ts "TypeParameter",
    "parameters">:
      doc "Parameter types" $
      T.list $ ts "TypeExpression",
    "returnType">:
      doc "Return type" $
      ts "TypeExpression"]

arrayTypeExpression :: Binding
arrayTypeExpression = define "ArrayTypeExpression" $
  doc "An array type (T[])" $
  T.wrap $ ts "TypeExpression"

unionTypeExpression :: Binding
unionTypeExpression = define "UnionTypeExpression" $
  doc "A union type (A | B | C)" $
  T.list $ ts "TypeExpression"

tupleTypeExpression :: Binding
tupleTypeExpression = define "TupleTypeExpression" $
  doc "A tuple type ([A, B] or readonly [A, B])" $
  T.list $ ts "TypeExpression"

intersectionTypeExpression :: Binding
intersectionTypeExpression = define "IntersectionTypeExpression" $
  doc "An intersection type (A & B & C)" $
  T.list $ ts "TypeExpression"

parameterizedTypeExpression :: Binding
parameterizedTypeExpression = define "ParameterizedTypeExpression" $
  doc "A parameterized type (e.g., Array<T>, Map<K, V>)" $
  T.record [
    "base">: ts "TypeExpression",
    "arguments">: T.list $ ts "TypeExpression"]

objectTypeExpression :: Binding
objectTypeExpression = define "ObjectTypeExpression" $
  doc "An object type with property signatures" $
  T.list $ ts "PropertySignature"

propertySignature :: Binding
propertySignature = define "PropertySignature" $
  doc "A property signature in an object type" $
  T.record [
    "name">:
      doc "Property name" $
      ts "Identifier",
    "type">:
      doc "Property type" $
      ts "TypeExpression",
    "optional">:
      doc "Whether the property is optional"
      T.boolean,
    "readonly">:
      doc "Whether the property is readonly"
      T.boolean,
    "comments">:
      doc "Optional JSDoc documentation comment to emit above this property" $
      T.optional $ ts "DocumentationComment"]

typeParameter :: Binding
typeParameter = define "TypeParameter" $
  doc "A type parameter (generic)" $
  T.record [
    "name">:
      doc "Parameter name" $
      ts "Identifier",
    "constraint">:
      doc "Optional constraint (extends clause)" $
      T.optional $ ts "TypeExpression",
    "default">:
      doc "Optional default type" $
      T.optional $ ts "TypeExpression"]

interfaceDeclaration :: Binding
interfaceDeclaration = define "InterfaceDeclaration" $
  doc "A TypeScript interface declaration (interface Foo<T> extends Bar { ... })" $
  T.record [
    "name">:
      doc "Interface name" $
      ts "Identifier",
    "typeParameters">:
      doc "Generic parameters" $
      T.list $ ts "TypeParameter",
    "extends">:
      doc "Interfaces this one extends" $
      T.list $ ts "TypeExpression",
    "members">:
      doc "Property signatures (the interface body)" $
      T.list $ ts "PropertySignature"]

typeAliasDeclaration :: Binding
typeAliasDeclaration = define "TypeAliasDeclaration" $
  doc "A TypeScript type alias declaration (type Foo<T> = ...)" $
  T.record [
    "name">:
      doc "Alias name" $
      ts "Identifier",
    "typeParameters">:
      doc "Generic parameters" $
      T.list $ ts "TypeParameter",
    "type">:
      doc "The right-hand-side type" $
      ts "TypeExpression"]

-- ============================================================================
-- Expressions
-- ============================================================================

expression :: Binding
expression = define "Expression" $
  doc "A TypeScript expression" $
  T.union [
    "identifier">:
      doc "A simple identifier" $
      ts "Identifier",
    "literal">:
      doc "A literal value" $
      ts "Literal",
    "array">:
      doc "An array expression [a, b, c]" $
      ts "ArrayExpression",
    "object">:
      doc "An object expression {a: 1, b: 2}" $
      ts "ObjectExpression",
    "function">:
      doc "A function expression" $
      ts "FunctionExpression",
    "arrow">:
      doc "An arrow function expression" $
      ts "ArrowFunctionExpression",
    "call">:
      doc "A function call expression" $
      ts "CallExpression",
    "member">:
      doc "A member access expression (obj.prop or obj[prop])" $
      ts "MemberExpression",
    "conditional">:
      doc "A conditional (ternary) expression" $
      ts "ConditionalExpression",
    "binary">:
      doc "A binary operation expression" $
      ts "BinaryExpression",
    "unary">:
      doc "A unary operation expression" $
      ts "UnaryExpression",
    "assignment">:
      doc "An assignment expression" $
      ts "AssignmentExpression",
    "sequence">:
      doc "A sequence expression (a, b, c)" $
      T.list $ ts "Expression",
    "this">:
      doc "The 'this' keyword"
      T.unit,
    "new">:
      doc "A 'new' expression" $
      ts "CallExpression",
    "yield">:
      doc "A yield expression" $
      T.optional $ ts "Expression",
    "await">:
      doc "An await expression" $
      ts "Expression",
    "spread">:
      doc "A spread expression (...x)" $
      ts "SpreadElement",
    "parenthesized">:
      doc "A parenthesized expression" $
      ts "Expression",
    "asExpression">:
      doc "A TypeScript type assertion `<expr> as <type>`" $
      ts "AsExpression"]

asExpression :: Binding
asExpression = define "AsExpression" $
  doc "A TypeScript `<expression> as <type>` cast" $
  T.record [
    "expression">:
      doc "The expression being cast" $
      ts "Expression",
    "type">:
      doc "The target type" $
      ts "TypeExpression"]

arrayExpression :: Binding
arrayExpression = define "ArrayExpression" $
  doc "An array expression [a, b, c]" $
  T.list $ ts "ArrayElement"

arrayElement :: Binding
arrayElement = define "ArrayElement" $
  doc "An element in an array expression" $
  T.union [
    "expression">:
      doc "A regular expression element" $
      ts "Expression",
    "spread">:
      doc "A spread element ...x" $
      ts "SpreadElement",
    "hole">:
      doc "An empty slot (elision)"
      T.unit]

objectExpression :: Binding
objectExpression = define "ObjectExpression" $
  doc "An object expression {a: 1, b: 2}" $
  T.list $ ts "Property"

property :: Binding
property = define "Property" $
  doc "A property in an object expression" $
  T.record [
    "key">:
      doc "Property key (identifier, literal, or computed)" $
      ts "Expression",
    "value">:
      doc "Property value" $
      ts "Expression",
    "kind">:
      doc "Property kind (init, get, set)" $
      ts "PropertyKind",
    "computed">:
      doc "Whether the key is computed [expr]"
      T.boolean,
    "shorthand">:
      doc "Whether using shorthand syntax {x} for {x: x}"
      T.boolean]

propertyKind :: Binding
propertyKind = define "PropertyKind" $
  doc "The kind of an object property" $
  T.union [
    "init">:
      doc "A normal property initialization"
      T.unit,
    "get">:
      doc "A getter"
      T.unit,
    "set">:
      doc "A setter"
      T.unit]

functionExpression :: Binding
functionExpression = define "FunctionExpression" $
  doc "A function expression" $
  T.record [
    "id">:
      doc "Optional function name" $
      T.optional $ ts "Identifier",
    "params">:
      doc "Function parameters" $
      T.list $ ts "Pattern",
    "body">:
      doc "Function body" $
      ts "BlockStatement",
    "async">:
      doc "Whether the function is async"
      T.boolean,
    "generator">:
      doc "Whether the function is a generator"
      T.boolean]

arrowFunctionExpression :: Binding
arrowFunctionExpression = define "ArrowFunctionExpression" $
  doc "An arrow function expression" $
  T.record [
    "params">:
      doc "Function parameters" $
      T.list $ ts "Pattern",
    "body">:
      doc "Function body (expression or block)" $
      ts "ArrowFunctionBody",
    "async">:
      doc "Whether the function is async"
      T.boolean]

arrowFunctionBody :: Binding
arrowFunctionBody = define "ArrowFunctionBody" $
  doc "The body of an arrow function (expression or block)" $
  T.union [
    "expression">: ts "Expression",
    "block">: ts "BlockStatement"]

callExpression :: Binding
callExpression = define "CallExpression" $
  doc "A function call expression" $
  T.record [
    "callee">:
      doc "The function being called" $
      ts "Expression",
    "arguments">:
      doc "The arguments" $
      T.list $ ts "Expression",
    "optional">:
      doc "Whether using optional chaining (?.)"
      T.boolean]

memberExpression :: Binding
memberExpression = define "MemberExpression" $
  doc "A member access expression" $
  T.record [
    "object">:
      doc "The object" $
      ts "Expression",
    "property">:
      doc "The property" $
      ts "Expression",
    "computed">:
      doc "Whether using bracket notation (obj[prop])"
      T.boolean,
    "optional">:
      doc "Whether using optional chaining (?.)"
      T.boolean]

conditionalExpression :: Binding
conditionalExpression = define "ConditionalExpression" $
  doc "A conditional (ternary) expression: test ? consequent : alternate" $
  T.record [
    "test">: ts "Expression",
    "consequent">: ts "Expression",
    "alternate">: ts "Expression"]

binaryExpression :: Binding
binaryExpression = define "BinaryExpression" $
  doc "A binary operation expression" $
  T.record [
    "operator">: ts "BinaryOperator",
    "left">: ts "Expression",
    "right">: ts "Expression"]

unaryExpression :: Binding
unaryExpression = define "UnaryExpression" $
  doc "A unary operation expression" $
  T.record [
    "operator">: ts "UnaryOperator",
    "argument">: ts "Expression",
    "prefix">:
      doc "Whether the operator is prefix (true) or postfix (false)"
      T.boolean]

assignmentExpression :: Binding
assignmentExpression = define "AssignmentExpression" $
  doc "An assignment expression" $
  T.record [
    "operator">: ts "AssignmentOperator",
    "left">: ts "Pattern",
    "right">: ts "Expression"]

spreadElement :: Binding
spreadElement = define "SpreadElement" $
  doc "A spread element (...x)" $
  T.wrap $ ts "Expression"

-- ============================================================================
-- Patterns (Destructuring)
-- ============================================================================

pattern_ :: Binding
pattern_ = define "Pattern" $
  doc "A binding pattern (for destructuring)" $
  T.union [
    "identifier">:
      doc "A simple identifier binding" $
      ts "Identifier",
    "object">:
      doc "An object destructuring pattern" $
      ts "ObjectPattern",
    "array">:
      doc "An array destructuring pattern" $
      ts "ArrayPattern",
    "assignment">:
      doc "A pattern with default value" $
      ts "AssignmentPattern",
    "rest">:
      doc "A rest element (...x)" $
      ts "RestElement",
    "typed">:
      doc "A pattern with a TypeScript type annotation (`x: T`)" $
      ts "TypedPattern"]

objectPattern :: Binding
objectPattern = define "ObjectPattern" $
  doc "An object destructuring pattern {a, b: c}" $
  T.record [
    "properties">:
      doc "The property patterns" $
      T.list $ ts "ObjectPatternProperty"]

objectPatternProperty :: Binding
objectPatternProperty = define "ObjectPatternProperty" $
  doc "A property in an object pattern" $
  T.union [
    "property">: ts "Property",
    "rest">: ts "RestElement"]

arrayPattern :: Binding
arrayPattern = define "ArrayPattern" $
  doc "An array destructuring pattern [a, b, c]" $
  T.list $ T.optional $ ts "Pattern"

assignmentPattern :: Binding
assignmentPattern = define "AssignmentPattern" $
  doc "A pattern with default value (param = default)" $
  T.record [
    "left">: ts "Pattern",
    "right">: ts "Expression"]

restElement :: Binding
restElement = define "RestElement" $
  doc "A rest element pattern (...x)" $
  T.wrap $ ts "Pattern"

typedPattern :: Binding
typedPattern = define "TypedPattern" $
  doc "A pattern with a TypeScript type annotation (`x: T`)" $
  T.record [
    "pattern">:
      doc "The underlying binding pattern" $
      ts "Pattern",
    "type">:
      doc "The TypeScript type annotation" $
      ts "TypeExpression"]

-- ============================================================================
-- Statements
-- ============================================================================

statement :: Binding
statement = define "Statement" $
  doc "A TypeScript statement" $
  T.union [
    "expression">:
      doc "An expression statement" $
      ts "Expression",
    "block">:
      doc "A block statement" $
      ts "BlockStatement",
    "empty">:
      doc "An empty statement (;)"
      T.unit,
    "debugger">:
      doc "A debugger statement"
      T.unit,
    "return">:
      doc "A return statement" $
      ts "ReturnStatement",
    "break">:
      doc "A break statement" $
      ts "BreakStatement",
    "continue">:
      doc "A continue statement" $
      ts "ContinueStatement",
    "if">:
      doc "An if statement" $
      ts "IfStatement",
    "switch">:
      doc "A switch statement" $
      ts "SwitchStatement",
    "throw">:
      doc "A throw statement" $
      ts "ThrowStatement",
    "try">:
      doc "A try statement" $
      ts "TryStatement",
    "while">:
      doc "A while statement" $
      ts "WhileStatement",
    "doWhile">:
      doc "A do-while statement" $
      ts "DoWhileStatement",
    "for">:
      doc "A for statement" $
      ts "ForStatement",
    "forIn">:
      doc "A for-in statement" $
      ts "ForInStatement",
    "forOf">:
      doc "A for-of statement" $
      ts "ForOfStatement",
    "variableDeclaration">:
      doc "A variable declaration" $
      ts "VariableDeclaration",
    "functionDeclaration">:
      doc "A function declaration" $
      ts "FunctionDeclaration",
    "classDeclaration">:
      doc "A class declaration" $
      ts "ClassDeclaration",
    "labeled">:
      doc "A labeled statement" $
      ts "LabeledStatement"]

labeledStatement :: Binding
labeledStatement = define "LabeledStatement" $
  doc "A labeled statement" $
  T.record [
    "label">: ts "Identifier",
    "body">: ts "Statement"]

blockStatement :: Binding
blockStatement = define "BlockStatement" $
  doc "A block statement { ... }" $
  T.list $ ts "Statement"

variableDeclaration :: Binding
variableDeclaration = define "VariableDeclaration" $
  doc "A variable declaration (var, let, const)" $
  T.record [
    "kind">: ts "VariableKind",
    "declarations">: T.list $ ts "VariableDeclarator"]

variableDeclarator :: Binding
variableDeclarator = define "VariableDeclarator" $
  doc "A variable declarator (id = init)" $
  T.record [
    "id">: ts "Pattern",
    "init">: T.optional $ ts "Expression"]

variableKind :: Binding
variableKind = define "VariableKind" $
  doc "The kind of variable declaration" $
  T.union [
    "var">: T.unit,
    "let">: T.unit,
    "const">: T.unit]

ifStatement :: Binding
ifStatement = define "IfStatement" $
  doc "An if statement" $
  T.record [
    "test">: ts "Expression",
    "consequent">: ts "Statement",
    "alternate">: T.optional $ ts "Statement"]

switchStatement :: Binding
switchStatement = define "SwitchStatement" $
  doc "A switch statement" $
  T.record [
    "discriminant">: ts "Expression",
    "cases">: T.list $ ts "SwitchCase"]

switchCase :: Binding
switchCase = define "SwitchCase" $
  doc "A case clause in a switch statement" $
  T.record [
    "test">:
      doc "The test expression (Nothing for default)" $
      T.optional $ ts "Expression",
    "consequent">:
      doc "The statements to execute" $
      T.list $ ts "Statement"]

forStatement :: Binding
forStatement = define "ForStatement" $
  doc "A for statement" $
  T.record [
    "init">:
      doc "Initialization" $
      T.optional $ ts "ForInit",
    "test">:
      doc "Test condition" $
      T.optional $ ts "Expression",
    "update">:
      doc "Update expression" $
      T.optional $ ts "Expression",
    "body">: ts "Statement"]

forInit :: Binding
forInit = define "ForInit" $
  doc "Initialization clause of a for statement" $
  T.union [
    "variable">: ts "VariableDeclaration",
    "expression">: ts "Expression"]

forInStatement :: Binding
forInStatement = define "ForInStatement" $
  doc "A for-in statement" $
  T.record [
    "left">: ts "ForInLeft",
    "right">: ts "Expression",
    "body">: ts "Statement"]

forInLeft :: Binding
forInLeft = define "ForInLeft" $
  doc "Left-hand side of a for-in or for-of statement" $
  T.union [
    "variable">: ts "VariableDeclaration",
    "pattern">: ts "Pattern"]

forOfStatement :: Binding
forOfStatement = define "ForOfStatement" $
  doc "A for-of statement" $
  T.record [
    "await">:
      doc "Whether this is a for-await-of"
      T.boolean,
    "left">: ts "ForInLeft",
    "right">: ts "Expression",
    "body">: ts "Statement"]

whileStatement :: Binding
whileStatement = define "WhileStatement" $
  doc "A while statement" $
  T.record [
    "test">: ts "Expression",
    "body">: ts "Statement"]

doWhileStatement :: Binding
doWhileStatement = define "DoWhileStatement" $
  doc "A do-while statement" $
  T.record [
    "body">: ts "Statement",
    "test">: ts "Expression"]

tryStatement :: Binding
tryStatement = define "TryStatement" $
  doc "A try statement" $
  T.record [
    "block">: ts "BlockStatement",
    "handler">: T.optional $ ts "CatchClause",
    "finalizer">: T.optional $ ts "BlockStatement"]

catchClause :: Binding
catchClause = define "CatchClause" $
  doc "A catch clause" $
  T.record [
    "param">:
      doc "The catch parameter (can be omitted in ES2019+)" $
      T.optional $ ts "Pattern",
    "body">: ts "BlockStatement"]

throwStatement :: Binding
throwStatement = define "ThrowStatement" $
  doc "A throw statement" $
  T.wrap $ ts "Expression"

returnStatement :: Binding
returnStatement = define "ReturnStatement" $
  doc "A return statement" $
  T.optional $ ts "Expression"

breakStatement :: Binding
breakStatement = define "BreakStatement" $
  doc "A break statement" $
  T.optional $ ts "Identifier"

continueStatement :: Binding
continueStatement = define "ContinueStatement" $
  doc "A continue statement" $
  T.optional $ ts "Identifier"

-- ============================================================================
-- Declarations
-- ============================================================================

functionDeclaration :: Binding
functionDeclaration = define "FunctionDeclaration" $
  doc "A function declaration" $
  T.record [
    "id">:
      doc "Function name" $
      ts "Identifier",
    "params">:
      doc "Function parameters" $
      T.list $ ts "Pattern",
    "body">:
      doc "Function body" $
      ts "BlockStatement",
    "async">:
      doc "Whether the function is async"
      T.boolean,
    "generator">:
      doc "Whether the function is a generator"
      T.boolean]

classDeclaration :: Binding
classDeclaration = define "ClassDeclaration" $
  doc "A class declaration" $
  T.record [
    "id">:
      doc "Class name" $
      ts "Identifier",
    "superClass">:
      doc "Optional superclass" $
      T.optional $ ts "Expression",
    "body">:
      doc "Class body" $
      ts "ClassBody"]

classBody :: Binding
classBody = define "ClassBody" $
  doc "A class body" $
  T.list $ ts "MethodDefinition"

methodDefinition :: Binding
methodDefinition = define "MethodDefinition" $
  doc "A method definition in a class" $
  T.record [
    "key">:
      doc "Method name" $
      ts "Expression",
    "value">:
      doc "Method function" $
      ts "FunctionExpression",
    "kind">:
      doc "Method kind" $
      ts "MethodKind",
    "computed">:
      doc "Whether the key is computed"
      T.boolean,
    "static">:
      doc "Whether the method is static"
      T.boolean]

methodKind :: Binding
methodKind = define "MethodKind" $
  doc "The kind of a class method" $
  T.union [
    "constructor">: T.unit,
    "method">: T.unit,
    "get">: T.unit,
    "set">: T.unit]

-- ============================================================================
-- Modules
-- ============================================================================

program :: Binding
program = define "Program" $
  doc "A TypeScript program (module)" $
  T.record [
    "body">:
      doc "The module items" $
      T.list $ ts "ModuleItem",
    "sourceType">:
      doc "Whether this is a module or script" $
      ts "SourceType"]

sourceType :: Binding
sourceType = define "SourceType" $
  doc "Whether the program is a module or script" $
  T.union [
    "module">: T.unit,
    "script">: T.unit]

moduleItem :: Binding
moduleItem = define "ModuleItem" $
  doc "A top-level item in a module" $
  T.union [
    "statement">: ts "Statement",
    "import">: ts "ImportDeclaration",
    "export">: ts "ExportDeclaration",
    "interface">:
      doc "A top-level interface declaration" $
      ts "InterfaceDeclaration",
    "typeAlias">:
      doc "A top-level type alias declaration" $
      ts "TypeAliasDeclaration"]

importDeclaration :: Binding
importDeclaration = define "ImportDeclaration" $
  doc "An import declaration" $
  T.record [
    "specifiers">:
      doc "What to import" $
      T.list $ ts "ImportClause",
    "source">:
      doc "The module to import from" $
      ts "StringLiteral"]

importClause :: Binding
importClause = define "ImportClause" $
  doc "An import clause (named, default, or namespace import)" $
  T.union [
    "named">: ts "ImportSpecifier",
    "default">: ts "ImportDefaultSpecifier",
    "namespace">: ts "ImportNamespaceSpecifier"]

importSpecifier :: Binding
importSpecifier = define "ImportSpecifier" $
  doc "A named import specifier (import {x as y} from ...)" $
  T.record [
    "imported">: ts "Identifier",
    "local">: ts "Identifier"]

importDefaultSpecifier :: Binding
importDefaultSpecifier = define "ImportDefaultSpecifier" $
  doc "A default import specifier (import x from ...)" $
  T.wrap $ ts "Identifier"

importNamespaceSpecifier :: Binding
importNamespaceSpecifier = define "ImportNamespaceSpecifier" $
  doc "A namespace import specifier (import * as x from ...)" $
  T.wrap $ ts "Identifier"

exportDeclaration :: Binding
exportDeclaration = define "ExportDeclaration" $
  doc "An export declaration" $
  T.union [
    "named">:
      doc "Named exports (export {x, y as z})" $
      ts "NamedExport",
    "default">:
      doc "Default export (export default ...)" $
      ts "Expression",
    "declaration">:
      doc "Export a declaration (export const x = ...)" $
      ts "Statement",
    "all">:
      doc "Export all (export * from ...)" $
      ts "ExportAllDeclaration"]

namedExport :: Binding
namedExport = define "NamedExport" $
  doc "Named exports (export {x, y as z})" $
  T.record [
    "specifiers">: T.list $ ts "ExportSpecifier",
    "source">: T.optional $ ts "StringLiteral"]

exportAllDeclaration :: Binding
exportAllDeclaration = define "ExportAllDeclaration" $
  doc "Export all declaration (export * from ...)" $
  T.record [
    "exported">: T.optional $ ts "Identifier",
    "source">: ts "StringLiteral"]

exportSpecifier :: Binding
exportSpecifier = define "ExportSpecifier" $
  doc "An export specifier (x as y)" $
  T.record [
    "local">: ts "Identifier",
    "exported">: ts "Identifier"]

-- ============================================================================
-- Operators
-- ============================================================================

binaryOperator :: Binding
binaryOperator = define "BinaryOperator" $
  doc "A binary operator" $
  T.union [
    -- Arithmetic
    "add">: doc "+" T.unit,
    "subtract">: doc "-" T.unit,
    "multiply">: doc "*" T.unit,
    "divide">: doc "/" T.unit,
    "modulo">: doc "%" T.unit,
    "exponentiate">: doc "**" T.unit,
    -- Comparison
    "equal">: doc "==" T.unit,
    "notEqual">: doc "!=" T.unit,
    "strictEqual">: doc "===" T.unit,
    "strictNotEqual">: doc "!==" T.unit,
    "lessThan">: doc "<" T.unit,
    "lessThanOrEqual">: doc "<=" T.unit,
    "greaterThan">: doc ">" T.unit,
    "greaterThanOrEqual">: doc ">=" T.unit,
    -- Logical
    "and">: doc "&&" T.unit,
    "or">: doc "||" T.unit,
    "nullishCoalescing">: doc "??" T.unit,
    -- Bitwise
    "bitwiseAnd">: doc "&" T.unit,
    "bitwiseOr">: doc "|" T.unit,
    "bitwiseXor">: doc "^" T.unit,
    "leftShift">: doc "<<" T.unit,
    "rightShift">: doc ">>" T.unit,
    "unsignedRightShift">: doc ">>>" T.unit,
    -- Other
    "in">: doc "in" T.unit,
    "instanceof">: doc "instanceof" T.unit]

unaryOperator :: Binding
unaryOperator = define "UnaryOperator" $
  doc "A unary operator" $
  T.union [
    "negate">: doc "-" T.unit,
    "plus">: doc "+" T.unit,
    "not">: doc "!" T.unit,
    "bitwiseNot">: doc "~" T.unit,
    "typeof">: doc "typeof" T.unit,
    "void">: doc "void" T.unit,
    "delete">: doc "delete" T.unit,
    "increment">: doc "++" T.unit,
    "decrement">: doc "--" T.unit]

assignmentOperator :: Binding
assignmentOperator = define "AssignmentOperator" $
  doc "An assignment operator" $
  T.union [
    "assign">: doc "=" T.unit,
    "addAssign">: doc "+=" T.unit,
    "subtractAssign">: doc "-=" T.unit,
    "multiplyAssign">: doc "*=" T.unit,
    "divideAssign">: doc "/=" T.unit,
    "moduloAssign">: doc "%=" T.unit,
    "exponentiateAssign">: doc "**=" T.unit,
    "leftShiftAssign">: doc "<<=" T.unit,
    "rightShiftAssign">: doc ">>=" T.unit,
    "unsignedRightShiftAssign">: doc ">>>=" T.unit,
    "bitwiseAndAssign">: doc "&=" T.unit,
    "bitwiseOrAssign">: doc "|=" T.unit,
    "bitwiseXorAssign">: doc "^=" T.unit,
    "andAssign">: doc "&&=" T.unit,
    "orAssign">: doc "||=" T.unit,
    "nullishAssign">: doc "??=" T.unit]

-- ============================================================================
-- Comments (for JSDoc)
-- ============================================================================

comment :: Binding
comment = define "Comment" $
  doc "A TypeScript comment" $
  T.union [
    "line">:
      doc "A single-line comment (// ...)"
      T.string,
    "block">:
      doc "A block comment, delimited by slash-star and star-slash"
      T.string,
    "documentation">:
      doc "A documentation comment, delimited by slash-double-star and star-slash (JSDoc)" $
      ts "DocumentationComment"]

documentationComment :: Binding
documentationComment = define "DocumentationComment" $
  doc "A documentation comment (JSDoc) with structured tags" $
  T.record [
    "description">:
      doc "The main description"
      T.string,
    "tags">:
      doc "Documentation tags (@param, @returns, etc.)" $
      T.list $ ts "DocumentationTag"]

documentationTag :: Binding
documentationTag = define "DocumentationTag" $
  doc "A documentation tag (@param, @returns, @type, etc.)" $
  T.record [
    "name">:
      doc "Tag name (param, returns, type, etc.)"
      T.string,
    "type">:
      doc "Optional type expression" $
      T.optional $ ts "TypeExpression",
    "paramName">:
      doc "Optional parameter name (for @param)" $
      T.optional $ ts "Identifier",
    "description">:
      doc "Tag description"
      T.string]

-- ============================================================================
-- Declarations with Comments (for JSDoc support)
-- ============================================================================

moduleItemWithComments :: Binding
moduleItemWithComments = define "ModuleItemWithComments" $
  doc "A module item with optional documentation" $
  T.record [
    "body">:
      doc "The module item" $
      ts "ModuleItem",
    "comments">:
      doc "Optional documentation comment" $
      T.optional $ ts "DocumentationComment"]

statementWithComments :: Binding
statementWithComments = define "StatementWithComments" $
  doc "A statement with optional documentation" $
  T.record [
    "body">:
      doc "The statement" $
      ts "Statement",
    "comments">:
      doc "Optional documentation comment" $
      T.optional $ ts "DocumentationComment"]

functionDeclarationWithComments :: Binding
functionDeclarationWithComments = define "FunctionDeclarationWithComments" $
  doc "A function declaration with optional JSDoc" $
  T.record [
    "body">:
      doc "The function declaration" $
      ts "FunctionDeclaration",
    "comments">:
      doc "Optional JSDoc comment" $
      T.optional $ ts "DocumentationComment"]

classDeclarationWithComments :: Binding
classDeclarationWithComments = define "ClassDeclarationWithComments" $
  doc "A class declaration with optional JSDoc" $
  T.record [
    "body">:
      doc "The class declaration" $
      ts "ClassDeclaration",
    "comments">:
      doc "Optional JSDoc comment" $
      T.optional $ ts "DocumentationComment"]
