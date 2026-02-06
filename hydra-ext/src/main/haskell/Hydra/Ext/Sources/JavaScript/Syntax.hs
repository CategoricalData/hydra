-- | A JavaScript/ECMAScript syntax model for code generation.
--
-- This model is based on the ECMAScript 2024 specification and is designed
-- to support Hydra's code generation needs. It focuses on the subset of
-- JavaScript syntax needed for generating functional, type-annotated code.

module Hydra.Ext.Sources.JavaScript.Syntax where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.javaScript.syntax"

define :: String -> Type -> Binding
define = datatype ns

js :: String -> Type
js = typeref ns

module_ :: Module
module_ = Module ns elements [Core.ns] [Core.ns] $
    Just "A JavaScript/ECMAScript syntax model for code generation"
  where
    elements = [
      -- Identifiers and names
      identifier,
      qualifiedName,

      -- Literals
      literal,
      stringLiteral,
      templateLiteral,
      templateElement,
      numericLiteral,

      -- Types (for JSDoc/TypeScript-style annotations)
      typeAnnotation,
      typeExpression,
      functionTypeExpression,
      arrayTypeExpression,
      unionTypeExpression,
      parameterizedTypeExpression,
      objectTypeExpression,
      propertySignature,
      typeParameter,

      -- Expressions
      expression,
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
  doc "A JavaScript identifier (variable, function, class name, etc.)" $
  T.wrap T.string

qualifiedName :: Binding
qualifiedName = define "QualifiedName" $
  doc "A qualified name like 'module.submodule.name'" $
  T.list $ js "Identifier"

-- ============================================================================
-- Literals
-- ============================================================================

literal :: Binding
literal = define "Literal" $
  doc "A literal value" $
  T.union [
    "string">:
      doc "A string literal" $
      js "StringLiteral",
    "number">:
      doc "A numeric literal" $
      js "NumericLiteral",
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
      js "TemplateLiteral"]

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
      T.list $ js "TemplateElement",
    "expressions">:
      doc "The interpolated expressions" $
      T.list $ js "Expression"]

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
  T.wrap $ js "TypeExpression"

typeExpression :: Binding
typeExpression = define "TypeExpression" $
  doc "A type expression" $
  T.union [
    "identifier">:
      doc "A named type (e.g., 'string', 'number', 'MyClass')" $
      js "Identifier",
    "literal">:
      doc "A literal type (e.g., 'hello', 42)" $
      js "Literal",
    "array">:
      doc "An array type" $
      js "ArrayTypeExpression",
    "function">:
      doc "A function type" $
      js "FunctionTypeExpression",
    "object">:
      doc "An object type" $
      js "ObjectTypeExpression",
    "union">:
      doc "A union type (A | B)" $
      js "UnionTypeExpression",
    "parameterized">:
      doc "A parameterized type (e.g., Array<T>, Map<K, V>)" $
      js "ParameterizedTypeExpression",
    "optional">:
      doc "An optional type (?T)" $
      js "TypeExpression",
    "any">:
      doc "The 'any' type"
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
      T.list $ js "TypeParameter",
    "parameters">:
      doc "Parameter types" $
      T.list $ js "TypeExpression",
    "returnType">:
      doc "Return type" $
      js "TypeExpression"]

arrayTypeExpression :: Binding
arrayTypeExpression = define "ArrayTypeExpression" $
  doc "An array type (T[])" $
  T.wrap $ js "TypeExpression"

unionTypeExpression :: Binding
unionTypeExpression = define "UnionTypeExpression" $
  doc "A union type (A | B | C)" $
  T.list $ js "TypeExpression"

parameterizedTypeExpression :: Binding
parameterizedTypeExpression = define "ParameterizedTypeExpression" $
  doc "A parameterized type (e.g., Array<T>, Map<K, V>)" $
  T.record [
    "base">: js "TypeExpression",
    "arguments">: T.list $ js "TypeExpression"]

objectTypeExpression :: Binding
objectTypeExpression = define "ObjectTypeExpression" $
  doc "An object type with property signatures" $
  T.list $ js "PropertySignature"

propertySignature :: Binding
propertySignature = define "PropertySignature" $
  doc "A property signature in an object type" $
  T.record [
    "name">:
      doc "Property name" $
      js "Identifier",
    "type">:
      doc "Property type" $
      js "TypeExpression",
    "optional">:
      doc "Whether the property is optional"
      T.boolean,
    "readonly">:
      doc "Whether the property is readonly"
      T.boolean]

typeParameter :: Binding
typeParameter = define "TypeParameter" $
  doc "A type parameter (generic)" $
  T.record [
    "name">:
      doc "Parameter name" $
      js "Identifier",
    "constraint">:
      doc "Optional constraint (extends clause)" $
      T.optional $ js "TypeExpression",
    "default">:
      doc "Optional default type" $
      T.optional $ js "TypeExpression"]

-- ============================================================================
-- Expressions
-- ============================================================================

expression :: Binding
expression = define "Expression" $
  doc "A JavaScript expression" $
  T.union [
    "identifier">:
      doc "A simple identifier" $
      js "Identifier",
    "literal">:
      doc "A literal value" $
      js "Literal",
    "array">:
      doc "An array expression [a, b, c]" $
      js "ArrayExpression",
    "object">:
      doc "An object expression {a: 1, b: 2}" $
      js "ObjectExpression",
    "function">:
      doc "A function expression" $
      js "FunctionExpression",
    "arrow">:
      doc "An arrow function expression" $
      js "ArrowFunctionExpression",
    "call">:
      doc "A function call expression" $
      js "CallExpression",
    "member">:
      doc "A member access expression (obj.prop or obj[prop])" $
      js "MemberExpression",
    "conditional">:
      doc "A conditional (ternary) expression" $
      js "ConditionalExpression",
    "binary">:
      doc "A binary operation expression" $
      js "BinaryExpression",
    "unary">:
      doc "A unary operation expression" $
      js "UnaryExpression",
    "assignment">:
      doc "An assignment expression" $
      js "AssignmentExpression",
    "sequence">:
      doc "A sequence expression (a, b, c)" $
      T.list $ js "Expression",
    "this">:
      doc "The 'this' keyword"
      T.unit,
    "new">:
      doc "A 'new' expression" $
      js "CallExpression",
    "yield">:
      doc "A yield expression" $
      T.optional $ js "Expression",
    "await">:
      doc "An await expression" $
      js "Expression",
    "spread">:
      doc "A spread expression (...x)" $
      js "SpreadElement",
    "parenthesized">:
      doc "A parenthesized expression" $
      js "Expression"]

arrayExpression :: Binding
arrayExpression = define "ArrayExpression" $
  doc "An array expression [a, b, c]" $
  T.list $ js "ArrayElement"

arrayElement :: Binding
arrayElement = define "ArrayElement" $
  doc "An element in an array expression" $
  T.union [
    "expression">:
      doc "A regular expression element" $
      js "Expression",
    "spread">:
      doc "A spread element ...x" $
      js "SpreadElement",
    "hole">:
      doc "An empty slot (elision)"
      T.unit]

objectExpression :: Binding
objectExpression = define "ObjectExpression" $
  doc "An object expression {a: 1, b: 2}" $
  T.list $ js "Property"

property :: Binding
property = define "Property" $
  doc "A property in an object expression" $
  T.record [
    "key">:
      doc "Property key (identifier, literal, or computed)" $
      js "Expression",
    "value">:
      doc "Property value" $
      js "Expression",
    "kind">:
      doc "Property kind (init, get, set)" $
      js "PropertyKind",
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
      T.optional $ js "Identifier",
    "params">:
      doc "Function parameters" $
      T.list $ js "Pattern",
    "body">:
      doc "Function body" $
      js "BlockStatement",
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
      T.list $ js "Pattern",
    "body">:
      doc "Function body (expression or block)" $
      js "ArrowFunctionBody",
    "async">:
      doc "Whether the function is async"
      T.boolean]

arrowFunctionBody :: Binding
arrowFunctionBody = define "ArrowFunctionBody" $
  doc "The body of an arrow function (expression or block)" $
  T.union [
    "expression">: js "Expression",
    "block">: js "BlockStatement"]

callExpression :: Binding
callExpression = define "CallExpression" $
  doc "A function call expression" $
  T.record [
    "callee">:
      doc "The function being called" $
      js "Expression",
    "arguments">:
      doc "The arguments" $
      T.list $ js "Expression",
    "optional">:
      doc "Whether using optional chaining (?.)"
      T.boolean]

memberExpression :: Binding
memberExpression = define "MemberExpression" $
  doc "A member access expression" $
  T.record [
    "object">:
      doc "The object" $
      js "Expression",
    "property">:
      doc "The property" $
      js "Expression",
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
    "test">: js "Expression",
    "consequent">: js "Expression",
    "alternate">: js "Expression"]

binaryExpression :: Binding
binaryExpression = define "BinaryExpression" $
  doc "A binary operation expression" $
  T.record [
    "operator">: js "BinaryOperator",
    "left">: js "Expression",
    "right">: js "Expression"]

unaryExpression :: Binding
unaryExpression = define "UnaryExpression" $
  doc "A unary operation expression" $
  T.record [
    "operator">: js "UnaryOperator",
    "argument">: js "Expression",
    "prefix">:
      doc "Whether the operator is prefix (true) or postfix (false)"
      T.boolean]

assignmentExpression :: Binding
assignmentExpression = define "AssignmentExpression" $
  doc "An assignment expression" $
  T.record [
    "operator">: js "AssignmentOperator",
    "left">: js "Pattern",
    "right">: js "Expression"]

spreadElement :: Binding
spreadElement = define "SpreadElement" $
  doc "A spread element (...x)" $
  T.wrap $ js "Expression"

-- ============================================================================
-- Patterns (Destructuring)
-- ============================================================================

pattern_ :: Binding
pattern_ = define "Pattern" $
  doc "A binding pattern (for destructuring)" $
  T.union [
    "identifier">:
      doc "A simple identifier binding" $
      js "Identifier",
    "object">:
      doc "An object destructuring pattern" $
      js "ObjectPattern",
    "array">:
      doc "An array destructuring pattern" $
      js "ArrayPattern",
    "assignment">:
      doc "A pattern with default value" $
      js "AssignmentPattern",
    "rest">:
      doc "A rest element (...x)" $
      js "RestElement"]

objectPattern :: Binding
objectPattern = define "ObjectPattern" $
  doc "An object destructuring pattern {a, b: c}" $
  T.record [
    "properties">:
      doc "The property patterns" $
      T.list $ js "ObjectPatternProperty"]

objectPatternProperty :: Binding
objectPatternProperty = define "ObjectPatternProperty" $
  doc "A property in an object pattern" $
  T.union [
    "property">: js "Property",
    "rest">: js "RestElement"]

arrayPattern :: Binding
arrayPattern = define "ArrayPattern" $
  doc "An array destructuring pattern [a, b, c]" $
  T.list $ T.optional $ js "Pattern"

assignmentPattern :: Binding
assignmentPattern = define "AssignmentPattern" $
  doc "A pattern with default value (param = default)" $
  T.record [
    "left">: js "Pattern",
    "right">: js "Expression"]

restElement :: Binding
restElement = define "RestElement" $
  doc "A rest element pattern (...x)" $
  T.wrap $ js "Pattern"

-- ============================================================================
-- Statements
-- ============================================================================

statement :: Binding
statement = define "Statement" $
  doc "A JavaScript statement" $
  T.union [
    "expression">:
      doc "An expression statement" $
      js "Expression",
    "block">:
      doc "A block statement" $
      js "BlockStatement",
    "empty">:
      doc "An empty statement (;)"
      T.unit,
    "debugger">:
      doc "A debugger statement"
      T.unit,
    "return">:
      doc "A return statement" $
      js "ReturnStatement",
    "break">:
      doc "A break statement" $
      js "BreakStatement",
    "continue">:
      doc "A continue statement" $
      js "ContinueStatement",
    "if">:
      doc "An if statement" $
      js "IfStatement",
    "switch">:
      doc "A switch statement" $
      js "SwitchStatement",
    "throw">:
      doc "A throw statement" $
      js "ThrowStatement",
    "try">:
      doc "A try statement" $
      js "TryStatement",
    "while">:
      doc "A while statement" $
      js "WhileStatement",
    "doWhile">:
      doc "A do-while statement" $
      js "DoWhileStatement",
    "for">:
      doc "A for statement" $
      js "ForStatement",
    "forIn">:
      doc "A for-in statement" $
      js "ForInStatement",
    "forOf">:
      doc "A for-of statement" $
      js "ForOfStatement",
    "variableDeclaration">:
      doc "A variable declaration" $
      js "VariableDeclaration",
    "functionDeclaration">:
      doc "A function declaration" $
      js "FunctionDeclaration",
    "classDeclaration">:
      doc "A class declaration" $
      js "ClassDeclaration",
    "labeled">:
      doc "A labeled statement" $
      js "LabeledStatement"]

labeledStatement :: Binding
labeledStatement = define "LabeledStatement" $
  doc "A labeled statement" $
  T.record [
    "label">: js "Identifier",
    "body">: js "Statement"]

blockStatement :: Binding
blockStatement = define "BlockStatement" $
  doc "A block statement { ... }" $
  T.list $ js "Statement"

variableDeclaration :: Binding
variableDeclaration = define "VariableDeclaration" $
  doc "A variable declaration (var, let, const)" $
  T.record [
    "kind">: js "VariableKind",
    "declarations">: T.list $ js "VariableDeclarator"]

variableDeclarator :: Binding
variableDeclarator = define "VariableDeclarator" $
  doc "A variable declarator (id = init)" $
  T.record [
    "id">: js "Pattern",
    "init">: T.optional $ js "Expression"]

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
    "test">: js "Expression",
    "consequent">: js "Statement",
    "alternate">: T.optional $ js "Statement"]

switchStatement :: Binding
switchStatement = define "SwitchStatement" $
  doc "A switch statement" $
  T.record [
    "discriminant">: js "Expression",
    "cases">: T.list $ js "SwitchCase"]

switchCase :: Binding
switchCase = define "SwitchCase" $
  doc "A case clause in a switch statement" $
  T.record [
    "test">:
      doc "The test expression (Nothing for default)" $
      T.optional $ js "Expression",
    "consequent">:
      doc "The statements to execute" $
      T.list $ js "Statement"]

forStatement :: Binding
forStatement = define "ForStatement" $
  doc "A for statement" $
  T.record [
    "init">:
      doc "Initialization" $
      T.optional $ js "ForInit",
    "test">:
      doc "Test condition" $
      T.optional $ js "Expression",
    "update">:
      doc "Update expression" $
      T.optional $ js "Expression",
    "body">: js "Statement"]

forInit :: Binding
forInit = define "ForInit" $
  doc "Initialization clause of a for statement" $
  T.union [
    "variable">: js "VariableDeclaration",
    "expression">: js "Expression"]

forInStatement :: Binding
forInStatement = define "ForInStatement" $
  doc "A for-in statement" $
  T.record [
    "left">: js "ForInLeft",
    "right">: js "Expression",
    "body">: js "Statement"]

forInLeft :: Binding
forInLeft = define "ForInLeft" $
  doc "Left-hand side of a for-in or for-of statement" $
  T.union [
    "variable">: js "VariableDeclaration",
    "pattern">: js "Pattern"]

forOfStatement :: Binding
forOfStatement = define "ForOfStatement" $
  doc "A for-of statement" $
  T.record [
    "await">:
      doc "Whether this is a for-await-of"
      T.boolean,
    "left">: js "ForInLeft",
    "right">: js "Expression",
    "body">: js "Statement"]

whileStatement :: Binding
whileStatement = define "WhileStatement" $
  doc "A while statement" $
  T.record [
    "test">: js "Expression",
    "body">: js "Statement"]

doWhileStatement :: Binding
doWhileStatement = define "DoWhileStatement" $
  doc "A do-while statement" $
  T.record [
    "body">: js "Statement",
    "test">: js "Expression"]

tryStatement :: Binding
tryStatement = define "TryStatement" $
  doc "A try statement" $
  T.record [
    "block">: js "BlockStatement",
    "handler">: T.optional $ js "CatchClause",
    "finalizer">: T.optional $ js "BlockStatement"]

catchClause :: Binding
catchClause = define "CatchClause" $
  doc "A catch clause" $
  T.record [
    "param">:
      doc "The catch parameter (can be omitted in ES2019+)" $
      T.optional $ js "Pattern",
    "body">: js "BlockStatement"]

throwStatement :: Binding
throwStatement = define "ThrowStatement" $
  doc "A throw statement" $
  T.wrap $ js "Expression"

returnStatement :: Binding
returnStatement = define "ReturnStatement" $
  doc "A return statement" $
  T.optional $ js "Expression"

breakStatement :: Binding
breakStatement = define "BreakStatement" $
  doc "A break statement" $
  T.optional $ js "Identifier"

continueStatement :: Binding
continueStatement = define "ContinueStatement" $
  doc "A continue statement" $
  T.optional $ js "Identifier"

-- ============================================================================
-- Declarations
-- ============================================================================

functionDeclaration :: Binding
functionDeclaration = define "FunctionDeclaration" $
  doc "A function declaration" $
  T.record [
    "id">:
      doc "Function name" $
      js "Identifier",
    "params">:
      doc "Function parameters" $
      T.list $ js "Pattern",
    "body">:
      doc "Function body" $
      js "BlockStatement",
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
      js "Identifier",
    "superClass">:
      doc "Optional superclass" $
      T.optional $ js "Expression",
    "body">:
      doc "Class body" $
      js "ClassBody"]

classBody :: Binding
classBody = define "ClassBody" $
  doc "A class body" $
  T.list $ js "MethodDefinition"

methodDefinition :: Binding
methodDefinition = define "MethodDefinition" $
  doc "A method definition in a class" $
  T.record [
    "key">:
      doc "Method name" $
      js "Expression",
    "value">:
      doc "Method function" $
      js "FunctionExpression",
    "kind">:
      doc "Method kind" $
      js "MethodKind",
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
  doc "A JavaScript program (module)" $
  T.record [
    "body">:
      doc "The module items" $
      T.list $ js "ModuleItem",
    "sourceType">:
      doc "Whether this is a module or script" $
      js "SourceType"]

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
    "statement">: js "Statement",
    "import">: js "ImportDeclaration",
    "export">: js "ExportDeclaration"]

importDeclaration :: Binding
importDeclaration = define "ImportDeclaration" $
  doc "An import declaration" $
  T.record [
    "specifiers">:
      doc "What to import" $
      T.list $ js "ImportClause",
    "source">:
      doc "The module to import from" $
      js "StringLiteral"]

importClause :: Binding
importClause = define "ImportClause" $
  doc "An import clause (named, default, or namespace import)" $
  T.union [
    "named">: js "ImportSpecifier",
    "default">: js "ImportDefaultSpecifier",
    "namespace">: js "ImportNamespaceSpecifier"]

importSpecifier :: Binding
importSpecifier = define "ImportSpecifier" $
  doc "A named import specifier (import {x as y} from ...)" $
  T.record [
    "imported">: js "Identifier",
    "local">: js "Identifier"]

importDefaultSpecifier :: Binding
importDefaultSpecifier = define "ImportDefaultSpecifier" $
  doc "A default import specifier (import x from ...)" $
  T.wrap $ js "Identifier"

importNamespaceSpecifier :: Binding
importNamespaceSpecifier = define "ImportNamespaceSpecifier" $
  doc "A namespace import specifier (import * as x from ...)" $
  T.wrap $ js "Identifier"

exportDeclaration :: Binding
exportDeclaration = define "ExportDeclaration" $
  doc "An export declaration" $
  T.union [
    "named">:
      doc "Named exports (export {x, y as z})" $
      js "NamedExport",
    "default">:
      doc "Default export (export default ...)" $
      js "Expression",
    "declaration">:
      doc "Export a declaration (export const x = ...)" $
      js "Statement",
    "all">:
      doc "Export all (export * from ...)" $
      js "ExportAllDeclaration"]

namedExport :: Binding
namedExport = define "NamedExport" $
  doc "Named exports (export {x, y as z})" $
  T.record [
    "specifiers">: T.list $ js "ExportSpecifier",
    "source">: T.optional $ js "StringLiteral"]

exportAllDeclaration :: Binding
exportAllDeclaration = define "ExportAllDeclaration" $
  doc "Export all declaration (export * from ...)" $
  T.record [
    "exported">: T.optional $ js "Identifier",
    "source">: js "StringLiteral"]

exportSpecifier :: Binding
exportSpecifier = define "ExportSpecifier" $
  doc "An export specifier (x as y)" $
  T.record [
    "local">: js "Identifier",
    "exported">: js "Identifier"]

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
  doc "A JavaScript comment" $
  T.union [
    "line">:
      doc "A single-line comment (// ...)"
      T.string,
    "block">:
      doc "A block comment (/* ... */)"
      T.string,
    "documentation">:
      doc "A documentation comment (/** ... */, i.e. JSDoc)" $
      js "DocumentationComment"]

documentationComment :: Binding
documentationComment = define "DocumentationComment" $
  doc "A documentation comment (JSDoc) with structured tags" $
  T.record [
    "description">:
      doc "The main description"
      T.string,
    "tags">:
      doc "Documentation tags (@param, @returns, etc.)" $
      T.list $ js "DocumentationTag"]

documentationTag :: Binding
documentationTag = define "DocumentationTag" $
  doc "A documentation tag (@param, @returns, @type, etc.)" $
  T.record [
    "name">:
      doc "Tag name (param, returns, type, etc.)"
      T.string,
    "type">:
      doc "Optional type expression" $
      T.optional $ js "TypeExpression",
    "paramName">:
      doc "Optional parameter name (for @param)" $
      T.optional $ js "Identifier",
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
      js "ModuleItem",
    "comments">:
      doc "Optional documentation comment" $
      T.optional $ js "DocumentationComment"]

statementWithComments :: Binding
statementWithComments = define "StatementWithComments" $
  doc "A statement with optional documentation" $
  T.record [
    "body">:
      doc "The statement" $
      js "Statement",
    "comments">:
      doc "Optional documentation comment" $
      T.optional $ js "DocumentationComment"]

functionDeclarationWithComments :: Binding
functionDeclarationWithComments = define "FunctionDeclarationWithComments" $
  doc "A function declaration with optional JSDoc" $
  T.record [
    "body">:
      doc "The function declaration" $
      js "FunctionDeclaration",
    "comments">:
      doc "Optional JSDoc comment" $
      T.optional $ js "DocumentationComment"]

classDeclarationWithComments :: Binding
classDeclarationWithComments = define "ClassDeclarationWithComments" $
  doc "A class declaration with optional JSDoc" $
  T.record [
    "body">:
      doc "The class declaration" $
      js "ClassDeclaration",
    "comments">:
      doc "Optional JSDoc comment" $
      T.optional $ js "DocumentationComment"]
