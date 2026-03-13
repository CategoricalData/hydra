-- Note: this is an automatically generated file. Do not edit.

-- | A JavaScript/ECMAScript syntax model for code generation

module Hydra.Ext.JavaScript.Syntax where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | A JavaScript identifier (variable, function, class name, etc.)
newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra.ext.javaScript.syntax.Identifier")

-- | A qualified name like 'module.submodule.name'
type QualifiedName = [Identifier]

_QualifiedName = (Core.Name "hydra.ext.javaScript.syntax.QualifiedName")

-- | A literal value
data Literal = 
  -- | A string literal
  LiteralString StringLiteral |
  -- | A numeric literal
  LiteralNumber NumericLiteral |
  -- | A boolean literal (true or false)
  LiteralBoolean Bool |
  -- | The null literal
  LiteralNull  |
  -- | The undefined literal
  LiteralUndefined  |
  -- | A BigInt literal (e.g., 123n)
  LiteralBigInt Integer |
  -- | A template literal
  LiteralTemplate TemplateLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.javaScript.syntax.Literal")

_Literal_string = (Core.Name "string")

_Literal_number = (Core.Name "number")

_Literal_boolean = (Core.Name "boolean")

_Literal_null = (Core.Name "null")

_Literal_undefined = (Core.Name "undefined")

_Literal_bigInt = (Core.Name "bigInt")

_Literal_template = (Core.Name "template")

-- | A string literal with quote style
data StringLiteral = 
  StringLiteral {
    -- | The string value
    stringLiteralValue :: String,
    -- | Whether to use single quotes (true) or double quotes (false)
    stringLiteralSingleQuote :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra.ext.javaScript.syntax.StringLiteral")

_StringLiteral_value = (Core.Name "value")

_StringLiteral_singleQuote = (Core.Name "singleQuote")

-- | A template literal (backtick string with interpolations)
data TemplateLiteral = 
  TemplateLiteral {
    -- | The static string parts
    templateLiteralQuasis :: [TemplateElement],
    -- | The interpolated expressions
    templateLiteralExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_TemplateLiteral = (Core.Name "hydra.ext.javaScript.syntax.TemplateLiteral")

_TemplateLiteral_quasis = (Core.Name "quasis")

_TemplateLiteral_expressions = (Core.Name "expressions")

-- | A static part of a template literal
data TemplateElement = 
  TemplateElement {
    -- | The raw string value
    templateElementValue :: String,
    -- | Whether this is the last element
    templateElementTail :: Bool}
  deriving (Eq, Ord, Read, Show)

_TemplateElement = (Core.Name "hydra.ext.javaScript.syntax.TemplateElement")

_TemplateElement_value = (Core.Name "value")

_TemplateElement_tail = (Core.Name "tail")

-- | A numeric literal (integer or floating-point)
data NumericLiteral = 
  -- | An integer literal
  NumericLiteralInteger I.Int64 |
  -- | A floating-point literal
  NumericLiteralFloat Double
  deriving (Eq, Ord, Read, Show)

_NumericLiteral = (Core.Name "hydra.ext.javaScript.syntax.NumericLiteral")

_NumericLiteral_integer = (Core.Name "integer")

_NumericLiteral_float = (Core.Name "float")

-- | A type annotation (for JSDoc comments or TypeScript)
newtype TypeAnnotation = 
  TypeAnnotation {
    unTypeAnnotation :: TypeExpression}
  deriving (Eq, Ord, Read, Show)

_TypeAnnotation = (Core.Name "hydra.ext.javaScript.syntax.TypeAnnotation")

-- | A type expression
data TypeExpression = 
  -- | A named type (e.g., 'string', 'number', 'MyClass')
  TypeExpressionIdentifier Identifier |
  -- | A literal type (e.g., 'hello', 42)
  TypeExpressionLiteral Literal |
  -- | An array type
  TypeExpressionArray ArrayTypeExpression |
  -- | A function type
  TypeExpressionFunction FunctionTypeExpression |
  -- | An object type
  TypeExpressionObject ObjectTypeExpression |
  -- | A union type (A | B)
  TypeExpressionUnion UnionTypeExpression |
  -- | A parameterized type (e.g., Array<T>, Map<K, V>)
  TypeExpressionParameterized ParameterizedTypeExpression |
  -- | An optional type (?T)
  TypeExpressionOptional TypeExpression |
  -- | The 'any' type
  TypeExpressionAny  |
  -- | The 'void' type
  TypeExpressionVoid  |
  -- | The 'never' type
  TypeExpressionNever 
  deriving (Eq, Ord, Read, Show)

_TypeExpression = (Core.Name "hydra.ext.javaScript.syntax.TypeExpression")

_TypeExpression_identifier = (Core.Name "identifier")

_TypeExpression_literal = (Core.Name "literal")

_TypeExpression_array = (Core.Name "array")

_TypeExpression_function = (Core.Name "function")

_TypeExpression_object = (Core.Name "object")

_TypeExpression_union = (Core.Name "union")

_TypeExpression_parameterized = (Core.Name "parameterized")

_TypeExpression_optional = (Core.Name "optional")

_TypeExpression_any = (Core.Name "any")

_TypeExpression_void = (Core.Name "void")

_TypeExpression_never = (Core.Name "never")

-- | A function type expression
data FunctionTypeExpression = 
  FunctionTypeExpression {
    -- | Type parameters (generics)
    functionTypeExpressionTypeParameters :: [TypeParameter],
    -- | Parameter types
    functionTypeExpressionParameters :: [TypeExpression],
    -- | Return type
    functionTypeExpressionReturnType :: TypeExpression}
  deriving (Eq, Ord, Read, Show)

_FunctionTypeExpression = (Core.Name "hydra.ext.javaScript.syntax.FunctionTypeExpression")

_FunctionTypeExpression_typeParameters = (Core.Name "typeParameters")

_FunctionTypeExpression_parameters = (Core.Name "parameters")

_FunctionTypeExpression_returnType = (Core.Name "returnType")

-- | An array type (T[])
newtype ArrayTypeExpression = 
  ArrayTypeExpression {
    unArrayTypeExpression :: TypeExpression}
  deriving (Eq, Ord, Read, Show)

_ArrayTypeExpression = (Core.Name "hydra.ext.javaScript.syntax.ArrayTypeExpression")

-- | A union type (A | B | C)
type UnionTypeExpression = [TypeExpression]

_UnionTypeExpression = (Core.Name "hydra.ext.javaScript.syntax.UnionTypeExpression")

-- | A parameterized type (e.g., Array<T>, Map<K, V>)
data ParameterizedTypeExpression = 
  ParameterizedTypeExpression {
    parameterizedTypeExpressionBase :: TypeExpression,
    parameterizedTypeExpressionArguments :: [TypeExpression]}
  deriving (Eq, Ord, Read, Show)

_ParameterizedTypeExpression = (Core.Name "hydra.ext.javaScript.syntax.ParameterizedTypeExpression")

_ParameterizedTypeExpression_base = (Core.Name "base")

_ParameterizedTypeExpression_arguments = (Core.Name "arguments")

-- | An object type with property signatures
type ObjectTypeExpression = [PropertySignature]

_ObjectTypeExpression = (Core.Name "hydra.ext.javaScript.syntax.ObjectTypeExpression")

-- | A property signature in an object type
data PropertySignature = 
  PropertySignature {
    -- | Property name
    propertySignatureName :: Identifier,
    -- | Property type
    propertySignatureType :: TypeExpression,
    -- | Whether the property is optional
    propertySignatureOptional :: Bool,
    -- | Whether the property is readonly
    propertySignatureReadonly :: Bool}
  deriving (Eq, Ord, Read, Show)

_PropertySignature = (Core.Name "hydra.ext.javaScript.syntax.PropertySignature")

_PropertySignature_name = (Core.Name "name")

_PropertySignature_type = (Core.Name "type")

_PropertySignature_optional = (Core.Name "optional")

_PropertySignature_readonly = (Core.Name "readonly")

-- | A type parameter (generic)
data TypeParameter = 
  TypeParameter {
    -- | Parameter name
    typeParameterName :: Identifier,
    -- | Optional constraint (extends clause)
    typeParameterConstraint :: (Maybe TypeExpression),
    -- | Optional default type
    typeParameterDefault :: (Maybe TypeExpression)}
  deriving (Eq, Ord, Read, Show)

_TypeParameter = (Core.Name "hydra.ext.javaScript.syntax.TypeParameter")

_TypeParameter_name = (Core.Name "name")

_TypeParameter_constraint = (Core.Name "constraint")

_TypeParameter_default = (Core.Name "default")

-- | A JavaScript expression
data Expression = 
  -- | A simple identifier
  ExpressionIdentifier Identifier |
  -- | A literal value
  ExpressionLiteral Literal |
  -- | An array expression [a, b, c]
  ExpressionArray ArrayExpression |
  -- | An object expression {a: 1, b: 2}
  ExpressionObject ObjectExpression |
  -- | A function expression
  ExpressionFunction FunctionExpression |
  -- | An arrow function expression
  ExpressionArrow ArrowFunctionExpression |
  -- | A function call expression
  ExpressionCall CallExpression |
  -- | A member access expression (obj.prop or obj[prop])
  ExpressionMember MemberExpression |
  -- | A conditional (ternary) expression
  ExpressionConditional ConditionalExpression |
  -- | A binary operation expression
  ExpressionBinary BinaryExpression |
  -- | A unary operation expression
  ExpressionUnary UnaryExpression |
  -- | An assignment expression
  ExpressionAssignment AssignmentExpression |
  -- | A sequence expression (a, b, c)
  ExpressionSequence [Expression] |
  -- | The 'this' keyword
  ExpressionThis  |
  -- | A 'new' expression
  ExpressionNew CallExpression |
  -- | A yield expression
  ExpressionYield (Maybe Expression) |
  -- | An await expression
  ExpressionAwait Expression |
  -- | A spread expression (...x)
  ExpressionSpread SpreadElement |
  -- | A parenthesized expression
  ExpressionParenthesized Expression
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.javaScript.syntax.Expression")

_Expression_identifier = (Core.Name "identifier")

_Expression_literal = (Core.Name "literal")

_Expression_array = (Core.Name "array")

_Expression_object = (Core.Name "object")

_Expression_function = (Core.Name "function")

_Expression_arrow = (Core.Name "arrow")

_Expression_call = (Core.Name "call")

_Expression_member = (Core.Name "member")

_Expression_conditional = (Core.Name "conditional")

_Expression_binary = (Core.Name "binary")

_Expression_unary = (Core.Name "unary")

_Expression_assignment = (Core.Name "assignment")

_Expression_sequence = (Core.Name "sequence")

_Expression_this = (Core.Name "this")

_Expression_new = (Core.Name "new")

_Expression_yield = (Core.Name "yield")

_Expression_await = (Core.Name "await")

_Expression_spread = (Core.Name "spread")

_Expression_parenthesized = (Core.Name "parenthesized")

-- | An array expression [a, b, c]
type ArrayExpression = [ArrayElement]

_ArrayExpression = (Core.Name "hydra.ext.javaScript.syntax.ArrayExpression")

-- | An object expression {a: 1, b: 2}
type ObjectExpression = [Property]

_ObjectExpression = (Core.Name "hydra.ext.javaScript.syntax.ObjectExpression")

-- | A function expression
data FunctionExpression = 
  FunctionExpression {
    -- | Optional function name
    functionExpressionId :: (Maybe Identifier),
    -- | Function parameters
    functionExpressionParams :: [Pattern],
    -- | Function body
    functionExpressionBody :: BlockStatement,
    -- | Whether the function is async
    functionExpressionAsync :: Bool,
    -- | Whether the function is a generator
    functionExpressionGenerator :: Bool}
  deriving (Eq, Ord, Read, Show)

_FunctionExpression = (Core.Name "hydra.ext.javaScript.syntax.FunctionExpression")

_FunctionExpression_id = (Core.Name "id")

_FunctionExpression_params = (Core.Name "params")

_FunctionExpression_body = (Core.Name "body")

_FunctionExpression_async = (Core.Name "async")

_FunctionExpression_generator = (Core.Name "generator")

-- | An arrow function expression
data ArrowFunctionExpression = 
  ArrowFunctionExpression {
    -- | Function parameters
    arrowFunctionExpressionParams :: [Pattern],
    -- | Function body (expression or block)
    arrowFunctionExpressionBody :: ArrowFunctionBody,
    -- | Whether the function is async
    arrowFunctionExpressionAsync :: Bool}
  deriving (Eq, Ord, Read, Show)

_ArrowFunctionExpression = (Core.Name "hydra.ext.javaScript.syntax.ArrowFunctionExpression")

_ArrowFunctionExpression_params = (Core.Name "params")

_ArrowFunctionExpression_body = (Core.Name "body")

_ArrowFunctionExpression_async = (Core.Name "async")

-- | The body of an arrow function (expression or block)
data ArrowFunctionBody = 
  ArrowFunctionBodyExpression Expression |
  ArrowFunctionBodyBlock BlockStatement
  deriving (Eq, Ord, Read, Show)

_ArrowFunctionBody = (Core.Name "hydra.ext.javaScript.syntax.ArrowFunctionBody")

_ArrowFunctionBody_expression = (Core.Name "expression")

_ArrowFunctionBody_block = (Core.Name "block")

-- | A function call expression
data CallExpression = 
  CallExpression {
    -- | The function being called
    callExpressionCallee :: Expression,
    -- | The arguments
    callExpressionArguments :: [Expression],
    -- | Whether using optional chaining (?.)
    callExpressionOptional :: Bool}
  deriving (Eq, Ord, Read, Show)

_CallExpression = (Core.Name "hydra.ext.javaScript.syntax.CallExpression")

_CallExpression_callee = (Core.Name "callee")

_CallExpression_arguments = (Core.Name "arguments")

_CallExpression_optional = (Core.Name "optional")

-- | A member access expression
data MemberExpression = 
  MemberExpression {
    -- | The object
    memberExpressionObject :: Expression,
    -- | The property
    memberExpressionProperty :: Expression,
    -- | Whether using bracket notation (obj[prop])
    memberExpressionComputed :: Bool,
    -- | Whether using optional chaining (?.)
    memberExpressionOptional :: Bool}
  deriving (Eq, Ord, Read, Show)

_MemberExpression = (Core.Name "hydra.ext.javaScript.syntax.MemberExpression")

_MemberExpression_object = (Core.Name "object")

_MemberExpression_property = (Core.Name "property")

_MemberExpression_computed = (Core.Name "computed")

_MemberExpression_optional = (Core.Name "optional")

-- | A conditional (ternary) expression: test ? consequent : alternate
data ConditionalExpression = 
  ConditionalExpression {
    conditionalExpressionTest :: Expression,
    conditionalExpressionConsequent :: Expression,
    conditionalExpressionAlternate :: Expression}
  deriving (Eq, Ord, Read, Show)

_ConditionalExpression = (Core.Name "hydra.ext.javaScript.syntax.ConditionalExpression")

_ConditionalExpression_test = (Core.Name "test")

_ConditionalExpression_consequent = (Core.Name "consequent")

_ConditionalExpression_alternate = (Core.Name "alternate")

-- | A binary operation expression
data BinaryExpression = 
  BinaryExpression {
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionLeft :: Expression,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra.ext.javaScript.syntax.BinaryExpression")

_BinaryExpression_operator = (Core.Name "operator")

_BinaryExpression_left = (Core.Name "left")

_BinaryExpression_right = (Core.Name "right")

-- | A unary operation expression
data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionArgument :: Expression,
    -- | Whether the operator is prefix (true) or postfix (false)
    unaryExpressionPrefix :: Bool}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra.ext.javaScript.syntax.UnaryExpression")

_UnaryExpression_operator = (Core.Name "operator")

_UnaryExpression_argument = (Core.Name "argument")

_UnaryExpression_prefix = (Core.Name "prefix")

-- | An assignment expression
data AssignmentExpression = 
  AssignmentExpression {
    assignmentExpressionOperator :: AssignmentOperator,
    assignmentExpressionLeft :: Pattern,
    assignmentExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_AssignmentExpression = (Core.Name "hydra.ext.javaScript.syntax.AssignmentExpression")

_AssignmentExpression_operator = (Core.Name "operator")

_AssignmentExpression_left = (Core.Name "left")

_AssignmentExpression_right = (Core.Name "right")

-- | A spread element (...x)
newtype SpreadElement = 
  SpreadElement {
    unSpreadElement :: Expression}
  deriving (Eq, Ord, Read, Show)

_SpreadElement = (Core.Name "hydra.ext.javaScript.syntax.SpreadElement")

-- | A property in an object expression
data Property = 
  Property {
    -- | Property key (identifier, literal, or computed)
    propertyKey :: Expression,
    -- | Property value
    propertyValue :: Expression,
    -- | Property kind (init, get, set)
    propertyKind :: PropertyKind,
    -- | Whether the key is computed [expr]
    propertyComputed :: Bool,
    -- | Whether using shorthand syntax {x} for {x: x}
    propertyShorthand :: Bool}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra.ext.javaScript.syntax.Property")

_Property_key = (Core.Name "key")

_Property_value = (Core.Name "value")

_Property_kind = (Core.Name "kind")

_Property_computed = (Core.Name "computed")

_Property_shorthand = (Core.Name "shorthand")

-- | The kind of an object property
data PropertyKind = 
  -- | A normal property initialization
  PropertyKindInit  |
  -- | A getter
  PropertyKindGet  |
  -- | A setter
  PropertyKindSet 
  deriving (Eq, Ord, Read, Show)

_PropertyKind = (Core.Name "hydra.ext.javaScript.syntax.PropertyKind")

_PropertyKind_init = (Core.Name "init")

_PropertyKind_get = (Core.Name "get")

_PropertyKind_set = (Core.Name "set")

-- | An element in an array expression
data ArrayElement = 
  -- | A regular expression element
  ArrayElementExpression Expression |
  -- | A spread element ...x
  ArrayElementSpread SpreadElement |
  -- | An empty slot (elision)
  ArrayElementHole 
  deriving (Eq, Ord, Read, Show)

_ArrayElement = (Core.Name "hydra.ext.javaScript.syntax.ArrayElement")

_ArrayElement_expression = (Core.Name "expression")

_ArrayElement_spread = (Core.Name "spread")

_ArrayElement_hole = (Core.Name "hole")

-- | A binding pattern (for destructuring)
data Pattern = 
  -- | A simple identifier binding
  PatternIdentifier Identifier |
  -- | An object destructuring pattern
  PatternObject ObjectPattern |
  -- | An array destructuring pattern
  PatternArray ArrayPattern |
  -- | A pattern with default value
  PatternAssignment AssignmentPattern |
  -- | A rest element (...x)
  PatternRest RestElement
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra.ext.javaScript.syntax.Pattern")

_Pattern_identifier = (Core.Name "identifier")

_Pattern_object = (Core.Name "object")

_Pattern_array = (Core.Name "array")

_Pattern_assignment = (Core.Name "assignment")

_Pattern_rest = (Core.Name "rest")

-- | An object destructuring pattern {a, b: c}
data ObjectPattern = 
  ObjectPattern {
    -- | The property patterns
    objectPatternProperties :: [ObjectPatternProperty]}
  deriving (Eq, Ord, Read, Show)

_ObjectPattern = (Core.Name "hydra.ext.javaScript.syntax.ObjectPattern")

_ObjectPattern_properties = (Core.Name "properties")

-- | A property in an object pattern
data ObjectPatternProperty = 
  ObjectPatternPropertyProperty Property |
  ObjectPatternPropertyRest RestElement
  deriving (Eq, Ord, Read, Show)

_ObjectPatternProperty = (Core.Name "hydra.ext.javaScript.syntax.ObjectPatternProperty")

_ObjectPatternProperty_property = (Core.Name "property")

_ObjectPatternProperty_rest = (Core.Name "rest")

-- | An array destructuring pattern [a, b, c]
type ArrayPattern = [Maybe Pattern]

_ArrayPattern = (Core.Name "hydra.ext.javaScript.syntax.ArrayPattern")

-- | A pattern with default value (param = default)
data AssignmentPattern = 
  AssignmentPattern {
    assignmentPatternLeft :: Pattern,
    assignmentPatternRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_AssignmentPattern = (Core.Name "hydra.ext.javaScript.syntax.AssignmentPattern")

_AssignmentPattern_left = (Core.Name "left")

_AssignmentPattern_right = (Core.Name "right")

-- | A rest element pattern (...x)
newtype RestElement = 
  RestElement {
    unRestElement :: Pattern}
  deriving (Eq, Ord, Read, Show)

_RestElement = (Core.Name "hydra.ext.javaScript.syntax.RestElement")

-- | A JavaScript statement
data Statement = 
  -- | An expression statement
  StatementExpression Expression |
  -- | A block statement
  StatementBlock BlockStatement |
  -- | An empty statement (;)
  StatementEmpty  |
  -- | A debugger statement
  StatementDebugger  |
  -- | A return statement
  StatementReturn ReturnStatement |
  -- | A break statement
  StatementBreak BreakStatement |
  -- | A continue statement
  StatementContinue ContinueStatement |
  -- | An if statement
  StatementIf IfStatement |
  -- | A switch statement
  StatementSwitch SwitchStatement |
  -- | A throw statement
  StatementThrow ThrowStatement |
  -- | A try statement
  StatementTry TryStatement |
  -- | A while statement
  StatementWhile WhileStatement |
  -- | A do-while statement
  StatementDoWhile DoWhileStatement |
  -- | A for statement
  StatementFor ForStatement |
  -- | A for-in statement
  StatementForIn ForInStatement |
  -- | A for-of statement
  StatementForOf ForOfStatement |
  -- | A variable declaration
  StatementVariableDeclaration VariableDeclaration |
  -- | A function declaration
  StatementFunctionDeclaration FunctionDeclaration |
  -- | A class declaration
  StatementClassDeclaration ClassDeclaration |
  -- | A labeled statement
  StatementLabeled LabeledStatement
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra.ext.javaScript.syntax.Statement")

_Statement_expression = (Core.Name "expression")

_Statement_block = (Core.Name "block")

_Statement_empty = (Core.Name "empty")

_Statement_debugger = (Core.Name "debugger")

_Statement_return = (Core.Name "return")

_Statement_break = (Core.Name "break")

_Statement_continue = (Core.Name "continue")

_Statement_if = (Core.Name "if")

_Statement_switch = (Core.Name "switch")

_Statement_throw = (Core.Name "throw")

_Statement_try = (Core.Name "try")

_Statement_while = (Core.Name "while")

_Statement_doWhile = (Core.Name "doWhile")

_Statement_for = (Core.Name "for")

_Statement_forIn = (Core.Name "forIn")

_Statement_forOf = (Core.Name "forOf")

_Statement_variableDeclaration = (Core.Name "variableDeclaration")

_Statement_functionDeclaration = (Core.Name "functionDeclaration")

_Statement_classDeclaration = (Core.Name "classDeclaration")

_Statement_labeled = (Core.Name "labeled")

-- | A labeled statement
data LabeledStatement = 
  LabeledStatement {
    labeledStatementLabel :: Identifier,
    labeledStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_LabeledStatement = (Core.Name "hydra.ext.javaScript.syntax.LabeledStatement")

_LabeledStatement_label = (Core.Name "label")

_LabeledStatement_body = (Core.Name "body")

-- | A block statement { ... }
type BlockStatement = [Statement]

_BlockStatement = (Core.Name "hydra.ext.javaScript.syntax.BlockStatement")

-- | A variable declaration (var, let, const)
data VariableDeclaration = 
  VariableDeclaration {
    variableDeclarationKind :: VariableKind,
    variableDeclarationDeclarations :: [VariableDeclarator]}
  deriving (Eq, Ord, Read, Show)

_VariableDeclaration = (Core.Name "hydra.ext.javaScript.syntax.VariableDeclaration")

_VariableDeclaration_kind = (Core.Name "kind")

_VariableDeclaration_declarations = (Core.Name "declarations")

-- | A variable declarator (id = init)
data VariableDeclarator = 
  VariableDeclarator {
    variableDeclaratorId :: Pattern,
    variableDeclaratorInit :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_VariableDeclarator = (Core.Name "hydra.ext.javaScript.syntax.VariableDeclarator")

_VariableDeclarator_id = (Core.Name "id")

_VariableDeclarator_init = (Core.Name "init")

-- | The kind of variable declaration
data VariableKind = 
  VariableKindVar  |
  VariableKindLet  |
  VariableKindConst 
  deriving (Eq, Ord, Read, Show)

_VariableKind = (Core.Name "hydra.ext.javaScript.syntax.VariableKind")

_VariableKind_var = (Core.Name "var")

_VariableKind_let = (Core.Name "let")

_VariableKind_const = (Core.Name "const")

-- | An if statement
data IfStatement = 
  IfStatement {
    ifStatementTest :: Expression,
    ifStatementConsequent :: Statement,
    ifStatementAlternate :: (Maybe Statement)}
  deriving (Eq, Ord, Read, Show)

_IfStatement = (Core.Name "hydra.ext.javaScript.syntax.IfStatement")

_IfStatement_test = (Core.Name "test")

_IfStatement_consequent = (Core.Name "consequent")

_IfStatement_alternate = (Core.Name "alternate")

-- | A switch statement
data SwitchStatement = 
  SwitchStatement {
    switchStatementDiscriminant :: Expression,
    switchStatementCases :: [SwitchCase]}
  deriving (Eq, Ord, Read, Show)

_SwitchStatement = (Core.Name "hydra.ext.javaScript.syntax.SwitchStatement")

_SwitchStatement_discriminant = (Core.Name "discriminant")

_SwitchStatement_cases = (Core.Name "cases")

-- | A case clause in a switch statement
data SwitchCase = 
  SwitchCase {
    -- | The test expression (Nothing for default)
    switchCaseTest :: (Maybe Expression),
    -- | The statements to execute
    switchCaseConsequent :: [Statement]}
  deriving (Eq, Ord, Read, Show)

_SwitchCase = (Core.Name "hydra.ext.javaScript.syntax.SwitchCase")

_SwitchCase_test = (Core.Name "test")

_SwitchCase_consequent = (Core.Name "consequent")

-- | A for statement
data ForStatement = 
  ForStatement {
    -- | Initialization
    forStatementInit :: (Maybe ForInit),
    -- | Test condition
    forStatementTest :: (Maybe Expression),
    -- | Update expression
    forStatementUpdate :: (Maybe Expression),
    forStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_ForStatement = (Core.Name "hydra.ext.javaScript.syntax.ForStatement")

_ForStatement_init = (Core.Name "init")

_ForStatement_test = (Core.Name "test")

_ForStatement_update = (Core.Name "update")

_ForStatement_body = (Core.Name "body")

-- | Initialization clause of a for statement
data ForInit = 
  ForInitVariable VariableDeclaration |
  ForInitExpression Expression
  deriving (Eq, Ord, Read, Show)

_ForInit = (Core.Name "hydra.ext.javaScript.syntax.ForInit")

_ForInit_variable = (Core.Name "variable")

_ForInit_expression = (Core.Name "expression")

-- | A for-in statement
data ForInStatement = 
  ForInStatement {
    forInStatementLeft :: ForInLeft,
    forInStatementRight :: Expression,
    forInStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_ForInStatement = (Core.Name "hydra.ext.javaScript.syntax.ForInStatement")

_ForInStatement_left = (Core.Name "left")

_ForInStatement_right = (Core.Name "right")

_ForInStatement_body = (Core.Name "body")

-- | Left-hand side of a for-in or for-of statement
data ForInLeft = 
  ForInLeftVariable VariableDeclaration |
  ForInLeftPattern Pattern
  deriving (Eq, Ord, Read, Show)

_ForInLeft = (Core.Name "hydra.ext.javaScript.syntax.ForInLeft")

_ForInLeft_variable = (Core.Name "variable")

_ForInLeft_pattern = (Core.Name "pattern")

-- | A for-of statement
data ForOfStatement = 
  ForOfStatement {
    -- | Whether this is a for-await-of
    forOfStatementAwait :: Bool,
    forOfStatementLeft :: ForInLeft,
    forOfStatementRight :: Expression,
    forOfStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_ForOfStatement = (Core.Name "hydra.ext.javaScript.syntax.ForOfStatement")

_ForOfStatement_await = (Core.Name "await")

_ForOfStatement_left = (Core.Name "left")

_ForOfStatement_right = (Core.Name "right")

_ForOfStatement_body = (Core.Name "body")

-- | A while statement
data WhileStatement = 
  WhileStatement {
    whileStatementTest :: Expression,
    whileStatementBody :: Statement}
  deriving (Eq, Ord, Read, Show)

_WhileStatement = (Core.Name "hydra.ext.javaScript.syntax.WhileStatement")

_WhileStatement_test = (Core.Name "test")

_WhileStatement_body = (Core.Name "body")

-- | A do-while statement
data DoWhileStatement = 
  DoWhileStatement {
    doWhileStatementBody :: Statement,
    doWhileStatementTest :: Expression}
  deriving (Eq, Ord, Read, Show)

_DoWhileStatement = (Core.Name "hydra.ext.javaScript.syntax.DoWhileStatement")

_DoWhileStatement_body = (Core.Name "body")

_DoWhileStatement_test = (Core.Name "test")

-- | A try statement
data TryStatement = 
  TryStatement {
    tryStatementBlock :: BlockStatement,
    tryStatementHandler :: (Maybe CatchClause),
    tryStatementFinalizer :: (Maybe BlockStatement)}
  deriving (Eq, Ord, Read, Show)

_TryStatement = (Core.Name "hydra.ext.javaScript.syntax.TryStatement")

_TryStatement_block = (Core.Name "block")

_TryStatement_handler = (Core.Name "handler")

_TryStatement_finalizer = (Core.Name "finalizer")

-- | A catch clause
data CatchClause = 
  CatchClause {
    -- | The catch parameter (can be omitted in ES2019+)
    catchClauseParam :: (Maybe Pattern),
    catchClauseBody :: BlockStatement}
  deriving (Eq, Ord, Read, Show)

_CatchClause = (Core.Name "hydra.ext.javaScript.syntax.CatchClause")

_CatchClause_param = (Core.Name "param")

_CatchClause_body = (Core.Name "body")

-- | A throw statement
newtype ThrowStatement = 
  ThrowStatement {
    unThrowStatement :: Expression}
  deriving (Eq, Ord, Read, Show)

_ThrowStatement = (Core.Name "hydra.ext.javaScript.syntax.ThrowStatement")

-- | A return statement
type ReturnStatement = (Maybe Expression)

_ReturnStatement = (Core.Name "hydra.ext.javaScript.syntax.ReturnStatement")

-- | A break statement
type BreakStatement = (Maybe Identifier)

_BreakStatement = (Core.Name "hydra.ext.javaScript.syntax.BreakStatement")

-- | A continue statement
type ContinueStatement = (Maybe Identifier)

_ContinueStatement = (Core.Name "hydra.ext.javaScript.syntax.ContinueStatement")

-- | A function declaration
data FunctionDeclaration = 
  FunctionDeclaration {
    -- | Function name
    functionDeclarationId :: Identifier,
    -- | Function parameters
    functionDeclarationParams :: [Pattern],
    -- | Function body
    functionDeclarationBody :: BlockStatement,
    -- | Whether the function is async
    functionDeclarationAsync :: Bool,
    -- | Whether the function is a generator
    functionDeclarationGenerator :: Bool}
  deriving (Eq, Ord, Read, Show)

_FunctionDeclaration = (Core.Name "hydra.ext.javaScript.syntax.FunctionDeclaration")

_FunctionDeclaration_id = (Core.Name "id")

_FunctionDeclaration_params = (Core.Name "params")

_FunctionDeclaration_body = (Core.Name "body")

_FunctionDeclaration_async = (Core.Name "async")

_FunctionDeclaration_generator = (Core.Name "generator")

-- | A class declaration
data ClassDeclaration = 
  ClassDeclaration {
    -- | Class name
    classDeclarationId :: Identifier,
    -- | Optional superclass
    classDeclarationSuperClass :: (Maybe Expression),
    -- | Class body
    classDeclarationBody :: ClassBody}
  deriving (Eq, Ord, Read, Show)

_ClassDeclaration = (Core.Name "hydra.ext.javaScript.syntax.ClassDeclaration")

_ClassDeclaration_id = (Core.Name "id")

_ClassDeclaration_superClass = (Core.Name "superClass")

_ClassDeclaration_body = (Core.Name "body")

-- | A class body
type ClassBody = [MethodDefinition]

_ClassBody = (Core.Name "hydra.ext.javaScript.syntax.ClassBody")

-- | A method definition in a class
data MethodDefinition = 
  MethodDefinition {
    -- | Method name
    methodDefinitionKey :: Expression,
    -- | Method function
    methodDefinitionValue :: FunctionExpression,
    -- | Method kind
    methodDefinitionKind :: MethodKind,
    -- | Whether the key is computed
    methodDefinitionComputed :: Bool,
    -- | Whether the method is static
    methodDefinitionStatic :: Bool}
  deriving (Eq, Ord, Read, Show)

_MethodDefinition = (Core.Name "hydra.ext.javaScript.syntax.MethodDefinition")

_MethodDefinition_key = (Core.Name "key")

_MethodDefinition_value = (Core.Name "value")

_MethodDefinition_kind = (Core.Name "kind")

_MethodDefinition_computed = (Core.Name "computed")

_MethodDefinition_static = (Core.Name "static")

-- | The kind of a class method
data MethodKind = 
  MethodKindConstructor  |
  MethodKindMethod  |
  MethodKindGet  |
  MethodKindSet 
  deriving (Eq, Ord, Read, Show)

_MethodKind = (Core.Name "hydra.ext.javaScript.syntax.MethodKind")

_MethodKind_constructor = (Core.Name "constructor")

_MethodKind_method = (Core.Name "method")

_MethodKind_get = (Core.Name "get")

_MethodKind_set = (Core.Name "set")

-- | A JavaScript program (module)
data Program = 
  Program {
    -- | The module items
    programBody :: [ModuleItem],
    -- | Whether this is a module or script
    programSourceType :: SourceType}
  deriving (Eq, Ord, Read, Show)

_Program = (Core.Name "hydra.ext.javaScript.syntax.Program")

_Program_body = (Core.Name "body")

_Program_sourceType = (Core.Name "sourceType")

-- | Whether the program is a module or script
data SourceType = 
  SourceTypeModule  |
  SourceTypeScript 
  deriving (Eq, Ord, Read, Show)

_SourceType = (Core.Name "hydra.ext.javaScript.syntax.SourceType")

_SourceType_module = (Core.Name "module")

_SourceType_script = (Core.Name "script")

-- | A top-level item in a module
data ModuleItem = 
  ModuleItemStatement Statement |
  ModuleItemImport ImportDeclaration |
  ModuleItemExport ExportDeclaration
  deriving (Eq, Ord, Read, Show)

_ModuleItem = (Core.Name "hydra.ext.javaScript.syntax.ModuleItem")

_ModuleItem_statement = (Core.Name "statement")

_ModuleItem_import = (Core.Name "import")

_ModuleItem_export = (Core.Name "export")

-- | An import declaration
data ImportDeclaration = 
  ImportDeclaration {
    -- | What to import
    importDeclarationSpecifiers :: [ImportClause],
    -- | The module to import from
    importDeclarationSource :: StringLiteral}
  deriving (Eq, Ord, Read, Show)

_ImportDeclaration = (Core.Name "hydra.ext.javaScript.syntax.ImportDeclaration")

_ImportDeclaration_specifiers = (Core.Name "specifiers")

_ImportDeclaration_source = (Core.Name "source")

-- | An import clause (named, default, or namespace import)
data ImportClause = 
  ImportClauseNamed ImportSpecifier |
  ImportClauseDefault ImportDefaultSpecifier |
  ImportClauseNamespace ImportNamespaceSpecifier
  deriving (Eq, Ord, Read, Show)

_ImportClause = (Core.Name "hydra.ext.javaScript.syntax.ImportClause")

_ImportClause_named = (Core.Name "named")

_ImportClause_default = (Core.Name "default")

_ImportClause_namespace = (Core.Name "namespace")

-- | A named import specifier (import {x as y} from ...)
data ImportSpecifier = 
  ImportSpecifier {
    importSpecifierImported :: Identifier,
    importSpecifierLocal :: Identifier}
  deriving (Eq, Ord, Read, Show)

_ImportSpecifier = (Core.Name "hydra.ext.javaScript.syntax.ImportSpecifier")

_ImportSpecifier_imported = (Core.Name "imported")

_ImportSpecifier_local = (Core.Name "local")

-- | A default import specifier (import x from ...)
newtype ImportDefaultSpecifier = 
  ImportDefaultSpecifier {
    unImportDefaultSpecifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_ImportDefaultSpecifier = (Core.Name "hydra.ext.javaScript.syntax.ImportDefaultSpecifier")

-- | A namespace import specifier (import * as x from ...)
newtype ImportNamespaceSpecifier = 
  ImportNamespaceSpecifier {
    unImportNamespaceSpecifier :: Identifier}
  deriving (Eq, Ord, Read, Show)

_ImportNamespaceSpecifier = (Core.Name "hydra.ext.javaScript.syntax.ImportNamespaceSpecifier")

-- | An export declaration
data ExportDeclaration = 
  -- | Named exports (export {x, y as z})
  ExportDeclarationNamed NamedExport |
  -- | Default export (export default ...)
  ExportDeclarationDefault Expression |
  -- | Export a declaration (export const x = ...)
  ExportDeclarationDeclaration Statement |
  -- | Export all (export * from ...)
  ExportDeclarationAll ExportAllDeclaration
  deriving (Eq, Ord, Read, Show)

_ExportDeclaration = (Core.Name "hydra.ext.javaScript.syntax.ExportDeclaration")

_ExportDeclaration_named = (Core.Name "named")

_ExportDeclaration_default = (Core.Name "default")

_ExportDeclaration_declaration = (Core.Name "declaration")

_ExportDeclaration_all = (Core.Name "all")

-- | Named exports (export {x, y as z})
data NamedExport = 
  NamedExport {
    namedExportSpecifiers :: [ExportSpecifier],
    namedExportSource :: (Maybe StringLiteral)}
  deriving (Eq, Ord, Read, Show)

_NamedExport = (Core.Name "hydra.ext.javaScript.syntax.NamedExport")

_NamedExport_specifiers = (Core.Name "specifiers")

_NamedExport_source = (Core.Name "source")

-- | Export all declaration (export * from ...)
data ExportAllDeclaration = 
  ExportAllDeclaration {
    exportAllDeclarationExported :: (Maybe Identifier),
    exportAllDeclarationSource :: StringLiteral}
  deriving (Eq, Ord, Read, Show)

_ExportAllDeclaration = (Core.Name "hydra.ext.javaScript.syntax.ExportAllDeclaration")

_ExportAllDeclaration_exported = (Core.Name "exported")

_ExportAllDeclaration_source = (Core.Name "source")

-- | An export specifier (x as y)
data ExportSpecifier = 
  ExportSpecifier {
    exportSpecifierLocal :: Identifier,
    exportSpecifierExported :: Identifier}
  deriving (Eq, Ord, Read, Show)

_ExportSpecifier = (Core.Name "hydra.ext.javaScript.syntax.ExportSpecifier")

_ExportSpecifier_local = (Core.Name "local")

_ExportSpecifier_exported = (Core.Name "exported")

-- | A binary operator
data BinaryOperator = 
  -- | +
  BinaryOperatorAdd  |
  -- | -
  BinaryOperatorSubtract  |
  -- | *
  BinaryOperatorMultiply  |
  -- | /
  BinaryOperatorDivide  |
  -- | %
  BinaryOperatorModulo  |
  -- | **
  BinaryOperatorExponentiate  |
  -- | ==
  BinaryOperatorEqual  |
  -- | !=
  BinaryOperatorNotEqual  |
  -- | ===
  BinaryOperatorStrictEqual  |
  -- | !==
  BinaryOperatorStrictNotEqual  |
  -- | <
  BinaryOperatorLessThan  |
  -- | <=
  BinaryOperatorLessThanOrEqual  |
  -- | >
  BinaryOperatorGreaterThan  |
  -- | >=
  BinaryOperatorGreaterThanOrEqual  |
  -- | &&
  BinaryOperatorAnd  |
  -- | ||
  BinaryOperatorOr  |
  -- | ??
  BinaryOperatorNullishCoalescing  |
  -- | &
  BinaryOperatorBitwiseAnd  |
  -- | |
  BinaryOperatorBitwiseOr  |
  -- | ^
  BinaryOperatorBitwiseXor  |
  -- | <<
  BinaryOperatorLeftShift  |
  -- | >>
  BinaryOperatorRightShift  |
  -- | >>>
  BinaryOperatorUnsignedRightShift  |
  -- | in
  BinaryOperatorIn  |
  -- | instanceof
  BinaryOperatorInstanceof 
  deriving (Eq, Ord, Read, Show)

_BinaryOperator = (Core.Name "hydra.ext.javaScript.syntax.BinaryOperator")

_BinaryOperator_add = (Core.Name "add")

_BinaryOperator_subtract = (Core.Name "subtract")

_BinaryOperator_multiply = (Core.Name "multiply")

_BinaryOperator_divide = (Core.Name "divide")

_BinaryOperator_modulo = (Core.Name "modulo")

_BinaryOperator_exponentiate = (Core.Name "exponentiate")

_BinaryOperator_equal = (Core.Name "equal")

_BinaryOperator_notEqual = (Core.Name "notEqual")

_BinaryOperator_strictEqual = (Core.Name "strictEqual")

_BinaryOperator_strictNotEqual = (Core.Name "strictNotEqual")

_BinaryOperator_lessThan = (Core.Name "lessThan")

_BinaryOperator_lessThanOrEqual = (Core.Name "lessThanOrEqual")

_BinaryOperator_greaterThan = (Core.Name "greaterThan")

_BinaryOperator_greaterThanOrEqual = (Core.Name "greaterThanOrEqual")

_BinaryOperator_and = (Core.Name "and")

_BinaryOperator_or = (Core.Name "or")

_BinaryOperator_nullishCoalescing = (Core.Name "nullishCoalescing")

_BinaryOperator_bitwiseAnd = (Core.Name "bitwiseAnd")

_BinaryOperator_bitwiseOr = (Core.Name "bitwiseOr")

_BinaryOperator_bitwiseXor = (Core.Name "bitwiseXor")

_BinaryOperator_leftShift = (Core.Name "leftShift")

_BinaryOperator_rightShift = (Core.Name "rightShift")

_BinaryOperator_unsignedRightShift = (Core.Name "unsignedRightShift")

_BinaryOperator_in = (Core.Name "in")

_BinaryOperator_instanceof = (Core.Name "instanceof")

-- | A unary operator
data UnaryOperator = 
  -- | -
  UnaryOperatorNegate  |
  -- | +
  UnaryOperatorPlus  |
  -- | !
  UnaryOperatorNot  |
  -- | ~
  UnaryOperatorBitwiseNot  |
  -- | typeof
  UnaryOperatorTypeof  |
  -- | void
  UnaryOperatorVoid  |
  -- | delete
  UnaryOperatorDelete  |
  -- | ++
  UnaryOperatorIncrement  |
  -- | --
  UnaryOperatorDecrement 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra.ext.javaScript.syntax.UnaryOperator")

_UnaryOperator_negate = (Core.Name "negate")

_UnaryOperator_plus = (Core.Name "plus")

_UnaryOperator_not = (Core.Name "not")

_UnaryOperator_bitwiseNot = (Core.Name "bitwiseNot")

_UnaryOperator_typeof = (Core.Name "typeof")

_UnaryOperator_void = (Core.Name "void")

_UnaryOperator_delete = (Core.Name "delete")

_UnaryOperator_increment = (Core.Name "increment")

_UnaryOperator_decrement = (Core.Name "decrement")

-- | An assignment operator
data AssignmentOperator = 
  -- | =
  AssignmentOperatorAssign  |
  -- | +=
  AssignmentOperatorAddAssign  |
  -- | -=
  AssignmentOperatorSubtractAssign  |
  -- | *=
  AssignmentOperatorMultiplyAssign  |
  -- | /=
  AssignmentOperatorDivideAssign  |
  -- | %=
  AssignmentOperatorModuloAssign  |
  -- | **=
  AssignmentOperatorExponentiateAssign  |
  -- | <<=
  AssignmentOperatorLeftShiftAssign  |
  -- | >>=
  AssignmentOperatorRightShiftAssign  |
  -- | >>>=
  AssignmentOperatorUnsignedRightShiftAssign  |
  -- | &=
  AssignmentOperatorBitwiseAndAssign  |
  -- | |=
  AssignmentOperatorBitwiseOrAssign  |
  -- | ^=
  AssignmentOperatorBitwiseXorAssign  |
  -- | &&=
  AssignmentOperatorAndAssign  |
  -- | ||=
  AssignmentOperatorOrAssign  |
  -- | ??=
  AssignmentOperatorNullishAssign 
  deriving (Eq, Ord, Read, Show)

_AssignmentOperator = (Core.Name "hydra.ext.javaScript.syntax.AssignmentOperator")

_AssignmentOperator_assign = (Core.Name "assign")

_AssignmentOperator_addAssign = (Core.Name "addAssign")

_AssignmentOperator_subtractAssign = (Core.Name "subtractAssign")

_AssignmentOperator_multiplyAssign = (Core.Name "multiplyAssign")

_AssignmentOperator_divideAssign = (Core.Name "divideAssign")

_AssignmentOperator_moduloAssign = (Core.Name "moduloAssign")

_AssignmentOperator_exponentiateAssign = (Core.Name "exponentiateAssign")

_AssignmentOperator_leftShiftAssign = (Core.Name "leftShiftAssign")

_AssignmentOperator_rightShiftAssign = (Core.Name "rightShiftAssign")

_AssignmentOperator_unsignedRightShiftAssign = (Core.Name "unsignedRightShiftAssign")

_AssignmentOperator_bitwiseAndAssign = (Core.Name "bitwiseAndAssign")

_AssignmentOperator_bitwiseOrAssign = (Core.Name "bitwiseOrAssign")

_AssignmentOperator_bitwiseXorAssign = (Core.Name "bitwiseXorAssign")

_AssignmentOperator_andAssign = (Core.Name "andAssign")

_AssignmentOperator_orAssign = (Core.Name "orAssign")

_AssignmentOperator_nullishAssign = (Core.Name "nullishAssign")

-- | A JavaScript comment
data Comment = 
  -- | A single-line comment (// ...)
  CommentLine String |
  -- | A block comment (/* ... */)
  CommentBlock String |
  -- | A documentation comment (/** ... */, i.e. JSDoc)
  CommentDocumentation DocumentationComment
  deriving (Eq, Ord, Read, Show)

_Comment = (Core.Name "hydra.ext.javaScript.syntax.Comment")

_Comment_line = (Core.Name "line")

_Comment_block = (Core.Name "block")

_Comment_documentation = (Core.Name "documentation")

-- | A documentation comment (JSDoc) with structured tags
data DocumentationComment = 
  DocumentationComment {
    -- | The main description
    documentationCommentDescription :: String,
    -- | Documentation tags (@param, @returns, etc.)
    documentationCommentTags :: [DocumentationTag]}
  deriving (Eq, Ord, Read, Show)

_DocumentationComment = (Core.Name "hydra.ext.javaScript.syntax.DocumentationComment")

_DocumentationComment_description = (Core.Name "description")

_DocumentationComment_tags = (Core.Name "tags")

-- | A documentation tag (@param, @returns, @type, etc.)
data DocumentationTag = 
  DocumentationTag {
    -- | Tag name (param, returns, type, etc.)
    documentationTagName :: String,
    -- | Optional type expression
    documentationTagType :: (Maybe TypeExpression),
    -- | Optional parameter name (for @param)
    documentationTagParamName :: (Maybe Identifier),
    -- | Tag description
    documentationTagDescription :: String}
  deriving (Eq, Ord, Read, Show)

_DocumentationTag = (Core.Name "hydra.ext.javaScript.syntax.DocumentationTag")

_DocumentationTag_name = (Core.Name "name")

_DocumentationTag_type = (Core.Name "type")

_DocumentationTag_paramName = (Core.Name "paramName")

_DocumentationTag_description = (Core.Name "description")

-- | A module item with optional documentation
data ModuleItemWithComments = 
  ModuleItemWithComments {
    -- | The module item
    moduleItemWithCommentsBody :: ModuleItem,
    -- | Optional documentation comment
    moduleItemWithCommentsComments :: (Maybe DocumentationComment)}
  deriving (Eq, Ord, Read, Show)

_ModuleItemWithComments = (Core.Name "hydra.ext.javaScript.syntax.ModuleItemWithComments")

_ModuleItemWithComments_body = (Core.Name "body")

_ModuleItemWithComments_comments = (Core.Name "comments")

-- | A statement with optional documentation
data StatementWithComments = 
  StatementWithComments {
    -- | The statement
    statementWithCommentsBody :: Statement,
    -- | Optional documentation comment
    statementWithCommentsComments :: (Maybe DocumentationComment)}
  deriving (Eq, Ord, Read, Show)

_StatementWithComments = (Core.Name "hydra.ext.javaScript.syntax.StatementWithComments")

_StatementWithComments_body = (Core.Name "body")

_StatementWithComments_comments = (Core.Name "comments")

-- | A function declaration with optional JSDoc
data FunctionDeclarationWithComments = 
  FunctionDeclarationWithComments {
    -- | The function declaration
    functionDeclarationWithCommentsBody :: FunctionDeclaration,
    -- | Optional JSDoc comment
    functionDeclarationWithCommentsComments :: (Maybe DocumentationComment)}
  deriving (Eq, Ord, Read, Show)

_FunctionDeclarationWithComments = (Core.Name "hydra.ext.javaScript.syntax.FunctionDeclarationWithComments")

_FunctionDeclarationWithComments_body = (Core.Name "body")

_FunctionDeclarationWithComments_comments = (Core.Name "comments")

-- | A class declaration with optional JSDoc
data ClassDeclarationWithComments = 
  ClassDeclarationWithComments {
    -- | The class declaration
    classDeclarationWithCommentsBody :: ClassDeclaration,
    -- | Optional JSDoc comment
    classDeclarationWithCommentsComments :: (Maybe DocumentationComment)}
  deriving (Eq, Ord, Read, Show)

_ClassDeclarationWithComments = (Core.Name "hydra.ext.javaScript.syntax.ClassDeclarationWithComments")

_ClassDeclarationWithComments_body = (Core.Name "body")

_ClassDeclarationWithComments_comments = (Core.Name "comments")
