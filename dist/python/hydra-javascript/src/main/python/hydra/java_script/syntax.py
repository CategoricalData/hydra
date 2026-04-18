# Note: this is an automatically generated file. Do not edit.

r"""A JavaScript/ECMAScript syntax model for code generation."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

class Identifier(Node[str]):
    r"""A JavaScript identifier (variable, function, class name, etc.)."""

Identifier.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Identifier")

# A qualified name like 'module.submodule.name'.
QualifiedName: TypeAlias = "frozenlist[Identifier]"

class LiteralString(Node["StringLiteral"]):
    r"""A string literal"""

class LiteralNumber(Node["NumericLiteral"]):
    r"""A numeric literal"""

class LiteralBoolean(Node[bool]):
    r"""A boolean literal (true or false)"""

class LiteralNull:
    r"""The null literal"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralNull)
    def __hash__(self):
        return hash("LiteralNull")

class LiteralUndefined:
    r"""The undefined literal"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralUndefined)
    def __hash__(self):
        return hash("LiteralUndefined")

class LiteralBigInt(Node[int]):
    r"""A BigInt literal (e.g., 123n)"""

class LiteralTemplate(Node["TemplateLiteral"]):
    r"""A template literal"""

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A literal value.
class Literal(metaclass=_LiteralMeta):
    r"""LiteralString | LiteralNumber | LiteralBoolean | LiteralNull | LiteralUndefined | LiteralBigInt | LiteralTemplate"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Literal")
    STRING = hydra.core.Name("string")
    NUMBER = hydra.core.Name("number")
    BOOLEAN = hydra.core.Name("boolean")
    NULL = hydra.core.Name("null")
    UNDEFINED = hydra.core.Name("undefined")
    BIG_INT = hydra.core.Name("bigInt")
    TEMPLATE = hydra.core.Name("template")

@dataclass(frozen=True)
class StringLiteral:
    r"""A string literal with quote style."""

    value: Annotated[str, "The string value"]
    single_quote: Annotated[bool, "Whether to use single quotes (true) or double quotes (false)"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.StringLiteral")
    VALUE = hydra.core.Name("value")
    SINGLE_QUOTE = hydra.core.Name("singleQuote")

@dataclass(frozen=True)
class TemplateLiteral:
    r"""A template literal (backtick string with interpolations)."""

    quasis: Annotated[frozenlist[TemplateElement], "The static string parts"]
    expressions: Annotated[frozenlist[Expression], "The interpolated expressions"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.TemplateLiteral")
    QUASIS = hydra.core.Name("quasis")
    EXPRESSIONS = hydra.core.Name("expressions")

@dataclass(frozen=True)
class TemplateElement:
    r"""A static part of a template literal."""

    value: Annotated[str, "The raw string value"]
    tail: Annotated[bool, "Whether this is the last element"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.TemplateElement")
    VALUE = hydra.core.Name("value")
    TAIL = hydra.core.Name("tail")

class NumericLiteralInteger(Node[int]):
    r"""An integer literal"""

class NumericLiteralFloat(Node[float]):
    r"""A floating-point literal"""

class _NumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A numeric literal (integer or floating-point).
class NumericLiteral(metaclass=_NumericLiteralMeta):
    r"""NumericLiteralInteger | NumericLiteralFloat"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.NumericLiteral")
    INTEGER = hydra.core.Name("integer")
    FLOAT = hydra.core.Name("float")

class TypeAnnotation(Node["TypeExpression"]):
    r"""A type annotation (for JSDoc comments or TypeScript)."""

TypeAnnotation.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.TypeAnnotation")

class TypeExpressionIdentifier(Node["Identifier"]):
    r"""A named type (e.g., 'string', 'number', 'MyClass')"""

class TypeExpressionLiteral(Node["Literal"]):
    r"""A literal type (e.g., 'hello', 42)"""

class TypeExpressionArray(Node["ArrayTypeExpression"]):
    r"""An array type"""

class TypeExpressionFunction(Node["FunctionTypeExpression"]):
    r"""A function type"""

class TypeExpressionObject(Node["ObjectTypeExpression"]):
    r"""An object type"""

class TypeExpressionUnion(Node["UnionTypeExpression"]):
    r"""A union type (A | B)"""

class TypeExpressionParameterized(Node["ParameterizedTypeExpression"]):
    r"""A parameterized type (e.g., Array<T>, Map<K, V>)"""

class TypeExpressionOptional(Node["TypeExpression"]):
    r"""An optional type (?T)"""

class TypeExpressionAny:
    r"""The 'any' type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeExpressionAny)
    def __hash__(self):
        return hash("TypeExpressionAny")

class TypeExpressionVoid:
    r"""The 'void' type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeExpressionVoid)
    def __hash__(self):
        return hash("TypeExpressionVoid")

class TypeExpressionNever:
    r"""The 'never' type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeExpressionNever)
    def __hash__(self):
        return hash("TypeExpressionNever")

class _TypeExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A type expression.
class TypeExpression(metaclass=_TypeExpressionMeta):
    r"""TypeExpressionIdentifier | TypeExpressionLiteral | TypeExpressionArray | TypeExpressionFunction | TypeExpressionObject | TypeExpressionUnion | TypeExpressionParameterized | TypeExpressionOptional | TypeExpressionAny | TypeExpressionVoid | TypeExpressionNever"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.TypeExpression")
    IDENTIFIER = hydra.core.Name("identifier")
    LITERAL = hydra.core.Name("literal")
    ARRAY = hydra.core.Name("array")
    FUNCTION = hydra.core.Name("function")
    OBJECT = hydra.core.Name("object")
    UNION = hydra.core.Name("union")
    PARAMETERIZED = hydra.core.Name("parameterized")
    OPTIONAL = hydra.core.Name("optional")
    ANY = hydra.core.Name("any")
    VOID = hydra.core.Name("void")
    NEVER = hydra.core.Name("never")

@dataclass(frozen=True)
class FunctionTypeExpression:
    r"""A function type expression."""

    type_parameters: Annotated[frozenlist[TypeParameter], "Type parameters (generics)"]
    parameters: Annotated[frozenlist[TypeExpression], "Parameter types"]
    return_type: Annotated[TypeExpression, "Return type"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.FunctionTypeExpression")
    TYPE_PARAMETERS = hydra.core.Name("typeParameters")
    PARAMETERS = hydra.core.Name("parameters")
    RETURN_TYPE = hydra.core.Name("returnType")

class ArrayTypeExpression(Node["TypeExpression"]):
    r"""An array type (T[])."""

ArrayTypeExpression.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ArrayTypeExpression")

# A union type (A | B | C).
UnionTypeExpression: TypeAlias = "frozenlist[TypeExpression]"

@dataclass(frozen=True)
class ParameterizedTypeExpression:
    r"""A parameterized type (e.g., Array<T>, Map<K, V>)."""

    base: TypeExpression
    arguments: frozenlist[TypeExpression]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ParameterizedTypeExpression")
    BASE = hydra.core.Name("base")
    ARGUMENTS = hydra.core.Name("arguments")

# An object type with property signatures.
ObjectTypeExpression: TypeAlias = "frozenlist[PropertySignature]"

@dataclass(frozen=True)
class PropertySignature:
    r"""A property signature in an object type."""

    name: Annotated[Identifier, "Property name"]
    type: Annotated[TypeExpression, "Property type"]
    optional: Annotated[bool, "Whether the property is optional"]
    readonly: Annotated[bool, "Whether the property is readonly"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.PropertySignature")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    OPTIONAL = hydra.core.Name("optional")
    READONLY = hydra.core.Name("readonly")

@dataclass(frozen=True)
class TypeParameter:
    r"""A type parameter (generic)."""

    name: Annotated[Identifier, "Parameter name"]
    constraint: Annotated[Maybe[TypeExpression], "Optional constraint (extends clause)"]
    default: Annotated[Maybe[TypeExpression], "Optional default type"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.TypeParameter")
    NAME = hydra.core.Name("name")
    CONSTRAINT = hydra.core.Name("constraint")
    DEFAULT = hydra.core.Name("default")

class ExpressionIdentifier(Node["Identifier"]):
    r"""A simple identifier"""

class ExpressionLiteral(Node["Literal"]):
    r"""A literal value"""

class ExpressionArray(Node["ArrayExpression"]):
    r"""An array expression [a, b, c]"""

class ExpressionObject(Node["ObjectExpression"]):
    r"""An object expression {a: 1, b: 2}"""

class ExpressionFunction(Node["FunctionExpression"]):
    r"""A function expression"""

class ExpressionArrow(Node["ArrowFunctionExpression"]):
    r"""An arrow function expression"""

class ExpressionCall(Node["CallExpression"]):
    r"""A function call expression"""

class ExpressionMember(Node["MemberExpression"]):
    r"""A member access expression (obj.prop or obj[prop])"""

class ExpressionConditional(Node["ConditionalExpression"]):
    r"""A conditional (ternary) expression"""

class ExpressionBinary(Node["BinaryExpression"]):
    r"""A binary operation expression"""

class ExpressionUnary(Node["UnaryExpression"]):
    r"""A unary operation expression"""

class ExpressionAssignment(Node["AssignmentExpression"]):
    r"""An assignment expression"""

class ExpressionSequence(Node["frozenlist[Expression]"]):
    r"""A sequence expression (a, b, c)"""

class ExpressionThis:
    r"""The 'this' keyword"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExpressionThis)
    def __hash__(self):
        return hash("ExpressionThis")

class ExpressionNew(Node["CallExpression"]):
    r"""A 'new' expression"""

class ExpressionYield(Node["Maybe[Expression]"]):
    r"""A yield expression"""

class ExpressionAwait(Node["Expression"]):
    r"""An await expression"""

class ExpressionSpread(Node["SpreadElement"]):
    r"""A spread expression (...x)"""

class ExpressionParenthesized(Node["Expression"]):
    r"""A parenthesized expression"""

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A JavaScript expression.
class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionIdentifier | ExpressionLiteral | ExpressionArray | ExpressionObject | ExpressionFunction | ExpressionArrow | ExpressionCall | ExpressionMember | ExpressionConditional | ExpressionBinary | ExpressionUnary | ExpressionAssignment | ExpressionSequence | ExpressionThis | ExpressionNew | ExpressionYield | ExpressionAwait | ExpressionSpread | ExpressionParenthesized"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Expression")
    IDENTIFIER = hydra.core.Name("identifier")
    LITERAL = hydra.core.Name("literal")
    ARRAY = hydra.core.Name("array")
    OBJECT = hydra.core.Name("object")
    FUNCTION = hydra.core.Name("function")
    ARROW = hydra.core.Name("arrow")
    CALL = hydra.core.Name("call")
    MEMBER = hydra.core.Name("member")
    CONDITIONAL = hydra.core.Name("conditional")
    BINARY = hydra.core.Name("binary")
    UNARY = hydra.core.Name("unary")
    ASSIGNMENT = hydra.core.Name("assignment")
    SEQUENCE = hydra.core.Name("sequence")
    THIS = hydra.core.Name("this")
    NEW = hydra.core.Name("new")
    YIELD = hydra.core.Name("yield")
    AWAIT = hydra.core.Name("await")
    SPREAD = hydra.core.Name("spread")
    PARENTHESIZED = hydra.core.Name("parenthesized")

# An array expression [a, b, c].
ArrayExpression: TypeAlias = "frozenlist[ArrayElement]"

# An object expression {a: 1, b: 2}.
ObjectExpression: TypeAlias = "frozenlist[Property]"

@dataclass(frozen=True)
class FunctionExpression:
    r"""A function expression."""

    id: Annotated[Maybe[Identifier], "Optional function name"]
    params: Annotated[frozenlist[Pattern], "Function parameters"]
    body: Annotated[BlockStatement, "Function body"]
    async_: Annotated[bool, "Whether the function is async"]
    generator: Annotated[bool, "Whether the function is a generator"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.FunctionExpression")
    ID = hydra.core.Name("id")
    PARAMS = hydra.core.Name("params")
    BODY = hydra.core.Name("body")
    ASYNC = hydra.core.Name("async")
    GENERATOR = hydra.core.Name("generator")

@dataclass(frozen=True)
class ArrowFunctionExpression:
    r"""An arrow function expression."""

    params: Annotated[frozenlist[Pattern], "Function parameters"]
    body: Annotated[ArrowFunctionBody, "Function body (expression or block)"]
    async_: Annotated[bool, "Whether the function is async"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ArrowFunctionExpression")
    PARAMS = hydra.core.Name("params")
    BODY = hydra.core.Name("body")
    ASYNC = hydra.core.Name("async")

class ArrowFunctionBodyExpression(Node["Expression"]):
    ...

class ArrowFunctionBodyBlock(Node["BlockStatement"]):
    ...

class _ArrowFunctionBodyMeta(type):
    def __getitem__(cls, item):
        return object

# The body of an arrow function (expression or block).
class ArrowFunctionBody(metaclass=_ArrowFunctionBodyMeta):
    r"""ArrowFunctionBodyExpression | ArrowFunctionBodyBlock"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ArrowFunctionBody")
    EXPRESSION = hydra.core.Name("expression")
    BLOCK = hydra.core.Name("block")

@dataclass(frozen=True)
class CallExpression:
    r"""A function call expression."""

    callee: Annotated[Expression, "The function being called"]
    arguments: Annotated[frozenlist[Expression], "The arguments"]
    optional: Annotated[bool, "Whether using optional chaining (?.)"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.CallExpression")
    CALLEE = hydra.core.Name("callee")
    ARGUMENTS = hydra.core.Name("arguments")
    OPTIONAL = hydra.core.Name("optional")

@dataclass(frozen=True)
class MemberExpression:
    r"""A member access expression."""

    object: Annotated[Expression, "The object"]
    property: Annotated[Expression, "The property"]
    computed: Annotated[bool, "Whether using bracket notation (obj[prop])"]
    optional: Annotated[bool, "Whether using optional chaining (?.)"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.MemberExpression")
    OBJECT = hydra.core.Name("object")
    PROPERTY = hydra.core.Name("property")
    COMPUTED = hydra.core.Name("computed")
    OPTIONAL = hydra.core.Name("optional")

@dataclass(frozen=True)
class ConditionalExpression:
    r"""A conditional (ternary) expression: test ? consequent : alternate."""

    test: Expression
    consequent: Expression
    alternate: Expression

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ConditionalExpression")
    TEST = hydra.core.Name("test")
    CONSEQUENT = hydra.core.Name("consequent")
    ALTERNATE = hydra.core.Name("alternate")

@dataclass(frozen=True)
class BinaryExpression:
    r"""A binary operation expression."""

    operator: BinaryOperator
    left: Expression
    right: Expression

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.BinaryExpression")
    OPERATOR = hydra.core.Name("operator")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class UnaryExpression:
    r"""A unary operation expression."""

    operator: UnaryOperator
    argument: Expression
    prefix: Annotated[bool, "Whether the operator is prefix (true) or postfix (false)"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.UnaryExpression")
    OPERATOR = hydra.core.Name("operator")
    ARGUMENT = hydra.core.Name("argument")
    PREFIX = hydra.core.Name("prefix")

@dataclass(frozen=True)
class AssignmentExpression:
    r"""An assignment expression."""

    operator: AssignmentOperator
    left: Pattern
    right: Expression

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.AssignmentExpression")
    OPERATOR = hydra.core.Name("operator")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class SpreadElement(Node["Expression"]):
    r"""A spread element (...x)."""

SpreadElement.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.SpreadElement")

@dataclass(frozen=True)
class Property:
    r"""A property in an object expression."""

    key: Annotated[Expression, "Property key (identifier, literal, or computed)"]
    value: Annotated[Expression, "Property value"]
    kind: Annotated[PropertyKind, "Property kind (init, get, set)"]
    computed: Annotated[bool, "Whether the key is computed [expr]"]
    shorthand: Annotated[bool, "Whether using shorthand syntax {x} for {x: x}"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Property")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")
    KIND = hydra.core.Name("kind")
    COMPUTED = hydra.core.Name("computed")
    SHORTHAND = hydra.core.Name("shorthand")

class PropertyKind(Enum):
    r"""The kind of an object property."""

    INIT = hydra.core.Name("init")
    r"""A normal property initialization"""

    GET = hydra.core.Name("get")
    r"""A getter"""

    SET = hydra.core.Name("set")
    r"""A setter"""

PropertyKind.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.PropertyKind")

class ArrayElementExpression(Node["Expression"]):
    r"""A regular expression element"""

class ArrayElementSpread(Node["SpreadElement"]):
    r"""A spread element ...x"""

class ArrayElementHole:
    r"""An empty slot (elision)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ArrayElementHole)
    def __hash__(self):
        return hash("ArrayElementHole")

class _ArrayElementMeta(type):
    def __getitem__(cls, item):
        return object

# An element in an array expression.
class ArrayElement(metaclass=_ArrayElementMeta):
    r"""ArrayElementExpression | ArrayElementSpread | ArrayElementHole"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ArrayElement")
    EXPRESSION = hydra.core.Name("expression")
    SPREAD = hydra.core.Name("spread")
    HOLE = hydra.core.Name("hole")

class PatternIdentifier(Node["Identifier"]):
    r"""A simple identifier binding"""

class PatternObject(Node["ObjectPattern"]):
    r"""An object destructuring pattern"""

class PatternArray(Node["ArrayPattern"]):
    r"""An array destructuring pattern"""

class PatternAssignment(Node["AssignmentPattern"]):
    r"""A pattern with default value"""

class PatternRest(Node["RestElement"]):
    r"""A rest element (...x)"""

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A binding pattern (for destructuring).
class Pattern(metaclass=_PatternMeta):
    r"""PatternIdentifier | PatternObject | PatternArray | PatternAssignment | PatternRest"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Pattern")
    IDENTIFIER = hydra.core.Name("identifier")
    OBJECT = hydra.core.Name("object")
    ARRAY = hydra.core.Name("array")
    ASSIGNMENT = hydra.core.Name("assignment")
    REST = hydra.core.Name("rest")

@dataclass(frozen=True)
class ObjectPattern:
    r"""An object destructuring pattern {a, b: c}."""

    properties: Annotated[frozenlist[ObjectPatternProperty], "The property patterns"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ObjectPattern")
    PROPERTIES = hydra.core.Name("properties")

class ObjectPatternPropertyProperty(Node["Property"]):
    ...

class ObjectPatternPropertyRest(Node["RestElement"]):
    ...

class _ObjectPatternPropertyMeta(type):
    def __getitem__(cls, item):
        return object

# A property in an object pattern.
class ObjectPatternProperty(metaclass=_ObjectPatternPropertyMeta):
    r"""ObjectPatternPropertyProperty | ObjectPatternPropertyRest"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ObjectPatternProperty")
    PROPERTY = hydra.core.Name("property")
    REST = hydra.core.Name("rest")

# An array destructuring pattern [a, b, c].
ArrayPattern: TypeAlias = "frozenlist[Maybe[Pattern]]"

@dataclass(frozen=True)
class AssignmentPattern:
    r"""A pattern with default value (param = default)."""

    left: Pattern
    right: Expression

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.AssignmentPattern")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class RestElement(Node["Pattern"]):
    r"""A rest element pattern (...x)."""

RestElement.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.RestElement")

class StatementExpression(Node["Expression"]):
    r"""An expression statement"""

class StatementBlock(Node["BlockStatement"]):
    r"""A block statement"""

class StatementEmpty:
    r"""An empty statement (;)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StatementEmpty)
    def __hash__(self):
        return hash("StatementEmpty")

class StatementDebugger:
    r"""A debugger statement"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StatementDebugger)
    def __hash__(self):
        return hash("StatementDebugger")

class StatementReturn(Node["ReturnStatement"]):
    r"""A return statement"""

class StatementBreak(Node["BreakStatement"]):
    r"""A break statement"""

class StatementContinue(Node["ContinueStatement"]):
    r"""A continue statement"""

class StatementIf(Node["IfStatement"]):
    r"""An if statement"""

class StatementSwitch(Node["SwitchStatement"]):
    r"""A switch statement"""

class StatementThrow(Node["ThrowStatement"]):
    r"""A throw statement"""

class StatementTry(Node["TryStatement"]):
    r"""A try statement"""

class StatementWhile(Node["WhileStatement"]):
    r"""A while statement"""

class StatementDoWhile(Node["DoWhileStatement"]):
    r"""A do-while statement"""

class StatementFor(Node["ForStatement"]):
    r"""A for statement"""

class StatementForIn(Node["ForInStatement"]):
    r"""A for-in statement"""

class StatementForOf(Node["ForOfStatement"]):
    r"""A for-of statement"""

class StatementVariableDeclaration(Node["VariableDeclaration"]):
    r"""A variable declaration"""

class StatementFunctionDeclaration(Node["FunctionDeclaration"]):
    r"""A function declaration"""

class StatementClassDeclaration(Node["ClassDeclaration"]):
    r"""A class declaration"""

class StatementLabeled(Node["LabeledStatement"]):
    r"""A labeled statement"""

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

# A JavaScript statement.
class Statement(metaclass=_StatementMeta):
    r"""StatementExpression | StatementBlock | StatementEmpty | StatementDebugger | StatementReturn | StatementBreak | StatementContinue | StatementIf | StatementSwitch | StatementThrow | StatementTry | StatementWhile | StatementDoWhile | StatementFor | StatementForIn | StatementForOf | StatementVariableDeclaration | StatementFunctionDeclaration | StatementClassDeclaration | StatementLabeled"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Statement")
    EXPRESSION = hydra.core.Name("expression")
    BLOCK = hydra.core.Name("block")
    EMPTY = hydra.core.Name("empty")
    DEBUGGER = hydra.core.Name("debugger")
    RETURN = hydra.core.Name("return")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    IF = hydra.core.Name("if")
    SWITCH = hydra.core.Name("switch")
    THROW = hydra.core.Name("throw")
    TRY = hydra.core.Name("try")
    WHILE = hydra.core.Name("while")
    DO_WHILE = hydra.core.Name("doWhile")
    FOR = hydra.core.Name("for")
    FOR_IN = hydra.core.Name("forIn")
    FOR_OF = hydra.core.Name("forOf")
    VARIABLE_DECLARATION = hydra.core.Name("variableDeclaration")
    FUNCTION_DECLARATION = hydra.core.Name("functionDeclaration")
    CLASS_DECLARATION = hydra.core.Name("classDeclaration")
    LABELED = hydra.core.Name("labeled")

@dataclass(frozen=True)
class LabeledStatement:
    r"""A labeled statement."""

    label: Identifier
    body: Statement

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.LabeledStatement")
    LABEL = hydra.core.Name("label")
    BODY = hydra.core.Name("body")

# A block statement { ... }.
BlockStatement: TypeAlias = "frozenlist[Statement]"

@dataclass(frozen=True)
class VariableDeclaration:
    r"""A variable declaration (var, let, const)."""

    kind: VariableKind
    declarations: frozenlist[VariableDeclarator]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.VariableDeclaration")
    KIND = hydra.core.Name("kind")
    DECLARATIONS = hydra.core.Name("declarations")

@dataclass(frozen=True)
class VariableDeclarator:
    r"""A variable declarator (id = init)."""

    id: Pattern
    init: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.VariableDeclarator")
    ID = hydra.core.Name("id")
    INIT = hydra.core.Name("init")

class VariableKind(Enum):
    r"""The kind of variable declaration."""

    VAR = hydra.core.Name("var")

    LET = hydra.core.Name("let")

    CONST = hydra.core.Name("const")

VariableKind.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.VariableKind")

@dataclass(frozen=True)
class IfStatement:
    r"""An if statement."""

    test: Expression
    consequent: Statement
    alternate: Maybe[Statement]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.IfStatement")
    TEST = hydra.core.Name("test")
    CONSEQUENT = hydra.core.Name("consequent")
    ALTERNATE = hydra.core.Name("alternate")

@dataclass(frozen=True)
class SwitchStatement:
    r"""A switch statement."""

    discriminant: Expression
    cases: frozenlist[SwitchCase]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.SwitchStatement")
    DISCRIMINANT = hydra.core.Name("discriminant")
    CASES = hydra.core.Name("cases")

@dataclass(frozen=True)
class SwitchCase:
    r"""A case clause in a switch statement."""

    test: Annotated[Maybe[Expression], "The test expression (Nothing for default)"]
    consequent: Annotated[frozenlist[Statement], "The statements to execute"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.SwitchCase")
    TEST = hydra.core.Name("test")
    CONSEQUENT = hydra.core.Name("consequent")

@dataclass(frozen=True)
class ForStatement:
    r"""A for statement."""

    init: Annotated[Maybe[ForInit], "Initialization"]
    test: Annotated[Maybe[Expression], "Test condition"]
    update: Annotated[Maybe[Expression], "Update expression"]
    body: Statement

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ForStatement")
    INIT = hydra.core.Name("init")
    TEST = hydra.core.Name("test")
    UPDATE = hydra.core.Name("update")
    BODY = hydra.core.Name("body")

class ForInitVariable(Node["VariableDeclaration"]):
    ...

class ForInitExpression(Node["Expression"]):
    ...

class _ForInitMeta(type):
    def __getitem__(cls, item):
        return object

# Initialization clause of a for statement.
class ForInit(metaclass=_ForInitMeta):
    r"""ForInitVariable | ForInitExpression"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ForInit")
    VARIABLE = hydra.core.Name("variable")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class ForInStatement:
    r"""A for-in statement."""

    left: ForInLeft
    right: Expression
    body: Statement

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ForInStatement")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")
    BODY = hydra.core.Name("body")

class ForInLeftVariable(Node["VariableDeclaration"]):
    ...

class ForInLeftPattern(Node["Pattern"]):
    ...

class _ForInLeftMeta(type):
    def __getitem__(cls, item):
        return object

# Left-hand side of a for-in or for-of statement.
class ForInLeft(metaclass=_ForInLeftMeta):
    r"""ForInLeftVariable | ForInLeftPattern"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ForInLeft")
    VARIABLE = hydra.core.Name("variable")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class ForOfStatement:
    r"""A for-of statement."""

    await_: Annotated[bool, "Whether this is a for-await-of"]
    left: ForInLeft
    right: Expression
    body: Statement

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ForOfStatement")
    AWAIT = hydra.core.Name("await")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class WhileStatement:
    r"""A while statement."""

    test: Expression
    body: Statement

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.WhileStatement")
    TEST = hydra.core.Name("test")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class DoWhileStatement:
    r"""A do-while statement."""

    body: Statement
    test: Expression

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.DoWhileStatement")
    BODY = hydra.core.Name("body")
    TEST = hydra.core.Name("test")

@dataclass(frozen=True)
class TryStatement:
    r"""A try statement."""

    block: BlockStatement
    handler: Maybe[CatchClause]
    finalizer: Maybe[BlockStatement]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.TryStatement")
    BLOCK = hydra.core.Name("block")
    HANDLER = hydra.core.Name("handler")
    FINALIZER = hydra.core.Name("finalizer")

@dataclass(frozen=True)
class CatchClause:
    r"""A catch clause."""

    param: Annotated[Maybe[Pattern], "The catch parameter (can be omitted in ES2019+)"]
    body: BlockStatement

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.CatchClause")
    PARAM = hydra.core.Name("param")
    BODY = hydra.core.Name("body")

class ThrowStatement(Node["Expression"]):
    r"""A throw statement."""

ThrowStatement.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ThrowStatement")

# A return statement.
ReturnStatement: TypeAlias = "Maybe[Expression]"

# A break statement.
BreakStatement: TypeAlias = "Maybe[Identifier]"

# A continue statement.
ContinueStatement: TypeAlias = "Maybe[Identifier]"

@dataclass(frozen=True)
class FunctionDeclaration:
    r"""A function declaration."""

    id: Annotated[Identifier, "Function name"]
    params: Annotated[frozenlist[Pattern], "Function parameters"]
    body: Annotated[BlockStatement, "Function body"]
    async_: Annotated[bool, "Whether the function is async"]
    generator: Annotated[bool, "Whether the function is a generator"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.FunctionDeclaration")
    ID = hydra.core.Name("id")
    PARAMS = hydra.core.Name("params")
    BODY = hydra.core.Name("body")
    ASYNC = hydra.core.Name("async")
    GENERATOR = hydra.core.Name("generator")

@dataclass(frozen=True)
class ClassDeclaration:
    r"""A class declaration."""

    id: Annotated[Identifier, "Class name"]
    super_class: Annotated[Maybe[Expression], "Optional superclass"]
    body: Annotated[ClassBody, "Class body"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ClassDeclaration")
    ID = hydra.core.Name("id")
    SUPER_CLASS = hydra.core.Name("superClass")
    BODY = hydra.core.Name("body")

# A class body.
ClassBody: TypeAlias = "frozenlist[MethodDefinition]"

@dataclass(frozen=True)
class MethodDefinition:
    r"""A method definition in a class."""

    key: Annotated[Expression, "Method name"]
    value: Annotated[FunctionExpression, "Method function"]
    kind: Annotated[MethodKind, "Method kind"]
    computed: Annotated[bool, "Whether the key is computed"]
    static: Annotated[bool, "Whether the method is static"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.MethodDefinition")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")
    KIND = hydra.core.Name("kind")
    COMPUTED = hydra.core.Name("computed")
    STATIC = hydra.core.Name("static")

class MethodKind(Enum):
    r"""The kind of a class method."""

    CONSTRUCTOR = hydra.core.Name("constructor")

    METHOD = hydra.core.Name("method")

    GET = hydra.core.Name("get")

    SET = hydra.core.Name("set")

MethodKind.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.MethodKind")

@dataclass(frozen=True)
class Program:
    r"""A JavaScript program (module)."""

    body: Annotated[frozenlist[ModuleItem], "The module items"]
    source_type: Annotated[SourceType, "Whether this is a module or script"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Program")
    BODY = hydra.core.Name("body")
    SOURCE_TYPE = hydra.core.Name("sourceType")

class SourceType(Enum):
    r"""Whether the program is a module or script."""

    MODULE = hydra.core.Name("module")

    SCRIPT = hydra.core.Name("script")

SourceType.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.SourceType")

class ModuleItemStatement(Node["Statement"]):
    ...

class ModuleItemImport(Node["ImportDeclaration"]):
    ...

class ModuleItemExport(Node["ExportDeclaration"]):
    ...

class _ModuleItemMeta(type):
    def __getitem__(cls, item):
        return object

# A top-level item in a module.
class ModuleItem(metaclass=_ModuleItemMeta):
    r"""ModuleItemStatement | ModuleItemImport | ModuleItemExport"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ModuleItem")
    STATEMENT = hydra.core.Name("statement")
    IMPORT = hydra.core.Name("import")
    EXPORT = hydra.core.Name("export")

@dataclass(frozen=True)
class ImportDeclaration:
    r"""An import declaration."""

    specifiers: Annotated[frozenlist[ImportClause], "What to import"]
    source: Annotated[StringLiteral, "The module to import from"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ImportDeclaration")
    SPECIFIERS = hydra.core.Name("specifiers")
    SOURCE = hydra.core.Name("source")

class ImportClauseNamed(Node["ImportSpecifier"]):
    ...

class ImportClauseDefault(Node["ImportDefaultSpecifier"]):
    ...

class ImportClauseNamespace(Node["ImportNamespaceSpecifier"]):
    ...

class _ImportClauseMeta(type):
    def __getitem__(cls, item):
        return object

# An import clause (named, default, or namespace import).
class ImportClause(metaclass=_ImportClauseMeta):
    r"""ImportClauseNamed | ImportClauseDefault | ImportClauseNamespace"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ImportClause")
    NAMED = hydra.core.Name("named")
    DEFAULT = hydra.core.Name("default")
    NAMESPACE = hydra.core.Name("namespace")

@dataclass(frozen=True)
class ImportSpecifier:
    r"""A named import specifier (import {x as y} from ...)."""

    imported: Identifier
    local: Identifier

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ImportSpecifier")
    IMPORTED = hydra.core.Name("imported")
    LOCAL = hydra.core.Name("local")

class ImportDefaultSpecifier(Node["Identifier"]):
    r"""A default import specifier (import x from ...)."""

ImportDefaultSpecifier.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ImportDefaultSpecifier")

class ImportNamespaceSpecifier(Node["Identifier"]):
    r"""A namespace import specifier (import * as x from ...)."""

ImportNamespaceSpecifier.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ImportNamespaceSpecifier")

class ExportDeclarationNamed(Node["NamedExport"]):
    r"""Named exports (export {x, y as z})"""

class ExportDeclarationDefault(Node["Expression"]):
    r"""Default export (export default ...)"""

class ExportDeclarationDeclaration(Node["Statement"]):
    r"""Export a declaration (export const x = ...)"""

class ExportDeclarationAll(Node["ExportAllDeclaration"]):
    r"""Export all (export * from ...)"""

class _ExportDeclarationMeta(type):
    def __getitem__(cls, item):
        return object

# An export declaration.
class ExportDeclaration(metaclass=_ExportDeclarationMeta):
    r"""ExportDeclarationNamed | ExportDeclarationDefault | ExportDeclarationDeclaration | ExportDeclarationAll"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ExportDeclaration")
    NAMED = hydra.core.Name("named")
    DEFAULT = hydra.core.Name("default")
    DECLARATION = hydra.core.Name("declaration")
    ALL = hydra.core.Name("all")

@dataclass(frozen=True)
class NamedExport:
    r"""Named exports (export {x, y as z})."""

    specifiers: frozenlist[ExportSpecifier]
    source: Maybe[StringLiteral]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.NamedExport")
    SPECIFIERS = hydra.core.Name("specifiers")
    SOURCE = hydra.core.Name("source")

@dataclass(frozen=True)
class ExportAllDeclaration:
    r"""Export all declaration (export * from ...)."""

    exported: Maybe[Identifier]
    source: StringLiteral

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ExportAllDeclaration")
    EXPORTED = hydra.core.Name("exported")
    SOURCE = hydra.core.Name("source")

@dataclass(frozen=True)
class ExportSpecifier:
    r"""An export specifier (x as y)."""

    local: Identifier
    exported: Identifier

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ExportSpecifier")
    LOCAL = hydra.core.Name("local")
    EXPORTED = hydra.core.Name("exported")

class BinaryOperator(Enum):
    r"""A binary operator."""

    ADD = hydra.core.Name("add")
    r"""+"""

    SUBTRACT = hydra.core.Name("subtract")
    r"""-"""

    MULTIPLY = hydra.core.Name("multiply")
    r"""*"""

    DIVIDE = hydra.core.Name("divide")
    r"""/"""

    MODULO = hydra.core.Name("modulo")
    r"""%"""

    EXPONENTIATE = hydra.core.Name("exponentiate")
    r"""**"""

    EQUAL = hydra.core.Name("equal")
    r"""=="""

    NOT_EQUAL = hydra.core.Name("notEqual")
    r"""!="""

    STRICT_EQUAL = hydra.core.Name("strictEqual")
    r"""==="""

    STRICT_NOT_EQUAL = hydra.core.Name("strictNotEqual")
    r"""!=="""

    LESS_THAN = hydra.core.Name("lessThan")
    r"""<"""

    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")
    r"""<="""

    GREATER_THAN = hydra.core.Name("greaterThan")
    r""">"""

    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")
    r""">="""

    AND = hydra.core.Name("and")
    r"""&&"""

    OR = hydra.core.Name("or")
    r"""||"""

    NULLISH_COALESCING = hydra.core.Name("nullishCoalescing")
    r"""??"""

    BITWISE_AND = hydra.core.Name("bitwiseAnd")
    r"""&"""

    BITWISE_OR = hydra.core.Name("bitwiseOr")
    r"""|"""

    BITWISE_XOR = hydra.core.Name("bitwiseXor")
    r"""^"""

    LEFT_SHIFT = hydra.core.Name("leftShift")
    r"""<<"""

    RIGHT_SHIFT = hydra.core.Name("rightShift")
    r""">>"""

    UNSIGNED_RIGHT_SHIFT = hydra.core.Name("unsignedRightShift")
    r""">>>"""

    IN = hydra.core.Name("in")
    r"""in"""

    INSTANCEOF = hydra.core.Name("instanceof")
    r"""instanceof"""

BinaryOperator.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.BinaryOperator")

class UnaryOperator(Enum):
    r"""A unary operator."""

    NEGATE = hydra.core.Name("negate")
    r"""-"""

    PLUS = hydra.core.Name("plus")
    r"""+"""

    NOT = hydra.core.Name("not")
    r"""!"""

    BITWISE_NOT = hydra.core.Name("bitwiseNot")
    r"""~"""

    TYPEOF = hydra.core.Name("typeof")
    r"""typeof"""

    VOID = hydra.core.Name("void")
    r"""void"""

    DELETE = hydra.core.Name("delete")
    r"""delete"""

    INCREMENT = hydra.core.Name("increment")
    r"""++"""

    DECREMENT = hydra.core.Name("decrement")
    r"""--"""

UnaryOperator.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.UnaryOperator")

class AssignmentOperator(Enum):
    r"""An assignment operator."""

    ASSIGN = hydra.core.Name("assign")
    r"""="""

    ADD_ASSIGN = hydra.core.Name("addAssign")
    r"""+="""

    SUBTRACT_ASSIGN = hydra.core.Name("subtractAssign")
    r"""-="""

    MULTIPLY_ASSIGN = hydra.core.Name("multiplyAssign")
    r"""*="""

    DIVIDE_ASSIGN = hydra.core.Name("divideAssign")
    r"""/="""

    MODULO_ASSIGN = hydra.core.Name("moduloAssign")
    r"""%="""

    EXPONENTIATE_ASSIGN = hydra.core.Name("exponentiateAssign")
    r"""**="""

    LEFT_SHIFT_ASSIGN = hydra.core.Name("leftShiftAssign")
    r"""<<="""

    RIGHT_SHIFT_ASSIGN = hydra.core.Name("rightShiftAssign")
    r""">>="""

    UNSIGNED_RIGHT_SHIFT_ASSIGN = hydra.core.Name("unsignedRightShiftAssign")
    r""">>>="""

    BITWISE_AND_ASSIGN = hydra.core.Name("bitwiseAndAssign")
    r"""&="""

    BITWISE_OR_ASSIGN = hydra.core.Name("bitwiseOrAssign")
    r"""|="""

    BITWISE_XOR_ASSIGN = hydra.core.Name("bitwiseXorAssign")
    r"""^="""

    AND_ASSIGN = hydra.core.Name("andAssign")
    r"""&&="""

    OR_ASSIGN = hydra.core.Name("orAssign")
    r"""||="""

    NULLISH_ASSIGN = hydra.core.Name("nullishAssign")
    r"""??="""

AssignmentOperator.TYPE_ = hydra.core.Name("hydra.javaScript.syntax.AssignmentOperator")

class CommentLine(Node[str]):
    r"""A single-line comment (// ...)"""

class CommentBlock(Node[str]):
    r"""A block comment (/* ... */)"""

class CommentDocumentation(Node["DocumentationComment"]):
    r"""A documentation comment (/** ... */, i.e. JSDoc)"""

class _CommentMeta(type):
    def __getitem__(cls, item):
        return object

# A JavaScript comment.
class Comment(metaclass=_CommentMeta):
    r"""CommentLine | CommentBlock | CommentDocumentation"""

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.Comment")
    LINE = hydra.core.Name("line")
    BLOCK = hydra.core.Name("block")
    DOCUMENTATION = hydra.core.Name("documentation")

@dataclass(frozen=True)
class DocumentationComment:
    r"""A documentation comment (JSDoc) with structured tags."""

    description: Annotated[str, "The main description"]
    tags: Annotated[frozenlist[DocumentationTag], "Documentation tags (@param, @returns, etc.)"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.DocumentationComment")
    DESCRIPTION = hydra.core.Name("description")
    TAGS = hydra.core.Name("tags")

@dataclass(frozen=True)
class DocumentationTag:
    r"""A documentation tag (@param, @returns, @type, etc.)."""

    name: Annotated[str, "Tag name (param, returns, type, etc.)"]
    type: Annotated[Maybe[TypeExpression], "Optional type expression"]
    param_name: Annotated[Maybe[Identifier], "Optional parameter name (for @param)"]
    description: Annotated[str, "Tag description"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.DocumentationTag")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")
    PARAM_NAME = hydra.core.Name("paramName")
    DESCRIPTION = hydra.core.Name("description")

@dataclass(frozen=True)
class ModuleItemWithComments:
    r"""A module item with optional documentation."""

    body: Annotated[ModuleItem, "The module item"]
    comments: Annotated[Maybe[DocumentationComment], "Optional documentation comment"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ModuleItemWithComments")
    BODY = hydra.core.Name("body")
    COMMENTS = hydra.core.Name("comments")

@dataclass(frozen=True)
class StatementWithComments:
    r"""A statement with optional documentation."""

    body: Annotated[Statement, "The statement"]
    comments: Annotated[Maybe[DocumentationComment], "Optional documentation comment"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.StatementWithComments")
    BODY = hydra.core.Name("body")
    COMMENTS = hydra.core.Name("comments")

@dataclass(frozen=True)
class FunctionDeclarationWithComments:
    r"""A function declaration with optional JSDoc."""

    body: Annotated[FunctionDeclaration, "The function declaration"]
    comments: Annotated[Maybe[DocumentationComment], "Optional JSDoc comment"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.FunctionDeclarationWithComments")
    BODY = hydra.core.Name("body")
    COMMENTS = hydra.core.Name("comments")

@dataclass(frozen=True)
class ClassDeclarationWithComments:
    r"""A class declaration with optional JSDoc."""

    body: Annotated[ClassDeclaration, "The class declaration"]
    comments: Annotated[Maybe[DocumentationComment], "Optional JSDoc comment"]

    TYPE_ = hydra.core.Name("hydra.javaScript.syntax.ClassDeclarationWithComments")
    BODY = hydra.core.Name("body")
    COMMENTS = hydra.core.Name("comments")
