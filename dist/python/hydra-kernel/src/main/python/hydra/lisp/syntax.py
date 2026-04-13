# Note: this is an automatically generated file. Do not edit.

r"""A unified Lisp syntax model covering Clojure, Emacs Lisp, Common Lisp, and Scheme (R7RS). Designed for code generation from Hydra types and terms."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class Program:
    r"""A Lisp program, consisting of a sequence of top-level forms."""

    dialect: Annotated[Dialect, "The target Lisp dialect"]
    module: Annotated[Maybe[ModuleDeclaration], "Optional module/namespace declaration"]
    imports: Annotated[frozenlist[ImportDeclaration], "Import/require declarations"]
    exports: Annotated[frozenlist[ExportDeclaration], "Export/provide declarations"]
    forms: Annotated[frozenlist[TopLevelFormWithComments], "The top-level forms in the program"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Program")
    DIALECT = hydra.core.Name("dialect")
    MODULE = hydra.core.Name("module")
    IMPORTS = hydra.core.Name("imports")
    EXPORTS = hydra.core.Name("exports")
    FORMS = hydra.core.Name("forms")

class TopLevelFormFunction(Node["FunctionDefinition"]):
    r"""A named function definition"""

class TopLevelFormVariable(Node["VariableDefinition"]):
    r"""A global variable definition"""

class TopLevelFormConstant(Node["ConstantDefinition"]):
    r"""A constant definition"""

class TopLevelFormRecordType(Node["RecordTypeDefinition"]):
    r"""A record/struct type definition"""

class TopLevelFormMacro(Node["MacroDefinition"]):
    r"""A macro definition"""

class TopLevelFormExpression(Node["Expression"]):
    r"""A bare expression at the top level"""

class _TopLevelFormMeta(type):
    def __getitem__(cls, item):
        return object

# A top-level form in a Lisp program.
class TopLevelForm(metaclass=_TopLevelFormMeta):
    r"""TopLevelFormFunction | TopLevelFormVariable | TopLevelFormConstant | TopLevelFormRecordType | TopLevelFormMacro | TopLevelFormExpression"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.TopLevelForm")
    FUNCTION = hydra.core.Name("function")
    VARIABLE = hydra.core.Name("variable")
    CONSTANT = hydra.core.Name("constant")
    RECORD_TYPE = hydra.core.Name("recordType")
    MACRO = hydra.core.Name("macro")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class TopLevelFormWithComments:
    r"""A top-level form together with optional documentation."""

    doc: Annotated[Maybe[Docstring], "Optional documentation string"]
    comment: Annotated[Maybe[Comment], "Optional comment"]
    form: Annotated[TopLevelForm, "The form itself"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.TopLevelFormWithComments")
    DOC = hydra.core.Name("doc")
    COMMENT = hydra.core.Name("comment")
    FORM = hydra.core.Name("form")

@dataclass(frozen=True)
class FunctionDefinition:
    r"""A named function definition. Serializes as (defn name [params] body) in Clojure, (defun name (params) body) in Emacs Lisp and Common Lisp, (define (name params) body) in Scheme."""

    name: Annotated[Symbol, "The function name"]
    params: Annotated[frozenlist[Symbol], "The parameter list"]
    rest_param: Annotated[Maybe[Symbol], "Optional rest/variadic parameter"]
    doc: Annotated[Maybe[Docstring], "Optional docstring"]
    type_hints: Annotated[frozenlist[TypeHint], "Optional type hints for parameters and return type"]
    body: Annotated[frozenlist[Expression], "The function body (one or more expressions)"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.FunctionDefinition")
    NAME = hydra.core.Name("name")
    PARAMS = hydra.core.Name("params")
    REST_PARAM = hydra.core.Name("restParam")
    DOC = hydra.core.Name("doc")
    TYPE_HINTS = hydra.core.Name("typeHints")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class VariableDefinition:
    r"""A global variable definition. Serializes as (def name value) in Clojure, (defvar name value) in Emacs Lisp and Common Lisp, (define name value) in Scheme."""

    name: Annotated[Symbol, "The variable name"]
    value: Annotated[Expression, "The initial value"]
    doc: Annotated[Maybe[Docstring], "Optional docstring"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.VariableDefinition")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class ConstantDefinition:
    r"""A constant definition. Serializes as (def ^:const name value) in Clojure, (defconst name value) in Emacs Lisp, (defconstant name value) in Common Lisp. Scheme has no dedicated constant form; uses define."""

    name: Annotated[Symbol, "The constant name"]
    value: Annotated[Expression, "The constant value"]
    doc: Annotated[Maybe[Docstring], "Optional docstring"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ConstantDefinition")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class RecordTypeDefinition:
    r"""A record/struct type definition. Serializes as (defrecord Name [fields]) in Clojure, (cl-defstruct name fields) in Emacs Lisp, (defstruct name fields) in Common Lisp, (define-record-type <Name> ...) in Scheme."""

    name: Annotated[Symbol, "The record type name"]
    fields: Annotated[frozenlist[FieldDefinition], "The field definitions"]
    doc: Annotated[Maybe[Docstring], "Optional docstring"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.RecordTypeDefinition")
    NAME = hydra.core.Name("name")
    FIELDS = hydra.core.Name("fields")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class FieldDefinition:
    r"""A field in a record type definition."""

    name: Annotated[Symbol, "The field name"]
    default_value: Annotated[Maybe[Expression], "Optional default value"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.FieldDefinition")
    NAME = hydra.core.Name("name")
    DEFAULT_VALUE = hydra.core.Name("defaultValue")

@dataclass(frozen=True)
class MacroDefinition:
    r"""A macro definition. Serializes as (defmacro name [params] body) in Clojure, (defmacro name (params) body) in Emacs Lisp and Common Lisp, (define-syntax name ...) in Scheme."""

    name: Annotated[Symbol, "The macro name"]
    params: Annotated[frozenlist[Symbol], "The parameter list"]
    rest_param: Annotated[Maybe[Symbol], "Optional rest parameter"]
    body: Annotated[frozenlist[Expression], "The macro body"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.MacroDefinition")
    NAME = hydra.core.Name("name")
    PARAMS = hydra.core.Name("params")
    REST_PARAM = hydra.core.Name("restParam")
    BODY = hydra.core.Name("body")

class ExpressionApplication(Node["Application"]):
    r"""Function application: (f arg1 arg2 ...)"""

class ExpressionLambda(Node["Lambda"]):
    r"""Anonymous function"""

class ExpressionLet(Node["LetExpression"]):
    r"""Local variable binding"""

class ExpressionIf(Node["IfExpression"]):
    r"""Conditional expression"""

class ExpressionCond(Node["CondExpression"]):
    r"""Multi-branch conditional"""

class ExpressionCase(Node["CaseExpression"]):
    r"""Case/match dispatch"""

class ExpressionAnd(Node["AndExpression"]):
    r"""Logical and (short-circuiting)"""

class ExpressionOr(Node["OrExpression"]):
    r"""Logical or (short-circuiting)"""

class ExpressionNot(Node["NotExpression"]):
    r"""Logical negation"""

class ExpressionDo(Node["DoExpression"]):
    r"""Sequential evaluation (progn/do/begin)"""

class ExpressionBegin(Node["BeginExpression"]):
    r"""Sequential evaluation (explicit begin block)"""

class ExpressionVariable(Node["VariableReference"]):
    r"""Variable reference"""

class ExpressionLiteral(Node["Literal"]):
    r"""A literal value"""

class ExpressionList(Node["ListLiteral"]):
    r"""A list literal"""

class ExpressionVector(Node["VectorLiteral"]):
    r"""A vector literal"""

class ExpressionMap(Node["MapLiteral"]):
    r"""A map/association literal"""

class ExpressionSet(Node["SetLiteral"]):
    r"""A set literal"""

class ExpressionCons(Node["ConsExpression"]):
    r"""A cons expression"""

class ExpressionDottedPair(Node["DottedPair"]):
    r"""A dotted pair literal"""

class ExpressionFieldAccess(Node["FieldAccess"]):
    r"""Field access on a record/struct"""

class ExpressionTypeAnnotation(Node["TypeAnnotation"]):
    r"""A type-annotated expression"""

class ExpressionQuote(Node["QuoteExpression"]):
    r"""A quoted expression"""

class ExpressionQuasiquote(Node["QuasiquoteExpression"]):
    r"""A quasiquoted expression"""

class ExpressionUnquote(Node["UnquoteExpression"]):
    r"""An unquoted expression within a quasiquote"""

class ExpressionSplicingUnquote(Node["SplicingUnquoteExpression"]):
    r"""A splicing unquote within a quasiquote"""

class ExpressionSExpression(Node["SExpression"]):
    r"""An arbitrary S-expression (escape hatch for dialect-specific forms)"""

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A Lisp expression.
class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionApplication | ExpressionLambda | ExpressionLet | ExpressionIf | ExpressionCond | ExpressionCase | ExpressionAnd | ExpressionOr | ExpressionNot | ExpressionDo | ExpressionBegin | ExpressionVariable | ExpressionLiteral | ExpressionList | ExpressionVector | ExpressionMap | ExpressionSet | ExpressionCons | ExpressionDottedPair | ExpressionFieldAccess | ExpressionTypeAnnotation | ExpressionQuote | ExpressionQuasiquote | ExpressionUnquote | ExpressionSplicingUnquote | ExpressionSExpression"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Expression")
    APPLICATION = hydra.core.Name("application")
    LAMBDA = hydra.core.Name("lambda")
    LET = hydra.core.Name("let")
    IF = hydra.core.Name("if")
    COND = hydra.core.Name("cond")
    CASE = hydra.core.Name("case")
    AND = hydra.core.Name("and")
    OR = hydra.core.Name("or")
    NOT = hydra.core.Name("not")
    DO = hydra.core.Name("do")
    BEGIN = hydra.core.Name("begin")
    VARIABLE = hydra.core.Name("variable")
    LITERAL = hydra.core.Name("literal")
    LIST = hydra.core.Name("list")
    VECTOR = hydra.core.Name("vector")
    MAP = hydra.core.Name("map")
    SET = hydra.core.Name("set")
    CONS = hydra.core.Name("cons")
    DOTTED_PAIR = hydra.core.Name("dottedPair")
    FIELD_ACCESS = hydra.core.Name("fieldAccess")
    TYPE_ANNOTATION = hydra.core.Name("typeAnnotation")
    QUOTE = hydra.core.Name("quote")
    QUASIQUOTE = hydra.core.Name("quasiquote")
    UNQUOTE = hydra.core.Name("unquote")
    SPLICING_UNQUOTE = hydra.core.Name("splicingUnquote")
    S_EXPRESSION = hydra.core.Name("sExpression")

@dataclass(frozen=True)
class Application:
    r"""Function application: (function arg1 arg2 ...)."""

    function: Annotated[Expression, "The function being applied"]
    arguments: Annotated[frozenlist[Expression], "The arguments"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Application")
    FUNCTION = hydra.core.Name("function")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class Lambda:
    r"""An anonymous function. Serializes as (fn [params] body) in Clojure, (lambda (params) body) in Emacs Lisp, Common Lisp, and Scheme. If name is provided, emits (fn name [params] body) in Clojure for self-reference."""

    name: Annotated[Maybe[Symbol], "Optional name for self-referential lambdas (Clojure named fn)"]
    params: Annotated[frozenlist[Symbol], "The parameter list"]
    rest_param: Annotated[Maybe[Symbol], "Optional rest parameter"]
    body: Annotated[frozenlist[Expression], "The lambda body"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Lambda")
    NAME = hydra.core.Name("name")
    PARAMS = hydra.core.Name("params")
    REST_PARAM = hydra.core.Name("restParam")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class VariableReference:
    r"""A reference to a variable by name."""

    name: Annotated[Symbol, "The variable name"]
    function_namespace: Annotated[bool, "Whether to reference from the function namespace. In Lisp-2 dialects (Common Lisp), this emits #'name. In Lisp-1 dialects, this has no effect."]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.VariableReference")
    NAME = hydra.core.Name("name")
    FUNCTION_NAMESPACE = hydra.core.Name("functionNamespace")

@dataclass(frozen=True)
class FieldAccess:
    r"""Field access on a record/struct. Serializes as (:field record) in Clojure, (struct-field record) in Emacs Lisp and Common Lisp, (record-field record) in Scheme."""

    record_type: Annotated[Symbol, "The record type name (used to form accessor name)"]
    field: Annotated[Symbol, "The field name"]
    target: Annotated[Expression, "The expression being accessed"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.FieldAccess")
    RECORD_TYPE = hydra.core.Name("recordType")
    FIELD = hydra.core.Name("field")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class TypeAnnotation:
    r"""An expression with a type annotation."""

    expression: Annotated[Expression, "The annotated expression"]
    type: Annotated[TypeSpecifier, "The type specifier"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.TypeAnnotation")
    EXPRESSION = hydra.core.Name("expression")
    TYPE = hydra.core.Name("type")

@dataclass(frozen=True)
class IfExpression:
    r"""Conditional: (if test then else)."""

    condition: Annotated[Expression, "The test expression"]
    then: Annotated[Expression, "The then branch"]
    else_: Annotated[Maybe[Expression], "Optional else branch"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.IfExpression")
    CONDITION = hydra.core.Name("condition")
    THEN = hydra.core.Name("then")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class CondExpression:
    r"""Multi-branch conditional. Serializes as (cond test1 expr1 test2 expr2 :else default) in Clojure, (cond (test1 expr1) (test2 expr2) (t default)) in Emacs Lisp and Common Lisp, (cond (test1 expr1) (test2 expr2) (else default)) in Scheme."""

    clauses: Annotated[frozenlist[CondClause], "The condition-expression pairs"]
    default: Annotated[Maybe[Expression], "Optional default expression"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.CondExpression")
    CLAUSES = hydra.core.Name("clauses")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class CondClause:
    r"""A clause in a cond expression."""

    condition: Annotated[Expression, "The test condition"]
    body: Annotated[Expression, "The result expression"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.CondClause")
    CONDITION = hydra.core.Name("condition")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class CaseExpression:
    r"""Case dispatch on a value. Serializes as (case x key1 expr1 key2 expr2 default) in Clojure, (case x (key1 expr1) (key2 expr2) (otherwise default)) in Common Lisp, (case x ((key1) expr1) ((key2) expr2) (else default)) in Scheme."""

    scrutinee: Annotated[Expression, "The expression being dispatched on"]
    clauses: Annotated[frozenlist[CaseClause], "The case clauses"]
    default: Annotated[Maybe[Expression], "Optional default clause"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.CaseExpression")
    SCRUTINEE = hydra.core.Name("scrutinee")
    CLAUSES = hydra.core.Name("clauses")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class CaseClause:
    r"""A clause in a case expression."""

    keys: Annotated[frozenlist[Expression], "The matching keys (one or more datum values)"]
    body: Annotated[Expression, "The result expression"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.CaseClause")
    KEYS = hydra.core.Name("keys")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class AndExpression:
    r"""Logical and: (and expr1 expr2 ...)."""

    expressions: Annotated[frozenlist[Expression], "The operand expressions"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.AndExpression")
    EXPRESSIONS = hydra.core.Name("expressions")

@dataclass(frozen=True)
class OrExpression:
    r"""Logical or: (or expr1 expr2 ...)."""

    expressions: Annotated[frozenlist[Expression], "The operand expressions"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.OrExpression")
    EXPRESSIONS = hydra.core.Name("expressions")

@dataclass(frozen=True)
class NotExpression:
    r"""Logical negation: (not expr)."""

    expression: Annotated[Expression, "The operand expression"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.NotExpression")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class DoExpression:
    r"""Sequential evaluation of expressions, returning the last. Serializes as (do expr1 expr2 ...) in Clojure, (progn expr1 expr2 ...) in Emacs Lisp and Common Lisp, (begin expr1 expr2 ...) in Scheme."""

    expressions: Annotated[frozenlist[Expression], "The expressions to evaluate in sequence"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.DoExpression")
    EXPRESSIONS = hydra.core.Name("expressions")

@dataclass(frozen=True)
class BeginExpression:
    r"""An explicit begin block (distinct from do for Scheme compatibility)."""

    expressions: Annotated[frozenlist[Expression], "The expressions to evaluate in sequence"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.BeginExpression")
    EXPRESSIONS = hydra.core.Name("expressions")

@dataclass(frozen=True)
class QuoteExpression:
    r"""A quoted form: 'expr or (quote expr)."""

    body: Annotated[Expression, "The quoted form"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.QuoteExpression")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class QuasiquoteExpression:
    r"""A quasiquoted form: `expr."""

    body: Annotated[Expression, "The quasiquoted form"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.QuasiquoteExpression")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class UnquoteExpression:
    r"""An unquoted form within a quasiquote: ~expr or ,expr."""

    body: Annotated[Expression, "The unquoted form"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.UnquoteExpression")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class SplicingUnquoteExpression:
    r"""A splicing unquote within a quasiquote: ~@expr or ,@expr."""

    body: Annotated[Expression, "The spliced form"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.SplicingUnquoteExpression")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class LetExpression:
    r"""Local variable bindings. Serializes as (let [x 1 y 2] body) in Clojure (always sequential), (let ((x 1) (y 2)) body) or (let* ...) in other dialects."""

    kind: Annotated[LetKind, "The kind of let (parallel or sequential)"]
    bindings: Annotated[frozenlist[LetBinding], "The variable bindings"]
    body: Annotated[frozenlist[Expression], "The body expressions"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.LetExpression")
    KIND = hydra.core.Name("kind")
    BINDINGS = hydra.core.Name("bindings")
    BODY = hydra.core.Name("body")

class LetKind(Enum):
    r"""The kind of let binding."""

    PARALLEL = hydra.core.Name("parallel")

    SEQUENTIAL = hydra.core.Name("sequential")

    RECURSIVE = hydra.core.Name("recursive")

LetKind.TYPE_ = hydra.core.Name("hydra.lisp.syntax.LetKind")

class LetBindingSimple(Node["SimpleBinding"]):
    r"""A simple name-value binding"""

class LetBindingDestructuring(Node["DestructuringBinding"]):
    r"""A destructuring binding"""

class _LetBindingMeta(type):
    def __getitem__(cls, item):
        return object

# A single binding in a let expression.
class LetBinding(metaclass=_LetBindingMeta):
    r"""LetBindingSimple | LetBindingDestructuring"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.LetBinding")
    SIMPLE = hydra.core.Name("simple")
    DESTRUCTURING = hydra.core.Name("destructuring")

@dataclass(frozen=True)
class SimpleBinding:
    r"""A simple name-value binding in a let expression."""

    name: Annotated[Symbol, "The bound variable"]
    value: Annotated[Expression, "The value expression"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.SimpleBinding")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class DestructuringBinding:
    r"""A destructuring binding in a let expression."""

    pattern: Annotated[DestructuringPattern, "The destructuring pattern"]
    value: Annotated[Expression, "The value to destructure"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.DestructuringBinding")
    PATTERN = hydra.core.Name("pattern")
    VALUE = hydra.core.Name("value")

class DestructuringPatternSequential(Node["frozenlist[Symbol]"]):
    r"""Sequential destructuring: [a b c] in Clojure, (a b c) in others"""

class DestructuringPatternAssociative(Node["frozenlist[Symbol]"]):
    r"""Associative/map destructuring: {:keys [a b]} in Clojure"""

class DestructuringPatternRest(Node["frozenlist[Symbol]"]):
    r"""Destructuring with a rest element: [a b & rest] (leading symbols + rest symbol)"""

class _DestructuringPatternMeta(type):
    def __getitem__(cls, item):
        return object

# A destructuring pattern.
class DestructuringPattern(metaclass=_DestructuringPatternMeta):
    r"""DestructuringPatternSequential | DestructuringPatternAssociative | DestructuringPatternRest"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.DestructuringPattern")
    SEQUENTIAL = hydra.core.Name("sequential")
    ASSOCIATIVE = hydra.core.Name("associative")
    REST = hydra.core.Name("rest")

class PatternConstructor(Node["ConstructorPattern"]):
    r"""A constructor pattern (for union/sum type matching)"""

class PatternLiteral(Node["LiteralPattern"]):
    r"""A literal pattern"""

class PatternWildcard(Node["WildcardPattern"]):
    r"""A wildcard pattern matching anything"""

class PatternVariable(Node["Symbol"]):
    r"""A variable pattern that binds the matched value"""

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

# A pattern for use in case expressions or match forms.
class Pattern(metaclass=_PatternMeta):
    r"""PatternConstructor | PatternLiteral | PatternWildcard | PatternVariable"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Pattern")
    CONSTRUCTOR = hydra.core.Name("constructor")
    LITERAL = hydra.core.Name("literal")
    WILDCARD = hydra.core.Name("wildcard")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class ConstructorPattern:
    r"""A constructor pattern matching a tagged value."""

    constructor: Annotated[Symbol, "The constructor/tag name"]
    arguments: Annotated[frozenlist[Pattern], "The sub-patterns for constructor arguments"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ConstructorPattern")
    CONSTRUCTOR = hydra.core.Name("constructor")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class LiteralPattern:
    r"""A pattern matching a literal value."""

    value: Annotated[Literal, "The literal to match"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.LiteralPattern")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class WildcardPattern:
    r"""A wildcard pattern that matches any value."""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.WildcardPattern")

class LiteralInteger(Node["IntegerLiteral"]):
    r"""An integer literal"""

class LiteralFloat(Node["FloatLiteral"]):
    r"""A floating-point literal"""

class LiteralString(Node[str]):
    r"""A string literal"""

class LiteralCharacter(Node["CharacterLiteral"]):
    r"""A character literal"""

class LiteralBoolean(Node[bool]):
    r"""A boolean literal (dialect-specific rendering)"""

class LiteralNil:
    r"""Nil/null/empty list (dialect-specific rendering)"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralNil)
    def __hash__(self):
        return hash("LiteralNil")

class LiteralKeyword(Node["Keyword"]):
    r"""A keyword literal"""

class LiteralSymbol(Node["Symbol"]):
    r"""A quoted symbol literal"""

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

# A Lisp literal value.
class Literal(metaclass=_LiteralMeta):
    r"""LiteralInteger | LiteralFloat | LiteralString | LiteralCharacter | LiteralBoolean | LiteralNil | LiteralKeyword | LiteralSymbol"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Literal")
    INTEGER = hydra.core.Name("integer")
    FLOAT = hydra.core.Name("float")
    STRING = hydra.core.Name("string")
    CHARACTER = hydra.core.Name("character")
    BOOLEAN = hydra.core.Name("boolean")
    NIL = hydra.core.Name("nil")
    KEYWORD = hydra.core.Name("keyword")
    SYMBOL = hydra.core.Name("symbol")

@dataclass(frozen=True)
class IntegerLiteral:
    r"""An integer literal."""

    value: Annotated[int, "The integer value"]
    bigint: Annotated[bool, "Whether this is explicitly a big integer (e.g. 42N in Clojure)"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.IntegerLiteral")
    VALUE = hydra.core.Name("value")
    BIGINT = hydra.core.Name("bigint")

@dataclass(frozen=True)
class FloatLiteral:
    r"""A floating-point literal."""

    value: Annotated[Decimal, "The float value"]
    precision: Annotated[Maybe[str], "Optional precision hint (e.g. 3.14d0 vs 3.14f0 in Common Lisp)"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.FloatLiteral")
    VALUE = hydra.core.Name("value")
    PRECISION = hydra.core.Name("precision")

@dataclass(frozen=True)
class CharacterLiteral:
    r"""A character literal. Concrete syntax varies: \a (Clojure), ?a (Emacs Lisp), #\a (Common Lisp, Scheme)."""

    value: Annotated[str, "The character value"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.CharacterLiteral")
    VALUE = hydra.core.Name("value")

class BooleanStyle(Enum):
    r"""The style of boolean literals in a dialect."""

    TRUE_FALSE = hydra.core.Name("trueFalse")

    T_NIL = hydra.core.Name("tNil")

    HASH_T_F = hydra.core.Name("hashTF")

BooleanStyle.TYPE_ = hydra.core.Name("hydra.lisp.syntax.BooleanStyle")

class NilStyle(Enum):
    r"""The style of nil/null in a dialect."""

    NIL = hydra.core.Name("nil")

    EMPTY_LIST = hydra.core.Name("emptyList")

NilStyle.TYPE_ = hydra.core.Name("hydra.lisp.syntax.NilStyle")

class Symbol(Node[str]):
    r"""A Lisp symbol (identifier)."""

Symbol.TYPE_ = hydra.core.Name("hydra.lisp.syntax.Symbol")

@dataclass(frozen=True)
class Keyword:
    r"""A keyword (self-evaluating symbol). Serializes as :name in Clojure, Emacs Lisp, and Common Lisp."""

    name: Annotated[str, "The keyword name (without the leading colon)"]
    namespace: Annotated[Maybe[str], "Optional namespace (e.g. my.ns/foo in Clojure)"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Keyword")
    NAME = hydra.core.Name("name")
    NAMESPACE = hydra.core.Name("namespace")

@dataclass(frozen=True)
class QualifiedSymbol:
    r"""A namespace-qualified symbol. Serializes as ns/name in Clojure, pkg:name or pkg::name in Common Lisp."""

    namespace: Annotated[str, "The namespace or package"]
    name: Annotated[str, "The local name"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.QualifiedSymbol")
    NAMESPACE = hydra.core.Name("namespace")
    NAME = hydra.core.Name("name")

class NamespaceName(Node[str]):
    r"""A namespace or package name."""

NamespaceName.TYPE_ = hydra.core.Name("hydra.lisp.syntax.NamespaceName")

@dataclass(frozen=True)
class ListLiteral:
    r"""A list literal: '(1 2 3) or (list 1 2 3)."""

    elements: Annotated[frozenlist[Expression], "The list elements"]
    quoted: Annotated[bool, "Whether to use quote syntax vs constructor syntax"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ListLiteral")
    ELEMENTS = hydra.core.Name("elements")
    QUOTED = hydra.core.Name("quoted")

@dataclass(frozen=True)
class VectorLiteral:
    r"""A vector literal. Serializes as [1 2 3] in Clojure and Emacs Lisp, #(1 2 3) in Common Lisp and Scheme."""

    elements: Annotated[frozenlist[Expression], "The vector elements"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.VectorLiteral")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class MapLiteral:
    r"""A map/dictionary literal. Serializes as {:a 1 :b 2} in Clojure, as an alist '((a . 1) (b . 2)) in other dialects."""

    entries: Annotated[frozenlist[MapEntry], "The key-value pairs"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.MapLiteral")
    ENTRIES = hydra.core.Name("entries")

@dataclass(frozen=True)
class MapEntry:
    r"""A key-value pair in a map literal."""

    key: Annotated[Expression, "The key expression"]
    value: Annotated[Expression, "The value expression"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.MapEntry")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class SetLiteral:
    r"""A set literal. Serializes as #{1 2 3} in Clojure. Other dialects use a list-based construction."""

    elements: Annotated[frozenlist[Expression], "The set elements"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.SetLiteral")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class ConsExpression:
    r"""A cons expression: (cons head tail)."""

    head: Annotated[Expression, "The head element"]
    tail: Annotated[Expression, "The tail (typically a list or another cons)"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ConsExpression")
    HEAD = hydra.core.Name("head")
    TAIL = hydra.core.Name("tail")

@dataclass(frozen=True)
class DottedPair:
    r"""A dotted pair literal: '(a . b). Not available in Clojure."""

    car: Annotated[Expression, "The first element"]
    cdr: Annotated[Expression, "The second element"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.DottedPair")
    CAR = hydra.core.Name("car")
    CDR = hydra.core.Name("cdr")

@dataclass(frozen=True)
class TypeHint:
    r"""A type hint or annotation. In Clojure: ^Type name. In Common Lisp: (declare (type Type name)). In Scheme and Emacs Lisp: typically unused."""

    name: Annotated[Symbol, "The annotated symbol"]
    type: Annotated[TypeSpecifier, "The type specifier"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.TypeHint")
    NAME = hydra.core.Name("name")
    TYPE = hydra.core.Name("type")

class TypeSpecifierNamed(Node["Symbol"]):
    r"""A named type reference"""

class TypeSpecifierList(Node["TypeSpecifier"]):
    r"""A list type"""

class TypeSpecifierFunction(Node["frozenlist[TypeSpecifier]"]):
    r"""A function type (params and return)"""

class TypeSpecifierMaybe(Node["TypeSpecifier"]):
    r"""An optional type"""

class TypeSpecifierMap(Node["frozenlist[TypeSpecifier]"]):
    r"""A map type (key and value type specifiers)"""

class TypeSpecifierSet(Node["TypeSpecifier"]):
    r"""A set type"""

class TypeSpecifierPair(Node["frozenlist[TypeSpecifier]"]):
    r"""A pair/tuple type (two type specifiers)"""

class TypeSpecifierEither(Node["frozenlist[TypeSpecifier]"]):
    r"""An either/union type (two type specifiers)"""

class TypeSpecifierUnit:
    r"""The unit type"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TypeSpecifierUnit)
    def __hash__(self):
        return hash("TypeSpecifierUnit")

class _TypeSpecifierMeta(type):
    def __getitem__(cls, item):
        return object

# A type specifier.
class TypeSpecifier(metaclass=_TypeSpecifierMeta):
    r"""TypeSpecifierNamed | TypeSpecifierList | TypeSpecifierFunction | TypeSpecifierMaybe | TypeSpecifierMap | TypeSpecifierSet | TypeSpecifierPair | TypeSpecifierEither | TypeSpecifierUnit"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.TypeSpecifier")
    NAMED = hydra.core.Name("named")
    LIST = hydra.core.Name("list")
    FUNCTION = hydra.core.Name("function")
    MAYBE = hydra.core.Name("maybe")
    MAP = hydra.core.Name("map")
    SET = hydra.core.Name("set")
    PAIR = hydra.core.Name("pair")
    EITHER = hydra.core.Name("either")
    UNIT = hydra.core.Name("unit")

@dataclass(frozen=True)
class ModuleDeclaration:
    r"""A module/namespace declaration. Serializes as (ns name ...) in Clojure, (provide 'name) in Emacs Lisp, (defpackage :name ... ) (in-package :name) in Common Lisp, (define-library (name) ...) in Scheme."""

    name: Annotated[NamespaceName, "The module/namespace name"]
    doc: Annotated[Maybe[Docstring], "Optional module documentation"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ModuleDeclaration")
    NAME = hydra.core.Name("name")
    DOC = hydra.core.Name("doc")

@dataclass(frozen=True)
class ImportDeclaration:
    r"""An import/require declaration. Serializes as (:require [name ...]) in Clojure, (require 'name) in Emacs Lisp, (:use :name) or (:import-from :name ...) in Common Lisp, (import (name)) in Scheme."""

    module: Annotated[NamespaceName, "The module being imported"]
    spec: Annotated[ImportSpec, "Import specification"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ImportDeclaration")
    MODULE = hydra.core.Name("module")
    SPEC = hydra.core.Name("spec")

class ImportSpecAll:
    r"""Import everything"""

    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ImportSpecAll)
    def __hash__(self):
        return hash("ImportSpecAll")

class ImportSpecAlias(Node["Symbol"]):
    r"""Import with an alias: (:require [name :as alias]) in Clojure"""

class ImportSpecOnly(Node["frozenlist[Symbol]"]):
    r"""Import specific symbols: (:require [name :refer [sym1 sym2]]) in Clojure"""

class ImportSpecRename(Node["frozenlist[frozenlist[Symbol]]"]):
    r"""Import with renaming: list of (from, to) symbol pairs"""

class _ImportSpecMeta(type):
    def __getitem__(cls, item):
        return object

# An import specification describing how to import symbols.
class ImportSpec(metaclass=_ImportSpecMeta):
    r"""ImportSpecAll | ImportSpecAlias | ImportSpecOnly | ImportSpecRename"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ImportSpec")
    ALL = hydra.core.Name("all")
    ALIAS = hydra.core.Name("alias")
    ONLY = hydra.core.Name("only")
    RENAME = hydra.core.Name("rename")

@dataclass(frozen=True)
class ExportDeclaration:
    r"""An export/provide declaration. Serializes as (provide 'name) in Emacs Lisp, (:export :sym1 :sym2) in Common Lisp, (export sym1 sym2) in Scheme. In Clojure, symbols are public by default."""

    symbols: Annotated[frozenlist[Symbol], "The symbols to export"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.ExportDeclaration")
    SYMBOLS = hydra.core.Name("symbols")

@dataclass(frozen=True)
class Comment:
    r"""A comment."""

    style: Annotated[CommentStyle, "The comment style"]
    text: Annotated[str, "The comment text"]

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.Comment")
    STYLE = hydra.core.Name("style")
    TEXT = hydra.core.Name("text")

class CommentStyle(Enum):
    r"""The style of a comment."""

    LINE = hydra.core.Name("line")

    BLOCK = hydra.core.Name("block")

    DATUM = hydra.core.Name("datum")

CommentStyle.TYPE_ = hydra.core.Name("hydra.lisp.syntax.CommentStyle")

class Docstring(Node[str]):
    r"""A documentation string."""

Docstring.TYPE_ = hydra.core.Name("hydra.lisp.syntax.Docstring")

class Dialect(Enum):
    r"""A Lisp dialect."""

    CLOJURE = hydra.core.Name("clojure")

    EMACS_LISP = hydra.core.Name("emacsLisp")

    COMMON_LISP = hydra.core.Name("commonLisp")

    SCHEME = hydra.core.Name("scheme")

Dialect.TYPE_ = hydra.core.Name("hydra.lisp.syntax.Dialect")

class SExpressionAtom(Node[str]):
    r"""An atomic value"""

class SExpressionList(Node["frozenlist[SExpression]"]):
    r"""A list of S-expressions"""

class _SExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A raw S-expression. This is an escape hatch for expressing arbitrary Lisp forms that do not fit into the structured AST above.
class SExpression(metaclass=_SExpressionMeta):
    r"""SExpressionAtom | SExpressionList"""

    TYPE_ = hydra.core.Name("hydra.lisp.syntax.SExpression")
    ATOM = hydra.core.Name("atom")
    LIST = hydra.core.Name("list")
