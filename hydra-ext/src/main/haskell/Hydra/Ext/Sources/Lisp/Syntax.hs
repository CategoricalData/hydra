-- | A unified Lisp syntax model covering features common to Clojure, Emacs Lisp, Common Lisp,
-- | and Scheme (R7RS). The model captures the full S-expression structure and special forms
-- | needed for code generation across dialects. Dialect-specific serialization is left to
-- | per-dialect serializers.

module Hydra.Ext.Sources.Lisp.Syntax where

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


ns :: Namespace
ns = Namespace "hydra.ext.lisp.syntax"

define :: String -> Type -> Binding
define = datatype ns

lisp :: String -> Type
lisp = typeref ns

module_ :: Module
module_ = Module ns (map toTypeDef definitions) [Core.ns] [Core.ns] $
    Just ("A unified Lisp syntax model covering Clojure, Emacs Lisp, Common Lisp, and Scheme (R7RS)."
      ++ " Designed for code generation from Hydra types and terms.")
  where
    definitions = toplevel ++ defnTypes ++ expressions ++ specialForms ++ bindings
      ++ patterns ++ literals ++ names ++ collections ++ types ++ modules ++ misc

    -- Top-level constructs
    toplevel = [
      program,
      topLevelForm,
      topLevelFormWithComments]

    -- Definitions
    defnTypes = [
      functionDefinition,
      variableDefinition,
      constantDefinition,
      recordTypeDefinition,
      fieldDefinition,
      macroDefinition]

    -- Expressions
    expressions = [
      expression,
      application,
      lambda,
      variableReference,
      fieldAccess,
      typeAnnotation]

    -- Special forms
    specialForms = [
      ifExpression,
      condExpression,
      condClause,
      caseExpression,
      caseClause,
      andExpression,
      orExpression,
      notExpression,
      doExpression,
      beginExpression,
      quoteExpression,
      quasiquoteExpression,
      unquoteExpression,
      splicingUnquoteExpression]

    -- Let bindings
    bindings = [
      letExpression,
      letKind,
      letBinding,
      simpleBinding,
      destructuringBinding,
      destructuringPattern]

    -- Patterns (for case/match)
    patterns = [
      pattern,
      constructorPattern,
      literalPattern,
      wildcardPattern]

    -- Literals
    literals = [
      literal,
      integerLiteral,
      floatLiteral,
      characterLiteral,
      booleanStyle,
      nilStyle]

    -- Names and symbols
    names = [
      symbol,
      keyword,
      qualifiedSymbol,
      namespaceName]

    -- Collection literals
    collections = [
      listLiteral,
      vectorLiteral,
      mapLiteral,
      mapEntry,
      setLiteral,
      consExpression,
      dottedPair]

    -- Type-related (for dialects with type hints)
    types = [
      typeHint,
      typeSpecifier]

    -- Module system
    modules = [
      moduleDeclaration,
      importDeclaration,
      importSpec,
      exportDeclaration]

    -- Miscellaneous
    misc = [
      comment,
      commentStyle,
      docstring,
      dialect,
      sExpression]


-- ================================================================================================
-- Top-level constructs
-- ================================================================================================

program :: Binding
program = define "Program" $
  doc "A Lisp program, consisting of a sequence of top-level forms" $
  T.record [
    "dialect">:
      doc "The target Lisp dialect" $
      lisp "Dialect",
    "module">:
      doc "Optional module/namespace declaration" $
      T.maybe (lisp "ModuleDeclaration"),
    "imports">:
      doc "Import/require declarations" $
      T.list (lisp "ImportDeclaration"),
    "exports">:
      doc "Export/provide declarations" $
      T.list (lisp "ExportDeclaration"),
    "forms">:
      doc "The top-level forms in the program" $
      T.list (lisp "TopLevelFormWithComments")]

topLevelForm :: Binding
topLevelForm = define "TopLevelForm" $
  doc "A top-level form in a Lisp program" $
  T.union [
    "function">:
      doc "A named function definition" $
      lisp "FunctionDefinition",
    "variable">:
      doc "A global variable definition" $
      lisp "VariableDefinition",
    "constant">:
      doc "A constant definition" $
      lisp "ConstantDefinition",
    "recordType">:
      doc "A record/struct type definition" $
      lisp "RecordTypeDefinition",
    "macro">:
      doc "A macro definition" $
      lisp "MacroDefinition",
    "expression">:
      doc "A bare expression at the top level" $
      lisp "Expression"]

topLevelFormWithComments :: Binding
topLevelFormWithComments = define "TopLevelFormWithComments" $
  doc "A top-level form together with optional documentation" $
  T.record [
    "doc">:
      doc "Optional documentation string" $
      T.maybe (lisp "Docstring"),
    "comment">:
      doc "Optional comment" $
      T.maybe (lisp "Comment"),
    "form">:
      doc "The form itself" $
      lisp "TopLevelForm"]


-- ================================================================================================
-- Definitions
-- ================================================================================================

functionDefinition :: Binding
functionDefinition = define "FunctionDefinition" $
  doc ("A named function definition."
    ++ " Serializes as (defn name [params] body) in Clojure,"
    ++ " (defun name (params) body) in Emacs Lisp and Common Lisp,"
    ++ " (define (name params) body) in Scheme") $
  T.record [
    "name">:
      doc "The function name" $
      lisp "Symbol",
    "params">:
      doc "The parameter list" $
      T.list (lisp "Symbol"),
    "restParam">:
      doc "Optional rest/variadic parameter" $
      T.maybe (lisp "Symbol"),
    "doc">:
      doc "Optional docstring" $
      T.maybe (lisp "Docstring"),
    "typeHints">:
      doc "Optional type hints for parameters and return type" $
      T.list (lisp "TypeHint"),
    "body">:
      doc "The function body (one or more expressions)" $
      T.list (lisp "Expression")]

variableDefinition :: Binding
variableDefinition = define "VariableDefinition" $
  doc ("A global variable definition."
    ++ " Serializes as (def name value) in Clojure,"
    ++ " (defvar name value) in Emacs Lisp and Common Lisp,"
    ++ " (define name value) in Scheme") $
  T.record [
    "name">:
      doc "The variable name" $
      lisp "Symbol",
    "value">:
      doc "The initial value" $
      lisp "Expression",
    "doc">:
      doc "Optional docstring" $
      T.maybe (lisp "Docstring")]

constantDefinition :: Binding
constantDefinition = define "ConstantDefinition" $
  doc ("A constant definition."
    ++ " Serializes as (def ^:const name value) in Clojure,"
    ++ " (defconst name value) in Emacs Lisp,"
    ++ " (defconstant name value) in Common Lisp."
    ++ " Scheme has no dedicated constant form; uses define.") $
  T.record [
    "name">:
      doc "The constant name" $
      lisp "Symbol",
    "value">:
      doc "The constant value" $
      lisp "Expression",
    "doc">:
      doc "Optional docstring" $
      T.maybe (lisp "Docstring")]

recordTypeDefinition :: Binding
recordTypeDefinition = define "RecordTypeDefinition" $
  doc ("A record/struct type definition."
    ++ " Serializes as (defrecord Name [fields]) in Clojure,"
    ++ " (cl-defstruct name fields) in Emacs Lisp,"
    ++ " (defstruct name fields) in Common Lisp,"
    ++ " (define-record-type <Name> ...) in Scheme") $
  T.record [
    "name">:
      doc "The record type name" $
      lisp "Symbol",
    "fields">:
      doc "The field definitions" $
      T.list (lisp "FieldDefinition"),
    "doc">:
      doc "Optional docstring" $
      T.maybe (lisp "Docstring")]

fieldDefinition :: Binding
fieldDefinition = define "FieldDefinition" $
  doc "A field in a record type definition" $
  T.record [
    "name">:
      doc "The field name" $
      lisp "Symbol",
    "defaultValue">:
      doc "Optional default value" $
      T.maybe (lisp "Expression")]

macroDefinition :: Binding
macroDefinition = define "MacroDefinition" $
  doc ("A macro definition."
    ++ " Serializes as (defmacro name [params] body) in Clojure,"
    ++ " (defmacro name (params) body) in Emacs Lisp and Common Lisp,"
    ++ " (define-syntax name ...) in Scheme") $
  T.record [
    "name">:
      doc "The macro name" $
      lisp "Symbol",
    "params">:
      doc "The parameter list" $
      T.list (lisp "Symbol"),
    "restParam">:
      doc "Optional rest parameter" $
      T.maybe (lisp "Symbol"),
    "body">:
      doc "The macro body" $
      T.list (lisp "Expression")]


-- ================================================================================================
-- Expressions
-- ================================================================================================

expression :: Binding
expression = define "Expression" $
  doc "A Lisp expression" $
  T.union [
    "application">:
      doc "Function application: (f arg1 arg2 ...)" $
      lisp "Application",
    "lambda">:
      doc "Anonymous function" $
      lisp "Lambda",
    "let">:
      doc "Local variable binding" $
      lisp "LetExpression",
    "if">:
      doc "Conditional expression" $
      lisp "IfExpression",
    "cond">:
      doc "Multi-branch conditional" $
      lisp "CondExpression",
    "case">:
      doc "Case/match dispatch" $
      lisp "CaseExpression",
    "and">:
      doc "Logical and (short-circuiting)" $
      lisp "AndExpression",
    "or">:
      doc "Logical or (short-circuiting)" $
      lisp "OrExpression",
    "not">:
      doc "Logical negation" $
      lisp "NotExpression",
    "do">:
      doc "Sequential evaluation (progn/do/begin)" $
      lisp "DoExpression",
    "begin">:
      doc "Sequential evaluation (explicit begin block)" $
      lisp "BeginExpression",
    "variable">:
      doc "Variable reference" $
      lisp "VariableReference",
    "literal">:
      doc "A literal value" $
      lisp "Literal",
    "list">:
      doc "A list literal" $
      lisp "ListLiteral",
    "vector">:
      doc "A vector literal" $
      lisp "VectorLiteral",
    "map">:
      doc "A map/association literal" $
      lisp "MapLiteral",
    "set">:
      doc "A set literal" $
      lisp "SetLiteral",
    "cons">:
      doc "A cons expression" $
      lisp "ConsExpression",
    "dottedPair">:
      doc "A dotted pair literal" $
      lisp "DottedPair",
    "fieldAccess">:
      doc "Field access on a record/struct" $
      lisp "FieldAccess",
    "typeAnnotation">:
      doc "A type-annotated expression" $
      lisp "TypeAnnotation",
    "quote">:
      doc "A quoted expression" $
      lisp "QuoteExpression",
    "quasiquote">:
      doc "A quasiquoted expression" $
      lisp "QuasiquoteExpression",
    "unquote">:
      doc "An unquoted expression within a quasiquote" $
      lisp "UnquoteExpression",
    "splicingUnquote">:
      doc "A splicing unquote within a quasiquote" $
      lisp "SplicingUnquoteExpression",
    "sExpression">:
      doc "An arbitrary S-expression (escape hatch for dialect-specific forms)" $
      lisp "SExpression"]

application :: Binding
application = define "Application" $
  doc "Function application: (function arg1 arg2 ...)" $
  T.record [
    "function">:
      doc "The function being applied" $
      lisp "Expression",
    "arguments">:
      doc "The arguments" $
      T.list (lisp "Expression")]

lambda :: Binding
lambda = define "Lambda" $
  doc ("An anonymous function."
    ++ " Serializes as (fn [params] body) in Clojure,"
    ++ " (lambda (params) body) in Emacs Lisp, Common Lisp, and Scheme."
    ++ " If name is provided, emits (fn name [params] body) in Clojure for self-reference.") $
  T.record [
    "name">:
      doc "Optional name for self-referential lambdas (Clojure named fn)" $
      T.maybe (lisp "Symbol"),
    "params">:
      doc "The parameter list" $
      T.list (lisp "Symbol"),
    "restParam">:
      doc "Optional rest parameter" $
      T.maybe (lisp "Symbol"),
    "body">:
      doc "The lambda body" $
      T.list (lisp "Expression")]

variableReference :: Binding
variableReference = define "VariableReference" $
  doc "A reference to a variable by name" $
  T.record [
    "name">:
      doc "The variable name" $
      lisp "Symbol",
    "functionNamespace">:
      doc ("Whether to reference from the function namespace."
        ++ " In Lisp-2 dialects (Common Lisp), this emits #'name."
        ++ " In Lisp-1 dialects, this has no effect.") $
      T.boolean]

fieldAccess :: Binding
fieldAccess = define "FieldAccess" $
  doc ("Field access on a record/struct."
    ++ " Serializes as (:field record) in Clojure,"
    ++ " (struct-field record) in Emacs Lisp and Common Lisp,"
    ++ " (record-field record) in Scheme") $
  T.record [
    "recordType">:
      doc "The record type name (used to form accessor name)" $
      lisp "Symbol",
    "field">:
      doc "The field name" $
      lisp "Symbol",
    "target">:
      doc "The expression being accessed" $
      lisp "Expression"]

typeAnnotation :: Binding
typeAnnotation = define "TypeAnnotation" $
  doc "An expression with a type annotation" $
  T.record [
    "expression">:
      doc "The annotated expression" $
      lisp "Expression",
    "type">:
      doc "The type specifier" $
      lisp "TypeSpecifier"]


-- ================================================================================================
-- Special forms
-- ================================================================================================

ifExpression :: Binding
ifExpression = define "IfExpression" $
  doc "Conditional: (if test then else)" $
  T.record [
    "condition">:
      doc "The test expression" $
      lisp "Expression",
    "then">:
      doc "The then branch" $
      lisp "Expression",
    "else">:
      doc "Optional else branch" $
      T.maybe (lisp "Expression")]

condExpression :: Binding
condExpression = define "CondExpression" $
  doc ("Multi-branch conditional."
    ++ " Serializes as (cond test1 expr1 test2 expr2 :else default) in Clojure,"
    ++ " (cond (test1 expr1) (test2 expr2) (t default)) in Emacs Lisp and Common Lisp,"
    ++ " (cond (test1 expr1) (test2 expr2) (else default)) in Scheme") $
  T.record [
    "clauses">:
      doc "The condition-expression pairs" $
      T.list (lisp "CondClause"),
    "default">:
      doc "Optional default expression" $
      T.maybe (lisp "Expression")]

condClause :: Binding
condClause = define "CondClause" $
  doc "A clause in a cond expression" $
  T.record [
    "condition">:
      doc "The test condition" $
      lisp "Expression",
    "body">:
      doc "The result expression" $
      lisp "Expression"]

caseExpression :: Binding
caseExpression = define "CaseExpression" $
  doc ("Case dispatch on a value."
    ++ " Serializes as (case x key1 expr1 key2 expr2 default) in Clojure,"
    ++ " (case x (key1 expr1) (key2 expr2) (otherwise default)) in Common Lisp,"
    ++ " (case x ((key1) expr1) ((key2) expr2) (else default)) in Scheme") $
  T.record [
    "scrutinee">:
      doc "The expression being dispatched on" $
      lisp "Expression",
    "clauses">:
      doc "The case clauses" $
      T.list (lisp "CaseClause"),
    "default">:
      doc "Optional default clause" $
      T.maybe (lisp "Expression")]

caseClause :: Binding
caseClause = define "CaseClause" $
  doc "A clause in a case expression" $
  T.record [
    "keys">:
      doc "The matching keys (one or more datum values)" $
      T.list (lisp "Expression"),
    "body">:
      doc "The result expression" $
      lisp "Expression"]

andExpression :: Binding
andExpression = define "AndExpression" $
  doc "Logical and: (and expr1 expr2 ...)" $
  T.record [
    "expressions">:
      doc "The operand expressions" $
      T.list (lisp "Expression")]

orExpression :: Binding
orExpression = define "OrExpression" $
  doc "Logical or: (or expr1 expr2 ...)" $
  T.record [
    "expressions">:
      doc "The operand expressions" $
      T.list (lisp "Expression")]

notExpression :: Binding
notExpression = define "NotExpression" $
  doc "Logical negation: (not expr)" $
  T.record [
    "expression">:
      doc "The operand expression" $
      lisp "Expression"]

doExpression :: Binding
doExpression = define "DoExpression" $
  doc ("Sequential evaluation of expressions, returning the last."
    ++ " Serializes as (do expr1 expr2 ...) in Clojure,"
    ++ " (progn expr1 expr2 ...) in Emacs Lisp and Common Lisp,"
    ++ " (begin expr1 expr2 ...) in Scheme") $
  T.record [
    "expressions">:
      doc "The expressions to evaluate in sequence" $
      T.list (lisp "Expression")]

beginExpression :: Binding
beginExpression = define "BeginExpression" $
  doc "An explicit begin block (distinct from do for Scheme compatibility)" $
  T.record [
    "expressions">:
      doc "The expressions to evaluate in sequence" $
      T.list (lisp "Expression")]

quoteExpression :: Binding
quoteExpression = define "QuoteExpression" $
  doc "A quoted form: 'expr or (quote expr)" $
  T.record [
    "body">:
      doc "The quoted form" $
      lisp "Expression"]

quasiquoteExpression :: Binding
quasiquoteExpression = define "QuasiquoteExpression" $
  doc "A quasiquoted form: `expr" $
  T.record [
    "body">:
      doc "The quasiquoted form" $
      lisp "Expression"]

unquoteExpression :: Binding
unquoteExpression = define "UnquoteExpression" $
  doc "An unquoted form within a quasiquote: ~expr or ,expr" $
  T.record [
    "body">:
      doc "The unquoted form" $
      lisp "Expression"]

splicingUnquoteExpression :: Binding
splicingUnquoteExpression = define "SplicingUnquoteExpression" $
  doc "A splicing unquote within a quasiquote: ~@expr or ,@expr" $
  T.record [
    "body">:
      doc "The spliced form" $
      lisp "Expression"]


-- ================================================================================================
-- Let bindings
-- ================================================================================================

letExpression :: Binding
letExpression = define "LetExpression" $
  doc ("Local variable bindings."
    ++ " Serializes as (let [x 1 y 2] body) in Clojure (always sequential),"
    ++ " (let ((x 1) (y 2)) body) or (let* ...) in other dialects") $
  T.record [
    "kind">:
      doc "The kind of let (parallel or sequential)" $
      lisp "LetKind",
    "bindings">:
      doc "The variable bindings" $
      T.list (lisp "LetBinding"),
    "body">:
      doc "The body expressions" $
      T.list (lisp "Expression")]

letKind :: Binding
letKind = define "LetKind" $
  doc "The kind of let binding" $
  T.enum ["parallel", "sequential", "recursive"]

letBinding :: Binding
letBinding = define "LetBinding" $
  doc "A single binding in a let expression" $
  T.union [
    "simple">:
      doc "A simple name-value binding" $
      lisp "SimpleBinding",
    "destructuring">:
      doc "A destructuring binding" $
      lisp "DestructuringBinding"]

simpleBinding :: Binding
simpleBinding = define "SimpleBinding" $
  doc "A simple name-value binding in a let expression" $
  T.record [
    "name">:
      doc "The bound variable" $
      lisp "Symbol",
    "value">:
      doc "The value expression" $
      lisp "Expression"]

destructuringBinding :: Binding
destructuringBinding = define "DestructuringBinding" $
  doc "A destructuring binding in a let expression" $
  T.record [
    "pattern">:
      doc "The destructuring pattern" $
      lisp "DestructuringPattern",
    "value">:
      doc "The value to destructure" $
      lisp "Expression"]

destructuringPattern :: Binding
destructuringPattern = define "DestructuringPattern" $
  doc "A destructuring pattern" $
  T.union [
    "sequential">:
      doc "Sequential destructuring: [a b c] in Clojure, (a b c) in others" $
      T.list (lisp "Symbol"),
    "associative">:
      doc "Associative/map destructuring: {:keys [a b]} in Clojure" $
      T.list (lisp "Symbol"),
    "rest">:
      doc "Destructuring with a rest element: [a b & rest] (leading symbols + rest symbol)" $
      T.list (lisp "Symbol")]


-- ================================================================================================
-- Patterns (for case/match)
-- ================================================================================================

pattern :: Binding
pattern = define "Pattern" $
  doc "A pattern for use in case expressions or match forms" $
  T.union [
    "constructor">:
      doc "A constructor pattern (for union/sum type matching)" $
      lisp "ConstructorPattern",
    "literal">:
      doc "A literal pattern" $
      lisp "LiteralPattern",
    "wildcard">:
      doc "A wildcard pattern matching anything" $
      lisp "WildcardPattern",
    "variable">:
      doc "A variable pattern that binds the matched value" $
      lisp "Symbol"]

constructorPattern :: Binding
constructorPattern = define "ConstructorPattern" $
  doc "A constructor pattern matching a tagged value" $
  T.record [
    "constructor">:
      doc "The constructor/tag name" $
      lisp "Symbol",
    "arguments">:
      doc "The sub-patterns for constructor arguments" $
      T.list (lisp "Pattern")]

literalPattern :: Binding
literalPattern = define "LiteralPattern" $
  doc "A pattern matching a literal value" $
  T.record [
    "value">:
      doc "The literal to match" $
      lisp "Literal"]

wildcardPattern :: Binding
wildcardPattern = define "WildcardPattern" $
  doc "A wildcard pattern that matches any value" $
  T.record []


-- ================================================================================================
-- Literals
-- ================================================================================================

literal :: Binding
literal = define "Literal" $
  doc "A Lisp literal value" $
  T.union [
    "integer">:
      doc "An integer literal" $
      lisp "IntegerLiteral",
    "float">:
      doc "A floating-point literal" $
      lisp "FloatLiteral",
    "string">:
      doc "A string literal" $
      T.string,
    "character">:
      doc "A character literal" $
      lisp "CharacterLiteral",
    "boolean">:
      doc "A boolean literal (dialect-specific rendering)" $
      T.boolean,
    "nil">:
      doc "Nil/null/empty list (dialect-specific rendering)" $
      T.unit,
    "keyword">:
      doc "A keyword literal" $
      lisp "Keyword",
    "symbol">:
      doc "A quoted symbol literal" $
      lisp "Symbol"]

integerLiteral :: Binding
integerLiteral = define "IntegerLiteral" $
  doc "An integer literal" $
  T.record [
    "value">:
      doc "The integer value" $
      T.bigint,
    "bigint">:
      doc "Whether this is explicitly a big integer (e.g. 42N in Clojure)" $
      T.boolean]

floatLiteral :: Binding
floatLiteral = define "FloatLiteral" $
  doc "A floating-point literal" $
  T.record [
    "value">:
      doc "The float value" $
      T.bigfloat,
    "precision">:
      doc "Optional precision hint (e.g. 3.14d0 vs 3.14f0 in Common Lisp)" $
      T.maybe T.string]

characterLiteral :: Binding
characterLiteral = define "CharacterLiteral" $
  doc ("A character literal."
    ++ " Concrete syntax varies: \\a (Clojure), ?a (Emacs Lisp), #\\a (Common Lisp, Scheme)") $
  T.record [
    "value">:
      doc "The character value" $
      T.string]

booleanStyle :: Binding
booleanStyle = define "BooleanStyle" $
  doc "The style of boolean literals in a dialect" $
  T.enum ["trueFalse", "tNil", "hashTF"]

nilStyle :: Binding
nilStyle = define "NilStyle" $
  doc "The style of nil/null in a dialect" $
  T.enum ["nil", "emptyList"]


-- ================================================================================================
-- Names and symbols
-- ================================================================================================

symbol :: Binding
symbol = define "Symbol" $
  doc "A Lisp symbol (identifier)" $
  T.wrap T.string

keyword :: Binding
keyword = define "Keyword" $
  doc "A keyword (self-evaluating symbol). Serializes as :name in Clojure, Emacs Lisp, and Common Lisp" $
  T.record [
    "name">:
      doc "The keyword name (without the leading colon)" $
      T.string,
    "namespace">:
      doc "Optional namespace (e.g. my.ns/foo in Clojure)" $
      T.maybe T.string]

qualifiedSymbol :: Binding
qualifiedSymbol = define "QualifiedSymbol" $
  doc ("A namespace-qualified symbol."
    ++ " Serializes as ns/name in Clojure, pkg:name or pkg::name in Common Lisp") $
  T.record [
    "namespace">:
      doc "The namespace or package" $
      T.string,
    "name">:
      doc "The local name" $
      T.string]

namespaceName :: Binding
namespaceName = define "NamespaceName" $
  doc "A namespace or package name" $
  T.wrap T.string


-- ================================================================================================
-- Collection literals
-- ================================================================================================

listLiteral :: Binding
listLiteral = define "ListLiteral" $
  doc "A list literal: '(1 2 3) or (list 1 2 3)" $
  T.record [
    "elements">:
      doc "The list elements" $
      T.list (lisp "Expression"),
    "quoted">:
      doc "Whether to use quote syntax vs constructor syntax" $
      T.boolean]

vectorLiteral :: Binding
vectorLiteral = define "VectorLiteral" $
  doc ("A vector literal."
    ++ " Serializes as [1 2 3] in Clojure and Emacs Lisp,"
    ++ " #(1 2 3) in Common Lisp and Scheme") $
  T.record [
    "elements">:
      doc "The vector elements" $
      T.list (lisp "Expression")]

mapLiteral :: Binding
mapLiteral = define "MapLiteral" $
  doc ("A map/dictionary literal."
    ++ " Serializes as {:a 1 :b 2} in Clojure,"
    ++ " as an alist '((a . 1) (b . 2)) in other dialects") $
  T.record [
    "entries">:
      doc "The key-value pairs" $
      T.list (lisp "MapEntry")]

mapEntry :: Binding
mapEntry = define "MapEntry" $
  doc "A key-value pair in a map literal" $
  T.record [
    "key">:
      doc "The key expression" $
      lisp "Expression",
    "value">:
      doc "The value expression" $
      lisp "Expression"]

setLiteral :: Binding
setLiteral = define "SetLiteral" $
  doc ("A set literal."
    ++ " Serializes as #{1 2 3} in Clojure."
    ++ " Other dialects use a list-based construction.") $
  T.record [
    "elements">:
      doc "The set elements" $
      T.list (lisp "Expression")]

consExpression :: Binding
consExpression = define "ConsExpression" $
  doc "A cons expression: (cons head tail)" $
  T.record [
    "head">:
      doc "The head element" $
      lisp "Expression",
    "tail">:
      doc "The tail (typically a list or another cons)" $
      lisp "Expression"]

dottedPair :: Binding
dottedPair = define "DottedPair" $
  doc "A dotted pair literal: '(a . b). Not available in Clojure." $
  T.record [
    "car">:
      doc "The first element" $
      lisp "Expression",
    "cdr">:
      doc "The second element" $
      lisp "Expression"]


-- ================================================================================================
-- Type-related
-- ================================================================================================

typeHint :: Binding
typeHint = define "TypeHint" $
  doc ("A type hint or annotation."
    ++ " In Clojure: ^Type name. In Common Lisp: (declare (type Type name))."
    ++ " In Scheme and Emacs Lisp: typically unused.") $
  T.record [
    "name">:
      doc "The annotated symbol" $
      lisp "Symbol",
    "type">:
      doc "The type specifier" $
      lisp "TypeSpecifier"]

typeSpecifier :: Binding
typeSpecifier = define "TypeSpecifier" $
  doc "A type specifier" $
  T.union [
    "named">:
      doc "A named type reference" $
      lisp "Symbol",
    "list">:
      doc "A list type" $
      lisp "TypeSpecifier",
    "function">:
      doc "A function type (params and return)" $
      T.list (lisp "TypeSpecifier"),
    "maybe">:
      doc "An optional type" $
      lisp "TypeSpecifier",
    "map">:
      doc "A map type (key and value type specifiers)" $
      T.list (lisp "TypeSpecifier"),
    "set">:
      doc "A set type" $
      lisp "TypeSpecifier",
    "pair">:
      doc "A pair/tuple type (two type specifiers)" $
      T.list (lisp "TypeSpecifier"),
    "either">:
      doc "An either/union type (two type specifiers)" $
      T.list (lisp "TypeSpecifier"),
    "unit">:
      doc "The unit type" $
      T.unit]


-- ================================================================================================
-- Module system
-- ================================================================================================

moduleDeclaration :: Binding
moduleDeclaration = define "ModuleDeclaration" $
  doc ("A module/namespace declaration."
    ++ " Serializes as (ns name ...) in Clojure,"
    ++ " (provide 'name) in Emacs Lisp,"
    ++ " (defpackage :name ... ) (in-package :name) in Common Lisp,"
    ++ " (define-library (name) ...) in Scheme") $
  T.record [
    "name">:
      doc "The module/namespace name" $
      lisp "NamespaceName",
    "doc">:
      doc "Optional module documentation" $
      T.maybe (lisp "Docstring")]

importDeclaration :: Binding
importDeclaration = define "ImportDeclaration" $
  doc ("An import/require declaration."
    ++ " Serializes as (:require [name ...]) in Clojure,"
    ++ " (require 'name) in Emacs Lisp,"
    ++ " (:use :name) or (:import-from :name ...) in Common Lisp,"
    ++ " (import (name)) in Scheme") $
  T.record [
    "module">:
      doc "The module being imported" $
      lisp "NamespaceName",
    "spec">:
      doc "Import specification" $
      lisp "ImportSpec"]

importSpec :: Binding
importSpec = define "ImportSpec" $
  doc "An import specification describing how to import symbols" $
  T.union [
    "all">:
      doc "Import everything" $
      T.unit,
    "alias">:
      doc "Import with an alias: (:require [name :as alias]) in Clojure" $
      lisp "Symbol",
    "only">:
      doc "Import specific symbols: (:require [name :refer [sym1 sym2]]) in Clojure" $
      T.list (lisp "Symbol"),
    "rename">:
      doc "Import with renaming: list of (from, to) symbol pairs" $
      T.list (T.list (lisp "Symbol"))]

exportDeclaration :: Binding
exportDeclaration = define "ExportDeclaration" $
  doc ("An export/provide declaration."
    ++ " Serializes as (provide 'name) in Emacs Lisp,"
    ++ " (:export :sym1 :sym2) in Common Lisp,"
    ++ " (export sym1 sym2) in Scheme."
    ++ " In Clojure, symbols are public by default.") $
  T.record [
    "symbols">:
      doc "The symbols to export" $
      T.list (lisp "Symbol")]


-- ================================================================================================
-- Miscellaneous
-- ================================================================================================

comment :: Binding
comment = define "Comment" $
  doc "A comment" $
  T.record [
    "style">:
      doc "The comment style" $
      lisp "CommentStyle",
    "text">:
      doc "The comment text" $
      T.string]

commentStyle :: Binding
commentStyle = define "CommentStyle" $
  doc "The style of a comment" $
  T.enum ["line", "block", "datum"]

docstring :: Binding
docstring = define "Docstring" $
  doc "A documentation string" $
  T.wrap T.string

dialect :: Binding
dialect = define "Dialect" $
  doc "A Lisp dialect" $
  T.enum ["clojure", "emacsLisp", "commonLisp", "scheme"]

sExpression :: Binding
sExpression = define "SExpression" $
  doc ("A raw S-expression. This is an escape hatch for expressing arbitrary Lisp forms"
    ++ " that do not fit into the structured AST above.") $
  T.union [
    "atom">:
      doc "An atomic value" $
      T.string,
    "list">:
      doc "A list of S-expressions" $
      T.list (lisp "SExpression")]
