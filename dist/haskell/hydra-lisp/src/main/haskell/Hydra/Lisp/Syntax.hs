-- Note: this is an automatically generated file. Do not edit.
-- | A unified Lisp syntax model covering Clojure, Emacs Lisp, Common Lisp, and Scheme (R7RS). Designed for code generation from Hydra types and terms.

module Hydra.Lisp.Syntax where
import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
-- | A Lisp program, consisting of a sequence of top-level forms
data Program =
  Program {
    -- | The target Lisp dialect
    programDialect :: Dialect,
    -- | Optional module/namespace declaration
    programModule :: (Maybe ModuleDeclaration),
    -- | Import/require declarations
    programImports :: [ImportDeclaration],
    -- | Export/provide declarations
    programExports :: [ExportDeclaration],
    -- | The top-level forms in the program
    programForms :: [TopLevelFormWithComments]}
  deriving (Eq, Ord, Read, Show)
_Program = Core.Name "hydra.lisp.syntax.Program"
_Program_dialect = Core.Name "dialect"
_Program_module = Core.Name "module"
_Program_imports = Core.Name "imports"
_Program_exports = Core.Name "exports"
_Program_forms = Core.Name "forms"
-- | A top-level form in a Lisp program
data TopLevelForm =
  -- | A named function definition
  TopLevelFormFunction FunctionDefinition |
  -- | A global variable definition
  TopLevelFormVariable VariableDefinition |
  -- | A constant definition
  TopLevelFormConstant ConstantDefinition |
  -- | A record/struct type definition
  TopLevelFormRecordType RecordTypeDefinition |
  -- | A macro definition
  TopLevelFormMacro MacroDefinition |
  -- | A bare expression at the top level
  TopLevelFormExpression Expression
  deriving (Eq, Ord, Read, Show)
_TopLevelForm = Core.Name "hydra.lisp.syntax.TopLevelForm"
_TopLevelForm_function = Core.Name "function"
_TopLevelForm_variable = Core.Name "variable"
_TopLevelForm_constant = Core.Name "constant"
_TopLevelForm_recordType = Core.Name "recordType"
_TopLevelForm_macro = Core.Name "macro"
_TopLevelForm_expression = Core.Name "expression"
-- | A top-level form together with optional documentation
data TopLevelFormWithComments =
  TopLevelFormWithComments {
    -- | Optional documentation string
    topLevelFormWithCommentsDoc :: (Maybe Docstring),
    -- | Optional comment
    topLevelFormWithCommentsComment :: (Maybe Comment),
    -- | The form itself
    topLevelFormWithCommentsForm :: TopLevelForm}
  deriving (Eq, Ord, Read, Show)
_TopLevelFormWithComments = Core.Name "hydra.lisp.syntax.TopLevelFormWithComments"
_TopLevelFormWithComments_doc = Core.Name "doc"
_TopLevelFormWithComments_comment = Core.Name "comment"
_TopLevelFormWithComments_form = Core.Name "form"
-- | A named function definition. Serializes as (defn name [params] body) in Clojure, (defun name (params) body) in Emacs Lisp and Common Lisp, (define (name params) body) in Scheme
data FunctionDefinition =
  FunctionDefinition {
    -- | The function name
    functionDefinitionName :: Symbol,
    -- | The parameter list
    functionDefinitionParams :: [Symbol],
    -- | Optional rest/variadic parameter
    functionDefinitionRestParam :: (Maybe Symbol),
    -- | Optional docstring
    functionDefinitionDoc :: (Maybe Docstring),
    -- | Optional type hints for parameters and return type
    functionDefinitionTypeHints :: [TypeHint],
    -- | The function body (one or more expressions)
    functionDefinitionBody :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_FunctionDefinition = Core.Name "hydra.lisp.syntax.FunctionDefinition"
_FunctionDefinition_name = Core.Name "name"
_FunctionDefinition_params = Core.Name "params"
_FunctionDefinition_restParam = Core.Name "restParam"
_FunctionDefinition_doc = Core.Name "doc"
_FunctionDefinition_typeHints = Core.Name "typeHints"
_FunctionDefinition_body = Core.Name "body"
-- | A global variable definition. Serializes as (def name value) in Clojure, (defvar name value) in Emacs Lisp and Common Lisp, (define name value) in Scheme
data VariableDefinition =
  VariableDefinition {
    -- | The variable name
    variableDefinitionName :: Symbol,
    -- | The initial value
    variableDefinitionValue :: Expression,
    -- | Optional docstring
    variableDefinitionDoc :: (Maybe Docstring)}
  deriving (Eq, Ord, Read, Show)
_VariableDefinition = Core.Name "hydra.lisp.syntax.VariableDefinition"
_VariableDefinition_name = Core.Name "name"
_VariableDefinition_value = Core.Name "value"
_VariableDefinition_doc = Core.Name "doc"
-- | A constant definition. Serializes as (def ^:const name value) in Clojure, (defconst name value) in Emacs Lisp, (defconstant name value) in Common Lisp. Scheme has no dedicated constant form; uses define.
data ConstantDefinition =
  ConstantDefinition {
    -- | The constant name
    constantDefinitionName :: Symbol,
    -- | The constant value
    constantDefinitionValue :: Expression,
    -- | Optional docstring
    constantDefinitionDoc :: (Maybe Docstring)}
  deriving (Eq, Ord, Read, Show)
_ConstantDefinition = Core.Name "hydra.lisp.syntax.ConstantDefinition"
_ConstantDefinition_name = Core.Name "name"
_ConstantDefinition_value = Core.Name "value"
_ConstantDefinition_doc = Core.Name "doc"
-- | A record/struct type definition. Serializes as (defrecord Name [fields]) in Clojure, (cl-defstruct name fields) in Emacs Lisp, (defstruct name fields) in Common Lisp, (define-record-type <Name> ...) in Scheme
data RecordTypeDefinition =
  RecordTypeDefinition {
    -- | The record type name
    recordTypeDefinitionName :: Symbol,
    -- | The field definitions
    recordTypeDefinitionFields :: [FieldDefinition],
    -- | Optional docstring
    recordTypeDefinitionDoc :: (Maybe Docstring)}
  deriving (Eq, Ord, Read, Show)
_RecordTypeDefinition = Core.Name "hydra.lisp.syntax.RecordTypeDefinition"
_RecordTypeDefinition_name = Core.Name "name"
_RecordTypeDefinition_fields = Core.Name "fields"
_RecordTypeDefinition_doc = Core.Name "doc"
-- | A field in a record type definition
data FieldDefinition =
  FieldDefinition {
    -- | The field name
    fieldDefinitionName :: Symbol,
    -- | Optional default value
    fieldDefinitionDefaultValue :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)
_FieldDefinition = Core.Name "hydra.lisp.syntax.FieldDefinition"
_FieldDefinition_name = Core.Name "name"
_FieldDefinition_defaultValue = Core.Name "defaultValue"
-- | A macro definition. Serializes as (defmacro name [params] body) in Clojure, (defmacro name (params) body) in Emacs Lisp and Common Lisp, (define-syntax name ...) in Scheme
data MacroDefinition =
  MacroDefinition {
    -- | The macro name
    macroDefinitionName :: Symbol,
    -- | The parameter list
    macroDefinitionParams :: [Symbol],
    -- | Optional rest parameter
    macroDefinitionRestParam :: (Maybe Symbol),
    -- | The macro body
    macroDefinitionBody :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_MacroDefinition = Core.Name "hydra.lisp.syntax.MacroDefinition"
_MacroDefinition_name = Core.Name "name"
_MacroDefinition_params = Core.Name "params"
_MacroDefinition_restParam = Core.Name "restParam"
_MacroDefinition_body = Core.Name "body"
-- | A Lisp expression
data Expression =
  -- | Function application: (f arg1 arg2 ...)
  ExpressionApplication Application |
  -- | Anonymous function
  ExpressionLambda Lambda |
  -- | Local variable binding
  ExpressionLet LetExpression |
  -- | Conditional expression
  ExpressionIf IfExpression |
  -- | Multi-branch conditional
  ExpressionCond CondExpression |
  -- | Case/match dispatch
  ExpressionCase CaseExpression |
  -- | Logical and (short-circuiting)
  ExpressionAnd AndExpression |
  -- | Logical or (short-circuiting)
  ExpressionOr OrExpression |
  -- | Logical negation
  ExpressionNot NotExpression |
  -- | Sequential evaluation (progn/do/begin)
  ExpressionDo DoExpression |
  -- | Sequential evaluation (explicit begin block)
  ExpressionBegin BeginExpression |
  -- | Variable reference
  ExpressionVariable VariableReference |
  -- | A literal value
  ExpressionLiteral Literal |
  -- | A list literal
  ExpressionList ListLiteral |
  -- | A vector literal
  ExpressionVector VectorLiteral |
  -- | A map/association literal
  ExpressionMap MapLiteral |
  -- | A set literal
  ExpressionSet SetLiteral |
  -- | A cons expression
  ExpressionCons ConsExpression |
  -- | A dotted pair literal
  ExpressionDottedPair DottedPair |
  -- | Field access on a record/struct
  ExpressionFieldAccess FieldAccess |
  -- | A type-annotated expression
  ExpressionTypeAnnotation TypeAnnotation |
  -- | A quoted expression
  ExpressionQuote QuoteExpression |
  -- | A quasiquoted expression
  ExpressionQuasiquote QuasiquoteExpression |
  -- | An unquoted expression within a quasiquote
  ExpressionUnquote UnquoteExpression |
  -- | A splicing unquote within a quasiquote
  ExpressionSplicingUnquote SplicingUnquoteExpression |
  -- | An arbitrary S-expression (escape hatch for dialect-specific forms)
  ExpressionSExpression SExpression
  deriving (Eq, Ord, Read, Show)
_Expression = Core.Name "hydra.lisp.syntax.Expression"
_Expression_application = Core.Name "application"
_Expression_lambda = Core.Name "lambda"
_Expression_let = Core.Name "let"
_Expression_if = Core.Name "if"
_Expression_cond = Core.Name "cond"
_Expression_case = Core.Name "case"
_Expression_and = Core.Name "and"
_Expression_or = Core.Name "or"
_Expression_not = Core.Name "not"
_Expression_do = Core.Name "do"
_Expression_begin = Core.Name "begin"
_Expression_variable = Core.Name "variable"
_Expression_literal = Core.Name "literal"
_Expression_list = Core.Name "list"
_Expression_vector = Core.Name "vector"
_Expression_map = Core.Name "map"
_Expression_set = Core.Name "set"
_Expression_cons = Core.Name "cons"
_Expression_dottedPair = Core.Name "dottedPair"
_Expression_fieldAccess = Core.Name "fieldAccess"
_Expression_typeAnnotation = Core.Name "typeAnnotation"
_Expression_quote = Core.Name "quote"
_Expression_quasiquote = Core.Name "quasiquote"
_Expression_unquote = Core.Name "unquote"
_Expression_splicingUnquote = Core.Name "splicingUnquote"
_Expression_sExpression = Core.Name "sExpression"
-- | Function application: (function arg1 arg2 ...)
data Application =
  Application {
    -- | The function being applied
    applicationFunction :: Expression,
    -- | The arguments
    applicationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_Application = Core.Name "hydra.lisp.syntax.Application"
_Application_function = Core.Name "function"
_Application_arguments = Core.Name "arguments"
-- | An anonymous function. Serializes as (fn [params] body) in Clojure, (lambda (params) body) in Emacs Lisp, Common Lisp, and Scheme. If name is provided, emits (fn name [params] body) in Clojure for self-reference.
data Lambda =
  Lambda {
    -- | Optional name for self-referential lambdas (Clojure named fn)
    lambdaName :: (Maybe Symbol),
    -- | The parameter list
    lambdaParams :: [Symbol],
    -- | Optional rest parameter
    lambdaRestParam :: (Maybe Symbol),
    -- | The lambda body
    lambdaBody :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_Lambda = Core.Name "hydra.lisp.syntax.Lambda"
_Lambda_name = Core.Name "name"
_Lambda_params = Core.Name "params"
_Lambda_restParam = Core.Name "restParam"
_Lambda_body = Core.Name "body"
-- | A reference to a variable by name
data VariableReference =
  VariableReference {
    -- | The variable name
    variableReferenceName :: Symbol,
    -- | Whether to reference from the function namespace. In Lisp-2 dialects (Common Lisp), this emits #'name. In Lisp-1 dialects, this has no effect.
    variableReferenceFunctionNamespace :: Bool}
  deriving (Eq, Ord, Read, Show)
_VariableReference = Core.Name "hydra.lisp.syntax.VariableReference"
_VariableReference_name = Core.Name "name"
_VariableReference_functionNamespace = Core.Name "functionNamespace"
-- | Field access on a record/struct. Serializes as (:field record) in Clojure, (struct-field record) in Emacs Lisp and Common Lisp, (record-field record) in Scheme
data FieldAccess =
  FieldAccess {
    -- | The record type name (used to form accessor name)
    fieldAccessRecordType :: Symbol,
    -- | The field name
    fieldAccessField :: Symbol,
    -- | The expression being accessed
    fieldAccessTarget :: Expression}
  deriving (Eq, Ord, Read, Show)
_FieldAccess = Core.Name "hydra.lisp.syntax.FieldAccess"
_FieldAccess_recordType = Core.Name "recordType"
_FieldAccess_field = Core.Name "field"
_FieldAccess_target = Core.Name "target"
-- | An expression with a type annotation
data TypeAnnotation =
  TypeAnnotation {
    -- | The annotated expression
    typeAnnotationExpression :: Expression,
    -- | The type specifier
    typeAnnotationType :: TypeSpecifier}
  deriving (Eq, Ord, Read, Show)
_TypeAnnotation = Core.Name "hydra.lisp.syntax.TypeAnnotation"
_TypeAnnotation_expression = Core.Name "expression"
_TypeAnnotation_type = Core.Name "type"
-- | Conditional: (if test then else)
data IfExpression =
  IfExpression {
    -- | The test expression
    ifExpressionCondition :: Expression,
    -- | The then branch
    ifExpressionThen :: Expression,
    -- | Optional else branch
    ifExpressionElse :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)
_IfExpression = Core.Name "hydra.lisp.syntax.IfExpression"
_IfExpression_condition = Core.Name "condition"
_IfExpression_then = Core.Name "then"
_IfExpression_else = Core.Name "else"
-- | Multi-branch conditional. Serializes as (cond test1 expr1 test2 expr2 :else default) in Clojure, (cond (test1 expr1) (test2 expr2) (t default)) in Emacs Lisp and Common Lisp, (cond (test1 expr1) (test2 expr2) (else default)) in Scheme
data CondExpression =
  CondExpression {
    -- | The condition-expression pairs
    condExpressionClauses :: [CondClause],
    -- | Optional default expression
    condExpressionDefault :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)
_CondExpression = Core.Name "hydra.lisp.syntax.CondExpression"
_CondExpression_clauses = Core.Name "clauses"
_CondExpression_default = Core.Name "default"
-- | A clause in a cond expression
data CondClause =
  CondClause {
    -- | The test condition
    condClauseCondition :: Expression,
    -- | The result expression
    condClauseBody :: Expression}
  deriving (Eq, Ord, Read, Show)
_CondClause = Core.Name "hydra.lisp.syntax.CondClause"
_CondClause_condition = Core.Name "condition"
_CondClause_body = Core.Name "body"
-- | Case dispatch on a value. Serializes as (case x key1 expr1 key2 expr2 default) in Clojure, (case x (key1 expr1) (key2 expr2) (otherwise default)) in Common Lisp, (case x ((key1) expr1) ((key2) expr2) (else default)) in Scheme
data CaseExpression =
  CaseExpression {
    -- | The expression being dispatched on
    caseExpressionScrutinee :: Expression,
    -- | The case clauses
    caseExpressionClauses :: [CaseClause],
    -- | Optional default clause
    caseExpressionDefault :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)
_CaseExpression = Core.Name "hydra.lisp.syntax.CaseExpression"
_CaseExpression_scrutinee = Core.Name "scrutinee"
_CaseExpression_clauses = Core.Name "clauses"
_CaseExpression_default = Core.Name "default"
-- | A clause in a case expression
data CaseClause =
  CaseClause {
    -- | The matching keys (one or more datum values)
    caseClauseKeys :: [Expression],
    -- | The result expression
    caseClauseBody :: Expression}
  deriving (Eq, Ord, Read, Show)
_CaseClause = Core.Name "hydra.lisp.syntax.CaseClause"
_CaseClause_keys = Core.Name "keys"
_CaseClause_body = Core.Name "body"
-- | Logical and: (and expr1 expr2 ...)
data AndExpression =
  AndExpression {
    -- | The operand expressions
    andExpressionExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_AndExpression = Core.Name "hydra.lisp.syntax.AndExpression"
_AndExpression_expressions = Core.Name "expressions"
-- | Logical or: (or expr1 expr2 ...)
data OrExpression =
  OrExpression {
    -- | The operand expressions
    orExpressionExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_OrExpression = Core.Name "hydra.lisp.syntax.OrExpression"
_OrExpression_expressions = Core.Name "expressions"
-- | Logical negation: (not expr)
data NotExpression =
  NotExpression {
    -- | The operand expression
    notExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)
_NotExpression = Core.Name "hydra.lisp.syntax.NotExpression"
_NotExpression_expression = Core.Name "expression"
-- | Sequential evaluation of expressions, returning the last. Serializes as (do expr1 expr2 ...) in Clojure, (progn expr1 expr2 ...) in Emacs Lisp and Common Lisp, (begin expr1 expr2 ...) in Scheme
data DoExpression =
  DoExpression {
    -- | The expressions to evaluate in sequence
    doExpressionExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_DoExpression = Core.Name "hydra.lisp.syntax.DoExpression"
_DoExpression_expressions = Core.Name "expressions"
-- | An explicit begin block (distinct from do for Scheme compatibility)
data BeginExpression =
  BeginExpression {
    -- | The expressions to evaluate in sequence
    beginExpressionExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_BeginExpression = Core.Name "hydra.lisp.syntax.BeginExpression"
_BeginExpression_expressions = Core.Name "expressions"
-- | A quoted form: 'expr or (quote expr)
data QuoteExpression =
  QuoteExpression {
    -- | The quoted form
    quoteExpressionBody :: Expression}
  deriving (Eq, Ord, Read, Show)
_QuoteExpression = Core.Name "hydra.lisp.syntax.QuoteExpression"
_QuoteExpression_body = Core.Name "body"
-- | A quasiquoted form: `expr
data QuasiquoteExpression =
  QuasiquoteExpression {
    -- | The quasiquoted form
    quasiquoteExpressionBody :: Expression}
  deriving (Eq, Ord, Read, Show)
_QuasiquoteExpression = Core.Name "hydra.lisp.syntax.QuasiquoteExpression"
_QuasiquoteExpression_body = Core.Name "body"
-- | An unquoted form within a quasiquote: ~expr or ,expr
data UnquoteExpression =
  UnquoteExpression {
    -- | The unquoted form
    unquoteExpressionBody :: Expression}
  deriving (Eq, Ord, Read, Show)
_UnquoteExpression = Core.Name "hydra.lisp.syntax.UnquoteExpression"
_UnquoteExpression_body = Core.Name "body"
-- | A splicing unquote within a quasiquote: ~@expr or ,@expr
data SplicingUnquoteExpression =
  SplicingUnquoteExpression {
    -- | The spliced form
    splicingUnquoteExpressionBody :: Expression}
  deriving (Eq, Ord, Read, Show)
_SplicingUnquoteExpression = Core.Name "hydra.lisp.syntax.SplicingUnquoteExpression"
_SplicingUnquoteExpression_body = Core.Name "body"
-- | Local variable bindings. Serializes as (let [x 1 y 2] body) in Clojure (always sequential), (let ((x 1) (y 2)) body) or (let* ...) in other dialects
data LetExpression =
  LetExpression {
    -- | The kind of let (parallel or sequential)
    letExpressionKind :: LetKind,
    -- | The variable bindings
    letExpressionBindings :: [LetBinding],
    -- | The body expressions
    letExpressionBody :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_LetExpression = Core.Name "hydra.lisp.syntax.LetExpression"
_LetExpression_kind = Core.Name "kind"
_LetExpression_bindings = Core.Name "bindings"
_LetExpression_body = Core.Name "body"
-- | The kind of let binding
data LetKind =
  LetKindParallel |
  LetKindSequential |
  LetKindRecursive
  deriving (Eq, Ord, Read, Show)
_LetKind = Core.Name "hydra.lisp.syntax.LetKind"
_LetKind_parallel = Core.Name "parallel"
_LetKind_sequential = Core.Name "sequential"
_LetKind_recursive = Core.Name "recursive"
-- | A single binding in a let expression
data LetBinding =
  -- | A simple name-value binding
  LetBindingSimple SimpleBinding |
  -- | A destructuring binding
  LetBindingDestructuring DestructuringBinding
  deriving (Eq, Ord, Read, Show)
_LetBinding = Core.Name "hydra.lisp.syntax.LetBinding"
_LetBinding_simple = Core.Name "simple"
_LetBinding_destructuring = Core.Name "destructuring"
-- | A simple name-value binding in a let expression
data SimpleBinding =
  SimpleBinding {
    -- | The bound variable
    simpleBindingName :: Symbol,
    -- | The value expression
    simpleBindingValue :: Expression}
  deriving (Eq, Ord, Read, Show)
_SimpleBinding = Core.Name "hydra.lisp.syntax.SimpleBinding"
_SimpleBinding_name = Core.Name "name"
_SimpleBinding_value = Core.Name "value"
-- | A destructuring binding in a let expression
data DestructuringBinding =
  DestructuringBinding {
    -- | The destructuring pattern
    destructuringBindingPattern :: DestructuringPattern,
    -- | The value to destructure
    destructuringBindingValue :: Expression}
  deriving (Eq, Ord, Read, Show)
_DestructuringBinding = Core.Name "hydra.lisp.syntax.DestructuringBinding"
_DestructuringBinding_pattern = Core.Name "pattern"
_DestructuringBinding_value = Core.Name "value"
-- | A destructuring pattern
data DestructuringPattern =
  -- | Sequential destructuring: [a b c] in Clojure, (a b c) in others
  DestructuringPatternSequential [Symbol] |
  -- | Associative/map destructuring: {:keys [a b]} in Clojure
  DestructuringPatternAssociative [Symbol] |
  -- | Destructuring with a rest element: [a b & rest] (leading symbols + rest symbol)
  DestructuringPatternRest [Symbol]
  deriving (Eq, Ord, Read, Show)
_DestructuringPattern = Core.Name "hydra.lisp.syntax.DestructuringPattern"
_DestructuringPattern_sequential = Core.Name "sequential"
_DestructuringPattern_associative = Core.Name "associative"
_DestructuringPattern_rest = Core.Name "rest"
-- | A pattern for use in case expressions or match forms
data Pattern =
  -- | A constructor pattern (for union/sum type matching)
  PatternConstructor ConstructorPattern |
  -- | A literal pattern
  PatternLiteral LiteralPattern |
  -- | A wildcard pattern matching anything
  PatternWildcard WildcardPattern |
  -- | A variable pattern that binds the matched value
  PatternVariable Symbol
  deriving (Eq, Ord, Read, Show)
_Pattern = Core.Name "hydra.lisp.syntax.Pattern"
_Pattern_constructor = Core.Name "constructor"
_Pattern_literal = Core.Name "literal"
_Pattern_wildcard = Core.Name "wildcard"
_Pattern_variable = Core.Name "variable"
-- | A constructor pattern matching a tagged value
data ConstructorPattern =
  ConstructorPattern {
    -- | The constructor/tag name
    constructorPatternConstructor :: Symbol,
    -- | The sub-patterns for constructor arguments
    constructorPatternArguments :: [Pattern]}
  deriving (Eq, Ord, Read, Show)
_ConstructorPattern = Core.Name "hydra.lisp.syntax.ConstructorPattern"
_ConstructorPattern_constructor = Core.Name "constructor"
_ConstructorPattern_arguments = Core.Name "arguments"
-- | A pattern matching a literal value
data LiteralPattern =
  LiteralPattern {
    -- | The literal to match
    literalPatternValue :: Literal}
  deriving (Eq, Ord, Read, Show)
_LiteralPattern = Core.Name "hydra.lisp.syntax.LiteralPattern"
_LiteralPattern_value = Core.Name "value"
-- | A wildcard pattern that matches any value
data WildcardPattern =
  WildcardPattern {}
  deriving (Eq, Ord, Read, Show)
_WildcardPattern = Core.Name "hydra.lisp.syntax.WildcardPattern"
-- | A Lisp literal value
data Literal =
  -- | An integer literal
  LiteralInteger IntegerLiteral |
  -- | A floating-point literal
  LiteralFloat FloatLiteral |
  -- | A string literal
  LiteralString String |
  -- | A character literal
  LiteralCharacter CharacterLiteral |
  -- | A boolean literal (dialect-specific rendering)
  LiteralBoolean Bool |
  -- | Nil/null/empty list (dialect-specific rendering)
  LiteralNil |
  -- | A keyword literal
  LiteralKeyword Keyword |
  -- | A quoted symbol literal
  LiteralSymbol Symbol
  deriving (Eq, Ord, Read, Show)
_Literal = Core.Name "hydra.lisp.syntax.Literal"
_Literal_integer = Core.Name "integer"
_Literal_float = Core.Name "float"
_Literal_string = Core.Name "string"
_Literal_character = Core.Name "character"
_Literal_boolean = Core.Name "boolean"
_Literal_nil = Core.Name "nil"
_Literal_keyword = Core.Name "keyword"
_Literal_symbol = Core.Name "symbol"
-- | An integer literal
data IntegerLiteral =
  IntegerLiteral {
    -- | The integer value
    integerLiteralValue :: Integer,
    -- | Whether this is explicitly a big integer (e.g. 42N in Clojure)
    integerLiteralBigint :: Bool}
  deriving (Eq, Ord, Read, Show)
_IntegerLiteral = Core.Name "hydra.lisp.syntax.IntegerLiteral"
_IntegerLiteral_value = Core.Name "value"
_IntegerLiteral_bigint = Core.Name "bigint"
-- | A floating-point literal
data FloatLiteral =
  FloatLiteral {
    -- | The float value
    floatLiteralValue :: Double,
    -- | Optional precision hint (e.g. 3.14d0 vs 3.14f0 in Common Lisp)
    floatLiteralPrecision :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_FloatLiteral = Core.Name "hydra.lisp.syntax.FloatLiteral"
_FloatLiteral_value = Core.Name "value"
_FloatLiteral_precision = Core.Name "precision"
-- | A character literal. Concrete syntax varies: \a (Clojure), ?a (Emacs Lisp), #\a (Common Lisp, Scheme)
data CharacterLiteral =
  CharacterLiteral {
    -- | The character value
    characterLiteralValue :: String}
  deriving (Eq, Ord, Read, Show)
_CharacterLiteral = Core.Name "hydra.lisp.syntax.CharacterLiteral"
_CharacterLiteral_value = Core.Name "value"
-- | The style of boolean literals in a dialect
data BooleanStyle =
  BooleanStyleTrueFalse |
  BooleanStyleTNil |
  BooleanStyleHashTF
  deriving (Eq, Ord, Read, Show)
_BooleanStyle = Core.Name "hydra.lisp.syntax.BooleanStyle"
_BooleanStyle_trueFalse = Core.Name "trueFalse"
_BooleanStyle_tNil = Core.Name "tNil"
_BooleanStyle_hashTF = Core.Name "hashTF"
-- | The style of nil/null in a dialect
data NilStyle =
  NilStyleNil |
  NilStyleEmptyList
  deriving (Eq, Ord, Read, Show)
_NilStyle = Core.Name "hydra.lisp.syntax.NilStyle"
_NilStyle_nil = Core.Name "nil"
_NilStyle_emptyList = Core.Name "emptyList"
-- | A Lisp symbol (identifier)
newtype Symbol =
  Symbol {
    unSymbol :: String}
  deriving (Eq, Ord, Read, Show)
_Symbol = Core.Name "hydra.lisp.syntax.Symbol"
-- | A keyword (self-evaluating symbol). Serializes as :name in Clojure, Emacs Lisp, and Common Lisp
data Keyword =
  Keyword {
    -- | The keyword name (without the leading colon)
    keywordName :: String,
    -- | Optional namespace (e.g. my.ns/foo in Clojure)
    keywordNamespace :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)
_Keyword = Core.Name "hydra.lisp.syntax.Keyword"
_Keyword_name = Core.Name "name"
_Keyword_namespace = Core.Name "namespace"
-- | A namespace-qualified symbol. Serializes as ns/name in Clojure, pkg:name or pkg::name in Common Lisp
data QualifiedSymbol =
  QualifiedSymbol {
    -- | The namespace or package
    qualifiedSymbolNamespace :: String,
    -- | The local name
    qualifiedSymbolName :: String}
  deriving (Eq, Ord, Read, Show)
_QualifiedSymbol = Core.Name "hydra.lisp.syntax.QualifiedSymbol"
_QualifiedSymbol_namespace = Core.Name "namespace"
_QualifiedSymbol_name = Core.Name "name"
-- | A namespace or package name
newtype NamespaceName =
  NamespaceName {
    unNamespaceName :: String}
  deriving (Eq, Ord, Read, Show)
_NamespaceName = Core.Name "hydra.lisp.syntax.NamespaceName"
-- | A list literal: '(1 2 3) or (list 1 2 3)
data ListLiteral =
  ListLiteral {
    -- | The list elements
    listLiteralElements :: [Expression],
    -- | Whether to use quote syntax vs constructor syntax
    listLiteralQuoted :: Bool}
  deriving (Eq, Ord, Read, Show)
_ListLiteral = Core.Name "hydra.lisp.syntax.ListLiteral"
_ListLiteral_elements = Core.Name "elements"
_ListLiteral_quoted = Core.Name "quoted"
-- | A vector literal. Serializes as [1 2 3] in Clojure and Emacs Lisp, #(1 2 3) in Common Lisp and Scheme
data VectorLiteral =
  VectorLiteral {
    -- | The vector elements
    vectorLiteralElements :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_VectorLiteral = Core.Name "hydra.lisp.syntax.VectorLiteral"
_VectorLiteral_elements = Core.Name "elements"
-- | A map/dictionary literal. Serializes as {:a 1 :b 2} in Clojure, as an alist '((a . 1) (b . 2)) in other dialects
data MapLiteral =
  MapLiteral {
    -- | The key-value pairs
    mapLiteralEntries :: [MapEntry]}
  deriving (Eq, Ord, Read, Show)
_MapLiteral = Core.Name "hydra.lisp.syntax.MapLiteral"
_MapLiteral_entries = Core.Name "entries"
-- | A key-value pair in a map literal
data MapEntry =
  MapEntry {
    -- | The key expression
    mapEntryKey :: Expression,
    -- | The value expression
    mapEntryValue :: Expression}
  deriving (Eq, Ord, Read, Show)
_MapEntry = Core.Name "hydra.lisp.syntax.MapEntry"
_MapEntry_key = Core.Name "key"
_MapEntry_value = Core.Name "value"
-- | A set literal. Serializes as #{1 2 3} in Clojure. Other dialects use a list-based construction.
data SetLiteral =
  SetLiteral {
    -- | The set elements
    setLiteralElements :: [Expression]}
  deriving (Eq, Ord, Read, Show)
_SetLiteral = Core.Name "hydra.lisp.syntax.SetLiteral"
_SetLiteral_elements = Core.Name "elements"
-- | A cons expression: (cons head tail)
data ConsExpression =
  ConsExpression {
    -- | The head element
    consExpressionHead :: Expression,
    -- | The tail (typically a list or another cons)
    consExpressionTail :: Expression}
  deriving (Eq, Ord, Read, Show)
_ConsExpression = Core.Name "hydra.lisp.syntax.ConsExpression"
_ConsExpression_head = Core.Name "head"
_ConsExpression_tail = Core.Name "tail"
-- | A dotted pair literal: '(a . b). Not available in Clojure.
data DottedPair =
  DottedPair {
    -- | The first element
    dottedPairCar :: Expression,
    -- | The second element
    dottedPairCdr :: Expression}
  deriving (Eq, Ord, Read, Show)
_DottedPair = Core.Name "hydra.lisp.syntax.DottedPair"
_DottedPair_car = Core.Name "car"
_DottedPair_cdr = Core.Name "cdr"
-- | A type hint or annotation. In Clojure: ^Type name. In Common Lisp: (declare (type Type name)). In Scheme and Emacs Lisp: typically unused.
data TypeHint =
  TypeHint {
    -- | The annotated symbol
    typeHintName :: Symbol,
    -- | The type specifier
    typeHintType :: TypeSpecifier}
  deriving (Eq, Ord, Read, Show)
_TypeHint = Core.Name "hydra.lisp.syntax.TypeHint"
_TypeHint_name = Core.Name "name"
_TypeHint_type = Core.Name "type"
-- | A type specifier
data TypeSpecifier =
  -- | A named type reference
  TypeSpecifierNamed Symbol |
  -- | A list type
  TypeSpecifierList TypeSpecifier |
  -- | A function type (params and return)
  TypeSpecifierFunction [TypeSpecifier] |
  -- | An optional type
  TypeSpecifierMaybe TypeSpecifier |
  -- | A map type (key and value type specifiers)
  TypeSpecifierMap [TypeSpecifier] |
  -- | A set type
  TypeSpecifierSet TypeSpecifier |
  -- | A pair/tuple type (two type specifiers)
  TypeSpecifierPair [TypeSpecifier] |
  -- | An either/union type (two type specifiers)
  TypeSpecifierEither [TypeSpecifier] |
  -- | The unit type
  TypeSpecifierUnit
  deriving (Eq, Ord, Read, Show)
_TypeSpecifier = Core.Name "hydra.lisp.syntax.TypeSpecifier"
_TypeSpecifier_named = Core.Name "named"
_TypeSpecifier_list = Core.Name "list"
_TypeSpecifier_function = Core.Name "function"
_TypeSpecifier_maybe = Core.Name "maybe"
_TypeSpecifier_map = Core.Name "map"
_TypeSpecifier_set = Core.Name "set"
_TypeSpecifier_pair = Core.Name "pair"
_TypeSpecifier_either = Core.Name "either"
_TypeSpecifier_unit = Core.Name "unit"
-- | A module/namespace declaration. Serializes as (ns name ...) in Clojure, (provide 'name) in Emacs Lisp, (defpackage :name ... ) (in-package :name) in Common Lisp, (define-library (name) ...) in Scheme
data ModuleDeclaration =
  ModuleDeclaration {
    -- | The module/namespace name
    moduleDeclarationName :: NamespaceName,
    -- | Optional module documentation
    moduleDeclarationDoc :: (Maybe Docstring)}
  deriving (Eq, Ord, Read, Show)
_ModuleDeclaration = Core.Name "hydra.lisp.syntax.ModuleDeclaration"
_ModuleDeclaration_name = Core.Name "name"
_ModuleDeclaration_doc = Core.Name "doc"
-- | An import/require declaration. Serializes as (:require [name ...]) in Clojure, (require 'name) in Emacs Lisp, (:use :name) or (:import-from :name ...) in Common Lisp, (import (name)) in Scheme
data ImportDeclaration =
  ImportDeclaration {
    -- | The module being imported
    importDeclarationModule :: NamespaceName,
    -- | Import specification
    importDeclarationSpec :: ImportSpec}
  deriving (Eq, Ord, Read, Show)
_ImportDeclaration = Core.Name "hydra.lisp.syntax.ImportDeclaration"
_ImportDeclaration_module = Core.Name "module"
_ImportDeclaration_spec = Core.Name "spec"
-- | An import specification describing how to import symbols
data ImportSpec =
  -- | Import everything
  ImportSpecAll |
  -- | Import with an alias: (:require [name :as alias]) in Clojure
  ImportSpecAlias Symbol |
  -- | Import specific symbols: (:require [name :refer [sym1 sym2]]) in Clojure
  ImportSpecOnly [Symbol] |
  -- | Import with renaming: list of (from, to) symbol pairs
  ImportSpecRename [[Symbol]]
  deriving (Eq, Ord, Read, Show)
_ImportSpec = Core.Name "hydra.lisp.syntax.ImportSpec"
_ImportSpec_all = Core.Name "all"
_ImportSpec_alias = Core.Name "alias"
_ImportSpec_only = Core.Name "only"
_ImportSpec_rename = Core.Name "rename"
-- | An export/provide declaration. Serializes as (provide 'name) in Emacs Lisp, (:export :sym1 :sym2) in Common Lisp, (export sym1 sym2) in Scheme. In Clojure, symbols are public by default.
data ExportDeclaration =
  ExportDeclaration {
    -- | The symbols to export
    exportDeclarationSymbols :: [Symbol]}
  deriving (Eq, Ord, Read, Show)
_ExportDeclaration = Core.Name "hydra.lisp.syntax.ExportDeclaration"
_ExportDeclaration_symbols = Core.Name "symbols"
-- | A comment
data Comment =
  Comment {
    -- | The comment style
    commentStyle :: CommentStyle,
    -- | The comment text
    commentText :: String}
  deriving (Eq, Ord, Read, Show)
_Comment = Core.Name "hydra.lisp.syntax.Comment"
_Comment_style = Core.Name "style"
_Comment_text = Core.Name "text"
-- | The style of a comment
data CommentStyle =
  CommentStyleLine |
  CommentStyleBlock |
  CommentStyleDatum
  deriving (Eq, Ord, Read, Show)
_CommentStyle = Core.Name "hydra.lisp.syntax.CommentStyle"
_CommentStyle_line = Core.Name "line"
_CommentStyle_block = Core.Name "block"
_CommentStyle_datum = Core.Name "datum"
-- | A documentation string
newtype Docstring =
  Docstring {
    unDocstring :: String}
  deriving (Eq, Ord, Read, Show)
_Docstring = Core.Name "hydra.lisp.syntax.Docstring"
-- | A Lisp dialect
data Dialect =
  DialectClojure |
  DialectEmacsLisp |
  DialectCommonLisp |
  DialectScheme
  deriving (Eq, Ord, Read, Show)
_Dialect = Core.Name "hydra.lisp.syntax.Dialect"
_Dialect_clojure = Core.Name "clojure"
_Dialect_emacsLisp = Core.Name "emacsLisp"
_Dialect_commonLisp = Core.Name "commonLisp"
_Dialect_scheme = Core.Name "scheme"
-- | A raw S-expression. This is an escape hatch for expressing arbitrary Lisp forms that do not fit into the structured AST above.
data SExpression =
  -- | An atomic value
  SExpressionAtom String |
  -- | A list of S-expressions
  SExpressionList [SExpression]
  deriving (Eq, Ord, Read, Show)
_SExpression = Core.Name "hydra.lisp.syntax.SExpression"
_SExpression_atom = Core.Name "atom"
_SExpression_list = Core.Name "list"
