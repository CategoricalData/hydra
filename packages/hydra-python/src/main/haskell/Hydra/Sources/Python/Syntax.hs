module Hydra.Sources.Python.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                           ((>:))
import qualified Hydra.Dsl.Types                           as T
import qualified Hydra.Sources.Kernel.Types.Core           as Core


ns :: ModuleName
ns = ModuleName "hydra.python.syntax"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns],
            moduleDescription = Just ("A Python syntax model, tracking the Python 3.14 PEG grammar:\n"
      ++ "  https://docs.python.org/3.14/reference/grammar.html")}
  where
    definitions = constructs ++ terminals ++ nonterminals

    -- These definitions are not based on the grammar, but are convenient for working with Python sources in Hydra.
    constructs = [
      annotatedStatement,
      pythonModule,
      quoteStyle,
      stringPrefix]

    -- Terminals from the PEG grammar (see below)
    terminals = [
      name,
      number,
      string,
      typeComment]

    -- Nonterminal productions from the PEG grammar (inline).
    -- Note: all significant deviations from the grammar are indicated with the word "Hydra" in comments.
    nonterminals = [
      file,
      interactive,
      eval,
      statement,
      simpleStatement,
      compoundStatement,
      assignment,
      typedAssignment,
      untypedAssignment,
      augAssignment,
      annotatedRhs,
      augAssign,
      returnStatement,
      raiseStatement,
      raiseExpression,
      delStatement,
      yieldStatement,
      assertStatement,
      importStatement,
      importName,
      importFrom,
      relativeImportPrefix,
      importFromTargets,
      importFromAsName,
      dottedAsName,
      dottedName,
      block,
      decorators,
      classDefinition,
      functionDefinition,
      functionDefRaw,
      parameters,
      slashNoDefaultParameters,
      slashWithDefaultParameters,
      paramNoDefaultParameters,
      paramWithDefaultParameters,
      slashNoDefault,
      slashWithDefault,
      starEtc,
      noDefaultStarEtc,
      noDefaultStarAnnotationStarEtc,
      commaStarEtc,
      keywords,
      paramNoDefault,
      paramNoDefaultStarAnnotation,
      paramWithDefault,
      paramMaybeDefault,
      param,
      paramStarAnnotation,
      annotation,
      starAnnotation,
      default_,
      ifStatement,
      ifTail,
      whileStatement,
      forStatement,
      withStatement,
      withItem,
      tryStatement,
      tryFinallyStatement,
      tryExceptStatement,
      tryExceptStarStatement,
      exceptBlock,
      exceptExpression,
      exceptStarBlock,
      matchStatement,
      subjectExpression,
      caseBlock,
      guard,
      patterns,
      pattern_,
      asPattern,
      orPattern,
      closedPattern,
      literalExpression,
      complexNumber,
      plusOrMinus,
      signedNumber,
      signedRealNumber,
      realNumber,
      imaginaryNumber,
      capturePattern,
      patternCaptureTarget,
      valuePattern,
      attribute,
      nameOrAttribute,
      groupPattern,
      sequencePattern,
      openSequencePattern,
      maybeSequencePattern,
      maybeStarPattern,
      starPattern,
      mappingPattern,
      itemsPattern,
      keyValuePattern,
      literalExpressionOrAttribute,
      doubleStarPattern,
      classPattern,
      positionalPatterns,
      keywordPatterns,
      keywordPattern,
      typeAlias,
      typeParameter,
      simpleTypeParameter,
      starTypeParameter,
      doubleStarTypeParameter,
      expression,
      conditional,
      yieldExpression,
      starExpression,
      starNamedExpressions,
      starNamedExpression,
      assignmentExpression,
      namedExpression,
      disjunction,
      conjunction,
      inversion,
      comparison,
      compareOpBitwiseOrPair,
      compareOp,
      bitwiseOr,
      bitwiseXor,
      bitwiseAnd,
      shiftExpression,
      shiftLhs,
      shiftOp,
      sum_,
      sumLhs,
      sumOp,
      term,
      termLhs,
      termOp,
      factor,
      power,
      awaitPrimary,
      primary,
      primaryWithRhs,
      primaryRhs,
      slices,
      sliceOrStarredExpression,
      slice,
      sliceExpression,
      atom,
      group,
      lambda_,
      lambdaParameters,
      lambdaSlashNoDefault,
      lambdaSlashWithDefault,
      lambdaStarEtc,
      lambdaKwds,
      lambdaParamNoDefault,
      lambdaParamWithDefault,
      lambdaParamMaybeDefault,
      list,
      tuple,
      set,
      dict,
      doubleStarredKvpair,
      kvpair,
      forIfClauses,
      forIfClause,
      listcomp,
      setcomp,
      genexp,
      genexpHead,
      dictcomp,
      args,
      posArg,
      starredExpression,
      kwargOrStarred,
      kwarg,
      kwargOrDoubleStarred,
      starTargetsListSeq,
      starTargetsTupleSeq,
      starTarget,
      targetWithStarAtom,
      tPrimaryAndName,
      tPrimaryAndSlices,
      starAtom,
      singleTarget,
      singleSubscriptAttributeTarget,
      tPrimary,
      tPrimaryAndGenexp,
      tPrimaryAndArguments,
      delTargets,
      delTarget,
      delTAtom,
      typeExpression,
      funcTypeComment]

def :: String -> Type -> TypeDefinition
def = datatype ns

python :: String -> Type
python = typeref ns

-- These definitions are not based on the grammar, but are convenient for working with Python sources in Hydra.

annotatedStatement :: TypeDefinition
annotatedStatement = def "AnnotatedStatement" $ T.record [ -- Note: added for Hydra-Python
  "comment">: T.string,
  "statement">: python "Statement"]

pythonModule :: TypeDefinition
pythonModule = def "Module" $
  -- Groups of statements are separated by a double newline; see also the "Block" production.
  T.wrap $ T.list $ nonemptyList $ python "Statement"

quoteStyle :: TypeDefinition
quoteStyle = def "QuoteStyle" $ T.enum [
  "single",        -- '...'
  "double",        -- "..."
  "tripleSingle",  -- '''...'''
  "tripleDouble"]  -- """..."""

-- A non-empty prefix on a regular (non-f, non-t) string literal.
-- The empty prefix is encoded as Maybe<StringPrefix> = Nothing on the String.
-- F-strings (f"") and t-strings (t"", 3.14+) have their own AST shapes and are
-- not represented here; they appear as separate arms of the Strings concatenation.
stringPrefix :: TypeDefinition
stringPrefix = def "StringPrefix" $ T.enum [
  "raw",       -- r"" / R""
  "bytes",     -- b"" / B""
  "rawBytes",  -- rb / br / Rb / bR / etc.
  "unicode"]   -- u"" (legacy 3.x compat)

-- Terminals from the PEG grammar (see below)

file :: TypeDefinition
file = def "File" $ T.wrap $ T.list $ python "Statement"

name :: TypeDefinition
name = def "Name" $ T.wrap T.string -- NAME in the grammar

number :: TypeDefinition
number = def "Number" $ T.union [ -- NUMBER in the grammar
  "integer">: T.bigint,
  "float">: T.float64,
  "imaginary">: T.float64]

string :: TypeDefinition
string = def "String" $ T.record [ -- STRING in the grammar (non-f, non-t variants)
  "value">: T.string,
  "prefix">: T.maybe $ python "StringPrefix",
  "quoteStyle">: python "QuoteStyle"]

typeComment :: TypeDefinition
typeComment = def "TypeComment" $ T.wrap T.string -- TYPE_COMMENT in the grammar

-- Nonterminal productions from the PEG grammar (inline).
-- Note: all significant deviations from the grammar are indicated with the word "Hydra" in comments.

-- # General grammatical elements and rules:
-- #
-- # * Strings with double quotes (") denote SOFT KEYWORDS
-- # * Strings with single quotes (') denote KEYWORDS
-- # * Upper case names (NAME) denote tokens in the Grammar/Tokens file
-- # * Rule names starting with "invalid_" are used for specialized syntax errors
-- #     - These rules are NOT used in the first pass of the parser.
-- #     - Only if the first pass fails to parse, a second pass including the invalid
-- #       rules will be executed.
-- #     - If the parser fails in the second phase with a generic syntax error, the
-- #       location of the generic failure of the first pass will be used (this avoids
-- #       reporting incorrect locations due to the invalid rules).
-- #     - The order of the alternatives involving invalid rules matter
-- #       (like any rule in PEG).
-- #
-- # Grammar Syntax (see PEP 617 for more information):
-- #
-- # rule_name: expression
-- #   Optionally, a type can be included right after the rule name, which
-- #   specifies the return type of the C or Python function corresponding to the
-- #   rule:
-- # rule_name[return_type]: expression
-- #   If the return type is omitted, then a void * is returned in C and an Any in
-- #   Python.
-- # e1 e2
-- #   Match e1, then match e2.
-- # e1 | e2
-- #   Match e1 or e2.
-- #   The first alternative can also appear on the line after the rule name for
-- #   formatting purposes. In that case, a | must be used before the first
-- #   alternative, like so:
-- #       rule_name[return_type]:
-- #            | first_alt
-- #            | second_alt
-- # ( e )
-- #   Match e (allows also to use other operators in the group like '(e)*')
-- # [ e ] or e?
-- #   Optionally match e.
-- # e*
-- #   Match zero or more occurrences of e.
-- # e+
-- #   Match one or more occurrences of e.
-- # s.e+
-- #   Match one or more occurrences of e, separated by s. The generated parse tree
-- #   does not include the separator. This is otherwise identical to (e (s e)*).
-- # &e
-- #   Succeed if e can be parsed, without consuming any input.
-- # !e
-- #   Fail if e can be parsed, without consuming any input.
-- # ~
-- #   Commit to the current alternative, even if it fails to parse.
-- # &&e
-- #   Eager parse e. The parser will not backtrack and will immediately
-- #   fail with SyntaxError if e cannot be parsed.
-- #
--
-- # STARTING RULES
-- # ==============
--
-- file: [statements] ENDMARKER

-- interactive: statement_newline

interactive :: TypeDefinition
interactive = def "Interactive" $ T.wrap $ python "Statement"

-- eval: expressions NEWLINE* ENDMARKER

assignment :: TypeDefinition
assignment = def "Assignment" $ T.union [
  "typed">: python "TypedAssignment",
  "untyped">: python "UntypedAssignment",
  "aug">: python "AugAssignment"]

augAssignment :: TypeDefinition
augAssignment = def "AugAssignment" $ T.record [
  "lhs">: python "SingleTarget",
  "augassign">: python "AugAssign",
  "rhs">: python "AnnotatedRhs"]

compoundStatement :: TypeDefinition
compoundStatement = def "CompoundStatement" $ T.union [
  "function">: python "FunctionDefinition",
  "if">: python "IfStatement",
  "classDef">: python "ClassDefinition",
  "with">: python "WithStatement",
  "for">: python "ForStatement",
  "try">: python "TryStatement",
  "while">: python "WhileStatement",
  "match">: python "MatchStatement"]

-- # SIMPLE STATEMENTS
-- # =================
--
-- # NOTE: annotated_rhs may start with 'yield'; yield_expr must start with 'yield'
-- assignment:
--     | NAME ':' expression ['=' annotated_rhs ]
--     | ('(' single_target ')'
--          | single_subscript_attribute_target) ':' expression ['=' annotated_rhs ]
--     | (star_targets '=' )+ (yield_expr | star_expressions) !'=' [TYPE_COMMENT]
--     | single_target augassign ~ (yield_expr | star_expressions)

eval :: TypeDefinition
eval = def "Eval" $ T.wrap $ nonemptyList $ python "Expression"

-- func_type: '(' [type_expressions] ')' '->' expression NEWLINE* ENDMARKER
--
-- Hydra: omitted. func_type is the PEG production for the `compile(..., 'func_type')`
-- mode used by typing tools that parse standalone function-type signatures from
-- comments. Hydra-Python's coder/serde never emits or consumes this form.

-- # GENERAL STATEMENTS
-- # ==================
--
-- statements: statement+
--
-- statement: compound_stmt  | simple_stmts

simpleStatement :: TypeDefinition
simpleStatement = def "SimpleStatement" $ T.union [
  "assignment">: python "Assignment",
  "typeAlias">: python "TypeAlias",
  "starExpressions">: nonemptyList $ python "StarExpression",
  "return">: python "ReturnStatement",
  "import">: python "ImportStatement",
  "raise">: python "RaiseStatement",
  "pass">: T.unit,
  "del">: python "DelStatement",
  "yield">: python "YieldStatement",
  "assert">: python "AssertStatement",
  "break">: T.unit,
  "continue">: T.unit,
  "global">: nonemptyList $ python "Name",
  "nonlocal">: nonemptyList $ python "Name"]

-- compound_stmt:
--     | function_def
--     | if_stmt
--     | class_def
--     | with_stmt
--     | for_stmt
--     | try_stmt
--     | while_stmt
--     | match_stmt

statement :: TypeDefinition
statement = def "Statement" $ T.union [
  "compound">: python "CompoundStatement",
  "simple">: nonemptyList $ python "SimpleStatement",
  "annotated">: python "AnnotatedStatement"] -- Added for Hydra-Python

-- statement_newline:
--     | compound_stmt NEWLINE
--     | simple_stmts
--     | NEWLINE
--     | ENDMARKER
--
-- simple_stmts:
--     | simple_stmt !';' NEWLINE  # Not needed, there for speedup
--     | ';'.simple_stmt+ [';'] NEWLINE
--
-- # NOTE: assignment MUST precede expression, else parsing a simple assignment
-- # will throw a SyntaxError.
-- simple_stmt:
--     | assignment
--     | type_alias
--     | star_expressions
--     | return_stmt
--     | import_stmt
--     | raise_stmt
--     | 'pass'
--     | del_stmt
--     | yield_stmt
--     | assert_stmt
--     | 'break'
--     | 'continue'
--     | global_stmt
--     | nonlocal_stmt

typedAssignment :: TypeDefinition
typedAssignment = def "TypedAssignment" $ T.record [
  "lhs">: python "SingleTarget",
  "type">: python "Expression",
  "rhs">: T.maybe $ python "AnnotatedRhs"]

untypedAssignment :: TypeDefinition
untypedAssignment = def "UntypedAssignment" $ T.record [
  "targets">: nonemptyList $ python "StarTarget",
  "rhs">: python "AnnotatedRhs",
  "typeComment">: T.maybe $ python "TypeComment"]

-- annotated_rhs: yield_expr | star_expressions

annotatedRhs :: TypeDefinition
annotatedRhs = def "AnnotatedRhs" $ T.union [
  "yield">: python "YieldExpression",
  "star">: nonemptyList $ python "StarExpression"]

-- augassign:
--     | '+='
--     | '-='
--     | '*='
--     | '@='
--     | '/='
--     | '%='
--     | '&='
--     | '|='
--     | '^='
--     | '<<='
--     | '>>='
--     | '**='
--     | '//='

augAssign :: TypeDefinition
augAssign = def "AugAssign" $ T.enum [
  "plusEqual",
  "minusEqual",
  "timesEqual",
  "atEqual",
  "slashEqual",
  "percentEqual",
  "ampersandEqual",
  "barEqual",
  "caretEqual",
  "leftShiftEqual",
  "rightShiftEqual",
  "starStarEqual",
  "doubleSlashEqual"]

-- return_stmt:
--     | 'return' [star_expressions]

delStatement :: TypeDefinition
delStatement = def "DelStatement" $ T.wrap $ python "DelTargets"

raiseExpression :: TypeDefinition
raiseExpression = def "RaiseExpression" $ T.record [
  "expression">: python "Expression",
  "from">: T.maybe $ python "Expression"]

-- global_stmt: 'global' ','.NAME+
--
-- nonlocal_stmt: 'nonlocal' ','.NAME+
--
-- del_stmt:
--     | 'del' del_targets &(';' | NEWLINE)

raiseStatement :: TypeDefinition
raiseStatement = def "RaiseStatement" $ T.wrap $ T.maybe $ python "RaiseExpression"

returnStatement :: TypeDefinition
returnStatement = def "ReturnStatement" $ T.wrap $ T.list $ python "StarExpression"

-- raise_stmt:
--     | 'raise' expression ['from' expression ]
--     | 'raise'

-- yield_stmt: yield_expr

yieldStatement :: TypeDefinition
yieldStatement = def "YieldStatement" $ T.wrap $ python "YieldExpression"

-- assert_stmt: 'assert' expression [',' expression ]

assertStatement :: TypeDefinition
assertStatement = def "AssertStatement" $ T.record [
  "expression1">: python "Expression",
  "expression2">: T.maybe $ python "Expression"]

-- import_stmt:
--     | import_name
--     | import_from

block :: TypeDefinition
block = def "Block" $ T.union [
  -- Statements in indented blocks are grouped in Hydra, so these groups can be separated by a double newline.
  "indented">: nonemptyList $ nonemptyList $ python "Statement",
  "simple">: nonemptyList $ python "SimpleStatement"]

dottedAsName :: TypeDefinition
dottedAsName = def "DottedAsName" $ T.record [
  "name">: python "DottedName",
  "as">: T.maybe $ python "Name"]

-- dotted_name:
--     | dotted_name '.' NAME
--     | NAME

dottedName :: TypeDefinition
dottedName = def "DottedName" $ T.wrap $ nonemptyList $ python "Name"

--
-- # COMPOUND STATEMENTS
-- # ===================
--
-- # Common elements
-- # ---------------
--
-- block:
--     | NEWLINE INDENT statements DEDENT
--     | simple_stmts

importFrom :: TypeDefinition
importFrom = def "ImportFrom" $ T.record [
  "prefixes">: T.list $ python "RelativeImportPrefix",
  "dottedName">: T.maybe $ python "DottedName",
  "targets">: python "ImportFromTargets"]

importFromAsName :: TypeDefinition
importFromAsName = def "ImportFromAsName" $ T.record [
  "name">: python "Name",
  "as">: T.maybe $ python "Name"]

-- dotted_as_names:
--     | ','.dotted_as_name+
--
-- dotted_as_name:
--     | dotted_name ['as' NAME ]

importFromTargets :: TypeDefinition
importFromTargets = def "ImportFromTargets" $ T.union [
  "simple">: nonemptyList $ python "ImportFromAsName",
  "parens">: nonemptyList $ python "ImportFromAsName",
  "star">: T.unit]

-- import_from_as_names:
--     | ','.import_from_as_name+
--
-- import_from_as_name:
--     | NAME ['as' NAME ]

importName :: TypeDefinition
importName = def "ImportName" $ T.wrap $ nonemptyList $ python "DottedAsName"

-- # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
-- import_from:
--     | 'from' ('.' | '...')* dotted_name 'import' import_from_targets
--     | 'from' ('.' | '...')+ 'import' import_from_targets

importStatement :: TypeDefinition
importStatement = def "ImportStatement" $ T.union [
  "name">: python "ImportName",
  "from">: python "ImportFrom"]

-- # Import statements
-- # -----------------
--
-- import_name: 'import' dotted_as_names

relativeImportPrefix :: TypeDefinition
relativeImportPrefix = def "RelativeImportPrefix" $ T.enum ["dot", "ellipsis"]

-- import_from_targets:
--     | '(' import_from_as_names [','] ')'
--     | import_from_as_names !','
--     | '*'

-- decorators: ('@' named_expression NEWLINE )+

classDefinition :: TypeDefinition
classDefinition = def "ClassDefinition" $ T.record [
  "decorators">: T.maybe $ python "Decorators",
  "name">: python "Name",
  "typeParams">: T.list $ python "TypeParameter",
  "arguments">: T.maybe $ python "Args",
  "body">: python "Block"]

-- class_def_raw:
--     | 'class' NAME [type_params] ['(' [arguments] ')' ] ':' block
--
-- # Function definitions
-- # --------------------
--
-- function_def:
--     | decorators function_def_raw
--     | function_def_raw

commaStarEtc :: TypeDefinition
commaStarEtc = def "CommaStarEtc" $ T.record [
  "paramMaybeDefault">: nonemptyList $ python "ParamMaybeDefault",
  "keywords">: T.maybe $ python "Keywords"]

-- kwds:
--     | '**' param_no_default

decorators :: TypeDefinition
decorators = def "Decorators" $ T.wrap $ nonemptyList $ python "NamedExpression"

-- # Class definitions
-- # -----------------
--
-- class_def:
--     | decorators class_def_raw
--     | class_def_raw

functionDefRaw :: TypeDefinition
functionDefRaw = def "FunctionDefRaw" $ T.record [
  "async">: T.boolean,
  "name">: python "Name",
  "typeParams">: T.list $ python "TypeParameter",
  "params">: T.maybe $ python "Parameters",
  "returnType">: T.maybe $ python "Expression",
  "funcTypeComment">: T.maybe $ python "FuncTypeComment",
  "block">: python "Block"]

-- # Function parameters
-- # -------------------
--
-- params:
--     | parameters
--
-- parameters:
--     | slash_no_default param_no_default* param_with_default* [star_etc]
--     | slash_with_default param_with_default* [star_etc]
--     | param_no_default+ param_with_default* [star_etc]
--     | param_with_default+ [star_etc]
--     | star_etc

functionDefinition :: TypeDefinition
functionDefinition = def "FunctionDefinition" $ T.record [
  "decorators">: T.maybe $ python "Decorators",
  "raw">: python "FunctionDefRaw"]

-- function_def_raw:
--     | 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
--     | 'async' 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block

keywords :: TypeDefinition
keywords = def "Keywords" $ T.wrap $ python "ParamNoDefault"

-- # One parameter.  This *includes* a following comma and type comment.
-- #
-- # There are three styles:
-- # - No default
-- # - With default
-- # - Maybe with default
-- #
-- # There are two alternative forms of each, to deal with type comments:
-- # - Ends in a comma followed by an optional type comment
-- # - No comma, optional type comment, must be followed by close paren
-- # The latter form is for a final parameter without trailing comma.
-- #
--
-- param_no_default:
--     | param ',' TYPE_COMMENT?
--     | param TYPE_COMMENT? &')'

noDefaultStarAnnotationStarEtc :: TypeDefinition
noDefaultStarAnnotationStarEtc = def "NoDefaultStarAnnotationStarEtc" $ T.record [
  "paramNoDefaultStarAnnotation">: python "ParamNoDefaultStarAnnotation",
  "paramMaybeDefault">: T.list $ python "ParamMaybeDefault",
  "keywords">: T.maybe $ python "Keywords"]

noDefaultStarEtc :: TypeDefinition
noDefaultStarEtc = def "NoDefaultStarEtc" $ T.record [
  "paramNoDefault">: python "ParamNoDefault",
  "paramMaybeDefault">: T.list $ python "ParamMaybeDefault",
  "keywords">: T.maybe $ python "Keywords"]

paramMaybeDefault :: TypeDefinition
paramMaybeDefault = def "ParamMaybeDefault" $ T.record [
  "param">: python "Param",
  "default">: T.maybe $ python "Default",
  "typeComment">: T.maybe $ python "TypeComment"]

paramNoDefault :: TypeDefinition
paramNoDefault = def "ParamNoDefault" $ T.record [
  "param">: python "Param",
  "typeComment">: T.maybe $ python "TypeComment"]

-- param_no_default_star_annotation:
--     | param_star_annotation ',' TYPE_COMMENT?
--     | param_star_annotation TYPE_COMMENT? &')'

paramNoDefaultParameters :: TypeDefinition
paramNoDefaultParameters = def "ParamNoDefaultParameters" $ T.record [
  "paramNoDefault">: nonemptyList $ python "ParamNoDefault",
  "paramWithDefault">: T.list $ python "ParamWithDefault",
  "starEtc">: T.maybe $ python "StarEtc"]

paramNoDefaultStarAnnotation :: TypeDefinition
paramNoDefaultStarAnnotation = def "ParamNoDefaultStarAnnotation" $ T.record [
  "paramStarAnnotation">: python "ParamStarAnnotation",
  "typeComment">: T.maybe $ python "TypeComment"]

-- param_with_default:
--     | param default ',' TYPE_COMMENT?
--     | param default TYPE_COMMENT? &')'

paramWithDefault :: TypeDefinition
paramWithDefault = def "ParamWithDefault" $ T.record [
  "param">: python "Param",
  "default">: python "Default",
  "typeComment">: T.maybe $ python "TypeComment"]

-- param_maybe_default:
--     | param default? ',' TYPE_COMMENT?
--     | param default? TYPE_COMMENT? &')'

paramWithDefaultParameters :: TypeDefinition
paramWithDefaultParameters = def "ParamWithDefaultParameters" $ T.record [
  "paramWithDefault">: nonemptyList $ python "ParamWithDefault",
  "starEtc">: T.maybe $ python "StarEtc"]

-- # Some duplication here because we can't write (',' | &')'),
-- # which is because we don't support empty alternatives (yet).
--
-- slash_no_default:
--     | param_no_default+ '/' ','
--     | param_no_default+ '/' &')'

parameters :: TypeDefinition
parameters = def "Parameters" $ T.union [
  "slashNoDefault">: python "SlashNoDefaultParameters",
  "slashWithDefault">: python "SlashWithDefaultParameters",
  "paramNoDefault">: python "ParamNoDefaultParameters",
  "paramWithDefault">: python "ParamWithDefaultParameters",
  "starEtc">: python "StarEtc"]

slashNoDefault :: TypeDefinition
slashNoDefault = def "SlashNoDefault" $ T.wrap $ nonemptyList $ python "ParamNoDefault"

-- slash_with_default:
--     | param_no_default* param_with_default+ '/' ','
--     | param_no_default* param_with_default+ '/' &')'

slashNoDefaultParameters :: TypeDefinition
slashNoDefaultParameters = def "SlashNoDefaultParameters" $ T.record [
  "slash">: python "SlashNoDefault",
  "paramNoDefault">: T.list $ python "ParamNoDefault",
  "paramWithDefault">: T.list $ python "ParamWithDefault",
  "starEtc">: T.maybe $ python "StarEtc"]

slashWithDefault :: TypeDefinition
slashWithDefault = def "SlashWithDefault" $ T.record [
  "paramNoDefault">: T.list $ python "ParamNoDefault",
  "paramWithDefault">: nonemptyList $ python "ParamWithDefault"]

-- star_etc:
--     | '*' param_no_default param_maybe_default* [kwds]
--     | '*' param_no_default_star_annotation param_maybe_default* [kwds]
--     | '*' ',' param_maybe_default+ [kwds]
--     | kwds

slashWithDefaultParameters :: TypeDefinition
slashWithDefaultParameters = def "SlashWithDefaultParameters" $ T.record [
  "paramNoDefault">: T.list $ python "ParamNoDefault",
  "paramWithDefault">: T.list $ python "ParamWithDefault",
  "starEtc">: T.maybe $ python "StarEtc"]

starEtc :: TypeDefinition
starEtc = def "StarEtc" $ T.union [
  "starNoDefault">: python "NoDefaultStarEtc",
  "starNoDefaultStarAnnotation">: python "NoDefaultStarAnnotationStarEtc",
  "starComma">: python "CommaStarEtc",
  "keywords">: python "Keywords"]

-- param: NAME annotation?

param :: TypeDefinition
param = def "Param" $ T.record [
  "name">: python "Name",
  "annotation">: T.maybe $ python "Annotation"]

-- param_star_annotation: NAME star_annotation

paramStarAnnotation :: TypeDefinition
paramStarAnnotation = def "ParamStarAnnotation" $ T.record [
  "name">: python "Name",
  "annotation">: python "StarAnnotation"]

-- annotation: ':' expression

annotation :: TypeDefinition
annotation = def "Annotation" $ T.wrap $ python "Expression"

-- star_annotation: ':' star_expression

starAnnotation :: TypeDefinition
starAnnotation = def "StarAnnotation" $ T.wrap $ python "StarExpression"

-- default: '=' expression  | invalid_default

caseBlock :: TypeDefinition
caseBlock = def "CaseBlock" $ T.record [
  "patterns">: python "Patterns",
  "guard">: T.maybe $ python "Guard",
  "body">: python "Block"]

default_ :: TypeDefinition
default_ = def "Default" $ T.wrap $ python "Expression"

-- # If statement
-- # ------------
--
-- if_stmt:
--     | 'if' named_expression ':' block elif_stmt
--     | 'if' named_expression ':' block [else_block]

exceptBlock :: TypeDefinition
exceptBlock = def "ExceptBlock" $ T.record [
  "expression">: T.maybe $ python "ExceptExpression",
  "body">: python "Block"]

exceptExpression :: TypeDefinition
exceptExpression = def "ExceptExpression" $ T.record [
  "expression">: python "Expression",
  "as">: T.maybe $ python "Name"]

-- except_star_block:
--     | 'except' '*' expression ['as' NAME ] ':' block

exceptStarBlock :: TypeDefinition
exceptStarBlock = def "ExceptStarBlock" $ T.record [
  "expression">: python "Expression",
  "as">: T.maybe $ python "Name",
  "body">: python "Block"]

-- finally_block:
--     | 'finally' ':' block
--
-- # Match statement
-- # ---------------
--
-- match_stmt:
--     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT

forStatement :: TypeDefinition
forStatement = def "ForStatement" $ T.record [
  "async">: T.boolean,
  "targets">: nonemptyList $ python "StarTarget",
  "expressions">: nonemptyList $ python "StarExpression",
  "typeComment">: T.maybe $ python "TypeComment",
  "body">: python "Block",
  "else">: T.maybe $ python "Block"]

-- # With statement
-- # --------------
--
-- with_stmt:
--     |         'with' '(' ','.with_item+ ','? ')' ':' [TYPE_COMMENT] block
--     |         'with' ','.with_item+ ':' [TYPE_COMMENT] block
--     | 'async' 'with' '(' ','.with_item+ ','? ')' ':' block
--     | 'async' 'with' ','.with_item+ ':' [TYPE_COMMENT] block

ifStatement :: TypeDefinition
ifStatement = def "IfStatement" $ T.record [
  "condition">: python "NamedExpression",
  "body">: python "Block",
  "continuation">: T.maybe $ python "IfTail"]

ifTail :: TypeDefinition
ifTail = def "IfTail" $ T.union [
  "elif">: python "IfStatement",
  "else">: python "Block"]

-- elif_stmt:
--     | 'elif' named_expression ':' block elif_stmt
--     | 'elif' named_expression ':' block [else_block]

-- else_block:
--     | 'else' ':' block
--
-- # While statement
-- # ---------------
--
-- while_stmt:
--     | 'while' named_expression ':' block [else_block]

matchStatement :: TypeDefinition
matchStatement = def "MatchStatement" $ T.record [
  "subject">: python "SubjectExpression",
  "cases">: nonemptyList $ python "CaseBlock"]

-- subject_expr:
--     | star_named_expression ',' star_named_expressions?
--     | named_expression

subjectExpression :: TypeDefinition
subjectExpression = def "SubjectExpression" $ T.union [
  "tuple">: nonemptyList $ python "StarNamedExpression",
  "simple">: python "NamedExpression"]

-- case_block:
--     | "case" patterns guard? ':' block

tryExceptStarStatement :: TypeDefinition
tryExceptStarStatement = def "TryExceptStarStatement" $ T.record [
  "body">: python "Block",
  "excepts">: nonemptyList $ python "ExceptStarBlock",
  "else">: T.maybe $ python "Block",
  "finally">: T.maybe $ python "Block"]

-- # Except statement
-- # ----------------
--
-- except_block:
--     | 'except' expression ['as' NAME ] ':' block
--     | 'except' ':' block

tryExceptStatement :: TypeDefinition
tryExceptStatement = def "TryExceptStatement" $ T.record [
  "body">: python "Block",
  "excepts">: nonemptyList $ python "ExceptBlock",
  "else">: T.maybe $ python "Block",
  "finally">: T.maybe $ python "Block"]

tryFinallyStatement :: TypeDefinition
tryFinallyStatement = def "TryFinallyStatement" $ T.record [
  "body">: python "Block",
  "finally">: python "Block"]

tryStatement :: TypeDefinition
tryStatement = def "TryStatement" $ T.union [
  "finally">: python "TryFinallyStatement",
  "except">: python "TryExceptStatement",
  "exceptStar">: python "TryExceptStarStatement"]

whileStatement :: TypeDefinition
whileStatement = def "WhileStatement" $ T.record [
  "condition">: python "NamedExpression",
  "body">: python "Block",
  "else">: T.maybe $ python "Block"]

-- # For statement
-- # -------------
--
-- for_stmt:
--     | 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
--     | 'async' 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]

withItem :: TypeDefinition
withItem = def "WithItem" $ T.record [
  "expression">: python "Expression",
  "as">: T.maybe $ python "StarTarget"]

-- # Try statement
-- # -------------
--
-- try_stmt:
--     | 'try' ':' block finally_block
--     | 'try' ':' block except_block+ [else_block] [finally_block]
--     | 'try' ':' block except_star_block+ [else_block] [finally_block]

withStatement :: TypeDefinition
withStatement = def "WithStatement" $ T.record [
  "async">: T.boolean,
  "items">: nonemptyList $ python "WithItem",
  "typeComment">: T.maybe $ python "TypeComment",
  "body">: python "Block"]

-- with_item:
--     | expression 'as' star_target &(',' | ')' | ':')
--     | expression

-- guard: 'if' named_expression

asPattern :: TypeDefinition
asPattern = def "AsPattern" $ T.record [
  "pattern">: python "OrPattern",
  "as">: python "PatternCaptureTarget"]

-- or_pattern:
--     | '|'.closed_pattern+

closedPattern :: TypeDefinition
closedPattern = def "ClosedPattern" $ T.union [
  "literal">: python "LiteralExpression",
  "capture">: python "CapturePattern",
  "wildcard">: T.unit,
  "value">: python "ValuePattern",
  "group">: python "GroupPattern",
  "sequence">: python "SequencePattern",
  "mapping">: python "MappingPattern",
  "class">: python "ClassPattern"]

-- # Literal patterns are used for equality and identity constraints
-- literal_pattern:
--     | signed_number !('+' | '-')
--     | complex_number
--     | strings
--     | 'None'
--     | 'True'
--     | 'False'

guard :: TypeDefinition
guard = def "Guard" $ T.wrap $ python "NamedExpression"

-- patterns:
--     | open_sequence_pattern
--     | pattern

orPattern :: TypeDefinition
orPattern = def "OrPattern" $ T.wrap $ nonemptyList $ python "ClosedPattern"

-- closed_pattern:
--     | literal_pattern
--     | capture_pattern
--     | wildcard_pattern
--     | value_pattern
--     | group_pattern
--     | sequence_pattern
--     | mapping_pattern
--     | class_pattern

pattern_ :: TypeDefinition
pattern_ = def "Pattern" $ T.union [
  "as">: python "AsPattern",
  "or">: python "OrPattern"]

-- as_pattern:
--     | or_pattern 'as' pattern_capture_target

patterns :: TypeDefinition
patterns = def "Patterns" $ T.union [
  "sequence">: python "OpenSequencePattern",
  "pattern">: python "Pattern"]

-- pattern:
--     | as_pattern
--     | or_pattern

  -- Note: identical to literal_expr

-- # Literal expressions are used to restrict permitted mapping pattern keys
-- literal_expr:
--     | signed_number !('+' | '-')
--     | complex_number
--     | strings
--     | 'None'
--     | 'True'
--     | 'False'

attribute :: TypeDefinition
attribute = def "Attribute" $ T.wrap $ nonemptyList $ python "Name"
-- Hydra: per the PEG, an `attr` is `name_or_attr '.' NAME` which expands to at least
-- two NAMEs (head + dot-trailer). Hydra's kernel type system has nonemptyList but no
-- list-of-length>=2 predicate; the singleton case is structurally permitted but
-- semantically invalid. Cross-cutting refinement-type item — see plan.

-- name_or_attr:
--     | attr
--     | NAME

capturePattern :: TypeDefinition
capturePattern = def "CapturePattern" $ T.wrap $ python "PatternCaptureTarget"

-- pattern_capture_target:
--     | !"_" NAME !('.' | '(' | '=')

classPattern :: TypeDefinition
classPattern = def "ClassPattern" $ T.record [
  "nameOrAttribute">: python "NameOrAttribute",
  "positionalPatterns">: T.maybe $ python "PositionalPatterns",
  "keywordPatterns">: T.maybe $ python "KeywordPatterns"]

-- positional_patterns:
--     | ','.pattern+

complexNumber :: TypeDefinition
complexNumber = def "ComplexNumber" $ T.record [
  "real">: python "SignedRealNumber",
  "plusOrMinus">: python "PlusOrMinus",
  "imaginary">: python "ImaginaryNumber"]

conditional :: TypeDefinition
conditional = def "Conditional" $ T.record [
  "body">: python "Disjunction",
  "if">: python "Disjunction",
  "else">: python "Expression"]

-- yield_expr:
--     | 'yield' 'from' expression
--     | 'yield' [star_expressions]

doubleStarPattern :: TypeDefinition
doubleStarPattern = def "DoubleStarPattern" $ T.wrap $ python "PatternCaptureTarget"

-- class_pattern:
--     | name_or_attr '(' ')'
--     | name_or_attr '(' positional_patterns ','? ')'
--     | name_or_attr '(' keyword_patterns ','? ')'
--     | name_or_attr '(' positional_patterns ',' keyword_patterns ','? ')'

doubleStarTypeParameter :: TypeDefinition
doubleStarTypeParameter = def "DoubleStarTypeParameter" $ T.record [
  "name">: python "Name",
  "default">: T.maybe $ python "Expression"]

-- type_param_bound: ':' expression
-- type_param_default: '=' expression
-- type_param_starred_default: '=' star_expression
--
-- # EXPRESSIONS
-- # -----------
--
-- expressions:
--     | expression (',' expression )+ [',']
--     | expression ','
--     | expression
--
-- expression:
--     | disjunction 'if' disjunction 'else' expression
--     | disjunction
--     | lambdef

expression :: TypeDefinition
expression = def "Expression" $ T.union [
  "conditional">: python "Conditional",
  "simple">: python "Disjunction",
  "lambda">: python "Lambda"]

groupPattern :: TypeDefinition
groupPattern = def "GroupPattern" $ T.wrap $ python "Pattern"

-- sequence_pattern:
--     | '[' maybe_sequence_pattern? ']'
--     | '(' open_sequence_pattern? ')'

imaginaryNumber :: TypeDefinition
imaginaryNumber = def "ImaginaryNumber" $ T.wrap T.float64

-- capture_pattern:
--     | pattern_capture_target

itemsPattern :: TypeDefinition
itemsPattern = def "ItemsPattern" $ T.wrap $ nonemptyList $ python "KeyValuePattern"

-- key_value_pattern:
--     | (literal_expr | attr) ':' pattern

keyValuePattern :: TypeDefinition
keyValuePattern = def "KeyValuePattern" $ T.record [
  "key">: python "LiteralExpressionOrAttribute",
  "value">: python "Pattern"]

keywordPattern :: TypeDefinition
keywordPattern = def "KeywordPattern" $ T.record [
  "name">: python "Name",
  "pattern">: python "Pattern"]

-- # Type statement
-- # ---------------
--
-- type_alias:
--     | "type" NAME [type_params] '=' expression

keywordPatterns :: TypeDefinition
keywordPatterns = def "KeywordPatterns" $ T.wrap $ nonemptyList $ python "KeywordPattern"

-- keyword_pattern:
--     | NAME '=' pattern

literalExpression :: TypeDefinition
literalExpression = def "LiteralExpression" $ T.union [
  "number">: python "SignedNumber",
  "complex">: python "ComplexNumber",
  "string">: python "String",
  "none">: T.unit,
  "true">: T.unit,
  "false">: T.unit]

-- complex_number:
--     | signed_real_number '+' imaginary_number
--     | signed_real_number '-' imaginary_number

literalExpressionOrAttribute :: TypeDefinition
literalExpressionOrAttribute = def "LiteralExpressionOrAttribute" $ T.union [
  "literal">: python "LiteralExpression",
  "attribute">: python "Attribute"]

-- double_star_pattern:
--     | '**' pattern_capture_target

mappingPattern :: TypeDefinition
mappingPattern = def "MappingPattern" $ T.record [
  "items">: T.maybe $ python "ItemsPattern",
  "doubleStar">: T.maybe $ python "DoubleStarPattern"]

-- items_pattern:
--     | ','.key_value_pattern+

maybeSequencePattern :: TypeDefinition
maybeSequencePattern = def "MaybeSequencePattern" $ T.wrap $ nonemptyList $ python "MaybeStarPattern"

-- maybe_star_pattern:
--     | star_pattern
--     | pattern

maybeStarPattern :: TypeDefinition
maybeStarPattern = def "MaybeStarPattern" $ T.union [
  "star">: python "StarPattern",
  "pattern">: python "Pattern"]

-- star_pattern:
--     | '*' pattern_capture_target
--     | '*' wildcard_pattern

nameOrAttribute :: TypeDefinition
nameOrAttribute = def "NameOrAttribute" $ T.wrap $ nonemptyList $ python "Name"

-- group_pattern:
--     | '(' pattern ')'

openSequencePattern :: TypeDefinition
openSequencePattern = def "OpenSequencePattern" $ T.record [
  "head">: python "MaybeStarPattern",
  "tail">: T.maybe $ python "MaybeSequencePattern"]

-- maybe_sequence_pattern:
--     | ','.maybe_star_pattern+ ','?

patternCaptureTarget :: TypeDefinition
patternCaptureTarget = def "PatternCaptureTarget" $ T.wrap $ python "Name"

-- wildcard_pattern:
--     | "_"
--
-- value_pattern:
--     | attr !('.' | '(' | '=')

plusOrMinus :: TypeDefinition
plusOrMinus = def "PlusOrMinus" $ T.enum ["plus", "minus"]

-- signed_number:
--     | NUMBER
--     | '-' NUMBER

positionalPatterns :: TypeDefinition
positionalPatterns = def "PositionalPatterns" $ T.wrap $ nonemptyList $ python "Pattern"

-- keyword_patterns:
--     | ','.keyword_pattern+

realNumber :: TypeDefinition
realNumber = def "RealNumber" $ T.union [ -- NUMBER token excluding imaginary literals
  "integer">: T.bigint,
  "float">: T.float64]

-- imaginary_number:
--     | NUMBER

sequencePattern :: TypeDefinition
sequencePattern = def "SequencePattern" $ T.union [
  "list">: T.maybe $ python "MaybeSequencePattern",
  "tuple">: T.maybe $ python "OpenSequencePattern"]

-- open_sequence_pattern:
--     | maybe_star_pattern ',' maybe_sequence_pattern?

signedNumber :: TypeDefinition
signedNumber = def "SignedNumber" $ T.union [
  "sign">: python "PlusOrMinus",
  "number">: python "Number"]

-- signed_real_number:
--     | real_number
--     | '-' real_number

signedRealNumber :: TypeDefinition
signedRealNumber = def "SignedRealNumber" $ T.union [
  "sign">: python "PlusOrMinus",
  "number">: python "RealNumber"]

-- real_number:
--     | NUMBER

simpleTypeParameter :: TypeDefinition
simpleTypeParameter = def "SimpleTypeParameter" $ T.record [
  "name">: python "Name",
  "bound">: T.maybe $ python "Expression",
  "default">: T.maybe $ python "Expression"]

starExpression :: TypeDefinition
starExpression = def "StarExpression" $ T.union [
  "star">: python "BitwiseOr",
  "simple">: python "Expression"]

starPattern :: TypeDefinition
starPattern = def "StarPattern" $ T.union [
  "capture">: python "PatternCaptureTarget",
  "wildcard">: T.unit]

-- mapping_pattern:
--     | '{' '}'
--     | '{' double_star_pattern ','? '}'
--     | '{' items_pattern ',' double_star_pattern ','? '}'
--     | '{' items_pattern ','? '}'

starTypeParameter :: TypeDefinition
starTypeParameter = def "StarTypeParameter" $ T.record [
  "name">: python "Name",
  "default">: T.maybe $ python "StarExpression"]

typeAlias :: TypeDefinition
typeAlias = def "TypeAlias" $ T.record [
  "name">: python "Name",
  "typeParams">: T.list $ python "TypeParameter",
  "expression">: python "Expression"]

-- # Type parameter declaration
-- # --------------------------
--
-- type_params:
--     | invalid_type_params
--     | '[' type_param_seq ']'
--
-- type_param_seq: ','.type_param+ [',']
--
-- type_param:
--     | NAME [type_param_bound] [type_param_default]
--     | '*' NAME [type_param_starred_default]
--     | '**' NAME [type_param_default]

typeParameter :: TypeDefinition
typeParameter = def "TypeParameter" $ T.union [
  "simple">: python "SimpleTypeParameter",
  "star">: python "StarTypeParameter",
  "doubleStar">: python "DoubleStarTypeParameter"]

valuePattern :: TypeDefinition
valuePattern = def "ValuePattern" $ T.wrap $ python "Attribute"

-- attr:
--     | name_or_attr '.' NAME

yieldExpression :: TypeDefinition
yieldExpression = def "YieldExpression" $ T.union [
  "from">: python "Expression",
  "simple">: T.list $ python "StarExpression"]

-- star_expressions:
--     | star_expression (',' star_expression )+ [',']
--     | star_expression ','
--     | star_expression
--
-- star_expression:
--     | '*' bitwise_or
--     | expression

-- star_named_expressions: ','.star_named_expression+ [',']

assignmentExpression :: TypeDefinition
assignmentExpression = def "AssignmentExpression" $ T.record [
  "name">: python "Name",
  "expression">: python "Expression"]

-- named_expression:
--     | assignment_expression
--     | expression !':='

atom :: TypeDefinition
atom = def "Atom" $ T.union [
  "name">: python "Name",
  "true">: T.unit,
  "false">: T.unit,
  "none">: T.unit,
  "string">: python "String",
  "number">: python "Number",
  "tuple">: python "Tuple",
  "group">: python "Group",
  "genexp">: python "Genexp",
  "list">: python "List",
  "listcomp">: python "Listcomp",
  "dict">: python "Dict",
  "set">: python "Set",
  "dictcomp">: python "Dictcomp",
  "setcomp">: python "Setcomp",
  "ellipsis">: T.unit]

-- group:
--     | '(' (yield_expr | named_expression) ')'

awaitPrimary :: TypeDefinition
awaitPrimary = def "AwaitPrimary" $ T.record [
  "await">: T.boolean,
  "primary">: python "Primary"]

-- primary:
--     | primary '.' NAME
--     | primary genexp
--     | primary '(' [arguments] ')'
--     | primary '[' slices ']'
--     | atom

bitwiseAnd :: TypeDefinition
bitwiseAnd = def "BitwiseAnd" $ T.record [
  "lhs">: T.maybe $ python "BitwiseAnd",
  "rhs">: python "ShiftExpression"]

-- shift_expr:
--     | shift_expr '<<' sum
--     | shift_expr '>>' sum
--     | sum

bitwiseOr :: TypeDefinition
bitwiseOr = def "BitwiseOr" $ T.record [
  "lhs">: T.maybe $ python "BitwiseOr",
  "rhs">: python "BitwiseXor"]

-- bitwise_xor:
--     | bitwise_xor '^' bitwise_and
--     | bitwise_and

bitwiseXor :: TypeDefinition
bitwiseXor = def "BitwiseXor" $ T.record [
  "lhs">: T.maybe $ python "BitwiseXor",
  "rhs">: python "BitwiseAnd"]

-- bitwise_and:
--     | bitwise_and '&' shift_expr
--     | shift_expr

compareOp :: TypeDefinition
compareOp = def "CompareOp" $ T.enum [
  "eq", "noteq", "lte", "lt", "gte", "gt", "notin", "in", "isnot", "is"]

-- eq_bitwise_or: '==' bitwise_or
-- noteq_bitwise_or:
--     | ('!=' ) bitwise_or
-- lte_bitwise_or: '<=' bitwise_or
-- lt_bitwise_or: '<' bitwise_or
-- gte_bitwise_or: '>=' bitwise_or
-- gt_bitwise_or: '>' bitwise_or
-- notin_bitwise_or: 'not' 'in' bitwise_or
-- in_bitwise_or: 'in' bitwise_or
-- isnot_bitwise_or: 'is' 'not' bitwise_or
-- is_bitwise_or: 'is' bitwise_or
--
-- # Bitwise operators
-- # -----------------
--
-- bitwise_or:
--     | bitwise_or '|' bitwise_xor
--     | bitwise_xor

compareOpBitwiseOrPair :: TypeDefinition
compareOpBitwiseOrPair = def "CompareOpBitwiseOrPair" $ T.record [
  "operator">: python "CompareOp",
  "rhs">: python "BitwiseOr"]

comparison :: TypeDefinition
comparison = def "Comparison" $ T.record [
  "lhs">: python "BitwiseOr",
  "rhs">: T.list $ python "CompareOpBitwiseOrPair"]

-- compare_op_bitwise_or_pair:
--     | eq_bitwise_or
--     | noteq_bitwise_or
--     | lte_bitwise_or
--     | lt_bitwise_or
--     | gte_bitwise_or
--     | gt_bitwise_or
--     | notin_bitwise_or
--     | in_bitwise_or
--     | isnot_bitwise_or
--     | is_bitwise_or

conjunction :: TypeDefinition
conjunction = def "Conjunction" $ T.wrap $ nonemptyList $ python "Inversion"

-- inversion:
--     | 'not' inversion
--     | comparison

disjunction :: TypeDefinition
disjunction = def "Disjunction" $ T.wrap $ nonemptyList $ python "Conjunction"

-- conjunction:
--     | inversion ('and' inversion )+
--     | inversion

factor :: TypeDefinition
factor = def "Factor" $ T.union [
  "positive">: python "Factor",
  "negative">: python "Factor",
  "complement">: python "Factor",
  "simple">: python "Power"]

-- power:
--     | await_primary '**' factor
--     | await_primary

group :: TypeDefinition
group = def "Group" $ T.union [
  "yield">: python "YieldExpression",
  "expression">: python "NamedExpression"]

-- # Lambda functions
-- # ----------------
--
-- lambdef:
--     | 'lambda' [lambda_params] ':' expression

inversion :: TypeDefinition
inversion = def "Inversion" $ T.union [
  "not">: python "Inversion",
  "simple">: python "Comparison"]

-- # Comparison operators
-- # --------------------
--
-- comparison:
--     | bitwise_or compare_op_bitwise_or_pair+
--     | bitwise_or

lambdaKwds :: TypeDefinition
lambdaKwds = def "LambdaKwds" $ T.wrap $ python "LambdaParamNoDefault"

-- lambda_param_no_default:
--     | lambda_param ','
--     | lambda_param &':'

lambdaParamMaybeDefault :: TypeDefinition
lambdaParamMaybeDefault = def "LambdaParamMaybeDefault" $ T.record [
  "param">: python "Name",
  "default">: T.maybe $ python "Default"]

-- lambda_param: NAME
--
-- # LITERALS
-- # ========
--
-- string: STRING — Atom.string and LiteralExpression.string hold a single
-- STRING literal directly (see the String definition above). Python's PEG
-- also supports juxtaposed-string concatenation (`(fstring|string)+`) and
-- t-strings (Python 3.14, PEP 750); those productions are not modelled
-- here because no current code path constructs them. If/when a Python
-- source parser or f-string-emitting coder is added, reintroduce
-- StringOrFstring + Fstring* + Tstring* and a corresponding union or
-- additional arm on Atom / LiteralExpression.

-- list:
--     | '[' [star_named_expressions] ']'

lambdaParamNoDefault :: TypeDefinition
lambdaParamNoDefault = def "LambdaParamNoDefault" $ T.wrap $ python "Name"

-- lambda_param_with_default:
--     | lambda_param default ','
--     | lambda_param default &':'

lambdaParamWithDefault :: TypeDefinition
lambdaParamWithDefault = def "LambdaParamWithDefault" $ T.record [
  "param">: python "Name",
  "default">: T.maybe $ python "Default"]

-- lambda_param_maybe_default:
--     | lambda_param default? ','
--     | lambda_param default? &':'

lambdaParameters :: TypeDefinition
lambdaParameters = def "LambdaParameters" $ T.record [
  "slashNoDefault">: T.maybe $ python "LambdaSlashNoDefault",
  "paramNoDefault">: T.list $ python "LambdaParamNoDefault",
  "paramWithDefault">: T.list $ python "LambdaParamWithDefault",
  "starEtc">: T.maybe $ python "LambdaStarEtc"]

-- lambda_slash_no_default:
--     | lambda_param_no_default+ '/' ','
--     | lambda_param_no_default+ '/' &':'

lambdaSlashNoDefault :: TypeDefinition
lambdaSlashNoDefault = def "LambdaSlashNoDefault" $ T.record [
  "parameters">: T.list $ python "LambdaParamNoDefault"]

-- lambda_slash_with_default:
--     | lambda_param_no_default* lambda_param_with_default+ '/' ','
--     | lambda_param_no_default* lambda_param_with_default+ '/' &':'

lambdaSlashWithDefault :: TypeDefinition
lambdaSlashWithDefault = def "LambdaSlashWithDefault" $ T.record [
  "paramNoDefault">: T.list $ python "LambdaParamNoDefault",
  "paramWithDefault">: nonemptyList $ python "LambdaParamWithDefault"]

-- lambda_star_etc:
--     | '*' lambda_param_no_default lambda_param_maybe_default* [lambda_kwds]
--     | '*' ',' lambda_param_maybe_default+ [lambda_kwds]
--     | lambda_kwds

lambdaStarEtc :: TypeDefinition
lambdaStarEtc = def "LambdaStarEtc" $ T.union [
  "star">: T.boolean,
  "paramNoDefault">: python "LambdaParamNoDefault",
  "paramMaybeDefault">: T.list $ python "LambdaParamMaybeDefault",
  "kwds">: python "LambdaKwds"]

-- lambda_kwds:
--     | '**' lambda_param_no_default

lambda_ :: TypeDefinition
lambda_ = def "Lambda" $ T.record [
  "params">: python "LambdaParameters",
  "body">: python "Expression"]

-- lambda_params:
--     | lambda_parameters
--
-- # lambda_parameters etc. duplicates parameters but without annotations
-- # or type comments, and if there's no comma after a parameter, we expect
-- # a colon, not a close parenthesis.  (For more, see parameters above.)
-- #
-- lambda_parameters:
--     | lambda_slash_no_default lambda_param_no_default* lambda_param_with_default* [lambda_star_etc]
--     | lambda_slash_with_default lambda_param_with_default* [lambda_star_etc]
--     | lambda_param_no_default+ lambda_param_with_default* [lambda_star_etc]
--     | lambda_param_with_default+ [lambda_star_etc]
--     | lambda_star_etc

list :: TypeDefinition
list = def "List" $ T.wrap $ T.list $ python "StarNamedExpression"

-- tuple:
--     | '(' [star_named_expression ',' [star_named_expressions]  ] ')'

namedExpression :: TypeDefinition
namedExpression = def "NamedExpression" $ T.union [
  "assignment">: python "AssignmentExpression",
  "simple">: python "Expression"]

-- disjunction:
--     | conjunction ('or' conjunction )+
--     | conjunction

power :: TypeDefinition
power = def "Power" $ T.record [
  "lhs">: python "AwaitPrimary",
  "rhs">: T.maybe $ python "Factor"]

-- # Primary elements
-- # ----------------
--
-- # Primary elements are things like "obj.something.something", "obj[something]", "obj(something)", "obj" ...
--
-- await_primary:
--     | 'await' primary
--     | primary

primary :: TypeDefinition
primary = def "Primary" $ T.union [
  "simple">: python "Atom",
  "compound">: python "PrimaryWithRhs"]

primaryRhs :: TypeDefinition
primaryRhs = def "PrimaryRhs" $ T.union [
  "project">: python "Name",
  "genexp">: python "Genexp",
  "call">: python "Args",
  "slices">: python "Slices"]

-- slices:
--     | slice !','
--     | ','.(slice | starred_expression)+ [',']

primaryWithRhs :: TypeDefinition
primaryWithRhs = def "PrimaryWithRhs" $ T.record [
  "primary">: python "Primary",
  "rhs">: python "PrimaryRhs"]

shiftExpression :: TypeDefinition
shiftExpression = def "ShiftExpression" $ T.record [
  "lhs">: T.maybe $ python "ShiftLhs",
  "rhs">: python "Sum"]

shiftLhs :: TypeDefinition
shiftLhs = def "ShiftLhs" $ T.record [
  "operand">: python "ShiftExpression",
  "operator">: python "ShiftOp"]

shiftOp :: TypeDefinition
shiftOp = def "ShiftOp" $ T.enum [
  "left", "right"]

-- # Arithmetic operators
-- # --------------------
--
-- sum:
--     | sum '+' term
--     | sum '-' term
--     | term

slice :: TypeDefinition
slice = def "Slice" $ T.union [
  "named">: python "NamedExpression",
  "slice_">: python "SliceExpression"]

sliceExpression :: TypeDefinition
sliceExpression = def "SliceExpression" $ T.record [
  "start">: T.maybe $ python "Expression",
  "stop">: T.maybe $ python "Expression",
  "step">: T.maybe $ python "Expression"]

-- atom:
--     | NAME
--     | 'True'
--     | 'False'
--     | 'None'
--     | strings
--     | NUMBER
--     | (tuple | group | genexp)
--     | (list | listcomp)
--     | (dict | set | dictcomp | setcomp)
--     | '...'

sliceOrStarredExpression :: TypeDefinition
sliceOrStarredExpression = def "SliceOrStarredExpression" $ T.union [
  "slice">: python "Slice",
  "starred">: python "StarredExpression"]

-- slice:
--     | [expression] ':' [expression] [':' [expression] ]
--     | named_expression

slices :: TypeDefinition
slices = def "Slices" $ T.record [
  "head">: python "Slice",
  "tail">: T.list $ python "SliceOrStarredExpression"]

starNamedExpression :: TypeDefinition
starNamedExpression = def "StarNamedExpression" $ T.union [
  "star">: python "BitwiseOr",
  "simple">: python "NamedExpression"]

-- assignment_expression:
--     | NAME ':=' ~ expression

starNamedExpressions :: TypeDefinition
starNamedExpressions = def "StarNamedExpressions" $ T.wrap $ nonemptyList $ python "StarNamedExpression"

-- star_named_expression:
--     | '*' bitwise_or
--     | named_expression

sumLhs :: TypeDefinition
sumLhs = def "SumLhs" $ T.record [
  "operand">: python "Sum",
  "operator">: python "SumOp"]

sumOp :: TypeDefinition
sumOp = def "SumOp" $ T.enum [
  "add", "sub"]

-- term:
--     | term '*' factor
--     | term '/' factor
--     | term '//' factor
--     | term '%' factor
--     | term '@' factor
--     | factor

sum_ :: TypeDefinition
sum_ = def "Sum" $ T.record [
  "lhs">: T.maybe $ python "SumLhs",
  "rhs">: python "Term"]

term :: TypeDefinition
term = def "Term" $ T.record [
  "lhs">: T.maybe $ python "TermLhs",
  "rhs">: python "Factor"]

termLhs :: TypeDefinition
termLhs = def "TermLhs" $ T.record [
  "operand">: python "Term",
  "operator">: python "TermOp"]

termOp :: TypeDefinition
termOp = def "TermOp" $ T.enum [
  "mul", "div", "floordiv", "mod", "matmul"]

-- factor:
--     | '+' factor
--     | '-' factor
--     | '~' factor
--     | power

tuple :: TypeDefinition
tuple = def "Tuple" $ T.wrap $ T.list $ python "StarNamedExpression"

-- set: '{' star_named_expressions '}'

dict :: TypeDefinition
dict = def "Dict" $ T.wrap $ T.list $ python "DoubleStarredKvpair"

-- double_starred_kvpairs: ','.double_starred_kvpair+ [',']
--
-- double_starred_kvpair:
--     | '**' bitwise_or
--     | kvpair

doubleStarredKvpair :: TypeDefinition
doubleStarredKvpair = def "DoubleStarredKvpair" $ T.union [
  "starred">: python "BitwiseOr",
  "pair">: python "Kvpair"]

set :: TypeDefinition
set = def "Set" $ T.wrap $ nonemptyList $ python "StarNamedExpression"

-- # Dicts
-- # -----
--
-- dict:
--     | '{' [double_starred_kvpairs] '}'

-- kvpair: expression ':' expression

args :: TypeDefinition
args = def "Args" $ T.record [
  "positional">: T.list $ python "PosArg",
  "kwargOrStarred">: T.list $ python "KwargOrStarred",
  "kwargOrDoubleStarred">: T.list $ python "KwargOrDoubleStarred"]

delTAtom :: TypeDefinition
delTAtom = def "DelTAtom" $ T.union [
  "name">: python "Name",
  "target">: python "DelTarget",
  "targets">: python "DelTargets"]

-- # TYPING ELEMENTS
-- # ---------------
--
-- # type_expressions allow */** but ignore them
-- type_expressions:
--     | ','.expression+ ',' '*' expression ',' '**' expression
--     | ','.expression+ ',' '*' expression
--     | ','.expression+ ',' '**' expression
--     | '*' expression ',' '**' expression
--     | '*' expression
--     | '**' expression
--     | ','.expression+

delTarget :: TypeDefinition
delTarget = def "DelTarget" $ T.union [
  "primaryAndName">: python "TPrimaryAndName",
  "primaryAndSlices">: python "TPrimaryAndSlices",
  "delTAtom">: python "DelTAtom"]

-- del_t_atom:
--     | NAME
--     | '(' del_target ')'
--     | '(' [del_targets] ')'
--     | '[' [del_targets] ']'

delTargets :: TypeDefinition
delTargets = def "DelTargets" $ T.wrap $ nonemptyList $ python "DelTarget"

-- del_target:
--     | t_primary '.' NAME !t_lookahead
--     | t_primary '[' slices ']' !t_lookahead
--     | del_t_atom

dictcomp :: TypeDefinition
dictcomp = def "Dictcomp" $ T.record [
  "kvpair">: python "Kvpair",
  "forIfClauses">: python "ForIfClauses"]

-- # FUNCTION CALL ARGUMENTS
-- # =======================
--
-- arguments:
--     | args [','] &')'
--
-- args:
--     | ','.(starred_expression | ( assignment_expression | expression !':=') !'=')+ [',' kwargs ]
--     | kwargs

forIfClause :: TypeDefinition
forIfClause = def "ForIfClause" $ T.record [
  "async">: T.boolean,
  "targets">: nonemptyList $ python "StarTarget",
  "in">: python "Disjunction",
  "ifs">: T.list $ python "Disjunction"]

-- listcomp:
--     | '[' named_expression for_if_clauses ']'

forIfClauses :: TypeDefinition
forIfClauses = def "ForIfClauses" $ T.wrap $ nonemptyList $ python "ForIfClause"

-- for_if_clause:
--     | 'async' 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
--     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )*

funcTypeComment :: TypeDefinition
funcTypeComment = def "FuncTypeComment" $ T.wrap $ python "TypeComment"

genexp :: TypeDefinition
genexp = def "Genexp" $ T.record [
  "head">: python "GenexpHead",
  "tail">: python "ForIfClauses"]

genexpHead :: TypeDefinition
genexpHead = def "GenexpHead" $ T.union [
  "assignment">: python "AssignmentExpression",
  "expression">: python "Expression"]

-- dictcomp:
--     | '{' kvpair for_if_clauses '}'

kvpair :: TypeDefinition
kvpair = def "Kvpair" $ T.record [
  "key">: python "Expression",
  "value">: python "Expression"]

-- # Comprehensions & Generators
-- # ---------------------------
--
-- for_if_clauses:
--     | for_if_clause+

kwarg :: TypeDefinition
kwarg = def "Kwarg" $ T.record [
  "name">: python "Name",
  "value">: python "Expression"]

-- kwarg_or_double_starred:
--     | NAME '=' expression
--     | '**' expression

kwargOrDoubleStarred :: TypeDefinition
kwargOrDoubleStarred = def "KwargOrDoubleStarred" $ T.union [
  "kwarg">: python "Kwarg",
  "doubleStarred">: python "Expression"]

-- # ASSIGNMENT TARGETS
-- # ==================
--
-- # Generic targets
-- # ---------------
--
-- # NOTE: star_targets may contain *bitwise_or, targets may not.
-- star_targets:
--     | star_target !','
--     | star_target (',' star_target )* [',']
--
-- star_targets_list_seq: ','.star_target+ [',']

kwargOrStarred :: TypeDefinition
kwargOrStarred = def "KwargOrStarred" $ T.union [
  "kwarg">: python "Kwarg",
  "starred">: python "StarredExpression"]

listcomp :: TypeDefinition
listcomp = def "Listcomp" $ T.record [
  "expression">: python "NamedExpression",
  "forIfClauses">: python "ForIfClauses"]

-- setcomp:
--     | '{' named_expression for_if_clauses '}'

posArg :: TypeDefinition
posArg = def "PosArg" $ T.union [
  "starred">: python "StarredExpression",
  "assignment">: python "AssignmentExpression",
  "expression">: python "Expression"]

-- kwargs:
--     | ','.kwarg_or_starred+ ',' ','.kwarg_or_double_starred+
--     | ','.kwarg_or_starred+
--     | ','.kwarg_or_double_starred+
--
-- starred_expression:
--     | '*' expression

setcomp :: TypeDefinition
setcomp = def "Setcomp" $ T.record [
  "expression">: python "NamedExpression",
  "forIfClauses">: python "ForIfClauses"]

-- genexp:
--     | '(' ( assignment_expression | expression !':=') for_if_clauses ')'

singleSubscriptAttributeTarget :: TypeDefinition
singleSubscriptAttributeTarget = def "SingleSubscriptAttributeTarget" $ T.union [
  "primaryAndName">: python "TPrimaryAndName",
  "primaryAndSlices">: python "TPrimaryAndSlices"]

-- t_primary:
--     | t_primary '.' NAME &t_lookahead
--     | t_primary '[' slices ']' &t_lookahead
--     | t_primary genexp &t_lookahead
--     | t_primary '(' [arguments] ')' &t_lookahead
--     | atom &t_lookahead

singleTarget :: TypeDefinition
singleTarget = def "SingleTarget" $ T.union [
  "subscriptAttributeTarget">: python "SingleSubscriptAttributeTarget",
  "name">: python "Name",
  "parens">: python "SingleTarget"]

-- single_subscript_attribute_target:
--     | t_primary '.' NAME !t_lookahead
--     | t_primary '[' slices ']' !t_lookahead

starAtom :: TypeDefinition
starAtom = def "StarAtom" $ T.union [
  "name">: python "Name",
  "targetWithStarAtom">: python "TargetWithStarAtom",
  "starTargetsTupleSeq">: T.maybe $ python "StarTargetsTupleSeq",
  "starTargetsListSeq">: T.maybe $ python "StarTargetsListSeq"]

-- single_target:
--     | single_subscript_attribute_target
--     | NAME
--     | '(' single_target ')'

starTarget :: TypeDefinition
starTarget = def "StarTarget" $ T.union [
  "starred">: python "StarTarget",
  "unstarred">: python "TargetWithStarAtom"]

-- target_with_star_atom:
--     | t_primary '.' NAME !t_lookahead
--     | t_primary '[' slices ']' !t_lookahead
--     | star_atom

starTargetsListSeq :: TypeDefinition
starTargetsListSeq = def "StarTargetsListSeq" $ T.wrap $ nonemptyList $ python "StarTarget"

-- star_targets_tuple_seq:
--     | star_target (',' star_target )+ [',']
--     | star_target ','

starTargetsTupleSeq :: TypeDefinition
starTargetsTupleSeq = def "StarTargetsTupleSeq" $ T.wrap $ nonemptyList $ python "StarTarget"

-- star_target:
--     | '*' (!'*' star_target)
--     | target_with_star_atom

starredExpression :: TypeDefinition
starredExpression = def "StarredExpression" $ T.wrap $ python "Expression"

-- kwarg_or_starred:
--     | NAME '=' expression
--     | starred_expression

tPrimary :: TypeDefinition
tPrimary = def "TPrimary" $ T.union [
  "primaryAndName">: python "TPrimaryAndName",
  "primaryAndSlices">: python "TPrimaryAndSlices",
  "primaryAndGenexp">: python "TPrimaryAndGenexp",
  "primaryAndArguments">: python "TPrimaryAndArguments",
  "atom">: python "Atom"]

tPrimaryAndArguments :: TypeDefinition
tPrimaryAndArguments = def "TPrimaryAndArguments" $ T.record [
  "primary">: python "TPrimary",
  "arguments">: T.maybe $ python "Args"]

-- t_lookahead: '(' | '[' | '.'
--
-- # Targets for del statements
-- # --------------------------
--
-- del_targets: ','.del_target+ [',']

tPrimaryAndGenexp :: TypeDefinition
tPrimaryAndGenexp = def "TPrimaryAndGenexp" $ T.record [
  "primary">: python "TPrimary",
  "genexp">: python "Genexp"]

tPrimaryAndName :: TypeDefinition
tPrimaryAndName = def "TPrimaryAndName" $ T.record [
  "primary">: python "TPrimary",
  "name">: python "Name"]

tPrimaryAndSlices :: TypeDefinition
tPrimaryAndSlices = def "TPrimaryAndSlices" $ T.record [
  "primary">: python "TPrimary",
  "slices">: python "Slices"]

-- star_atom:
--     | NAME
--     | '(' target_with_star_atom ')'
--     | '(' [star_targets_tuple_seq] ')'
--     | '[' [star_targets_list_seq] ']'

targetWithStarAtom :: TypeDefinition
targetWithStarAtom = def "TargetWithStarAtom" $ T.union [
  "project">: python "TPrimaryAndName",
  "slices">: python "TPrimaryAndSlices",
  "atom">: python "StarAtom"]

typeExpression :: TypeDefinition
typeExpression = def "TypeExpression" $ T.union [
  "expression">: python "Expression",
  "starredExpression">: python "Expression",
  "doubleStarredExpression">: python "Expression"]

-- func_type_comment:
--     | NEWLINE TYPE_COMMENT &(NEWLINE INDENT)   # Must be followed by indented block
--     | TYPE_COMMENT
