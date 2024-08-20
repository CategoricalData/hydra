{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Python.Src.Python3 (python3Module) where

import Hydra.Kernel
import Hydra.Dsl.Grammars
import Hydra.Tools.GrammarToModule
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Grammar as G

import qualified Data.List as L


python3Module :: Module
python3Module = grammarToModule ns python3Grammar $
    Just "A Python 3 syntax model, based on the BNF/PEG grammar at https://docs.python.org/3/reference/grammar.html as of 2023-04-03."
  where
    ns = Namespace "hydra/ext/python/python3"

python3Grammar :: G.Grammar
python3Grammar = G.Grammar $ tokens ++ productions

amp_ = terminal "&"
assign_ = terminal ":="
star_ = terminal "*"
at_ = terminal "@"
close_curly_ = terminal "}"
close_paren_ = terminal ")"
close_square_ = terminal "]"
colon_ = terminal ":"
comma_ = terminal ","
dot_ = terminal "."
double_equal_ = terminal "=="
double_gt_ = terminal ">>"
double_lt_ = terminal "<<"
double_slash_ = terminal "//"
double_star_ = terminal "**"
ellipsis_ = terminal "..."
endmarker_ = terminal ""
equal_ = terminal "="
gt_ = terminal ">"
gte_ = terminal ">="
hat_ = terminal "^"
lt_ = terminal "<"
lte_ = terminal "<="
minus_ = terminal "-"
newline_ = terminal "\n"
noteq_ = terminal "!="
open_curly_ = terminal "{"
open_paren_ = terminal "("
open_square_ = terminal "["
percent_ = terminal "%"
pipe_ = terminal "|"
plus_ = terminal "+"
right_arrow_ = terminal "->"
semi_ = terminal ";"
slash_ = terminal "/"
tilde_ = terminal "~"
underscore_ = terminal "_"

tokens :: [G.Production]
tokens = [
  define "Async" [terminal "async"],                -- TODO: not specified in the BNF
  define "Await" [terminal "await"],                -- TODO: not specified in the BNF
  define "Dedent" [terminal "\n"],                  -- TODO: not specified in the BNF
  define "Indent" [terminal "\t"],                  -- TODO: not specified in the BNF
  define "Name" [regex "[A-Za-z0-9][A-Za-z0-9_]*"], -- TODO: not specified in the BNF
  define "Number" [regex "[0-9]+"],                 -- TODO: not specified in the BNF
  define "String" [regex "\"[^\"]*\""],              -- TODO: not specified in the BNF
  define "TypeComment" [terminal ""]]               -- TODO: not specified in the BNF

productions :: [G.Production]
productions = [
-- # STARTING RULES
-- # ==============

-- file: [statements] ENDMARKER
  define "File" [
    star "Statement"],

-- interactive: statement_newline
  define "Interactive" [
    "StatementNewline"],

-- eval: expressions NEWLINE* ENDMARKER
  define "Eval" [
    list[sepp comma_ "Expression", star newline_]],

-- func_type: '(' [type_expressions] ')' '->' expression NEWLINE* ENDMARKER
  define "FuncType" [open_paren_, opt "TypeExpressions", close_paren_, right_arrow_, star newline_, endmarker_],

-- fstring: star_expressions
  define "Fstring" [
    sepp comma_ "StarExpression"],

-- # GENERAL STATEMENTS
-- # ==================

-- statements: statement+

-- statement: compound_stmt  | simple_stmts
  define "Statement" [
    "CompoundStmt",
    "SimpleStmts"],

-- statement_newline:
--     | compound_stmt NEWLINE
--     | simple_stmts
--     | NEWLINE
--     | ENDMARKER
  define "StatementNewline" [
    list["CompoundStmt", newline_],
    "SimpleStmts",
    newline_,
    endmarker_],

-- simple_stmts:
--     | simple_stmt !';' NEWLINE  # Not needed, there for speedup
--     | ';'.simple_stmt+ [';'] NEWLINE
  define "SimpleStmts" [
    list ["SimpleStmt", star(list[semi_, dot_, "SimpleStmt"]), newline_]],

-- # NOTE: assignment MUST precede expression, else parsing a simple assignment
-- # will throw a SyntaxError.
-- simple_stmt:
--     | assignment
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
  define "SimpleStmt" [
    "Assignment",
    sepp comma_ "StarExpression",
    "ReturnStmt",
    "ImportStmt",
    "RaiseStmt",
    terminal "pass",
    "DelStmt",
    "YieldStmt",
    "AssertStmt",
    terminal "break",
    terminal "continue",
    "GlobalStmt",
    "NonlocalStmt"],

-- compound_stmt:
--     | function_def
--     | if_stmt
--     | class_def
--     | with_stmt
--     | for_stmt
--     | try_stmt
--     | while_stmt
--     | match_stmt
  define "CompoundStmt" [
    "FunctionDef",
    "IfStmt",
    "ClassDef",
    "WithStmt",
    "ForStmt",
    "TryStmt",
    "WhileStmt",
    "MatchStmt"],

-- # SIMPLE STATEMENTS
-- # =================

-- # NOTE: annotated_rhs may start with 'yield'; yield_expr must start with 'yield'
-- assignment:
--     | NAME ':' expression ['=' annotated_rhs ]
--     | ('(' single_target ')'
--          | single_subscript_attribute_target) ':' expression ['=' annotated_rhs ]
--     | (star_targets '=' )+ (yield_expr | star_expressions) !'=' [TYPE_COMMENT]
--     | single_target augassign ~ (yield_expr | star_expressions)
  define "Assignment" [
    list["Name", colon_, "Expression", opt(list[equal_, "AnnotatedRhs"])],
    list[alts[list[open_paren_, "SingleTarget", close_paren_], "SingleSubscriptAttributeTarget"], colon_, "Expression", opt(list[equal_, "AnnotatedRhs"])],
    list[plus(list[sepp comma_ "StarTarget", equal_]), alts["YieldExpr", sepp comma_ "StarExpression"], opt("TypeComment")],
    list["SingleTarget", "Augassign", tilde_, alts["YieldExpr", sepp comma_ "StarExpression"]]],

-- annotated_rhs: yield_expr | star_expressions
  define "AnnotatedRhs" [
    "YieldExpr",
    sepp comma_ "StarExpression"],

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
  define "Augassign" [
    terminal "+=",
    terminal "-=",
    terminal "*=",
    terminal "@=",
    terminal "/=",
    terminal "%=",
    terminal "&=",
    terminal "|=",
    terminal "^=",
    terminal "<<=",
    terminal ">>=",
    terminal "**=",
    terminal "//="],

-- return_stmt:
--     | 'return' [star_expressions]
  define "ReturnStmt" [
    list [terminal "return", opt(sepp comma_ "StarExpression")]],

-- raise_stmt:
--     | 'raise' expression ['from' expression ]
--     | 'raise'
  define "RaiseStmt" [
    list[terminal "raise", "Expression", opt(list[terminal "from", "Expression"])],
    terminal "raise"],

-- global_stmt: 'global' ','.NAME+
  define "GlobalStmt" [
    list[terminal "global", comma_, dot_, plus(list[dot_, "Name"])]], -- Note: interpreting the above as (.NAME)+, not .(Name+)

-- nonlocal_stmt: 'nonlocal' ','.NAME+
  define "NonlocalStmt" [
    list[terminal "nonlocal", comma_, plus(list[dot_, "Name"])]], -- Note: interpreting the above as (.NAME)+, not .(Name+)

-- del_stmt:
--     | 'del' del_targets &(';' | NEWLINE)
  define "DelStmt" [
    list[terminal "del", "DelTargets"]],

-- yield_stmt: yield_expr
  define "YieldStmt" [
    "YieldExpr"],

-- assert_stmt: 'assert' expression [',' expression ]
  define "AssertStmt" [
    list[terminal "assert", "Expression", opt(list[comma_, "Expression"])]],

-- import_stmt: import_name | import_from
  define "ImportStmt" [
    "ImportName",
    "ImportFrom"],

-- # Import statements
-- # -----------------

-- import_name: 'import' dotted_as_names
  define "ImportName" [
    list[terminal "import", sep comma_ "DottedAsName"]],

-- # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
-- import_from:
--     | 'from' ('.' | '...')* dotted_name 'import' import_from_targets
--     | 'from' ('.' | '...')+ 'import' import_from_targets
  define "ImportFrom" [
    list[terminal "from", star(alts[dot_, ellipsis_]), "DottedName", terminal "import", "ImportFromTargets"],
    list[terminal "from", plus(alts[dot_, ellipsis_]), terminal "import", "ImportFromTargets"]],

-- import_from_targets:
--     | '(' import_from_as_names [','] ')'
--     | import_from_as_names !','
--     | '*'
  define "ImportFromTargets" [
    list[open_paren_, "ImportFromAsNames", opt(comma_), close_paren_],
    "ImportFromAsNames"],

-- import_from_as_names:
--     | ','.import_from_as_name+
  define "ImportFromAsNames" [
    list[comma_, plus(list[dot_, "ImportFromAsName"])]], -- Note: interpreting the above as (.import_from_as_name)+, not .(import_from_as_name+)

-- import_from_as_name:
--     | NAME ['as' NAME ]
  define "ImportFromAsName" [
    list["Name", opt(list[terminal "as", "Name"])]],

-- dotted_as_names:
--     | ','.dotted_as_name+
-- dotted_as_name:
--     | dotted_name ['as' NAME ]
  define "DottedAsName" [
    list["DottedName", opt(list[terminal "as", "Name"])]],

-- dotted_name:
--     | dotted_name '.' NAME
--     | NAME
  define "DottedName" [
    sep dot_ "Name"],

-- # COMPOUND STATEMENTS
-- # ===================

-- # Common elements
-- # ---------------

-- block:
--     | NEWLINE INDENT statements DEDENT
--     | simple_stmts
  define "Block" [
    list[newline_, "Indent", star "Statement", "Dedent"],
    "SimpleStmts"],

-- decorators: ('@' named_expression NEWLINE )+
  define "Decorators" [
    plus(list[at_, "NamedExpression", newline_])],

-- # Class definitions
-- # -----------------

-- class_def:
--     | decorators class_def_raw
--     | class_def_raw
  define "ClassDef" [
    list["Decorators", "ClassDefRaw"],
    "ClassDefRaw"],

-- class_def_raw:
--     | 'class' NAME ['(' [arguments] ')' ] ':' block
  define "ClassDefRaw" [
    list[terminal "class", "Name", opt(list[open_paren_, opt("Arguments"), close_paren_]), colon_, "Block"]],

-- # Function definitions
-- # --------------------

-- function_def:
--     | decorators function_def_raw
--     | function_def_raw
  define "FunctionDef" [
    list["Decorators", "FunctionDefRaw"],
    "FunctionDefRaw"],

-- function_def_raw:
--     | 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
--     | ASYNC 'def' NAME '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
  define "FunctionDefRaw" [
    list[
      opt(list["Async", terminal "def"]), "Name",
      open_paren_, opt("Parameters"), close_paren_,
      opt(list[right_arrow_, "Expression"]),
      colon_, opt("FuncTypeComment"), "Block"]],

-- # Function parameters
-- # -------------------

-- params:
--     | parameters

-- parameters:
--     | slash_no_default param_no_default* param_with_default* [star_etc]
--     | slash_with_default param_with_default* [star_etc]
--     | param_no_default+ param_with_default* [star_etc]
--     | param_with_default+ [star_etc]
--     | star_etc
  define "Parameters" [
    list["SlashNoDefault", star("ParamNoDefault"), star("ParamWithDefault"), opt("StarEtc")],
    list["SlashWithDefault", star("ParamWithDefault"), opt("StarEtc")],
    list[plus("ParamNoDefault"), star("ParamWithDefault"), opt("StarEtc")],
    list[plus("ParamWithDefault"), opt("StarEtc")],
    "StarEtc"],

-- # Some duplication here because we can't write (',' | &')'),
-- # which is because we don't support empty alternatives (yet).

-- slash_no_default:
--     | param_no_default+ '/' ','
--     | param_no_default+ '/' &')'
  define "SlashNoDefault" [
    list[plus("ParamNoDefault"), slash_, opt(comma_)]],

-- slash_with_default:
--     | param_no_default* param_with_default+ '/' ','
--     | param_no_default* param_with_default+ '/' &')'
  define "SlashWithDefault" [
    list[star("ParamNoDefault"), plus("ParamWithDefault"), slash_, opt(comma_)]],

-- star_etc:
--     | '*' param_no_default param_maybe_default* [kwds]
--     | '*' param_no_default_star_annotation param_maybe_default* [kwds]
--     | '*' ',' param_maybe_default+ [kwds]
--     | kwds
  define "StarEtc" [
    list[star_, "ParamNoDefault", star("ParamMaybeDefault"), opt("Kwds")],
    list[star_, "ParamNoDefaultStarAnnotation", star("ParamMaybeDefault"), opt("Kwds")],
    list[star_, comma_, plus("ParamMaybeDefault"), opt("Kwds")],
    "Kwds"],

-- kwds:
--     | '**' param_no_default
  define "Kwds" [
    list[double_star_, "ParamNoDefault"]],

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
  define "ParamNoDefault" [
    list["Param", opt(comma_), opt("TypeComment")]],

-- param_no_default_star_annotation::r
--     | param_star_annotation ',' TYPE_COMMENT?
--     | param_star_annotation TYPE_COMMENT? &')'
  define "ParamNoDefaultStarAnnotation" [
    list["ParamStarAnnotation", opt(comma_), opt("TypeComment")]],

-- param_with_default:
--     | param default ',' TYPE_COMMENT?
--     | param default TYPE_COMMENT? &')'
  define "ParamWithDefault" [
    list["Param", "Default", opt(comma_), opt("TypeComment")]],

-- param_maybe_default:
--     | param default? ',' TYPE_COMMENT?
--     | param default? TYPE_COMMENT? &')'
  define "ParamMaybeDefault" [
    list["Param", opt("Default"), opt(comma_), opt("TypeComment")]],

-- param: NAME annotation?
  define "Param" [
    list["Name", opt("Annotation")]],

-- param_star_annotation: NAME star_annotation
  define "ParamStarAnnotation" [
    list["Name", "StarAnnotation"]],

-- annotation: ':' expression
  define "Annotation" [
    list[colon_, "Expression"]],

-- star_annotation: ':' star_expression
  define "StarAnnotation" [
    list[colon_, "StarExpression"]],

-- default: '=' expression  | invalid_default
  define "Default" [
    list[equal_, "Expression"]],
    -- "InvalidDefault"], -- TODO: invalid_default is referenced but not defined in the source grammar

-- # If statement
-- # ------------

-- if_stmt:
--     | 'if' named_expression ':' block elif_stmt
--     | 'if' named_expression ':' block [else_block]
  define "IfStmt" [
    list[terminal "if", "NamedExpression", colon_, "Block", alts["ElifStmt", "ElseBlock"]]],

-- elif_stmt:
--     | 'elif' named_expression ':' block elif_stmt
--     | 'elif' named_expression ':' block [else_block]
  define "ElifStmt" [
    list[terminal "elif", "NamedExpression", colon_, "Block", alts["ElifStmt", "ElseBlock"]]],

-- else_block:
--     | 'else' ':' block
  define "ElseBlock" [
    list[terminal "else", colon_, "Block"]],

-- # While statement
-- # ---------------

-- while_stmt:
--     | 'while' named_expression ':' block [else_block]
  define "WhileStmt" [
    list[terminal "while", "NamedExpression", colon_, "Block", opt("ElseBlock")]],

-- # For statement
-- # -------------

-- for_stmt:
--     | 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
--     | ASYNC 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
  define "ForStmt" [
    list[opt("Async"), terminal "for", sepp comma_ "StarTarget", terminal "in", tilde_, sepp comma_ "StarExpression", colon_, opt("TypeComment"), "Block", opt("ElseBlock")]],

-- # With statement
-- # --------------

-- with_stmt:
--     | 'with' '(' ','.with_item+ ','? ')' ':' block
--     | 'with' ','.with_item+ ':' [TYPE_COMMENT] block
--     | ASYNC 'with' '(' ','.with_item+ ','? ')' ':' block
--     | ASYNC 'with' ','.with_item+ ':' [TYPE_COMMENT] block
  define "WithStmt" [
    list[opt("Async"), terminal "with", alts[
      list[open_paren_, sepp comma_ "WithItem", close_paren_, colon_],
      list[sepp comma_ "WithItem", colon_, opt("TypeComment")]], "Block"]],

-- with_item:
--     | expression 'as' star_target &(',' | ')' | ':')
--     | expression
  define "WithItem" [
    list["Expression", opt(list[terminal "as", "StarTarget"])]],

-- # Try statement
-- # -------------

-- try_stmt:
--     | 'try' ':' block finally_block
--     | 'try' ':' block except_block+ [else_block] [finally_block]
--     | 'try' ':' block except_star_block+ [else_block] [finally_block]
  define "TryStmt" [
    list[terminal "try", colon_, "Block", alts[
      "FinallyBlock",
      list[plus("ExceptBlock"), opt("ElseBlock"), opt("FinallyBlock")],
      list[plus("ExceptStarBlock"), opt("ElseBlock"), opt("FinallyBlock")]]]],

-- # Except statement
-- # ----------------

-- except_block:
--     | 'except' expression ['as' NAME ] ':' block
--     | 'except' ':' block
  define "ExceptBlock" [
    list[terminal "except", opt(list["Expression", opt(list[terminal "as", "Name"])]), colon_, "Block"]],

-- except_star_block:
--     | 'except' '*' expression ['as' NAME ] ':' block
  define "ExceptStarBlock" [
    list[terminal "except", star_, "Expression", opt(list[terminal "as", "Name"]), colon_, "Block"]],

-- finally_block:
--     | 'finally' ':' block
  define "FinallyBlock" [
    list[terminal "finally", colon_, "Block"]],

-- # Match statement
-- # ---------------

-- match_stmt:
--     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT
  define "MatchStmt" [
    list[terminal "match", "SubjectExpr", colon_, newline_, "Indent", plus("CaseBlock"), "Dedent"]],

-- subject_expr:
--     | star_named_expression ',' star_named_expressions?
--     | named_expression
  define "SubjectExpr" [
    list["StarNamedExpression", comma_, opt(sepp comma_ "StarNamedExpression")]],

-- case_block:
--     | "case" patterns guard? ':' block
  define "CaseBlock" [
    list[terminal "case", "Patterns", opt("Guard"), colon_, "Block"]],

-- guard: 'if' named_expression
  define "Guard" [
    list[terminal "if", "NamedExpression"]],

-- patterns:
--     | open_sequence_pattern
--     | pattern
  define "Patterns" [
    "OpenSequencePattern",
    "Pattern"],

-- pattern:
--     | as_pattern
--     | or_pattern
  define "Pattern" [
    "AsPattern",
    "OrPattern"],

-- as_pattern:
--     | or_pattern 'as' pattern_capture_target
  define "AsPattern" [
    list["OrPattern", terminal "as", "PatternCaptureTarget"]],

-- or_pattern:
--     | '|'.closed_pattern+
  define "OrPattern" [
    list[pipe_, sep dot_ "ClosedPattern"]],

-- closed_pattern:
--     | literal_pattern
--     | capture_pattern
--     | wildcard_pattern
--     | value_pattern
--     | group_pattern
--     | sequence_pattern
--     | mapping_pattern
--     | class_pattern
  define "ClosedPattern" [
    "LiteralPattern",
    "CapturePattern",
    "WildcardPattern",
    "ValuePattern",
    "GroupPattern",
    "SequencePattern",
    "MappingPattern",
    "ClassPattern"],

-- # Literal patterns are used for equality and identity constraints
-- literal_pattern:
--     | signed_number !('+' | '-')
--     | complex_number
--     | strings
--     | 'None'
--     | 'True'
--     | 'False'
  define "LiteralPattern" [
    "SignedNumber",
    "ComplexNumber",
    plus("String"),
    terminal "None",
    terminal "True",
    terminal "False"],

-- # Literal expressions are used to restrict permitted mapping pattern keys
-- literal_expr:
--     | signed_number !('+' | '-')
--     | complex_number
--     | strings
--     | 'None'
--     | 'True'
--     | 'False'

-- complex_number:
--     | signed_real_number '+' imaginary_number
--     | signed_real_number '-' imaginary_number
  define "ComplexNumber" [
    list["SignedRealNumber", alts[plus_, minus_], "ImaginaryNumber"]],

-- signed_number:
--     | NUMBER
--     | '-' NUMBER
  define "SignedNumber" [
    list[opt(minus_), "Number"]],

-- signed_real_number:
--     | real_number
--     | '-' real_number
  define "SignedRealNumber" [
    list[opt(minus_), "RealNumber"]],

-- real_number:
--     | NUMBER
  define "RealNumber" [
    "Number"],

-- imaginary_number:
--     | NUMBER
  define "ImaginaryNumber" [
    "Number"],

-- capture_pattern:
--     | pattern_capture_target
  define "CapturePattern" [
    "PatternCaptureTarget"],

-- pattern_capture_target:
--     | !"_" NAME !('.' | '(' | '=')
  define "PatternCaptureTarget" [
    "Name"],

-- wildcard_pattern:
--     | "_"
  define "WildcardPattern" [
    underscore_],

-- value_pattern:
--     | attr !('.' | '(' | '=')
  define "ValuePattern" [
    "Attr"],

-- attr:
--     | name_or_attr '.' NAME
  define "Attr" [
    list["NameOrAttr", dot_, "Name"]],

-- name_or_attr:
--     | attr
--     | NAME
  define "NameOrAttr" [
    "Attr",
    "Name"],

-- group_pattern:
--     | '(' pattern ')'
  define "GroupPattern" [
    list[open_paren_, "Pattern", close_paren_]],

-- sequence_pattern:
--     | '[' maybe_sequence_pattern? ']'
--     | '(' open_sequence_pattern? ')'
  define "SequencePattern" [
    list[open_square_, opt("MaybeSequencePattern"), close_square_],
    list[open_paren_, opt("MaybeSequencePattern"), close_paren_]],

-- open_sequence_pattern:
--     | maybe_star_pattern ',' maybe_sequence_pattern?
  define "OpenSequencePattern" [
    list["MaybeStarPattern", comma_, opt("MaybeSequencePattern")]],

-- maybe_sequence_pattern:
--     | ','.maybe_star_pattern+ ','?
  define "MaybeSequencePattern" [
    seq comma_ "MaybeStarPattern"],

-- maybe_star_pattern:
--     | star_pattern
--     | pattern
  define "MaybeStarPattern" [
    "StarPattern",
    "Pattern"],

-- star_pattern:
--     | '*' pattern_capture_target
--     | '*' wildcard_pattern
  define "StarPattern" [
    list[star_, alts["PatternCaptureTarget", "WildcardPattern"]]],

-- mapping_pattern:
--     | '{' '}'
--     | '{' double_star_pattern ','? '}'
--     | '{' items_pattern ',' double_star_pattern ','? '}'
--     | '{' items_pattern ','? '}'
  define "MappingPattern" [
    "empty">: list[open_curly_, close_curly_],
    list[open_curly_, "DoubleStarPattern", opt(comma_), close_curly_],
    list[open_curly_, "ItemsPattern", comma_, "DoubleStarPattern", opt(comma_), close_curly_],
    list[open_curly_, "ItemsPattern", opt(comma_), close_curly_]],

-- items_pattern:
--     | ','.key_value_pattern+
  define "ItemsPattern" [
    sep comma_ "KeyValuePattern"],

-- key_value_pattern:
--     | (literal_expr | attr) ':' pattern
  define "KeyValuePattern" [
    list[alts["LiteralPattern", "Attr"], colon_, "Pattern"]],

-- double_star_pattern:
--     | '**' pattern_capture_target
  define "DoubleStarPattern" [
    list[double_star_, "PatternCaptureTarget"]],

-- class_pattern:
--     | name_or_attr '(' ')'
--     | name_or_attr '(' positional_patterns ','? ')'
--     | name_or_attr '(' keyword_patterns ','? ')'
--     | name_or_attr '(' positional_patterns ',' keyword_patterns ','? ')'
  define "ClassPattern" [
    list["NameOrAttr", open_paren_, alts[
      nil,
      list[seq comma_ "Pattern", opt(comma_)],
      list[seq comma_ "KeywordPattern", opt(comma_)],
      list[seq comma_ "Pattern", comma_, seq comma_ "KeywordPattern", opt(comma_)]
    ], close_paren_]],

-- positional_patterns:
--     | ','.pattern+
-- keyword_patterns:
--     | ','.keyword_pattern+
-- keyword_pattern:
--     | NAME '=' pattern
  define "KeywordPattern" [
    list["Name", equal_, "Pattern"]],
    
-- # EXPRESSIONS
-- # -----------

-- expressions:
--     | expression (',' expression )+ [',']
--     | expression ','
--     | expression

-- expression:
--     | disjunction 'if' disjunction 'else' expression
--     | disjunction
--     | lambdef
  define "Expression" [
    list["Disjunction", terminal "if", "Disjunction", terminal "else", "Expression"],
    "Disjunction",
    "Lambdef"],
    
-- yield_expr:
--     | 'yield' 'from' expression
--     | 'yield' [star_expressions]
  define "YieldExpr" [
    list[terminal "yield", alts[list[terminal "from", "Expression"], opt(sepp comma_ "StarExpression")]]],
    
-- star_expressions:
--     | star_expression (',' star_expression )+ [',']
--     | star_expression ','
--     | star_expression

-- star_expression:
--     | '*' bitwise_or
--     | expression
  define "StarExpression" [
    list[star_, "BitwiseOr"],
    "Expression"],
    
-- star_named_expressions: ','.star_named_expression+ [',']
--
-- star_named_expression:
--     | '*' bitwise_or
--     | named_expression
  define "StarNamedExpression" [
    list[star_, "BitwiseOr"],
    "NamedExpression"],
    
-- assignment_expression:
--     | NAME ':=' ~ expression
  define "AssignmentExpression" [
    list["Name", assign_, tilde_, "Expression"]],
    
-- named_expression:
--     | assignment_expression
--     | expression !':='
  define "NamedExpression" [
    "AssignmentExpression",
    "Expression"],
    
-- disjunction:
--     | conjunction ('or' conjunction )+
--     | conjunction
  define "Disjunction" [
    sep (terminal "or") "Conjunction"],
 
-- conjunction:
--     | inversion ('and' inversion )+
--     | inversion
  define "Conjunction" [
    sep (terminal "and") "Inversion"],
    
-- inversion:
--     | 'not' inversion
--     | comparison
  define "Inversion" [
    list[terminal "not", "Inversion"],
    "Comparison"],
    
-- # Comparison operators
-- # --------------------

-- comparison:
--     | bitwise_or compare_op_bitwise_or_pair+
--     | bitwise_or
  define "Comparison" [
    list["BitwiseOr", star("CompareOpBitwiseOrPair")]],
    
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
  define "CompareOpBitwiseOrPair" [
    "EqBitwiseOr",
    "NoteqBitwiseOr",
    "LteBitwiseOr",
    "LtBitwiseOr",
    "GteBitwiseOr",
    "GtBitwiseOr",
    "NotinBitwiseOr",
    "InBitwiseOr",
    "IsnotBitwiseOr",
    "IsBitwiseOr"],
    
-- eq_bitwise_or: '==' bitwise_or
  define "EqBitwiseOr" [
    list[double_equal_, "BitwiseOr"]],
    
-- noteq_bitwise_or:
--     | ('!=' ) bitwise_or
  define "NoteqBitwiseOr" [
    list[noteq_, "BitwiseOr"]],

-- lte_bitwise_or: '<=' bitwise_or
  define "LteBitwiseOr" [
    list[lte_, "BitwiseOr"]],

-- lt_bitwise_or: '<' bitwise_or
  define "LtBitwiseOr" [
    list[lt_, "BitwiseOr"]],

-- gte_bitwise_or: '>=' bitwise_or
  define "GteBitwiseOr" [
    list[gte_, "BitwiseOr"]],

-- gt_bitwise_or: '>' bitwise_or
  define "GtBitwiseOr" [
    list[gt_, "BitwiseOr"]],

-- notin_bitwise_or: 'not' 'in' bitwise_or
  define "NotinBitwiseOr" [
    list[terminal "not", terminal "in", "BitwiseOr"]],

-- in_bitwise_or: 'in' bitwise_or
  define "InBitwiseOr" [
    list[terminal "in", "BitwiseOr"]],

-- isnot_bitwise_or: 'is' 'not' bitwise_or
  define "IsnotBitwiseOr" [
    list[terminal "is", terminal "not", "BitwiseOr"]],

-- is_bitwise_or: 'is' bitwise_or
  define "IsBitwiseOr" [
    list[terminal "is", "BitwiseOr"]],

-- # Bitwise operators
-- # -----------------

-- bitwise_or:
--     | bitwise_or '|' bitwise_xor
--     | bitwise_xor
  define "BitwiseOr" [
    list[opt(list["BitwiseOr", pipe_]), "BitwiseXor"]],

-- bitwise_xor:
--     | bitwise_xor '^' bitwise_and
--     | bitwise_and
  define "BitwiseXor" [
    list[opt(list["BitwiseXor", hat_]), "BitwiseAnd"]],
    
-- bitwise_and:
--     | bitwise_and '&' shift_expr
--     | shift_expr
  define "BitwiseAnd" [
    list[opt(list["BitwiseAnd", amp_]), "ShiftExpr"]],
    
-- shift_expr:
--     | shift_expr '<<' sum
--     | shift_expr '>>' sum
--     | sum
  define "ShiftExpr" [
    list[
      opt(list["ShiftExpr", alts[double_lt_, double_gt_]]),
      "Sum"]],
    
-- # Arithmetic operators
-- # --------------------

-- sum:
--     | sum '+' term
--     | sum '-' term
--     | term
  define "Sum" [
    list[opt(list["Sum", alts[plus_, minus_]]), "Term"]],
    
-- term:
--     | term '*' factor
--     | term '/' factor
--     | term '//' factor
--     | term '%' factor
--     | term '@' factor
--     | factor
  define "Term" [
    list[opt(list["Term", alts[star_, slash_, double_slash_, percent_, at_]]), "Factor"]],
    
-- factor:
--     | '+' factor
--     | '-' factor
--     | '~' factor
--     | power
  define "Factor" [
    list[alts[plus_, minus_, tilde_], "Factor"],
    "Power"],

-- power:
--     | await_primary '**' factor
--     | await_primary
  define "Power" [
    list["AwaitPrimary", opt(list[double_star_, "Factor"])]],

-- # Primary elements
-- # ----------------

-- # Primary elements are things like "obj.something.something", "obj[something]", "obj(something)", "obj" ...

-- await_primary:
--     | AWAIT primary
--     | primary
  define "AwaitPrimary" [
    list[opt("Await"), "Primary"]],

-- primary:
--     | primary '.' NAME
--     | primary genexp
--     | primary '(' [arguments] ')'
--     | primary '[' slices ']'
--     | atom
  define "Primary" [
    list["Primary", alts[
      list[dot_, "Name"],
      "Genexp",
      list[open_paren_, opt("Arguments"), close_paren_],
      list[open_square_, "Slices", close_square_]]],
    "Atom"],

-- slices:
--     | slice !','
--     | ','.(slice | starred_expression)+ [',']
  define "Slices" [
    "Slice",
    sepp comma_ (alts["Slice", "StarredExpression"])],

-- slice:
--     | [expression] ':' [expression] [':' [expression] ]
--     | named_expression
  define "Slice" [
    list[opt("Expression"), colon_, opt("Expression"), opt(list[colon_, opt("Expression")])],
    "NamedExpression"],

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
  define "Atom" [
    "Name",
    terminal "True",
    terminal "False",
    terminal "None",
    plus("String"),
    "Number",
    "Tuple",
    "Group",
    "Genexp",
    "List",
    "Listcomp",
    "Dict",
    "Set",
    "Dictcomp",
    "Setcomp",
    ellipsis_],

-- group:
--     | '(' (yield_expr | named_expression) ')'
  define "Group" [
    list[open_paren_, alts["YieldExpr", "NamedExpression"], close_paren_]],

-- # Lambda functions
-- # ----------------

-- lambdef:
--     | 'lambda' [lambda_params] ':' expression
  define "Lambdef" [
    list[terminal "lambda", opt("LambdaParameters"), colon_, "Expression"]],

-- lambda_params:
--     | lambda_parameters

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
  define "LambdaParameters" [
    list["LambdaSlashNoDefault", star("LambdaParamNoDefault"), star("LambdaParamWithDefault"), opt("LambdaStarEtc")],
    list["LambdaSlashWithDefault", star("LambdaParamWithDefault"), opt("LambdaStarEtc")],
    list[plus("LambdaParamNoDefault"), star("LambdaParamWithDefault"), opt("LambdaStarEtc")],
    list[plus("LambdaParamWithDefault"), opt("LambdaStarEtc")],
    "LambdaStarEtc"],

-- lambda_slash_no_default:
--     | lambda_param_no_default+ '/' ','
--     | lambda_param_no_default+ '/' &':'
  define "LambdaSlashNoDefault" [
    list[plus("LambdaParamNoDefault"), slash_, opt(comma_)]],

-- lambda_slash_with_default:
--     | lambda_param_no_default* lambda_param_with_default+ '/' ','
--     | lambda_param_no_default* lambda_param_with_default+ '/' &':'
  define "LambdaSlashWithDefault" [
    list[star("LambdaParamNoDefault"), plus("LambdaParamWithDefault"), slash_, opt(comma_)]],

-- lambda_star_etc:
--     | '*' lambda_param_no_default lambda_param_maybe_default* [lambda_kwds]
--     | '*' ',' lambda_param_maybe_default+ [lambda_kwds]
--     | lambda_kwds
  define "LambdaStarEtc" [
    list[star_, "LambdaParamNoDefault", star("LambdaParamMaybeDefault"), opt("LambdaKwds")],
    list[star_, comma_, plus("LambdaParamMaybeDefault"), opt("LambdaKwds")],
    "LambdaKwds"],

-- lambda_kwds:
--     | '**' lambda_param_no_default
  define "LambdaKwds" [
    list[double_star_, "LambdaParamNoDefault"]],

-- lambda_param_no_default:
--     | lambda_param ','
--     | lambda_param &':'
  define "LambdaParamNoDefault" [
    list["LambdaParam", opt(comma_)]],

-- lambda_param_with_default:
--     | lambda_param default ','
--     | lambda_param default &':'
  define "LambdaParamWithDefault" [
    list["LambdaParam", "Default", opt(comma_)]],

-- lambda_param_maybe_default:
--     | lambda_param default? ','
--     | lambda_param default? &':'
  define "LambdaParamMaybeDefault" [
    list["LambdaParam", opt("Default"), opt(comma_)]],

-- lambda_param: NAME
  define "LambdaParam" [
    "Name"],

-- # LITERALS
-- # ========

-- strings: STRING+

-- list:
--     | '[' [star_named_expressions] ']'
  define "List" [
    list[open_square_, sepp comma_ "StarNamedExpression", close_square_]],

-- tuple:
--     | '(' [star_named_expression ',' [star_named_expressions]  ] ')'
  define "Tuple" [
    list[open_paren_, opt(sepp comma_ "StarNamedExpression"), close_paren_]],

-- set: '{' star_named_expressions '}'
  define "Set" [
    list[open_curly_, sepp comma_ "StarNamedExpression", close_curly_]],

-- # Dicts
-- # -----

-- dict:
--     | '{' [double_starred_kvpairs] '}'
  define "Dict" [
    list[open_curly_, opt(sepp comma_ "DoubleStarredKvpair"), close_curly_]],

-- double_starred_kvpairs: ','.double_starred_kvpair+ [',']

-- double_starred_kvpair:
--     | '**' bitwise_or
--     | kvpair
  define "DoubleStarredKvpair" [
    list[double_star_, "BitwiseOr"],
    "Kvpair"],

-- kvpair: expression ':' expression
  define "Kvpair" [
    list["Expression", colon_, "Expression"]],

-- # Comprehensions & Generators
-- # ---------------------------

-- for_if_clauses:
--     | for_if_clause+

-- for_if_clause:
--     | ASYNC 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
--     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
  define "ForIfClause" [
    list[opt("Async"), terminal "for", sepp comma_ "StarTarget", terminal "in", tilde_, "Disjunction", star(list[terminal "if", "Disjunction"])]],

-- listcomp:
--     | '[' named_expression for_if_clauses ']'
  define "Listcomp" [
    list[open_square_, "NamedExpression", plus("ForIfClause"), close_square_]],
    
-- setcomp:
--     | '{' named_expression for_if_clauses '}'
  define "Setcomp" [
    list[open_curly_, "NamedExpression", plus("ForIfClause"), close_curly_]],
    
-- genexp:
--     | '(' ( assignment_expression | expression !':=') for_if_clauses ')'
  define "Genexp" [
    list[open_paren_, alts["AssignmentExpression", "Expression"], plus("ForIfClause"), close_paren_]],

-- dictcomp:
--     | '{' kvpair for_if_clauses '}'
  define "Dictcomp" [
    list[open_curly_, "Kvpair", plus("ForIfClause"), close_curly_]],
    
-- # FUNCTION CALL ARGUMENTS
-- # =======================
--
-- arguments:
--     | args [','] &')'
  define "Arguments" [
    list["Args", opt(comma_)]],

-- args:
--     | ','.(starred_expression | ( assignment_expression | expression !':=') !'=')+ [',' kwargs ]
--     | kwargs
  define "Args" [
    list[sep comma_ (alts["StarredExpression", "AssignmentExpression", "Expression"]), opt(list[comma_, "Kwargs"])],
    "Kwargs"],

-- kwargs:
--     | ','.kwarg_or_starred+ ',' ','.kwarg_or_double_starred+
--     | ','.kwarg_or_starred+
--     | ','.kwarg_or_double_starred+
  define "Kwargs" [
    list[comma_, alts[
      list[plus(list[dot_, "KwargOrStarred"]), opt(list[comma_, comma_, plus(list[dot_, "KwargOrDoubleStarred"])])],
      plus(list[dot_, "KwargOrDoubleStarred"])]]],

-- starred_expression:
--     | '*' expression
  define "StarredExpression" [
    list[star_, "Expression"]],

-- kwarg_or_starred:
--     | NAME '=' expression
--     | starred_expression
  define "KwargOrStarred" [
    list["Name", equal_, "Expression"],
    "StarredExpression"],

-- kwarg_or_double_starred:
--     | NAME '=' expression
--     | '**' expression
  define "KwargOrDoubleStarred" [
    list["Name", equal_, "Expression"],
    list[double_star_, "Expression"]],

-- # ASSIGNMENT TARGETS
-- # ==================

-- # Generic targets
-- # ---------------

-- # NOTE: star_targets may contain *bitwise_or, targets may not.
-- star_targets:
--     | star_target !','
--     | star_target (',' star_target )* [',']

-- star_targets_list_seq: ','.star_target+ [',']
  define "StarTargetsListSeq" [
    sepp comma_ (plus(list[dot_, "StarTarget"]))],

-- star_targets_tuple_seq:
--     | star_target (',' star_target )+ [',']
--     | star_target ','
  define "StarTargetsTupleSeq" [
    sepp comma_ (plus("StarTarget"))],

-- star_target:
--     | '*' (!'*' star_target)
--     | target_with_star_atom
  define "StarTarget" [
    list[star_, "StarTarget"],
    "TargetWithStarAtom"],

-- target_with_star_atom:
--     | t_primary '.' NAME !t_lookahead
--     | t_primary '[' slices ']' !t_lookahead
--     | star_atom
  define "TargetWithStarAtom" [
    list["TPrimary", dot_, "Name"],
    list["TPrimary", open_square_, "Slices", close_square_],
    "StarAtom"],

-- star_atom:
--     | NAME
--     | '(' target_with_star_atom ')'
--     | '(' [star_targets_tuple_seq] ')'
--     | '[' [star_targets_list_seq] ']'
  define "StarAtom" [
    "Name",
    list[open_paren_, "TargetWithStarAtom", close_paren_],
    list[open_paren_, "StarTargetsTupleSeq", close_paren_],
    list[open_square_, "StarTargetsListSeq", close_square_]],

-- single_target:
--     | single_subscript_attribute_target
--     | NAME
--     | '(' single_target ')'
  define "SingleTarget" [
    "SingleSubscriptAttributeTarget",
    "Name",
    list[open_paren_, "SingleTarget", close_paren_]],

-- single_subscript_attribute_target:
--     | t_primary '.' NAME !t_lookahead
--     | t_primary '[' slices ']' !t_lookahead
  define "SingleSubscriptAttributeTarget" [
    list["TPrimary", dot_, "Name"],
    list["TPrimary", open_square_, "Slices", close_square_]],

-- t_primary:
--     | t_primary '.' NAME &t_lookahead
--     | t_primary '[' slices ']' &t_lookahead
--     | t_primary genexp &t_lookahead
--     | t_primary '(' [arguments] ')' &t_lookahead
--     | atom &t_lookahead
  define "TPrimary" [
    list["TPrimary", dot_, "Name"],
    list["TPrimary", open_square_, "Slices", close_square_],
    list["TPrimary", "Genexp"],
    list["TPrimary", open_paren_, opt("Arguments"), close_paren_],
    "Atom"],

-- t_lookahead: '(' | '[' | '.'
  define "TLookahead" [
    open_paren_, open_square_, dot_],

-- # Targets for del statements
-- # --------------------------

-- del_targets: ','.del_target+ [',']
  define "DelTargets" [
    list["TPrimary", dot_, "Name"],
    list["TPrimary", open_square_, "Slices", close_square_],
    "DelTAtom"],

-- del_target:
--     | t_primary '.' NAME !t_lookahead
--     | t_primary '[' slices ']' !t_lookahead
--     | del_t_atom
  define "DelTarget" [
    list["TPrimary", dot_, "Name"],
    list["TPrimary", open_square_, "Slices", close_square_],
    "DelTAtom"],

-- del_t_atom:
--     | NAME
--     | '(' del_target ')'
--     | '(' [del_targets] ')'
--     | '[' [del_targets] ']'
  define "DelTAtom" [
    "Name",
    list[open_paren_, "DelTarget", close_paren_],
    list[open_paren_, opt("DelTargets"), close_paren_],
    list[open_square_, opt("DelTargets"), close_square_]],

-- # TYPING ELEMENTS
-- # ---------------

-- # type_expressions allow */** but ignore them
-- type_expressions:
--     | ','.expression+ ',' '*' expression ',' '**' expression
--     | ','.expression+ ',' '*' expression
--     | ','.expression+ ',' '**' expression
--     | '*' expression ',' '**' expression
--     | '*' expression
--     | '**' expression
--     | ','.expression+
  define "TypeExpressions" [
    list[sepp comma_ "Expression", star_, "Expression", opt(list[comma_, double_star_, "Expression"])],
    list[sepp comma_ "Expression", star_, double_star_, "Expression"],
    list[star_, "Expression", opt(list[comma_, double_star_, "Expression"])],
    list[double_star_, "Expression"],
    list[sepp comma_ "Expression"]],

-- func_type_comment:
--     | NEWLINE TYPE_COMMENT &(NEWLINE INDENT)   # Must be followed by indented block
--     | TYPE_COMMENT
  define "FuncTypeComment" [
    list[opt(newline_), "TypeComment"]]]
