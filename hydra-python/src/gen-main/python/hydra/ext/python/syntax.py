# Note: this is an automatically generated file. Do not edit.

r"""A Python syntax model, based on the Python v3 PEG grammar retrieved on 2024-12-22 from https://docs.python.org/3/reference/grammar.html."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias
import hydra.core

@dataclass(frozen=True)
class AnnotatedStatement:
    comment: str
    statement: Statement

ANNOTATED_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.AnnotatedStatement")
ANNOTATED_STATEMENT__COMMENT__NAME = hydra.core.Name("comment")
ANNOTATED_STATEMENT__STATEMENT__NAME = hydra.core.Name("statement")

class Module(Node["frozenlist[frozenlist[Statement]]"]):
...

MODULE__NAME = hydra.core.Name("hydra.ext.python.syntax.Module")

class QuoteStyle(Enum):
    SINGLE = "single"
    
    DOUBLE = "double"
    
    TRIPLE = "triple"

QUOTE_STYLE__NAME = hydra.core.Name("hydra.ext.python.syntax.QuoteStyle")
QUOTE_STYLE__SINGLE__NAME = hydra.core.Name("single")
QUOTE_STYLE__DOUBLE__NAME = hydra.core.Name("double")
QUOTE_STYLE__TRIPLE__NAME = hydra.core.Name("triple")

class Name(Node[str]):
...

NAME__NAME = hydra.core.Name("hydra.ext.python.syntax.Name")

class NumberInteger(Node[int]):
...

class NumberFloat(Node[Decimal]):
...

class _NumberMeta(type):
    def __getitem__(cls, item):
        return object

class Number(metaclass=_NumberMeta):
    r"""NumberInteger | NumberFloat"""
    
    pass

NUMBER__NAME = hydra.core.Name("hydra.ext.python.syntax.Number")
NUMBER__INTEGER__NAME = hydra.core.Name("integer")
NUMBER__FLOAT__NAME = hydra.core.Name("float")

@dataclass(frozen=True)
class String:
    value: str
    quote_style: QuoteStyle

STRING__NAME = hydra.core.Name("hydra.ext.python.syntax.String")
STRING__VALUE__NAME = hydra.core.Name("value")
STRING__QUOTE_STYLE__NAME = hydra.core.Name("quoteStyle")

class TypeComment(Node[str]):
...

TYPE_COMMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.TypeComment")

class File(Node["frozenlist[Statement]"]):
...

FILE__NAME = hydra.core.Name("hydra.ext.python.syntax.File")

class Interactive(Node["Statement"]):
...

INTERACTIVE__NAME = hydra.core.Name("hydra.ext.python.syntax.Interactive")

class Eval(Node["frozenlist[Expression]"]):
...

EVAL__NAME = hydra.core.Name("hydra.ext.python.syntax.Eval")

@dataclass(frozen=True)
class FuncType:
    type: frozenlist[TypeExpression]
    body: Expression

FUNC_TYPE__NAME = hydra.core.Name("hydra.ext.python.syntax.FuncType")
FUNC_TYPE__TYPE__NAME = hydra.core.Name("type")
FUNC_TYPE__BODY__NAME = hydra.core.Name("body")

class StatementCompound(Node["CompoundStatement"]):
...

class StatementSimple(Node["frozenlist[SimpleStatement]"]):
...

class StatementAnnotated(Node["AnnotatedStatement"]):
...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementCompound | StatementSimple | StatementAnnotated"""
    
    pass

STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.Statement")
STATEMENT__COMPOUND__NAME = hydra.core.Name("compound")
STATEMENT__SIMPLE__NAME = hydra.core.Name("simple")
STATEMENT__ANNOTATED__NAME = hydra.core.Name("annotated")

class SimpleStatementAssignment(Node["Assignment"]):
...

class SimpleStatementTypeAlias(Node["TypeAlias"]):
...

class SimpleStatementStarExpressions(Node["frozenlist[StarExpression]"]):
...

class SimpleStatementReturn(Node["ReturnStatement"]):
...

class SimpleStatementImport(Node["ImportStatement"]):
...

class SimpleStatementRaise(Node["RaiseStatement"]):
...

class SimpleStatementPass:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SimpleStatementPass)
    def __hash__(self):
        return hash("SimpleStatementPass")

class SimpleStatementDel(Node["DelStatement"]):
...

class SimpleStatementYield(Node["YieldStatement"]):
...

class SimpleStatementAssert(Node["AssertStatement"]):
...

class SimpleStatementBreak:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SimpleStatementBreak)
    def __hash__(self):
        return hash("SimpleStatementBreak")

class SimpleStatementContinue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SimpleStatementContinue)
    def __hash__(self):
        return hash("SimpleStatementContinue")

class SimpleStatementGlobal(Node["frozenlist[Name]"]):
...

class SimpleStatementNonlocal(Node["frozenlist[Name]"]):
...

class _SimpleStatementMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleStatement(metaclass=_SimpleStatementMeta):
    r"""SimpleStatementAssignment | SimpleStatementTypeAlias | SimpleStatementStarExpressions | SimpleStatementReturn | SimpleStatementImport | SimpleStatementRaise | SimpleStatementPass | SimpleStatementDel | SimpleStatementYield | SimpleStatementAssert | SimpleStatementBreak | SimpleStatementContinue | SimpleStatementGlobal | SimpleStatementNonlocal"""
    
    pass

SIMPLE_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.SimpleStatement")
SIMPLE_STATEMENT__ASSIGNMENT__NAME = hydra.core.Name("assignment")
SIMPLE_STATEMENT__TYPE_ALIAS__NAME = hydra.core.Name("typeAlias")
SIMPLE_STATEMENT__STAR_EXPRESSIONS__NAME = hydra.core.Name("starExpressions")
SIMPLE_STATEMENT__RETURN__NAME = hydra.core.Name("return")
SIMPLE_STATEMENT__IMPORT__NAME = hydra.core.Name("import")
SIMPLE_STATEMENT__RAISE__NAME = hydra.core.Name("raise")
SIMPLE_STATEMENT__PASS__NAME = hydra.core.Name("pass")
SIMPLE_STATEMENT__DEL__NAME = hydra.core.Name("del")
SIMPLE_STATEMENT__YIELD__NAME = hydra.core.Name("yield")
SIMPLE_STATEMENT__ASSERT__NAME = hydra.core.Name("assert")
SIMPLE_STATEMENT__BREAK__NAME = hydra.core.Name("break")
SIMPLE_STATEMENT__CONTINUE__NAME = hydra.core.Name("continue")
SIMPLE_STATEMENT__GLOBAL__NAME = hydra.core.Name("global")
SIMPLE_STATEMENT__NONLOCAL__NAME = hydra.core.Name("nonlocal")

class CompoundStatementFunction(Node["FunctionDefinition"]):
...

class CompoundStatementIf(Node["IfStatement"]):
...

class CompoundStatementClassDef(Node["ClassDefinition"]):
...

class CompoundStatementWith(Node["WithStatement"]):
...

class CompoundStatementFor(Node["ForStatement"]):
...

class CompoundStatementTry(Node["TryStatement"]):
...

class CompoundStatementWhile(Node["WhileStatement"]):
...

class CompoundStatementMatch(Node["MatchStatement"]):
...

class _CompoundStatementMeta(type):
    def __getitem__(cls, item):
        return object

class CompoundStatement(metaclass=_CompoundStatementMeta):
    r"""CompoundStatementFunction | CompoundStatementIf | CompoundStatementClassDef | CompoundStatementWith | CompoundStatementFor | CompoundStatementTry | CompoundStatementWhile | CompoundStatementMatch"""
    
    pass

COMPOUND_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.CompoundStatement")
COMPOUND_STATEMENT__FUNCTION__NAME = hydra.core.Name("function")
COMPOUND_STATEMENT__IF__NAME = hydra.core.Name("if")
COMPOUND_STATEMENT__CLASS_DEF__NAME = hydra.core.Name("classDef")
COMPOUND_STATEMENT__WITH__NAME = hydra.core.Name("with")
COMPOUND_STATEMENT__FOR__NAME = hydra.core.Name("for")
COMPOUND_STATEMENT__TRY__NAME = hydra.core.Name("try")
COMPOUND_STATEMENT__WHILE__NAME = hydra.core.Name("while")
COMPOUND_STATEMENT__MATCH__NAME = hydra.core.Name("match")

class AssignmentTyped(Node["TypedAssignment"]):
...

class AssignmentUntyped(Node["UntypedAssignment"]):
...

class AssignmentAug(Node["AugAssignment"]):
...

class _AssignmentMeta(type):
    def __getitem__(cls, item):
        return object

class Assignment(metaclass=_AssignmentMeta):
    r"""AssignmentTyped | AssignmentUntyped | AssignmentAug"""
    
    pass

ASSIGNMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.Assignment")
ASSIGNMENT__TYPED__NAME = hydra.core.Name("typed")
ASSIGNMENT__UNTYPED__NAME = hydra.core.Name("untyped")
ASSIGNMENT__AUG__NAME = hydra.core.Name("aug")

@dataclass(frozen=True)
class TypedAssignment:
    lhs: SingleTarget
    type: Expression
    rhs: Maybe[AnnotatedRhs]

TYPED_ASSIGNMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.TypedAssignment")
TYPED_ASSIGNMENT__LHS__NAME = hydra.core.Name("lhs")
TYPED_ASSIGNMENT__TYPE__NAME = hydra.core.Name("type")
TYPED_ASSIGNMENT__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class UntypedAssignment:
    targets: frozenlist[StarTarget]
    rhs: AnnotatedRhs
    type_comment: Maybe[TypeComment]

UNTYPED_ASSIGNMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.UntypedAssignment")
UNTYPED_ASSIGNMENT__TARGETS__NAME = hydra.core.Name("targets")
UNTYPED_ASSIGNMENT__RHS__NAME = hydra.core.Name("rhs")
UNTYPED_ASSIGNMENT__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class AugAssignment:
    lhs: SingleTarget
    augassign: AugAssign
    rhs: AnnotatedRhs

AUG_ASSIGNMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.AugAssignment")
AUG_ASSIGNMENT__LHS__NAME = hydra.core.Name("lhs")
AUG_ASSIGNMENT__AUGASSIGN__NAME = hydra.core.Name("augassign")
AUG_ASSIGNMENT__RHS__NAME = hydra.core.Name("rhs")

class AnnotatedRhsYield(Node["YieldExpression"]):
...

class AnnotatedRhsStar(Node["frozenlist[StarExpression]"]):
...

class _AnnotatedRhsMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotatedRhs(metaclass=_AnnotatedRhsMeta):
    r"""AnnotatedRhsYield | AnnotatedRhsStar"""
    
    pass

ANNOTATED_RHS__NAME = hydra.core.Name("hydra.ext.python.syntax.AnnotatedRhs")
ANNOTATED_RHS__YIELD__NAME = hydra.core.Name("yield")
ANNOTATED_RHS__STAR__NAME = hydra.core.Name("star")

class AugAssign(Enum):
    PLUS_EQUAL = "plusEqual"
    
    MINUS_EQUAL = "minusEqual"
    
    TIMES_EQUAL = "timesEqual"
    
    AT_EQUAL = "atEqual"
    
    SLASH_EQUAL = "slashEqual"
    
    PERCENT_EQUAL = "percentEqual"
    
    AMPERSAND_EQUAL = "ampersandEqual"
    
    BAR_EQUAL = "barEqual"
    
    CARET_EQUAL = "caretEqual"
    
    LEFT_SHIFT_EQUAL = "leftShiftEqual"
    
    RIGHT_SHIFT_EQUAL = "rightShiftEqual"
    
    STAR_STAR_EQUAL = "starStarEqual"
    
    DOUBLE_SLASH_EQUAL = "doubleSlashEqual"

AUG_ASSIGN__NAME = hydra.core.Name("hydra.ext.python.syntax.AugAssign")
AUG_ASSIGN__PLUS_EQUAL__NAME = hydra.core.Name("plusEqual")
AUG_ASSIGN__MINUS_EQUAL__NAME = hydra.core.Name("minusEqual")
AUG_ASSIGN__TIMES_EQUAL__NAME = hydra.core.Name("timesEqual")
AUG_ASSIGN__AT_EQUAL__NAME = hydra.core.Name("atEqual")
AUG_ASSIGN__SLASH_EQUAL__NAME = hydra.core.Name("slashEqual")
AUG_ASSIGN__PERCENT_EQUAL__NAME = hydra.core.Name("percentEqual")
AUG_ASSIGN__AMPERSAND_EQUAL__NAME = hydra.core.Name("ampersandEqual")
AUG_ASSIGN__BAR_EQUAL__NAME = hydra.core.Name("barEqual")
AUG_ASSIGN__CARET_EQUAL__NAME = hydra.core.Name("caretEqual")
AUG_ASSIGN__LEFT_SHIFT_EQUAL__NAME = hydra.core.Name("leftShiftEqual")
AUG_ASSIGN__RIGHT_SHIFT_EQUAL__NAME = hydra.core.Name("rightShiftEqual")
AUG_ASSIGN__STAR_STAR_EQUAL__NAME = hydra.core.Name("starStarEqual")
AUG_ASSIGN__DOUBLE_SLASH_EQUAL__NAME = hydra.core.Name("doubleSlashEqual")

class ReturnStatement(Node["frozenlist[StarExpression]"]):
...

RETURN_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.ReturnStatement")

class RaiseStatement(Node["Maybe[RaiseExpression]"]):
...

RAISE_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.RaiseStatement")

@dataclass(frozen=True)
class RaiseExpression:
    expression: Expression
    from_: Maybe[Expression]

RAISE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.RaiseExpression")
RAISE_EXPRESSION__EXPRESSION__NAME = hydra.core.Name("expression")
RAISE_EXPRESSION__FROM__NAME = hydra.core.Name("from")

class DelStatement(Node["DelTargets"]):
...

DEL_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.DelStatement")

class YieldStatement(Node["YieldExpression"]):
...

YIELD_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.YieldStatement")

@dataclass(frozen=True)
class AssertStatement:
    expression1: Expression
    expression2: Maybe[Expression]

ASSERT_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.AssertStatement")
ASSERT_STATEMENT__EXPRESSION1__NAME = hydra.core.Name("expression1")
ASSERT_STATEMENT__EXPRESSION2__NAME = hydra.core.Name("expression2")

class ImportStatementName(Node["ImportName"]):
...

class ImportStatementFrom(Node["ImportFrom"]):
...

class _ImportStatementMeta(type):
    def __getitem__(cls, item):
        return object

class ImportStatement(metaclass=_ImportStatementMeta):
    r"""ImportStatementName | ImportStatementFrom"""
    
    pass

IMPORT_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.ImportStatement")
IMPORT_STATEMENT__NAME__NAME = hydra.core.Name("name")
IMPORT_STATEMENT__FROM__NAME = hydra.core.Name("from")

class ImportName(Node["frozenlist[DottedAsName]"]):
...

IMPORT_NAME__NAME = hydra.core.Name("hydra.ext.python.syntax.ImportName")

@dataclass(frozen=True)
class ImportFrom:
    prefixes: frozenlist[RelativeImportPrefix]
    dotted_name: Maybe[DottedName]
    targets: ImportFromTargets

IMPORT_FROM__NAME = hydra.core.Name("hydra.ext.python.syntax.ImportFrom")
IMPORT_FROM__PREFIXES__NAME = hydra.core.Name("prefixes")
IMPORT_FROM__DOTTED_NAME__NAME = hydra.core.Name("dottedName")
IMPORT_FROM__TARGETS__NAME = hydra.core.Name("targets")

class RelativeImportPrefix(Enum):
    DOT = "dot"
    
    ELLIPSIS = "ellipsis"

RELATIVE_IMPORT_PREFIX__NAME = hydra.core.Name("hydra.ext.python.syntax.RelativeImportPrefix")
RELATIVE_IMPORT_PREFIX__DOT__NAME = hydra.core.Name("dot")
RELATIVE_IMPORT_PREFIX__ELLIPSIS__NAME = hydra.core.Name("ellipsis")

class ImportFromTargetsSimple(Node["frozenlist[ImportFromAsName]"]):
...

class ImportFromTargetsParens(Node["frozenlist[ImportFromAsName]"]):
...

class ImportFromTargetsStar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ImportFromTargetsStar)
    def __hash__(self):
        return hash("ImportFromTargetsStar")

class _ImportFromTargetsMeta(type):
    def __getitem__(cls, item):
        return object

class ImportFromTargets(metaclass=_ImportFromTargetsMeta):
    r"""ImportFromTargetsSimple | ImportFromTargetsParens | ImportFromTargetsStar"""
    
    pass

IMPORT_FROM_TARGETS__NAME = hydra.core.Name("hydra.ext.python.syntax.ImportFromTargets")
IMPORT_FROM_TARGETS__SIMPLE__NAME = hydra.core.Name("simple")
IMPORT_FROM_TARGETS__PARENS__NAME = hydra.core.Name("parens")
IMPORT_FROM_TARGETS__STAR__NAME = hydra.core.Name("star")

@dataclass(frozen=True)
class ImportFromAsName:
    name: Name
    as_: Maybe[Name]

IMPORT_FROM_AS_NAME__NAME = hydra.core.Name("hydra.ext.python.syntax.ImportFromAsName")
IMPORT_FROM_AS_NAME__NAME__NAME = hydra.core.Name("name")
IMPORT_FROM_AS_NAME__AS__NAME = hydra.core.Name("as")

@dataclass(frozen=True)
class DottedAsName:
    name: DottedName
    as_: Maybe[Name]

DOTTED_AS_NAME__NAME = hydra.core.Name("hydra.ext.python.syntax.DottedAsName")
DOTTED_AS_NAME__NAME__NAME = hydra.core.Name("name")
DOTTED_AS_NAME__AS__NAME = hydra.core.Name("as")

class DottedName(Node["frozenlist[Name]"]):
...

DOTTED_NAME__NAME = hydra.core.Name("hydra.ext.python.syntax.DottedName")

class BlockIndented(Node["frozenlist[frozenlist[Statement]]"]):
...

class BlockSimple(Node["frozenlist[SimpleStatement]"]):
...

class _BlockMeta(type):
    def __getitem__(cls, item):
        return object

class Block(metaclass=_BlockMeta):
    r"""BlockIndented | BlockSimple"""
    
    pass

BLOCK__NAME = hydra.core.Name("hydra.ext.python.syntax.Block")
BLOCK__INDENTED__NAME = hydra.core.Name("indented")
BLOCK__SIMPLE__NAME = hydra.core.Name("simple")

class Decorators(Node["frozenlist[NamedExpression]"]):
...

DECORATORS__NAME = hydra.core.Name("hydra.ext.python.syntax.Decorators")

@dataclass(frozen=True)
class ClassDefinition:
    decorators: Maybe[Decorators]
    name: Name
    type_params: frozenlist[TypeParameter]
    arguments: Maybe[Args]
    body: Block

CLASS_DEFINITION__NAME = hydra.core.Name("hydra.ext.python.syntax.ClassDefinition")
CLASS_DEFINITION__DECORATORS__NAME = hydra.core.Name("decorators")
CLASS_DEFINITION__NAME__NAME = hydra.core.Name("name")
CLASS_DEFINITION__TYPE_PARAMS__NAME = hydra.core.Name("typeParams")
CLASS_DEFINITION__ARGUMENTS__NAME = hydra.core.Name("arguments")
CLASS_DEFINITION__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class FunctionDefinition:
    decorators: Maybe[Decorators]
    raw: FunctionDefRaw

FUNCTION_DEFINITION__NAME = hydra.core.Name("hydra.ext.python.syntax.FunctionDefinition")
FUNCTION_DEFINITION__DECORATORS__NAME = hydra.core.Name("decorators")
FUNCTION_DEFINITION__RAW__NAME = hydra.core.Name("raw")

@dataclass(frozen=True)
class FunctionDefRaw:
    async_: bool
    name: Name
    type_params: frozenlist[TypeParameter]
    params: Maybe[Parameters]
    return_type: Maybe[Expression]
    func_type_comment: Maybe[FuncTypeComment]
    block: Block

FUNCTION_DEF_RAW__NAME = hydra.core.Name("hydra.ext.python.syntax.FunctionDefRaw")
FUNCTION_DEF_RAW__ASYNC__NAME = hydra.core.Name("async")
FUNCTION_DEF_RAW__NAME__NAME = hydra.core.Name("name")
FUNCTION_DEF_RAW__TYPE_PARAMS__NAME = hydra.core.Name("typeParams")
FUNCTION_DEF_RAW__PARAMS__NAME = hydra.core.Name("params")
FUNCTION_DEF_RAW__RETURN_TYPE__NAME = hydra.core.Name("returnType")
FUNCTION_DEF_RAW__FUNC_TYPE_COMMENT__NAME = hydra.core.Name("funcTypeComment")
FUNCTION_DEF_RAW__BLOCK__NAME = hydra.core.Name("block")

class ParametersSlashNoDefault(Node["SlashNoDefaultParameters"]):
...

class ParametersSlashWithDefault(Node["SlashWithDefaultParameters"]):
...

class ParametersParamNoDefault(Node["ParamNoDefaultParameters"]):
...

class ParametersParamWithDefault(Node["ParamWithDefaultParameters"]):
...

class ParametersStarEtc(Node["StarEtc"]):
...

class _ParametersMeta(type):
    def __getitem__(cls, item):
        return object

class Parameters(metaclass=_ParametersMeta):
    r"""ParametersSlashNoDefault | ParametersSlashWithDefault | ParametersParamNoDefault | ParametersParamWithDefault | ParametersStarEtc"""
    
    pass

PARAMETERS__NAME = hydra.core.Name("hydra.ext.python.syntax.Parameters")
PARAMETERS__SLASH_NO_DEFAULT__NAME = hydra.core.Name("slashNoDefault")
PARAMETERS__SLASH_WITH_DEFAULT__NAME = hydra.core.Name("slashWithDefault")
PARAMETERS__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
PARAMETERS__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")
PARAMETERS__STAR_ETC__NAME = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class SlashNoDefaultParameters:
    slash: SlashNoDefault
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]

SLASH_NO_DEFAULT_PARAMETERS__NAME = hydra.core.Name("hydra.ext.python.syntax.SlashNoDefaultParameters")
SLASH_NO_DEFAULT_PARAMETERS__SLASH__NAME = hydra.core.Name("slash")
SLASH_NO_DEFAULT_PARAMETERS__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
SLASH_NO_DEFAULT_PARAMETERS__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")
SLASH_NO_DEFAULT_PARAMETERS__STAR_ETC__NAME = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class SlashWithDefaultParameters:
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]

SLASH_WITH_DEFAULT_PARAMETERS__NAME = hydra.core.Name("hydra.ext.python.syntax.SlashWithDefaultParameters")
SLASH_WITH_DEFAULT_PARAMETERS__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
SLASH_WITH_DEFAULT_PARAMETERS__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")
SLASH_WITH_DEFAULT_PARAMETERS__STAR_ETC__NAME = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class ParamNoDefaultParameters:
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]

PARAM_NO_DEFAULT_PARAMETERS__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamNoDefaultParameters")
PARAM_NO_DEFAULT_PARAMETERS__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
PARAM_NO_DEFAULT_PARAMETERS__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")
PARAM_NO_DEFAULT_PARAMETERS__STAR_ETC__NAME = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class ParamWithDefaultParameters:
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]

PARAM_WITH_DEFAULT_PARAMETERS__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamWithDefaultParameters")
PARAM_WITH_DEFAULT_PARAMETERS__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")
PARAM_WITH_DEFAULT_PARAMETERS__STAR_ETC__NAME = hydra.core.Name("starEtc")

class SlashNoDefault(Node["frozenlist[ParamNoDefault]"]):
...

SLASH_NO_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.SlashNoDefault")

@dataclass(frozen=True)
class SlashWithDefault:
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]

SLASH_WITH_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.SlashWithDefault")
SLASH_WITH_DEFAULT__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
SLASH_WITH_DEFAULT__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")

class StarEtcStarNoDefault(Node["NoDefaultStarEtc"]):
...

class StarEtcStarNoDefaultStarAnnotation(Node["NoDefaultStarAnnotationStarEtc"]):
...

class StarEtcStarComma(Node["CommaStarEtc"]):
...

class StarEtcKeywords(Node["Keywords"]):
...

class _StarEtcMeta(type):
    def __getitem__(cls, item):
        return object

class StarEtc(metaclass=_StarEtcMeta):
    r"""StarEtcStarNoDefault | StarEtcStarNoDefaultStarAnnotation | StarEtcStarComma | StarEtcKeywords"""
    
    pass

STAR_ETC__NAME = hydra.core.Name("hydra.ext.python.syntax.StarEtc")
STAR_ETC__STAR_NO_DEFAULT__NAME = hydra.core.Name("starNoDefault")
STAR_ETC__STAR_NO_DEFAULT_STAR_ANNOTATION__NAME = hydra.core.Name("starNoDefaultStarAnnotation")
STAR_ETC__STAR_COMMA__NAME = hydra.core.Name("starComma")
STAR_ETC__KEYWORDS__NAME = hydra.core.Name("keywords")

@dataclass(frozen=True)
class NoDefaultStarEtc:
    param_no_default: ParamNoDefault
    param_maybe_default: frozenlist[ParamMaybeDefault]
    keywords: Maybe[Keywords]

NO_DEFAULT_STAR_ETC__NAME = hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarEtc")
NO_DEFAULT_STAR_ETC__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
NO_DEFAULT_STAR_ETC__PARAM_MAYBE_DEFAULT__NAME = hydra.core.Name("paramMaybeDefault")
NO_DEFAULT_STAR_ETC__KEYWORDS__NAME = hydra.core.Name("keywords")

@dataclass(frozen=True)
class NoDefaultStarAnnotationStarEtc:
    param_no_default_star_annotation: ParamNoDefaultStarAnnotation
    param_maybe_default: frozenlist[ParamMaybeDefault]
    keywords: Maybe[Keywords]

NO_DEFAULT_STAR_ANNOTATION_STAR_ETC__NAME = hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc")
NO_DEFAULT_STAR_ANNOTATION_STAR_ETC__PARAM_NO_DEFAULT_STAR_ANNOTATION__NAME = hydra.core.Name("paramNoDefaultStarAnnotation")
NO_DEFAULT_STAR_ANNOTATION_STAR_ETC__PARAM_MAYBE_DEFAULT__NAME = hydra.core.Name("paramMaybeDefault")
NO_DEFAULT_STAR_ANNOTATION_STAR_ETC__KEYWORDS__NAME = hydra.core.Name("keywords")

@dataclass(frozen=True)
class CommaStarEtc:
    param_maybe_default: frozenlist[ParamMaybeDefault]
    keywords: Maybe[Keywords]

COMMA_STAR_ETC__NAME = hydra.core.Name("hydra.ext.python.syntax.CommaStarEtc")
COMMA_STAR_ETC__PARAM_MAYBE_DEFAULT__NAME = hydra.core.Name("paramMaybeDefault")
COMMA_STAR_ETC__KEYWORDS__NAME = hydra.core.Name("keywords")

class Keywords(Node["ParamNoDefault"]):
...

KEYWORDS__NAME = hydra.core.Name("hydra.ext.python.syntax.Keywords")

@dataclass(frozen=True)
class ParamNoDefault:
    param: Param
    type_comment: Maybe[TypeComment]

PARAM_NO_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamNoDefault")
PARAM_NO_DEFAULT__PARAM__NAME = hydra.core.Name("param")
PARAM_NO_DEFAULT__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class ParamNoDefaultStarAnnotation:
    param_star_annotation: ParamStarAnnotation
    type_comment: Maybe[TypeComment]

PARAM_NO_DEFAULT_STAR_ANNOTATION__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamNoDefaultStarAnnotation")
PARAM_NO_DEFAULT_STAR_ANNOTATION__PARAM_STAR_ANNOTATION__NAME = hydra.core.Name("paramStarAnnotation")
PARAM_NO_DEFAULT_STAR_ANNOTATION__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class ParamWithDefault:
    param: Param
    default: Default
    type_comment: Maybe[TypeComment]

PARAM_WITH_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamWithDefault")
PARAM_WITH_DEFAULT__PARAM__NAME = hydra.core.Name("param")
PARAM_WITH_DEFAULT__DEFAULT__NAME = hydra.core.Name("default")
PARAM_WITH_DEFAULT__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class ParamMaybeDefault:
    param: Param
    default: Maybe[Default]
    type_comment: Maybe[TypeComment]

PARAM_MAYBE_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamMaybeDefault")
PARAM_MAYBE_DEFAULT__PARAM__NAME = hydra.core.Name("param")
PARAM_MAYBE_DEFAULT__DEFAULT__NAME = hydra.core.Name("default")
PARAM_MAYBE_DEFAULT__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class Param:
    name: Name
    annotation: Maybe[Annotation]

PARAM__NAME = hydra.core.Name("hydra.ext.python.syntax.Param")
PARAM__NAME__NAME = hydra.core.Name("name")
PARAM__ANNOTATION__NAME = hydra.core.Name("annotation")

@dataclass(frozen=True)
class ParamStarAnnotation:
    name: Name
    annotation: StarAnnotation

PARAM_STAR_ANNOTATION__NAME = hydra.core.Name("hydra.ext.python.syntax.ParamStarAnnotation")
PARAM_STAR_ANNOTATION__NAME__NAME = hydra.core.Name("name")
PARAM_STAR_ANNOTATION__ANNOTATION__NAME = hydra.core.Name("annotation")

class Annotation(Node["Expression"]):
...

ANNOTATION__NAME = hydra.core.Name("hydra.ext.python.syntax.Annotation")

class StarAnnotation(Node["StarExpression"]):
...

STAR_ANNOTATION__NAME = hydra.core.Name("hydra.ext.python.syntax.StarAnnotation")

class Default(Node["Expression"]):
...

DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.Default")

@dataclass(frozen=True)
class IfStatement:
    condition: NamedExpression
    body: Block
    continuation: Maybe[IfTail]

IF_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.IfStatement")
IF_STATEMENT__CONDITION__NAME = hydra.core.Name("condition")
IF_STATEMENT__BODY__NAME = hydra.core.Name("body")
IF_STATEMENT__CONTINUATION__NAME = hydra.core.Name("continuation")

class IfTailElif(Node["IfStatement"]):
...

class IfTailElse(Node["Block"]):
...

class _IfTailMeta(type):
    def __getitem__(cls, item):
        return object

class IfTail(metaclass=_IfTailMeta):
    r"""IfTailElif | IfTailElse"""
    
    pass

IF_TAIL__NAME = hydra.core.Name("hydra.ext.python.syntax.IfTail")
IF_TAIL__ELIF__NAME = hydra.core.Name("elif")
IF_TAIL__ELSE__NAME = hydra.core.Name("else")

@dataclass(frozen=True)
class WhileStatement:
    condition: NamedExpression
    body: Block
    else_: Maybe[Block]

WHILE_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.WhileStatement")
WHILE_STATEMENT__CONDITION__NAME = hydra.core.Name("condition")
WHILE_STATEMENT__BODY__NAME = hydra.core.Name("body")
WHILE_STATEMENT__ELSE__NAME = hydra.core.Name("else")

@dataclass(frozen=True)
class ForStatement:
    async_: bool
    targets: frozenlist[StarTarget]
    expressions: frozenlist[StarExpression]
    type_comment: Maybe[TypeComment]
    body: Block
    else_: Maybe[Block]

FOR_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.ForStatement")
FOR_STATEMENT__ASYNC__NAME = hydra.core.Name("async")
FOR_STATEMENT__TARGETS__NAME = hydra.core.Name("targets")
FOR_STATEMENT__EXPRESSIONS__NAME = hydra.core.Name("expressions")
FOR_STATEMENT__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")
FOR_STATEMENT__BODY__NAME = hydra.core.Name("body")
FOR_STATEMENT__ELSE__NAME = hydra.core.Name("else")

@dataclass(frozen=True)
class WithStatement:
    async_: bool
    items: frozenlist[WithItem]
    type_comment: Maybe[TypeComment]
    body: Block

WITH_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.WithStatement")
WITH_STATEMENT__ASYNC__NAME = hydra.core.Name("async")
WITH_STATEMENT__ITEMS__NAME = hydra.core.Name("items")
WITH_STATEMENT__TYPE_COMMENT__NAME = hydra.core.Name("typeComment")
WITH_STATEMENT__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class WithItem:
    expression: Expression
    as_: Maybe[StarTarget]

WITH_ITEM__NAME = hydra.core.Name("hydra.ext.python.syntax.WithItem")
WITH_ITEM__EXPRESSION__NAME = hydra.core.Name("expression")
WITH_ITEM__AS__NAME = hydra.core.Name("as")

class TryStatementFinally(Node["TryFinallyStatement"]):
...

class TryStatementExcept(Node["TryExceptStatement"]):
...

class TryStatementExceptStar(Node["TryExceptStarStatement"]):
...

class _TryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class TryStatement(metaclass=_TryStatementMeta):
    r"""TryStatementFinally | TryStatementExcept | TryStatementExceptStar"""
    
    pass

TRY_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.TryStatement")
TRY_STATEMENT__FINALLY__NAME = hydra.core.Name("finally")
TRY_STATEMENT__EXCEPT__NAME = hydra.core.Name("except")
TRY_STATEMENT__EXCEPT_STAR__NAME = hydra.core.Name("exceptStar")

@dataclass(frozen=True)
class TryFinallyStatement:
    body: Block
    finally_: Block

TRY_FINALLY_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.TryFinallyStatement")
TRY_FINALLY_STATEMENT__BODY__NAME = hydra.core.Name("body")
TRY_FINALLY_STATEMENT__FINALLY__NAME = hydra.core.Name("finally")

@dataclass(frozen=True)
class TryExceptStatement:
    body: Block
    excepts: frozenlist[ExceptBlock]
    else_: Maybe[Block]
    finally_: Maybe[Block]

TRY_EXCEPT_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.TryExceptStatement")
TRY_EXCEPT_STATEMENT__BODY__NAME = hydra.core.Name("body")
TRY_EXCEPT_STATEMENT__EXCEPTS__NAME = hydra.core.Name("excepts")
TRY_EXCEPT_STATEMENT__ELSE__NAME = hydra.core.Name("else")
TRY_EXCEPT_STATEMENT__FINALLY__NAME = hydra.core.Name("finally")

@dataclass(frozen=True)
class TryExceptStarStatement:
    body: Block
    excepts: frozenlist[ExceptStarBlock]
    else_: Maybe[Block]
    finally_: Maybe[Block]

TRY_EXCEPT_STAR_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.TryExceptStarStatement")
TRY_EXCEPT_STAR_STATEMENT__BODY__NAME = hydra.core.Name("body")
TRY_EXCEPT_STAR_STATEMENT__EXCEPTS__NAME = hydra.core.Name("excepts")
TRY_EXCEPT_STAR_STATEMENT__ELSE__NAME = hydra.core.Name("else")
TRY_EXCEPT_STAR_STATEMENT__FINALLY__NAME = hydra.core.Name("finally")

@dataclass(frozen=True)
class ExceptBlock:
    expression: Maybe[ExceptExpression]
    body: Block

EXCEPT_BLOCK__NAME = hydra.core.Name("hydra.ext.python.syntax.ExceptBlock")
EXCEPT_BLOCK__EXPRESSION__NAME = hydra.core.Name("expression")
EXCEPT_BLOCK__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class ExceptExpression:
    expression: Expression
    as_: Maybe[Name]

EXCEPT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.ExceptExpression")
EXCEPT_EXPRESSION__EXPRESSION__NAME = hydra.core.Name("expression")
EXCEPT_EXPRESSION__AS__NAME = hydra.core.Name("as")

@dataclass(frozen=True)
class ExceptStarBlock:
    expression: Expression
    as_: Maybe[Name]
    body: Block

EXCEPT_STAR_BLOCK__NAME = hydra.core.Name("hydra.ext.python.syntax.ExceptStarBlock")
EXCEPT_STAR_BLOCK__EXPRESSION__NAME = hydra.core.Name("expression")
EXCEPT_STAR_BLOCK__AS__NAME = hydra.core.Name("as")
EXCEPT_STAR_BLOCK__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class MatchStatement:
    subject: SubjectExpression
    cases: frozenlist[CaseBlock]

MATCH_STATEMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.MatchStatement")
MATCH_STATEMENT__SUBJECT__NAME = hydra.core.Name("subject")
MATCH_STATEMENT__CASES__NAME = hydra.core.Name("cases")

class SubjectExpressionTuple(Node["frozenlist[StarNamedExpression]"]):
...

class SubjectExpressionSimple(Node["NamedExpression"]):
...

class _SubjectExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class SubjectExpression(metaclass=_SubjectExpressionMeta):
    r"""SubjectExpressionTuple | SubjectExpressionSimple"""
    
    pass

SUBJECT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.SubjectExpression")
SUBJECT_EXPRESSION__TUPLE__NAME = hydra.core.Name("tuple")
SUBJECT_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")

@dataclass(frozen=True)
class CaseBlock:
    patterns: Patterns
    guard: Maybe[Guard]
    body: Block

CASE_BLOCK__NAME = hydra.core.Name("hydra.ext.python.syntax.CaseBlock")
CASE_BLOCK__PATTERNS__NAME = hydra.core.Name("patterns")
CASE_BLOCK__GUARD__NAME = hydra.core.Name("guard")
CASE_BLOCK__BODY__NAME = hydra.core.Name("body")

class Guard(Node["NamedExpression"]):
...

GUARD__NAME = hydra.core.Name("hydra.ext.python.syntax.Guard")

class PatternsSequence(Node["OpenSequencePattern"]):
...

class PatternsPattern(Node["Pattern"]):
...

class _PatternsMeta(type):
    def __getitem__(cls, item):
        return object

class Patterns(metaclass=_PatternsMeta):
    r"""PatternsSequence | PatternsPattern"""
    
    pass

PATTERNS__NAME = hydra.core.Name("hydra.ext.python.syntax.Patterns")
PATTERNS__SEQUENCE__NAME = hydra.core.Name("sequence")
PATTERNS__PATTERN__NAME = hydra.core.Name("pattern")

class PatternAs(Node["AsPattern"]):
...

class PatternOr(Node["OrPattern"]):
...

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

class Pattern(metaclass=_PatternMeta):
    r"""PatternAs | PatternOr"""
    
    pass

PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.Pattern")
PATTERN__AS__NAME = hydra.core.Name("as")
PATTERN__OR__NAME = hydra.core.Name("or")

@dataclass(frozen=True)
class AsPattern:
    pattern: OrPattern
    as_: PatternCaptureTarget

AS_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.AsPattern")
AS_PATTERN__PATTERN__NAME = hydra.core.Name("pattern")
AS_PATTERN__AS__NAME = hydra.core.Name("as")

class OrPattern(Node["frozenlist[ClosedPattern]"]):
...

OR_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.OrPattern")

class ClosedPatternLiteral(Node["LiteralExpression"]):
...

class ClosedPatternCapture(Node["CapturePattern"]):
...

class ClosedPatternWildcard:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ClosedPatternWildcard)
    def __hash__(self):
        return hash("ClosedPatternWildcard")

class ClosedPatternValue(Node["ValuePattern"]):
...

class ClosedPatternGroup(Node["GroupPattern"]):
...

class ClosedPatternSequence(Node["SequencePattern"]):
...

class ClosedPatternMapping(Node["MappingPattern"]):
...

class ClosedPatternClass(Node["ClassPattern"]):
...

class _ClosedPatternMeta(type):
    def __getitem__(cls, item):
        return object

class ClosedPattern(metaclass=_ClosedPatternMeta):
    r"""ClosedPatternLiteral | ClosedPatternCapture | ClosedPatternWildcard | ClosedPatternValue | ClosedPatternGroup | ClosedPatternSequence | ClosedPatternMapping | ClosedPatternClass"""
    
    pass

CLOSED_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.ClosedPattern")
CLOSED_PATTERN__LITERAL__NAME = hydra.core.Name("literal")
CLOSED_PATTERN__CAPTURE__NAME = hydra.core.Name("capture")
CLOSED_PATTERN__WILDCARD__NAME = hydra.core.Name("wildcard")
CLOSED_PATTERN__VALUE__NAME = hydra.core.Name("value")
CLOSED_PATTERN__GROUP__NAME = hydra.core.Name("group")
CLOSED_PATTERN__SEQUENCE__NAME = hydra.core.Name("sequence")
CLOSED_PATTERN__MAPPING__NAME = hydra.core.Name("mapping")
CLOSED_PATTERN__CLASS__NAME = hydra.core.Name("class")

class LiteralExpressionNumber(Node["SignedNumber"]):
...

class LiteralExpressionComplex(Node["ComplexNumber"]):
...

class LiteralExpressionString(Node[str]):
...

class LiteralExpressionNone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralExpressionNone)
    def __hash__(self):
        return hash("LiteralExpressionNone")

class LiteralExpressionTrue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralExpressionTrue)
    def __hash__(self):
        return hash("LiteralExpressionTrue")

class LiteralExpressionFalse:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralExpressionFalse)
    def __hash__(self):
        return hash("LiteralExpressionFalse")

class _LiteralExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class LiteralExpression(metaclass=_LiteralExpressionMeta):
    r"""LiteralExpressionNumber | LiteralExpressionComplex | LiteralExpressionString | LiteralExpressionNone | LiteralExpressionTrue | LiteralExpressionFalse"""
    
    pass

LITERAL_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.LiteralExpression")
LITERAL_EXPRESSION__NUMBER__NAME = hydra.core.Name("number")
LITERAL_EXPRESSION__COMPLEX__NAME = hydra.core.Name("complex")
LITERAL_EXPRESSION__STRING__NAME = hydra.core.Name("string")
LITERAL_EXPRESSION__NONE__NAME = hydra.core.Name("none")
LITERAL_EXPRESSION__TRUE__NAME = hydra.core.Name("true")
LITERAL_EXPRESSION__FALSE__NAME = hydra.core.Name("false")

@dataclass(frozen=True)
class ComplexNumber:
    real: SignedRealNumber
    plus_or_minus: PlusOrMinus
    imaginary: ImaginaryNumber

COMPLEX_NUMBER__NAME = hydra.core.Name("hydra.ext.python.syntax.ComplexNumber")
COMPLEX_NUMBER__REAL__NAME = hydra.core.Name("real")
COMPLEX_NUMBER__PLUS_OR_MINUS__NAME = hydra.core.Name("plusOrMinus")
COMPLEX_NUMBER__IMAGINARY__NAME = hydra.core.Name("imaginary")

class PlusOrMinus(Enum):
    PLUS = "plus"
    
    MINUS = "minus"

PLUS_OR_MINUS__NAME = hydra.core.Name("hydra.ext.python.syntax.PlusOrMinus")
PLUS_OR_MINUS__PLUS__NAME = hydra.core.Name("plus")
PLUS_OR_MINUS__MINUS__NAME = hydra.core.Name("minus")

class SignedNumberSign(Node["PlusOrMinus"]):
...

class SignedNumberNumber(Node["Number"]):
...

class _SignedNumberMeta(type):
    def __getitem__(cls, item):
        return object

class SignedNumber(metaclass=_SignedNumberMeta):
    r"""SignedNumberSign | SignedNumberNumber"""
    
    pass

SIGNED_NUMBER__NAME = hydra.core.Name("hydra.ext.python.syntax.SignedNumber")
SIGNED_NUMBER__SIGN__NAME = hydra.core.Name("sign")
SIGNED_NUMBER__NUMBER__NAME = hydra.core.Name("number")

class SignedRealNumberSign(Node["PlusOrMinus"]):
...

class SignedRealNumberNumber(Node["RealNumber"]):
...

class _SignedRealNumberMeta(type):
    def __getitem__(cls, item):
        return object

class SignedRealNumber(metaclass=_SignedRealNumberMeta):
    r"""SignedRealNumberSign | SignedRealNumberNumber"""
    
    pass

SIGNED_REAL_NUMBER__NAME = hydra.core.Name("hydra.ext.python.syntax.SignedRealNumber")
SIGNED_REAL_NUMBER__SIGN__NAME = hydra.core.Name("sign")
SIGNED_REAL_NUMBER__NUMBER__NAME = hydra.core.Name("number")

class RealNumber(Node["Number"]):
...

REAL_NUMBER__NAME = hydra.core.Name("hydra.ext.python.syntax.RealNumber")

class ImaginaryNumber(Node["Number"]):
...

IMAGINARY_NUMBER__NAME = hydra.core.Name("hydra.ext.python.syntax.ImaginaryNumber")

class CapturePattern(Node["PatternCaptureTarget"]):
...

CAPTURE_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.CapturePattern")

class PatternCaptureTarget(Node["Name"]):
...

PATTERN_CAPTURE_TARGET__NAME = hydra.core.Name("hydra.ext.python.syntax.PatternCaptureTarget")

class ValuePattern(Node["Attribute"]):
...

VALUE_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.ValuePattern")

class Attribute(Node["frozenlist[Name]"]):
...

ATTRIBUTE__NAME = hydra.core.Name("hydra.ext.python.syntax.Attribute")

class NameOrAttribute(Node["frozenlist[Name]"]):
...

NAME_OR_ATTRIBUTE__NAME = hydra.core.Name("hydra.ext.python.syntax.NameOrAttribute")

class GroupPattern(Node["Pattern"]):
...

GROUP_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.GroupPattern")

class SequencePatternList(Node["Maybe[MaybeSequencePattern]"]):
...

class SequencePatternTuple(Node["Maybe[OpenSequencePattern]"]):
...

class _SequencePatternMeta(type):
    def __getitem__(cls, item):
        return object

class SequencePattern(metaclass=_SequencePatternMeta):
    r"""SequencePatternList | SequencePatternTuple"""
    
    pass

SEQUENCE_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.SequencePattern")
SEQUENCE_PATTERN__LIST__NAME = hydra.core.Name("list")
SEQUENCE_PATTERN__TUPLE__NAME = hydra.core.Name("tuple")

@dataclass(frozen=True)
class OpenSequencePattern:
    head: MaybeStarPattern
    tail: Maybe[MaybeSequencePattern]

OPEN_SEQUENCE_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.OpenSequencePattern")
OPEN_SEQUENCE_PATTERN__HEAD__NAME = hydra.core.Name("head")
OPEN_SEQUENCE_PATTERN__TAIL__NAME = hydra.core.Name("tail")

class MaybeSequencePattern(Node["frozenlist[MaybeStarPattern]"]):
...

MAYBE_SEQUENCE_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.MaybeSequencePattern")

class MaybeStarPatternStar(Node["StarPattern"]):
...

class MaybeStarPatternPattern(Node["Pattern"]):
...

class _MaybeStarPatternMeta(type):
    def __getitem__(cls, item):
        return object

class MaybeStarPattern(metaclass=_MaybeStarPatternMeta):
    r"""MaybeStarPatternStar | MaybeStarPatternPattern"""
    
    pass

MAYBE_STAR_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.MaybeStarPattern")
MAYBE_STAR_PATTERN__STAR__NAME = hydra.core.Name("star")
MAYBE_STAR_PATTERN__PATTERN__NAME = hydra.core.Name("pattern")

class StarPatternCapture(Node["PatternCaptureTarget"]):
...

class StarPatternWildcard:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StarPatternWildcard)
    def __hash__(self):
        return hash("StarPatternWildcard")

class _StarPatternMeta(type):
    def __getitem__(cls, item):
        return object

class StarPattern(metaclass=_StarPatternMeta):
    r"""StarPatternCapture | StarPatternWildcard"""
    
    pass

STAR_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.StarPattern")
STAR_PATTERN__CAPTURE__NAME = hydra.core.Name("capture")
STAR_PATTERN__WILDCARD__NAME = hydra.core.Name("wildcard")

@dataclass(frozen=True)
class MappingPattern:
    items: Maybe[ItemsPattern]
    double_star: Maybe[DoubleStarPattern]

MAPPING_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.MappingPattern")
MAPPING_PATTERN__ITEMS__NAME = hydra.core.Name("items")
MAPPING_PATTERN__DOUBLE_STAR__NAME = hydra.core.Name("doubleStar")

class ItemsPattern(Node["frozenlist[KeyValuePattern]"]):
...

ITEMS_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.ItemsPattern")

@dataclass(frozen=True)
class KeyValuePattern:
    key: LiteralExpressionOrAttribute
    value: Pattern

KEY_VALUE_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.KeyValuePattern")
KEY_VALUE_PATTERN__KEY__NAME = hydra.core.Name("key")
KEY_VALUE_PATTERN__VALUE__NAME = hydra.core.Name("value")

class LiteralExpressionOrAttributeLiteral(Node["LiteralExpression"]):
...

class LiteralExpressionOrAttributeAttribute(Node["Attribute"]):
...

class _LiteralExpressionOrAttributeMeta(type):
    def __getitem__(cls, item):
        return object

class LiteralExpressionOrAttribute(metaclass=_LiteralExpressionOrAttributeMeta):
    r"""LiteralExpressionOrAttributeLiteral | LiteralExpressionOrAttributeAttribute"""
    
    pass

LITERAL_EXPRESSION_OR_ATTRIBUTE__NAME = hydra.core.Name("hydra.ext.python.syntax.LiteralExpressionOrAttribute")
LITERAL_EXPRESSION_OR_ATTRIBUTE__LITERAL__NAME = hydra.core.Name("literal")
LITERAL_EXPRESSION_OR_ATTRIBUTE__ATTRIBUTE__NAME = hydra.core.Name("attribute")

class DoubleStarPattern(Node["PatternCaptureTarget"]):
...

DOUBLE_STAR_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.DoubleStarPattern")

@dataclass(frozen=True)
class ClassPattern:
    name_or_attribute: NameOrAttribute
    positional_patterns: Maybe[PositionalPatterns]
    keyword_patterns: Maybe[KeywordPatterns]

CLASS_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.ClassPattern")
CLASS_PATTERN__NAME_OR_ATTRIBUTE__NAME = hydra.core.Name("nameOrAttribute")
CLASS_PATTERN__POSITIONAL_PATTERNS__NAME = hydra.core.Name("positionalPatterns")
CLASS_PATTERN__KEYWORD_PATTERNS__NAME = hydra.core.Name("keywordPatterns")

class PositionalPatterns(Node["frozenlist[Pattern]"]):
...

POSITIONAL_PATTERNS__NAME = hydra.core.Name("hydra.ext.python.syntax.PositionalPatterns")

class KeywordPatterns(Node["frozenlist[KeywordPattern]"]):
...

KEYWORD_PATTERNS__NAME = hydra.core.Name("hydra.ext.python.syntax.KeywordPatterns")

@dataclass(frozen=True)
class KeywordPattern:
    name: Name
    pattern: Pattern

KEYWORD_PATTERN__NAME = hydra.core.Name("hydra.ext.python.syntax.KeywordPattern")
KEYWORD_PATTERN__NAME__NAME = hydra.core.Name("name")
KEYWORD_PATTERN__PATTERN__NAME = hydra.core.Name("pattern")

@dataclass(frozen=True)
class TypeAlias:
    name: Name
    type_params: frozenlist[TypeParameter]
    expression: Expression

TYPE_ALIAS__NAME = hydra.core.Name("hydra.ext.python.syntax.TypeAlias")
TYPE_ALIAS__NAME__NAME = hydra.core.Name("name")
TYPE_ALIAS__TYPE_PARAMS__NAME = hydra.core.Name("typeParams")
TYPE_ALIAS__EXPRESSION__NAME = hydra.core.Name("expression")

class TypeParameterSimple(Node["SimpleTypeParameter"]):
...

class TypeParameterStar(Node["StarTypeParameter"]):
...

class TypeParameterDoubleStar(Node["DoubleStarTypeParameter"]):
...

class _TypeParameterMeta(type):
    def __getitem__(cls, item):
        return object

class TypeParameter(metaclass=_TypeParameterMeta):
    r"""TypeParameterSimple | TypeParameterStar | TypeParameterDoubleStar"""
    
    pass

TYPE_PARAMETER__NAME = hydra.core.Name("hydra.ext.python.syntax.TypeParameter")
TYPE_PARAMETER__SIMPLE__NAME = hydra.core.Name("simple")
TYPE_PARAMETER__STAR__NAME = hydra.core.Name("star")
TYPE_PARAMETER__DOUBLE_STAR__NAME = hydra.core.Name("doubleStar")

@dataclass(frozen=True)
class SimpleTypeParameter:
    name: Name
    bound: Maybe[Expression]
    default: Maybe[Expression]

SIMPLE_TYPE_PARAMETER__NAME = hydra.core.Name("hydra.ext.python.syntax.SimpleTypeParameter")
SIMPLE_TYPE_PARAMETER__NAME__NAME = hydra.core.Name("name")
SIMPLE_TYPE_PARAMETER__BOUND__NAME = hydra.core.Name("bound")
SIMPLE_TYPE_PARAMETER__DEFAULT__NAME = hydra.core.Name("default")

@dataclass(frozen=True)
class StarTypeParameter:
    name: Name
    default: Maybe[StarExpression]

STAR_TYPE_PARAMETER__NAME = hydra.core.Name("hydra.ext.python.syntax.StarTypeParameter")
STAR_TYPE_PARAMETER__NAME__NAME = hydra.core.Name("name")
STAR_TYPE_PARAMETER__DEFAULT__NAME = hydra.core.Name("default")

@dataclass(frozen=True)
class DoubleStarTypeParameter:
    name: Name
    default: Maybe[Expression]

DOUBLE_STAR_TYPE_PARAMETER__NAME = hydra.core.Name("hydra.ext.python.syntax.DoubleStarTypeParameter")
DOUBLE_STAR_TYPE_PARAMETER__NAME__NAME = hydra.core.Name("name")
DOUBLE_STAR_TYPE_PARAMETER__DEFAULT__NAME = hydra.core.Name("default")

class ExpressionConditional(Node["Conditional"]):
...

class ExpressionSimple(Node["Disjunction"]):
...

class ExpressionLambda(Node["Lambda"]):
...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionConditional | ExpressionSimple | ExpressionLambda"""
    
    pass

EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.Expression")
EXPRESSION__CONDITIONAL__NAME = hydra.core.Name("conditional")
EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")
EXPRESSION__LAMBDA__NAME = hydra.core.Name("lambda")

@dataclass(frozen=True)
class Conditional:
    body: Disjunction
    if_: Disjunction
    else_: Expression

CONDITIONAL__NAME = hydra.core.Name("hydra.ext.python.syntax.Conditional")
CONDITIONAL__BODY__NAME = hydra.core.Name("body")
CONDITIONAL__IF__NAME = hydra.core.Name("if")
CONDITIONAL__ELSE__NAME = hydra.core.Name("else")

class YieldExpressionFrom(Node["Expression"]):
...

class YieldExpressionSimple(Node["frozenlist[StarExpression]"]):
...

class _YieldExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class YieldExpression(metaclass=_YieldExpressionMeta):
    r"""YieldExpressionFrom | YieldExpressionSimple"""
    
    pass

YIELD_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.YieldExpression")
YIELD_EXPRESSION__FROM__NAME = hydra.core.Name("from")
YIELD_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")

class StarExpressionStar(Node["BitwiseOr"]):
...

class StarExpressionSimple(Node["Expression"]):
...

class _StarExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class StarExpression(metaclass=_StarExpressionMeta):
    r"""StarExpressionStar | StarExpressionSimple"""
    
    pass

STAR_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.StarExpression")
STAR_EXPRESSION__STAR__NAME = hydra.core.Name("star")
STAR_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")

class StarNamedExpressions(Node["frozenlist[StarNamedExpression]"]):
...

STAR_NAMED_EXPRESSIONS__NAME = hydra.core.Name("hydra.ext.python.syntax.StarNamedExpressions")

class StarNamedExpressionStar(Node["BitwiseOr"]):
...

class StarNamedExpressionSimple(Node["NamedExpression"]):
...

class _StarNamedExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class StarNamedExpression(metaclass=_StarNamedExpressionMeta):
    r"""StarNamedExpressionStar | StarNamedExpressionSimple"""
    
    pass

STAR_NAMED_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.StarNamedExpression")
STAR_NAMED_EXPRESSION__STAR__NAME = hydra.core.Name("star")
STAR_NAMED_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")

@dataclass(frozen=True)
class AssignmentExpression:
    name: Name
    expression: Expression

ASSIGNMENT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.AssignmentExpression")
ASSIGNMENT_EXPRESSION__NAME__NAME = hydra.core.Name("name")
ASSIGNMENT_EXPRESSION__EXPRESSION__NAME = hydra.core.Name("expression")

class NamedExpressionAssignment(Node["AssignmentExpression"]):
...

class NamedExpressionSimple(Node["Expression"]):
...

class _NamedExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class NamedExpression(metaclass=_NamedExpressionMeta):
    r"""NamedExpressionAssignment | NamedExpressionSimple"""
    
    pass

NAMED_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.NamedExpression")
NAMED_EXPRESSION__ASSIGNMENT__NAME = hydra.core.Name("assignment")
NAMED_EXPRESSION__SIMPLE__NAME = hydra.core.Name("simple")

class Disjunction(Node["frozenlist[Conjunction]"]):
...

DISJUNCTION__NAME = hydra.core.Name("hydra.ext.python.syntax.Disjunction")

class Conjunction(Node["frozenlist[Inversion]"]):
...

CONJUNCTION__NAME = hydra.core.Name("hydra.ext.python.syntax.Conjunction")

class InversionNot(Node["Inversion"]):
...

class InversionSimple(Node["Comparison"]):
...

class _InversionMeta(type):
    def __getitem__(cls, item):
        return object

class Inversion(metaclass=_InversionMeta):
    r"""InversionNot | InversionSimple"""
    
    pass

INVERSION__NAME = hydra.core.Name("hydra.ext.python.syntax.Inversion")
INVERSION__NOT__NAME = hydra.core.Name("not")
INVERSION__SIMPLE__NAME = hydra.core.Name("simple")

@dataclass(frozen=True)
class Comparison:
    lhs: BitwiseOr
    rhs: frozenlist[CompareOpBitwiseOrPair]

COMPARISON__NAME = hydra.core.Name("hydra.ext.python.syntax.Comparison")
COMPARISON__LHS__NAME = hydra.core.Name("lhs")
COMPARISON__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class CompareOpBitwiseOrPair:
    operator: CompareOp
    rhs: BitwiseOr

COMPARE_OP_BITWISE_OR_PAIR__NAME = hydra.core.Name("hydra.ext.python.syntax.CompareOpBitwiseOrPair")
COMPARE_OP_BITWISE_OR_PAIR__OPERATOR__NAME = hydra.core.Name("operator")
COMPARE_OP_BITWISE_OR_PAIR__RHS__NAME = hydra.core.Name("rhs")

class CompareOp(Enum):
    EQ = "eq"
    
    NOTEQ = "noteq"
    
    LTE = "lte"
    
    LT = "lt"
    
    GTE = "gte"
    
    GT = "gt"
    
    NOTIN = "notin"
    
    IN = "in"
    
    ISNOT = "isnot"
    
    IS = "is"

COMPARE_OP__NAME = hydra.core.Name("hydra.ext.python.syntax.CompareOp")
COMPARE_OP__EQ__NAME = hydra.core.Name("eq")
COMPARE_OP__NOTEQ__NAME = hydra.core.Name("noteq")
COMPARE_OP__LTE__NAME = hydra.core.Name("lte")
COMPARE_OP__LT__NAME = hydra.core.Name("lt")
COMPARE_OP__GTE__NAME = hydra.core.Name("gte")
COMPARE_OP__GT__NAME = hydra.core.Name("gt")
COMPARE_OP__NOTIN__NAME = hydra.core.Name("notin")
COMPARE_OP__IN__NAME = hydra.core.Name("in")
COMPARE_OP__ISNOT__NAME = hydra.core.Name("isnot")
COMPARE_OP__IS__NAME = hydra.core.Name("is")

@dataclass(frozen=True)
class BitwiseOr:
    lhs: Maybe[BitwiseOr]
    rhs: BitwiseXor

BITWISE_OR__NAME = hydra.core.Name("hydra.ext.python.syntax.BitwiseOr")
BITWISE_OR__LHS__NAME = hydra.core.Name("lhs")
BITWISE_OR__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class BitwiseXor:
    lhs: Maybe[BitwiseXor]
    rhs: BitwiseAnd

BITWISE_XOR__NAME = hydra.core.Name("hydra.ext.python.syntax.BitwiseXor")
BITWISE_XOR__LHS__NAME = hydra.core.Name("lhs")
BITWISE_XOR__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class BitwiseAnd:
    lhs: Maybe[BitwiseAnd]
    rhs: ShiftExpression

BITWISE_AND__NAME = hydra.core.Name("hydra.ext.python.syntax.BitwiseAnd")
BITWISE_AND__LHS__NAME = hydra.core.Name("lhs")
BITWISE_AND__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class ShiftExpression:
    lhs: Maybe[ShiftLhs]
    rhs: Sum

SHIFT_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.ShiftExpression")
SHIFT_EXPRESSION__LHS__NAME = hydra.core.Name("lhs")
SHIFT_EXPRESSION__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class ShiftLhs:
    operand: ShiftExpression
    operator: ShiftOp

SHIFT_LHS__NAME = hydra.core.Name("hydra.ext.python.syntax.ShiftLhs")
SHIFT_LHS__OPERAND__NAME = hydra.core.Name("operand")
SHIFT_LHS__OPERATOR__NAME = hydra.core.Name("operator")

class ShiftOp(Enum):
    LEFT = "left"
    
    RIGHT = "right"

SHIFT_OP__NAME = hydra.core.Name("hydra.ext.python.syntax.ShiftOp")
SHIFT_OP__LEFT__NAME = hydra.core.Name("left")
SHIFT_OP__RIGHT__NAME = hydra.core.Name("right")

@dataclass(frozen=True)
class Sum:
    lhs: Maybe[SumLhs]
    rhs: Term

SUM__NAME = hydra.core.Name("hydra.ext.python.syntax.Sum")
SUM__LHS__NAME = hydra.core.Name("lhs")
SUM__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class SumLhs:
    operand: Sum
    operator: SumOp

SUM_LHS__NAME = hydra.core.Name("hydra.ext.python.syntax.SumLhs")
SUM_LHS__OPERAND__NAME = hydra.core.Name("operand")
SUM_LHS__OPERATOR__NAME = hydra.core.Name("operator")

class SumOp(Enum):
    ADD = "add"
    
    SUB = "sub"

SUM_OP__NAME = hydra.core.Name("hydra.ext.python.syntax.SumOp")
SUM_OP__ADD__NAME = hydra.core.Name("add")
SUM_OP__SUB__NAME = hydra.core.Name("sub")

@dataclass(frozen=True)
class Term:
    lhs: Maybe[TermLhs]
    rhs: Factor

TERM__NAME = hydra.core.Name("hydra.ext.python.syntax.Term")
TERM__LHS__NAME = hydra.core.Name("lhs")
TERM__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class TermLhs:
    operand: Term
    operator: TermOp

TERM_LHS__NAME = hydra.core.Name("hydra.ext.python.syntax.TermLhs")
TERM_LHS__OPERAND__NAME = hydra.core.Name("operand")
TERM_LHS__OPERATOR__NAME = hydra.core.Name("operator")

class TermOp(Enum):
    MUL = "mul"
    
    DIV = "div"
    
    FLOORDIV = "floordiv"
    
    MOD = "mod"
    
    MATMUL = "matmul"

TERM_OP__NAME = hydra.core.Name("hydra.ext.python.syntax.TermOp")
TERM_OP__MUL__NAME = hydra.core.Name("mul")
TERM_OP__DIV__NAME = hydra.core.Name("div")
TERM_OP__FLOORDIV__NAME = hydra.core.Name("floordiv")
TERM_OP__MOD__NAME = hydra.core.Name("mod")
TERM_OP__MATMUL__NAME = hydra.core.Name("matmul")

class FactorPositive(Node["Factor"]):
...

class FactorNegative(Node["Factor"]):
...

class FactorComplement(Node["Factor"]):
...

class FactorSimple(Node["Power"]):
...

class _FactorMeta(type):
    def __getitem__(cls, item):
        return object

class Factor(metaclass=_FactorMeta):
    r"""FactorPositive | FactorNegative | FactorComplement | FactorSimple"""
    
    pass

FACTOR__NAME = hydra.core.Name("hydra.ext.python.syntax.Factor")
FACTOR__POSITIVE__NAME = hydra.core.Name("positive")
FACTOR__NEGATIVE__NAME = hydra.core.Name("negative")
FACTOR__COMPLEMENT__NAME = hydra.core.Name("complement")
FACTOR__SIMPLE__NAME = hydra.core.Name("simple")

@dataclass(frozen=True)
class Power:
    lhs: AwaitPrimary
    rhs: Maybe[Factor]

POWER__NAME = hydra.core.Name("hydra.ext.python.syntax.Power")
POWER__LHS__NAME = hydra.core.Name("lhs")
POWER__RHS__NAME = hydra.core.Name("rhs")

@dataclass(frozen=True)
class AwaitPrimary:
    await_: bool
    primary: Primary

AWAIT_PRIMARY__NAME = hydra.core.Name("hydra.ext.python.syntax.AwaitPrimary")
AWAIT_PRIMARY__AWAIT__NAME = hydra.core.Name("await")
AWAIT_PRIMARY__PRIMARY__NAME = hydra.core.Name("primary")

class PrimarySimple(Node["Atom"]):
...

class PrimaryCompound(Node["PrimaryWithRhs"]):
...

class _PrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class Primary(metaclass=_PrimaryMeta):
    r"""PrimarySimple | PrimaryCompound"""
    
    pass

PRIMARY__NAME = hydra.core.Name("hydra.ext.python.syntax.Primary")
PRIMARY__SIMPLE__NAME = hydra.core.Name("simple")
PRIMARY__COMPOUND__NAME = hydra.core.Name("compound")

@dataclass(frozen=True)
class PrimaryWithRhs:
    primary: Primary
    rhs: PrimaryRhs

PRIMARY_WITH_RHS__NAME = hydra.core.Name("hydra.ext.python.syntax.PrimaryWithRhs")
PRIMARY_WITH_RHS__PRIMARY__NAME = hydra.core.Name("primary")
PRIMARY_WITH_RHS__RHS__NAME = hydra.core.Name("rhs")

class PrimaryRhsProject(Node["Name"]):
...

class PrimaryRhsGenexp(Node["Genexp"]):
...

class PrimaryRhsCall(Node["Args"]):
...

class PrimaryRhsSlices(Node["Slices"]):
...

class _PrimaryRhsMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryRhs(metaclass=_PrimaryRhsMeta):
    r"""PrimaryRhsProject | PrimaryRhsGenexp | PrimaryRhsCall | PrimaryRhsSlices"""
    
    pass

PRIMARY_RHS__NAME = hydra.core.Name("hydra.ext.python.syntax.PrimaryRhs")
PRIMARY_RHS__PROJECT__NAME = hydra.core.Name("project")
PRIMARY_RHS__GENEXP__NAME = hydra.core.Name("genexp")
PRIMARY_RHS__CALL__NAME = hydra.core.Name("call")
PRIMARY_RHS__SLICES__NAME = hydra.core.Name("slices")

@dataclass(frozen=True)
class Slices:
    head: Slice
    tail: frozenlist[SliceOrStarredExpression]

SLICES__NAME = hydra.core.Name("hydra.ext.python.syntax.Slices")
SLICES__HEAD__NAME = hydra.core.Name("head")
SLICES__TAIL__NAME = hydra.core.Name("tail")

class SliceOrStarredExpressionSlice(Node["Slice"]):
...

class SliceOrStarredExpressionStarred(Node["StarredExpression"]):
...

class _SliceOrStarredExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class SliceOrStarredExpression(metaclass=_SliceOrStarredExpressionMeta):
    r"""SliceOrStarredExpressionSlice | SliceOrStarredExpressionStarred"""
    
    pass

SLICE_OR_STARRED_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.SliceOrStarredExpression")
SLICE_OR_STARRED_EXPRESSION__SLICE__NAME = hydra.core.Name("slice")
SLICE_OR_STARRED_EXPRESSION__STARRED__NAME = hydra.core.Name("starred")

class SliceNamed(Node["NamedExpression"]):
...

class SliceSlice(Node["SliceExpression"]):
...

class _SliceMeta(type):
    def __getitem__(cls, item):
        return object

class Slice(metaclass=_SliceMeta):
    r"""SliceNamed | SliceSlice"""
    
    pass

SLICE__NAME = hydra.core.Name("hydra.ext.python.syntax.Slice")
SLICE__NAMED__NAME = hydra.core.Name("named")
SLICE__SLICE__NAME = hydra.core.Name("slice")

@dataclass(frozen=True)
class SliceExpression:
    start: Maybe[Expression]
    stop: Maybe[Expression]
    step: Maybe[Expression]

SLICE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.SliceExpression")
SLICE_EXPRESSION__START__NAME = hydra.core.Name("start")
SLICE_EXPRESSION__STOP__NAME = hydra.core.Name("stop")
SLICE_EXPRESSION__STEP__NAME = hydra.core.Name("step")

class AtomName(Node["Name"]):
...

class AtomTrue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AtomTrue)
    def __hash__(self):
        return hash("AtomTrue")

class AtomFalse:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AtomFalse)
    def __hash__(self):
        return hash("AtomFalse")

class AtomNone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AtomNone)
    def __hash__(self):
        return hash("AtomNone")

class AtomString(Node["String"]):
...

class AtomNumber(Node["Number"]):
...

class AtomTuple(Node["Tuple"]):
...

class AtomGroup(Node["Group"]):
...

class AtomGenexp(Node["Genexp"]):
...

class AtomList(Node["List"]):
...

class AtomListcomp(Node["Listcomp"]):
...

class AtomDict(Node["Dict"]):
...

class AtomSet(Node["Set"]):
...

class AtomDictcomp(Node["Dictcomp"]):
...

class AtomSetcomp(Node["Setcomp"]):
...

class AtomEllipsis:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AtomEllipsis)
    def __hash__(self):
        return hash("AtomEllipsis")

class _AtomMeta(type):
    def __getitem__(cls, item):
        return object

class Atom(metaclass=_AtomMeta):
    r"""AtomName | AtomTrue | AtomFalse | AtomNone | AtomString | AtomNumber | AtomTuple | AtomGroup | AtomGenexp | AtomList | AtomListcomp | AtomDict | AtomSet | AtomDictcomp | AtomSetcomp | AtomEllipsis"""
    
    pass

ATOM__NAME = hydra.core.Name("hydra.ext.python.syntax.Atom")
ATOM__NAME__NAME = hydra.core.Name("name")
ATOM__TRUE__NAME = hydra.core.Name("true")
ATOM__FALSE__NAME = hydra.core.Name("false")
ATOM__NONE__NAME = hydra.core.Name("none")
ATOM__STRING__NAME = hydra.core.Name("string")
ATOM__NUMBER__NAME = hydra.core.Name("number")
ATOM__TUPLE__NAME = hydra.core.Name("tuple")
ATOM__GROUP__NAME = hydra.core.Name("group")
ATOM__GENEXP__NAME = hydra.core.Name("genexp")
ATOM__LIST__NAME = hydra.core.Name("list")
ATOM__LISTCOMP__NAME = hydra.core.Name("listcomp")
ATOM__DICT__NAME = hydra.core.Name("dict")
ATOM__SET__NAME = hydra.core.Name("set")
ATOM__DICTCOMP__NAME = hydra.core.Name("dictcomp")
ATOM__SETCOMP__NAME = hydra.core.Name("setcomp")
ATOM__ELLIPSIS__NAME = hydra.core.Name("ellipsis")

class GroupYield(Node["YieldExpression"]):
...

class GroupExpression(Node["NamedExpression"]):
...

class _GroupMeta(type):
    def __getitem__(cls, item):
        return object

class Group(metaclass=_GroupMeta):
    r"""GroupYield | GroupExpression"""
    
    pass

GROUP__NAME = hydra.core.Name("hydra.ext.python.syntax.Group")
GROUP__YIELD__NAME = hydra.core.Name("yield")
GROUP__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class Lambda:
    params: LambdaParameters
    body: Expression

LAMBDA__NAME = hydra.core.Name("hydra.ext.python.syntax.Lambda")
LAMBDA__PARAMS__NAME = hydra.core.Name("params")
LAMBDA__BODY__NAME = hydra.core.Name("body")

@dataclass(frozen=True)
class LambdaParameters:
    slash_no_default: Maybe[LambdaSlashNoDefault]
    param_no_default: frozenlist[LambdaParamNoDefault]
    param_with_default: frozenlist[LambdaParamWithDefault]
    star_etc: Maybe[LambdaStarEtc]

LAMBDA_PARAMETERS__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaParameters")
LAMBDA_PARAMETERS__SLASH_NO_DEFAULT__NAME = hydra.core.Name("slashNoDefault")
LAMBDA_PARAMETERS__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
LAMBDA_PARAMETERS__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")
LAMBDA_PARAMETERS__STAR_ETC__NAME = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class LambdaSlashNoDefault:
    parameters: frozenlist[LambdaParamNoDefault]

LAMBDA_SLASH_NO_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaSlashNoDefault")
LAMBDA_SLASH_NO_DEFAULT__PARAMETERS__NAME = hydra.core.Name("parameters")

@dataclass(frozen=True)
class LambdaSlashWithDefault:
    param_no_default: frozenlist[LambdaParamNoDefault]
    param_with_default: frozenlist[LambdaParamWithDefault]

LAMBDA_SLASH_WITH_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaSlashWithDefault")
LAMBDA_SLASH_WITH_DEFAULT__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
LAMBDA_SLASH_WITH_DEFAULT__PARAM_WITH_DEFAULT__NAME = hydra.core.Name("paramWithDefault")

class LambdaStarEtcStar(Node[bool]):
...

class LambdaStarEtcParamNoDefault(Node["LambdaParamNoDefault"]):
...

class LambdaStarEtcParamMaybeDefault(Node["frozenlist[LambdaParamMaybeDefault]"]):
...

class LambdaStarEtcKwds(Node["LambdaKwds"]):
...

class _LambdaStarEtcMeta(type):
    def __getitem__(cls, item):
        return object

class LambdaStarEtc(metaclass=_LambdaStarEtcMeta):
    r"""LambdaStarEtcStar | LambdaStarEtcParamNoDefault | LambdaStarEtcParamMaybeDefault | LambdaStarEtcKwds"""
    
    pass

LAMBDA_STAR_ETC__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaStarEtc")
LAMBDA_STAR_ETC__STAR__NAME = hydra.core.Name("star")
LAMBDA_STAR_ETC__PARAM_NO_DEFAULT__NAME = hydra.core.Name("paramNoDefault")
LAMBDA_STAR_ETC__PARAM_MAYBE_DEFAULT__NAME = hydra.core.Name("paramMaybeDefault")
LAMBDA_STAR_ETC__KWDS__NAME = hydra.core.Name("kwds")

class LambdaKwds(Node["LambdaParamNoDefault"]):
...

LAMBDA_KWDS__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaKwds")

class LambdaParamNoDefault(Node["Name"]):
...

LAMBDA_PARAM_NO_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaParamNoDefault")

@dataclass(frozen=True)
class LambdaParamWithDefault:
    param: Name
    default: Maybe[Default]

LAMBDA_PARAM_WITH_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaParamWithDefault")
LAMBDA_PARAM_WITH_DEFAULT__PARAM__NAME = hydra.core.Name("param")
LAMBDA_PARAM_WITH_DEFAULT__DEFAULT__NAME = hydra.core.Name("default")

@dataclass(frozen=True)
class LambdaParamMaybeDefault:
    param: Name
    default: Maybe[Default]

LAMBDA_PARAM_MAYBE_DEFAULT__NAME = hydra.core.Name("hydra.ext.python.syntax.LambdaParamMaybeDefault")
LAMBDA_PARAM_MAYBE_DEFAULT__PARAM__NAME = hydra.core.Name("param")
LAMBDA_PARAM_MAYBE_DEFAULT__DEFAULT__NAME = hydra.core.Name("default")

class List(Node["frozenlist[StarNamedExpression]"]):
...

LIST__NAME = hydra.core.Name("hydra.ext.python.syntax.List")

class Tuple(Node["frozenlist[StarNamedExpression]"]):
...

TUPLE__NAME = hydra.core.Name("hydra.ext.python.syntax.Tuple")

class Set(Node["frozenlist[StarNamedExpression]"]):
...

SET__NAME = hydra.core.Name("hydra.ext.python.syntax.Set")

class Dict(Node["frozenlist[DoubleStarredKvpair]"]):
...

DICT__NAME = hydra.core.Name("hydra.ext.python.syntax.Dict")

class DoubleStarredKvpairStarred(Node["BitwiseOr"]):
...

class DoubleStarredKvpairPair(Node["Kvpair"]):
...

class _DoubleStarredKvpairMeta(type):
    def __getitem__(cls, item):
        return object

class DoubleStarredKvpair(metaclass=_DoubleStarredKvpairMeta):
    r"""DoubleStarredKvpairStarred | DoubleStarredKvpairPair"""
    
    pass

DOUBLE_STARRED_KVPAIR__NAME = hydra.core.Name("hydra.ext.python.syntax.DoubleStarredKvpair")
DOUBLE_STARRED_KVPAIR__STARRED__NAME = hydra.core.Name("starred")
DOUBLE_STARRED_KVPAIR__PAIR__NAME = hydra.core.Name("pair")

@dataclass(frozen=True)
class Kvpair:
    key: Expression
    value: Expression

KVPAIR__NAME = hydra.core.Name("hydra.ext.python.syntax.Kvpair")
KVPAIR__KEY__NAME = hydra.core.Name("key")
KVPAIR__VALUE__NAME = hydra.core.Name("value")

class ForIfClauses(Node["frozenlist[ForIfClause]"]):
...

FOR_IF_CLAUSES__NAME = hydra.core.Name("hydra.ext.python.syntax.ForIfClauses")

@dataclass(frozen=True)
class ForIfClause:
    async_: bool
    targets: frozenlist[StarTarget]
    in_: Disjunction
    ifs: frozenlist[Disjunction]

FOR_IF_CLAUSE__NAME = hydra.core.Name("hydra.ext.python.syntax.ForIfClause")
FOR_IF_CLAUSE__ASYNC__NAME = hydra.core.Name("async")
FOR_IF_CLAUSE__TARGETS__NAME = hydra.core.Name("targets")
FOR_IF_CLAUSE__IN__NAME = hydra.core.Name("in")
FOR_IF_CLAUSE__IFS__NAME = hydra.core.Name("ifs")

@dataclass(frozen=True)
class Listcomp:
    expression: NamedExpression
    for_if_clauses: ForIfClauses

LISTCOMP__NAME = hydra.core.Name("hydra.ext.python.syntax.Listcomp")
LISTCOMP__EXPRESSION__NAME = hydra.core.Name("expression")
LISTCOMP__FOR_IF_CLAUSES__NAME = hydra.core.Name("forIfClauses")

@dataclass(frozen=True)
class Setcomp:
    expression: NamedExpression
    for_if_clauses: ForIfClauses

SETCOMP__NAME = hydra.core.Name("hydra.ext.python.syntax.Setcomp")
SETCOMP__EXPRESSION__NAME = hydra.core.Name("expression")
SETCOMP__FOR_IF_CLAUSES__NAME = hydra.core.Name("forIfClauses")

@dataclass(frozen=True)
class Genexp:
    head: GenexpHead
    tail: ForIfClauses

GENEXP__NAME = hydra.core.Name("hydra.ext.python.syntax.Genexp")
GENEXP__HEAD__NAME = hydra.core.Name("head")
GENEXP__TAIL__NAME = hydra.core.Name("tail")

class GenexpHeadAssignment(Node["AssignmentExpression"]):
...

class GenexpHeadExpression(Node["Expression"]):
...

class _GenexpHeadMeta(type):
    def __getitem__(cls, item):
        return object

class GenexpHead(metaclass=_GenexpHeadMeta):
    r"""GenexpHeadAssignment | GenexpHeadExpression"""
    
    pass

GENEXP_HEAD__NAME = hydra.core.Name("hydra.ext.python.syntax.GenexpHead")
GENEXP_HEAD__ASSIGNMENT__NAME = hydra.core.Name("assignment")
GENEXP_HEAD__EXPRESSION__NAME = hydra.core.Name("expression")

@dataclass(frozen=True)
class Dictcomp:
    kvpair: Kvpair
    for_if_clauses: ForIfClauses

DICTCOMP__NAME = hydra.core.Name("hydra.ext.python.syntax.Dictcomp")
DICTCOMP__KVPAIR__NAME = hydra.core.Name("kvpair")
DICTCOMP__FOR_IF_CLAUSES__NAME = hydra.core.Name("forIfClauses")

@dataclass(frozen=True)
class Args:
    positional: frozenlist[PosArg]
    kwarg_or_starred: frozenlist[KwargOrStarred]
    kwarg_or_double_starred: frozenlist[KwargOrDoubleStarred]

ARGS__NAME = hydra.core.Name("hydra.ext.python.syntax.Args")
ARGS__POSITIONAL__NAME = hydra.core.Name("positional")
ARGS__KWARG_OR_STARRED__NAME = hydra.core.Name("kwargOrStarred")
ARGS__KWARG_OR_DOUBLE_STARRED__NAME = hydra.core.Name("kwargOrDoubleStarred")

class PosArgStarred(Node["StarredExpression"]):
...

class PosArgAssignment(Node["AssignmentExpression"]):
...

class PosArgExpression(Node["Expression"]):
...

class _PosArgMeta(type):
    def __getitem__(cls, item):
        return object

class PosArg(metaclass=_PosArgMeta):
    r"""PosArgStarred | PosArgAssignment | PosArgExpression"""
    
    pass

POS_ARG__NAME = hydra.core.Name("hydra.ext.python.syntax.PosArg")
POS_ARG__STARRED__NAME = hydra.core.Name("starred")
POS_ARG__ASSIGNMENT__NAME = hydra.core.Name("assignment")
POS_ARG__EXPRESSION__NAME = hydra.core.Name("expression")

class StarredExpression(Node["Expression"]):
...

STARRED_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.StarredExpression")

class KwargOrStarredKwarg(Node["Kwarg"]):
...

class KwargOrStarredStarred(Node["StarredExpression"]):
...

class _KwargOrStarredMeta(type):
    def __getitem__(cls, item):
        return object

class KwargOrStarred(metaclass=_KwargOrStarredMeta):
    r"""KwargOrStarredKwarg | KwargOrStarredStarred"""
    
    pass

KWARG_OR_STARRED__NAME = hydra.core.Name("hydra.ext.python.syntax.KwargOrStarred")
KWARG_OR_STARRED__KWARG__NAME = hydra.core.Name("kwarg")
KWARG_OR_STARRED__STARRED__NAME = hydra.core.Name("starred")

@dataclass(frozen=True)
class Kwarg:
    name: Name
    value: Expression

KWARG__NAME = hydra.core.Name("hydra.ext.python.syntax.Kwarg")
KWARG__NAME__NAME = hydra.core.Name("name")
KWARG__VALUE__NAME = hydra.core.Name("value")

class KwargOrDoubleStarredKwarg(Node["Kwarg"]):
...

class KwargOrDoubleStarredDoubleStarred(Node["Expression"]):
...

class _KwargOrDoubleStarredMeta(type):
    def __getitem__(cls, item):
        return object

class KwargOrDoubleStarred(metaclass=_KwargOrDoubleStarredMeta):
    r"""KwargOrDoubleStarredKwarg | KwargOrDoubleStarredDoubleStarred"""
    
    pass

KWARG_OR_DOUBLE_STARRED__NAME = hydra.core.Name("hydra.ext.python.syntax.KwargOrDoubleStarred")
KWARG_OR_DOUBLE_STARRED__KWARG__NAME = hydra.core.Name("kwarg")
KWARG_OR_DOUBLE_STARRED__DOUBLE_STARRED__NAME = hydra.core.Name("doubleStarred")

class StarTargetsListSeq(Node["frozenlist[StarTarget]"]):
...

STAR_TARGETS_LIST_SEQ__NAME = hydra.core.Name("hydra.ext.python.syntax.StarTargetsListSeq")

class StarTargetsTupleSeq(Node["frozenlist[StarTarget]"]):
...

STAR_TARGETS_TUPLE_SEQ__NAME = hydra.core.Name("hydra.ext.python.syntax.StarTargetsTupleSeq")

class StarTargetStarred(Node["StarTarget"]):
...

class StarTargetUnstarred(Node["TargetWithStarAtom"]):
...

class _StarTargetMeta(type):
    def __getitem__(cls, item):
        return object

class StarTarget(metaclass=_StarTargetMeta):
    r"""StarTargetStarred | StarTargetUnstarred"""
    
    pass

STAR_TARGET__NAME = hydra.core.Name("hydra.ext.python.syntax.StarTarget")
STAR_TARGET__STARRED__NAME = hydra.core.Name("starred")
STAR_TARGET__UNSTARRED__NAME = hydra.core.Name("unstarred")

class TargetWithStarAtomProject(Node["TPrimaryAndName"]):
...

class TargetWithStarAtomSlices(Node["TPrimaryAndSlices"]):
...

class TargetWithStarAtomAtom(Node["StarAtom"]):
...

class _TargetWithStarAtomMeta(type):
    def __getitem__(cls, item):
        return object

class TargetWithStarAtom(metaclass=_TargetWithStarAtomMeta):
    r"""TargetWithStarAtomProject | TargetWithStarAtomSlices | TargetWithStarAtomAtom"""
    
    pass

TARGET_WITH_STAR_ATOM__NAME = hydra.core.Name("hydra.ext.python.syntax.TargetWithStarAtom")
TARGET_WITH_STAR_ATOM__PROJECT__NAME = hydra.core.Name("project")
TARGET_WITH_STAR_ATOM__SLICES__NAME = hydra.core.Name("slices")
TARGET_WITH_STAR_ATOM__ATOM__NAME = hydra.core.Name("atom")

@dataclass(frozen=True)
class TPrimaryAndName:
    primary: TPrimary
    name: Name

T_PRIMARY_AND_NAME__NAME = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndName")
T_PRIMARY_AND_NAME__PRIMARY__NAME = hydra.core.Name("primary")
T_PRIMARY_AND_NAME__NAME__NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class TPrimaryAndSlices:
    primary: TPrimary
    slices: Slices

T_PRIMARY_AND_SLICES__NAME = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndSlices")
T_PRIMARY_AND_SLICES__PRIMARY__NAME = hydra.core.Name("primary")
T_PRIMARY_AND_SLICES__SLICES__NAME = hydra.core.Name("slices")

class StarAtomName(Node["Name"]):
...

class StarAtomTargetWithStarAtom(Node["TargetWithStarAtom"]):
...

class StarAtomStarTargetsTupleSeq(Node["Maybe[StarTargetsTupleSeq]"]):
...

class StarAtomStarTargetsListSeq(Node["Maybe[StarTargetsListSeq]"]):
...

class _StarAtomMeta(type):
    def __getitem__(cls, item):
        return object

class StarAtom(metaclass=_StarAtomMeta):
    r"""StarAtomName | StarAtomTargetWithStarAtom | StarAtomStarTargetsTupleSeq | StarAtomStarTargetsListSeq"""
    
    pass

STAR_ATOM__NAME = hydra.core.Name("hydra.ext.python.syntax.StarAtom")
STAR_ATOM__NAME__NAME = hydra.core.Name("name")
STAR_ATOM__TARGET_WITH_STAR_ATOM__NAME = hydra.core.Name("targetWithStarAtom")
STAR_ATOM__STAR_TARGETS_TUPLE_SEQ__NAME = hydra.core.Name("starTargetsTupleSeq")
STAR_ATOM__STAR_TARGETS_LIST_SEQ__NAME = hydra.core.Name("starTargetsListSeq")

class SingleTargetSubscriptAttributeTarget(Node["SingleSubscriptAttributeTarget"]):
...

class SingleTargetName(Node["Name"]):
...

class SingleTargetParens(Node["SingleTarget"]):
...

class _SingleTargetMeta(type):
    def __getitem__(cls, item):
        return object

class SingleTarget(metaclass=_SingleTargetMeta):
    r"""SingleTargetSubscriptAttributeTarget | SingleTargetName | SingleTargetParens"""
    
    pass

SINGLE_TARGET__NAME = hydra.core.Name("hydra.ext.python.syntax.SingleTarget")
SINGLE_TARGET__SUBSCRIPT_ATTRIBUTE_TARGET__NAME = hydra.core.Name("subscriptAttributeTarget")
SINGLE_TARGET__NAME__NAME = hydra.core.Name("name")
SINGLE_TARGET__PARENS__NAME = hydra.core.Name("parens")

class SingleSubscriptAttributeTargetPrimaryAndName(Node["TPrimaryAndName"]):
...

class SingleSubscriptAttributeTargetPrimaryAndSlices(Node["TPrimaryAndSlices"]):
...

class _SingleSubscriptAttributeTargetMeta(type):
    def __getitem__(cls, item):
        return object

class SingleSubscriptAttributeTarget(metaclass=_SingleSubscriptAttributeTargetMeta):
    r"""SingleSubscriptAttributeTargetPrimaryAndName | SingleSubscriptAttributeTargetPrimaryAndSlices"""
    
    pass

SINGLE_SUBSCRIPT_ATTRIBUTE_TARGET__NAME = hydra.core.Name("hydra.ext.python.syntax.SingleSubscriptAttributeTarget")
SINGLE_SUBSCRIPT_ATTRIBUTE_TARGET__PRIMARY_AND_NAME__NAME = hydra.core.Name("primaryAndName")
SINGLE_SUBSCRIPT_ATTRIBUTE_TARGET__PRIMARY_AND_SLICES__NAME = hydra.core.Name("primaryAndSlices")

class TPrimaryPrimaryAndName(Node["TPrimaryAndName"]):
...

class TPrimaryPrimaryAndSlices(Node["TPrimaryAndSlices"]):
...

class TPrimaryPrimaryAndGenexp(Node["TPrimaryAndGenexp"]):
...

class TPrimaryPrimaryAndArguments(Node["TPrimaryAndArguments"]):
...

class TPrimaryAtom(Node["Atom"]):
...

class _TPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class TPrimary(metaclass=_TPrimaryMeta):
    r"""TPrimaryPrimaryAndName | TPrimaryPrimaryAndSlices | TPrimaryPrimaryAndGenexp | TPrimaryPrimaryAndArguments | TPrimaryAtom"""
    
    pass

T_PRIMARY__NAME = hydra.core.Name("hydra.ext.python.syntax.TPrimary")
T_PRIMARY__PRIMARY_AND_NAME__NAME = hydra.core.Name("primaryAndName")
T_PRIMARY__PRIMARY_AND_SLICES__NAME = hydra.core.Name("primaryAndSlices")
T_PRIMARY__PRIMARY_AND_GENEXP__NAME = hydra.core.Name("primaryAndGenexp")
T_PRIMARY__PRIMARY_AND_ARGUMENTS__NAME = hydra.core.Name("primaryAndArguments")
T_PRIMARY__ATOM__NAME = hydra.core.Name("atom")

@dataclass(frozen=True)
class TPrimaryAndGenexp:
    primary: TPrimary
    genexp: Genexp

T_PRIMARY_AND_GENEXP__NAME = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndGenexp")
T_PRIMARY_AND_GENEXP__PRIMARY__NAME = hydra.core.Name("primary")
T_PRIMARY_AND_GENEXP__GENEXP__NAME = hydra.core.Name("genexp")

@dataclass(frozen=True)
class TPrimaryAndArguments:
    primary: TPrimary
    arguments: Maybe[Args]

T_PRIMARY_AND_ARGUMENTS__NAME = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndArguments")
T_PRIMARY_AND_ARGUMENTS__PRIMARY__NAME = hydra.core.Name("primary")
T_PRIMARY_AND_ARGUMENTS__ARGUMENTS__NAME = hydra.core.Name("arguments")

class DelTargets(Node["frozenlist[DelTarget]"]):
...

DEL_TARGETS__NAME = hydra.core.Name("hydra.ext.python.syntax.DelTargets")

class DelTargetPrimaryAndName(Node["TPrimaryAndName"]):
...

class DelTargetPrimaryAndSlices(Node["TPrimaryAndSlices"]):
...

class DelTargetDelTAtom(Node["DelTAtom"]):
...

class _DelTargetMeta(type):
    def __getitem__(cls, item):
        return object

class DelTarget(metaclass=_DelTargetMeta):
    r"""DelTargetPrimaryAndName | DelTargetPrimaryAndSlices | DelTargetDelTAtom"""
    
    pass

DEL_TARGET__NAME = hydra.core.Name("hydra.ext.python.syntax.DelTarget")
DEL_TARGET__PRIMARY_AND_NAME__NAME = hydra.core.Name("primaryAndName")
DEL_TARGET__PRIMARY_AND_SLICES__NAME = hydra.core.Name("primaryAndSlices")
DEL_TARGET__DEL_T_ATOM__NAME = hydra.core.Name("delTAtom")

class DelTAtomName(Node["Name"]):
...

class DelTAtomTarget(Node["DelTarget"]):
...

class DelTAtomTargets(Node["DelTargets"]):
...

class _DelTAtomMeta(type):
    def __getitem__(cls, item):
        return object

class DelTAtom(metaclass=_DelTAtomMeta):
    r"""DelTAtomName | DelTAtomTarget | DelTAtomTargets"""
    
    pass

DEL_T_ATOM__NAME = hydra.core.Name("hydra.ext.python.syntax.DelTAtom")
DEL_T_ATOM__NAME__NAME = hydra.core.Name("name")
DEL_T_ATOM__TARGET__NAME = hydra.core.Name("target")
DEL_T_ATOM__TARGETS__NAME = hydra.core.Name("targets")

class TypeExpressionExpression(Node["Expression"]):
...

class TypeExpressionStarredExpression(Node["Expression"]):
...

class TypeExpressionDoubleStarredExpression(Node["Expression"]):
...

class _TypeExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class TypeExpression(metaclass=_TypeExpressionMeta):
    r"""TypeExpressionExpression | TypeExpressionStarredExpression | TypeExpressionDoubleStarredExpression"""
    
    pass

TYPE_EXPRESSION__NAME = hydra.core.Name("hydra.ext.python.syntax.TypeExpression")
TYPE_EXPRESSION__EXPRESSION__NAME = hydra.core.Name("expression")
TYPE_EXPRESSION__STARRED_EXPRESSION__NAME = hydra.core.Name("starredExpression")
TYPE_EXPRESSION__DOUBLE_STARRED_EXPRESSION__NAME = hydra.core.Name("doubleStarredExpression")

class FuncTypeComment(Node["TypeComment"]):
...

FUNC_TYPE_COMMENT__NAME = hydra.core.Name("hydra.ext.python.syntax.FuncTypeComment")
