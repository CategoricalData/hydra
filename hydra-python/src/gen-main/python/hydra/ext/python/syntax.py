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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AnnotatedStatement")
    COMMENT = hydra.core.Name("comment")
    STATEMENT = hydra.core.Name("statement")

class Module(Node["frozenlist[frozenlist[Statement]]"]):
    ...

Module.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Module")

class QuoteStyle(Enum):
    SINGLE = hydra.core.Name("single")
    
    DOUBLE = hydra.core.Name("double")
    
    TRIPLE = hydra.core.Name("triple")

QuoteStyle.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.QuoteStyle")

class Name(Node[str]):
    ...

Name.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Name")

class NumberInteger(Node[int]):
    ...

class NumberFloat(Node[Decimal]):
    ...

class _NumberMeta(type):
    def __getitem__(cls, item):
        return object

class Number(metaclass=_NumberMeta):
    r"""NumberInteger | NumberFloat"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Number")
    INTEGER = hydra.core.Name("integer")
    FLOAT = hydra.core.Name("float")

@dataclass(frozen=True)
class String:
    value: str
    quote_style: QuoteStyle
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.String")
    VALUE = hydra.core.Name("value")
    QUOTE_STYLE = hydra.core.Name("quoteStyle")

class TypeComment(Node[str]):
    ...

TypeComment.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TypeComment")

class File(Node["frozenlist[Statement]"]):
    ...

File.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.File")

class Interactive(Node["Statement"]):
    ...

Interactive.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Interactive")

class Eval(Node["frozenlist[Expression]"]):
    ...

Eval.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Eval")

@dataclass(frozen=True)
class FuncType:
    type: frozenlist[TypeExpression]
    body: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.FuncType")
    TYPE = hydra.core.Name("type")
    BODY = hydra.core.Name("body")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Statement")
    COMPOUND = hydra.core.Name("compound")
    SIMPLE = hydra.core.Name("simple")
    ANNOTATED = hydra.core.Name("annotated")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SimpleStatement")
    ASSIGNMENT = hydra.core.Name("assignment")
    TYPE_ALIAS = hydra.core.Name("typeAlias")
    STAR_EXPRESSIONS = hydra.core.Name("starExpressions")
    RETURN = hydra.core.Name("return")
    IMPORT = hydra.core.Name("import")
    RAISE = hydra.core.Name("raise")
    PASS = hydra.core.Name("pass")
    DEL = hydra.core.Name("del")
    YIELD = hydra.core.Name("yield")
    ASSERT = hydra.core.Name("assert")
    BREAK = hydra.core.Name("break")
    CONTINUE = hydra.core.Name("continue")
    GLOBAL = hydra.core.Name("global")
    NONLOCAL = hydra.core.Name("nonlocal")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.CompoundStatement")
    FUNCTION = hydra.core.Name("function")
    IF = hydra.core.Name("if")
    CLASS_DEF = hydra.core.Name("classDef")
    WITH = hydra.core.Name("with")
    FOR = hydra.core.Name("for")
    TRY = hydra.core.Name("try")
    WHILE = hydra.core.Name("while")
    MATCH = hydra.core.Name("match")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Assignment")
    TYPED = hydra.core.Name("typed")
    UNTYPED = hydra.core.Name("untyped")
    AUG = hydra.core.Name("aug")

@dataclass(frozen=True)
class TypedAssignment:
    lhs: SingleTarget
    type: Expression
    rhs: Maybe[AnnotatedRhs]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TypedAssignment")
    LHS = hydra.core.Name("lhs")
    TYPE = hydra.core.Name("type")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class UntypedAssignment:
    targets: frozenlist[StarTarget]
    rhs: AnnotatedRhs
    type_comment: Maybe[TypeComment]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.UntypedAssignment")
    TARGETS = hydra.core.Name("targets")
    RHS = hydra.core.Name("rhs")
    TYPE_COMMENT = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class AugAssignment:
    lhs: SingleTarget
    augassign: AugAssign
    rhs: AnnotatedRhs
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AugAssignment")
    LHS = hydra.core.Name("lhs")
    AUGASSIGN = hydra.core.Name("augassign")
    RHS = hydra.core.Name("rhs")

class AnnotatedRhsYield(Node["YieldExpression"]):
    ...

class AnnotatedRhsStar(Node["frozenlist[StarExpression]"]):
    ...

class _AnnotatedRhsMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotatedRhs(metaclass=_AnnotatedRhsMeta):
    r"""AnnotatedRhsYield | AnnotatedRhsStar"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AnnotatedRhs")
    YIELD = hydra.core.Name("yield")
    STAR = hydra.core.Name("star")

class AugAssign(Enum):
    PLUS_EQUAL = hydra.core.Name("plusEqual")
    
    MINUS_EQUAL = hydra.core.Name("minusEqual")
    
    TIMES_EQUAL = hydra.core.Name("timesEqual")
    
    AT_EQUAL = hydra.core.Name("atEqual")
    
    SLASH_EQUAL = hydra.core.Name("slashEqual")
    
    PERCENT_EQUAL = hydra.core.Name("percentEqual")
    
    AMPERSAND_EQUAL = hydra.core.Name("ampersandEqual")
    
    BAR_EQUAL = hydra.core.Name("barEqual")
    
    CARET_EQUAL = hydra.core.Name("caretEqual")
    
    LEFT_SHIFT_EQUAL = hydra.core.Name("leftShiftEqual")
    
    RIGHT_SHIFT_EQUAL = hydra.core.Name("rightShiftEqual")
    
    STAR_STAR_EQUAL = hydra.core.Name("starStarEqual")
    
    DOUBLE_SLASH_EQUAL = hydra.core.Name("doubleSlashEqual")

AugAssign.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AugAssign")

class ReturnStatement(Node["frozenlist[StarExpression]"]):
    ...

ReturnStatement.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ReturnStatement")

class RaiseStatement(Node["Maybe[RaiseExpression]"]):
    ...

RaiseStatement.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.RaiseStatement")

@dataclass(frozen=True)
class RaiseExpression:
    expression: Expression
    from_: Maybe[Expression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.RaiseExpression")
    EXPRESSION = hydra.core.Name("expression")
    FROM = hydra.core.Name("from")

class DelStatement(Node["DelTargets"]):
    ...

DelStatement.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DelStatement")

class YieldStatement(Node["YieldExpression"]):
    ...

YieldStatement.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.YieldStatement")

@dataclass(frozen=True)
class AssertStatement:
    expression1: Expression
    expression2: Maybe[Expression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AssertStatement")
    EXPRESSION1 = hydra.core.Name("expression1")
    EXPRESSION2 = hydra.core.Name("expression2")

class ImportStatementName(Node["ImportName"]):
    ...

class ImportStatementFrom(Node["ImportFrom"]):
    ...

class _ImportStatementMeta(type):
    def __getitem__(cls, item):
        return object

class ImportStatement(metaclass=_ImportStatementMeta):
    r"""ImportStatementName | ImportStatementFrom"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ImportStatement")
    NAME = hydra.core.Name("name")
    FROM = hydra.core.Name("from")

class ImportName(Node["frozenlist[DottedAsName]"]):
    ...

ImportName.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ImportName")

@dataclass(frozen=True)
class ImportFrom:
    prefixes: frozenlist[RelativeImportPrefix]
    dotted_name: Maybe[DottedName]
    targets: ImportFromTargets
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ImportFrom")
    PREFIXES = hydra.core.Name("prefixes")
    DOTTED_NAME = hydra.core.Name("dottedName")
    TARGETS = hydra.core.Name("targets")

class RelativeImportPrefix(Enum):
    DOT = hydra.core.Name("dot")
    
    ELLIPSIS = hydra.core.Name("ellipsis")

RelativeImportPrefix.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.RelativeImportPrefix")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ImportFromTargets")
    SIMPLE = hydra.core.Name("simple")
    PARENS = hydra.core.Name("parens")
    STAR = hydra.core.Name("star")

@dataclass(frozen=True)
class ImportFromAsName:
    name: Name
    as_: Maybe[Name]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ImportFromAsName")
    NAME = hydra.core.Name("name")
    AS = hydra.core.Name("as")

@dataclass(frozen=True)
class DottedAsName:
    name: DottedName
    as_: Maybe[Name]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DottedAsName")
    NAME = hydra.core.Name("name")
    AS = hydra.core.Name("as")

class DottedName(Node["frozenlist[Name]"]):
    ...

DottedName.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DottedName")

class BlockIndented(Node["frozenlist[frozenlist[Statement]]"]):
    ...

class BlockSimple(Node["frozenlist[SimpleStatement]"]):
    ...

class _BlockMeta(type):
    def __getitem__(cls, item):
        return object

class Block(metaclass=_BlockMeta):
    r"""BlockIndented | BlockSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Block")
    INDENTED = hydra.core.Name("indented")
    SIMPLE = hydra.core.Name("simple")

class Decorators(Node["frozenlist[NamedExpression]"]):
    ...

Decorators.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Decorators")

@dataclass(frozen=True)
class ClassDefinition:
    decorators: Maybe[Decorators]
    name: Name
    type_params: frozenlist[TypeParameter]
    arguments: Maybe[Args]
    body: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ClassDefinition")
    DECORATORS = hydra.core.Name("decorators")
    NAME = hydra.core.Name("name")
    TYPE_PARAMS = hydra.core.Name("typeParams")
    ARGUMENTS = hydra.core.Name("arguments")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class FunctionDefinition:
    decorators: Maybe[Decorators]
    raw: FunctionDefRaw
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.FunctionDefinition")
    DECORATORS = hydra.core.Name("decorators")
    RAW = hydra.core.Name("raw")

@dataclass(frozen=True)
class FunctionDefRaw:
    async_: bool
    name: Name
    type_params: frozenlist[TypeParameter]
    params: Maybe[Parameters]
    return_type: Maybe[Expression]
    func_type_comment: Maybe[FuncTypeComment]
    block: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.FunctionDefRaw")
    ASYNC = hydra.core.Name("async")
    NAME = hydra.core.Name("name")
    TYPE_PARAMS = hydra.core.Name("typeParams")
    PARAMS = hydra.core.Name("params")
    RETURN_TYPE = hydra.core.Name("returnType")
    FUNC_TYPE_COMMENT = hydra.core.Name("funcTypeComment")
    BLOCK = hydra.core.Name("block")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Parameters")
    SLASH_NO_DEFAULT = hydra.core.Name("slashNoDefault")
    SLASH_WITH_DEFAULT = hydra.core.Name("slashWithDefault")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")
    STAR_ETC = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class SlashNoDefaultParameters:
    slash: SlashNoDefault
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SlashNoDefaultParameters")
    SLASH = hydra.core.Name("slash")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")
    STAR_ETC = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class SlashWithDefaultParameters:
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SlashWithDefaultParameters")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")
    STAR_ETC = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class ParamNoDefaultParameters:
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamNoDefaultParameters")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")
    STAR_ETC = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class ParamWithDefaultParameters:
    param_with_default: frozenlist[ParamWithDefault]
    star_etc: Maybe[StarEtc]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamWithDefaultParameters")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")
    STAR_ETC = hydra.core.Name("starEtc")

class SlashNoDefault(Node["frozenlist[ParamNoDefault]"]):
    ...

SlashNoDefault.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SlashNoDefault")

@dataclass(frozen=True)
class SlashWithDefault:
    param_no_default: frozenlist[ParamNoDefault]
    param_with_default: frozenlist[ParamWithDefault]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SlashWithDefault")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarEtc")
    STAR_NO_DEFAULT = hydra.core.Name("starNoDefault")
    STAR_NO_DEFAULT_STAR_ANNOTATION = hydra.core.Name("starNoDefaultStarAnnotation")
    STAR_COMMA = hydra.core.Name("starComma")
    KEYWORDS = hydra.core.Name("keywords")

@dataclass(frozen=True)
class NoDefaultStarEtc:
    param_no_default: ParamNoDefault
    param_maybe_default: frozenlist[ParamMaybeDefault]
    keywords: Maybe[Keywords]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarEtc")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_MAYBE_DEFAULT = hydra.core.Name("paramMaybeDefault")
    KEYWORDS = hydra.core.Name("keywords")

@dataclass(frozen=True)
class NoDefaultStarAnnotationStarEtc:
    param_no_default_star_annotation: ParamNoDefaultStarAnnotation
    param_maybe_default: frozenlist[ParamMaybeDefault]
    keywords: Maybe[Keywords]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.NoDefaultStarAnnotationStarEtc")
    PARAM_NO_DEFAULT_STAR_ANNOTATION = hydra.core.Name("paramNoDefaultStarAnnotation")
    PARAM_MAYBE_DEFAULT = hydra.core.Name("paramMaybeDefault")
    KEYWORDS = hydra.core.Name("keywords")

@dataclass(frozen=True)
class CommaStarEtc:
    param_maybe_default: frozenlist[ParamMaybeDefault]
    keywords: Maybe[Keywords]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.CommaStarEtc")
    PARAM_MAYBE_DEFAULT = hydra.core.Name("paramMaybeDefault")
    KEYWORDS = hydra.core.Name("keywords")

class Keywords(Node["ParamNoDefault"]):
    ...

Keywords.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Keywords")

@dataclass(frozen=True)
class ParamNoDefault:
    param: Param
    type_comment: Maybe[TypeComment]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamNoDefault")
    PARAM = hydra.core.Name("param")
    TYPE_COMMENT = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class ParamNoDefaultStarAnnotation:
    param_star_annotation: ParamStarAnnotation
    type_comment: Maybe[TypeComment]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamNoDefaultStarAnnotation")
    PARAM_STAR_ANNOTATION = hydra.core.Name("paramStarAnnotation")
    TYPE_COMMENT = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class ParamWithDefault:
    param: Param
    default: Default
    type_comment: Maybe[TypeComment]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamWithDefault")
    PARAM = hydra.core.Name("param")
    DEFAULT = hydra.core.Name("default")
    TYPE_COMMENT = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class ParamMaybeDefault:
    param: Param
    default: Maybe[Default]
    type_comment: Maybe[TypeComment]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamMaybeDefault")
    PARAM = hydra.core.Name("param")
    DEFAULT = hydra.core.Name("default")
    TYPE_COMMENT = hydra.core.Name("typeComment")

@dataclass(frozen=True)
class Param:
    name: Name
    annotation: Maybe[Annotation]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Param")
    NAME = hydra.core.Name("name")
    ANNOTATION = hydra.core.Name("annotation")

@dataclass(frozen=True)
class ParamStarAnnotation:
    name: Name
    annotation: StarAnnotation
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ParamStarAnnotation")
    NAME = hydra.core.Name("name")
    ANNOTATION = hydra.core.Name("annotation")

class Annotation(Node["Expression"]):
    ...

Annotation.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Annotation")

class StarAnnotation(Node["StarExpression"]):
    ...

StarAnnotation.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarAnnotation")

class Default(Node["Expression"]):
    ...

Default.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Default")

@dataclass(frozen=True)
class IfStatement:
    condition: NamedExpression
    body: Block
    continuation: Maybe[IfTail]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.IfStatement")
    CONDITION = hydra.core.Name("condition")
    BODY = hydra.core.Name("body")
    CONTINUATION = hydra.core.Name("continuation")

class IfTailElif(Node["IfStatement"]):
    ...

class IfTailElse(Node["Block"]):
    ...

class _IfTailMeta(type):
    def __getitem__(cls, item):
        return object

class IfTail(metaclass=_IfTailMeta):
    r"""IfTailElif | IfTailElse"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.IfTail")
    ELIF = hydra.core.Name("elif")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class WhileStatement:
    condition: NamedExpression
    body: Block
    else_: Maybe[Block]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.WhileStatement")
    CONDITION = hydra.core.Name("condition")
    BODY = hydra.core.Name("body")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class ForStatement:
    async_: bool
    targets: frozenlist[StarTarget]
    expressions: frozenlist[StarExpression]
    type_comment: Maybe[TypeComment]
    body: Block
    else_: Maybe[Block]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ForStatement")
    ASYNC = hydra.core.Name("async")
    TARGETS = hydra.core.Name("targets")
    EXPRESSIONS = hydra.core.Name("expressions")
    TYPE_COMMENT = hydra.core.Name("typeComment")
    BODY = hydra.core.Name("body")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class WithStatement:
    async_: bool
    items: frozenlist[WithItem]
    type_comment: Maybe[TypeComment]
    body: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.WithStatement")
    ASYNC = hydra.core.Name("async")
    ITEMS = hydra.core.Name("items")
    TYPE_COMMENT = hydra.core.Name("typeComment")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class WithItem:
    expression: Expression
    as_: Maybe[StarTarget]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.WithItem")
    EXPRESSION = hydra.core.Name("expression")
    AS = hydra.core.Name("as")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TryStatement")
    FINALLY = hydra.core.Name("finally")
    EXCEPT = hydra.core.Name("except")
    EXCEPT_STAR = hydra.core.Name("exceptStar")

@dataclass(frozen=True)
class TryFinallyStatement:
    body: Block
    finally_: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TryFinallyStatement")
    BODY = hydra.core.Name("body")
    FINALLY = hydra.core.Name("finally")

@dataclass(frozen=True)
class TryExceptStatement:
    body: Block
    excepts: frozenlist[ExceptBlock]
    else_: Maybe[Block]
    finally_: Maybe[Block]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TryExceptStatement")
    BODY = hydra.core.Name("body")
    EXCEPTS = hydra.core.Name("excepts")
    ELSE = hydra.core.Name("else")
    FINALLY = hydra.core.Name("finally")

@dataclass(frozen=True)
class TryExceptStarStatement:
    body: Block
    excepts: frozenlist[ExceptStarBlock]
    else_: Maybe[Block]
    finally_: Maybe[Block]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TryExceptStarStatement")
    BODY = hydra.core.Name("body")
    EXCEPTS = hydra.core.Name("excepts")
    ELSE = hydra.core.Name("else")
    FINALLY = hydra.core.Name("finally")

@dataclass(frozen=True)
class ExceptBlock:
    expression: Maybe[ExceptExpression]
    body: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ExceptBlock")
    EXPRESSION = hydra.core.Name("expression")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class ExceptExpression:
    expression: Expression
    as_: Maybe[Name]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ExceptExpression")
    EXPRESSION = hydra.core.Name("expression")
    AS = hydra.core.Name("as")

@dataclass(frozen=True)
class ExceptStarBlock:
    expression: Expression
    as_: Maybe[Name]
    body: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ExceptStarBlock")
    EXPRESSION = hydra.core.Name("expression")
    AS = hydra.core.Name("as")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class MatchStatement:
    subject: SubjectExpression
    cases: frozenlist[CaseBlock]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.MatchStatement")
    SUBJECT = hydra.core.Name("subject")
    CASES = hydra.core.Name("cases")

class SubjectExpressionTuple(Node["frozenlist[StarNamedExpression]"]):
    ...

class SubjectExpressionSimple(Node["NamedExpression"]):
    ...

class _SubjectExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class SubjectExpression(metaclass=_SubjectExpressionMeta):
    r"""SubjectExpressionTuple | SubjectExpressionSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SubjectExpression")
    TUPLE = hydra.core.Name("tuple")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class CaseBlock:
    patterns: Patterns
    guard: Maybe[Guard]
    body: Block
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.CaseBlock")
    PATTERNS = hydra.core.Name("patterns")
    GUARD = hydra.core.Name("guard")
    BODY = hydra.core.Name("body")

class Guard(Node["NamedExpression"]):
    ...

Guard.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Guard")

class PatternsSequence(Node["OpenSequencePattern"]):
    ...

class PatternsPattern(Node["Pattern"]):
    ...

class _PatternsMeta(type):
    def __getitem__(cls, item):
        return object

class Patterns(metaclass=_PatternsMeta):
    r"""PatternsSequence | PatternsPattern"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Patterns")
    SEQUENCE = hydra.core.Name("sequence")
    PATTERN = hydra.core.Name("pattern")

class PatternAs(Node["AsPattern"]):
    ...

class PatternOr(Node["OrPattern"]):
    ...

class _PatternMeta(type):
    def __getitem__(cls, item):
        return object

class Pattern(metaclass=_PatternMeta):
    r"""PatternAs | PatternOr"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Pattern")
    AS = hydra.core.Name("as")
    OR = hydra.core.Name("or")

@dataclass(frozen=True)
class AsPattern:
    pattern: OrPattern
    as_: PatternCaptureTarget
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AsPattern")
    PATTERN = hydra.core.Name("pattern")
    AS = hydra.core.Name("as")

class OrPattern(Node["frozenlist[ClosedPattern]"]):
    ...

OrPattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.OrPattern")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ClosedPattern")
    LITERAL = hydra.core.Name("literal")
    CAPTURE = hydra.core.Name("capture")
    WILDCARD = hydra.core.Name("wildcard")
    VALUE = hydra.core.Name("value")
    GROUP = hydra.core.Name("group")
    SEQUENCE = hydra.core.Name("sequence")
    MAPPING = hydra.core.Name("mapping")
    CLASS = hydra.core.Name("class")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LiteralExpression")
    NUMBER = hydra.core.Name("number")
    COMPLEX = hydra.core.Name("complex")
    STRING = hydra.core.Name("string")
    NONE = hydra.core.Name("none")
    TRUE = hydra.core.Name("true")
    FALSE = hydra.core.Name("false")

@dataclass(frozen=True)
class ComplexNumber:
    real: SignedRealNumber
    plus_or_minus: PlusOrMinus
    imaginary: ImaginaryNumber
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ComplexNumber")
    REAL = hydra.core.Name("real")
    PLUS_OR_MINUS = hydra.core.Name("plusOrMinus")
    IMAGINARY = hydra.core.Name("imaginary")

class PlusOrMinus(Enum):
    PLUS = hydra.core.Name("plus")
    
    MINUS = hydra.core.Name("minus")

PlusOrMinus.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.PlusOrMinus")

class SignedNumberSign(Node["PlusOrMinus"]):
    ...

class SignedNumberNumber(Node["Number"]):
    ...

class _SignedNumberMeta(type):
    def __getitem__(cls, item):
        return object

class SignedNumber(metaclass=_SignedNumberMeta):
    r"""SignedNumberSign | SignedNumberNumber"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SignedNumber")
    SIGN = hydra.core.Name("sign")
    NUMBER = hydra.core.Name("number")

class SignedRealNumberSign(Node["PlusOrMinus"]):
    ...

class SignedRealNumberNumber(Node["RealNumber"]):
    ...

class _SignedRealNumberMeta(type):
    def __getitem__(cls, item):
        return object

class SignedRealNumber(metaclass=_SignedRealNumberMeta):
    r"""SignedRealNumberSign | SignedRealNumberNumber"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SignedRealNumber")
    SIGN = hydra.core.Name("sign")
    NUMBER = hydra.core.Name("number")

class RealNumber(Node["Number"]):
    ...

RealNumber.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.RealNumber")

class ImaginaryNumber(Node["Number"]):
    ...

ImaginaryNumber.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ImaginaryNumber")

class CapturePattern(Node["PatternCaptureTarget"]):
    ...

CapturePattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.CapturePattern")

class PatternCaptureTarget(Node["Name"]):
    ...

PatternCaptureTarget.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.PatternCaptureTarget")

class ValuePattern(Node["Attribute"]):
    ...

ValuePattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ValuePattern")

class Attribute(Node["frozenlist[Name]"]):
    ...

Attribute.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Attribute")

class NameOrAttribute(Node["frozenlist[Name]"]):
    ...

NameOrAttribute.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.NameOrAttribute")

class GroupPattern(Node["Pattern"]):
    ...

GroupPattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.GroupPattern")

class SequencePatternList(Node["Maybe[MaybeSequencePattern]"]):
    ...

class SequencePatternTuple(Node["Maybe[OpenSequencePattern]"]):
    ...

class _SequencePatternMeta(type):
    def __getitem__(cls, item):
        return object

class SequencePattern(metaclass=_SequencePatternMeta):
    r"""SequencePatternList | SequencePatternTuple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SequencePattern")
    LIST = hydra.core.Name("list")
    TUPLE = hydra.core.Name("tuple")

@dataclass(frozen=True)
class OpenSequencePattern:
    head: MaybeStarPattern
    tail: Maybe[MaybeSequencePattern]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.OpenSequencePattern")
    HEAD = hydra.core.Name("head")
    TAIL = hydra.core.Name("tail")

class MaybeSequencePattern(Node["frozenlist[MaybeStarPattern]"]):
    ...

MaybeSequencePattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.MaybeSequencePattern")

class MaybeStarPatternStar(Node["StarPattern"]):
    ...

class MaybeStarPatternPattern(Node["Pattern"]):
    ...

class _MaybeStarPatternMeta(type):
    def __getitem__(cls, item):
        return object

class MaybeStarPattern(metaclass=_MaybeStarPatternMeta):
    r"""MaybeStarPatternStar | MaybeStarPatternPattern"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.MaybeStarPattern")
    STAR = hydra.core.Name("star")
    PATTERN = hydra.core.Name("pattern")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarPattern")
    CAPTURE = hydra.core.Name("capture")
    WILDCARD = hydra.core.Name("wildcard")

@dataclass(frozen=True)
class MappingPattern:
    items: Maybe[ItemsPattern]
    double_star: Maybe[DoubleStarPattern]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.MappingPattern")
    ITEMS = hydra.core.Name("items")
    DOUBLE_STAR = hydra.core.Name("doubleStar")

class ItemsPattern(Node["frozenlist[KeyValuePattern]"]):
    ...

ItemsPattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ItemsPattern")

@dataclass(frozen=True)
class KeyValuePattern:
    key: LiteralExpressionOrAttribute
    value: Pattern
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.KeyValuePattern")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class LiteralExpressionOrAttributeLiteral(Node["LiteralExpression"]):
    ...

class LiteralExpressionOrAttributeAttribute(Node["Attribute"]):
    ...

class _LiteralExpressionOrAttributeMeta(type):
    def __getitem__(cls, item):
        return object

class LiteralExpressionOrAttribute(metaclass=_LiteralExpressionOrAttributeMeta):
    r"""LiteralExpressionOrAttributeLiteral | LiteralExpressionOrAttributeAttribute"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LiteralExpressionOrAttribute")
    LITERAL = hydra.core.Name("literal")
    ATTRIBUTE = hydra.core.Name("attribute")

class DoubleStarPattern(Node["PatternCaptureTarget"]):
    ...

DoubleStarPattern.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DoubleStarPattern")

@dataclass(frozen=True)
class ClassPattern:
    name_or_attribute: NameOrAttribute
    positional_patterns: Maybe[PositionalPatterns]
    keyword_patterns: Maybe[KeywordPatterns]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ClassPattern")
    NAME_OR_ATTRIBUTE = hydra.core.Name("nameOrAttribute")
    POSITIONAL_PATTERNS = hydra.core.Name("positionalPatterns")
    KEYWORD_PATTERNS = hydra.core.Name("keywordPatterns")

class PositionalPatterns(Node["frozenlist[Pattern]"]):
    ...

PositionalPatterns.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.PositionalPatterns")

class KeywordPatterns(Node["frozenlist[KeywordPattern]"]):
    ...

KeywordPatterns.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.KeywordPatterns")

@dataclass(frozen=True)
class KeywordPattern:
    name: Name
    pattern: Pattern
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.KeywordPattern")
    NAME = hydra.core.Name("name")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class TypeAlias:
    name: Name
    type_params: frozenlist[TypeParameter]
    expression: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TypeAlias")
    NAME = hydra.core.Name("name")
    TYPE_PARAMS = hydra.core.Name("typeParams")
    EXPRESSION = hydra.core.Name("expression")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TypeParameter")
    SIMPLE = hydra.core.Name("simple")
    STAR = hydra.core.Name("star")
    DOUBLE_STAR = hydra.core.Name("doubleStar")

@dataclass(frozen=True)
class SimpleTypeParameter:
    name: Name
    bound: Maybe[Expression]
    default: Maybe[Expression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SimpleTypeParameter")
    NAME = hydra.core.Name("name")
    BOUND = hydra.core.Name("bound")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class StarTypeParameter:
    name: Name
    default: Maybe[StarExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarTypeParameter")
    NAME = hydra.core.Name("name")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class DoubleStarTypeParameter:
    name: Name
    default: Maybe[Expression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DoubleStarTypeParameter")
    NAME = hydra.core.Name("name")
    DEFAULT = hydra.core.Name("default")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Expression")
    CONDITIONAL = hydra.core.Name("conditional")
    SIMPLE = hydra.core.Name("simple")
    LAMBDA = hydra.core.Name("lambda")

@dataclass(frozen=True)
class Conditional:
    body: Disjunction
    if_: Disjunction
    else_: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Conditional")
    BODY = hydra.core.Name("body")
    IF = hydra.core.Name("if")
    ELSE = hydra.core.Name("else")

class YieldExpressionFrom(Node["Expression"]):
    ...

class YieldExpressionSimple(Node["frozenlist[StarExpression]"]):
    ...

class _YieldExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class YieldExpression(metaclass=_YieldExpressionMeta):
    r"""YieldExpressionFrom | YieldExpressionSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.YieldExpression")
    FROM = hydra.core.Name("from")
    SIMPLE = hydra.core.Name("simple")

class StarExpressionStar(Node["BitwiseOr"]):
    ...

class StarExpressionSimple(Node["Expression"]):
    ...

class _StarExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class StarExpression(metaclass=_StarExpressionMeta):
    r"""StarExpressionStar | StarExpressionSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarExpression")
    STAR = hydra.core.Name("star")
    SIMPLE = hydra.core.Name("simple")

class StarNamedExpressions(Node["frozenlist[StarNamedExpression]"]):
    ...

StarNamedExpressions.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarNamedExpressions")

class StarNamedExpressionStar(Node["BitwiseOr"]):
    ...

class StarNamedExpressionSimple(Node["NamedExpression"]):
    ...

class _StarNamedExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class StarNamedExpression(metaclass=_StarNamedExpressionMeta):
    r"""StarNamedExpressionStar | StarNamedExpressionSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarNamedExpression")
    STAR = hydra.core.Name("star")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class AssignmentExpression:
    name: Name
    expression: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AssignmentExpression")
    NAME = hydra.core.Name("name")
    EXPRESSION = hydra.core.Name("expression")

class NamedExpressionAssignment(Node["AssignmentExpression"]):
    ...

class NamedExpressionSimple(Node["Expression"]):
    ...

class _NamedExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class NamedExpression(metaclass=_NamedExpressionMeta):
    r"""NamedExpressionAssignment | NamedExpressionSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.NamedExpression")
    ASSIGNMENT = hydra.core.Name("assignment")
    SIMPLE = hydra.core.Name("simple")

class Disjunction(Node["frozenlist[Conjunction]"]):
    ...

Disjunction.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Disjunction")

class Conjunction(Node["frozenlist[Inversion]"]):
    ...

Conjunction.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Conjunction")

class InversionNot(Node["Inversion"]):
    ...

class InversionSimple(Node["Comparison"]):
    ...

class _InversionMeta(type):
    def __getitem__(cls, item):
        return object

class Inversion(metaclass=_InversionMeta):
    r"""InversionNot | InversionSimple"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Inversion")
    NOT = hydra.core.Name("not")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class Comparison:
    lhs: BitwiseOr
    rhs: frozenlist[CompareOpBitwiseOrPair]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Comparison")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class CompareOpBitwiseOrPair:
    operator: CompareOp
    rhs: BitwiseOr
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.CompareOpBitwiseOrPair")
    OPERATOR = hydra.core.Name("operator")
    RHS = hydra.core.Name("rhs")

class CompareOp(Enum):
    EQ = hydra.core.Name("eq")
    
    NOTEQ = hydra.core.Name("noteq")
    
    LTE = hydra.core.Name("lte")
    
    LT = hydra.core.Name("lt")
    
    GTE = hydra.core.Name("gte")
    
    GT = hydra.core.Name("gt")
    
    NOTIN = hydra.core.Name("notin")
    
    IN = hydra.core.Name("in")
    
    ISNOT = hydra.core.Name("isnot")
    
    IS = hydra.core.Name("is")

CompareOp.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.CompareOp")

@dataclass(frozen=True)
class BitwiseOr:
    lhs: Maybe[BitwiseOr]
    rhs: BitwiseXor
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.BitwiseOr")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class BitwiseXor:
    lhs: Maybe[BitwiseXor]
    rhs: BitwiseAnd
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.BitwiseXor")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class BitwiseAnd:
    lhs: Maybe[BitwiseAnd]
    rhs: ShiftExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.BitwiseAnd")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class ShiftExpression:
    lhs: Maybe[ShiftLhs]
    rhs: Sum
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ShiftExpression")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class ShiftLhs:
    operand: ShiftExpression
    operator: ShiftOp
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ShiftLhs")
    OPERAND = hydra.core.Name("operand")
    OPERATOR = hydra.core.Name("operator")

class ShiftOp(Enum):
    LEFT = hydra.core.Name("left")
    
    RIGHT = hydra.core.Name("right")

ShiftOp.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ShiftOp")

@dataclass(frozen=True)
class Sum:
    lhs: Maybe[SumLhs]
    rhs: Term
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Sum")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class SumLhs:
    operand: Sum
    operator: SumOp
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SumLhs")
    OPERAND = hydra.core.Name("operand")
    OPERATOR = hydra.core.Name("operator")

class SumOp(Enum):
    ADD = hydra.core.Name("add")
    
    SUB = hydra.core.Name("sub")

SumOp.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SumOp")

@dataclass(frozen=True)
class Term:
    lhs: Maybe[TermLhs]
    rhs: Factor
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Term")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class TermLhs:
    operand: Term
    operator: TermOp
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TermLhs")
    OPERAND = hydra.core.Name("operand")
    OPERATOR = hydra.core.Name("operator")

class TermOp(Enum):
    MUL = hydra.core.Name("mul")
    
    DIV = hydra.core.Name("div")
    
    FLOORDIV = hydra.core.Name("floordiv")
    
    MOD = hydra.core.Name("mod")
    
    MATMUL = hydra.core.Name("matmul")

TermOp.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TermOp")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Factor")
    POSITIVE = hydra.core.Name("positive")
    NEGATIVE = hydra.core.Name("negative")
    COMPLEMENT = hydra.core.Name("complement")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class Power:
    lhs: AwaitPrimary
    rhs: Maybe[Factor]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Power")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class AwaitPrimary:
    await_: bool
    primary: Primary
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.AwaitPrimary")
    AWAIT = hydra.core.Name("await")
    PRIMARY = hydra.core.Name("primary")

class PrimarySimple(Node["Atom"]):
    ...

class PrimaryCompound(Node["PrimaryWithRhs"]):
    ...

class _PrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class Primary(metaclass=_PrimaryMeta):
    r"""PrimarySimple | PrimaryCompound"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Primary")
    SIMPLE = hydra.core.Name("simple")
    COMPOUND = hydra.core.Name("compound")

@dataclass(frozen=True)
class PrimaryWithRhs:
    primary: Primary
    rhs: PrimaryRhs
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.PrimaryWithRhs")
    PRIMARY = hydra.core.Name("primary")
    RHS = hydra.core.Name("rhs")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.PrimaryRhs")
    PROJECT = hydra.core.Name("project")
    GENEXP = hydra.core.Name("genexp")
    CALL = hydra.core.Name("call")
    SLICES = hydra.core.Name("slices")

@dataclass(frozen=True)
class Slices:
    head: Slice
    tail: frozenlist[SliceOrStarredExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Slices")
    HEAD = hydra.core.Name("head")
    TAIL = hydra.core.Name("tail")

class SliceOrStarredExpressionSlice(Node["Slice"]):
    ...

class SliceOrStarredExpressionStarred(Node["StarredExpression"]):
    ...

class _SliceOrStarredExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class SliceOrStarredExpression(metaclass=_SliceOrStarredExpressionMeta):
    r"""SliceOrStarredExpressionSlice | SliceOrStarredExpressionStarred"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SliceOrStarredExpression")
    SLICE = hydra.core.Name("slice")
    STARRED = hydra.core.Name("starred")

class SliceNamed(Node["NamedExpression"]):
    ...

class SliceSlice_(Node["SliceExpression"]):
    ...

class _SliceMeta(type):
    def __getitem__(cls, item):
        return object

class Slice(metaclass=_SliceMeta):
    r"""SliceNamed | SliceSlice_"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Slice")
    NAMED = hydra.core.Name("named")
    SLICE_ = hydra.core.Name("slice_")

@dataclass(frozen=True)
class SliceExpression:
    start: Maybe[Expression]
    stop: Maybe[Expression]
    step: Maybe[Expression]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SliceExpression")
    START = hydra.core.Name("start")
    STOP = hydra.core.Name("stop")
    STEP = hydra.core.Name("step")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Atom")
    NAME = hydra.core.Name("name")
    TRUE = hydra.core.Name("true")
    FALSE = hydra.core.Name("false")
    NONE = hydra.core.Name("none")
    STRING = hydra.core.Name("string")
    NUMBER = hydra.core.Name("number")
    TUPLE = hydra.core.Name("tuple")
    GROUP = hydra.core.Name("group")
    GENEXP = hydra.core.Name("genexp")
    LIST = hydra.core.Name("list")
    LISTCOMP = hydra.core.Name("listcomp")
    DICT = hydra.core.Name("dict")
    SET = hydra.core.Name("set")
    DICTCOMP = hydra.core.Name("dictcomp")
    SETCOMP = hydra.core.Name("setcomp")
    ELLIPSIS = hydra.core.Name("ellipsis")

class GroupYield(Node["YieldExpression"]):
    ...

class GroupExpression(Node["NamedExpression"]):
    ...

class _GroupMeta(type):
    def __getitem__(cls, item):
        return object

class Group(metaclass=_GroupMeta):
    r"""GroupYield | GroupExpression"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Group")
    YIELD = hydra.core.Name("yield")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class Lambda:
    params: LambdaParameters
    body: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Lambda")
    PARAMS = hydra.core.Name("params")
    BODY = hydra.core.Name("body")

@dataclass(frozen=True)
class LambdaParameters:
    slash_no_default: Maybe[LambdaSlashNoDefault]
    param_no_default: frozenlist[LambdaParamNoDefault]
    param_with_default: frozenlist[LambdaParamWithDefault]
    star_etc: Maybe[LambdaStarEtc]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaParameters")
    SLASH_NO_DEFAULT = hydra.core.Name("slashNoDefault")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")
    STAR_ETC = hydra.core.Name("starEtc")

@dataclass(frozen=True)
class LambdaSlashNoDefault:
    parameters: frozenlist[LambdaParamNoDefault]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaSlashNoDefault")
    PARAMETERS = hydra.core.Name("parameters")

@dataclass(frozen=True)
class LambdaSlashWithDefault:
    param_no_default: frozenlist[LambdaParamNoDefault]
    param_with_default: frozenlist[LambdaParamWithDefault]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaSlashWithDefault")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_WITH_DEFAULT = hydra.core.Name("paramWithDefault")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaStarEtc")
    STAR = hydra.core.Name("star")
    PARAM_NO_DEFAULT = hydra.core.Name("paramNoDefault")
    PARAM_MAYBE_DEFAULT = hydra.core.Name("paramMaybeDefault")
    KWDS = hydra.core.Name("kwds")

class LambdaKwds(Node["LambdaParamNoDefault"]):
    ...

LambdaKwds.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaKwds")

class LambdaParamNoDefault(Node["Name"]):
    ...

LambdaParamNoDefault.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaParamNoDefault")

@dataclass(frozen=True)
class LambdaParamWithDefault:
    param: Name
    default: Maybe[Default]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaParamWithDefault")
    PARAM = hydra.core.Name("param")
    DEFAULT = hydra.core.Name("default")

@dataclass(frozen=True)
class LambdaParamMaybeDefault:
    param: Name
    default: Maybe[Default]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.LambdaParamMaybeDefault")
    PARAM = hydra.core.Name("param")
    DEFAULT = hydra.core.Name("default")

class List(Node["frozenlist[StarNamedExpression]"]):
    ...

List.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.List")

class Tuple(Node["frozenlist[StarNamedExpression]"]):
    ...

Tuple.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Tuple")

class Set(Node["frozenlist[StarNamedExpression]"]):
    ...

Set.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Set")

class Dict(Node["frozenlist[DoubleStarredKvpair]"]):
    ...

Dict.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Dict")

class DoubleStarredKvpairStarred(Node["BitwiseOr"]):
    ...

class DoubleStarredKvpairPair(Node["Kvpair"]):
    ...

class _DoubleStarredKvpairMeta(type):
    def __getitem__(cls, item):
        return object

class DoubleStarredKvpair(metaclass=_DoubleStarredKvpairMeta):
    r"""DoubleStarredKvpairStarred | DoubleStarredKvpairPair"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DoubleStarredKvpair")
    STARRED = hydra.core.Name("starred")
    PAIR = hydra.core.Name("pair")

@dataclass(frozen=True)
class Kvpair:
    key: Expression
    value: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Kvpair")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class ForIfClauses(Node["frozenlist[ForIfClause]"]):
    ...

ForIfClauses.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ForIfClauses")

@dataclass(frozen=True)
class ForIfClause:
    async_: bool
    targets: frozenlist[StarTarget]
    in_: Disjunction
    ifs: frozenlist[Disjunction]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.ForIfClause")
    ASYNC = hydra.core.Name("async")
    TARGETS = hydra.core.Name("targets")
    IN = hydra.core.Name("in")
    IFS = hydra.core.Name("ifs")

@dataclass(frozen=True)
class Listcomp:
    expression: NamedExpression
    for_if_clauses: ForIfClauses
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Listcomp")
    EXPRESSION = hydra.core.Name("expression")
    FOR_IF_CLAUSES = hydra.core.Name("forIfClauses")

@dataclass(frozen=True)
class Setcomp:
    expression: NamedExpression
    for_if_clauses: ForIfClauses
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Setcomp")
    EXPRESSION = hydra.core.Name("expression")
    FOR_IF_CLAUSES = hydra.core.Name("forIfClauses")

@dataclass(frozen=True)
class Genexp:
    head: GenexpHead
    tail: ForIfClauses
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Genexp")
    HEAD = hydra.core.Name("head")
    TAIL = hydra.core.Name("tail")

class GenexpHeadAssignment(Node["AssignmentExpression"]):
    ...

class GenexpHeadExpression(Node["Expression"]):
    ...

class _GenexpHeadMeta(type):
    def __getitem__(cls, item):
        return object

class GenexpHead(metaclass=_GenexpHeadMeta):
    r"""GenexpHeadAssignment | GenexpHeadExpression"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.GenexpHead")
    ASSIGNMENT = hydra.core.Name("assignment")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class Dictcomp:
    kvpair: Kvpair
    for_if_clauses: ForIfClauses
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Dictcomp")
    KVPAIR = hydra.core.Name("kvpair")
    FOR_IF_CLAUSES = hydra.core.Name("forIfClauses")

@dataclass(frozen=True)
class Args:
    positional: frozenlist[PosArg]
    kwarg_or_starred: frozenlist[KwargOrStarred]
    kwarg_or_double_starred: frozenlist[KwargOrDoubleStarred]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Args")
    POSITIONAL = hydra.core.Name("positional")
    KWARG_OR_STARRED = hydra.core.Name("kwargOrStarred")
    KWARG_OR_DOUBLE_STARRED = hydra.core.Name("kwargOrDoubleStarred")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.PosArg")
    STARRED = hydra.core.Name("starred")
    ASSIGNMENT = hydra.core.Name("assignment")
    EXPRESSION = hydra.core.Name("expression")

class StarredExpression(Node["Expression"]):
    ...

StarredExpression.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarredExpression")

class KwargOrStarredKwarg(Node["Kwarg"]):
    ...

class KwargOrStarredStarred(Node["StarredExpression"]):
    ...

class _KwargOrStarredMeta(type):
    def __getitem__(cls, item):
        return object

class KwargOrStarred(metaclass=_KwargOrStarredMeta):
    r"""KwargOrStarredKwarg | KwargOrStarredStarred"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.KwargOrStarred")
    KWARG = hydra.core.Name("kwarg")
    STARRED = hydra.core.Name("starred")

@dataclass(frozen=True)
class Kwarg:
    name: Name
    value: Expression
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.Kwarg")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class KwargOrDoubleStarredKwarg(Node["Kwarg"]):
    ...

class KwargOrDoubleStarredDoubleStarred(Node["Expression"]):
    ...

class _KwargOrDoubleStarredMeta(type):
    def __getitem__(cls, item):
        return object

class KwargOrDoubleStarred(metaclass=_KwargOrDoubleStarredMeta):
    r"""KwargOrDoubleStarredKwarg | KwargOrDoubleStarredDoubleStarred"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.KwargOrDoubleStarred")
    KWARG = hydra.core.Name("kwarg")
    DOUBLE_STARRED = hydra.core.Name("doubleStarred")

class StarTargetsListSeq(Node["frozenlist[StarTarget]"]):
    ...

StarTargetsListSeq.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarTargetsListSeq")

class StarTargetsTupleSeq(Node["frozenlist[StarTarget]"]):
    ...

StarTargetsTupleSeq.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarTargetsTupleSeq")

class StarTargetStarred(Node["StarTarget"]):
    ...

class StarTargetUnstarred(Node["TargetWithStarAtom"]):
    ...

class _StarTargetMeta(type):
    def __getitem__(cls, item):
        return object

class StarTarget(metaclass=_StarTargetMeta):
    r"""StarTargetStarred | StarTargetUnstarred"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarTarget")
    STARRED = hydra.core.Name("starred")
    UNSTARRED = hydra.core.Name("unstarred")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TargetWithStarAtom")
    PROJECT = hydra.core.Name("project")
    SLICES = hydra.core.Name("slices")
    ATOM = hydra.core.Name("atom")

@dataclass(frozen=True)
class TPrimaryAndName:
    primary: TPrimary
    name: Name
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndName")
    PRIMARY = hydra.core.Name("primary")
    NAME = hydra.core.Name("name")

@dataclass(frozen=True)
class TPrimaryAndSlices:
    primary: TPrimary
    slices: Slices
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndSlices")
    PRIMARY = hydra.core.Name("primary")
    SLICES = hydra.core.Name("slices")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.StarAtom")
    NAME = hydra.core.Name("name")
    TARGET_WITH_STAR_ATOM = hydra.core.Name("targetWithStarAtom")
    STAR_TARGETS_TUPLE_SEQ = hydra.core.Name("starTargetsTupleSeq")
    STAR_TARGETS_LIST_SEQ = hydra.core.Name("starTargetsListSeq")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SingleTarget")
    SUBSCRIPT_ATTRIBUTE_TARGET = hydra.core.Name("subscriptAttributeTarget")
    NAME = hydra.core.Name("name")
    PARENS = hydra.core.Name("parens")

class SingleSubscriptAttributeTargetPrimaryAndName(Node["TPrimaryAndName"]):
    ...

class SingleSubscriptAttributeTargetPrimaryAndSlices(Node["TPrimaryAndSlices"]):
    ...

class _SingleSubscriptAttributeTargetMeta(type):
    def __getitem__(cls, item):
        return object

class SingleSubscriptAttributeTarget(metaclass=_SingleSubscriptAttributeTargetMeta):
    r"""SingleSubscriptAttributeTargetPrimaryAndName | SingleSubscriptAttributeTargetPrimaryAndSlices"""
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.SingleSubscriptAttributeTarget")
    PRIMARY_AND_NAME = hydra.core.Name("primaryAndName")
    PRIMARY_AND_SLICES = hydra.core.Name("primaryAndSlices")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TPrimary")
    PRIMARY_AND_NAME = hydra.core.Name("primaryAndName")
    PRIMARY_AND_SLICES = hydra.core.Name("primaryAndSlices")
    PRIMARY_AND_GENEXP = hydra.core.Name("primaryAndGenexp")
    PRIMARY_AND_ARGUMENTS = hydra.core.Name("primaryAndArguments")
    ATOM = hydra.core.Name("atom")

@dataclass(frozen=True)
class TPrimaryAndGenexp:
    primary: TPrimary
    genexp: Genexp
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndGenexp")
    PRIMARY = hydra.core.Name("primary")
    GENEXP = hydra.core.Name("genexp")

@dataclass(frozen=True)
class TPrimaryAndArguments:
    primary: TPrimary
    arguments: Maybe[Args]
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TPrimaryAndArguments")
    PRIMARY = hydra.core.Name("primary")
    ARGUMENTS = hydra.core.Name("arguments")

class DelTargets(Node["frozenlist[DelTarget]"]):
    ...

DelTargets.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DelTargets")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DelTarget")
    PRIMARY_AND_NAME = hydra.core.Name("primaryAndName")
    PRIMARY_AND_SLICES = hydra.core.Name("primaryAndSlices")
    DEL_T_ATOM = hydra.core.Name("delTAtom")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.DelTAtom")
    NAME = hydra.core.Name("name")
    TARGET = hydra.core.Name("target")
    TARGETS = hydra.core.Name("targets")

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
    
    TYPE_ = hydra.core.Name("hydra.ext.python.syntax.TypeExpression")
    EXPRESSION = hydra.core.Name("expression")
    STARRED_EXPRESSION = hydra.core.Name("starredExpression")
    DOUBLE_STARRED_EXPRESSION = hydra.core.Name("doubleStarredExpression")

class FuncTypeComment(Node["TypeComment"]):
    ...

FuncTypeComment.TYPE_ = hydra.core.Name("hydra.ext.python.syntax.FuncTypeComment")
