# Note: this is an automatically generated file. Do not edit.

r"""A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class BetweenExpression:
    not_: bool
    expression: Expression
    lower_bound: Expression
    upper_bound: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.BetweenExpression")
    NOT = hydra.core.Name("not")
    EXPRESSION = hydra.core.Name("expression")
    LOWER_BOUND = hydra.core.Name("lowerBound")
    UPPER_BOUND = hydra.core.Name("upperBound")

@dataclass(frozen=True)
class BinaryExpression:
    left: Expression
    operator: BinaryOperator
    right: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.BinaryExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class BinaryOperator(Enum):
    CASE_INSENSITIVE_EQUAL = hydra.core.Name("caseInsensitiveEqual")

    CONTAINS = hydra.core.Name("contains")

    DIVIDE = hydra.core.Name("divide")

    ENDS_WITH = hydra.core.Name("endsWith")

    EQUAL = hydra.core.Name("equal")

    GREATER = hydra.core.Name("greater")

    GREATER_OR_EQUAL = hydra.core.Name("greaterOrEqual")

    HAS = hydra.core.Name("has")

    HAS_PREFIX = hydra.core.Name("hasPrefix")

    HAS_SUFFIX = hydra.core.Name("hasSuffix")

    LESS = hydra.core.Name("less")

    LESS_OR_EQUAL = hydra.core.Name("lessOrEqual")

    MATCHES_REGEX = hydra.core.Name("matchesRegex")

    MINUS = hydra.core.Name("minus")

    NOT_EQUAL = hydra.core.Name("notEqual")

    PLUS = hydra.core.Name("plus")

    STARTS_WITH = hydra.core.Name("startsWith")

    TIMES = hydra.core.Name("times")

BinaryOperator.TYPE_ = hydra.core.Name("hydra.kusto.kql.BinaryOperator")

class BuiltInFunction(Enum):
    AGO = hydra.core.Name("ago")

    BIN = hydra.core.Name("bin")

    COUNT = hydra.core.Name("count")

    DCOUNT = hydra.core.Name("dcount")

    ENDOFDAY = hydra.core.Name("endofday")

    EXTRACT = hydra.core.Name("extract")

    FORMAT_DATETIME = hydra.core.Name("format_datetime")

    MATERIALIZE = hydra.core.Name("materialize")

    NOW = hydra.core.Name("now")

    RANGE = hydra.core.Name("range")

    STARTOFDAY = hydra.core.Name("startofday")

    STRCAT = hydra.core.Name("strcat")

    TODYNAMIC = hydra.core.Name("todynamic")

BuiltInFunction.TYPE_ = hydra.core.Name("hydra.kusto.kql.BuiltInFunction")

@dataclass(frozen=True)
class ColumnAlias:
    column: ColumnName
    alias: ColumnName

    TYPE_ = hydra.core.Name("hydra.kusto.kql.ColumnAlias")
    COLUMN = hydra.core.Name("column")
    ALIAS = hydra.core.Name("alias")

@dataclass(frozen=True)
class ColumnAssignment:
    column: ColumnName
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.ColumnAssignment")
    COLUMN = hydra.core.Name("column")
    EXPRESSION = hydra.core.Name("expression")

class ColumnName(Node[str]):
    ...

ColumnName.TYPE_ = hydra.core.Name("hydra.kusto.kql.ColumnName")

class ColumnsAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ColumnsAll)
    def __hash__(self):
        return hash("ColumnsAll")

class ColumnsSingle(Node["ColumnName"]):
    ...

class _ColumnsMeta(type):
    def __getitem__(cls, item):
        return object

class Columns(metaclass=_ColumnsMeta):
    r"""ColumnsAll | ColumnsSingle"""

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Columns")
    ALL = hydra.core.Name("all")
    SINGLE = hydra.core.Name("single")

class CommandCount:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CommandCount)
    def __hash__(self):
        return hash("CommandCount")

class CommandDistinct(Node["frozenlist[ColumnName]"]):
    r"""See https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/distinct-operator"""

class CommandExtend(Node["frozenlist[ColumnAssignment]"]):
    ...

class CommandJoin(Node["JoinCommand"]):
    ...

class CommandLimit(Node[int]):
    ...

class CommandMvexpand(Node["ColumnName"]):
    ...

class CommandOrderBy(Node["frozenlist[SortBy]"]):
    ...

class CommandParse(Node["ParseCommand"]):
    ...

class CommandPrint(Node["PrintCommand"]):
    ...

class CommandProject(Node["frozenlist[Projection]"]):
    ...

class CommandProjectAway(Node["frozenlist[ColumnName]"]):
    ...

class CommandProjectRename(Node["frozenlist[ColumnAlias]"]):
    ...

class CommandRender(Node[str]):
    ...

class CommandSearch(Node["SearchCommand"]):
    ...

class CommandSortBy(Node["frozenlist[SortBy]"]):
    ...

class CommandSummarize(Node["SummarizeCommand"]):
    ...

class CommandTake(Node[int]):
    r"""Limit a search to a specified number of results"""

class CommandTop(Node["TopCommand"]):
    ...

class CommandUnion(Node["UnionCommand"]):
    ...

class CommandWhere(Node["Expression"]):
    ...

class _CommandMeta(type):
    def __getitem__(cls, item):
        return object

class Command(metaclass=_CommandMeta):
    r"""CommandCount | CommandDistinct | CommandExtend | CommandJoin | CommandLimit | CommandMvexpand | CommandOrderBy | CommandParse | CommandPrint | CommandProject | CommandProjectAway | CommandProjectRename | CommandRender | CommandSearch | CommandSortBy | CommandSummarize | CommandTake | CommandTop | CommandUnion | CommandWhere"""

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Command")
    COUNT = hydra.core.Name("count")
    DISTINCT = hydra.core.Name("distinct")
    EXTEND = hydra.core.Name("extend")
    JOIN = hydra.core.Name("join")
    LIMIT = hydra.core.Name("limit")
    MVEXPAND = hydra.core.Name("mvexpand")
    ORDER_BY = hydra.core.Name("orderBy")
    PARSE = hydra.core.Name("parse")
    PRINT = hydra.core.Name("print")
    PROJECT = hydra.core.Name("project")
    PROJECT_AWAY = hydra.core.Name("projectAway")
    PROJECT_RENAME = hydra.core.Name("projectRename")
    RENDER = hydra.core.Name("render")
    SEARCH = hydra.core.Name("search")
    SORT_BY = hydra.core.Name("sortBy")
    SUMMARIZE = hydra.core.Name("summarize")
    TAKE = hydra.core.Name("take")
    TOP = hydra.core.Name("top")
    UNION = hydra.core.Name("union")
    WHERE = hydra.core.Name("where")

class Datetime(Node[str]):
    ...

Datetime.TYPE_ = hydra.core.Name("hydra.kusto.kql.Datetime")

@dataclass(frozen=True)
class Duration:
    value: int
    unit: DurationUnit

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Duration")
    VALUE = hydra.core.Name("value")
    UNIT = hydra.core.Name("unit")

class DurationUnit(Enum):
    SECOND = hydra.core.Name("second")

    MINUTE = hydra.core.Name("minute")

    HOUR = hydra.core.Name("hour")

DurationUnit.TYPE_ = hydra.core.Name("hydra.kusto.kql.DurationUnit")

class ExpressionAnd(Node["frozenlist[Expression]"]):
    ...

class ExpressionAny:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ExpressionAny)
    def __hash__(self):
        return hash("ExpressionAny")

class ExpressionBetween(Node["BetweenExpression"]):
    ...

class ExpressionBinary(Node["BinaryExpression"]):
    ...

class ExpressionBraces(Node["Expression"]):
    ...

class ExpressionColumn(Node["ColumnName"]):
    ...

class ExpressionDataset(Node["TableName"]):
    ...

class ExpressionIndex(Node["IndexExpression"]):
    ...

class ExpressionList(Node["frozenlist[Expression]"]):
    ...

class ExpressionLiteral(Node["Literal"]):
    ...

class ExpressionOr(Node["frozenlist[Expression]"]):
    ...

class ExpressionParentheses(Node["Expression"]):
    ...

class ExpressionProperty(Node["PropertyExpression"]):
    ...

class ExpressionUnary(Node["UnaryExpression"]):
    ...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionAnd | ExpressionAny | ExpressionBetween | ExpressionBinary | ExpressionBraces | ExpressionColumn | ExpressionDataset | ExpressionIndex | ExpressionList | ExpressionLiteral | ExpressionOr | ExpressionParentheses | ExpressionProperty | ExpressionUnary"""

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Expression")
    AND = hydra.core.Name("and")
    ANY = hydra.core.Name("any")
    BETWEEN = hydra.core.Name("between")
    BINARY = hydra.core.Name("binary")
    BRACES = hydra.core.Name("braces")
    COLUMN = hydra.core.Name("column")
    DATASET = hydra.core.Name("dataset")
    INDEX = hydra.core.Name("index")
    LIST = hydra.core.Name("list")
    LITERAL = hydra.core.Name("literal")
    OR = hydra.core.Name("or")
    PARENTHESES = hydra.core.Name("parentheses")
    PROPERTY = hydra.core.Name("property")
    UNARY = hydra.core.Name("unary")

class FunctionBuiltIn(Node["BuiltInFunction"]):
    ...

class FunctionCustom(Node["FunctionName"]):
    ...

class _FunctionMeta(type):
    def __getitem__(cls, item):
        return object

class Function(metaclass=_FunctionMeta):
    r"""FunctionBuiltIn | FunctionCustom"""

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Function")
    BUILT_IN = hydra.core.Name("builtIn")
    CUSTOM = hydra.core.Name("custom")

@dataclass(frozen=True)
class FunctionExpression:
    function: Function
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.FunctionExpression")
    FUNCTION = hydra.core.Name("function")
    ARGUMENTS = hydra.core.Name("arguments")

class FunctionName(Node[str]):
    ...

FunctionName.TYPE_ = hydra.core.Name("hydra.kusto.kql.FunctionName")

@dataclass(frozen=True)
class IndexExpression:
    expression: Expression
    index: str

    TYPE_ = hydra.core.Name("hydra.kusto.kql.IndexExpression")
    EXPRESSION = hydra.core.Name("expression")
    INDEX = hydra.core.Name("index")

@dataclass(frozen=True)
class JoinCommand:
    kind: JoinKind
    expression: TableName
    on: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.JoinCommand")
    KIND = hydra.core.Name("kind")
    EXPRESSION = hydra.core.Name("expression")
    ON = hydra.core.Name("on")

class JoinKind(Enum):
    LEFTOUTER = hydra.core.Name("leftouter")

    LEFTSEMI = hydra.core.Name("leftsemi")

    LEFTANTI = hydra.core.Name("leftanti")

    FULLOUTER = hydra.core.Name("fullouter")

    INNER = hydra.core.Name("inner")

    INNERUNIQUE = hydra.core.Name("innerunique")

    RIGHTOUTER = hydra.core.Name("rightouter")

    RIGHTSEMI = hydra.core.Name("rightsemi")

    RIGHTANTI = hydra.core.Name("rightanti")

JoinKind.TYPE_ = hydra.core.Name("hydra.kusto.kql.JoinKind")

@dataclass(frozen=True)
class KeyValuePair:
    key: str
    value: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.KeyValuePair")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class LetBinding:
    name: ColumnName
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.LetBinding")
    NAME = hydra.core.Name("name")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class LetExpression:
    bindings: frozenlist[LetBinding]
    expression: TabularExpression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.LetExpression")
    BINDINGS = hydra.core.Name("bindings")
    EXPRESSION = hydra.core.Name("expression")

class LiteralDuration(Node["Duration"]):
    ...

class LiteralDatetime(Node["Datetime"]):
    ...

class LiteralString(Node[str]):
    ...

class LiteralInt(Node[int]):
    ...

class LiteralLong(Node[int]):
    ...

class LiteralDouble(Node[float]):
    ...

class LiteralBoolean(Node[bool]):
    ...

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

class Literal(metaclass=_LiteralMeta):
    r"""LiteralDuration | LiteralDatetime | LiteralString | LiteralInt | LiteralLong | LiteralDouble | LiteralBoolean"""

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Literal")
    DURATION = hydra.core.Name("duration")
    DATETIME = hydra.core.Name("datetime")
    STRING = hydra.core.Name("string")
    INT = hydra.core.Name("int")
    LONG = hydra.core.Name("long")
    DOUBLE = hydra.core.Name("double")
    BOOLEAN = hydra.core.Name("boolean")

class Order(Enum):
    ASCENDING = hydra.core.Name("ascending")

    DESCENDING = hydra.core.Name("descending")

Order.TYPE_ = hydra.core.Name("hydra.kusto.kql.Order")

@dataclass(frozen=True)
class Parameter:
    key: str
    value: Literal

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Parameter")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class ParseCommand:
    column: ColumnName
    pairs: frozenlist[KeyValuePair]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.ParseCommand")
    COLUMN = hydra.core.Name("column")
    PAIRS = hydra.core.Name("pairs")

class PipelineExpression(Node["frozenlist[TabularExpression]"]):
    ...

PipelineExpression.TYPE_ = hydra.core.Name("hydra.kusto.kql.PipelineExpression")

@dataclass(frozen=True)
class PrintCommand:
    column: Maybe[ColumnName]
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.PrintCommand")
    COLUMN = hydra.core.Name("column")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class Projection:
    expression: Expression
    alias: Maybe[ColumnName]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.Projection")
    EXPRESSION = hydra.core.Name("expression")
    ALIAS = hydra.core.Name("alias")

@dataclass(frozen=True)
class PropertyExpression:
    expression: Expression
    property: str

    TYPE_ = hydra.core.Name("hydra.kusto.kql.PropertyExpression")
    EXPRESSION = hydra.core.Name("expression")
    PROPERTY = hydra.core.Name("property")

class Query(Node["TabularExpression"]):
    ...

Query.TYPE_ = hydra.core.Name("hydra.kusto.kql.Query")

@dataclass(frozen=True)
class SearchCommand:
    r"""Search across all datasets and columns or, if provided, specific datasets and/or columns."""

    datasets: frozenlist[TableName]
    pattern: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.SearchCommand")
    DATASETS = hydra.core.Name("datasets")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class SummarizeCommand:
    columns: frozenlist[ColumnAssignment]
    by: frozenlist[ColumnName]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.SummarizeCommand")
    COLUMNS = hydra.core.Name("columns")
    BY = hydra.core.Name("by")

class TableName(Node[str]):
    ...

TableName.TYPE_ = hydra.core.Name("hydra.kusto.kql.TableName")

@dataclass(frozen=True)
class TopCommand:
    count: int
    sort: frozenlist[SortBy]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.TopCommand")
    COUNT = hydra.core.Name("count")
    SORT = hydra.core.Name("sort")

@dataclass(frozen=True)
class SortBy:
    column: ColumnName
    order: Maybe[Order]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.SortBy")
    COLUMN = hydra.core.Name("column")
    ORDER = hydra.core.Name("order")

class TabularExpressionCommand(Node["Command"]):
    ...

class TabularExpressionPipeline(Node["PipelineExpression"]):
    ...

class TabularExpressionLet(Node["LetExpression"]):
    ...

class TabularExpressionTable(Node["TableName"]):
    ...

class _TabularExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class TabularExpression(metaclass=_TabularExpressionMeta):
    r"""TabularExpressionCommand | TabularExpressionPipeline | TabularExpressionLet | TabularExpressionTable"""

    TYPE_ = hydra.core.Name("hydra.kusto.kql.TabularExpression")
    COMMAND = hydra.core.Name("command")
    PIPELINE = hydra.core.Name("pipeline")
    LET = hydra.core.Name("let")
    TABLE = hydra.core.Name("table")

@dataclass(frozen=True)
class UnaryExpression:
    operator: UnaryOperator
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.kusto.kql.UnaryExpression")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

class UnaryOperator(Enum):
    NOT = hydra.core.Name("not")

UnaryOperator.TYPE_ = hydra.core.Name("hydra.kusto.kql.UnaryOperator")

@dataclass(frozen=True)
class UnionCommand:
    parameters: frozenlist[Parameter]
    kind: Maybe[UnionKind]
    with_source: Maybe[ColumnName]
    is_fuzzy: Maybe[bool]
    tables: frozenlist[TableName]

    TYPE_ = hydra.core.Name("hydra.kusto.kql.UnionCommand")
    PARAMETERS = hydra.core.Name("parameters")
    KIND = hydra.core.Name("kind")
    WITH_SOURCE = hydra.core.Name("withSource")
    IS_FUZZY = hydra.core.Name("isFuzzy")
    TABLES = hydra.core.Name("tables")

class UnionKind(Enum):
    INNER = hydra.core.Name("inner")

    OUTER = hydra.core.Name("outer")

UnionKind.TYPE_ = hydra.core.Name("hydra.kusto.kql.UnionKind")
