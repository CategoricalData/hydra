# Note: this is an automatically generated file. Do not edit.

r"""A common model for pattern-matching queries over property graphs."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core
import hydra.pg.model

class AggregationQuery(Enum):
    COUNT = hydra.core.Name("count")

AggregationQuery.TYPE_ = hydra.core.Name("hydra.pg.query.AggregationQuery")

class ApplicationQuery(Node["frozenlist[Query]"]):
    ...

ApplicationQuery.TYPE_ = hydra.core.Name("hydra.pg.query.ApplicationQuery")

@dataclass(frozen=True)
class AssociativeExpression:
    operator: BinaryOperator
    operands: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.pg.query.AssociativeExpression")
    OPERATOR = hydra.core.Name("operator")
    OPERANDS = hydra.core.Name("operands")

@dataclass(frozen=True)
class BinaryExpression:
    left: Expression
    operator: BinaryOperator
    right: Expression

    TYPE_ = hydra.core.Name("hydra.pg.query.BinaryExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class BinaryBooleanOperator(Enum):
    AND = hydra.core.Name("and")

    OR = hydra.core.Name("or")

    XOR = hydra.core.Name("xor")

BinaryBooleanOperator.TYPE_ = hydra.core.Name("hydra.pg.query.BinaryBooleanOperator")

class BinaryOperatorBoolean(Node["BinaryBooleanOperator"]):
    ...

class BinaryOperatorComparison(Node["ComparisonOperator"]):
    ...

class BinaryOperatorPower:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, BinaryOperatorPower)
    def __hash__(self):
        return hash("BinaryOperatorPower")

class _BinaryOperatorMeta(type):
    def __getitem__(cls, item):
        return object

class BinaryOperator(metaclass=_BinaryOperatorMeta):
    r"""BinaryOperatorBoolean | BinaryOperatorComparison | BinaryOperatorPower"""

    TYPE_ = hydra.core.Name("hydra.pg.query.BinaryOperator")
    BOOLEAN = hydra.core.Name("boolean")
    COMPARISON = hydra.core.Name("comparison")
    POWER = hydra.core.Name("power")

@dataclass(frozen=True)
class Binding:
    key: Variable
    value: Query

    TYPE_ = hydra.core.Name("hydra.pg.query.Binding")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class ComparisonOperator(Enum):
    EQ = hydra.core.Name("eq")

    NEQ = hydra.core.Name("neq")

    LT = hydra.core.Name("lt")

    LTE = hydra.core.Name("lte")

    GT = hydra.core.Name("gt")

    GTE = hydra.core.Name("gte")

ComparisonOperator.TYPE_ = hydra.core.Name("hydra.pg.query.ComparisonOperator")

@dataclass(frozen=True)
class EdgeProjectionPattern:
    direction: hydra.pg.model.Direction
    label: Maybe[hydra.pg.model.EdgeLabel]
    properties: frozenlist[PropertyPattern]
    vertex: Maybe[VertexPattern]

    TYPE_ = hydra.core.Name("hydra.pg.query.EdgeProjectionPattern")
    DIRECTION = hydra.core.Name("direction")
    LABEL = hydra.core.Name("label")
    PROPERTIES = hydra.core.Name("properties")
    VERTEX = hydra.core.Name("vertex")

class ExpressionAssociative(Node["AssociativeExpression"]):
    ...

class ExpressionBinary(Node["BinaryExpression"]):
    ...

class ExpressionProperty(Node["PropertyProjection"]):
    ...

class ExpressionUnary(Node["UnaryExpression"]):
    ...

class ExpressionVariable(Node["Variable"]):
    ...

class ExpressionVertex(Node["VertexPattern"]):
    ...

class _ExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class Expression(metaclass=_ExpressionMeta):
    r"""ExpressionAssociative | ExpressionBinary | ExpressionProperty | ExpressionUnary | ExpressionVariable | ExpressionVertex"""

    TYPE_ = hydra.core.Name("hydra.pg.query.Expression")
    ASSOCIATIVE = hydra.core.Name("associative")
    BINARY = hydra.core.Name("binary")
    PROPERTY = hydra.core.Name("property")
    UNARY = hydra.core.Name("unary")
    VARIABLE = hydra.core.Name("variable")
    VERTEX = hydra.core.Name("vertex")

@dataclass(frozen=True)
class LetQuery:
    bindings: frozenlist[Binding]
    environment: Query

    TYPE_ = hydra.core.Name("hydra.pg.query.LetQuery")
    BINDINGS = hydra.core.Name("bindings")
    ENVIRONMENT = hydra.core.Name("environment")

@dataclass(frozen=True)
class MatchQuery:
    optional: bool
    pattern: frozenlist[Projection]
    where: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.pg.query.MatchQuery")
    OPTIONAL = hydra.core.Name("optional")
    PATTERN = hydra.core.Name("pattern")
    WHERE = hydra.core.Name("where")

@dataclass(frozen=True)
class Projection:
    value: Expression
    as_: Maybe[Variable]

    TYPE_ = hydra.core.Name("hydra.pg.query.Projection")
    VALUE = hydra.core.Name("value")
    AS = hydra.core.Name("as")

@dataclass(frozen=True)
class Projections:
    all: bool
    explicit: frozenlist[Projection]

    TYPE_ = hydra.core.Name("hydra.pg.query.Projections")
    ALL = hydra.core.Name("all")
    EXPLICIT = hydra.core.Name("explicit")

@dataclass(frozen=True)
class PropertyPattern:
    key: hydra.pg.model.PropertyKey
    value: PropertyValuePattern

    TYPE_ = hydra.core.Name("hydra.pg.query.PropertyPattern")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class PropertyProjection:
    base: Expression
    key: hydra.pg.model.PropertyKey

    TYPE_ = hydra.core.Name("hydra.pg.query.PropertyProjection")
    BASE = hydra.core.Name("base")
    KEY = hydra.core.Name("key")

class PropertyValue(Node[str]):
    ...

PropertyValue.TYPE_ = hydra.core.Name("hydra.pg.query.PropertyValue")

class PropertyValuePatternVariable(Node["hydra.pg.model.PropertyKey"]):
    ...

class PropertyValuePatternValue(Node[str]):
    ...

class _PropertyValuePatternMeta(type):
    def __getitem__(cls, item):
        return object

class PropertyValuePattern(metaclass=_PropertyValuePatternMeta):
    r"""PropertyValuePatternVariable | PropertyValuePatternValue"""

    TYPE_ = hydra.core.Name("hydra.pg.query.PropertyValuePattern")
    VARIABLE = hydra.core.Name("variable")
    VALUE = hydra.core.Name("value")

class QueryApplication(Node["ApplicationQuery"]):
    ...

class QueryAggregate(Node["AggregationQuery"]):
    ...

class QueryLetQuery(Node["LetQuery"]):
    ...

class QueryMatch(Node["MatchQuery"]):
    ...

class QuerySelect(Node["SelectQuery"]):
    ...

class QueryValue(Node[str]):
    ...

class _QueryMeta(type):
    def __getitem__(cls, item):
        return object

class Query(metaclass=_QueryMeta):
    r"""QueryApplication | QueryAggregate | QueryLetQuery | QueryMatch | QuerySelect | QueryValue"""

    TYPE_ = hydra.core.Name("hydra.pg.query.Query")
    APPLICATION = hydra.core.Name("application")
    AGGREGATE = hydra.core.Name("aggregate")
    LET_QUERY = hydra.core.Name("LetQuery")
    MATCH = hydra.core.Name("match")
    SELECT = hydra.core.Name("select")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class SelectQuery:
    distinct: bool
    projection: Projections

    TYPE_ = hydra.core.Name("hydra.pg.query.SelectQuery")
    DISTINCT = hydra.core.Name("distinct")
    PROJECTION = hydra.core.Name("projection")

@dataclass(frozen=True)
class UnaryExpression:
    operator: UnaryOperator
    operand: Expression

    TYPE_ = hydra.core.Name("hydra.pg.query.UnaryExpression")
    OPERATOR = hydra.core.Name("operator")
    OPERAND = hydra.core.Name("operand")

class UnaryOperator(Enum):
    NEGATE = hydra.core.Name("negate")

UnaryOperator.TYPE_ = hydra.core.Name("hydra.pg.query.UnaryOperator")

class Variable(Node[str]):
    ...

Variable.TYPE_ = hydra.core.Name("hydra.pg.query.Variable")

@dataclass(frozen=True)
class VertexPattern:
    variable: Maybe[Variable]
    label: Maybe[hydra.pg.model.VertexLabel]
    properties: frozenlist[PropertyPattern]
    edges: frozenlist[EdgeProjectionPattern]

    TYPE_ = hydra.core.Name("hydra.pg.query.VertexPattern")
    VARIABLE = hydra.core.Name("variable")
    LABEL = hydra.core.Name("label")
    PROPERTIES = hydra.core.Name("properties")
    EDGES = hydra.core.Name("edges")
