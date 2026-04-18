# Note: this is an automatically generated file. Do not edit.

r"""Algebraic expression trees for the path algebra by Angles et al., extended for GQL support."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class QueryExpression:
    r"""Complete query with path algebra and result projection."""

    path_expression: PathExpression
    result_projection: Maybe[ResultProjection]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.QueryExpression")
    PATH_EXPRESSION = hydra.core.Name("pathExpression")
    RESULT_PROJECTION = hydra.core.Name("resultProjection")

class PathExpressionBase(Node["BaseExpression"]):
    r"""Base case: extract paths from graph"""

class PathExpressionSelection(Node["SelectionExpression"]):
    r"""Selection operator (σ): filter paths by condition"""

class PathExpressionJoin(Node["JoinExpression"]):
    r"""Join operator (⊲⊳): concatenate compatible paths"""

class PathExpressionUnion(Node["UnionExpression"]):
    r"""Union operator (∪): combine path sets"""

class PathExpressionRecursive(Node["RecursiveExpression"]):
    r"""Recursive operator (φ): compute transitive closure with semantics"""

class PathExpressionGroupBy(Node["GroupByExpression"]):
    r"""Group-by operator (γ): organize paths into solution space"""

class PathExpressionOrderBy(Node["OrderByExpression"]):
    r"""Order-by operator (τ): sort solution space"""

class PathExpressionProjection(Node["ProjectionExpression"]):
    r"""Projection operator (π): extract paths from solution space"""

class _PathExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# A path algebra expression that evaluates to a set of paths.
class PathExpression(metaclass=_PathExpressionMeta):
    r"""PathExpressionBase | PathExpressionSelection | PathExpressionJoin | PathExpressionUnion | PathExpressionRecursive | PathExpressionGroupBy | PathExpressionOrderBy | PathExpressionProjection"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathExpression")
    BASE = hydra.core.Name("base")
    SELECTION = hydra.core.Name("selection")
    JOIN = hydra.core.Name("join")
    UNION = hydra.core.Name("union")
    RECURSIVE = hydra.core.Name("recursive")
    GROUP_BY = hydra.core.Name("groupBy")
    ORDER_BY = hydra.core.Name("orderBy")
    PROJECTION = hydra.core.Name("projection")

class BaseExpressionPaths0(Node["GraphReference"]):
    r"""Paths0(G): all paths of length 0 (nodes)"""

class BaseExpressionPaths1(Node["GraphReference"]):
    r"""Paths1(G): all paths of length 1 (edges)"""

class BaseExpressionPathsStar(Node["GraphReference"]):
    r"""Paths*(G): all paths in graph (infinite without restrictions)"""

class _BaseExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# Base path expressions that extract paths from graph.
class BaseExpression(metaclass=_BaseExpressionMeta):
    r"""BaseExpressionPaths0 | BaseExpressionPaths1 | BaseExpressionPathsStar"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.BaseExpression")
    PATHS0 = hydra.core.Name("paths0")
    PATHS1 = hydra.core.Name("paths1")
    PATHS_STAR = hydra.core.Name("pathsStar")

class GraphReference(Node[str]):
    r"""Reference to a property graph."""

GraphReference.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.GraphReference")

@dataclass(frozen=True)
class SelectionExpression:
    r"""Selection operator: σ_condition(expression)."""

    condition: SelectionCondition
    expression: PathExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.SelectionExpression")
    CONDITION = hydra.core.Name("condition")
    EXPRESSION = hydra.core.Name("expression")

class SelectionConditionSimple(Node["SimpleCondition"]):
    ...

class SelectionConditionAnd(Node["AndCondition"]):
    ...

class SelectionConditionOr(Node["OrCondition"]):
    ...

class SelectionConditionNot(Node["NotCondition"]):
    ...

class _SelectionConditionMeta(type):
    def __getitem__(cls, item):
        return object

# Conditions for filtering paths.
class SelectionCondition(metaclass=_SelectionConditionMeta):
    r"""SelectionConditionSimple | SelectionConditionAnd | SelectionConditionOr | SelectionConditionNot"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.SelectionCondition")
    SIMPLE = hydra.core.Name("simple")
    AND = hydra.core.Name("and")
    OR = hydra.core.Name("or")
    NOT = hydra.core.Name("not")

class SimpleConditionLabelEquals(Node["LabelCondition"]):
    ...

class SimpleConditionPropertyEquals(Node["PropertyCondition"]):
    ...

class SimpleConditionPropertyComparison(Node["PropertyComparisonCondition"]):
    ...

class SimpleConditionLengthEquals(Node["LengthCondition"]):
    ...

class _SimpleConditionMeta(type):
    def __getitem__(cls, item):
        return object

# Atomic selection conditions.
class SimpleCondition(metaclass=_SimpleConditionMeta):
    r"""SimpleConditionLabelEquals | SimpleConditionPropertyEquals | SimpleConditionPropertyComparison | SimpleConditionLengthEquals"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.SimpleCondition")
    LABEL_EQUALS = hydra.core.Name("labelEquals")
    PROPERTY_EQUALS = hydra.core.Name("propertyEquals")
    PROPERTY_COMPARISON = hydra.core.Name("propertyComparison")
    LENGTH_EQUALS = hydra.core.Name("lengthEquals")

@dataclass(frozen=True)
class LabelCondition:
    r"""Conditions on node/edge labels: label(node(i)) = v."""

    target: PathElement
    value: str

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.LabelCondition")
    TARGET = hydra.core.Name("target")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class PropertyCondition:
    r"""Property equality conditions: node(i).prop = v."""

    target: PathElement
    property: str
    value: LiteralValue

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertyCondition")
    TARGET = hydra.core.Name("target")
    PROPERTY = hydra.core.Name("property")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class PropertyComparisonCondition:
    r"""Property comparison conditions: node(i).prop > v, etc."""

    target: PathElement
    property: str
    operator: ComparisonOperator
    value: LiteralValue

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertyComparisonCondition")
    TARGET = hydra.core.Name("target")
    PROPERTY = hydra.core.Name("property")
    OPERATOR = hydra.core.Name("operator")
    VALUE = hydra.core.Name("value")

class ComparisonOperator(Enum):
    r"""Comparison operators for property conditions."""

    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    LESS_THAN = hydra.core.Name("lessThan")

    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")

    GREATER_THAN = hydra.core.Name("greaterThan")

    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")

ComparisonOperator.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.ComparisonOperator")

class LiteralValueString(Node[str]):
    ...

class LiteralValueInteger(Node[int]):
    ...

class LiteralValueFloat(Node[float]):
    ...

class LiteralValueBoolean(Node[bool]):
    ...

class _LiteralValueMeta(type):
    def __getitem__(cls, item):
        return object

# Literal values for comparisons.
class LiteralValue(metaclass=_LiteralValueMeta):
    r"""LiteralValueString | LiteralValueInteger | LiteralValueFloat | LiteralValueBoolean"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.LiteralValue")
    STRING = hydra.core.Name("string")
    INTEGER = hydra.core.Name("integer")
    FLOAT = hydra.core.Name("float")
    BOOLEAN = hydra.core.Name("boolean")

@dataclass(frozen=True)
class LengthCondition:
    r"""Condition on path length: len() = i."""

    length: int

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.LengthCondition")
    LENGTH = hydra.core.Name("length")

class PathElementNode(Node[int]):
    ...

class PathElementEdge(Node[int]):
    ...

class PathElementFirst:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PathElementFirst)
    def __hash__(self):
        return hash("PathElementFirst")

class PathElementLast:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PathElementLast)
    def __hash__(self):
        return hash("PathElementLast")

class _PathElementMeta(type):
    def __getitem__(cls, item):
        return object

# References to elements within a path.
class PathElement(metaclass=_PathElementMeta):
    r"""PathElementNode | PathElementEdge | PathElementFirst | PathElementLast"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathElement")
    NODE = hydra.core.Name("node")
    EDGE = hydra.core.Name("edge")
    FIRST = hydra.core.Name("first")
    LAST = hydra.core.Name("last")

@dataclass(frozen=True)
class AndCondition:
    left: SelectionCondition
    right: SelectionCondition

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.AndCondition")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class OrCondition:
    left: SelectionCondition
    right: SelectionCondition

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.OrCondition")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class NotCondition:
    condition: SelectionCondition

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.NotCondition")
    CONDITION = hydra.core.Name("condition")

@dataclass(frozen=True)
class JoinExpression:
    r"""Join operator: expr1 ⊲⊳ expr2."""

    left: PathExpression
    right: PathExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.JoinExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class UnionExpression:
    r"""Union operator: expr1 ∪ expr2."""

    left: PathExpression
    right: PathExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.UnionExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class RecursiveExpression:
    r"""Recursive operator with path semantics."""

    semantics: PathSemantics
    expression: PathExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.RecursiveExpression")
    SEMANTICS = hydra.core.Name("semantics")
    EXPRESSION = hydra.core.Name("expression")

class PathSemantics(Enum):
    r"""Path semantics for recursive operations."""

    WALK = hydra.core.Name("walk")

    TRAIL = hydra.core.Name("trail")

    ACYCLIC = hydra.core.Name("acyclic")

    SIMPLE = hydra.core.Name("simple")

    SHORTEST = hydra.core.Name("shortest")

PathSemantics.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathSemantics")

class SolutionSpaceExpressionGroupBy(Node["GroupByExpression"]):
    ...

class SolutionSpaceExpressionOrderBy(Node["OrderByExpression"]):
    ...

class _SolutionSpaceExpressionMeta(type):
    def __getitem__(cls, item):
        return object

# Expressions that work with solution spaces.
class SolutionSpaceExpression(metaclass=_SolutionSpaceExpressionMeta):
    r"""SolutionSpaceExpressionGroupBy | SolutionSpaceExpressionOrderBy"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.SolutionSpaceExpression")
    GROUP_BY = hydra.core.Name("groupBy")
    ORDER_BY = hydra.core.Name("orderBy")

@dataclass(frozen=True)
class GroupByExpression:
    r"""Group-by operator: γ_criterion(expression)."""

    criterion: GroupByCriterion
    expression: PathExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.GroupByExpression")
    CRITERION = hydra.core.Name("criterion")
    EXPRESSION = hydra.core.Name("expression")

class GroupByCriterion(Enum):
    r"""Grouping criteria corresponding to paper's γ variants."""

    NONE = hydra.core.Name("none")

    SOURCE = hydra.core.Name("source")

    TARGET = hydra.core.Name("target")

    LENGTH = hydra.core.Name("length")

    SOURCE_TARGET = hydra.core.Name("sourceTarget")

    SOURCE_LENGTH = hydra.core.Name("sourceLength")

    TARGET_LENGTH = hydra.core.Name("targetLength")

    SOURCE_TARGET_LENGTH = hydra.core.Name("sourceTargetLength")

GroupByCriterion.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.GroupByCriterion")

@dataclass(frozen=True)
class OrderByExpression:
    r"""Order-by operator: τ_criterion(solutionSpace)."""

    criterion: OrderByCriterion
    expression: SolutionSpaceExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.OrderByExpression")
    CRITERION = hydra.core.Name("criterion")
    EXPRESSION = hydra.core.Name("expression")

class OrderByCriterion(Enum):
    r"""Ordering criteria corresponding to paper's τ variants."""

    PARTITION = hydra.core.Name("partition")

    GROUP = hydra.core.Name("group")

    PATH = hydra.core.Name("path")

    PARTITION_GROUP = hydra.core.Name("partitionGroup")

    PARTITION_PATH = hydra.core.Name("partitionPath")

    GROUP_PATH = hydra.core.Name("groupPath")

    PARTITION_GROUP_PATH = hydra.core.Name("partitionGroupPath")

OrderByCriterion.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.OrderByCriterion")

@dataclass(frozen=True)
class ProjectionExpression:
    r"""Projection operator: π_(#P,#G,#A)(solutionSpace)."""

    partitions: ProjectionSpec
    groups: ProjectionSpec
    paths: ProjectionSpec
    expression: SolutionSpaceExpression

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.ProjectionExpression")
    PARTITIONS = hydra.core.Name("partitions")
    GROUPS = hydra.core.Name("groups")
    PATHS = hydra.core.Name("paths")
    EXPRESSION = hydra.core.Name("expression")

class ProjectionSpecAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ProjectionSpecAll)
    def __hash__(self):
        return hash("ProjectionSpecAll")

class ProjectionSpecLimited(Node[int]):
    ...

class _ProjectionSpecMeta(type):
    def __getitem__(cls, item):
        return object

# Projection specification: * or specific number.
class ProjectionSpec(metaclass=_ProjectionSpecMeta):
    r"""ProjectionSpecAll | ProjectionSpecLimited"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.ProjectionSpec")
    ALL = hydra.core.Name("all")
    LIMITED = hydra.core.Name("limited")

@dataclass(frozen=True)
class ResultProjection:
    r"""Extract specific values from paths for RETURN clause."""

    projections: frozenlist[PropertyExtraction]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.ResultProjection")
    PROJECTIONS = hydra.core.Name("projections")

@dataclass(frozen=True)
class PropertyExtraction:
    r"""Extract properties from path elements."""

    alias: Maybe[str]
    source: PropertySource

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertyExtraction")
    ALIAS = hydra.core.Name("alias")
    SOURCE = hydra.core.Name("source")

class PropertySourceNodeProperty(Node["NodePropertyRef"]):
    ...

class PropertySourceEdgeProperty(Node["EdgePropertyRef"]):
    ...

class PropertySourcePathProperty(Node["PathPropertyRef"]):
    ...

class _PropertySourceMeta(type):
    def __getitem__(cls, item):
        return object

# Source of a property value.
class PropertySource(metaclass=_PropertySourceMeta):
    r"""PropertySourceNodeProperty | PropertySourceEdgeProperty | PropertySourcePathProperty"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PropertySource")
    NODE_PROPERTY = hydra.core.Name("nodeProperty")
    EDGE_PROPERTY = hydra.core.Name("edgeProperty")
    PATH_PROPERTY = hydra.core.Name("pathProperty")

@dataclass(frozen=True)
class NodePropertyRef:
    r"""Reference to a node property: node.property."""

    element: PathElement
    property: str

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.NodePropertyRef")
    ELEMENT = hydra.core.Name("element")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class EdgePropertyRef:
    r"""Reference to an edge property: edge.property."""

    element: PathElement
    property: str

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.EdgePropertyRef")
    ELEMENT = hydra.core.Name("element")
    PROPERTY = hydra.core.Name("property")

class PathPropertyRef(Enum):
    r"""Reference to path-level properties: length, etc."""

    LENGTH = hydra.core.Name("length")

    START_NODE = hydra.core.Name("startNode")

    END_NODE = hydra.core.Name("endNode")

PathPropertyRef.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.expressions.PathPropertyRef")
