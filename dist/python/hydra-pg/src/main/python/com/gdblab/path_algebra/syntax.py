# Note: this is an automatically generated file. Do not edit.

r"""A syntax model for the path algebra grammar by Angles et al. See the paper "Path-based Algebraic Foundations of Graph Query Languages" and the ANTLR grammar at https://github.com/pathalgebra/AlgebraParser."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node
from typing import TypeAlias, cast
import hydra.core

Number: TypeAlias = "int"

Text: TypeAlias = "str"

Label: TypeAlias = "str"

Variable: TypeAlias = "str"

PathName: TypeAlias = "str"

@dataclass(frozen=True)
class PathQuery:
    projection: Projection
    restrictor_ext: Maybe[RestrictorExt]
    path_pattern: PathPattern
    group_by: Maybe[GroupBy]
    order_by: Maybe[OrderBy]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.PathQuery")
    PROJECTION = hydra.core.Name("projection")
    RESTRICTOR_EXT = hydra.core.Name("restrictorExt")
    PATH_PATTERN = hydra.core.Name("pathPattern")
    GROUP_BY = hydra.core.Name("groupBy")
    ORDER_BY = hydra.core.Name("orderBy")

@dataclass(frozen=True)
class Projection:
    part_proj: PartProj
    group_proj: GroupProj
    path_proj: PathProj

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Projection")
    PART_PROJ = hydra.core.Name("partProj")
    GROUP_PROJ = hydra.core.Name("groupProj")
    PATH_PROJ = hydra.core.Name("pathProj")

class PartProjAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PartProjAll)
    def __hash__(self):
        return hash("PartProjAll")

class PartProjLimited(Node["Number"]):
    ...

class _PartProjMeta(type):
    def __getitem__(cls, item):
        return object

class PartProj(metaclass=_PartProjMeta):
    r"""PartProjAll | PartProjLimited"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.PartProj")
    ALL = hydra.core.Name("all")
    LIMITED = hydra.core.Name("limited")

class GroupProjAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GroupProjAll)
    def __hash__(self):
        return hash("GroupProjAll")

class GroupProjLimited(Node["Number"]):
    ...

class _GroupProjMeta(type):
    def __getitem__(cls, item):
        return object

class GroupProj(metaclass=_GroupProjMeta):
    r"""GroupProjAll | GroupProjLimited"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.GroupProj")
    ALL = hydra.core.Name("all")
    LIMITED = hydra.core.Name("limited")

class PathProjAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PathProjAll)
    def __hash__(self):
        return hash("PathProjAll")

class PathProjLimited(Node["Number"]):
    ...

class _PathProjMeta(type):
    def __getitem__(cls, item):
        return object

class PathProj(metaclass=_PathProjMeta):
    r"""PathProjAll | PathProjLimited"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.PathProj")
    ALL = hydra.core.Name("all")
    LIMITED = hydra.core.Name("limited")

class RestrictorExt(Enum):
    WALK = hydra.core.Name("walk")

    TRAIL = hydra.core.Name("trail")

    SIMPLE = hydra.core.Name("simple")

    ACYCLIC = hydra.core.Name("acyclic")

    SHORTEST = hydra.core.Name("shortest")

RestrictorExt.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.RestrictorExt")

class OrderBy(Node["OrderByOption"]):
    ...

OrderBy.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.OrderBy")

class GroupBy(Node["GroupByOption"]):
    ...

GroupBy.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.GroupBy")

class OrderByOption(Enum):
    PARTITION = hydra.core.Name("partition")

    GROUP = hydra.core.Name("group")

    PATH = hydra.core.Name("path")

    PARTITION_GROUP = hydra.core.Name("partitionGroup")

    PARTITION_PATH = hydra.core.Name("partitionPath")

    GROUP_PATH = hydra.core.Name("groupPath")

    PARTITION_GROUP_PATH = hydra.core.Name("partitionGroupPath")

OrderByOption.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.OrderByOption")

class GroupByOption(Enum):
    SOURCE = hydra.core.Name("source")

    TARGET = hydra.core.Name("target")

    LENGTH = hydra.core.Name("length")

    SOURCE_TARGET = hydra.core.Name("sourceTarget")

    SOURCE_LENGTH = hydra.core.Name("sourceLength")

    TARGET_LENGTH = hydra.core.Name("targetLength")

    SOURCE_TARGET_LENGTH = hydra.core.Name("sourceTargetLength")

GroupByOption.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.GroupByOption")

@dataclass(frozen=True)
class PathPattern:
    path_name: PathName
    start_node: NodePattern
    edge: EdgePattern
    end_node: NodePattern
    condition: Maybe[ComplexCondition]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.PathPattern")
    PATH_NAME = hydra.core.Name("pathName")
    START_NODE = hydra.core.Name("startNode")
    EDGE = hydra.core.Name("edge")
    END_NODE = hydra.core.Name("endNode")
    CONDITION = hydra.core.Name("condition")

@dataclass(frozen=True)
class NodePattern:
    variable: Maybe[Variable]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.NodePattern")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class EdgePattern:
    direction: EdgeDirection
    rpq: Maybe[Rpq]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.EdgePattern")
    DIRECTION = hydra.core.Name("direction")
    RPQ = hydra.core.Name("rpq")

class EdgeDirection(Enum):
    OUTGOING = hydra.core.Name("outgoing")

    INCOMING = hydra.core.Name("incoming")

    UNDIRECTED = hydra.core.Name("undirected")

EdgeDirection.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.EdgeDirection")

class RpqParenthesis(Node["Rpq"]):
    ...

class RpqLabel(Node["Label"]):
    ...

class RpqNegated(Node["Label"]):
    ...

class RpqReverse(Node["Label"]):
    ...

class RpqOptional(Node["Rpq"]):
    ...

class RpqPlus(Node["Plus"]):
    ...

class RpqStar(Node["Star"]):
    ...

class RpqConcatenation(Node["Concatenation"]):
    ...

class RpqAlternation(Node["Alternation"]):
    ...

class _RpqMeta(type):
    def __getitem__(cls, item):
        return object

class Rpq(metaclass=_RpqMeta):
    r"""RpqParenthesis | RpqLabel | RpqNegated | RpqReverse | RpqOptional | RpqPlus | RpqStar | RpqConcatenation | RpqAlternation"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Rpq")
    PARENTHESIS = hydra.core.Name("parenthesis")
    LABEL = hydra.core.Name("label")
    NEGATED = hydra.core.Name("negated")
    REVERSE = hydra.core.Name("reverse")
    OPTIONAL = hydra.core.Name("optional")
    PLUS = hydra.core.Name("plus")
    STAR = hydra.core.Name("star")
    CONCATENATION = hydra.core.Name("concatenation")
    ALTERNATION = hydra.core.Name("alternation")

@dataclass(frozen=True)
class Plus:
    expression: Rpq
    restrictor: Maybe[RpqRestrictor]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Plus")
    EXPRESSION = hydra.core.Name("expression")
    RESTRICTOR = hydra.core.Name("restrictor")

@dataclass(frozen=True)
class Star:
    expression: Rpq
    restrictor: Maybe[RpqRestrictor]

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Star")
    EXPRESSION = hydra.core.Name("expression")
    RESTRICTOR = hydra.core.Name("restrictor")

@dataclass(frozen=True)
class Concatenation:
    left: Rpq
    right: Rpq

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Concatenation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class Alternation:
    left: Rpq
    right: Rpq

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Alternation")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class RpqRestrictor(Node["RestrictorExt"]):
    ...

RpqRestrictor.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.RpqRestrictor")

class ComplexConditionSimple(Node["Condition"]):
    ...

class ComplexConditionCompound(Node["CompoundComplexCondition"]):
    ...

class _ComplexConditionMeta(type):
    def __getitem__(cls, item):
        return object

class ComplexCondition(metaclass=_ComplexConditionMeta):
    r"""ComplexConditionSimple | ComplexConditionCompound"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.ComplexCondition")
    SIMPLE = hydra.core.Name("simple")
    COMPOUND = hydra.core.Name("compound")

@dataclass(frozen=True)
class CompoundComplexCondition:
    lhs: Condition
    operator: BoolOp
    rhs: ComplexCondition

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.CompoundComplexCondition")
    LHS = hydra.core.Name("lhs")
    OPERATOR = hydra.core.Name("operator")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class Condition:
    function: Function
    compare_sym: CompareSym
    value: Text

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Condition")
    FUNCTION = hydra.core.Name("function")
    COMPARE_SYM = hydra.core.Name("compareSym")
    VALUE = hydra.core.Name("value")

class CompareSym(Enum):
    EQUAL = hydra.core.Name("equal")

    NOT_EQUAL = hydra.core.Name("notEqual")

    LESS_THAN = hydra.core.Name("lessThan")

    GREATER_THAN = hydra.core.Name("greaterThan")

    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")

    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")

CompareSym.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.CompareSym")

class FunctionSimple(Node["SimpleFunction"]):
    ...

class FunctionNested(Node["NestedFunction"]):
    ...

class FunctionComplex(Node["ComplexFunction"]):
    ...

class _FunctionMeta(type):
    def __getitem__(cls, item):
        return object

class Function(metaclass=_FunctionMeta):
    r"""FunctionSimple | FunctionNested | FunctionComplex"""

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.Function")
    SIMPLE = hydra.core.Name("simple")
    NESTED = hydra.core.Name("nested")
    COMPLEX = hydra.core.Name("complex")

@dataclass(frozen=True)
class SimpleFunction:
    name: Text
    argument: Text

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.SimpleFunction")
    NAME = hydra.core.Name("name")
    ARGUMENT = hydra.core.Name("argument")

@dataclass(frozen=True)
class NestedFunction:
    name: Text
    inner_function: Function

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.NestedFunction")
    NAME = hydra.core.Name("name")
    INNER_FUNCTION = hydra.core.Name("innerFunction")

@dataclass(frozen=True)
class ComplexFunction:
    name: Text
    inner_function: Function
    additional_arg: Text

    TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.ComplexFunction")
    NAME = hydra.core.Name("name")
    INNER_FUNCTION = hydra.core.Name("innerFunction")
    ADDITIONAL_ARG = hydra.core.Name("additionalArg")

class BoolOp(Enum):
    AND = hydra.core.Name("and")

    OR = hydra.core.Name("or")

BoolOp.TYPE_ = hydra.core.Name("com.gdblab.pathAlgebra.syntax.BoolOp")
