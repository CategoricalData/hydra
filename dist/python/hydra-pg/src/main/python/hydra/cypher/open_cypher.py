# Note: this is an automatically generated file. Do not edit.

r"""A Cypher model based on the OpenCypher specification (version 23), copyright Neo Technology, available at:
  https://opencypher.org/resources/."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class QueryRegular(Node["RegularQuery"]):
    ...

class QueryStandalone(Node["StandaloneCall"]):
    ...

class _QueryMeta(type):
    def __getitem__(cls, item):
        return object

class Query(metaclass=_QueryMeta):
    r"""QueryRegular | QueryStandalone"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Query")
    REGULAR = hydra.core.Name("regular")
    STANDALONE = hydra.core.Name("standalone")

@dataclass(frozen=True)
class RegularQuery:
    head: SingleQuery
    rest: frozenlist[Union]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RegularQuery")
    HEAD = hydra.core.Name("head")
    REST = hydra.core.Name("rest")

@dataclass(frozen=True)
class Union:
    all: bool
    query: SingleQuery

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Union")
    ALL = hydra.core.Name("all")
    QUERY = hydra.core.Name("query")

class SingleQuerySinglePart(Node["SinglePartQuery"]):
    ...

class SingleQueryMultiPart(Node["MultiPartQuery"]):
    ...

class _SingleQueryMeta(type):
    def __getitem__(cls, item):
        return object

class SingleQuery(metaclass=_SingleQueryMeta):
    r"""SingleQuerySinglePart | SingleQueryMultiPart"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.SingleQuery")
    SINGLE_PART = hydra.core.Name("singlePart")
    MULTI_PART = hydra.core.Name("multiPart")

@dataclass(frozen=True)
class SinglePartQuery:
    reading: frozenlist[ReadingClause]
    updating: frozenlist[UpdatingClause]
    return_: Maybe[Return]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.SinglePartQuery")
    READING = hydra.core.Name("reading")
    UPDATING = hydra.core.Name("updating")
    RETURN = hydra.core.Name("return")

@dataclass(frozen=True)
class WithClause:
    reading: frozenlist[ReadingClause]
    updating: frozenlist[UpdatingClause]
    with_: With

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.WithClause")
    READING = hydra.core.Name("reading")
    UPDATING = hydra.core.Name("updating")
    WITH = hydra.core.Name("with")

@dataclass(frozen=True)
class MultiPartQuery:
    with_: frozenlist[WithClause]
    body: SinglePartQuery

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MultiPartQuery")
    WITH = hydra.core.Name("with")
    BODY = hydra.core.Name("body")

class UpdatingClauseCreate(Node["Create"]):
    ...

class UpdatingClauseMerge(Node["Merge"]):
    ...

class UpdatingClauseDelete(Node["Delete"]):
    ...

class UpdatingClauseSet(Node["Set"]):
    ...

class UpdatingClauseRemove(Node["Remove"]):
    ...

class _UpdatingClauseMeta(type):
    def __getitem__(cls, item):
        return object

class UpdatingClause(metaclass=_UpdatingClauseMeta):
    r"""UpdatingClauseCreate | UpdatingClauseMerge | UpdatingClauseDelete | UpdatingClauseSet | UpdatingClauseRemove"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.UpdatingClause")
    CREATE = hydra.core.Name("create")
    MERGE = hydra.core.Name("merge")
    DELETE = hydra.core.Name("delete")
    SET = hydra.core.Name("set")
    REMOVE = hydra.core.Name("remove")

class ReadingClauseMatch(Node["Match"]):
    ...

class ReadingClauseUnwind(Node["Unwind"]):
    ...

class ReadingClauseInQueryCall(Node["InQueryCall"]):
    ...

class _ReadingClauseMeta(type):
    def __getitem__(cls, item):
        return object

class ReadingClause(metaclass=_ReadingClauseMeta):
    r"""ReadingClauseMatch | ReadingClauseUnwind | ReadingClauseInQueryCall"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ReadingClause")
    MATCH = hydra.core.Name("match")
    UNWIND = hydra.core.Name("unwind")
    IN_QUERY_CALL = hydra.core.Name("inQueryCall")

@dataclass(frozen=True)
class Match:
    optional: bool
    pattern: Pattern
    where: Maybe[Where]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Match")
    OPTIONAL = hydra.core.Name("optional")
    PATTERN = hydra.core.Name("pattern")
    WHERE = hydra.core.Name("where")

@dataclass(frozen=True)
class Unwind:
    expression: Expression
    variable: Variable

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Unwind")
    EXPRESSION = hydra.core.Name("expression")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class Merge:
    pattern_part: PatternPart
    actions: frozenlist[MergeAction]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Merge")
    PATTERN_PART = hydra.core.Name("patternPart")
    ACTIONS = hydra.core.Name("actions")

class MatchOrCreate(Enum):
    MATCH = hydra.core.Name("match")

    CREATE = hydra.core.Name("create")

MatchOrCreate.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MatchOrCreate")

@dataclass(frozen=True)
class MergeAction:
    action: MatchOrCreate
    set: Set

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MergeAction")
    ACTION = hydra.core.Name("action")
    SET = hydra.core.Name("set")

class Create(Node["Pattern"]):
    ...

Create.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Create")

class Set(Node["frozenlist[SetItem]"]):
    ...

Set.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Set")

class SetItemProperty(Node["PropertyEquals"]):
    ...

class SetItemVariableEqual(Node["VariableEquals"]):
    ...

class SetItemVariablePlusEqual(Node["VariablePlusEquals"]):
    ...

class SetItemVariableLabels(Node["VariableAndNodeLabels"]):
    ...

class _SetItemMeta(type):
    def __getitem__(cls, item):
        return object

class SetItem(metaclass=_SetItemMeta):
    r"""SetItemProperty | SetItemVariableEqual | SetItemVariablePlusEqual | SetItemVariableLabels"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.SetItem")
    PROPERTY = hydra.core.Name("property")
    VARIABLE_EQUAL = hydra.core.Name("variableEqual")
    VARIABLE_PLUS_EQUAL = hydra.core.Name("variablePlusEqual")
    VARIABLE_LABELS = hydra.core.Name("variableLabels")

@dataclass(frozen=True)
class PropertyEquals:
    lhs: PropertyExpression
    rhs: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PropertyEquals")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class VariableEquals:
    lhs: Variable
    rhs: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.VariableEquals")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class VariablePlusEquals:
    lhs: Variable
    rhs: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.VariablePlusEquals")
    LHS = hydra.core.Name("lhs")
    RHS = hydra.core.Name("rhs")

@dataclass(frozen=True)
class VariableAndNodeLabels:
    variable: Variable
    labels: NodeLabels

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.VariableAndNodeLabels")
    VARIABLE = hydra.core.Name("variable")
    LABELS = hydra.core.Name("labels")

@dataclass(frozen=True)
class Delete:
    detach: bool
    expressions: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Delete")
    DETACH = hydra.core.Name("detach")
    EXPRESSIONS = hydra.core.Name("expressions")

class Remove(Node["frozenlist[RemoveItem]"]):
    ...

Remove.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Remove")

class RemoveItemVariableLabels(Node["VariableAndNodeLabels"]):
    ...

class RemoveItemProperty(Node["PropertyExpression"]):
    ...

class _RemoveItemMeta(type):
    def __getitem__(cls, item):
        return object

class RemoveItem(metaclass=_RemoveItemMeta):
    r"""RemoveItemVariableLabels | RemoveItemProperty"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RemoveItem")
    VARIABLE_LABELS = hydra.core.Name("variableLabels")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class InQueryCall:
    call: ExplicitProcedureInvocation
    yield_items: Maybe[YieldItems]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.InQueryCall")
    CALL = hydra.core.Name("call")
    YIELD_ITEMS = hydra.core.Name("yieldItems")

class ProcedureInvocationExplicit(Node["ExplicitProcedureInvocation"]):
    ...

class ProcedureInvocationImplicit(Node["ImplicitProcedureInvocation"]):
    ...

class _ProcedureInvocationMeta(type):
    def __getitem__(cls, item):
        return object

class ProcedureInvocation(metaclass=_ProcedureInvocationMeta):
    r"""ProcedureInvocationExplicit | ProcedureInvocationImplicit"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ProcedureInvocation")
    EXPLICIT = hydra.core.Name("explicit")
    IMPLICIT = hydra.core.Name("implicit")

class StarOrYieldItemsStar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, StarOrYieldItemsStar)
    def __hash__(self):
        return hash("StarOrYieldItemsStar")

class StarOrYieldItemsItems(Node["YieldItems"]):
    ...

class _StarOrYieldItemsMeta(type):
    def __getitem__(cls, item):
        return object

class StarOrYieldItems(metaclass=_StarOrYieldItemsMeta):
    r"""StarOrYieldItemsStar | StarOrYieldItemsItems"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StarOrYieldItems")
    STAR = hydra.core.Name("star")
    ITEMS = hydra.core.Name("items")

@dataclass(frozen=True)
class StandaloneCall:
    call: ProcedureInvocation
    yield_items: Maybe[StarOrYieldItems]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StandaloneCall")
    CALL = hydra.core.Name("call")
    YIELD_ITEMS = hydra.core.Name("yieldItems")

@dataclass(frozen=True)
class YieldItems:
    items: frozenlist[YieldItem]
    where: Maybe[Where]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.YieldItems")
    ITEMS = hydra.core.Name("items")
    WHERE = hydra.core.Name("where")

@dataclass(frozen=True)
class YieldItem:
    field: Maybe[ProcedureResultField]
    variable: Variable

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.YieldItem")
    FIELD = hydra.core.Name("field")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class With:
    projection: ProjectionBody
    where: Maybe[Where]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.With")
    PROJECTION = hydra.core.Name("projection")
    WHERE = hydra.core.Name("where")

class Return(Node["ProjectionBody"]):
    ...

Return.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Return")

@dataclass(frozen=True)
class ProjectionBody:
    distinct: bool
    projection_items: ProjectionItems
    order: Maybe[Order]
    skip: Maybe[Skip]
    limit: Maybe[Limit]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ProjectionBody")
    DISTINCT = hydra.core.Name("distinct")
    PROJECTION_ITEMS = hydra.core.Name("projectionItems")
    ORDER = hydra.core.Name("order")
    SKIP = hydra.core.Name("skip")
    LIMIT = hydra.core.Name("limit")

@dataclass(frozen=True)
class ProjectionItems:
    star: bool
    explicit: frozenlist[ProjectionItem]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ProjectionItems")
    STAR = hydra.core.Name("star")
    EXPLICIT = hydra.core.Name("explicit")

@dataclass(frozen=True)
class ProjectionItem:
    expression: Expression
    variable: Maybe[Variable]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ProjectionItem")
    EXPRESSION = hydra.core.Name("expression")
    VARIABLE = hydra.core.Name("variable")

class Order(Node["frozenlist[SortItem]"]):
    ...

Order.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Order")

class Skip(Node["Expression"]):
    ...

Skip.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Skip")

class Limit(Node["Expression"]):
    ...

Limit.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Limit")

class SortOrder(Enum):
    ASCENDING = hydra.core.Name("ascending")

    DESCENDING = hydra.core.Name("descending")

SortOrder.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.SortOrder")

@dataclass(frozen=True)
class SortItem:
    expression: Expression
    order: Maybe[SortOrder]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.SortItem")
    EXPRESSION = hydra.core.Name("expression")
    ORDER = hydra.core.Name("order")

class Where(Node["Expression"]):
    ...

Where.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Where")

class Pattern(Node["frozenlist[PatternPart]"]):
    ...

Pattern.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Pattern")

@dataclass(frozen=True)
class PatternPart:
    variable: Maybe[Variable]
    pattern: AnonymousPatternPart

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PatternPart")
    VARIABLE = hydra.core.Name("variable")
    PATTERN = hydra.core.Name("pattern")

class AnonymousPatternPart(Node["PatternElement"]):
    ...

AnonymousPatternPart.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.AnonymousPatternPart")

@dataclass(frozen=True)
class NodePatternChain:
    node_pattern: NodePattern
    chain: frozenlist[PatternElementChain]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NodePatternChain")
    NODE_PATTERN = hydra.core.Name("nodePattern")
    CHAIN = hydra.core.Name("chain")

class PatternElementChained(Node["NodePatternChain"]):
    ...

class PatternElementParenthesized(Node["PatternElement"]):
    ...

class _PatternElementMeta(type):
    def __getitem__(cls, item):
        return object

class PatternElement(metaclass=_PatternElementMeta):
    r"""PatternElementChained | PatternElementParenthesized"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PatternElement")
    CHAINED = hydra.core.Name("chained")
    PARENTHESIZED = hydra.core.Name("parenthesized")

@dataclass(frozen=True)
class RelationshipsPattern:
    node_pattern: NodePattern
    chain: frozenlist[PatternElementChain]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RelationshipsPattern")
    NODE_PATTERN = hydra.core.Name("nodePattern")
    CHAIN = hydra.core.Name("chain")

@dataclass(frozen=True)
class NodePattern:
    variable: Maybe[Variable]
    labels: Maybe[NodeLabels]
    properties: Maybe[Properties]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NodePattern")
    VARIABLE = hydra.core.Name("variable")
    LABELS = hydra.core.Name("labels")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class PatternElementChain:
    relationship: RelationshipPattern
    node: NodePattern

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PatternElementChain")
    RELATIONSHIP = hydra.core.Name("relationship")
    NODE = hydra.core.Name("node")

@dataclass(frozen=True)
class RelationshipPattern:
    left_arrow: bool
    detail: Maybe[RelationshipDetail]
    right_arrow: bool

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RelationshipPattern")
    LEFT_ARROW = hydra.core.Name("leftArrow")
    DETAIL = hydra.core.Name("detail")
    RIGHT_ARROW = hydra.core.Name("rightArrow")

@dataclass(frozen=True)
class RelationshipDetail:
    variable: Maybe[Variable]
    types: Maybe[RelationshipTypes]
    range_: Maybe[RangeLiteral]
    properties: Maybe[Properties]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RelationshipDetail")
    VARIABLE = hydra.core.Name("variable")
    TYPES = hydra.core.Name("types")
    RANGE = hydra.core.Name("range")
    PROPERTIES = hydra.core.Name("properties")

class PropertiesMap(Node["MapLiteral"]):
    ...

class PropertiesParameter(Node["Parameter"]):
    ...

class _PropertiesMeta(type):
    def __getitem__(cls, item):
        return object

class Properties(metaclass=_PropertiesMeta):
    r"""PropertiesMap | PropertiesParameter"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Properties")
    MAP = hydra.core.Name("map")
    PARAMETER = hydra.core.Name("parameter")

class RelationshipTypes(Node["frozenlist[RelTypeName]"]):
    ...

RelationshipTypes.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RelationshipTypes")

class NodeLabels(Node["frozenlist[NodeLabel]"]):
    ...

NodeLabels.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NodeLabels")

class NodeLabel(Node[str]):
    ...

NodeLabel.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NodeLabel")

@dataclass(frozen=True)
class RangeLiteral:
    start: Maybe[int]
    end: Maybe[int]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RangeLiteral")
    START = hydra.core.Name("start")
    END = hydra.core.Name("end")

class RelTypeName(Node[str]):
    ...

RelTypeName.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RelTypeName")

@dataclass(frozen=True)
class PropertyExpression:
    atom: Atom
    lookups: frozenlist[PropertyLookup]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PropertyExpression")
    ATOM = hydra.core.Name("atom")
    LOOKUPS = hydra.core.Name("lookups")

class Expression(Node["OrExpression"]):
    ...

Expression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Expression")

class OrExpression(Node["frozenlist[XorExpression]"]):
    ...

OrExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.OrExpression")

class XorExpression(Node["frozenlist[AndExpression]"]):
    ...

XorExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.XorExpression")

class AndExpression(Node["frozenlist[NotExpression]"]):
    ...

AndExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.AndExpression")

@dataclass(frozen=True)
class NotExpression:
    not_: bool
    expression: ComparisonExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NotExpression")
    NOT = hydra.core.Name("not")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class ComparisonExpression:
    left: StringListNullPredicateExpression
    right: frozenlist[PartialComparisonExpression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ComparisonExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class ComparisonOperator(Enum):
    EQ = hydra.core.Name("eq")

    NEQ = hydra.core.Name("neq")

    LT = hydra.core.Name("lt")

    GT = hydra.core.Name("gt")

    LTE = hydra.core.Name("lte")

    GTE = hydra.core.Name("gte")

ComparisonOperator.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ComparisonOperator")

@dataclass(frozen=True)
class PartialComparisonExpression:
    operator: ComparisonOperator
    right: StringListNullPredicateExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PartialComparisonExpression")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class StringListNullPredicateExpression:
    left: AddOrSubtractExpression
    right: frozenlist[StringListNullPredicateRightHandSide]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StringListNullPredicateExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class StringListNullPredicateRightHandSideString(Node["StringPredicateExpression"]):
    ...

class StringListNullPredicateRightHandSideList(Node["ListPredicateExpression"]):
    ...

class StringListNullPredicateRightHandSideNull(Node["NullPredicateExpression"]):
    ...

class _StringListNullPredicateRightHandSideMeta(type):
    def __getitem__(cls, item):
        return object

class StringListNullPredicateRightHandSide(metaclass=_StringListNullPredicateRightHandSideMeta):
    r"""StringListNullPredicateRightHandSideString | StringListNullPredicateRightHandSideList | StringListNullPredicateRightHandSideNull"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StringListNullPredicateRightHandSide")
    STRING = hydra.core.Name("string")
    LIST = hydra.core.Name("list")
    NULL = hydra.core.Name("null")

@dataclass(frozen=True)
class StringPredicateExpression:
    operator: StringPredicateOperator
    expression: AddOrSubtractExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StringPredicateExpression")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

class StringPredicateOperator(Enum):
    STARTS_WITH = hydra.core.Name("startsWith")

    ENDS_WITH = hydra.core.Name("endsWith")

    CONTAINS = hydra.core.Name("contains")

StringPredicateOperator.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StringPredicateOperator")

class ListPredicateExpression(Node["AddOrSubtractExpression"]):
    ...

ListPredicateExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ListPredicateExpression")

class NullPredicateExpression(Node[bool]):
    ...

NullPredicateExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NullPredicateExpression")

@dataclass(frozen=True)
class AddOrSubtractExpression:
    left: MultiplyDivideModuloExpression
    right: frozenlist[AddOrSubtractRightHandSide]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.AddOrSubtractExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class AddOrSubtractRightHandSide:
    operator: AddOrSubtractOperator
    expression: MultiplyDivideModuloExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.AddOrSubtractRightHandSide")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

class AddOrSubtractOperator(Enum):
    ADD = hydra.core.Name("add")

    SUBTRACT = hydra.core.Name("subtract")

AddOrSubtractOperator.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.AddOrSubtractOperator")

@dataclass(frozen=True)
class MultiplyDivideModuloExpression:
    left: PowerOfExpression
    right: frozenlist[MultiplyDivideModuloRightHandSide]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MultiplyDivideModuloExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class MultiplyDivideModuloRightHandSide:
    operator: MultiplyDivideModuloOperator
    expression: PowerOfExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MultiplyDivideModuloRightHandSide")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

class MultiplyDivideModuloOperator(Enum):
    MULTIPLY = hydra.core.Name("multiply")

    DIVIDE = hydra.core.Name("divide")

    MODULO = hydra.core.Name("modulo")

MultiplyDivideModuloOperator.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MultiplyDivideModuloOperator")

class PowerOfExpression(Node["frozenlist[UnaryAddOrSubtractExpression]"]):
    ...

PowerOfExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PowerOfExpression")

@dataclass(frozen=True)
class UnaryAddOrSubtractExpression:
    operator: Maybe[AddOrSubtractOperator]
    expression: NonArithmeticOperatorExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.UnaryAddOrSubtractExpression")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

class ListOperatorExpressionOrPropertyLookupList(Node["ListOperatorExpression"]):
    ...

class ListOperatorExpressionOrPropertyLookupProperty(Node["PropertyLookup"]):
    ...

class _ListOperatorExpressionOrPropertyLookupMeta(type):
    def __getitem__(cls, item):
        return object

class ListOperatorExpressionOrPropertyLookup(metaclass=_ListOperatorExpressionOrPropertyLookupMeta):
    r"""ListOperatorExpressionOrPropertyLookupList | ListOperatorExpressionOrPropertyLookupProperty"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ListOperatorExpressionOrPropertyLookup")
    LIST = hydra.core.Name("list")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class NonArithmeticOperatorExpression:
    atom: Atom
    lists_and_lookups: frozenlist[ListOperatorExpressionOrPropertyLookup]
    labels: Maybe[NodeLabels]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NonArithmeticOperatorExpression")
    ATOM = hydra.core.Name("atom")
    LISTS_AND_LOOKUPS = hydra.core.Name("listsAndLookups")
    LABELS = hydra.core.Name("labels")

@dataclass(frozen=True)
class RangeExpression:
    start: Maybe[Expression]
    end: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.RangeExpression")
    START = hydra.core.Name("start")
    END = hydra.core.Name("end")

class ListOperatorExpressionSingle(Node["Expression"]):
    ...

class ListOperatorExpressionRange(Node["RangeExpression"]):
    ...

class _ListOperatorExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ListOperatorExpression(metaclass=_ListOperatorExpressionMeta):
    r"""ListOperatorExpressionSingle | ListOperatorExpressionRange"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ListOperatorExpression")
    SINGLE = hydra.core.Name("single")
    RANGE = hydra.core.Name("range")

class PropertyLookup(Node["PropertyKeyName"]):
    ...

PropertyLookup.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PropertyLookup")

class AtomLiteral(Node["Literal"]):
    ...

class AtomParameter(Node["Parameter"]):
    ...

class AtomCase(Node["CaseExpression"]):
    ...

class AtomCountStar:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AtomCountStar)
    def __hash__(self):
        return hash("AtomCountStar")

class AtomListComprehension(Node["ListComprehension"]):
    ...

class AtomPatternComprehension(Node["PatternComprehension"]):
    ...

class AtomQuantifier(Node["Quantifier"]):
    ...

class AtomPatternPredicate(Node["PatternPredicate"]):
    ...

class AtomParenthesized(Node["ParenthesizedExpression"]):
    ...

class AtomFunctionInvocation(Node["FunctionInvocation"]):
    ...

class AtomExistentialSubquery(Node["ExistentialSubquery"]):
    ...

class AtomVariable(Node["Variable"]):
    ...

class _AtomMeta(type):
    def __getitem__(cls, item):
        return object

class Atom(metaclass=_AtomMeta):
    r"""AtomLiteral | AtomParameter | AtomCase | AtomCountStar | AtomListComprehension | AtomPatternComprehension | AtomQuantifier | AtomPatternPredicate | AtomParenthesized | AtomFunctionInvocation | AtomExistentialSubquery | AtomVariable"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Atom")
    LITERAL = hydra.core.Name("literal")
    PARAMETER = hydra.core.Name("parameter")
    CASE = hydra.core.Name("case")
    COUNT_STAR = hydra.core.Name("countStar")
    LIST_COMPREHENSION = hydra.core.Name("listComprehension")
    PATTERN_COMPREHENSION = hydra.core.Name("patternComprehension")
    QUANTIFIER = hydra.core.Name("quantifier")
    PATTERN_PREDICATE = hydra.core.Name("patternPredicate")
    PARENTHESIZED = hydra.core.Name("parenthesized")
    FUNCTION_INVOCATION = hydra.core.Name("functionInvocation")
    EXISTENTIAL_SUBQUERY = hydra.core.Name("existentialSubquery")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class CaseExpression:
    expression: Maybe[Expression]
    alternatives: frozenlist[CaseAlternative]
    else_: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.CaseExpression")
    EXPRESSION = hydra.core.Name("expression")
    ALTERNATIVES = hydra.core.Name("alternatives")
    ELSE = hydra.core.Name("else")

@dataclass(frozen=True)
class CaseAlternative:
    condition: Expression
    result: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.CaseAlternative")
    CONDITION = hydra.core.Name("condition")
    RESULT = hydra.core.Name("result")

@dataclass(frozen=True)
class ListComprehension:
    left: FilterExpression
    right: Maybe[Expression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ListComprehension")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class PatternComprehension:
    variable: Maybe[Variable]
    pattern: RelationshipsPattern
    where: Maybe[Where]
    right: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PatternComprehension")
    VARIABLE = hydra.core.Name("variable")
    PATTERN = hydra.core.Name("pattern")
    WHERE = hydra.core.Name("where")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class Quantifier:
    operator: QuantifierOperator
    expression: FilterExpression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Quantifier")
    OPERATOR = hydra.core.Name("operator")
    EXPRESSION = hydra.core.Name("expression")

class QuantifierOperator(Enum):
    ALL = hydra.core.Name("all")

    ANY = hydra.core.Name("any")

    NONE = hydra.core.Name("none")

    SINGLE = hydra.core.Name("single")

QuantifierOperator.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.QuantifierOperator")

@dataclass(frozen=True)
class FilterExpression:
    id_in_coll: IdInColl
    where: Maybe[Where]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.FilterExpression")
    ID_IN_COLL = hydra.core.Name("idInColl")
    WHERE = hydra.core.Name("where")

class PatternPredicate(Node["RelationshipsPattern"]):
    ...

PatternPredicate.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PatternPredicate")

class ParenthesizedExpression(Node["Expression"]):
    ...

ParenthesizedExpression.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ParenthesizedExpression")

@dataclass(frozen=True)
class IdInColl:
    variable: Variable
    expression: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.IdInColl")
    VARIABLE = hydra.core.Name("variable")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class FunctionInvocation:
    name: QualifiedName
    distinct: bool
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.FunctionInvocation")
    NAME = hydra.core.Name("name")
    DISTINCT = hydra.core.Name("distinct")
    ARGUMENTS = hydra.core.Name("arguments")

@dataclass(frozen=True)
class QualifiedName:
    namespace: str
    local: str

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.QualifiedName")
    NAMESPACE = hydra.core.Name("namespace")
    LOCAL = hydra.core.Name("local")

@dataclass(frozen=True)
class PatternWhere:
    pattern: Pattern
    where: Maybe[Where]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PatternWhere")
    PATTERN = hydra.core.Name("pattern")
    WHERE = hydra.core.Name("where")

class ExistentialSubqueryRegular(Node["RegularQuery"]):
    ...

class ExistentialSubqueryPattern(Node["PatternWhere"]):
    ...

class _ExistentialSubqueryMeta(type):
    def __getitem__(cls, item):
        return object

class ExistentialSubquery(metaclass=_ExistentialSubqueryMeta):
    r"""ExistentialSubqueryRegular | ExistentialSubqueryPattern"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ExistentialSubquery")
    REGULAR = hydra.core.Name("regular")
    PATTERN = hydra.core.Name("pattern")

@dataclass(frozen=True)
class ExplicitProcedureInvocation:
    name: QualifiedName
    arguments: frozenlist[Expression]

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ExplicitProcedureInvocation")
    NAME = hydra.core.Name("name")
    ARGUMENTS = hydra.core.Name("arguments")

class ImplicitProcedureInvocation(Node["QualifiedName"]):
    ...

ImplicitProcedureInvocation.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ImplicitProcedureInvocation")

class ProcedureResultField(Node[str]):
    ...

ProcedureResultField.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ProcedureResultField")

class Variable(Node[str]):
    ...

Variable.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Variable")

class LiteralBoolean(Node[bool]):
    ...

class LiteralNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LiteralNull)
    def __hash__(self):
        return hash("LiteralNull")

class LiteralNumber(Node["NumberLiteral"]):
    ...

class LiteralString(Node["StringLiteral"]):
    ...

class LiteralList(Node["ListLiteral"]):
    ...

class LiteralMap(Node["MapLiteral"]):
    ...

class _LiteralMeta(type):
    def __getitem__(cls, item):
        return object

class Literal(metaclass=_LiteralMeta):
    r"""LiteralBoolean | LiteralNull | LiteralNumber | LiteralString | LiteralList | LiteralMap"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Literal")
    BOOLEAN = hydra.core.Name("boolean")
    NULL = hydra.core.Name("null")
    NUMBER = hydra.core.Name("number")
    STRING = hydra.core.Name("string")
    LIST = hydra.core.Name("list")
    MAP = hydra.core.Name("map")

class NumberLiteralDouble(Node[float]):
    ...

class NumberLiteralInteger(Node[int]):
    ...

class _NumberLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class NumberLiteral(metaclass=_NumberLiteralMeta):
    r"""NumberLiteralDouble | NumberLiteralInteger"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.NumberLiteral")
    DOUBLE = hydra.core.Name("double")
    INTEGER = hydra.core.Name("integer")

class StringLiteral(Node[str]):
    ...

StringLiteral.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.StringLiteral")

class ListLiteral(Node["frozenlist[Expression]"]):
    ...

ListLiteral.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.ListLiteral")

class MapLiteral(Node["frozenlist[KeyValuePair]"]):
    ...

MapLiteral.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.MapLiteral")

@dataclass(frozen=True)
class KeyValuePair:
    key: PropertyKeyName
    value: Expression

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.KeyValuePair")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class PropertyKeyName(Node[str]):
    ...

PropertyKeyName.TYPE_ = hydra.core.Name("hydra.cypher.openCypher.PropertyKeyName")

class ParameterSymbolic(Node[str]):
    ...

class ParameterInteger(Node[int]):
    ...

class _ParameterMeta(type):
    def __getitem__(cls, item):
        return object

class Parameter(metaclass=_ParameterMeta):
    r"""ParameterSymbolic | ParameterInteger"""

    TYPE_ = hydra.core.Name("hydra.cypher.openCypher.Parameter")
    SYMBOLIC = hydra.core.Name("symbolic")
    INTEGER = hydra.core.Name("integer")
