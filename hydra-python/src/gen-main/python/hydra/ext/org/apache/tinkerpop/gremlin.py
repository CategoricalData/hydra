# Note: this is an automatically generated file. Do not edit.

r"""A Gremlin model, based on the Gremlin ANTLR grammar (master branch, as of 2024-06-30)."""

from __future__ import annotations
from dataclasses import dataclass
from decimal import Decimal
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

class QueryList(Node["frozenlist[Query]"]):
    ...

QueryList.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.QueryList")

class QueryTraversalSource(Node["TraversalSourceQuery"]):
    ...

class QueryRootTraversal(Node["RootTraversalQuery"]):
    ...

class QueryToString:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, QueryToString)
    def __hash__(self):
        return hash("QueryToString")

class QueryEmpty:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, QueryEmpty)
    def __hash__(self):
        return hash("QueryEmpty")

class _QueryMeta(type):
    def __getitem__(cls, item):
        return object

class Query(metaclass=_QueryMeta):
    r"""QueryTraversalSource | QueryRootTraversal | QueryToString | QueryEmpty"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.Query")
    TRAVERSAL_SOURCE = hydra.core.Name("traversalSource")
    ROOT_TRAVERSAL = hydra.core.Name("rootTraversal")
    TO_STRING = hydra.core.Name("toString")
    EMPTY = hydra.core.Name("empty")

@dataclass(frozen=True)
class TraversalSourceQuery:
    source: TraversalSource
    transaction_part: Maybe[TransactionPart]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceQuery")
    SOURCE = hydra.core.Name("source")
    TRANSACTION_PART = hydra.core.Name("transactionPart")

@dataclass(frozen=True)
class RootTraversalQuery:
    root: RootTraversal
    terminal_method: Maybe[TraversalTerminalMethod]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RootTraversalQuery")
    ROOT = hydra.core.Name("root")
    TERMINAL_METHOD = hydra.core.Name("terminalMethod")

class TraversalSource(Node["frozenlist[TraversalSourceSelfMethod]"]):
    ...

TraversalSource.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSource")

class TransactionPart(Enum):
    BEGIN = hydra.core.Name("begin")

    COMMIT = hydra.core.Name("commit")

    ROLLBACK = hydra.core.Name("rollback")

TransactionPart.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TransactionPart")

@dataclass(frozen=True)
class RootTraversal:
    source: TraversalSource
    spawn_method: TraversalSourceSpawnMethod
    chained: frozenlist[ChainedTraversalElement]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RootTraversal")
    SOURCE = hydra.core.Name("source")
    SPAWN_METHOD = hydra.core.Name("spawnMethod")
    CHAINED = hydra.core.Name("chained")

class TraversalSourceSelfMethodWithBulk(Node[bool]):
    ...

class TraversalSourceSelfMethodWithPath:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalSourceSelfMethodWithPath)
    def __hash__(self):
        return hash("TraversalSourceSelfMethodWithPath")

class TraversalSourceSelfMethodWithSack(Node["GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument"]):
    ...

class TraversalSourceSelfMethodWithSideEffect(Node["StringArgumentAndGenericLiteralArgument"]):
    ...

class TraversalSourceSelfMethodWithStrategies(Node["frozenlist[TraversalStrategy]"]):
    ...

class TraversalSourceSelfMethodWithoutStrategies(Node["frozenlist[Identifier]"]):
    ...

class TraversalSourceSelfMethodWith(Node["StringArgumentAndOptionalGenericLiteralArgument"]):
    ...

class _TraversalSourceSelfMethodMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalSourceSelfMethod(metaclass=_TraversalSourceSelfMethodMeta):
    r"""TraversalSourceSelfMethodWithBulk | TraversalSourceSelfMethodWithPath | TraversalSourceSelfMethodWithSack | TraversalSourceSelfMethodWithSideEffect | TraversalSourceSelfMethodWithStrategies | TraversalSourceSelfMethodWithoutStrategies | TraversalSourceSelfMethodWith"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSelfMethod")
    WITH_BULK = hydra.core.Name("withBulk")
    WITH_PATH = hydra.core.Name("withPath")
    WITH_SACK = hydra.core.Name("withSack")
    WITH_SIDE_EFFECT = hydra.core.Name("withSideEffect")
    WITH_STRATEGIES = hydra.core.Name("withStrategies")
    WITHOUT_STRATEGIES = hydra.core.Name("withoutStrategies")
    WITH = hydra.core.Name("with")

@dataclass(frozen=True)
class GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument:
    literal: GenericLiteralArgument
    bi_function: Maybe[TraversalBiFunctionArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndOptionalTraversalBiFunctionArgument")
    LITERAL = hydra.core.Name("literal")
    BI_FUNCTION = hydra.core.Name("biFunction")

@dataclass(frozen=True)
class StringArgumentAndGenericLiteralArgument:
    string: StringArgument
    literal: GenericLiteralArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndGenericLiteralArgument")
    STRING = hydra.core.Name("string")
    LITERAL = hydra.core.Name("literal")

@dataclass(frozen=True)
class StringArgumentAndOptionalGenericLiteralArgument:
    string: StringArgument
    literal: Maybe[GenericLiteralArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalGenericLiteralArgument")
    STRING = hydra.core.Name("string")
    LITERAL = hydra.core.Name("literal")

class TraversalSourceSpawnMethodAddE(Node["StringArgumentOrNestedTraversal"]):
    ...

class TraversalSourceSpawnMethodAddV(Node["Maybe[StringArgumentOrNestedTraversal]"]):
    ...

class TraversalSourceSpawnMethodE(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class TraversalSourceSpawnMethodV(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class TraversalSourceSpawnMethodMergeV(Node["GenericLiteralMapNullableArgumentOrNestedTraversal"]):
    ...

class TraversalSourceSpawnMethodMergeE(Node["GenericLiteralMapNullableArgumentOrNestedTraversal"]):
    ...

class TraversalSourceSpawnMethodInject(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class TraversalSourceSpawnMethodIo(Node["StringArgument"]):
    ...

class TraversalSourceSpawnMethodCall(Node["Maybe[ServiceCall]"]):
    ...

class TraversalSourceSpawnMethodUnion(Node["frozenlist[NestedTraversal]"]):
    ...

class _TraversalSourceSpawnMethodMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalSourceSpawnMethod(metaclass=_TraversalSourceSpawnMethodMeta):
    r"""TraversalSourceSpawnMethodAddE | TraversalSourceSpawnMethodAddV | TraversalSourceSpawnMethodE | TraversalSourceSpawnMethodV | TraversalSourceSpawnMethodMergeV | TraversalSourceSpawnMethodMergeE | TraversalSourceSpawnMethodInject | TraversalSourceSpawnMethodIo | TraversalSourceSpawnMethodCall | TraversalSourceSpawnMethodUnion"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSourceSpawnMethod")
    ADD_E = hydra.core.Name("addE")
    ADD_V = hydra.core.Name("addV")
    E = hydra.core.Name("e")
    V = hydra.core.Name("v")
    MERGE_V = hydra.core.Name("mergeV")
    MERGE_E = hydra.core.Name("mergeE")
    INJECT = hydra.core.Name("inject")
    IO = hydra.core.Name("io")
    CALL = hydra.core.Name("call")
    UNION = hydra.core.Name("union")

class GenericLiteralMapNullableArgumentOrNestedTraversalMap(Node["GenericLiteralMapNullableArgument"]):
    ...

class GenericLiteralMapNullableArgumentOrNestedTraversalTraversal(Node["NestedTraversal"]):
    ...

class _GenericLiteralMapNullableArgumentOrNestedTraversalMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralMapNullableArgumentOrNestedTraversal(metaclass=_GenericLiteralMapNullableArgumentOrNestedTraversalMeta):
    r"""GenericLiteralMapNullableArgumentOrNestedTraversalMap | GenericLiteralMapNullableArgumentOrNestedTraversalTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal")
    MAP = hydra.core.Name("map")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class ServiceCall:
    service: StringArgument
    arguments: ServiceArguments

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall")
    SERVICE = hydra.core.Name("service")
    ARGUMENTS = hydra.core.Name("arguments")

class ServiceArgumentsMap(Node["Maybe[GenericLiteralMapArgument]"]):
    ...

class ServiceArgumentsTraversal(Node["Maybe[NestedTraversal]"]):
    ...

class _ServiceArgumentsMeta(type):
    def __getitem__(cls, item):
        return object

class ServiceArguments(metaclass=_ServiceArgumentsMeta):
    r"""ServiceArgumentsMap | ServiceArgumentsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ServiceArguments")
    MAP = hydra.core.Name("map")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class ChainedTraversal:
    first: TraversalMethod
    rest: ChainedTraversalElement

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversal")
    FIRST = hydra.core.Name("first")
    REST = hydra.core.Name("rest")

class ChainedTraversalElementMethod(Node["TraversalMethod"]):
    ...

class ChainedTraversalElementSelf(Node["TraversalSelfMethod"]):
    ...

class _ChainedTraversalElementMeta(type):
    def __getitem__(cls, item):
        return object

class ChainedTraversalElement(metaclass=_ChainedTraversalElementMeta):
    r"""ChainedTraversalElementMethod | ChainedTraversalElementSelf"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ChainedTraversalElement")
    METHOD = hydra.core.Name("method")
    SELF = hydra.core.Name("self")

class NestedTraversalRoot(Node["RootTraversal"]):
    ...

class NestedTraversalChained(Node["ChainedTraversal"]):
    ...

class NestedTraversalAnonymous(Node["ChainedTraversal"]):
    ...

class _NestedTraversalMeta(type):
    def __getitem__(cls, item):
        return object

class NestedTraversal(metaclass=_NestedTraversalMeta):
    r"""NestedTraversalRoot | NestedTraversalChained | NestedTraversalAnonymous"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal")
    ROOT = hydra.core.Name("root")
    CHAINED = hydra.core.Name("chained")
    ANONYMOUS = hydra.core.Name("anonymous")

@dataclass(frozen=True)
class TerminatedTraversal:
    root: RootTraversal
    terminal: TraversalTerminalMethod

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TerminatedTraversal")
    ROOT = hydra.core.Name("root")
    TERMINAL = hydra.core.Name("terminal")

class TraversalMethodV(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class TraversalMethodE(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class TraversalMethodAddE(Node["StringArgumentOrNestedTraversal"]):
    ...

class TraversalMethodAddV(Node["Maybe[StringArgumentOrNestedTraversal]"]):
    ...

class TraversalMethodMergeE(Node["Maybe[GenericLiteralMapNullableArgumentOrNestedTraversal]"]):
    ...

class TraversalMethodMergeV(Node["Maybe[GenericLiteralMapNullableArgumentOrNestedTraversal]"]):
    ...

class TraversalMethodAggregate(Node["OptionalTraversalScopeArgumentAndStringArgument"]):
    ...

class TraversalMethodAll(Node["TraversalPredicate"]):
    ...

class TraversalMethodAnd(Node["frozenlist[NestedTraversal]"]):
    ...

class TraversalMethodAny(Node["TraversalPredicate"]):
    ...

class TraversalMethodAs(Node["StringArgumentAndOptionalStringLiteralVarargs"]):
    ...

class TraversalMethodBarrier(Node["Maybe[TraversalSackMethodArgumentOrIntegerArgument]"]):
    ...

class TraversalMethodBoth(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodBothE(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodBothV:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodBothV)
    def __hash__(self):
        return hash("TraversalMethodBothV")

class TraversalMethodBranch(Node["NestedTraversal"]):
    ...

class TraversalMethodBy(Node["ByArgs"]):
    ...

class TraversalMethodCap(Node["StringArgumentAndOptionalStringLiteralVarargs"]):
    ...

class TraversalMethodChoose(Node["ChooseArgs"]):
    ...

class TraversalMethodCoalesce(Node["frozenlist[NestedTraversal]"]):
    ...

class TraversalMethodCoin(Node["FloatArgument"]):
    ...

class TraversalMethodConjoin(Node["StringArgument"]):
    ...

class TraversalMethodConnectedComponent:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodConnectedComponent)
    def __hash__(self):
        return hash("TraversalMethodConnectedComponent")

class TraversalMethodConstant(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodCount(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodCyclicPath:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodCyclicPath)
    def __hash__(self):
        return hash("TraversalMethodCyclicPath")

class TraversalMethodDedup(Node["DedupArgs"]):
    ...

class TraversalMethodDifference(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodDisjunct(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodDrop:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodDrop)
    def __hash__(self):
        return hash("TraversalMethodDrop")

class TraversalMethodElementMap(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodEmit(Node["Maybe[PredicateOrTraversal]"]):
    ...

class TraversalMethodFilter(Node["PredicateOrTraversal"]):
    ...

class TraversalMethodFlatMap(Node["NestedTraversal"]):
    ...

class TraversalMethodFold(Node["Maybe[GenericLiteralArgumentAndTraversalBiFunctionArgument]"]):
    ...

class TraversalMethodFrom(Node["FromArgs"]):
    ...

class TraversalMethodGroup(Node["Maybe[StringArgument]"]):
    ...

class TraversalMethodGroupCount(Node["Maybe[StringArgument]"]):
    ...

class TraversalMethodHas(Node["HasArgs"]):
    ...

class TraversalMethodHasId(Node["GenericLiteralArgumentAndTraversalPredicate"]):
    ...

class TraversalMethodHasKey(Node["TraversalPredicateOrStringLiteralVarargs"]):
    ...

class TraversalMethodHasLabel(Node["TraversalPredicateOrStringLiteralVarargs"]):
    ...

class TraversalMethodHasNot(Node["StringNullableArgument"]):
    ...

class TraversalMethodHasValue(Node["TraversalPredicateOrGenericLiteralArgument"]):
    ...

class TraversalMethodId:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodId)
    def __hash__(self):
        return hash("TraversalMethodId")

class TraversalMethodIdentity:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodIdentity)
    def __hash__(self):
        return hash("TraversalMethodIdentity")

class TraversalMethodIn(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodInE(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodIntersect(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodInV:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodInV)
    def __hash__(self):
        return hash("TraversalMethodInV")

class TraversalMethodIndex:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodIndex)
    def __hash__(self):
        return hash("TraversalMethodIndex")

class TraversalMethodInject(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class TraversalMethodIs(Node["TraversalPredicateOrGenericLiteralArgument"]):
    ...

class TraversalMethodKey:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodKey)
    def __hash__(self):
        return hash("TraversalMethodKey")

class TraversalMethodLabel:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodLabel)
    def __hash__(self):
        return hash("TraversalMethodLabel")

class TraversalMethodLimit(Node["OptionalTraversalScopeArgumentAndIntegerArgument"]):
    ...

class TraversalMethodLocal(Node["NestedTraversal"]):
    ...

class TraversalMethodLoops(Node["Maybe[StringArgument]"]):
    ...

class TraversalMethodMap(Node["NestedTraversal"]):
    ...

class TraversalMethodMatch(Node["frozenlist[NestedTraversal]"]):
    ...

class TraversalMethodMath(Node["StringArgument"]):
    ...

class TraversalMethodMax(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodMean(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodMin(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodNone(Node["TraversalPredicate"]):
    ...

class TraversalMethodNot(Node["NestedTraversal"]):
    ...

class TraversalMethodOption(Node["OptionArgs"]):
    ...

class TraversalMethodOptional(Node["NestedTraversal"]):
    ...

class TraversalMethodOr(Node["frozenlist[NestedTraversal]"]):
    ...

class TraversalMethodOrder(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodOtherV:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodOtherV)
    def __hash__(self):
        return hash("TraversalMethodOtherV")

class TraversalMethodOut(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodOutE(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodOutV:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodOutV)
    def __hash__(self):
        return hash("TraversalMethodOutV")

class TraversalMethodPageRank(Node["Maybe[FloatArgument]"]):
    ...

class TraversalMethodPath:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodPath)
    def __hash__(self):
        return hash("TraversalMethodPath")

class TraversalMethodPeerPressure:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodPeerPressure)
    def __hash__(self):
        return hash("TraversalMethodPeerPressure")

class TraversalMethodProfile(Node["Maybe[StringArgument]"]):
    ...

class TraversalMethodProject(Node["StringArgumentAndOptionalStringLiteralVarargs"]):
    ...

class TraversalMethodProperties(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodProperty(Node["PropertyArgs"]):
    ...

class TraversalMethodPropertyMap(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodRange(Node["RangeArgs"]):
    ...

class TraversalMethodRead:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodRead)
    def __hash__(self):
        return hash("TraversalMethodRead")

class TraversalMethodRepeat(Node["OptionalStringArgumentAndNestedTraversal"]):
    ...

class TraversalMethodSack(Node["Maybe[TraversalBiFunctionArgument]"]):
    ...

class TraversalMethodSample(Node["OptionalTraversalScopeArgumentAndIntegerArgument"]):
    ...

class TraversalMethodSelect(Node["SelectArgs"]):
    ...

class TraversalMethodCombine(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodProduct(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodMerge(Node["GenericLiteralArgument"]):
    ...

class TraversalMethodShortestPath:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodShortestPath)
    def __hash__(self):
        return hash("TraversalMethodShortestPath")

class TraversalMethodSideEffect(Node["NestedTraversal"]):
    ...

class TraversalMethodSimplePath:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodSimplePath)
    def __hash__(self):
        return hash("TraversalMethodSimplePath")

class TraversalMethodSkip(Node["OptionalTraversalScopeArgumentAndIntegerArgument"]):
    ...

class TraversalMethodStore(Node["StringArgument"]):
    ...

class TraversalMethodSubgraph(Node["StringArgument"]):
    ...

class TraversalMethodSum(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodTail(Node["Maybe[TailArgs]"]):
    ...

class TraversalMethodFail(Node["Maybe[StringArgument]"]):
    ...

class TraversalMethodTimes(Node["IntegerArgument"]):
    ...

class TraversalMethodTo(Node["ToArgs"]):
    ...

class TraversalMethodToE(Node["DirectionAndVarargs"]):
    ...

class TraversalMethodToV(Node["TraversalDirectionArgument"]):
    ...

class TraversalMethodTree(Node["Maybe[StringArgument]"]):
    ...

class TraversalMethodUnfold:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodUnfold)
    def __hash__(self):
        return hash("TraversalMethodUnfold")

class TraversalMethodUnion(Node["frozenlist[NestedTraversal]"]):
    ...

class TraversalMethodUntil(Node["PredicateOrTraversal"]):
    ...

class TraversalMethodValue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodValue)
    def __hash__(self):
        return hash("TraversalMethodValue")

class TraversalMethodValueMap(Node["ValueMapArgs"]):
    ...

class TraversalMethodValues(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodWhere(Node["WhereArgs"]):
    ...

class TraversalMethodWith(Node["WithArgs"]):
    ...

class TraversalMethodWrite:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodWrite)
    def __hash__(self):
        return hash("TraversalMethodWrite")

class TraversalMethodElement(Node["frozenlist[StringNullableArgument]"]):
    ...

class TraversalMethodCall(Node["ServiceCall"]):
    ...

class TraversalMethodConcat(Node["ConcatArgs"]):
    ...

class TraversalMethodAsString(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodFormat(Node["StringArgument"]):
    ...

class TraversalMethodToUpper(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodToLower(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodLength(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodTrim(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodLTrim(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodRTrim(Node["Maybe[TraversalScopeArgument]"]):
    ...

class TraversalMethodReverse:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodReverse)
    def __hash__(self):
        return hash("TraversalMethodReverse")

class TraversalMethodReplace(Node["ReplaceArgs"]):
    ...

class TraversalMethodSplit(Node["SplitArgs"]):
    ...

class TraversalMethodSubstring(Node["SubstringArgs"]):
    ...

class TraversalMethodAsDate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalMethodAsDate)
    def __hash__(self):
        return hash("TraversalMethodAsDate")

class TraversalMethodDateAdd(Node["DateAddArgs"]):
    ...

class TraversalMethodDateDiff(Node["DateDiffArgs"]):
    ...

class _TraversalMethodMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalMethod(metaclass=_TraversalMethodMeta):
    r"""TraversalMethodV | TraversalMethodE | TraversalMethodAddE | TraversalMethodAddV | TraversalMethodMergeE | TraversalMethodMergeV | TraversalMethodAggregate | TraversalMethodAll | TraversalMethodAnd | TraversalMethodAny | TraversalMethodAs | TraversalMethodBarrier | TraversalMethodBoth | TraversalMethodBothE | TraversalMethodBothV | TraversalMethodBranch | TraversalMethodBy | TraversalMethodCap | TraversalMethodChoose | TraversalMethodCoalesce | TraversalMethodCoin | TraversalMethodConjoin | TraversalMethodConnectedComponent | TraversalMethodConstant | TraversalMethodCount | TraversalMethodCyclicPath | TraversalMethodDedup | TraversalMethodDifference | TraversalMethodDisjunct | TraversalMethodDrop | TraversalMethodElementMap | TraversalMethodEmit | TraversalMethodFilter | TraversalMethodFlatMap | TraversalMethodFold | TraversalMethodFrom | TraversalMethodGroup | TraversalMethodGroupCount | TraversalMethodHas | TraversalMethodHasId | TraversalMethodHasKey | TraversalMethodHasLabel | TraversalMethodHasNot | TraversalMethodHasValue | TraversalMethodId | TraversalMethodIdentity | TraversalMethodIn | TraversalMethodInE | TraversalMethodIntersect | TraversalMethodInV | TraversalMethodIndex | TraversalMethodInject | TraversalMethodIs | TraversalMethodKey | TraversalMethodLabel | TraversalMethodLimit | TraversalMethodLocal | TraversalMethodLoops | TraversalMethodMap | TraversalMethodMatch | TraversalMethodMath | TraversalMethodMax | TraversalMethodMean | TraversalMethodMin | TraversalMethodNone | TraversalMethodNot | TraversalMethodOption | TraversalMethodOptional | TraversalMethodOr | TraversalMethodOrder | TraversalMethodOtherV | TraversalMethodOut | TraversalMethodOutE | TraversalMethodOutV | TraversalMethodPageRank | TraversalMethodPath | TraversalMethodPeerPressure | TraversalMethodProfile | TraversalMethodProject | TraversalMethodProperties | TraversalMethodProperty | TraversalMethodPropertyMap | TraversalMethodRange | TraversalMethodRead | TraversalMethodRepeat | TraversalMethodSack | TraversalMethodSample | TraversalMethodSelect | TraversalMethodCombine | TraversalMethodProduct | TraversalMethodMerge | TraversalMethodShortestPath | TraversalMethodSideEffect | TraversalMethodSimplePath | TraversalMethodSkip | TraversalMethodStore | TraversalMethodSubgraph | TraversalMethodSum | TraversalMethodTail | TraversalMethodFail | TraversalMethodTimes | TraversalMethodTo | TraversalMethodToE | TraversalMethodToV | TraversalMethodTree | TraversalMethodUnfold | TraversalMethodUnion | TraversalMethodUntil | TraversalMethodValue | TraversalMethodValueMap | TraversalMethodValues | TraversalMethodWhere | TraversalMethodWith | TraversalMethodWrite | TraversalMethodElement | TraversalMethodCall | TraversalMethodConcat | TraversalMethodAsString | TraversalMethodFormat | TraversalMethodToUpper | TraversalMethodToLower | TraversalMethodLength | TraversalMethodTrim | TraversalMethodLTrim | TraversalMethodRTrim | TraversalMethodReverse | TraversalMethodReplace | TraversalMethodSplit | TraversalMethodSubstring | TraversalMethodAsDate | TraversalMethodDateAdd | TraversalMethodDateDiff"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod")
    V = hydra.core.Name("v")
    E = hydra.core.Name("e")
    ADD_E = hydra.core.Name("addE")
    ADD_V = hydra.core.Name("addV")
    MERGE_E = hydra.core.Name("mergeE")
    MERGE_V = hydra.core.Name("mergeV")
    AGGREGATE = hydra.core.Name("aggregate")
    ALL = hydra.core.Name("all")
    AND = hydra.core.Name("and")
    ANY = hydra.core.Name("any")
    AS = hydra.core.Name("as")
    BARRIER = hydra.core.Name("barrier")
    BOTH = hydra.core.Name("both")
    BOTH_E = hydra.core.Name("bothE")
    BOTH_V = hydra.core.Name("bothV")
    BRANCH = hydra.core.Name("branch")
    BY = hydra.core.Name("by")
    CAP = hydra.core.Name("cap")
    CHOOSE = hydra.core.Name("choose")
    COALESCE = hydra.core.Name("coalesce")
    COIN = hydra.core.Name("coin")
    CONJOIN = hydra.core.Name("conjoin")
    CONNECTED_COMPONENT = hydra.core.Name("connectedComponent")
    CONSTANT = hydra.core.Name("constant")
    COUNT = hydra.core.Name("count")
    CYCLIC_PATH = hydra.core.Name("cyclicPath")
    DEDUP = hydra.core.Name("dedup")
    DIFFERENCE = hydra.core.Name("difference")
    DISJUNCT = hydra.core.Name("disjunct")
    DROP = hydra.core.Name("drop")
    ELEMENT_MAP = hydra.core.Name("elementMap")
    EMIT = hydra.core.Name("emit")
    FILTER = hydra.core.Name("filter")
    FLAT_MAP = hydra.core.Name("flatMap")
    FOLD = hydra.core.Name("fold")
    FROM = hydra.core.Name("from")
    GROUP = hydra.core.Name("group")
    GROUP_COUNT = hydra.core.Name("groupCount")
    HAS = hydra.core.Name("has")
    HAS_ID = hydra.core.Name("hasId")
    HAS_KEY = hydra.core.Name("hasKey")
    HAS_LABEL = hydra.core.Name("hasLabel")
    HAS_NOT = hydra.core.Name("hasNot")
    HAS_VALUE = hydra.core.Name("hasValue")
    ID = hydra.core.Name("id")
    IDENTITY = hydra.core.Name("identity")
    IN = hydra.core.Name("in")
    IN_E = hydra.core.Name("inE")
    INTERSECT = hydra.core.Name("intersect")
    IN_V = hydra.core.Name("inV")
    INDEX = hydra.core.Name("index")
    INJECT = hydra.core.Name("inject")
    IS = hydra.core.Name("is")
    KEY = hydra.core.Name("key")
    LABEL = hydra.core.Name("label")
    LIMIT = hydra.core.Name("limit")
    LOCAL = hydra.core.Name("local")
    LOOPS = hydra.core.Name("loops")
    MAP = hydra.core.Name("map")
    MATCH = hydra.core.Name("match")
    MATH = hydra.core.Name("math")
    MAX = hydra.core.Name("max")
    MEAN = hydra.core.Name("mean")
    MIN = hydra.core.Name("min")
    NONE = hydra.core.Name("none")
    NOT = hydra.core.Name("not")
    OPTION = hydra.core.Name("option")
    OPTIONAL = hydra.core.Name("optional")
    OR = hydra.core.Name("or")
    ORDER = hydra.core.Name("order")
    OTHER_V = hydra.core.Name("otherV")
    OUT = hydra.core.Name("out")
    OUT_E = hydra.core.Name("outE")
    OUT_V = hydra.core.Name("outV")
    PAGE_RANK = hydra.core.Name("pageRank")
    PATH = hydra.core.Name("path")
    PEER_PRESSURE = hydra.core.Name("peerPressure")
    PROFILE = hydra.core.Name("profile")
    PROJECT = hydra.core.Name("project")
    PROPERTIES = hydra.core.Name("properties")
    PROPERTY = hydra.core.Name("property")
    PROPERTY_MAP = hydra.core.Name("propertyMap")
    RANGE = hydra.core.Name("range")
    READ = hydra.core.Name("read")
    REPEAT = hydra.core.Name("repeat")
    SACK = hydra.core.Name("sack")
    SAMPLE = hydra.core.Name("sample")
    SELECT = hydra.core.Name("select")
    COMBINE = hydra.core.Name("combine")
    PRODUCT = hydra.core.Name("product")
    MERGE = hydra.core.Name("merge")
    SHORTEST_PATH = hydra.core.Name("shortestPath")
    SIDE_EFFECT = hydra.core.Name("sideEffect")
    SIMPLE_PATH = hydra.core.Name("simplePath")
    SKIP = hydra.core.Name("skip")
    STORE = hydra.core.Name("store")
    SUBGRAPH = hydra.core.Name("subgraph")
    SUM = hydra.core.Name("sum")
    TAIL = hydra.core.Name("tail")
    FAIL = hydra.core.Name("fail")
    TIMES = hydra.core.Name("times")
    TO = hydra.core.Name("to")
    TO_E = hydra.core.Name("toE")
    TO_V = hydra.core.Name("toV")
    TREE = hydra.core.Name("tree")
    UNFOLD = hydra.core.Name("unfold")
    UNION = hydra.core.Name("union")
    UNTIL = hydra.core.Name("until")
    VALUE = hydra.core.Name("value")
    VALUE_MAP = hydra.core.Name("valueMap")
    VALUES = hydra.core.Name("values")
    WHERE = hydra.core.Name("where")
    WITH = hydra.core.Name("with")
    WRITE = hydra.core.Name("write")
    ELEMENT = hydra.core.Name("element")
    CALL = hydra.core.Name("call")
    CONCAT = hydra.core.Name("concat")
    AS_STRING = hydra.core.Name("asString")
    FORMAT = hydra.core.Name("format")
    TO_UPPER = hydra.core.Name("toUpper")
    TO_LOWER = hydra.core.Name("toLower")
    LENGTH = hydra.core.Name("length")
    TRIM = hydra.core.Name("trim")
    L_TRIM = hydra.core.Name("lTrim")
    R_TRIM = hydra.core.Name("rTrim")
    REVERSE = hydra.core.Name("reverse")
    REPLACE = hydra.core.Name("replace")
    SPLIT = hydra.core.Name("split")
    SUBSTRING = hydra.core.Name("substring")
    AS_DATE = hydra.core.Name("asDate")
    DATE_ADD = hydra.core.Name("dateAdd")
    DATE_DIFF = hydra.core.Name("dateDiff")

class StringArgumentOrNestedTraversalString(Node["StringArgument"]):
    ...

class StringArgumentOrNestedTraversalTraversal(Node["NestedTraversal"]):
    ...

class _StringArgumentOrNestedTraversalMeta(type):
    def __getitem__(cls, item):
        return object

class StringArgumentOrNestedTraversal(metaclass=_StringArgumentOrNestedTraversalMeta):
    r"""StringArgumentOrNestedTraversalString | StringArgumentOrNestedTraversalTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal")
    STRING = hydra.core.Name("string")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class OptionalTraversalScopeArgumentAndStringArgument:
    scope: Maybe[TraversalScopeArgument]
    string: StringArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument")
    SCOPE = hydra.core.Name("scope")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class StringArgumentAndOptionalStringLiteralVarargs:
    first: StringArgument
    rest: frozenlist[StringNullableArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs")
    FIRST = hydra.core.Name("first")
    REST = hydra.core.Name("rest")

class TraversalSackMethodArgumentOrIntegerArgumentConsumer(Node["TraversalSackMethodArgument"]):
    ...

class TraversalSackMethodArgumentOrIntegerArgumentInt(Node["IntegerArgument"]):
    ...

class _TraversalSackMethodArgumentOrIntegerArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalSackMethodArgumentOrIntegerArgument(metaclass=_TraversalSackMethodArgumentOrIntegerArgumentMeta):
    r"""TraversalSackMethodArgumentOrIntegerArgumentConsumer | TraversalSackMethodArgumentOrIntegerArgumentInt"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument")
    CONSUMER = hydra.core.Name("consumer")
    INT = hydra.core.Name("int")

class ByArgsOrder(Node["TraversalOrderArgument"]):
    ...

class ByArgsToken(Node["TraversalTokenArgument"]):
    ...

class ByArgsOther(Node["ByOtherArgs"]):
    ...

class _ByArgsMeta(type):
    def __getitem__(cls, item):
        return object

class ByArgs(metaclass=_ByArgsMeta):
    r"""ByArgsOrder | ByArgsToken | ByArgsOther"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ByArgs")
    ORDER = hydra.core.Name("order")
    TOKEN = hydra.core.Name("token")
    OTHER = hydra.core.Name("other")

class ByOtherArgsComparator(Node["Maybe[TraversalComparatorArgument]"]):
    ...

class ByOtherArgsOther(Node["Maybe[TraversalFunctionArgumentOrStringArgumentOrNestedTraversal]"]):
    ...

class _ByOtherArgsMeta(type):
    def __getitem__(cls, item):
        return object

class ByOtherArgs(metaclass=_ByOtherArgsMeta):
    r"""ByOtherArgsComparator | ByOtherArgsOther"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ByOtherArgs")
    COMPARATOR = hydra.core.Name("comparator")
    OTHER = hydra.core.Name("other")

class TraversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction(Node["TraversalFunctionArgument"]):
    ...

class TraversalFunctionArgumentOrStringArgumentOrNestedTraversalString(Node["StringArgument"]):
    ...

class TraversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal(Node["NestedTraversal"]):
    ...

class _TraversalFunctionArgumentOrStringArgumentOrNestedTraversalMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalFunctionArgumentOrStringArgumentOrNestedTraversal(metaclass=_TraversalFunctionArgumentOrStringArgumentOrNestedTraversalMeta):
    r"""TraversalFunctionArgumentOrStringArgumentOrNestedTraversalFunction | TraversalFunctionArgumentOrStringArgumentOrNestedTraversalString | TraversalFunctionArgumentOrStringArgumentOrNestedTraversalTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgumentOrStringArgumentOrNestedTraversal")
    FUNCTION = hydra.core.Name("function")
    STRING = hydra.core.Name("string")
    TRAVERSAL = hydra.core.Name("traversal")

class ChooseArgsFunction(Node["TraversalFunctionArgument"]):
    ...

class ChooseArgsPredicateTraversal(Node["PredicateTraversalArgument"]):
    ...

class ChooseArgsTraversal(Node["NestedTraversalArgument"]):
    ...

class _ChooseArgsMeta(type):
    def __getitem__(cls, item):
        return object

class ChooseArgs(metaclass=_ChooseArgsMeta):
    r"""ChooseArgsFunction | ChooseArgsPredicateTraversal | ChooseArgsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ChooseArgs")
    FUNCTION = hydra.core.Name("function")
    PREDICATE_TRAVERSAL = hydra.core.Name("predicateTraversal")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class PredicateTraversalArgument:
    predicate: TraversalPredicate
    traversal1: NestedTraversal
    traversal2: Maybe[NestedTraversal]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PredicateTraversalArgument")
    PREDICATE = hydra.core.Name("predicate")
    TRAVERSAL1 = hydra.core.Name("traversal1")
    TRAVERSAL2 = hydra.core.Name("traversal2")

@dataclass(frozen=True)
class NestedTraversalArgument:
    traversal1: NestedTraversal
    traversal2: Maybe[NestedTraversal]
    traversal3: Maybe[NestedTraversal]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversalArgument")
    TRAVERSAL1 = hydra.core.Name("traversal1")
    TRAVERSAL2 = hydra.core.Name("traversal2")
    TRAVERSAL3 = hydra.core.Name("traversal3")

class DedupArgsScopeString(Node["ScopeStringArgument"]):
    ...

class DedupArgsString(Node["frozenlist[StringNullableArgument]"]):
    ...

class _DedupArgsMeta(type):
    def __getitem__(cls, item):
        return object

class DedupArgs(metaclass=_DedupArgsMeta):
    r"""DedupArgsScopeString | DedupArgsString"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DedupArgs")
    SCOPE_STRING = hydra.core.Name("scopeString")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class ScopeStringArgument:
    scope: TraversalScopeArgument
    strings: frozenlist[StringNullableArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ScopeStringArgument")
    SCOPE = hydra.core.Name("scope")
    STRINGS = hydra.core.Name("strings")

class PredicateOrTraversalPredicate(Node["TraversalPredicate"]):
    ...

class PredicateOrTraversalTraversal(Node["NestedTraversal"]):
    ...

class _PredicateOrTraversalMeta(type):
    def __getitem__(cls, item):
        return object

class PredicateOrTraversal(metaclass=_PredicateOrTraversalMeta):
    r"""PredicateOrTraversalPredicate | PredicateOrTraversalTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal")
    PREDICATE = hydra.core.Name("predicate")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class GenericLiteralArgumentAndTraversalBiFunctionArgument:
    literal: GenericLiteralArgument
    bi_function: TraversalBiFunctionArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument")
    LITERAL = hydra.core.Name("literal")
    BI_FUNCTION = hydra.core.Name("biFunction")

class FromArgsString(Node["StringArgument"]):
    ...

class FromArgsVertex(Node["StructureVertexArgument"]):
    ...

class FromArgsTraversal(Node["NestedTraversal"]):
    ...

class _FromArgsMeta(type):
    def __getitem__(cls, item):
        return object

class FromArgs(metaclass=_FromArgsMeta):
    r"""FromArgsString | FromArgsVertex | FromArgsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.FromArgs")
    STRING = hydra.core.Name("string")
    VERTEX = hydra.core.Name("vertex")
    TRAVERSAL = hydra.core.Name("traversal")

class HasArgsString(Node["HasStringArgumentAndOptionalStringLiteralVarargs"]):
    ...

class HasArgsTraversalToken(Node["HasTraversalTokenArgs"]):
    ...

class _HasArgsMeta(type):
    def __getitem__(cls, item):
        return object

class HasArgs(metaclass=_HasArgsMeta):
    r"""HasArgsString | HasArgsTraversalToken"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasArgs")
    STRING = hydra.core.Name("string")
    TRAVERSAL_TOKEN = hydra.core.Name("traversalToken")

@dataclass(frozen=True)
class HasStringArgumentAndOptionalStringLiteralVarargs:
    string: StringNullableArgument
    rest: Maybe[HasStringArgumentAndOptionalStringLiteralVarargsRest]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargs")
    STRING = hydra.core.Name("string")
    REST = hydra.core.Name("rest")

class HasStringArgumentAndOptionalStringLiteralVarargsRestObject(Node["GenericLiteralArgument"]):
    ...

class HasStringArgumentAndOptionalStringLiteralVarargsRestPredicate(Node["TraversalPredicate"]):
    ...

class HasStringArgumentAndOptionalStringLiteralVarargsRestStringObject(Node["StringNullableArgumentAndGenericLiteralArgument"]):
    ...

class HasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate(Node["StringNullableArgumentAndTraversalPredicate"]):
    ...

class HasStringArgumentAndOptionalStringLiteralVarargsRestTraversal(Node["NestedTraversal"]):
    ...

class _HasStringArgumentAndOptionalStringLiteralVarargsRestMeta(type):
    def __getitem__(cls, item):
        return object

class HasStringArgumentAndOptionalStringLiteralVarargsRest(metaclass=_HasStringArgumentAndOptionalStringLiteralVarargsRestMeta):
    r"""HasStringArgumentAndOptionalStringLiteralVarargsRestObject | HasStringArgumentAndOptionalStringLiteralVarargsRestPredicate | HasStringArgumentAndOptionalStringLiteralVarargsRestStringObject | HasStringArgumentAndOptionalStringLiteralVarargsRestStringPredicate | HasStringArgumentAndOptionalStringLiteralVarargsRestTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasStringArgumentAndOptionalStringLiteralVarargsRest")
    OBJECT = hydra.core.Name("object")
    PREDICATE = hydra.core.Name("predicate")
    STRING_OBJECT = hydra.core.Name("stringObject")
    STRING_PREDICATE = hydra.core.Name("stringPredicate")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class StringNullableArgumentAndGenericLiteralArgument:
    string: StringNullableArgument
    literal: GenericLiteralArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgumentAndGenericLiteralArgument")
    STRING = hydra.core.Name("string")
    LITERAL = hydra.core.Name("literal")

@dataclass(frozen=True)
class StringNullableArgumentAndTraversalPredicate:
    string: StringNullableArgument
    predicate: TraversalPredicate

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgumentAndTraversalPredicate")
    STRING = hydra.core.Name("string")
    PREDICATE = hydra.core.Name("predicate")

@dataclass(frozen=True)
class HasTraversalTokenArgs:
    traversal_token: TraversalTokenArgument
    rest: HasTraversalTokenArgsRest

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgs")
    TRAVERSAL_TOKEN = hydra.core.Name("traversalToken")
    REST = hydra.core.Name("rest")

class HasTraversalTokenArgsRestLiteral(Node["GenericLiteralArgument"]):
    ...

class HasTraversalTokenArgsRestPredicate(Node["TraversalPredicate"]):
    ...

class HasTraversalTokenArgsRestTraversal(Node["NestedTraversal"]):
    ...

class _HasTraversalTokenArgsRestMeta(type):
    def __getitem__(cls, item):
        return object

class HasTraversalTokenArgsRest(metaclass=_HasTraversalTokenArgsRestMeta):
    r"""HasTraversalTokenArgsRestLiteral | HasTraversalTokenArgsRestPredicate | HasTraversalTokenArgsRestTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.HasTraversalTokenArgsRest")
    LITERAL = hydra.core.Name("literal")
    PREDICATE = hydra.core.Name("predicate")
    TRAVERSAL = hydra.core.Name("traversal")

class GenericLiteralArgumentAndTraversalPredicateLiteral(Node["GenericLiteralArgument"]):
    ...

class GenericLiteralArgumentAndTraversalPredicatePredicate(Node["TraversalPredicate"]):
    ...

class _GenericLiteralArgumentAndTraversalPredicateMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralArgumentAndTraversalPredicate(metaclass=_GenericLiteralArgumentAndTraversalPredicateMeta):
    r"""GenericLiteralArgumentAndTraversalPredicateLiteral | GenericLiteralArgumentAndTraversalPredicatePredicate"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate")
    LITERAL = hydra.core.Name("literal")
    PREDICATE = hydra.core.Name("predicate")

class TraversalPredicateOrStringLiteralVarargsPredicate(Node["TraversalPredicate"]):
    ...

class TraversalPredicateOrStringLiteralVarargsString(Node["frozenlist[StringNullableArgument]"]):
    ...

class _TraversalPredicateOrStringLiteralVarargsMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalPredicateOrStringLiteralVarargs(metaclass=_TraversalPredicateOrStringLiteralVarargsMeta):
    r"""TraversalPredicateOrStringLiteralVarargsPredicate | TraversalPredicateOrStringLiteralVarargsString"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs")
    PREDICATE = hydra.core.Name("predicate")
    STRING = hydra.core.Name("string")

class TraversalPredicateOrGenericLiteralArgumentPredicate(Node["TraversalPredicate"]):
    ...

class TraversalPredicateOrGenericLiteralArgumentLiteral(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class _TraversalPredicateOrGenericLiteralArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalPredicateOrGenericLiteralArgument(metaclass=_TraversalPredicateOrGenericLiteralArgumentMeta):
    r"""TraversalPredicateOrGenericLiteralArgumentPredicate | TraversalPredicateOrGenericLiteralArgumentLiteral"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument")
    PREDICATE = hydra.core.Name("predicate")
    LITERAL = hydra.core.Name("literal")

class OptionArgsPredicateTraversal(Node["TraversalPredicateAndNestedTraversal"]):
    ...

class OptionArgsMergeMap(Node["TraversalMergeArgumentAndGenericLiteralMapNullableArgument"]):
    ...

class OptionArgsMergeTraversal(Node["TraversalMergeArgumentAndNestedTraversal"]):
    ...

class OptionArgsObjectTraversal(Node["GenericLiteralArgumentAndNestedTraversal"]):
    ...

class OptionArgsTraversal(Node["NestedTraversal"]):
    ...

class _OptionArgsMeta(type):
    def __getitem__(cls, item):
        return object

class OptionArgs(metaclass=_OptionArgsMeta):
    r"""OptionArgsPredicateTraversal | OptionArgsMergeMap | OptionArgsMergeTraversal | OptionArgsObjectTraversal | OptionArgsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs")
    PREDICATE_TRAVERSAL = hydra.core.Name("predicateTraversal")
    MERGE_MAP = hydra.core.Name("mergeMap")
    MERGE_TRAVERSAL = hydra.core.Name("mergeTraversal")
    OBJECT_TRAVERSAL = hydra.core.Name("objectTraversal")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class TraversalPredicateAndNestedTraversal:
    predicate: TraversalPredicate
    traversal: NestedTraversal

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateAndNestedTraversal")
    PREDICATE = hydra.core.Name("predicate")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class TraversalMergeArgumentAndGenericLiteralMapNullableArgument:
    merge: TraversalMergeArgument
    map: GenericLiteralMapNullableArgument
    cardinality: Maybe[TraversalCardinality]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndGenericLiteralMapNullableArgument")
    MERGE = hydra.core.Name("merge")
    MAP = hydra.core.Name("map")
    CARDINALITY = hydra.core.Name("cardinality")

@dataclass(frozen=True)
class TraversalMergeArgumentAndNestedTraversal:
    merge: TraversalMergeArgument
    traversal: NestedTraversal

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgumentAndNestedTraversal")
    MERGE = hydra.core.Name("merge")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class GenericLiteralArgumentAndNestedTraversal:
    object: GenericLiteralArgument
    traversal: NestedTraversal

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndNestedTraversal")
    OBJECT = hydra.core.Name("object")
    TRAVERSAL = hydra.core.Name("traversal")

class PropertyArgsCardinalityObjects(Node["TraversalCardinalityArgumentAndObjects"]):
    ...

class PropertyArgsObjects(Node["frozenlist[GenericLiteralArgument]"]):
    ...

class PropertyArgsObject(Node["GenericLiteralMapNullableArgument"]):
    ...

class PropertyArgsCardinalityObject(Node["GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument"]):
    ...

class _PropertyArgsMeta(type):
    def __getitem__(cls, item):
        return object

class PropertyArgs(metaclass=_PropertyArgsMeta):
    r"""PropertyArgsCardinalityObjects | PropertyArgsObjects | PropertyArgsObject | PropertyArgsCardinalityObject"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs")
    CARDINALITY_OBJECTS = hydra.core.Name("cardinalityObjects")
    OBJECTS = hydra.core.Name("objects")
    OBJECT = hydra.core.Name("object")
    CARDINALITY_OBJECT = hydra.core.Name("cardinalityObject")

@dataclass(frozen=True)
class TraversalCardinalityArgumentAndObjects:
    cardinality: TraversalCardinalityArgument
    objects: frozenlist[GenericLiteralArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgumentAndObjects")
    CARDINALITY = hydra.core.Name("cardinality")
    OBJECTS = hydra.core.Name("objects")

@dataclass(frozen=True)
class GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument:
    cardinality: TraversalCardinalityArgument
    object: GenericLiteralMapNullableArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentAndTraversalCardinalityArgument")
    CARDINALITY = hydra.core.Name("cardinality")
    OBJECT = hydra.core.Name("object")

@dataclass(frozen=True)
class RangeArgs:
    scope: Maybe[TraversalScopeArgument]
    min: IntegerArgument
    max: IntegerArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RangeArgs")
    SCOPE = hydra.core.Name("scope")
    MIN = hydra.core.Name("min")
    MAX = hydra.core.Name("max")

@dataclass(frozen=True)
class OptionalStringArgumentAndNestedTraversal:
    string: Maybe[StringArgument]
    traversal: NestedTraversal

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal")
    STRING = hydra.core.Name("string")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class OptionalTraversalScopeArgumentAndIntegerArgument:
    scope: Maybe[TraversalScopeArgument]
    long: IntegerArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument")
    SCOPE = hydra.core.Name("scope")
    LONG = hydra.core.Name("long")

class SelectArgsColumn(Node["TraversalColumnArgument"]):
    ...

class SelectArgsPopStrings(Node["PopStringsArgument"]):
    ...

class SelectArgsPopTraversal(Node["TraversalPopArgumentAndNestedTraversal"]):
    ...

class SelectArgsStrings(Node["frozenlist[StringArgument]"]):
    ...

class SelectArgsTraversal(Node["NestedTraversal"]):
    ...

class _SelectArgsMeta(type):
    def __getitem__(cls, item):
        return object

class SelectArgs(metaclass=_SelectArgsMeta):
    r"""SelectArgsColumn | SelectArgsPopStrings | SelectArgsPopTraversal | SelectArgsStrings | SelectArgsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.SelectArgs")
    COLUMN = hydra.core.Name("column")
    POP_STRINGS = hydra.core.Name("popStrings")
    POP_TRAVERSAL = hydra.core.Name("popTraversal")
    STRINGS = hydra.core.Name("strings")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class PopStringsArgument:
    pop: TraversalPopArgument
    string: frozenlist[StringArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PopStringsArgument")
    POP = hydra.core.Name("pop")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class TraversalPopArgumentAndNestedTraversal:
    pop: TraversalPopArgument
    traversal: NestedTraversal

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgumentAndNestedTraversal")
    POP = hydra.core.Name("pop")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class TailArgs:
    scope: Maybe[TraversalScopeArgument]
    integer: Maybe[IntegerArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TailArgs")
    SCOPE = hydra.core.Name("scope")
    INTEGER = hydra.core.Name("integer")

class ToArgsDirection(Node["DirectionAndVarargs"]):
    ...

class ToArgsString(Node["StringArgument"]):
    ...

class ToArgsVertex(Node["StructureVertexArgument"]):
    ...

class ToArgsTraversal(Node["NestedTraversal"]):
    ...

class _ToArgsMeta(type):
    def __getitem__(cls, item):
        return object

class ToArgs(metaclass=_ToArgsMeta):
    r"""ToArgsDirection | ToArgsString | ToArgsVertex | ToArgsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ToArgs")
    DIRECTION = hydra.core.Name("direction")
    STRING = hydra.core.Name("string")
    VERTEX = hydra.core.Name("vertex")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class DirectionAndVarargs:
    direction: TraversalDirectionArgument
    varargs: frozenlist[StringNullableArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DirectionAndVarargs")
    DIRECTION = hydra.core.Name("direction")
    VARARGS = hydra.core.Name("varargs")

class ValueMapArgsString(Node["frozenlist[StringNullableArgument]"]):
    ...

class ValueMapArgsBoolean(Node["ValueMapBooleanArgs"]):
    ...

class _ValueMapArgsMeta(type):
    def __getitem__(cls, item):
        return object

class ValueMapArgs(metaclass=_ValueMapArgsMeta):
    r"""ValueMapArgsString | ValueMapArgsBoolean"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs")
    STRING = hydra.core.Name("string")
    BOOLEAN = hydra.core.Name("boolean")

@dataclass(frozen=True)
class ValueMapBooleanArgs:
    value: BooleanArgument
    keys: Maybe[frozenlist[StringNullableArgument]]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ValueMapBooleanArgs")
    VALUE = hydra.core.Name("value")
    KEYS = hydra.core.Name("keys")

class WhereArgsPredicate(Node["WhereWithPredicateArgs"]):
    ...

class WhereArgsString(Node["StringArgument"]):
    ...

class WhereArgsTraversal(Node["NestedTraversal"]):
    ...

class _WhereArgsMeta(type):
    def __getitem__(cls, item):
        return object

class WhereArgs(metaclass=_WhereArgsMeta):
    r"""WhereArgsPredicate | WhereArgsString | WhereArgsTraversal"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WhereArgs")
    PREDICATE = hydra.core.Name("predicate")
    STRING = hydra.core.Name("string")
    TRAVERSAL = hydra.core.Name("traversal")

@dataclass(frozen=True)
class WhereWithPredicateArgs:
    left_arg: Maybe[StringArgument]
    predicate: TraversalPredicate

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WhereWithPredicateArgs")
    LEFT_ARG = hydra.core.Name("leftArg")
    PREDICATE = hydra.core.Name("predicate")

@dataclass(frozen=True)
class WithArgs:
    keys: WithArgsKeys
    values: Maybe[WithArgsValues]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithArgs")
    KEYS = hydra.core.Name("keys")
    VALUES = hydra.core.Name("values")

class WithArgsKeysWithOption(Node["WithOptionKeys"]):
    ...

class WithArgsKeysString(Node["StringArgument"]):
    ...

class _WithArgsKeysMeta(type):
    def __getitem__(cls, item):
        return object

class WithArgsKeys(metaclass=_WithArgsKeysMeta):
    r"""WithArgsKeysWithOption | WithArgsKeysString"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithArgsKeys")
    WITH_OPTION = hydra.core.Name("withOption")
    STRING = hydra.core.Name("string")

class WithArgsValuesWithOptions(Node["WithOptionsValues"]):
    ...

class WithArgsValuesIo(Node["IoOptionsValues"]):
    ...

class WithArgsValuesObject(Node["GenericLiteralArgument"]):
    ...

class _WithArgsValuesMeta(type):
    def __getitem__(cls, item):
        return object

class WithArgsValues(metaclass=_WithArgsValuesMeta):
    r"""WithArgsValuesWithOptions | WithArgsValuesIo | WithArgsValuesObject"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithArgsValues")
    WITH_OPTIONS = hydra.core.Name("withOptions")
    IO = hydra.core.Name("io")
    OBJECT = hydra.core.Name("object")

class ConcatArgsTraversal(Node["frozenlist[NestedTraversal]"]):
    ...

class ConcatArgsString(Node["frozenlist[StringNullableArgument]"]):
    ...

class _ConcatArgsMeta(type):
    def __getitem__(cls, item):
        return object

class ConcatArgs(metaclass=_ConcatArgsMeta):
    r"""ConcatArgsTraversal | ConcatArgsString"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ConcatArgs")
    TRAVERSAL = hydra.core.Name("traversal")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class ReplaceArgs:
    scope: Maybe[TraversalScopeArgument]
    from_: StringNullableArgument
    to: StringNullableArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ReplaceArgs")
    SCOPE = hydra.core.Name("scope")
    FROM = hydra.core.Name("from")
    TO = hydra.core.Name("to")

@dataclass(frozen=True)
class SplitArgs:
    scope: Maybe[TraversalScopeArgument]
    delimiter: StringNullableArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.SplitArgs")
    SCOPE = hydra.core.Name("scope")
    DELIMITER = hydra.core.Name("delimiter")

@dataclass(frozen=True)
class SubstringArgs:
    scope: Maybe[TraversalScopeArgument]
    start: IntegerArgument
    end: Maybe[IntegerArgument]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.SubstringArgs")
    SCOPE = hydra.core.Name("scope")
    START = hydra.core.Name("start")
    END = hydra.core.Name("end")

@dataclass(frozen=True)
class DateAddArgs:
    unit: TraversalDTArgument
    duration: IntegerArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DateAddArgs")
    UNIT = hydra.core.Name("unit")
    DURATION = hydra.core.Name("duration")

class DateDiffArgsTraversal(Node["NestedTraversal"]):
    ...

class DateDiffArgsDate(Node["DateArgument"]):
    ...

class _DateDiffArgsMeta(type):
    def __getitem__(cls, item):
        return object

class DateDiffArgs(metaclass=_DateDiffArgsMeta):
    r"""DateDiffArgsTraversal | DateDiffArgsDate"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs")
    TRAVERSAL = hydra.core.Name("traversal")
    DATE = hydra.core.Name("date")

@dataclass(frozen=True)
class StructureVertex:
    new: bool
    id: GenericLiteralArgument
    label: StringArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StructureVertex")
    NEW = hydra.core.Name("new")
    ID = hydra.core.Name("id")
    LABEL = hydra.core.Name("label")

@dataclass(frozen=True)
class TraversalStrategy:
    new: bool
    class_: Identifier
    configurations: frozenlist[Configuration]

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalStrategy")
    NEW = hydra.core.Name("new")
    CLASS = hydra.core.Name("class")
    CONFIGURATIONS = hydra.core.Name("configurations")

@dataclass(frozen=True)
class Configuration:
    key: KeywordOrIdentifier
    value: GenericLiteralArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.Configuration")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class KeywordOrIdentifierKeyword(Node["Keyword"]):
    ...

class KeywordOrIdentifierIdentifier(Node["Identifier"]):
    ...

class _KeywordOrIdentifierMeta(type):
    def __getitem__(cls, item):
        return object

class KeywordOrIdentifier(metaclass=_KeywordOrIdentifierMeta):
    r"""KeywordOrIdentifierKeyword | KeywordOrIdentifierIdentifier"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.KeywordOrIdentifier")
    KEYWORD = hydra.core.Name("keyword")
    IDENTIFIER = hydra.core.Name("identifier")

class TraversalScope(Enum):
    LOCAL = hydra.core.Name("local")

    GLOBAL = hydra.core.Name("global")

TraversalScope.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalScope")

class TraversalToken(Enum):
    ID = hydra.core.Name("id")

    LABEL = hydra.core.Name("label")

    KEY = hydra.core.Name("key")

    VALUE = hydra.core.Name("value")

TraversalToken.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken")

class TraversalMerge(Enum):
    ON_CREATE = hydra.core.Name("onCreate")

    ON_MATCH = hydra.core.Name("onMatch")

    OUT_V = hydra.core.Name("outV")

    IN_V = hydra.core.Name("inV")

TraversalMerge.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMerge")

class TraversalOrder(Enum):
    INCR = hydra.core.Name("incr")

    DECR = hydra.core.Name("decr")

    ASC = hydra.core.Name("asc")

    DESC = hydra.core.Name("desc")

    SHUFFLE = hydra.core.Name("shuffle")

TraversalOrder.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalOrder")

class TraversalDirection(Enum):
    IN = hydra.core.Name("in")

    OUT = hydra.core.Name("out")

    BOTH = hydra.core.Name("both")

TraversalDirection.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirection")

class TraversalCardinalitySingle(Node["GenericLiteral"]):
    ...

class TraversalCardinalitySet(Node["GenericLiteral"]):
    ...

class TraversalCardinalityList(Node["GenericLiteral"]):
    ...

class _TraversalCardinalityMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalCardinality(metaclass=_TraversalCardinalityMeta):
    r"""TraversalCardinalitySingle | TraversalCardinalitySet | TraversalCardinalityList"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinality")
    SINGLE = hydra.core.Name("single")
    SET = hydra.core.Name("set")
    LIST = hydra.core.Name("list")

class TraversalColumn(Enum):
    KEYS = hydra.core.Name("keys")

    VALUES = hydra.core.Name("values")

TraversalColumn.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalColumn")

class TraversalPop(Enum):
    FIRST = hydra.core.Name("first")

    LAST = hydra.core.Name("last")

    ALL = hydra.core.Name("all")

    MIXED = hydra.core.Name("mixed")

TraversalPop.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPop")

class TraversalOperator(Enum):
    ADD_ALL = hydra.core.Name("addAll")

    AND = hydra.core.Name("and")

    ASSIGN = hydra.core.Name("assign")

    DIV = hydra.core.Name("div")

    MAX = hydra.core.Name("max")

    MIN = hydra.core.Name("min")

    MINUS = hydra.core.Name("minus")

    MULT = hydra.core.Name("mult")

    OR = hydra.core.Name("or")

    SUM = hydra.core.Name("sum")

    SUM_LONG = hydra.core.Name("sumLong")

TraversalOperator.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalOperator")

class TraversalPick(Enum):
    ANY = hydra.core.Name("any")

    NONE = hydra.core.Name("none")

TraversalPick.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPick")

class TraversalDT(Enum):
    SECOND = hydra.core.Name("second")

    MINUTE = hydra.core.Name("minute")

    HOUR = hydra.core.Name("hour")

    DAY = hydra.core.Name("day")

TraversalDT.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalDT")

class TraversalPredicateEq(Node["GenericLiteralArgument"]):
    ...

class TraversalPredicateNeq(Node["GenericLiteralArgument"]):
    ...

class TraversalPredicateLt(Node["GenericLiteralArgument"]):
    ...

class TraversalPredicateLte(Node["GenericLiteralArgument"]):
    ...

class TraversalPredicateGt(Node["GenericLiteralArgument"]):
    ...

class TraversalPredicateGte(Node["GenericLiteralArgument"]):
    ...

class TraversalPredicateInside(Node["RangeArgument"]):
    ...

class TraversalPredicateOutside(Node["RangeArgument"]):
    ...

class TraversalPredicateBetween(Node["RangeArgument"]):
    ...

class TraversalPredicateWithin(Node["Maybe[GenericLiteralArgument]"]):
    ...

class TraversalPredicateWithout(Node["Maybe[GenericLiteralArgument]"]):
    ...

class TraversalPredicateNot(Node["TraversalPredicate"]):
    ...

class TraversalPredicateStartingWith(Node["StringArgument"]):
    ...

class TraversalPredicateNotStartingWith(Node["StringArgument"]):
    ...

class TraversalPredicateEndingWith(Node["StringArgument"]):
    ...

class TraversalPredicateNotEndingWith(Node["StringArgument"]):
    ...

class TraversalPredicateContaining(Node["StringArgument"]):
    ...

class TraversalPredicateNotContaining(Node["StringArgument"]):
    ...

class TraversalPredicateRegex(Node["StringArgument"]):
    ...

class TraversalPredicateNotRegex(Node["StringArgument"]):
    ...

class TraversalPredicateAnd(Node["TwoTraversalPredicates"]):
    ...

class TraversalPredicateOr(Node["TwoTraversalPredicates"]):
    ...

class TraversalPredicateNegate(Node["TraversalPredicate"]):
    ...

class _TraversalPredicateMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalPredicate(metaclass=_TraversalPredicateMeta):
    r"""TraversalPredicateEq | TraversalPredicateNeq | TraversalPredicateLt | TraversalPredicateLte | TraversalPredicateGt | TraversalPredicateGte | TraversalPredicateInside | TraversalPredicateOutside | TraversalPredicateBetween | TraversalPredicateWithin | TraversalPredicateWithout | TraversalPredicateNot | TraversalPredicateStartingWith | TraversalPredicateNotStartingWith | TraversalPredicateEndingWith | TraversalPredicateNotEndingWith | TraversalPredicateContaining | TraversalPredicateNotContaining | TraversalPredicateRegex | TraversalPredicateNotRegex | TraversalPredicateAnd | TraversalPredicateOr | TraversalPredicateNegate"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate")
    EQ = hydra.core.Name("eq")
    NEQ = hydra.core.Name("neq")
    LT = hydra.core.Name("lt")
    LTE = hydra.core.Name("lte")
    GT = hydra.core.Name("gt")
    GTE = hydra.core.Name("gte")
    INSIDE = hydra.core.Name("inside")
    OUTSIDE = hydra.core.Name("outside")
    BETWEEN = hydra.core.Name("between")
    WITHIN = hydra.core.Name("within")
    WITHOUT = hydra.core.Name("without")
    NOT = hydra.core.Name("not")
    STARTING_WITH = hydra.core.Name("startingWith")
    NOT_STARTING_WITH = hydra.core.Name("notStartingWith")
    ENDING_WITH = hydra.core.Name("endingWith")
    NOT_ENDING_WITH = hydra.core.Name("notEndingWith")
    CONTAINING = hydra.core.Name("containing")
    NOT_CONTAINING = hydra.core.Name("notContaining")
    REGEX = hydra.core.Name("regex")
    NOT_REGEX = hydra.core.Name("notRegex")
    AND = hydra.core.Name("and")
    OR = hydra.core.Name("or")
    NEGATE = hydra.core.Name("negate")

@dataclass(frozen=True)
class TwoTraversalPredicates:
    left: TraversalPredicate
    right: TraversalPredicate

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class TraversalTerminalMethodExplain:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodExplain)
    def __hash__(self):
        return hash("TraversalTerminalMethodExplain")

class TraversalTerminalMethodIterate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodIterate)
    def __hash__(self):
        return hash("TraversalTerminalMethodIterate")

class TraversalTerminalMethodHasNext:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodHasNext)
    def __hash__(self):
        return hash("TraversalTerminalMethodHasNext")

class TraversalTerminalMethodTryNext:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodTryNext)
    def __hash__(self):
        return hash("TraversalTerminalMethodTryNext")

class TraversalTerminalMethodNext(Node["Maybe[IntegerLiteral]"]):
    ...

class TraversalTerminalMethodToList:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodToList)
    def __hash__(self):
        return hash("TraversalTerminalMethodToList")

class TraversalTerminalMethodToSet:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodToSet)
    def __hash__(self):
        return hash("TraversalTerminalMethodToSet")

class TraversalTerminalMethodToBulkSet:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalTerminalMethodToBulkSet)
    def __hash__(self):
        return hash("TraversalTerminalMethodToBulkSet")

class _TraversalTerminalMethodMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalTerminalMethod(metaclass=_TraversalTerminalMethodMeta):
    r"""TraversalTerminalMethodExplain | TraversalTerminalMethodIterate | TraversalTerminalMethodHasNext | TraversalTerminalMethodTryNext | TraversalTerminalMethodNext | TraversalTerminalMethodToList | TraversalTerminalMethodToSet | TraversalTerminalMethodToBulkSet"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalTerminalMethod")
    EXPLAIN = hydra.core.Name("explain")
    ITERATE = hydra.core.Name("iterate")
    HAS_NEXT = hydra.core.Name("hasNext")
    TRY_NEXT = hydra.core.Name("tryNext")
    NEXT = hydra.core.Name("next")
    TO_LIST = hydra.core.Name("toList")
    TO_SET = hydra.core.Name("toSet")
    TO_BULK_SET = hydra.core.Name("toBulkSet")

class TraversalSelfMethod(Enum):
    DISCARD = hydra.core.Name("discard")

TraversalSelfMethod.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSelfMethod")

class TraversalFunctionToken(Node["TraversalToken"]):
    ...

class TraversalFunctionColumn(Node["TraversalColumn"]):
    ...

class _TraversalFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalFunction(metaclass=_TraversalFunctionMeta):
    r"""TraversalFunctionToken | TraversalFunctionColumn"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunction")
    TOKEN = hydra.core.Name("token")
    COLUMN = hydra.core.Name("column")

@dataclass(frozen=True)
class RangeArgument:
    min: GenericLiteralArgument
    max: GenericLiteralArgument

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument")
    MIN = hydra.core.Name("min")
    MAX = hydra.core.Name("max")

class WithOptionKeysShortestPath(Node["ShortestPathConstants"]):
    ...

class WithOptionKeysConnectedComponent(Node["ConnectedComponentConstants"]):
    ...

class WithOptionKeysPageRank(Node["PageRankConstants"]):
    ...

class WithOptionKeysPeerPressure(Node["PeerPressureConstants"]):
    ...

class WithOptionKeysIo(Node["IoOptionsKeys"]):
    ...

class WithOptionKeysWithOptionsTokens:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, WithOptionKeysWithOptionsTokens)
    def __hash__(self):
        return hash("WithOptionKeysWithOptionsTokens")

class WithOptionKeysWithOptionsIndexer:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, WithOptionKeysWithOptionsIndexer)
    def __hash__(self):
        return hash("WithOptionKeysWithOptionsIndexer")

class _WithOptionKeysMeta(type):
    def __getitem__(cls, item):
        return object

class WithOptionKeys(metaclass=_WithOptionKeysMeta):
    r"""WithOptionKeysShortestPath | WithOptionKeysConnectedComponent | WithOptionKeysPageRank | WithOptionKeysPeerPressure | WithOptionKeysIo | WithOptionKeysWithOptionsTokens | WithOptionKeysWithOptionsIndexer"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithOptionKeys")
    SHORTEST_PATH = hydra.core.Name("shortestPath")
    CONNECTED_COMPONENT = hydra.core.Name("connectedComponent")
    PAGE_RANK = hydra.core.Name("pageRank")
    PEER_PRESSURE = hydra.core.Name("peerPressure")
    IO = hydra.core.Name("io")
    WITH_OPTIONS_TOKENS = hydra.core.Name("withOptionsTokens")
    WITH_OPTIONS_INDEXER = hydra.core.Name("withOptionsIndexer")

class ConnectedComponentConstants(Enum):
    COMPONENT = hydra.core.Name("component")

    EDGES = hydra.core.Name("edges")

    PROPERTY_NAME = hydra.core.Name("propertyName")

ConnectedComponentConstants.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ConnectedComponentConstants")

class PageRankConstants(Enum):
    EDGES = hydra.core.Name("edges")

    TIMES = hydra.core.Name("times")

    PROPERTY_NAME = hydra.core.Name("propertyName")

PageRankConstants.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PageRankConstants")

class PeerPressureConstants(Enum):
    EDGES = hydra.core.Name("edges")

    TIMES = hydra.core.Name("times")

    PROPERTY_NAME = hydra.core.Name("propertyName")

PeerPressureConstants.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.PeerPressureConstants")

class ShortestPathConstants(Enum):
    TARGET = hydra.core.Name("target")

    EDGES = hydra.core.Name("edges")

    DISTANCE = hydra.core.Name("distance")

    MAX_DISTANCE = hydra.core.Name("maxDistance")

    INCLUDE_EDGES = hydra.core.Name("includeEdges")

ShortestPathConstants.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.ShortestPathConstants")

class WithOptionsValues(Enum):
    TOKENS = hydra.core.Name("tokens")

    NONE = hydra.core.Name("none")

    IDS = hydra.core.Name("ids")

    LABELS = hydra.core.Name("labels")

    KEYS = hydra.core.Name("keys")

    VALUES = hydra.core.Name("values")

    ALL = hydra.core.Name("all")

    LIST = hydra.core.Name("list")

    MAP = hydra.core.Name("map")

WithOptionsValues.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.WithOptionsValues")

class IoOptionsKeys(Enum):
    READER = hydra.core.Name("reader")

    WRITER = hydra.core.Name("writer")

IoOptionsKeys.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsKeys")

class IoOptionsValues(Enum):
    GRYO = hydra.core.Name("gryo")

    GRAPHSON = hydra.core.Name("graphson")

    GRAPHML = hydra.core.Name("graphml")

IoOptionsValues.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IoOptionsValues")

class BooleanArgumentValue(Node[bool]):
    ...

class BooleanArgumentVariable(Node["Identifier"]):
    ...

class _BooleanArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class BooleanArgument(metaclass=_BooleanArgumentMeta):
    r"""BooleanArgumentValue | BooleanArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.BooleanArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class IntegerArgumentValue(Node["IntegerLiteral"]):
    ...

class IntegerArgumentVariable(Node["Identifier"]):
    ...

class _IntegerArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class IntegerArgument(metaclass=_IntegerArgumentMeta):
    r"""IntegerArgumentValue | IntegerArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class FloatArgumentValue(Node["FloatLiteral"]):
    ...

class FloatArgumentVariable(Node["Identifier"]):
    ...

class _FloatArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class FloatArgument(metaclass=_FloatArgumentMeta):
    r"""FloatArgumentValue | FloatArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.FloatArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class StringArgumentValue(Node[str]):
    ...

class StringArgumentVariable(Node["Identifier"]):
    ...

class _StringArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class StringArgument(metaclass=_StringArgumentMeta):
    r"""StringArgumentValue | StringArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class StringNullableArgumentValue(Node[Maybe[str]]):
    ...

class StringNullableArgumentVariable(Node["Identifier"]):
    ...

class _StringNullableArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class StringNullableArgument(metaclass=_StringNullableArgumentMeta):
    r"""StringNullableArgumentValue | StringNullableArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class DateArgumentValue(Node["DateLiteral"]):
    ...

class DateArgumentVariable(Node["Identifier"]):
    ...

class _DateArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class DateArgument(metaclass=_DateArgumentMeta):
    r"""DateArgumentValue | DateArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DateArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class GenericLiteralArgumentValue(Node["GenericLiteral"]):
    ...

class GenericLiteralArgumentVariable(Node["Identifier"]):
    ...

class _GenericLiteralArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralArgument(metaclass=_GenericLiteralArgumentMeta):
    r"""GenericLiteralArgumentValue | GenericLiteralArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class GenericLiteralListArgumentValue(Node["GenericLiteralList"]):
    ...

class GenericLiteralListArgumentVariable(Node["Identifier"]):
    ...

class _GenericLiteralListArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralListArgument(metaclass=_GenericLiteralListArgumentMeta):
    r"""GenericLiteralListArgumentValue | GenericLiteralListArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralListArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class GenericLiteralMapArgumentValue(Node["GenericLiteralMap"]):
    ...

class GenericLiteralMapArgumentVariable(Node["Identifier"]):
    ...

class _GenericLiteralMapArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralMapArgument(metaclass=_GenericLiteralMapArgumentMeta):
    r"""GenericLiteralMapArgumentValue | GenericLiteralMapArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class GenericLiteralMapNullableArgumentValue(Node["Maybe[GenericLiteralMap]"]):
    ...

class GenericLiteralMapNullableArgumentVariable(Node["Identifier"]):
    ...

class _GenericLiteralMapNullableArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralMapNullableArgument(metaclass=_GenericLiteralMapNullableArgumentMeta):
    r"""GenericLiteralMapNullableArgumentValue | GenericLiteralMapNullableArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class StructureVertexArgumentValue(Node["StructureVertex"]):
    ...

class StructureVertexArgumentVariable(Node["Identifier"]):
    ...

class _StructureVertexArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class StructureVertexArgument(metaclass=_StructureVertexArgumentMeta):
    r"""StructureVertexArgumentValue | StructureVertexArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StructureVertexArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalCardinalityArgumentValue(Node["TraversalCardinality"]):
    ...

class TraversalCardinalityArgumentVariable(Node["Identifier"]):
    ...

class _TraversalCardinalityArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalCardinalityArgument(metaclass=_TraversalCardinalityArgumentMeta):
    r"""TraversalCardinalityArgumentValue | TraversalCardinalityArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalCardinalityArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalColumnArgumentValue(Node["TraversalColumn"]):
    ...

class TraversalColumnArgumentVariable(Node["Identifier"]):
    ...

class _TraversalColumnArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalColumnArgument(metaclass=_TraversalColumnArgumentMeta):
    r"""TraversalColumnArgumentValue | TraversalColumnArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalColumnArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalDirectionArgumentValue(Node["TraversalDirection"]):
    ...

class TraversalDirectionArgumentVariable(Node["Identifier"]):
    ...

class _TraversalDirectionArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalDirectionArgument(metaclass=_TraversalDirectionArgumentMeta):
    r"""TraversalDirectionArgumentValue | TraversalDirectionArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalMergeArgumentValue(Node["TraversalMerge"]):
    ...

class TraversalMergeArgumentVariable(Node["Identifier"]):
    ...

class _TraversalMergeArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalMergeArgument(metaclass=_TraversalMergeArgumentMeta):
    r"""TraversalMergeArgumentValue | TraversalMergeArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMergeArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalOrderArgumentValue(Node["TraversalOrder"]):
    ...

class TraversalOrderArgumentVariable(Node["Identifier"]):
    ...

class _TraversalOrderArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalOrderArgument(metaclass=_TraversalOrderArgumentMeta):
    r"""TraversalOrderArgumentValue | TraversalOrderArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalOrderArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalPopArgumentValue(Node["TraversalPop"]):
    ...

class TraversalPopArgumentVariable(Node["Identifier"]):
    ...

class _TraversalPopArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalPopArgument(metaclass=_TraversalPopArgumentMeta):
    r"""TraversalPopArgumentValue | TraversalPopArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPopArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalSackMethodArgumentValue:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TraversalSackMethodArgumentValue)
    def __hash__(self):
        return hash("TraversalSackMethodArgumentValue")

class TraversalSackMethodArgumentVariable(Node["Identifier"]):
    ...

class _TraversalSackMethodArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalSackMethodArgument(metaclass=_TraversalSackMethodArgumentMeta):
    r"""TraversalSackMethodArgumentValue | TraversalSackMethodArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalScopeArgumentValue(Node["TraversalScope"]):
    ...

class TraversalScopeArgumentVariable(Node["Identifier"]):
    ...

class _TraversalScopeArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalScopeArgument(metaclass=_TraversalScopeArgumentMeta):
    r"""TraversalScopeArgumentValue | TraversalScopeArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalTokenArgumentValue(Node["TraversalToken"]):
    ...

class TraversalTokenArgumentVariable(Node["Identifier"]):
    ...

class _TraversalTokenArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalTokenArgument(metaclass=_TraversalTokenArgumentMeta):
    r"""TraversalTokenArgumentValue | TraversalTokenArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalTokenArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalComparatorArgumentValue(Node["TraversalOrder"]):
    ...

class TraversalComparatorArgumentVariable(Node["Identifier"]):
    ...

class _TraversalComparatorArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalComparatorArgument(metaclass=_TraversalComparatorArgumentMeta):
    r"""TraversalComparatorArgumentValue | TraversalComparatorArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalComparatorArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalFunctionArgumentValue(Node["TraversalFunction"]):
    ...

class TraversalFunctionArgumentVariable(Node["Identifier"]):
    ...

class _TraversalFunctionArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalFunctionArgument(metaclass=_TraversalFunctionArgumentMeta):
    r"""TraversalFunctionArgumentValue | TraversalFunctionArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunctionArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalBiFunctionArgumentValue(Node["TraversalOperator"]):
    ...

class TraversalBiFunctionArgumentVariable(Node["Identifier"]):
    ...

class _TraversalBiFunctionArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalBiFunctionArgument(metaclass=_TraversalBiFunctionArgumentMeta):
    r"""TraversalBiFunctionArgumentValue | TraversalBiFunctionArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class TraversalDTArgumentValue(Node["TraversalDT"]):
    ...

class TraversalDTArgumentVariable(Node["Identifier"]):
    ...

class _TraversalDTArgumentMeta(type):
    def __getitem__(cls, item):
        return object

class TraversalDTArgument(metaclass=_TraversalDTArgumentMeta):
    r"""TraversalDTArgumentValue | TraversalDTArgumentVariable"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalDTArgument")
    VALUE = hydra.core.Name("value")
    VARIABLE = hydra.core.Name("variable")

class GenericLiteralList(Node["frozenlist[GenericLiteral]"]):
    ...

GenericLiteralList.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralList")

class GenericLiteralRangeInteger(Node["IntegerRange"]):
    ...

class GenericLiteralRangeString(Node["StringRange"]):
    ...

class _GenericLiteralRangeMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteralRange(metaclass=_GenericLiteralRangeMeta):
    r"""GenericLiteralRangeInteger | GenericLiteralRangeString"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralRange")
    INTEGER = hydra.core.Name("integer")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class IntegerRange:
    left: IntegerLiteral
    right: IntegerLiteral

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IntegerRange")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class StringRange:
    left: str
    right: str

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.StringRange")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class GenericLiteralSet(Node["frozenlist[GenericLiteral]"]):
    ...

GenericLiteralSet.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralSet")

class GenericLiteralCollection(Node["frozenlist[GenericLiteral]"]):
    ...

GenericLiteralCollection.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralCollection")

class GenericLiteralNumeric(Node["NumericLiteral"]):
    ...

class GenericLiteralBoolean(Node[bool]):
    ...

class GenericLiteralString(Node[str]):
    ...

class GenericLiteralDate(Node["DateLiteral"]):
    ...

class GenericLiteralNull:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GenericLiteralNull)
    def __hash__(self):
        return hash("GenericLiteralNull")

class GenericLiteralNan:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GenericLiteralNan)
    def __hash__(self):
        return hash("GenericLiteralNan")

class GenericLiteralInf:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GenericLiteralInf)
    def __hash__(self):
        return hash("GenericLiteralInf")

class GenericLiteralTraversalToken(Node["TraversalToken"]):
    ...

class GenericLiteralTraversalCardinality(Node["TraversalCardinality"]):
    ...

class GenericLiteralTraversalDirection(Node["TraversalDirection"]):
    ...

class GenericLiteralTraversalMerge(Node["TraversalMerge"]):
    ...

class GenericLiteralTraversalPick(Node["TraversalPick"]):
    ...

class GenericLiteralTraversalDT(Node["TraversalDT"]):
    ...

class GenericLiteralStructureVertex(Node["StructureVertex"]):
    ...

class GenericLiteralGenericLiteralSet(Node["GenericLiteralSet"]):
    ...

class GenericLiteralGenericLiteralCollection(Node["GenericLiteralCollection"]):
    ...

class GenericLiteralGenericLiteralRange(Node["GenericLiteralRange"]):
    ...

class GenericLiteralNestedTraversal(Node["NestedTraversal"]):
    ...

class GenericLiteralTerminatedTraversal(Node["TerminatedTraversal"]):
    ...

class GenericLiteralGenericLiteralMap(Node["GenericLiteralMap"]):
    ...

class _GenericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class GenericLiteral(metaclass=_GenericLiteralMeta):
    r"""GenericLiteralNumeric | GenericLiteralBoolean | GenericLiteralString | GenericLiteralDate | GenericLiteralNull | GenericLiteralNan | GenericLiteralInf | GenericLiteralTraversalToken | GenericLiteralTraversalCardinality | GenericLiteralTraversalDirection | GenericLiteralTraversalMerge | GenericLiteralTraversalPick | GenericLiteralTraversalDT | GenericLiteralStructureVertex | GenericLiteralGenericLiteralSet | GenericLiteralGenericLiteralCollection | GenericLiteralGenericLiteralRange | GenericLiteralNestedTraversal | GenericLiteralTerminatedTraversal | GenericLiteralGenericLiteralMap"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteral")
    NUMERIC = hydra.core.Name("numeric")
    BOOLEAN = hydra.core.Name("boolean")
    STRING = hydra.core.Name("string")
    DATE = hydra.core.Name("date")
    NULL = hydra.core.Name("null")
    NAN = hydra.core.Name("nan")
    INF = hydra.core.Name("inf")
    TRAVERSAL_TOKEN = hydra.core.Name("traversalToken")
    TRAVERSAL_CARDINALITY = hydra.core.Name("traversalCardinality")
    TRAVERSAL_DIRECTION = hydra.core.Name("traversalDirection")
    TRAVERSAL_MERGE = hydra.core.Name("traversalMerge")
    TRAVERSAL_PICK = hydra.core.Name("traversalPick")
    TRAVERSAL_D_T = hydra.core.Name("traversalDT")
    STRUCTURE_VERTEX = hydra.core.Name("structureVertex")
    GENERIC_LITERAL_SET = hydra.core.Name("genericLiteralSet")
    GENERIC_LITERAL_COLLECTION = hydra.core.Name("genericLiteralCollection")
    GENERIC_LITERAL_RANGE = hydra.core.Name("genericLiteralRange")
    NESTED_TRAVERSAL = hydra.core.Name("nestedTraversal")
    TERMINATED_TRAVERSAL = hydra.core.Name("terminatedTraversal")
    GENERIC_LITERAL_MAP = hydra.core.Name("genericLiteralMap")

class GenericLiteralMap(Node["frozenlist[MapEntry]"]):
    ...

GenericLiteralMap.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMap")

class MapEntryKey(Node["MapKey"]):
    ...

class MapEntryValue(Node["GenericLiteral"]):
    ...

class _MapEntryMeta(type):
    def __getitem__(cls, item):
        return object

class MapEntry(metaclass=_MapEntryMeta):
    r"""MapEntryKey | MapEntryValue"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.MapEntry")
    KEY = hydra.core.Name("key")
    VALUE = hydra.core.Name("value")

class MapKeyString(Node[str]):
    ...

class MapKeyNumeric(Node["NumericLiteral"]):
    ...

class MapKeyTraversalToken(Node["TraversalToken"]):
    ...

class MapKeyTraversalDirection(Node["TraversalDirection"]):
    ...

class MapKeySet(Node["GenericLiteralSet"]):
    ...

class MapKeyCollection(Node["GenericLiteralCollection"]):
    ...

class MapKeyMap(Node["GenericLiteralMap"]):
    ...

class MapKeyKeyword(Node["Keyword"]):
    ...

class MapKeyIdentifier(Node["Identifier"]):
    ...

class _MapKeyMeta(type):
    def __getitem__(cls, item):
        return object

class MapKey(metaclass=_MapKeyMeta):
    r"""MapKeyString | MapKeyNumeric | MapKeyTraversalToken | MapKeyTraversalDirection | MapKeySet | MapKeyCollection | MapKeyMap | MapKeyKeyword | MapKeyIdentifier"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.MapKey")
    STRING = hydra.core.Name("string")
    NUMERIC = hydra.core.Name("numeric")
    TRAVERSAL_TOKEN = hydra.core.Name("traversalToken")
    TRAVERSAL_DIRECTION = hydra.core.Name("traversalDirection")
    SET = hydra.core.Name("set")
    COLLECTION = hydra.core.Name("collection")
    MAP = hydra.core.Name("map")
    KEYWORD = hydra.core.Name("keyword")
    IDENTIFIER = hydra.core.Name("identifier")

class IntegerLiteral(Node[int]):
    ...

IntegerLiteral.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.IntegerLiteral")

class FloatLiteral(Node[Decimal]):
    ...

FloatLiteral.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.FloatLiteral")

class NumericLiteralInteger(Node["IntegerLiteral"]):
    ...

class NumericLiteralFloat(Node["FloatLiteral"]):
    ...

class _NumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class NumericLiteral(metaclass=_NumericLiteralMeta):
    r"""NumericLiteralInteger | NumericLiteralFloat"""

    TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.NumericLiteral")
    INTEGER = hydra.core.Name("integer")
    FLOAT = hydra.core.Name("float")

class DateLiteral(Node["Maybe[StringArgument]"]):
    ...

DateLiteral.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.DateLiteral")

class Keyword(Enum):
    EDGES = hydra.core.Name("edges")

    KEYS = hydra.core.Name("keys")

    NEW = hydra.core.Name("new")

    VALUES = hydra.core.Name("values")

Keyword.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.Keyword")

class Identifier(Node[str]):
    ...

Identifier.TYPE_ = hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.Identifier")
