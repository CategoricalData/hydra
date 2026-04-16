# Note: this is an automatically generated file. Do not edit.

r"""A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at: https://github.com/opengql/grammar/blob/main/GQL.g4."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from functools import lru_cache
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class GqlProgram:
    activity: Maybe[ProgramActivity]
    close: Maybe[SessionCloseCommand]

    TYPE_ = hydra.core.Name("openGql.grammar.GqlProgram")
    ACTIVITY = hydra.core.Name("activity")
    CLOSE = hydra.core.Name("close")

class ProgramActivitySession(Node["SessionActivity"]):
    ...

class ProgramActivityTransaction(Node["TransactionActivity"]):
    ...

class _ProgramActivityMeta(type):
    def __getitem__(cls, item):
        return object

class ProgramActivity(metaclass=_ProgramActivityMeta):
    r"""ProgramActivitySession | ProgramActivityTransaction"""

    TYPE_ = hydra.core.Name("openGql.grammar.ProgramActivity")
    SESSION = hydra.core.Name("session")
    TRANSACTION = hydra.core.Name("transaction")

class SessionActivityReset(Node["frozenlist[SessionResetCommand]"]):
    ...

class SessionActivitySetAndResetCommands(Node["SessionSetAndResetCommands"]):
    ...

class _SessionActivityMeta(type):
    def __getitem__(cls, item):
        return object

class SessionActivity(metaclass=_SessionActivityMeta):
    r"""SessionActivityReset | SessionActivitySetAndResetCommands"""

    TYPE_ = hydra.core.Name("openGql.grammar.SessionActivity")
    RESET = hydra.core.Name("reset")
    SET_AND_RESET_COMMANDS = hydra.core.Name("setAndResetCommands")

@dataclass(frozen=True)
class SessionSetAndResetCommands:
    set: frozenlist[SessionSetCommand]
    reset: frozenlist[SessionResetCommand]

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetAndResetCommands")
    SET = hydra.core.Name("set")
    RESET = hydra.core.Name("reset")

class TransactionActivityStart(Node["StartAndMaybeProcedureAndMaybeEnd"]):
    ...

class TransactionActivityProcedure(Node["ProcedureAndMaybeEnd"]):
    ...

class TransactionActivityEnd(Node["EndTransactionCommand"]):
    ...

class _TransactionActivityMeta(type):
    def __getitem__(cls, item):
        return object

class TransactionActivity(metaclass=_TransactionActivityMeta):
    r"""TransactionActivityStart | TransactionActivityProcedure | TransactionActivityEnd"""

    TYPE_ = hydra.core.Name("openGql.grammar.TransactionActivity")
    START = hydra.core.Name("start")
    PROCEDURE = hydra.core.Name("procedure")
    END = hydra.core.Name("end")

@dataclass(frozen=True)
class StartAndMaybeProcedureAndMaybeEnd:
    start: StartTransactionCommand
    procedure_and_end: Maybe[ProcedureAndMaybeEnd]

    TYPE_ = hydra.core.Name("openGql.grammar.StartAndMaybeProcedureAndMaybeEnd")
    START = hydra.core.Name("start")
    PROCEDURE_AND_END = hydra.core.Name("procedureAndEnd")

@dataclass(frozen=True)
class ProcedureAndMaybeEnd:
    procedure: ProcedureSpecification
    end: Maybe[EndTransactionCommand]

    TYPE_ = hydra.core.Name("openGql.grammar.ProcedureAndMaybeEnd")
    PROCEDURE = hydra.core.Name("procedure")
    END = hydra.core.Name("end")

class EndTransactionCommandRollback(Node["RollbackCommand"]):
    ...

class EndTransactionCommandCommit(Node["CommitCommand"]):
    ...

class _EndTransactionCommandMeta(type):
    def __getitem__(cls, item):
        return object

class EndTransactionCommand(metaclass=_EndTransactionCommandMeta):
    r"""EndTransactionCommandRollback | EndTransactionCommandCommit"""

    TYPE_ = hydra.core.Name("openGql.grammar.EndTransactionCommand")
    ROLLBACK = hydra.core.Name("rollback")
    COMMIT = hydra.core.Name("commit")

class SessionSetCommandSchema(Node["SessionSetSchemaClause"]):
    ...

class SessionSetCommandGraph(Node["SessionSetGraphClause"]):
    ...

class SessionSetCommandTimeZone(Node["SessionSetTimeZoneClause"]):
    ...

class SessionSetCommandParameter(Node["SessionSetParameterClause"]):
    ...

class _SessionSetCommandMeta(type):
    def __getitem__(cls, item):
        return object

class SessionSetCommand(metaclass=_SessionSetCommandMeta):
    r"""SessionSetCommandSchema | SessionSetCommandGraph | SessionSetCommandTimeZone | SessionSetCommandParameter"""

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetCommand")
    SCHEMA = hydra.core.Name("schema")
    GRAPH = hydra.core.Name("graph")
    TIME_ZONE = hydra.core.Name("timeZone")
    PARAMETER = hydra.core.Name("parameter")

SessionSetSchemaClause: TypeAlias = "SchemaReference"

SessionSetGraphClause: TypeAlias = "GraphExpression"

SessionSetTimeZoneClause: TypeAlias = "SetTimeZoneValue"

SetTimeZoneValue: TypeAlias = "TimeZoneString"

class SessionSetParameterClauseGraph(Node["SessionSetGraphParameterClause"]):
    ...

class SessionSetParameterClauseBindings(Node["SessionSetBindingTableParameterClause"]):
    ...

class SessionSetParameterClauseValue(Node["SessionSetValueParameterClause"]):
    ...

class _SessionSetParameterClauseMeta(type):
    def __getitem__(cls, item):
        return object

class SessionSetParameterClause(metaclass=_SessionSetParameterClauseMeta):
    r"""SessionSetParameterClauseGraph | SessionSetParameterClauseBindings | SessionSetParameterClauseValue"""

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetParameterClause")
    GRAPH = hydra.core.Name("graph")
    BINDINGS = hydra.core.Name("bindings")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class SessionSetGraphParameterClause:
    graph: SessionSetParameterName
    initializer: OptTypedGraphInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetGraphParameterClause")
    GRAPH = hydra.core.Name("graph")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class SessionSetBindingTableParameterClause:
    binding: bool
    param: SessionSetParameterName
    init: OptTypedBindingTableInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetBindingTableParameterClause")
    BINDING = hydra.core.Name("binding")
    PARAM = hydra.core.Name("param")
    INIT = hydra.core.Name("init")

@dataclass(frozen=True)
class SessionSetValueParameterClause:
    value: SessionSetParameterName
    initializer: OptTypedValueInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetValueParameterClause")
    VALUE = hydra.core.Name("value")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class SessionSetParameterName:
    if_not_exists: bool
    parameter: SessionParameterSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.SessionSetParameterName")
    IF_NOT_EXISTS = hydra.core.Name("ifNotExists")
    PARAMETER = hydra.core.Name("parameter")

SessionResetCommand: TypeAlias = "Maybe[SessionResetArguments]"

class SessionResetArgumentsParametersOrCharacteristics(Node["AllParametersOrCharacteristics"]):
    ...

class SessionResetArgumentsSchema:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SessionResetArgumentsSchema)
    def __hash__(self):
        return hash("SessionResetArgumentsSchema")

class SessionResetArgumentsGraph:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SessionResetArgumentsGraph)
    def __hash__(self):
        return hash("SessionResetArgumentsGraph")

class SessionResetArgumentsTimeZone:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SessionResetArgumentsTimeZone)
    def __hash__(self):
        return hash("SessionResetArgumentsTimeZone")

class SessionResetArgumentsParameterSessionSpecification(Node["ParameterSessionSpecification"]):
    ...

class _SessionResetArgumentsMeta(type):
    def __getitem__(cls, item):
        return object

class SessionResetArguments(metaclass=_SessionResetArgumentsMeta):
    r"""SessionResetArgumentsParametersOrCharacteristics | SessionResetArgumentsSchema | SessionResetArgumentsGraph | SessionResetArgumentsTimeZone | SessionResetArgumentsParameterSessionSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.SessionResetArguments")
    PARAMETERS_OR_CHARACTERISTICS = hydra.core.Name("parametersOrCharacteristics")
    SCHEMA = hydra.core.Name("schema")
    GRAPH = hydra.core.Name("graph")
    TIME_ZONE = hydra.core.Name("timeZone")
    PARAMETER_SESSION_SPECIFICATION = hydra.core.Name("parameterSessionSpecification")

@dataclass(frozen=True)
class AllParametersOrCharacteristics:
    all: bool
    type: ParametersOrCharacteristics

    TYPE_ = hydra.core.Name("openGql.grammar.AllParametersOrCharacteristics")
    ALL = hydra.core.Name("all")
    TYPE = hydra.core.Name("type")

class ParametersOrCharacteristics(Enum):
    PARAMETERS = hydra.core.Name("parameters")

    CHARACTERISTICS = hydra.core.Name("characteristics")

ParametersOrCharacteristics.TYPE_ = hydra.core.Name("openGql.grammar.ParametersOrCharacteristics")

@dataclass(frozen=True)
class ParameterSessionSpecification:
    parameter: bool
    session_parameter_specification: SessionParameterSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.ParameterSessionSpecification")
    PARAMETER = hydra.core.Name("parameter")
    SESSION_PARAMETER_SPECIFICATION = hydra.core.Name("sessionParameterSpecification")

SessionCloseCommand: TypeAlias = "None"

SessionParameterSpecification: TypeAlias = "ParameterName"

StartTransactionCommand: TypeAlias = "Maybe[TransactionCharacteristics]"

TransactionCharacteristics: TypeAlias = "frozenlist[TransactionMode]"

TransactionMode: TypeAlias = "TransactionAccessMode"

class TransactionAccessMode(Enum):
    READ_ONLY = hydra.core.Name("readOnly")

    READ_WRITE = hydra.core.Name("readWrite")

TransactionAccessMode.TYPE_ = hydra.core.Name("openGql.grammar.TransactionAccessMode")

RollbackCommand: TypeAlias = "None"

CommitCommand: TypeAlias = "None"

NestedProcedureSpecification: TypeAlias = "ProcedureSpecification"

ProcedureSpecification: TypeAlias = "ProcedureBody"

NestedDataModifyingProcedureSpecification: TypeAlias = "ProcedureBody"

NestedQuerySpecification: TypeAlias = "ProcedureBody"

@dataclass(frozen=True)
class ProcedureBody:
    at_schema: Maybe[AtSchemaClause]
    bindings: Maybe[BindingVariableDefinitionBlock]
    statements: StatementBlock

    TYPE_ = hydra.core.Name("openGql.grammar.ProcedureBody")
    AT_SCHEMA = hydra.core.Name("atSchema")
    BINDINGS = hydra.core.Name("bindings")
    STATEMENTS = hydra.core.Name("statements")

BindingVariableDefinitionBlock: TypeAlias = "frozenlist[BindingVariableDefinition]"

class BindingVariableDefinitionGraph(Node["GraphVariableDefinition"]):
    ...

class BindingVariableDefinitionTable(Node["BindingTableVariableDefinition"]):
    ...

class BindingVariableDefinitionValue(Node["ValueVariableDefinition"]):
    ...

class _BindingVariableDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class BindingVariableDefinition(metaclass=_BindingVariableDefinitionMeta):
    r"""BindingVariableDefinitionGraph | BindingVariableDefinitionTable | BindingVariableDefinitionValue"""

    TYPE_ = hydra.core.Name("openGql.grammar.BindingVariableDefinition")
    GRAPH = hydra.core.Name("graph")
    TABLE = hydra.core.Name("table")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class StatementBlock:
    statement: Statement
    next_statements: frozenlist[NextStatement]

    TYPE_ = hydra.core.Name("openGql.grammar.StatementBlock")
    STATEMENT = hydra.core.Name("statement")
    NEXT_STATEMENTS = hydra.core.Name("nextStatements")

class StatementLinearCatalogModifying(Node["LinearCatalogModifyingStatement"]):
    ...

class StatementLinearDataModifying(Node["LinearDataModifyingStatement"]):
    ...

class StatementCompositeQuery(Node["CompositeQueryStatement"]):
    ...

class _StatementMeta(type):
    def __getitem__(cls, item):
        return object

class Statement(metaclass=_StatementMeta):
    r"""StatementLinearCatalogModifying | StatementLinearDataModifying | StatementCompositeQuery"""

    TYPE_ = hydra.core.Name("openGql.grammar.Statement")
    LINEAR_CATALOG_MODIFYING = hydra.core.Name("linearCatalogModifying")
    LINEAR_DATA_MODIFYING = hydra.core.Name("linearDataModifying")
    COMPOSITE_QUERY = hydra.core.Name("compositeQuery")

@dataclass(frozen=True)
class NextStatement:
    yield_clause: Maybe[YieldClause]
    statement: Statement

    TYPE_ = hydra.core.Name("openGql.grammar.NextStatement")
    YIELD_CLAUSE = hydra.core.Name("yieldClause")
    STATEMENT = hydra.core.Name("statement")

@dataclass(frozen=True)
class GraphVariableDefinition:
    variable: BindingVariable
    initializer: OptTypedGraphInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.GraphVariableDefinition")
    VARIABLE = hydra.core.Name("variable")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class OptTypedGraphInitializer:
    type: Maybe[TypedGraphReferenceValueType]
    initializer: GraphInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.OptTypedGraphInitializer")
    TYPE = hydra.core.Name("type")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class TypedGraphReferenceValueType:
    typed: Maybe[Typed]
    value_type: GraphReferenceValueType

    TYPE_ = hydra.core.Name("openGql.grammar.TypedGraphReferenceValueType")
    TYPED = hydra.core.Name("typed")
    VALUE_TYPE = hydra.core.Name("valueType")

GraphInitializer: TypeAlias = "None"

@dataclass(frozen=True)
class BindingTableVariableDefinition:
    binding: bool
    variable: BindingVariable
    initializer: OptTypedBindingTableInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.BindingTableVariableDefinition")
    BINDING = hydra.core.Name("binding")
    VARIABLE = hydra.core.Name("variable")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class OptTypedBindingTableInitializer:
    type: Maybe[TypedBindingTableReferenceValueType]
    initializer: BindingTableInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.OptTypedBindingTableInitializer")
    TYPE = hydra.core.Name("type")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class TypedBindingTableReferenceValueType:
    typed: Maybe[Typed]
    value_type: BindingTableReferenceValueType

    TYPE_ = hydra.core.Name("openGql.grammar.TypedBindingTableReferenceValueType")
    TYPED = hydra.core.Name("typed")
    VALUE_TYPE = hydra.core.Name("valueType")

BindingTableInitializer: TypeAlias = "None"

@dataclass(frozen=True)
class ValueVariableDefinition:
    variable: BindingVariable
    initializer: OptTypedValueInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.ValueVariableDefinition")
    VARIABLE = hydra.core.Name("variable")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class OptTypedValueInitializer:
    type: Maybe[TypedValueType]
    initializer: ValueInitializer

    TYPE_ = hydra.core.Name("openGql.grammar.OptTypedValueInitializer")
    TYPE = hydra.core.Name("type")
    INITIALIZER = hydra.core.Name("initializer")

@dataclass(frozen=True)
class TypedValueType:
    typed: Maybe[Typed]
    value_type: ValueType

    TYPE_ = hydra.core.Name("openGql.grammar.TypedValueType")
    TYPED = hydra.core.Name("typed")
    VALUE_TYPE = hydra.core.Name("valueType")

ValueInitializer: TypeAlias = "None"

class GraphExpressionObject(Node["ObjectExpressionPrimary"]):
    ...

class GraphExpressionReference(Node["GraphReference"]):
    ...

class GraphExpressionName(Node["ObjectNameOrBindingVariable"]):
    ...

class GraphExpressionCurrent(Node["CurrentGraph"]):
    ...

class _GraphExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class GraphExpression(metaclass=_GraphExpressionMeta):
    r"""GraphExpressionObject | GraphExpressionReference | GraphExpressionName | GraphExpressionCurrent"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphExpression")
    OBJECT = hydra.core.Name("object")
    REFERENCE = hydra.core.Name("reference")
    NAME = hydra.core.Name("name")
    CURRENT = hydra.core.Name("current")

class CurrentGraph(Enum):
    GRAPH = hydra.core.Name("graph")

    PROPERTY_GRAPH = hydra.core.Name("propertyGraph")

CurrentGraph.TYPE_ = hydra.core.Name("openGql.grammar.CurrentGraph")

class BindingTableExpressionNested(Node["NestedBindingTableQuerySpecification"]):
    ...

class BindingTableExpressionObject(Node["ObjectExpressionPrimary"]):
    ...

class BindingTableExpressionTable(Node["BindingTableReference"]):
    ...

class BindingTableExpressionName(Node["ObjectNameOrBindingVariable"]):
    ...

class _BindingTableExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class BindingTableExpression(metaclass=_BindingTableExpressionMeta):
    r"""BindingTableExpressionNested | BindingTableExpressionObject | BindingTableExpressionTable | BindingTableExpressionName"""

    TYPE_ = hydra.core.Name("openGql.grammar.BindingTableExpression")
    NESTED = hydra.core.Name("nested")
    OBJECT = hydra.core.Name("object")
    TABLE = hydra.core.Name("table")
    NAME = hydra.core.Name("name")

NestedBindingTableQuerySpecification: TypeAlias = "None"

class ObjectExpressionPrimaryVariable(Node["PrimaryValueExpression"]):
    ...

class ObjectExpressionPrimaryParenthesized(Node["ParenthesizedValueExpression"]):
    ...

class ObjectExpressionPrimaryNonParenthesized(Node["NonParenthesizedPrimaryValueExpressionSpecialCase"]):
    ...

class _ObjectExpressionPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectExpressionPrimary(metaclass=_ObjectExpressionPrimaryMeta):
    r"""ObjectExpressionPrimaryVariable | ObjectExpressionPrimaryParenthesized | ObjectExpressionPrimaryNonParenthesized"""

    TYPE_ = hydra.core.Name("openGql.grammar.ObjectExpressionPrimary")
    VARIABLE = hydra.core.Name("variable")
    PARENTHESIZED = hydra.core.Name("parenthesized")
    NON_PARENTHESIZED = hydra.core.Name("nonParenthesized")

LinearCatalogModifyingStatement: TypeAlias = "frozenlist[SimpleCatalogModifyingStatement]"

class SimpleCatalogModifyingStatementPrimitive(Node["PrimitiveCatalogModifyingStatement"]):
    ...

class SimpleCatalogModifyingStatementCallProcedure(Node["CallCatalogModifyingProcedureStatement"]):
    ...

class _SimpleCatalogModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleCatalogModifyingStatement(metaclass=_SimpleCatalogModifyingStatementMeta):
    r"""SimpleCatalogModifyingStatementPrimitive | SimpleCatalogModifyingStatementCallProcedure"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimpleCatalogModifyingStatement")
    PRIMITIVE = hydra.core.Name("primitive")
    CALL_PROCEDURE = hydra.core.Name("callProcedure")

class PrimitiveCatalogModifyingStatementCreateSchema(Node["CreateSchemaStatement"]):
    ...

class PrimitiveCatalogModifyingStatementDropSchema(Node["DropSchemaStatement"]):
    ...

class PrimitiveCatalogModifyingStatementCreateGraph(Node["CreateGraphStatement"]):
    ...

class PrimitiveCatalogModifyingStatementDropGraph(Node["DropGraphStatement"]):
    ...

class PrimitiveCatalogModifyingStatementCreateGraphType(Node["CreateGraphTypeStatement"]):
    ...

class PrimitiveCatalogModifyingStatementDropGraphType(Node["DropGraphTypeStatement"]):
    ...

class _PrimitiveCatalogModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class PrimitiveCatalogModifyingStatement(metaclass=_PrimitiveCatalogModifyingStatementMeta):
    r"""PrimitiveCatalogModifyingStatementCreateSchema | PrimitiveCatalogModifyingStatementDropSchema | PrimitiveCatalogModifyingStatementCreateGraph | PrimitiveCatalogModifyingStatementDropGraph | PrimitiveCatalogModifyingStatementCreateGraphType | PrimitiveCatalogModifyingStatementDropGraphType"""

    TYPE_ = hydra.core.Name("openGql.grammar.PrimitiveCatalogModifyingStatement")
    CREATE_SCHEMA = hydra.core.Name("createSchema")
    DROP_SCHEMA = hydra.core.Name("dropSchema")
    CREATE_GRAPH = hydra.core.Name("createGraph")
    DROP_GRAPH = hydra.core.Name("dropGraph")
    CREATE_GRAPH_TYPE = hydra.core.Name("createGraphType")
    DROP_GRAPH_TYPE = hydra.core.Name("dropGraphType")

@dataclass(frozen=True)
class CreateSchemaStatement:
    if_not_exists: bool
    parent_and_name: CatalogSchemaParentAndName

    TYPE_ = hydra.core.Name("openGql.grammar.CreateSchemaStatement")
    IF_NOT_EXISTS = hydra.core.Name("ifNotExists")
    PARENT_AND_NAME = hydra.core.Name("parentAndName")

@dataclass(frozen=True)
class DropSchemaStatement:
    if_exists: bool
    parent_and_name: CatalogSchemaParentAndName

    TYPE_ = hydra.core.Name("openGql.grammar.DropSchemaStatement")
    IF_EXISTS = hydra.core.Name("ifExists")
    PARENT_AND_NAME = hydra.core.Name("parentAndName")

@dataclass(frozen=True)
class CreateGraphStatement:
    create_option: CreateGraphOption
    parent_and_name: CatalogGraphParentAndName
    type: GraphTypeOption
    source: Maybe[GraphSource]

    TYPE_ = hydra.core.Name("openGql.grammar.CreateGraphStatement")
    CREATE_OPTION = hydra.core.Name("createOption")
    PARENT_AND_NAME = hydra.core.Name("parentAndName")
    TYPE = hydra.core.Name("type")
    SOURCE = hydra.core.Name("source")

class CreateGraphOptionGraphIfNotExists(Node[bool]):
    ...

class CreateGraphOptionOrReplace:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CreateGraphOptionOrReplace)
    def __hash__(self):
        return hash("CreateGraphOptionOrReplace")

class _CreateGraphOptionMeta(type):
    def __getitem__(cls, item):
        return object

class CreateGraphOption(metaclass=_CreateGraphOptionMeta):
    r"""CreateGraphOptionGraphIfNotExists | CreateGraphOptionOrReplace"""

    TYPE_ = hydra.core.Name("openGql.grammar.CreateGraphOption")
    GRAPH_IF_NOT_EXISTS = hydra.core.Name("graphIfNotExists")
    OR_REPLACE = hydra.core.Name("orReplace")

class GraphTypeOptionOpenGraphType(Node["OpenGraphType"]):
    ...

class GraphTypeOptionOfGraphType(Node["OfGraphType"]):
    ...

class _GraphTypeOptionMeta(type):
    def __getitem__(cls, item):
        return object

class GraphTypeOption(metaclass=_GraphTypeOptionMeta):
    r"""GraphTypeOptionOpenGraphType | GraphTypeOptionOfGraphType"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphTypeOption")
    OPEN_GRAPH_TYPE = hydra.core.Name("openGraphType")
    OF_GRAPH_TYPE = hydra.core.Name("ofGraphType")

@dataclass(frozen=True)
class OpenGraphType:
    typed: Maybe[Typed]
    graph: bool

    TYPE_ = hydra.core.Name("openGql.grammar.OpenGraphType")
    TYPED = hydra.core.Name("typed")
    GRAPH = hydra.core.Name("graph")

class OfGraphTypeLikeGraph(Node["GraphTypeLikeGraph"]):
    ...

class OfGraphTypeReference(Node["TypedGraphTypeReference"]):
    ...

class OfGraphTypeNested(Node["TypedNestedGraphTypeSpecification"]):
    ...

class _OfGraphTypeMeta(type):
    def __getitem__(cls, item):
        return object

class OfGraphType(metaclass=_OfGraphTypeMeta):
    r"""OfGraphTypeLikeGraph | OfGraphTypeReference | OfGraphTypeNested"""

    TYPE_ = hydra.core.Name("openGql.grammar.OfGraphType")
    LIKE_GRAPH = hydra.core.Name("likeGraph")
    REFERENCE = hydra.core.Name("reference")
    NESTED = hydra.core.Name("nested")

GraphTypeLikeGraph: TypeAlias = "GraphExpression"

GraphSource: TypeAlias = "GraphExpression"

@dataclass(frozen=True)
class TypedGraphTypeReference:
    typed: Maybe[Typed]
    reference: GraphTypeReference

    TYPE_ = hydra.core.Name("openGql.grammar.TypedGraphTypeReference")
    TYPED = hydra.core.Name("typed")
    REFERENCE = hydra.core.Name("reference")

@dataclass(frozen=True)
class TypedNestedGraphTypeSpecification:
    typed: Maybe[Typed]
    graph: bool
    specification: NestedGraphTypeSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.TypedNestedGraphTypeSpecification")
    TYPED = hydra.core.Name("typed")
    GRAPH = hydra.core.Name("graph")
    SPECIFICATION = hydra.core.Name("specification")

@dataclass(frozen=True)
class DropGraphStatement:
    if_exists: bool
    parent_and_name: CatalogGraphParentAndName

    TYPE_ = hydra.core.Name("openGql.grammar.DropGraphStatement")
    IF_EXISTS = hydra.core.Name("ifExists")
    PARENT_AND_NAME = hydra.core.Name("parentAndName")

@dataclass(frozen=True)
class CreateGraphTypeStatement:
    create_option: CreateGraphTypeOption
    parent_and_name: CatalogGraphTypeParentAndName
    source: GraphTypeSource

    TYPE_ = hydra.core.Name("openGql.grammar.CreateGraphTypeStatement")
    CREATE_OPTION = hydra.core.Name("createOption")
    PARENT_AND_NAME = hydra.core.Name("parentAndName")
    SOURCE = hydra.core.Name("source")

class CreateGraphTypeOptionTypeIfNotExists(Node[bool]):
    ...

class CreateGraphTypeOptionOrReplace:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CreateGraphTypeOptionOrReplace)
    def __hash__(self):
        return hash("CreateGraphTypeOptionOrReplace")

class _CreateGraphTypeOptionMeta(type):
    def __getitem__(cls, item):
        return object

class CreateGraphTypeOption(metaclass=_CreateGraphTypeOptionMeta):
    r"""CreateGraphTypeOptionTypeIfNotExists | CreateGraphTypeOptionOrReplace"""

    TYPE_ = hydra.core.Name("openGql.grammar.CreateGraphTypeOption")
    TYPE_IF_NOT_EXISTS = hydra.core.Name("typeIfNotExists")
    OR_REPLACE = hydra.core.Name("orReplace")

class GraphTypeSourceCopyOf(Node["CopyOfGraphType"]):
    ...

class GraphTypeSourceLikeGraph(Node["GraphTypeLikeGraph"]):
    ...

class GraphTypeSourceNestedSpecification(Node["NestedGraphTypeSpecification"]):
    ...

class _GraphTypeSourceMeta(type):
    def __getitem__(cls, item):
        return object

class GraphTypeSource(metaclass=_GraphTypeSourceMeta):
    r"""GraphTypeSourceCopyOf | GraphTypeSourceLikeGraph | GraphTypeSourceNestedSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphTypeSource")
    COPY_OF = hydra.core.Name("copyOf")
    LIKE_GRAPH = hydra.core.Name("likeGraph")
    NESTED_SPECIFICATION = hydra.core.Name("nestedSpecification")

CopyOfGraphType: TypeAlias = "GraphTypeReference"

@dataclass(frozen=True)
class DropGraphTypeStatement:
    if_exists: bool
    parent_and_name: CatalogGraphTypeParentAndName

    TYPE_ = hydra.core.Name("openGql.grammar.DropGraphTypeStatement")
    IF_EXISTS = hydra.core.Name("ifExists")
    PARENT_AND_NAME = hydra.core.Name("parentAndName")

CallCatalogModifyingProcedureStatement: TypeAlias = "CallProcedureStatement"

class LinearDataModifyingStatementFocused(Node["FocusedLinearDataModifyingStatement"]):
    ...

class LinearDataModifyingStatementAmbient(Node["AmbientLinearDataModifyingStatement"]):
    ...

class _LinearDataModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class LinearDataModifyingStatement(metaclass=_LinearDataModifyingStatementMeta):
    r"""LinearDataModifyingStatementFocused | LinearDataModifyingStatementAmbient"""

    TYPE_ = hydra.core.Name("openGql.grammar.LinearDataModifyingStatement")
    FOCUSED = hydra.core.Name("focused")
    AMBIENT = hydra.core.Name("ambient")

class FocusedLinearDataModifyingStatementSimple(Node["FocusedLinearDataModifyingStatementBody"]):
    ...

class FocusedLinearDataModifyingStatementNested(Node["FocusedNestedDataModifyingProcedureSpecification"]):
    ...

class _FocusedLinearDataModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class FocusedLinearDataModifyingStatement(metaclass=_FocusedLinearDataModifyingStatementMeta):
    r"""FocusedLinearDataModifyingStatementSimple | FocusedLinearDataModifyingStatementNested"""

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedLinearDataModifyingStatement")
    SIMPLE = hydra.core.Name("simple")
    NESTED = hydra.core.Name("nested")

@dataclass(frozen=True)
class FocusedLinearDataModifyingStatementBody:
    use_graph: UseGraphClause
    simple_access: SimpleLinearDataAccessingStatement
    primitive_result: Maybe[PrimitiveResultStatement]

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedLinearDataModifyingStatementBody")
    USE_GRAPH = hydra.core.Name("useGraph")
    SIMPLE_ACCESS = hydra.core.Name("simpleAccess")
    PRIMITIVE_RESULT = hydra.core.Name("primitiveResult")

@dataclass(frozen=True)
class FocusedNestedDataModifyingProcedureSpecification:
    use_graph: UseGraphClause
    nested_spec: NestedDataModifyingProcedureSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedNestedDataModifyingProcedureSpecification")
    USE_GRAPH = hydra.core.Name("useGraph")
    NESTED_SPEC = hydra.core.Name("nestedSpec")

class AmbientLinearDataModifyingStatementSimple(Node["AmbientLinearDataModifyingStatementBody"]):
    ...

class AmbientLinearDataModifyingStatementNested(Node["NestedDataModifyingProcedureSpecification"]):
    ...

class _AmbientLinearDataModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class AmbientLinearDataModifyingStatement(metaclass=_AmbientLinearDataModifyingStatementMeta):
    r"""AmbientLinearDataModifyingStatementSimple | AmbientLinearDataModifyingStatementNested"""

    TYPE_ = hydra.core.Name("openGql.grammar.AmbientLinearDataModifyingStatement")
    SIMPLE = hydra.core.Name("simple")
    NESTED = hydra.core.Name("nested")

@dataclass(frozen=True)
class AmbientLinearDataModifyingStatementBody:
    simple_access: SimpleLinearDataAccessingStatement
    primitive_result: Maybe[PrimitiveResultStatement]

    TYPE_ = hydra.core.Name("openGql.grammar.AmbientLinearDataModifyingStatementBody")
    SIMPLE_ACCESS = hydra.core.Name("simpleAccess")
    PRIMITIVE_RESULT = hydra.core.Name("primitiveResult")

SimpleLinearDataAccessingStatement: TypeAlias = "frozenlist[SimpleDataAccessingStatement]"

class SimpleDataAccessingStatementQuery(Node["SimpleQueryStatement"]):
    ...

class SimpleDataAccessingStatementModifying(Node["SimpleDataModifyingStatement"]):
    ...

class _SimpleDataAccessingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleDataAccessingStatement(metaclass=_SimpleDataAccessingStatementMeta):
    r"""SimpleDataAccessingStatementQuery | SimpleDataAccessingStatementModifying"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimpleDataAccessingStatement")
    QUERY = hydra.core.Name("query")
    MODIFYING = hydra.core.Name("modifying")

class SimpleDataModifyingStatementPrimitive(Node["PrimitiveDataModifyingStatement"]):
    ...

class SimpleDataModifyingStatementCallProcedure(Node["CallDataModifyingProcedureStatement"]):
    ...

class _SimpleDataModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleDataModifyingStatement(metaclass=_SimpleDataModifyingStatementMeta):
    r"""SimpleDataModifyingStatementPrimitive | SimpleDataModifyingStatementCallProcedure"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimpleDataModifyingStatement")
    PRIMITIVE = hydra.core.Name("primitive")
    CALL_PROCEDURE = hydra.core.Name("callProcedure")

class PrimitiveDataModifyingStatementInsert(Node["InsertStatement"]):
    ...

class PrimitiveDataModifyingStatementSet(Node["SetStatement"]):
    ...

class PrimitiveDataModifyingStatementRemove(Node["RemoveStatement"]):
    ...

class PrimitiveDataModifyingStatementDelete(Node["DeleteStatement"]):
    ...

class _PrimitiveDataModifyingStatementMeta(type):
    def __getitem__(cls, item):
        return object

class PrimitiveDataModifyingStatement(metaclass=_PrimitiveDataModifyingStatementMeta):
    r"""PrimitiveDataModifyingStatementInsert | PrimitiveDataModifyingStatementSet | PrimitiveDataModifyingStatementRemove | PrimitiveDataModifyingStatementDelete"""

    TYPE_ = hydra.core.Name("openGql.grammar.PrimitiveDataModifyingStatement")
    INSERT = hydra.core.Name("insert")
    SET = hydra.core.Name("set")
    REMOVE = hydra.core.Name("remove")
    DELETE = hydra.core.Name("delete")

InsertStatement: TypeAlias = "InsertGraphPattern"

SetStatement: TypeAlias = "SetItemList"

SetItemList: TypeAlias = "frozenlist[SetItem]"

class SetItemProperty(Node["SetPropertyItem"]):
    ...

class SetItemAllProperties(Node["SetAllPropertiesItem"]):
    ...

class SetItemLabel(Node["SetLabelItem"]):
    ...

class _SetItemMeta(type):
    def __getitem__(cls, item):
        return object

class SetItem(metaclass=_SetItemMeta):
    r"""SetItemProperty | SetItemAllProperties | SetItemLabel"""

    TYPE_ = hydra.core.Name("openGql.grammar.SetItem")
    PROPERTY = hydra.core.Name("property")
    ALL_PROPERTIES = hydra.core.Name("allProperties")
    LABEL = hydra.core.Name("label")

@dataclass(frozen=True)
class SetPropertyItem:
    variable: BindingVariableReference
    property_name: PropertyName
    value: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.SetPropertyItem")
    VARIABLE = hydra.core.Name("variable")
    PROPERTY_NAME = hydra.core.Name("propertyName")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class SetAllPropertiesItem:
    variable: BindingVariableReference
    properties: Maybe[PropertyKeyValuePairList]

    TYPE_ = hydra.core.Name("openGql.grammar.SetAllPropertiesItem")
    VARIABLE = hydra.core.Name("variable")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class SetLabelItem:
    variable: BindingVariableReference
    is_or_colon: IsOrColon
    label: LabelName

    TYPE_ = hydra.core.Name("openGql.grammar.SetLabelItem")
    VARIABLE = hydra.core.Name("variable")
    IS_OR_COLON = hydra.core.Name("isOrColon")
    LABEL = hydra.core.Name("label")

RemoveStatement: TypeAlias = "RemoveItemList"

RemoveItemList: TypeAlias = "frozenlist[RemoveItem]"

class RemoveItemProperty(Node["RemovePropertyItem"]):
    ...

class RemoveItemLabel(Node["RemoveLabelItem"]):
    ...

class _RemoveItemMeta(type):
    def __getitem__(cls, item):
        return object

class RemoveItem(metaclass=_RemoveItemMeta):
    r"""RemoveItemProperty | RemoveItemLabel"""

    TYPE_ = hydra.core.Name("openGql.grammar.RemoveItem")
    PROPERTY = hydra.core.Name("property")
    LABEL = hydra.core.Name("label")

@dataclass(frozen=True)
class RemovePropertyItem:
    variable: BindingVariableReference
    property_name: PropertyName

    TYPE_ = hydra.core.Name("openGql.grammar.RemovePropertyItem")
    VARIABLE = hydra.core.Name("variable")
    PROPERTY_NAME = hydra.core.Name("propertyName")

@dataclass(frozen=True)
class RemoveLabelItem:
    variable: BindingVariableReference
    is_or_colon: IsOrColon
    label: LabelName

    TYPE_ = hydra.core.Name("openGql.grammar.RemoveLabelItem")
    VARIABLE = hydra.core.Name("variable")
    IS_OR_COLON = hydra.core.Name("isOrColon")
    LABEL = hydra.core.Name("label")

@dataclass(frozen=True)
class DeleteStatement:
    detach: Maybe[DetachOption]
    items: DeleteItemList

    TYPE_ = hydra.core.Name("openGql.grammar.DeleteStatement")
    DETACH = hydra.core.Name("detach")
    ITEMS = hydra.core.Name("items")

class DetachOption(Enum):
    DETACH = hydra.core.Name("detach")

    NO_DETACH = hydra.core.Name("noDetach")

DetachOption.TYPE_ = hydra.core.Name("openGql.grammar.DetachOption")

DeleteItemList: TypeAlias = "frozenlist[DeleteItem]"

DeleteItem: TypeAlias = "ValueExpression"

CallDataModifyingProcedureStatement: TypeAlias = "CallProcedureStatement"

CompositeQueryStatement: TypeAlias = "CompositeQueryExpression"

class CompositeQueryExpressionSimple(Node["CompositeQueryExpressionConjunction"]):
    ...

class CompositeQueryExpressionPrimary(Node["CompositeQueryPrimary"]):
    ...

class _CompositeQueryExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class CompositeQueryExpression(metaclass=_CompositeQueryExpressionMeta):
    r"""CompositeQueryExpressionSimple | CompositeQueryExpressionPrimary"""

    TYPE_ = hydra.core.Name("openGql.grammar.CompositeQueryExpression")
    SIMPLE = hydra.core.Name("simple")
    PRIMARY = hydra.core.Name("primary")

@dataclass(frozen=True)
class CompositeQueryExpressionConjunction:
    left: CompositeQueryExpression
    conjunction: QueryConjunction
    right: CompositeQueryPrimary

    TYPE_ = hydra.core.Name("openGql.grammar.CompositeQueryExpressionConjunction")
    LEFT = hydra.core.Name("left")
    CONJUNCTION = hydra.core.Name("conjunction")
    RIGHT = hydra.core.Name("right")

class QueryConjunctionSetOperator(Node["SetOperator"]):
    ...

class QueryConjunctionOtherwise:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, QueryConjunctionOtherwise)
    def __hash__(self):
        return hash("QueryConjunctionOtherwise")

class _QueryConjunctionMeta(type):
    def __getitem__(cls, item):
        return object

class QueryConjunction(metaclass=_QueryConjunctionMeta):
    r"""QueryConjunctionSetOperator | QueryConjunctionOtherwise"""

    TYPE_ = hydra.core.Name("openGql.grammar.QueryConjunction")
    SET_OPERATOR = hydra.core.Name("setOperator")
    OTHERWISE = hydra.core.Name("otherwise")

@dataclass(frozen=True)
class SetOperator:
    operator_type: SetOperatorType
    quantifier: Maybe[SetQuantifier]

    TYPE_ = hydra.core.Name("openGql.grammar.SetOperator")
    OPERATOR_TYPE = hydra.core.Name("operatorType")
    QUANTIFIER = hydra.core.Name("quantifier")

class SetOperatorType(Enum):
    UNION = hydra.core.Name("union")

    EXCEPT = hydra.core.Name("except")

    INTERSECT = hydra.core.Name("intersect")

SetOperatorType.TYPE_ = hydra.core.Name("openGql.grammar.SetOperatorType")

CompositeQueryPrimary: TypeAlias = "LinearQueryStatement"

class LinearQueryStatementFocused(Node["FocusedLinearQueryStatement"]):
    ...

class LinearQueryStatementAmbient(Node["AmbientLinearQueryStatement"]):
    ...

class _LinearQueryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class LinearQueryStatement(metaclass=_LinearQueryStatementMeta):
    r"""LinearQueryStatementFocused | LinearQueryStatementAmbient"""

    TYPE_ = hydra.core.Name("openGql.grammar.LinearQueryStatement")
    FOCUSED = hydra.core.Name("focused")
    AMBIENT = hydra.core.Name("ambient")

class FocusedLinearQueryStatementParts(Node["FocusedLinearQueryStatementPartsAndResult"]):
    ...

class FocusedLinearQueryStatementPrimitive(Node["FocusedPrimitiveResultStatement"]):
    ...

class FocusedLinearQueryStatementNested(Node["FocusedNestedQuerySpecification"]):
    ...

class FocusedLinearQueryStatementSelect(Node["SelectStatement"]):
    ...

class _FocusedLinearQueryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class FocusedLinearQueryStatement(metaclass=_FocusedLinearQueryStatementMeta):
    r"""FocusedLinearQueryStatementParts | FocusedLinearQueryStatementPrimitive | FocusedLinearQueryStatementNested | FocusedLinearQueryStatementSelect"""

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedLinearQueryStatement")
    PARTS = hydra.core.Name("parts")
    PRIMITIVE = hydra.core.Name("primitive")
    NESTED = hydra.core.Name("nested")
    SELECT = hydra.core.Name("select")

@dataclass(frozen=True)
class FocusedLinearQueryStatementPartsAndResult:
    parts: frozenlist[FocusedLinearQueryStatementPart]
    result: FocusedLinearQueryAndPrimitiveResultStatementPart

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedLinearQueryStatementPartsAndResult")
    PARTS = hydra.core.Name("parts")
    RESULT = hydra.core.Name("result")

@dataclass(frozen=True)
class FocusedLinearQueryStatementPart:
    use_graph: UseGraphClause
    simple: SimpleLinearQueryStatement

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedLinearQueryStatementPart")
    USE_GRAPH = hydra.core.Name("useGraph")
    SIMPLE = hydra.core.Name("simple")

@dataclass(frozen=True)
class FocusedLinearQueryAndPrimitiveResultStatementPart:
    use_graph: UseGraphClause
    simple: SimpleLinearQueryStatement
    primitive_result: PrimitiveResultStatement

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedLinearQueryAndPrimitiveResultStatementPart")
    USE_GRAPH = hydra.core.Name("useGraph")
    SIMPLE = hydra.core.Name("simple")
    PRIMITIVE_RESULT = hydra.core.Name("primitiveResult")

@dataclass(frozen=True)
class FocusedPrimitiveResultStatement:
    use_graph: UseGraphClause
    primitive_result: PrimitiveResultStatement

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedPrimitiveResultStatement")
    USE_GRAPH = hydra.core.Name("useGraph")
    PRIMITIVE_RESULT = hydra.core.Name("primitiveResult")

@dataclass(frozen=True)
class FocusedNestedQuerySpecification:
    use_graph: UseGraphClause
    nested: NestedQuerySpecification

    TYPE_ = hydra.core.Name("openGql.grammar.FocusedNestedQuerySpecification")
    USE_GRAPH = hydra.core.Name("useGraph")
    NESTED = hydra.core.Name("nested")

class AmbientLinearQueryStatementSimple(Node["AmbientLinearQueryStatementSimpleAndPrimitiveResult"]):
    ...

class AmbientLinearQueryStatementNested(Node["NestedQuerySpecification"]):
    ...

class _AmbientLinearQueryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class AmbientLinearQueryStatement(metaclass=_AmbientLinearQueryStatementMeta):
    r"""AmbientLinearQueryStatementSimple | AmbientLinearQueryStatementNested"""

    TYPE_ = hydra.core.Name("openGql.grammar.AmbientLinearQueryStatement")
    SIMPLE = hydra.core.Name("simple")
    NESTED = hydra.core.Name("nested")

@dataclass(frozen=True)
class AmbientLinearQueryStatementSimpleAndPrimitiveResult:
    simple: Maybe[SimpleLinearQueryStatement]
    primitive_result: PrimitiveResultStatement

    TYPE_ = hydra.core.Name("openGql.grammar.AmbientLinearQueryStatementSimpleAndPrimitiveResult")
    SIMPLE = hydra.core.Name("simple")
    PRIMITIVE_RESULT = hydra.core.Name("primitiveResult")

SimpleLinearQueryStatement: TypeAlias = "frozenlist[SimpleQueryStatement]"

class SimpleQueryStatementPrimitive(Node["PrimitiveQueryStatement"]):
    ...

class SimpleQueryStatementCall(Node["CallQueryStatement"]):
    ...

class _SimpleQueryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class SimpleQueryStatement(metaclass=_SimpleQueryStatementMeta):
    r"""SimpleQueryStatementPrimitive | SimpleQueryStatementCall"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimpleQueryStatement")
    PRIMITIVE = hydra.core.Name("primitive")
    CALL = hydra.core.Name("call")

class PrimitiveQueryStatementMatch(Node["MatchStatement"]):
    ...

class PrimitiveQueryStatementLet(Node["LetStatement"]):
    ...

class PrimitiveQueryStatementFor(Node["ForStatement"]):
    ...

class PrimitiveQueryStatementFilter(Node["FilterStatement"]):
    ...

class PrimitiveQueryStatementOrderByAndPage(Node["OrderByAndPageStatement"]):
    ...

class _PrimitiveQueryStatementMeta(type):
    def __getitem__(cls, item):
        return object

class PrimitiveQueryStatement(metaclass=_PrimitiveQueryStatementMeta):
    r"""PrimitiveQueryStatementMatch | PrimitiveQueryStatementLet | PrimitiveQueryStatementFor | PrimitiveQueryStatementFilter | PrimitiveQueryStatementOrderByAndPage"""

    TYPE_ = hydra.core.Name("openGql.grammar.PrimitiveQueryStatement")
    MATCH = hydra.core.Name("match")
    LET = hydra.core.Name("let")
    FOR = hydra.core.Name("for")
    FILTER = hydra.core.Name("filter")
    ORDER_BY_AND_PAGE = hydra.core.Name("orderByAndPage")

class MatchStatementSimple(Node["SimpleMatchStatement"]):
    ...

class MatchStatementOptional(Node["OptionalMatchStatement"]):
    ...

class _MatchStatementMeta(type):
    def __getitem__(cls, item):
        return object

class MatchStatement(metaclass=_MatchStatementMeta):
    r"""MatchStatementSimple | MatchStatementOptional"""

    TYPE_ = hydra.core.Name("openGql.grammar.MatchStatement")
    SIMPLE = hydra.core.Name("simple")
    OPTIONAL = hydra.core.Name("optional")

SimpleMatchStatement: TypeAlias = "GraphPatternBindingTable"

OptionalMatchStatement: TypeAlias = "OptionalOperand"

class OptionalOperandSimple(Node["SimpleMatchStatement"]):
    ...

class OptionalOperandBraceBlock(Node["MatchStatementBlock"]):
    ...

class OptionalOperandParenBlock(Node["MatchStatementBlock"]):
    ...

class _OptionalOperandMeta(type):
    def __getitem__(cls, item):
        return object

class OptionalOperand(metaclass=_OptionalOperandMeta):
    r"""OptionalOperandSimple | OptionalOperandBraceBlock | OptionalOperandParenBlock"""

    TYPE_ = hydra.core.Name("openGql.grammar.OptionalOperand")
    SIMPLE = hydra.core.Name("simple")
    BRACE_BLOCK = hydra.core.Name("braceBlock")
    PAREN_BLOCK = hydra.core.Name("parenBlock")

MatchStatementBlock: TypeAlias = "frozenlist[MatchStatement]"

CallQueryStatement: TypeAlias = "CallProcedureStatement"

class FilterStatementWhereClause(Node["WhereClause"]):
    ...

class FilterStatementSearchCondition(Node["SearchCondition"]):
    ...

class _FilterStatementMeta(type):
    def __getitem__(cls, item):
        return object

class FilterStatement(metaclass=_FilterStatementMeta):
    r"""FilterStatementWhereClause | FilterStatementSearchCondition"""

    TYPE_ = hydra.core.Name("openGql.grammar.FilterStatement")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    SEARCH_CONDITION = hydra.core.Name("searchCondition")

LetStatement: TypeAlias = "LetVariableDefinitionList"

LetVariableDefinitionList: TypeAlias = "frozenlist[LetVariableDefinition]"

class LetVariableDefinitionValueVariable(Node["ValueVariableDefinition"]):
    ...

class LetVariableDefinitionBindingEqualsValue(Node["BindingEqualsValue"]):
    ...

class _LetVariableDefinitionMeta(type):
    def __getitem__(cls, item):
        return object

class LetVariableDefinition(metaclass=_LetVariableDefinitionMeta):
    r"""LetVariableDefinitionValueVariable | LetVariableDefinitionBindingEqualsValue"""

    TYPE_ = hydra.core.Name("openGql.grammar.LetVariableDefinition")
    VALUE_VARIABLE = hydra.core.Name("valueVariable")
    BINDING_EQUALS_VALUE = hydra.core.Name("bindingEqualsValue")

@dataclass(frozen=True)
class BindingEqualsValue:
    binding: BindingVariable
    value: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.BindingEqualsValue")
    BINDING = hydra.core.Name("binding")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class ForStatement:
    item: ForItem
    ordinality_or_offset: Maybe[ForOrdinalityOrOffset]

    TYPE_ = hydra.core.Name("openGql.grammar.ForStatement")
    ITEM = hydra.core.Name("item")
    ORDINALITY_OR_OFFSET = hydra.core.Name("ordinalityOrOffset")

@dataclass(frozen=True)
class ForItem:
    alias: ForItemAlias
    source: ForItemSource

    TYPE_ = hydra.core.Name("openGql.grammar.ForItem")
    ALIAS = hydra.core.Name("alias")
    SOURCE = hydra.core.Name("source")

ForItemAlias: TypeAlias = "BindingVariable"

ForItemSource: TypeAlias = "ValueExpression"

@dataclass(frozen=True)
class ForOrdinalityOrOffset:
    type: OrdinalityOrOffsetType
    variable: BindingVariable

    TYPE_ = hydra.core.Name("openGql.grammar.ForOrdinalityOrOffset")
    TYPE = hydra.core.Name("type")
    VARIABLE = hydra.core.Name("variable")

class OrdinalityOrOffsetType(Enum):
    ORDINALITY = hydra.core.Name("ordinality")

    OFFSET = hydra.core.Name("offset")

OrdinalityOrOffsetType.TYPE_ = hydra.core.Name("openGql.grammar.OrdinalityOrOffsetType")

class OrderByAndPageStatementOrderByAndOptionalOffsetAndLimit(Node["OrderByAndOptionalOffsetAndLimit"]):
    ...

class OrderByAndPageStatementOffsetAndOptionalLimit(Node["OffsetAndOptionalLimit"]):
    ...

class OrderByAndPageStatementLimitOnly(Node["LimitClause"]):
    ...

class _OrderByAndPageStatementMeta(type):
    def __getitem__(cls, item):
        return object

class OrderByAndPageStatement(metaclass=_OrderByAndPageStatementMeta):
    r"""OrderByAndPageStatementOrderByAndOptionalOffsetAndLimit | OrderByAndPageStatementOffsetAndOptionalLimit | OrderByAndPageStatementLimitOnly"""

    TYPE_ = hydra.core.Name("openGql.grammar.OrderByAndPageStatement")
    ORDER_BY_AND_OPTIONAL_OFFSET_AND_LIMIT = hydra.core.Name("orderByAndOptionalOffsetAndLimit")
    OFFSET_AND_OPTIONAL_LIMIT = hydra.core.Name("offsetAndOptionalLimit")
    LIMIT_ONLY = hydra.core.Name("limitOnly")

@dataclass(frozen=True)
class OrderByAndOptionalOffsetAndLimit:
    order_by: OrderByClause
    offset: Maybe[OffsetClause]
    limit: Maybe[LimitClause]

    TYPE_ = hydra.core.Name("openGql.grammar.OrderByAndOptionalOffsetAndLimit")
    ORDER_BY = hydra.core.Name("orderBy")
    OFFSET = hydra.core.Name("offset")
    LIMIT = hydra.core.Name("limit")

@dataclass(frozen=True)
class OffsetAndOptionalLimit:
    offset: OffsetClause
    limit: Maybe[LimitClause]

    TYPE_ = hydra.core.Name("openGql.grammar.OffsetAndOptionalLimit")
    OFFSET = hydra.core.Name("offset")
    LIMIT = hydra.core.Name("limit")

class PrimitiveResultStatementReturnAndOptionalOrderBy(Node["ReturnAndOptionalOrderByAndPage"]):
    ...

class PrimitiveResultStatementFinish:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, PrimitiveResultStatementFinish)
    def __hash__(self):
        return hash("PrimitiveResultStatementFinish")

class _PrimitiveResultStatementMeta(type):
    def __getitem__(cls, item):
        return object

class PrimitiveResultStatement(metaclass=_PrimitiveResultStatementMeta):
    r"""PrimitiveResultStatementReturnAndOptionalOrderBy | PrimitiveResultStatementFinish"""

    TYPE_ = hydra.core.Name("openGql.grammar.PrimitiveResultStatement")
    RETURN_AND_OPTIONAL_ORDER_BY = hydra.core.Name("returnAndOptionalOrderBy")
    FINISH = hydra.core.Name("finish")

@dataclass(frozen=True)
class ReturnAndOptionalOrderByAndPage:
    return_: ReturnStatement
    order_by_and_page: Maybe[OrderByAndPageStatement]

    TYPE_ = hydra.core.Name("openGql.grammar.ReturnAndOptionalOrderByAndPage")
    RETURN = hydra.core.Name("return")
    ORDER_BY_AND_PAGE = hydra.core.Name("orderByAndPage")

ReturnStatement: TypeAlias = "ReturnStatementBody"

class ReturnStatementBodyItems(Node["ReturnItemsAndGroupBy"]):
    ...

class ReturnStatementBodyNoBindings:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ReturnStatementBodyNoBindings)
    def __hash__(self):
        return hash("ReturnStatementBodyNoBindings")

class _ReturnStatementBodyMeta(type):
    def __getitem__(cls, item):
        return object

class ReturnStatementBody(metaclass=_ReturnStatementBodyMeta):
    r"""ReturnStatementBodyItems | ReturnStatementBodyNoBindings"""

    TYPE_ = hydra.core.Name("openGql.grammar.ReturnStatementBody")
    ITEMS = hydra.core.Name("items")
    NO_BINDINGS = hydra.core.Name("noBindings")

@dataclass(frozen=True)
class ReturnItemsAndGroupBy:
    quantifier: Maybe[SetQuantifier]
    items: ReturnItems
    group_by: Maybe[GroupByClause]

    TYPE_ = hydra.core.Name("openGql.grammar.ReturnItemsAndGroupBy")
    QUANTIFIER = hydra.core.Name("quantifier")
    ITEMS = hydra.core.Name("items")
    GROUP_BY = hydra.core.Name("groupBy")

class ReturnItemsAsterisk:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ReturnItemsAsterisk)
    def __hash__(self):
        return hash("ReturnItemsAsterisk")

class ReturnItemsItemList(Node["ReturnItemList"]):
    ...

class _ReturnItemsMeta(type):
    def __getitem__(cls, item):
        return object

class ReturnItems(metaclass=_ReturnItemsMeta):
    r"""ReturnItemsAsterisk | ReturnItemsItemList"""

    TYPE_ = hydra.core.Name("openGql.grammar.ReturnItems")
    ASTERISK = hydra.core.Name("asterisk")
    ITEM_LIST = hydra.core.Name("itemList")

ReturnItemList: TypeAlias = "frozenlist[ReturnItem]"

@dataclass(frozen=True)
class ReturnItem:
    expression: AggregatingValueExpression
    alias: Maybe[ReturnItemAlias]

    TYPE_ = hydra.core.Name("openGql.grammar.ReturnItem")
    EXPRESSION = hydra.core.Name("expression")
    ALIAS = hydra.core.Name("alias")

ReturnItemAlias: TypeAlias = "str"

@dataclass(frozen=True)
class SelectStatement:
    quantifier: Maybe[SetQuantifier]
    items: SelectItems
    body: Maybe[SelectStatementBodyAndClauses]

    TYPE_ = hydra.core.Name("openGql.grammar.SelectStatement")
    QUANTIFIER = hydra.core.Name("quantifier")
    ITEMS = hydra.core.Name("items")
    BODY = hydra.core.Name("body")

class SelectItemsAsterisk:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, SelectItemsAsterisk)
    def __hash__(self):
        return hash("SelectItemsAsterisk")

class SelectItemsItemList(Node["SelectItemList"]):
    ...

class _SelectItemsMeta(type):
    def __getitem__(cls, item):
        return object

class SelectItems(metaclass=_SelectItemsMeta):
    r"""SelectItemsAsterisk | SelectItemsItemList"""

    TYPE_ = hydra.core.Name("openGql.grammar.SelectItems")
    ASTERISK = hydra.core.Name("asterisk")
    ITEM_LIST = hydra.core.Name("itemList")

@dataclass(frozen=True)
class SelectStatementBodyAndClauses:
    body: SelectStatementBody
    where: Maybe[WhereClause]
    group_by: Maybe[GroupByClause]
    having: Maybe[HavingClause]
    order_by: Maybe[OrderByClause]
    offset: Maybe[OffsetClause]
    limit: Maybe[LimitClause]

    TYPE_ = hydra.core.Name("openGql.grammar.SelectStatementBodyAndClauses")
    BODY = hydra.core.Name("body")
    WHERE = hydra.core.Name("where")
    GROUP_BY = hydra.core.Name("groupBy")
    HAVING = hydra.core.Name("having")
    ORDER_BY = hydra.core.Name("orderBy")
    OFFSET = hydra.core.Name("offset")
    LIMIT = hydra.core.Name("limit")

SelectItemList: TypeAlias = "frozenlist[SelectItem]"

@dataclass(frozen=True)
class SelectItem:
    expression: AggregatingValueExpression
    alias: Maybe[SelectItemAlias]

    TYPE_ = hydra.core.Name("openGql.grammar.SelectItem")
    EXPRESSION = hydra.core.Name("expression")
    ALIAS = hydra.core.Name("alias")

SelectItemAlias: TypeAlias = "str"

HavingClause: TypeAlias = "SearchCondition"

class SelectStatementBodyGraphMatchList(Node["SelectGraphMatchList"]):
    ...

class SelectStatementBodyQuerySpecification(Node["SelectQuerySpecification"]):
    ...

class _SelectStatementBodyMeta(type):
    def __getitem__(cls, item):
        return object

class SelectStatementBody(metaclass=_SelectStatementBodyMeta):
    r"""SelectStatementBodyGraphMatchList | SelectStatementBodyQuerySpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.SelectStatementBody")
    GRAPH_MATCH_LIST = hydra.core.Name("graphMatchList")
    QUERY_SPECIFICATION = hydra.core.Name("querySpecification")

SelectGraphMatchList: TypeAlias = "frozenlist[SelectGraphMatch]"

@dataclass(frozen=True)
class SelectGraphMatch:
    graph_expression: GraphExpression
    match_statement: MatchStatement

    TYPE_ = hydra.core.Name("openGql.grammar.SelectGraphMatch")
    GRAPH_EXPRESSION = hydra.core.Name("graphExpression")
    MATCH_STATEMENT = hydra.core.Name("matchStatement")

class SelectQuerySpecificationNested(Node["NestedQuerySpecification"]):
    ...

class SelectQuerySpecificationGraphAndNested(Node["GraphAndNestedQuerySpecification"]):
    ...

class _SelectQuerySpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class SelectQuerySpecification(metaclass=_SelectQuerySpecificationMeta):
    r"""SelectQuerySpecificationNested | SelectQuerySpecificationGraphAndNested"""

    TYPE_ = hydra.core.Name("openGql.grammar.SelectQuerySpecification")
    NESTED = hydra.core.Name("nested")
    GRAPH_AND_NESTED = hydra.core.Name("graphAndNested")

@dataclass(frozen=True)
class GraphAndNestedQuerySpecification:
    graph_expression: GraphExpression
    nested: NestedQuerySpecification

    TYPE_ = hydra.core.Name("openGql.grammar.GraphAndNestedQuerySpecification")
    GRAPH_EXPRESSION = hydra.core.Name("graphExpression")
    NESTED = hydra.core.Name("nested")

@dataclass(frozen=True)
class CallProcedureStatement:
    optional: bool
    call: ProcedureCall

    TYPE_ = hydra.core.Name("openGql.grammar.CallProcedureStatement")
    OPTIONAL = hydra.core.Name("optional")
    CALL = hydra.core.Name("call")

class ProcedureCallInline(Node["InlineProcedureCall"]):
    ...

class ProcedureCallNamed(Node["NamedProcedureCall"]):
    ...

class _ProcedureCallMeta(type):
    def __getitem__(cls, item):
        return object

class ProcedureCall(metaclass=_ProcedureCallMeta):
    r"""ProcedureCallInline | ProcedureCallNamed"""

    TYPE_ = hydra.core.Name("openGql.grammar.ProcedureCall")
    INLINE = hydra.core.Name("inline")
    NAMED = hydra.core.Name("named")

@dataclass(frozen=True)
class InlineProcedureCall:
    scope: Maybe[VariableScopeClause]
    nested: NestedProcedureSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.InlineProcedureCall")
    SCOPE = hydra.core.Name("scope")
    NESTED = hydra.core.Name("nested")

VariableScopeClause: TypeAlias = "Maybe[BindingVariableReferenceList]"

BindingVariableReferenceList: TypeAlias = "frozenlist[BindingVariableReference]"

@dataclass(frozen=True)
class NamedProcedureCall:
    reference: ProcedureReference
    arguments: Maybe[ProcedureArgumentList]
    yield_: Maybe[YieldClause]

    TYPE_ = hydra.core.Name("openGql.grammar.NamedProcedureCall")
    REFERENCE = hydra.core.Name("reference")
    ARGUMENTS = hydra.core.Name("arguments")
    YIELD = hydra.core.Name("yield")

ProcedureArgumentList: TypeAlias = "frozenlist[ProcedureArgument]"

ProcedureArgument: TypeAlias = "ValueExpression"

AtSchemaClause: TypeAlias = "SchemaReference"

UseGraphClause: TypeAlias = "GraphExpression"

@dataclass(frozen=True)
class GraphPatternBindingTable:
    pattern: GraphPattern
    yield_clause: Maybe[GraphPatternYieldClause]

    TYPE_ = hydra.core.Name("openGql.grammar.GraphPatternBindingTable")
    PATTERN = hydra.core.Name("pattern")
    YIELD_CLAUSE = hydra.core.Name("yieldClause")

GraphPatternYieldClause: TypeAlias = "GraphPatternYieldItemList"

class GraphPatternYieldItemListItems(Node["frozenlist[GraphPatternYieldItem]"]):
    ...

class GraphPatternYieldItemListNoBindings:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GraphPatternYieldItemListNoBindings)
    def __hash__(self):
        return hash("GraphPatternYieldItemListNoBindings")

class _GraphPatternYieldItemListMeta(type):
    def __getitem__(cls, item):
        return object

class GraphPatternYieldItemList(metaclass=_GraphPatternYieldItemListMeta):
    r"""GraphPatternYieldItemListItems | GraphPatternYieldItemListNoBindings"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphPatternYieldItemList")
    ITEMS = hydra.core.Name("items")
    NO_BINDINGS = hydra.core.Name("noBindings")

GraphPatternYieldItem: TypeAlias = "BindingVariableReference"

@dataclass(frozen=True)
class GraphPattern:
    match_mode: Maybe[MatchMode]
    path_patterns: PathPatternList
    keep_clause: Maybe[KeepClause]
    where_clause: Maybe[GraphPatternWhereClause]

    TYPE_ = hydra.core.Name("openGql.grammar.GraphPattern")
    MATCH_MODE = hydra.core.Name("matchMode")
    PATH_PATTERNS = hydra.core.Name("pathPatterns")
    KEEP_CLAUSE = hydra.core.Name("keepClause")
    WHERE_CLAUSE = hydra.core.Name("whereClause")

class MatchModeRepeatableElements(Node["RepeatableElementsMatchMode"]):
    ...

class MatchModeDifferentEdges(Node["DifferentEdgesMatchMode"]):
    ...

class _MatchModeMeta(type):
    def __getitem__(cls, item):
        return object

class MatchMode(metaclass=_MatchModeMeta):
    r"""MatchModeRepeatableElements | MatchModeDifferentEdges"""

    TYPE_ = hydra.core.Name("openGql.grammar.MatchMode")
    REPEATABLE_ELEMENTS = hydra.core.Name("repeatableElements")
    DIFFERENT_EDGES = hydra.core.Name("differentEdges")

RepeatableElementsMatchMode: TypeAlias = "ElementBindingsOrElements"

DifferentEdgesMatchMode: TypeAlias = "EdgeBindingsOrEdges"

class ElementBindingsOrElementsElementBindings(Node[bool]):
    ...

class ElementBindingsOrElementsElements:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ElementBindingsOrElementsElements)
    def __hash__(self):
        return hash("ElementBindingsOrElementsElements")

class _ElementBindingsOrElementsMeta(type):
    def __getitem__(cls, item):
        return object

class ElementBindingsOrElements(metaclass=_ElementBindingsOrElementsMeta):
    r"""ElementBindingsOrElementsElementBindings | ElementBindingsOrElementsElements"""

    TYPE_ = hydra.core.Name("openGql.grammar.ElementBindingsOrElements")
    ELEMENT_BINDINGS = hydra.core.Name("elementBindings")
    ELEMENTS = hydra.core.Name("elements")

class EdgeBindingsOrEdgesEdgeBindings(Node[bool]):
    ...

class EdgeBindingsOrEdgesEdges:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, EdgeBindingsOrEdgesEdges)
    def __hash__(self):
        return hash("EdgeBindingsOrEdgesEdges")

class _EdgeBindingsOrEdgesMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeBindingsOrEdges(metaclass=_EdgeBindingsOrEdgesMeta):
    r"""EdgeBindingsOrEdgesEdgeBindings | EdgeBindingsOrEdgesEdges"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeBindingsOrEdges")
    EDGE_BINDINGS = hydra.core.Name("edgeBindings")
    EDGES = hydra.core.Name("edges")

PathPatternList: TypeAlias = "frozenlist[PathPattern]"

@dataclass(frozen=True)
class PathPattern:
    variable_declaration: Maybe[PathVariableDeclaration]
    prefix: Maybe[PathPatternPrefix]
    expression: PathPatternExpression

    TYPE_ = hydra.core.Name("openGql.grammar.PathPattern")
    VARIABLE_DECLARATION = hydra.core.Name("variableDeclaration")
    PREFIX = hydra.core.Name("prefix")
    EXPRESSION = hydra.core.Name("expression")

PathVariableDeclaration: TypeAlias = "PathVariable"

KeepClause: TypeAlias = "PathPatternPrefix"

GraphPatternWhereClause: TypeAlias = "SearchCondition"

InsertGraphPattern: TypeAlias = "InsertPathPatternList"

InsertPathPatternList: TypeAlias = "frozenlist[InsertPathPattern]"

@dataclass(frozen=True)
class InsertPathPattern:
    start_node: InsertNodePattern
    edges_and_nodes: frozenlist[InsertEdgeAndNode]

    TYPE_ = hydra.core.Name("openGql.grammar.InsertPathPattern")
    START_NODE = hydra.core.Name("startNode")
    EDGES_AND_NODES = hydra.core.Name("edgesAndNodes")

@dataclass(frozen=True)
class InsertEdgeAndNode:
    edge: InsertEdgePattern
    node: InsertNodePattern

    TYPE_ = hydra.core.Name("openGql.grammar.InsertEdgeAndNode")
    EDGE = hydra.core.Name("edge")
    NODE = hydra.core.Name("node")

InsertNodePattern: TypeAlias = "Maybe[InsertElementPatternFiller]"

class InsertEdgePatternPointingLeft(Node["InsertEdgePointingLeft"]):
    ...

class InsertEdgePatternPointingRight(Node["InsertEdgePointingRight"]):
    ...

class InsertEdgePatternUndirected(Node["InsertEdgeUndirected"]):
    ...

class _InsertEdgePatternMeta(type):
    def __getitem__(cls, item):
        return object

class InsertEdgePattern(metaclass=_InsertEdgePatternMeta):
    r"""InsertEdgePatternPointingLeft | InsertEdgePatternPointingRight | InsertEdgePatternUndirected"""

    TYPE_ = hydra.core.Name("openGql.grammar.InsertEdgePattern")
    POINTING_LEFT = hydra.core.Name("pointingLeft")
    POINTING_RIGHT = hydra.core.Name("pointingRight")
    UNDIRECTED = hydra.core.Name("undirected")

InsertEdgePointingLeft: TypeAlias = "Maybe[InsertElementPatternFiller]"

InsertEdgePointingRight: TypeAlias = "Maybe[InsertElementPatternFiller]"

InsertEdgeUndirected: TypeAlias = "Maybe[InsertElementPatternFiller]"

@dataclass(frozen=True)
class InsertElementPatternFiller:
    variable_declaration: Maybe[ElementVariableDeclaration]
    label_and_properties: Maybe[LabelAndPropertySetSpecification]

    TYPE_ = hydra.core.Name("openGql.grammar.InsertElementPatternFiller")
    VARIABLE_DECLARATION = hydra.core.Name("variableDeclaration")
    LABEL_AND_PROPERTIES = hydra.core.Name("labelAndProperties")

@dataclass(frozen=True)
class LabelAndPropertySetSpecification:
    is_or_colon: Maybe[IsOrColon]
    label_set: Maybe[LabelSetSpecification]
    property_specification: Maybe[ElementPropertySpecification]

    TYPE_ = hydra.core.Name("openGql.grammar.LabelAndPropertySetSpecification")
    IS_OR_COLON = hydra.core.Name("isOrColon")
    LABEL_SET = hydra.core.Name("labelSet")
    PROPERTY_SPECIFICATION = hydra.core.Name("propertySpecification")

class PathPatternPrefixModePrefix(Node["PathModePrefix"]):
    ...

class PathPatternPrefixSearchPrefix(Node["PathSearchPrefix"]):
    ...

class _PathPatternPrefixMeta(type):
    def __getitem__(cls, item):
        return object

class PathPatternPrefix(metaclass=_PathPatternPrefixMeta):
    r"""PathPatternPrefixModePrefix | PathPatternPrefixSearchPrefix"""

    TYPE_ = hydra.core.Name("openGql.grammar.PathPatternPrefix")
    MODE_PREFIX = hydra.core.Name("modePrefix")
    SEARCH_PREFIX = hydra.core.Name("searchPrefix")

@dataclass(frozen=True)
class PathModePrefix:
    mode: PathMode
    or_paths: Maybe[PathOrPaths]

    TYPE_ = hydra.core.Name("openGql.grammar.PathModePrefix")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")

class PathMode(Enum):
    WALK = hydra.core.Name("walk")

    TRAIL = hydra.core.Name("trail")

    SIMPLE = hydra.core.Name("simple")

    ACYCLIC = hydra.core.Name("acyclic")

PathMode.TYPE_ = hydra.core.Name("openGql.grammar.PathMode")

class PathSearchPrefixAll(Node["AllPathSearch"]):
    ...

class PathSearchPrefixAny(Node["AnyPathSearch"]):
    ...

class PathSearchPrefixShortest(Node["ShortestPathSearch"]):
    ...

class _PathSearchPrefixMeta(type):
    def __getitem__(cls, item):
        return object

class PathSearchPrefix(metaclass=_PathSearchPrefixMeta):
    r"""PathSearchPrefixAll | PathSearchPrefixAny | PathSearchPrefixShortest"""

    TYPE_ = hydra.core.Name("openGql.grammar.PathSearchPrefix")
    ALL = hydra.core.Name("all")
    ANY = hydra.core.Name("any")
    SHORTEST = hydra.core.Name("shortest")

@dataclass(frozen=True)
class AllPathSearch:
    mode: Maybe[PathMode]
    or_paths: Maybe[PathOrPaths]

    TYPE_ = hydra.core.Name("openGql.grammar.AllPathSearch")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")

class PathOrPaths(Enum):
    PATH = hydra.core.Name("path")

    PATHS = hydra.core.Name("paths")

PathOrPaths.TYPE_ = hydra.core.Name("openGql.grammar.PathOrPaths")

@dataclass(frozen=True)
class AnyPathSearch:
    number_of_paths: Maybe[NumberOfPaths]
    mode: Maybe[PathMode]
    or_paths: Maybe[PathOrPaths]

    TYPE_ = hydra.core.Name("openGql.grammar.AnyPathSearch")
    NUMBER_OF_PATHS = hydra.core.Name("numberOfPaths")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")

NumberOfPaths: TypeAlias = "NonNegativeIntegerSpecification"

class ShortestPathSearchAllShortest(Node["AllShortestPathSearch"]):
    ...

class ShortestPathSearchAnyShortest(Node["AnyShortestPathSearch"]):
    ...

class ShortestPathSearchCountedShortest(Node["CountedShortestPathSearch"]):
    ...

class ShortestPathSearchCountedShortestGroup(Node["CountedShortestGroupSearch"]):
    ...

class _ShortestPathSearchMeta(type):
    def __getitem__(cls, item):
        return object

class ShortestPathSearch(metaclass=_ShortestPathSearchMeta):
    r"""ShortestPathSearchAllShortest | ShortestPathSearchAnyShortest | ShortestPathSearchCountedShortest | ShortestPathSearchCountedShortestGroup"""

    TYPE_ = hydra.core.Name("openGql.grammar.ShortestPathSearch")
    ALL_SHORTEST = hydra.core.Name("allShortest")
    ANY_SHORTEST = hydra.core.Name("anyShortest")
    COUNTED_SHORTEST = hydra.core.Name("countedShortest")
    COUNTED_SHORTEST_GROUP = hydra.core.Name("countedShortestGroup")

@dataclass(frozen=True)
class AllShortestPathSearch:
    mode: Maybe[PathMode]
    or_paths: Maybe[PathOrPaths]

    TYPE_ = hydra.core.Name("openGql.grammar.AllShortestPathSearch")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")

@dataclass(frozen=True)
class AnyShortestPathSearch:
    mode: Maybe[PathMode]
    or_paths: Maybe[PathOrPaths]

    TYPE_ = hydra.core.Name("openGql.grammar.AnyShortestPathSearch")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")

@dataclass(frozen=True)
class CountedShortestPathSearch:
    number_of_paths: NumberOfPaths
    mode: Maybe[PathMode]
    or_paths: Maybe[PathOrPaths]

    TYPE_ = hydra.core.Name("openGql.grammar.CountedShortestPathSearch")
    NUMBER_OF_PATHS = hydra.core.Name("numberOfPaths")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")

@dataclass(frozen=True)
class CountedShortestGroupSearch:
    number_of_groups: Maybe[NumberOfGroups]
    mode: Maybe[PathMode]
    or_paths: Maybe[PathOrPaths]
    groups: bool

    TYPE_ = hydra.core.Name("openGql.grammar.CountedShortestGroupSearch")
    NUMBER_OF_GROUPS = hydra.core.Name("numberOfGroups")
    MODE = hydra.core.Name("mode")
    OR_PATHS = hydra.core.Name("orPaths")
    GROUPS = hydra.core.Name("groups")

NumberOfGroups: TypeAlias = "NonNegativeIntegerSpecification"

class PathPatternExpressionTerm(Node["PathTerm"]):
    ...

class PathPatternExpressionMultisetAlternation(Node["frozenlist[PathTerm]"]):
    ...

class PathPatternExpressionPatternUnion(Node["frozenlist[PathTerm]"]):
    ...

class _PathPatternExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PathPatternExpression(metaclass=_PathPatternExpressionMeta):
    r"""PathPatternExpressionTerm | PathPatternExpressionMultisetAlternation | PathPatternExpressionPatternUnion"""

    TYPE_ = hydra.core.Name("openGql.grammar.PathPatternExpression")
    TERM = hydra.core.Name("term")
    MULTISET_ALTERNATION = hydra.core.Name("multisetAlternation")
    PATTERN_UNION = hydra.core.Name("patternUnion")

PathTerm: TypeAlias = "frozenlist[PathFactor]"

class PathFactorPrimary(Node["PathPrimary"]):
    ...

class PathFactorQuantifiedPrimary(Node["QuantifiedPathPrimary"]):
    ...

class PathFactorQuestionedPrimary(Node["QuestionedPathPrimary"]):
    ...

class _PathFactorMeta(type):
    def __getitem__(cls, item):
        return object

class PathFactor(metaclass=_PathFactorMeta):
    r"""PathFactorPrimary | PathFactorQuantifiedPrimary | PathFactorQuestionedPrimary"""

    TYPE_ = hydra.core.Name("openGql.grammar.PathFactor")
    PRIMARY = hydra.core.Name("primary")
    QUANTIFIED_PRIMARY = hydra.core.Name("quantifiedPrimary")
    QUESTIONED_PRIMARY = hydra.core.Name("questionedPrimary")

@dataclass(frozen=True)
class QuantifiedPathPrimary:
    primary: PathPrimary
    quantifier: GraphPatternQuantifier

    TYPE_ = hydra.core.Name("openGql.grammar.QuantifiedPathPrimary")
    PRIMARY = hydra.core.Name("primary")
    QUANTIFIER = hydra.core.Name("quantifier")

QuestionedPathPrimary: TypeAlias = "PathPrimary"

class PathPrimaryElementPattern(Node["ElementPattern"]):
    ...

class PathPrimaryParenthesizedExpression(Node["ParenthesizedPathPatternExpression"]):
    ...

class PathPrimarySimplifiedExpression(Node["SimplifiedPathPatternExpression"]):
    ...

class _PathPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class PathPrimary(metaclass=_PathPrimaryMeta):
    r"""PathPrimaryElementPattern | PathPrimaryParenthesizedExpression | PathPrimarySimplifiedExpression"""

    TYPE_ = hydra.core.Name("openGql.grammar.PathPrimary")
    ELEMENT_PATTERN = hydra.core.Name("elementPattern")
    PARENTHESIZED_EXPRESSION = hydra.core.Name("parenthesizedExpression")
    SIMPLIFIED_EXPRESSION = hydra.core.Name("simplifiedExpression")

class ElementPatternNode(Node["NodePattern"]):
    ...

class ElementPatternEdge(Node["EdgePattern"]):
    ...

class _ElementPatternMeta(type):
    def __getitem__(cls, item):
        return object

class ElementPattern(metaclass=_ElementPatternMeta):
    r"""ElementPatternNode | ElementPatternEdge"""

    TYPE_ = hydra.core.Name("openGql.grammar.ElementPattern")
    NODE = hydra.core.Name("node")
    EDGE = hydra.core.Name("edge")

NodePattern: TypeAlias = "ElementPatternFiller"

@dataclass(frozen=True)
class ElementPatternFiller:
    variable_declaration: Maybe[ElementVariableDeclaration]
    is_label_expression: Maybe[IsLabelExpression]
    predicate: Maybe[ElementPatternPredicate]

    TYPE_ = hydra.core.Name("openGql.grammar.ElementPatternFiller")
    VARIABLE_DECLARATION = hydra.core.Name("variableDeclaration")
    IS_LABEL_EXPRESSION = hydra.core.Name("isLabelExpression")
    PREDICATE = hydra.core.Name("predicate")

@dataclass(frozen=True)
class ElementVariableDeclaration:
    temp: Maybe[bool]
    variable: ElementVariable

    TYPE_ = hydra.core.Name("openGql.grammar.ElementVariableDeclaration")
    TEMP = hydra.core.Name("temp")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class IsLabelExpression:
    is_or_colon: IsOrColon
    label: LabelExpression

    TYPE_ = hydra.core.Name("openGql.grammar.IsLabelExpression")
    IS_OR_COLON = hydra.core.Name("isOrColon")
    LABEL = hydra.core.Name("label")

class IsOrColon(Enum):
    IS = hydra.core.Name("is")

    COLON = hydra.core.Name("colon")

IsOrColon.TYPE_ = hydra.core.Name("openGql.grammar.IsOrColon")

class ElementPatternPredicateWhereClause(Node["ElementPatternWhereClause"]):
    ...

class ElementPatternPredicatePropertySpecification(Node["ElementPropertySpecification"]):
    ...

class _ElementPatternPredicateMeta(type):
    def __getitem__(cls, item):
        return object

class ElementPatternPredicate(metaclass=_ElementPatternPredicateMeta):
    r"""ElementPatternPredicateWhereClause | ElementPatternPredicatePropertySpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.ElementPatternPredicate")
    WHERE_CLAUSE = hydra.core.Name("whereClause")
    PROPERTY_SPECIFICATION = hydra.core.Name("propertySpecification")

ElementPatternWhereClause: TypeAlias = "SearchCondition"

ElementPropertySpecification: TypeAlias = "PropertyKeyValuePairList"

PropertyKeyValuePairList: TypeAlias = "frozenlist[PropertyKeyValuePair]"

@dataclass(frozen=True)
class PropertyKeyValuePair:
    name: PropertyName
    value: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.PropertyKeyValuePair")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class EdgePatternFullEdge(Node["FullEdgePattern"]):
    ...

class EdgePatternAbbreviatedEdge(Node["AbbreviatedEdgePattern"]):
    ...

class _EdgePatternMeta(type):
    def __getitem__(cls, item):
        return object

class EdgePattern(metaclass=_EdgePatternMeta):
    r"""EdgePatternFullEdge | EdgePatternAbbreviatedEdge"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgePattern")
    FULL_EDGE = hydra.core.Name("fullEdge")
    ABBREVIATED_EDGE = hydra.core.Name("abbreviatedEdge")

class FullEdgePatternPointingLeft(Node["FullEdgePointingLeft"]):
    ...

class FullEdgePatternUndirected(Node["FullEdgeUndirected"]):
    ...

class FullEdgePatternPointingRight(Node["FullEdgePointingRight"]):
    ...

class FullEdgePatternLeftOrUndirected(Node["FullEdgeLeftOrUndirected"]):
    ...

class FullEdgePatternUndirectedOrRight(Node["FullEdgeUndirectedOrRight"]):
    ...

class FullEdgePatternLeftOrRight(Node["FullEdgeLeftOrRight"]):
    ...

class FullEdgePatternAnyDirection(Node["FullEdgeAnyDirection"]):
    ...

class _FullEdgePatternMeta(type):
    def __getitem__(cls, item):
        return object

class FullEdgePattern(metaclass=_FullEdgePatternMeta):
    r"""FullEdgePatternPointingLeft | FullEdgePatternUndirected | FullEdgePatternPointingRight | FullEdgePatternLeftOrUndirected | FullEdgePatternUndirectedOrRight | FullEdgePatternLeftOrRight | FullEdgePatternAnyDirection"""

    TYPE_ = hydra.core.Name("openGql.grammar.FullEdgePattern")
    POINTING_LEFT = hydra.core.Name("pointingLeft")
    UNDIRECTED = hydra.core.Name("undirected")
    POINTING_RIGHT = hydra.core.Name("pointingRight")
    LEFT_OR_UNDIRECTED = hydra.core.Name("leftOrUndirected")
    UNDIRECTED_OR_RIGHT = hydra.core.Name("undirectedOrRight")
    LEFT_OR_RIGHT = hydra.core.Name("leftOrRight")
    ANY_DIRECTION = hydra.core.Name("anyDirection")

FullEdgePointingLeft: TypeAlias = "ElementPatternFiller"

FullEdgeUndirected: TypeAlias = "ElementPatternFiller"

FullEdgePointingRight: TypeAlias = "ElementPatternFiller"

FullEdgeLeftOrUndirected: TypeAlias = "ElementPatternFiller"

FullEdgeUndirectedOrRight: TypeAlias = "ElementPatternFiller"

FullEdgeLeftOrRight: TypeAlias = "ElementPatternFiller"

FullEdgeAnyDirection: TypeAlias = "ElementPatternFiller"

class AbbreviatedEdgePattern(Enum):
    LEFT_ARROW = hydra.core.Name("leftArrow")

    TILDE = hydra.core.Name("tilde")

    RIGHT_ARROW = hydra.core.Name("rightArrow")

    LEFT_ARROW_TILDE = hydra.core.Name("leftArrowTilde")

    TILDE_RIGHT_ARROW = hydra.core.Name("tildeRightArrow")

    LEFT_MINUS_RIGHT = hydra.core.Name("leftMinusRight")

    MINUS_SIGN = hydra.core.Name("minusSign")

AbbreviatedEdgePattern.TYPE_ = hydra.core.Name("openGql.grammar.AbbreviatedEdgePattern")

@dataclass(frozen=True)
class ParenthesizedPathPatternExpression:
    subpath_declaration: Maybe[SubpathVariableDeclaration]
    path_mode: Maybe[PathModePrefix]
    expression: PathPatternExpression
    where_clause: Maybe[ParenthesizedPathPatternWhereClause]

    TYPE_ = hydra.core.Name("openGql.grammar.ParenthesizedPathPatternExpression")
    SUBPATH_DECLARATION = hydra.core.Name("subpathDeclaration")
    PATH_MODE = hydra.core.Name("pathMode")
    EXPRESSION = hydra.core.Name("expression")
    WHERE_CLAUSE = hydra.core.Name("whereClause")

SubpathVariableDeclaration: TypeAlias = "SubpathVariable"

ParenthesizedPathPatternWhereClause: TypeAlias = "SearchCondition"

class LabelExpressionNegation(Node["LabelExpression"]):
    ...

class LabelExpressionConjunction(Node["ConjunctionLabelExpression"]):
    ...

class LabelExpressionDisjunction(Node["DisjunctionLabelExpression"]):
    ...

class LabelExpressionName(Node["LabelName"]):
    ...

class LabelExpressionWildcard:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LabelExpressionWildcard)
    def __hash__(self):
        return hash("LabelExpressionWildcard")

class LabelExpressionParenthesized(Node["LabelExpression"]):
    ...

class _LabelExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class LabelExpression(metaclass=_LabelExpressionMeta):
    r"""LabelExpressionNegation | LabelExpressionConjunction | LabelExpressionDisjunction | LabelExpressionName | LabelExpressionWildcard | LabelExpressionParenthesized"""

    TYPE_ = hydra.core.Name("openGql.grammar.LabelExpression")
    NEGATION = hydra.core.Name("negation")
    CONJUNCTION = hydra.core.Name("conjunction")
    DISJUNCTION = hydra.core.Name("disjunction")
    NAME = hydra.core.Name("name")
    WILDCARD = hydra.core.Name("wildcard")
    PARENTHESIZED = hydra.core.Name("parenthesized")

@dataclass(frozen=True)
class ConjunctionLabelExpression:
    left: LabelExpression
    right: LabelExpression

    TYPE_ = hydra.core.Name("openGql.grammar.ConjunctionLabelExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class DisjunctionLabelExpression:
    left: LabelExpression
    right: LabelExpression

    TYPE_ = hydra.core.Name("openGql.grammar.DisjunctionLabelExpression")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

PathVariableReference: TypeAlias = "BindingVariableReference"

ElementVariableReference: TypeAlias = "BindingVariableReference"

class GraphPatternQuantifierAsterisk:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GraphPatternQuantifierAsterisk)
    def __hash__(self):
        return hash("GraphPatternQuantifierAsterisk")

class GraphPatternQuantifierPlusSign:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GraphPatternQuantifierPlusSign)
    def __hash__(self):
        return hash("GraphPatternQuantifierPlusSign")

class GraphPatternQuantifierFixed(Node["FixedQuantifier"]):
    ...

class GraphPatternQuantifierGeneral(Node["GeneralQuantifier"]):
    ...

class _GraphPatternQuantifierMeta(type):
    def __getitem__(cls, item):
        return object

class GraphPatternQuantifier(metaclass=_GraphPatternQuantifierMeta):
    r"""GraphPatternQuantifierAsterisk | GraphPatternQuantifierPlusSign | GraphPatternQuantifierFixed | GraphPatternQuantifierGeneral"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphPatternQuantifier")
    ASTERISK = hydra.core.Name("asterisk")
    PLUS_SIGN = hydra.core.Name("plusSign")
    FIXED = hydra.core.Name("fixed")
    GENERAL = hydra.core.Name("general")

FixedQuantifier: TypeAlias = "UnsignedInteger"

@dataclass(frozen=True)
class GeneralQuantifier:
    lower_bound: Maybe[LowerBound]
    upper_bound: Maybe[UpperBound]

    TYPE_ = hydra.core.Name("openGql.grammar.GeneralQuantifier")
    LOWER_BOUND = hydra.core.Name("lowerBound")
    UPPER_BOUND = hydra.core.Name("upperBound")

LowerBound: TypeAlias = "UnsignedInteger"

UpperBound: TypeAlias = "UnsignedInteger"

class SimplifiedPathPatternExpressionLeft(Node["SimplifiedDefaultingLeft"]):
    ...

class SimplifiedPathPatternExpressionUndirected(Node["SimplifiedDefaultingUndirected"]):
    ...

class SimplifiedPathPatternExpressionRight(Node["SimplifiedDefaultingRight"]):
    ...

class SimplifiedPathPatternExpressionLeftOrUndirected(Node["SimplifiedDefaultingLeftOrUndirected"]):
    ...

class SimplifiedPathPatternExpressionUndirectedOrRight(Node["SimplifiedDefaultingUndirectedOrRight"]):
    ...

class SimplifiedPathPatternExpressionLeftOrRight(Node["SimplifiedDefaultingLeftOrRight"]):
    ...

class SimplifiedPathPatternExpressionAnyDirection(Node["SimplifiedDefaultingAnyDirection"]):
    ...

class _SimplifiedPathPatternExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedPathPatternExpression(metaclass=_SimplifiedPathPatternExpressionMeta):
    r"""SimplifiedPathPatternExpressionLeft | SimplifiedPathPatternExpressionUndirected | SimplifiedPathPatternExpressionRight | SimplifiedPathPatternExpressionLeftOrUndirected | SimplifiedPathPatternExpressionUndirectedOrRight | SimplifiedPathPatternExpressionLeftOrRight | SimplifiedPathPatternExpressionAnyDirection"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedPathPatternExpression")
    LEFT = hydra.core.Name("left")
    UNDIRECTED = hydra.core.Name("undirected")
    RIGHT = hydra.core.Name("right")
    LEFT_OR_UNDIRECTED = hydra.core.Name("leftOrUndirected")
    UNDIRECTED_OR_RIGHT = hydra.core.Name("undirectedOrRight")
    LEFT_OR_RIGHT = hydra.core.Name("leftOrRight")
    ANY_DIRECTION = hydra.core.Name("anyDirection")

SimplifiedDefaultingLeft: TypeAlias = "SimplifiedContents"

SimplifiedDefaultingUndirected: TypeAlias = "SimplifiedContents"

SimplifiedDefaultingRight: TypeAlias = "SimplifiedContents"

SimplifiedDefaultingLeftOrUndirected: TypeAlias = "SimplifiedContents"

SimplifiedDefaultingUndirectedOrRight: TypeAlias = "SimplifiedContents"

SimplifiedDefaultingLeftOrRight: TypeAlias = "SimplifiedContents"

SimplifiedDefaultingAnyDirection: TypeAlias = "SimplifiedContents"

class SimplifiedContentsTerm(Node["SimplifiedTerm"]):
    ...

class SimplifiedContentsPathUnion(Node["SimplifiedPathUnion"]):
    ...

class SimplifiedContentsMultisetAlternation(Node["SimplifiedMultisetAlternation"]):
    ...

class _SimplifiedContentsMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedContents(metaclass=_SimplifiedContentsMeta):
    r"""SimplifiedContentsTerm | SimplifiedContentsPathUnion | SimplifiedContentsMultisetAlternation"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedContents")
    TERM = hydra.core.Name("term")
    PATH_UNION = hydra.core.Name("pathUnion")
    MULTISET_ALTERNATION = hydra.core.Name("multisetAlternation")

SimplifiedPathUnion: TypeAlias = "frozenlist[SimplifiedTerm]"

SimplifiedMultisetAlternation: TypeAlias = "frozenlist[SimplifiedTerm]"

class SimplifiedTermFactorLow(Node["SimplifiedFactorLow"]):
    ...

class SimplifiedTermConcatenation(Node["SimplifiedConcatenation"]):
    ...

class _SimplifiedTermMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedTerm(metaclass=_SimplifiedTermMeta):
    r"""SimplifiedTermFactorLow | SimplifiedTermConcatenation"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedTerm")
    FACTOR_LOW = hydra.core.Name("factorLow")
    CONCATENATION = hydra.core.Name("concatenation")

@dataclass(frozen=True)
class SimplifiedConcatenation:
    initial_term: SimplifiedTerm
    next_factor: SimplifiedFactorLow

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedConcatenation")
    INITIAL_TERM = hydra.core.Name("initialTerm")
    NEXT_FACTOR = hydra.core.Name("nextFactor")

class SimplifiedFactorLowFactorHigh(Node["SimplifiedFactorHigh"]):
    ...

class SimplifiedFactorLowConjunction(Node["SimplifiedConjunction"]):
    ...

class _SimplifiedFactorLowMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedFactorLow(metaclass=_SimplifiedFactorLowMeta):
    r"""SimplifiedFactorLowFactorHigh | SimplifiedFactorLowConjunction"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedFactorLow")
    FACTOR_HIGH = hydra.core.Name("factorHigh")
    CONJUNCTION = hydra.core.Name("conjunction")

@dataclass(frozen=True)
class SimplifiedConjunction:
    left: SimplifiedFactorLow
    right: SimplifiedFactorHigh

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedConjunction")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

class SimplifiedFactorHighTertiary(Node["SimplifiedTertiary"]):
    ...

class SimplifiedFactorHighQuantified(Node["SimplifiedQuantified"]):
    ...

class SimplifiedFactorHighQuestioned(Node["SimplifiedQuestioned"]):
    ...

class _SimplifiedFactorHighMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedFactorHigh(metaclass=_SimplifiedFactorHighMeta):
    r"""SimplifiedFactorHighTertiary | SimplifiedFactorHighQuantified | SimplifiedFactorHighQuestioned"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedFactorHigh")
    TERTIARY = hydra.core.Name("tertiary")
    QUANTIFIED = hydra.core.Name("quantified")
    QUESTIONED = hydra.core.Name("questioned")

@dataclass(frozen=True)
class SimplifiedQuantified:
    tertiary: SimplifiedTertiary
    quantifier: GraphPatternQuantifier

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedQuantified")
    TERTIARY = hydra.core.Name("tertiary")
    QUANTIFIER = hydra.core.Name("quantifier")

SimplifiedQuestioned: TypeAlias = "SimplifiedTertiary"

class SimplifiedTertiaryDirectionOverride(Node["SimplifiedDirectionOverride"]):
    ...

class SimplifiedTertiarySecondary(Node["SimplifiedSecondary"]):
    ...

class _SimplifiedTertiaryMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedTertiary(metaclass=_SimplifiedTertiaryMeta):
    r"""SimplifiedTertiaryDirectionOverride | SimplifiedTertiarySecondary"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedTertiary")
    DIRECTION_OVERRIDE = hydra.core.Name("directionOverride")
    SECONDARY = hydra.core.Name("secondary")

class SimplifiedDirectionOverrideOverrideLeft(Node["SimplifiedOverrideLeft"]):
    ...

class SimplifiedDirectionOverrideOverrideUndirected(Node["SimplifiedOverrideUndirected"]):
    ...

class SimplifiedDirectionOverrideOverrideRight(Node["SimplifiedOverrideRight"]):
    ...

class SimplifiedDirectionOverrideOverrideLeftOrUndirected(Node["SimplifiedOverrideLeftOrUndirected"]):
    ...

class SimplifiedDirectionOverrideOverrideUndirectedOrRight(Node["SimplifiedOverrideUndirectedOrRight"]):
    ...

class SimplifiedDirectionOverrideOverrideLeftOrRight(Node["SimplifiedOverrideLeftOrRight"]):
    ...

class SimplifiedDirectionOverrideOverrideAnyDirection(Node["SimplifiedOverrideAnyDirection"]):
    ...

class _SimplifiedDirectionOverrideMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedDirectionOverride(metaclass=_SimplifiedDirectionOverrideMeta):
    r"""SimplifiedDirectionOverrideOverrideLeft | SimplifiedDirectionOverrideOverrideUndirected | SimplifiedDirectionOverrideOverrideRight | SimplifiedDirectionOverrideOverrideLeftOrUndirected | SimplifiedDirectionOverrideOverrideUndirectedOrRight | SimplifiedDirectionOverrideOverrideLeftOrRight | SimplifiedDirectionOverrideOverrideAnyDirection"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedDirectionOverride")
    OVERRIDE_LEFT = hydra.core.Name("overrideLeft")
    OVERRIDE_UNDIRECTED = hydra.core.Name("overrideUndirected")
    OVERRIDE_RIGHT = hydra.core.Name("overrideRight")
    OVERRIDE_LEFT_OR_UNDIRECTED = hydra.core.Name("overrideLeftOrUndirected")
    OVERRIDE_UNDIRECTED_OR_RIGHT = hydra.core.Name("overrideUndirectedOrRight")
    OVERRIDE_LEFT_OR_RIGHT = hydra.core.Name("overrideLeftOrRight")
    OVERRIDE_ANY_DIRECTION = hydra.core.Name("overrideAnyDirection")

SimplifiedOverrideLeft: TypeAlias = "SimplifiedSecondary"

SimplifiedOverrideUndirected: TypeAlias = "SimplifiedSecondary"

SimplifiedOverrideRight: TypeAlias = "SimplifiedSecondary"

SimplifiedOverrideLeftOrUndirected: TypeAlias = "SimplifiedSecondary"

SimplifiedOverrideUndirectedOrRight: TypeAlias = "SimplifiedSecondary"

SimplifiedOverrideLeftOrRight: TypeAlias = "SimplifiedSecondary"

SimplifiedOverrideAnyDirection: TypeAlias = "SimplifiedSecondary"

class SimplifiedSecondaryPrimary(Node["SimplifiedPrimary"]):
    ...

class SimplifiedSecondaryNegation(Node["SimplifiedNegation"]):
    ...

class _SimplifiedSecondaryMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedSecondary(metaclass=_SimplifiedSecondaryMeta):
    r"""SimplifiedSecondaryPrimary | SimplifiedSecondaryNegation"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedSecondary")
    PRIMARY = hydra.core.Name("primary")
    NEGATION = hydra.core.Name("negation")

SimplifiedNegation: TypeAlias = "SimplifiedPrimary"

class SimplifiedPrimaryLabelName(Node["LabelName"]):
    ...

class SimplifiedPrimaryParenthesizedContents(Node["SimplifiedContents"]):
    ...

class _SimplifiedPrimaryMeta(type):
    def __getitem__(cls, item):
        return object

class SimplifiedPrimary(metaclass=_SimplifiedPrimaryMeta):
    r"""SimplifiedPrimaryLabelName | SimplifiedPrimaryParenthesizedContents"""

    TYPE_ = hydra.core.Name("openGql.grammar.SimplifiedPrimary")
    LABEL_NAME = hydra.core.Name("labelName")
    PARENTHESIZED_CONTENTS = hydra.core.Name("parenthesizedContents")

WhereClause: TypeAlias = "SearchCondition"

YieldClause: TypeAlias = "YieldItemList"

YieldItemList: TypeAlias = "frozenlist[YieldItem]"

@dataclass(frozen=True)
class YieldItem:
    name: YieldItemName
    alias: Maybe[YieldItemAlias]

    TYPE_ = hydra.core.Name("openGql.grammar.YieldItem")
    NAME = hydra.core.Name("name")
    ALIAS = hydra.core.Name("alias")

YieldItemName: TypeAlias = "FieldName"

YieldItemAlias: TypeAlias = "BindingVariable"

GroupByClause: TypeAlias = "GroupingElementList"

class GroupingElementListElements(Node["frozenlist[GroupingElement]"]):
    ...

class GroupingElementListEmptySet:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GroupingElementListEmptySet)
    def __hash__(self):
        return hash("GroupingElementListEmptySet")

class _GroupingElementListMeta(type):
    def __getitem__(cls, item):
        return object

class GroupingElementList(metaclass=_GroupingElementListMeta):
    r"""GroupingElementListElements | GroupingElementListEmptySet"""

    TYPE_ = hydra.core.Name("openGql.grammar.GroupingElementList")
    ELEMENTS = hydra.core.Name("elements")
    EMPTY_SET = hydra.core.Name("emptySet")

GroupingElement: TypeAlias = "BindingVariableReference"

OrderByClause: TypeAlias = "SortSpecificationList"

SortSpecificationList: TypeAlias = "frozenlist[SortSpecification]"

@dataclass(frozen=True)
class SortSpecification:
    sort_key: SortKey
    ordering: Maybe[OrderingSpecification]
    null_ordering: Maybe[NullOrdering]

    TYPE_ = hydra.core.Name("openGql.grammar.SortSpecification")
    SORT_KEY = hydra.core.Name("sortKey")
    ORDERING = hydra.core.Name("ordering")
    NULL_ORDERING = hydra.core.Name("nullOrdering")

SortKey: TypeAlias = "AggregatingValueExpression"

class OrderingSpecification(Enum):
    ASCENDING = hydra.core.Name("ascending")

    DESCENDING = hydra.core.Name("descending")

OrderingSpecification.TYPE_ = hydra.core.Name("openGql.grammar.OrderingSpecification")

class NullOrdering(Enum):
    NULLS_FIRST = hydra.core.Name("nullsFirst")

    NULLS_LAST = hydra.core.Name("nullsLast")

NullOrdering.TYPE_ = hydra.core.Name("openGql.grammar.NullOrdering")

LimitClause: TypeAlias = "NonNegativeIntegerSpecification"

@dataclass(frozen=True)
class OffsetClause:
    synonym: OffsetSynonym
    value: NonNegativeIntegerSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.OffsetClause")
    SYNONYM = hydra.core.Name("synonym")
    VALUE = hydra.core.Name("value")

class OffsetSynonym(Enum):
    OFFSET = hydra.core.Name("offset")

    SKIP_RESERVED_WORD = hydra.core.Name("skipReservedWord")

OffsetSynonym.TYPE_ = hydra.core.Name("openGql.grammar.OffsetSynonym")

class SchemaReferenceAbsoluteReference(Node["AbsoluteCatalogSchemaReference"]):
    ...

class SchemaReferenceRelativeReference(Node["RelativeCatalogSchemaReference"]):
    ...

class SchemaReferenceParameterSpecification(Node["ReferenceParameterSpecification"]):
    ...

class _SchemaReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class SchemaReference(metaclass=_SchemaReferenceMeta):
    r"""SchemaReferenceAbsoluteReference | SchemaReferenceRelativeReference | SchemaReferenceParameterSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.SchemaReference")
    ABSOLUTE_REFERENCE = hydra.core.Name("absoluteReference")
    RELATIVE_REFERENCE = hydra.core.Name("relativeReference")
    PARAMETER_SPECIFICATION = hydra.core.Name("parameterSpecification")

class AbsoluteCatalogSchemaReferenceRoot:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AbsoluteCatalogSchemaReferenceRoot)
    def __hash__(self):
        return hash("AbsoluteCatalogSchemaReferenceRoot")

class AbsoluteCatalogSchemaReferenceDirectoryAndSchema(Node["AbsoluteDirectoryAndSchema"]):
    ...

class _AbsoluteCatalogSchemaReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class AbsoluteCatalogSchemaReference(metaclass=_AbsoluteCatalogSchemaReferenceMeta):
    r"""AbsoluteCatalogSchemaReferenceRoot | AbsoluteCatalogSchemaReferenceDirectoryAndSchema"""

    TYPE_ = hydra.core.Name("openGql.grammar.AbsoluteCatalogSchemaReference")
    ROOT = hydra.core.Name("root")
    DIRECTORY_AND_SCHEMA = hydra.core.Name("directoryAndSchema")

@dataclass(frozen=True)
class AbsoluteDirectoryAndSchema:
    directory_path: AbsoluteDirectoryPath
    schema_name: SchemaName

    TYPE_ = hydra.core.Name("openGql.grammar.AbsoluteDirectoryAndSchema")
    DIRECTORY_PATH = hydra.core.Name("directoryPath")
    SCHEMA_NAME = hydra.core.Name("schemaName")

CatalogSchemaParentAndName: TypeAlias = "AbsoluteDirectoryAndSchema"

class RelativeCatalogSchemaReferencePredefinedReference(Node["PredefinedSchemaReference"]):
    ...

class RelativeCatalogSchemaReferenceDirectoryAndSchema(Node["RelativeDirectoryAndSchema"]):
    ...

class _RelativeCatalogSchemaReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class RelativeCatalogSchemaReference(metaclass=_RelativeCatalogSchemaReferenceMeta):
    r"""RelativeCatalogSchemaReferencePredefinedReference | RelativeCatalogSchemaReferenceDirectoryAndSchema"""

    TYPE_ = hydra.core.Name("openGql.grammar.RelativeCatalogSchemaReference")
    PREDEFINED_REFERENCE = hydra.core.Name("predefinedReference")
    DIRECTORY_AND_SCHEMA = hydra.core.Name("directoryAndSchema")

@dataclass(frozen=True)
class RelativeDirectoryAndSchema:
    directory_path: RelativeDirectoryPath
    schema_name: SchemaName

    TYPE_ = hydra.core.Name("openGql.grammar.RelativeDirectoryAndSchema")
    DIRECTORY_PATH = hydra.core.Name("directoryPath")
    SCHEMA_NAME = hydra.core.Name("schemaName")

class PredefinedSchemaReference(Enum):
    HOME_SCHEMA = hydra.core.Name("homeSchema")

    CURRENT_SCHEMA = hydra.core.Name("currentSchema")

    PERIOD = hydra.core.Name("period")

PredefinedSchemaReference.TYPE_ = hydra.core.Name("openGql.grammar.PredefinedSchemaReference")

AbsoluteDirectoryPath: TypeAlias = "Maybe[SimpleDirectoryPath]"

@dataclass(frozen=True)
class RelativeDirectoryPath:
    parent_directories: int
    simple_path: Maybe[SimpleDirectoryPath]

    TYPE_ = hydra.core.Name("openGql.grammar.RelativeDirectoryPath")
    PARENT_DIRECTORIES = hydra.core.Name("parentDirectories")
    SIMPLE_PATH = hydra.core.Name("simplePath")

SimpleDirectoryPath: TypeAlias = "frozenlist[DirectoryName]"

class GraphReferenceParentAndGraphName(Node["ParentAndGraphName"]):
    ...

class GraphReferenceDelimitedGraphName(Node["DelimitedGraphName"]):
    ...

class GraphReferenceHomeGraph(Node["HomeGraph"]):
    ...

class GraphReferenceParameterSpecification(Node["ReferenceParameterSpecification"]):
    ...

class _GraphReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class GraphReference(metaclass=_GraphReferenceMeta):
    r"""GraphReferenceParentAndGraphName | GraphReferenceDelimitedGraphName | GraphReferenceHomeGraph | GraphReferenceParameterSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphReference")
    PARENT_AND_GRAPH_NAME = hydra.core.Name("parentAndGraphName")
    DELIMITED_GRAPH_NAME = hydra.core.Name("delimitedGraphName")
    HOME_GRAPH = hydra.core.Name("homeGraph")
    PARAMETER_SPECIFICATION = hydra.core.Name("parameterSpecification")

@dataclass(frozen=True)
class ParentAndGraphName:
    parent_reference: CatalogObjectParentReference
    graph_name: GraphName

    TYPE_ = hydra.core.Name("openGql.grammar.ParentAndGraphName")
    PARENT_REFERENCE = hydra.core.Name("parentReference")
    GRAPH_NAME = hydra.core.Name("graphName")

@dataclass(frozen=True)
class CatalogGraphParentAndName:
    parent_reference: Maybe[CatalogObjectParentReference]
    graph_name: GraphName

    TYPE_ = hydra.core.Name("openGql.grammar.CatalogGraphParentAndName")
    PARENT_REFERENCE = hydra.core.Name("parentReference")
    GRAPH_NAME = hydra.core.Name("graphName")

class HomeGraph(Enum):
    HOME_PROPERTY_GRAPH = hydra.core.Name("homePropertyGraph")

    HOME_GRAPH = hydra.core.Name("homeGraph")

HomeGraph.TYPE_ = hydra.core.Name("openGql.grammar.HomeGraph")

class GraphTypeReferenceParentAndTypeName(Node["CatalogGraphTypeParentAndName"]):
    ...

class GraphTypeReferenceParameterSpecification(Node["ReferenceParameterSpecification"]):
    ...

class _GraphTypeReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class GraphTypeReference(metaclass=_GraphTypeReferenceMeta):
    r"""GraphTypeReferenceParentAndTypeName | GraphTypeReferenceParameterSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphTypeReference")
    PARENT_AND_TYPE_NAME = hydra.core.Name("parentAndTypeName")
    PARAMETER_SPECIFICATION = hydra.core.Name("parameterSpecification")

@dataclass(frozen=True)
class CatalogGraphTypeParentAndName:
    parent_reference: Maybe[CatalogObjectParentReference]
    graph_type_name: GraphTypeName

    TYPE_ = hydra.core.Name("openGql.grammar.CatalogGraphTypeParentAndName")
    PARENT_REFERENCE = hydra.core.Name("parentReference")
    GRAPH_TYPE_NAME = hydra.core.Name("graphTypeName")

class BindingTableReferenceParentAndTableName(Node["ParentAndTableName"]):
    ...

class BindingTableReferenceDelimitedBindingTableName(Node["DelimitedBindingTableName"]):
    ...

class BindingTableReferenceParameterSpecification(Node["ReferenceParameterSpecification"]):
    ...

class _BindingTableReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class BindingTableReference(metaclass=_BindingTableReferenceMeta):
    r"""BindingTableReferenceParentAndTableName | BindingTableReferenceDelimitedBindingTableName | BindingTableReferenceParameterSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.BindingTableReference")
    PARENT_AND_TABLE_NAME = hydra.core.Name("parentAndTableName")
    DELIMITED_BINDING_TABLE_NAME = hydra.core.Name("delimitedBindingTableName")
    PARAMETER_SPECIFICATION = hydra.core.Name("parameterSpecification")

@dataclass(frozen=True)
class ParentAndTableName:
    parent_reference: CatalogObjectParentReference
    table_name: BindingTableName

    TYPE_ = hydra.core.Name("openGql.grammar.ParentAndTableName")
    PARENT_REFERENCE = hydra.core.Name("parentReference")
    TABLE_NAME = hydra.core.Name("tableName")

class ProcedureReferenceParentAndProcedureName(Node["CatalogProcedureParentAndName"]):
    ...

class ProcedureReferenceParameterSpecification(Node["ReferenceParameterSpecification"]):
    ...

class _ProcedureReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class ProcedureReference(metaclass=_ProcedureReferenceMeta):
    r"""ProcedureReferenceParentAndProcedureName | ProcedureReferenceParameterSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.ProcedureReference")
    PARENT_AND_PROCEDURE_NAME = hydra.core.Name("parentAndProcedureName")
    PARAMETER_SPECIFICATION = hydra.core.Name("parameterSpecification")

@dataclass(frozen=True)
class CatalogProcedureParentAndName:
    parent_reference: Maybe[CatalogObjectParentReference]
    procedure_name: ProcedureName

    TYPE_ = hydra.core.Name("openGql.grammar.CatalogProcedureParentAndName")
    PARENT_REFERENCE = hydra.core.Name("parentReference")
    PROCEDURE_NAME = hydra.core.Name("procedureName")

class CatalogObjectParentReferenceSchemaAndObjects(Node["SchemaAndObjects"]):
    ...

class CatalogObjectParentReferenceObjectsOnly(Node["frozenlist[ObjectName]"]):
    ...

class _CatalogObjectParentReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class CatalogObjectParentReference(metaclass=_CatalogObjectParentReferenceMeta):
    r"""CatalogObjectParentReferenceSchemaAndObjects | CatalogObjectParentReferenceObjectsOnly"""

    TYPE_ = hydra.core.Name("openGql.grammar.CatalogObjectParentReference")
    SCHEMA_AND_OBJECTS = hydra.core.Name("schemaAndObjects")
    OBJECTS_ONLY = hydra.core.Name("objectsOnly")

@dataclass(frozen=True)
class SchemaAndObjects:
    schema_reference: SchemaReference
    objects: frozenlist[ObjectName]

    TYPE_ = hydra.core.Name("openGql.grammar.SchemaAndObjects")
    SCHEMA_REFERENCE = hydra.core.Name("schemaReference")
    OBJECTS = hydra.core.Name("objects")

ReferenceParameterSpecification: TypeAlias = "None"

NestedGraphTypeSpecification: TypeAlias = "GraphTypeSpecificationBody"

GraphTypeSpecificationBody: TypeAlias = "ElementTypeList"

ElementTypeList: TypeAlias = "frozenlist[ElementTypeSpecification]"

class ElementTypeSpecificationNodeType(Node["NodeTypeSpecification"]):
    ...

class ElementTypeSpecificationEdgeType(Node["EdgeTypeSpecification"]):
    ...

class _ElementTypeSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class ElementTypeSpecification(metaclass=_ElementTypeSpecificationMeta):
    r"""ElementTypeSpecificationNodeType | ElementTypeSpecificationEdgeType"""

    TYPE_ = hydra.core.Name("openGql.grammar.ElementTypeSpecification")
    NODE_TYPE = hydra.core.Name("nodeType")
    EDGE_TYPE = hydra.core.Name("edgeType")

class NodeTypeSpecificationPattern(Node["NodeTypePattern"]):
    ...

class NodeTypeSpecificationPhrase(Node["NodeTypePhrase"]):
    ...

class _NodeTypeSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class NodeTypeSpecification(metaclass=_NodeTypeSpecificationMeta):
    r"""NodeTypeSpecificationPattern | NodeTypeSpecificationPhrase"""

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypeSpecification")
    PATTERN = hydra.core.Name("pattern")
    PHRASE = hydra.core.Name("phrase")

@dataclass(frozen=True)
class NodeTypePattern:
    synonym_and_type_name: Maybe[NodeSynonymAndTypeName]
    alias: Maybe[LocalNodeTypeAlias]
    filler: Maybe[NodeTypeFiller]

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypePattern")
    SYNONYM_AND_TYPE_NAME = hydra.core.Name("synonymAndTypeName")
    ALIAS = hydra.core.Name("alias")
    FILLER = hydra.core.Name("filler")

@dataclass(frozen=True)
class NodeSynonymAndTypeName:
    node_synonym: NodeSynonym
    type_name: Maybe[NodeTypeName]

    TYPE_ = hydra.core.Name("openGql.grammar.NodeSynonymAndTypeName")
    NODE_SYNONYM = hydra.core.Name("nodeSynonym")
    TYPE_NAME = hydra.core.Name("typeName")

@dataclass(frozen=True)
class NodeTypePhrase:
    synonym: NodeSynonym
    type_phrase_filler: NodeTypePhraseFiller
    alias: Maybe[LocalNodeTypeAlias]

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypePhrase")
    SYNONYM = hydra.core.Name("synonym")
    TYPE_PHRASE_FILLER = hydra.core.Name("typePhraseFiller")
    ALIAS = hydra.core.Name("alias")

class NodeTypePhraseFillerTypeName(Node["NodeTypeNameWithFiller"]):
    ...

class NodeTypePhraseFillerFillerOnly(Node["NodeTypeFiller"]):
    ...

class _NodeTypePhraseFillerMeta(type):
    def __getitem__(cls, item):
        return object

class NodeTypePhraseFiller(metaclass=_NodeTypePhraseFillerMeta):
    r"""NodeTypePhraseFillerTypeName | NodeTypePhraseFillerFillerOnly"""

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypePhraseFiller")
    TYPE_NAME = hydra.core.Name("typeName")
    FILLER_ONLY = hydra.core.Name("fillerOnly")

@dataclass(frozen=True)
class NodeTypeNameWithFiller:
    type_name: NodeTypeName
    filler: Maybe[NodeTypeFiller]

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypeNameWithFiller")
    TYPE_NAME = hydra.core.Name("typeName")
    FILLER = hydra.core.Name("filler")

class NodeTypeFillerKeyLabelSet(Node["NodeKeyLabelSetWithContent"]):
    ...

class NodeTypeFillerImpliedContent(Node["NodeTypeImpliedContent"]):
    ...

class _NodeTypeFillerMeta(type):
    def __getitem__(cls, item):
        return object

class NodeTypeFiller(metaclass=_NodeTypeFillerMeta):
    r"""NodeTypeFillerKeyLabelSet | NodeTypeFillerImpliedContent"""

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypeFiller")
    KEY_LABEL_SET = hydra.core.Name("keyLabelSet")
    IMPLIED_CONTENT = hydra.core.Name("impliedContent")

@dataclass(frozen=True)
class NodeKeyLabelSetWithContent:
    key_label_set: NodeTypeKeyLabelSet
    implied_content: Maybe[NodeTypeImpliedContent]

    TYPE_ = hydra.core.Name("openGql.grammar.NodeKeyLabelSetWithContent")
    KEY_LABEL_SET = hydra.core.Name("keyLabelSet")
    IMPLIED_CONTENT = hydra.core.Name("impliedContent")

LocalNodeTypeAlias: TypeAlias = "str"

class NodeTypeImpliedContentLabelSet(Node["NodeTypeLabelSet"]):
    ...

class NodeTypeImpliedContentPropertyTypes(Node["NodeTypePropertyTypes"]):
    ...

class NodeTypeImpliedContentLabelSetWithProperties(Node["NodeLabelSetWithProperties"]):
    ...

class _NodeTypeImpliedContentMeta(type):
    def __getitem__(cls, item):
        return object

class NodeTypeImpliedContent(metaclass=_NodeTypeImpliedContentMeta):
    r"""NodeTypeImpliedContentLabelSet | NodeTypeImpliedContentPropertyTypes | NodeTypeImpliedContentLabelSetWithProperties"""

    TYPE_ = hydra.core.Name("openGql.grammar.NodeTypeImpliedContent")
    LABEL_SET = hydra.core.Name("labelSet")
    PROPERTY_TYPES = hydra.core.Name("propertyTypes")
    LABEL_SET_WITH_PROPERTIES = hydra.core.Name("labelSetWithProperties")

@dataclass(frozen=True)
class NodeLabelSetWithProperties:
    label_set: NodeTypeLabelSet
    property_types: NodeTypePropertyTypes

    TYPE_ = hydra.core.Name("openGql.grammar.NodeLabelSetWithProperties")
    LABEL_SET = hydra.core.Name("labelSet")
    PROPERTY_TYPES = hydra.core.Name("propertyTypes")

NodeTypeKeyLabelSet: TypeAlias = "Maybe[LabelSetPhrase]"

NodeTypeLabelSet: TypeAlias = "LabelSetPhrase"

NodeTypePropertyTypes: TypeAlias = "PropertyTypesSpecification"

class EdgeTypeSpecificationPattern(Node["EdgeTypePattern"]):
    ...

class EdgeTypeSpecificationPhrase(Node["EdgeTypePhrase"]):
    ...

class _EdgeTypeSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeTypeSpecification(metaclass=_EdgeTypeSpecificationMeta):
    r"""EdgeTypeSpecificationPattern | EdgeTypeSpecificationPhrase"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypeSpecification")
    PATTERN = hydra.core.Name("pattern")
    PHRASE = hydra.core.Name("phrase")

@dataclass(frozen=True)
class EdgeTypePattern:
    kind_and_synonym: Maybe[EdgeKindAndSynonym]
    pattern_type: EdgeTypePatternType

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePattern")
    KIND_AND_SYNONYM = hydra.core.Name("kindAndSynonym")
    PATTERN_TYPE = hydra.core.Name("patternType")

@dataclass(frozen=True)
class EdgeKindAndSynonym:
    kind: Maybe[EdgeKind]
    synonym: EdgeSynonym
    type_name: Maybe[EdgeTypeName]

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeKindAndSynonym")
    KIND = hydra.core.Name("kind")
    SYNONYM = hydra.core.Name("synonym")
    TYPE_NAME = hydra.core.Name("typeName")

class EdgeTypePatternTypeDirected(Node["EdgeTypePatternDirected"]):
    ...

class EdgeTypePatternTypeUndirected(Node["EdgeTypePatternUndirected"]):
    ...

class _EdgeTypePatternTypeMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeTypePatternType(metaclass=_EdgeTypePatternTypeMeta):
    r"""EdgeTypePatternTypeDirected | EdgeTypePatternTypeUndirected"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePatternType")
    DIRECTED = hydra.core.Name("directed")
    UNDIRECTED = hydra.core.Name("undirected")

@dataclass(frozen=True)
class EdgeTypePhrase:
    kind: EdgeKind
    synonym: EdgeSynonym
    type_name_and_filler: EdgeTypePhraseFiller
    endpoint_pair: EndpointPairPhrase

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePhrase")
    KIND = hydra.core.Name("kind")
    SYNONYM = hydra.core.Name("synonym")
    TYPE_NAME_AND_FILLER = hydra.core.Name("typeNameAndFiller")
    ENDPOINT_PAIR = hydra.core.Name("endpointPair")

class EdgeTypePhraseFillerTypeNameWithFiller(Node["EdgeTypeNameWithFiller"]):
    ...

class EdgeTypePhraseFillerFillerOnly(Node["EdgeTypeFiller"]):
    ...

class _EdgeTypePhraseFillerMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeTypePhraseFiller(metaclass=_EdgeTypePhraseFillerMeta):
    r"""EdgeTypePhraseFillerTypeNameWithFiller | EdgeTypePhraseFillerFillerOnly"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePhraseFiller")
    TYPE_NAME_WITH_FILLER = hydra.core.Name("typeNameWithFiller")
    FILLER_ONLY = hydra.core.Name("fillerOnly")

@dataclass(frozen=True)
class EdgeTypeNameWithFiller:
    type_name: EdgeTypeName
    filler: Maybe[EdgeTypeFiller]

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypeNameWithFiller")
    TYPE_NAME = hydra.core.Name("typeName")
    FILLER = hydra.core.Name("filler")

class EdgeTypeFillerKeyLabelSetWithContent(Node["EdgeKeyLabelSetWithContent"]):
    ...

class EdgeTypeFillerImpliedContent(Node["EdgeTypeImpliedContent"]):
    ...

class _EdgeTypeFillerMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeTypeFiller(metaclass=_EdgeTypeFillerMeta):
    r"""EdgeTypeFillerKeyLabelSetWithContent | EdgeTypeFillerImpliedContent"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypeFiller")
    KEY_LABEL_SET_WITH_CONTENT = hydra.core.Name("keyLabelSetWithContent")
    IMPLIED_CONTENT = hydra.core.Name("impliedContent")

@dataclass(frozen=True)
class EdgeKeyLabelSetWithContent:
    key_label_set: EdgeTypeKeyLabelSet
    implied_content: Maybe[EdgeTypeImpliedContent]

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeKeyLabelSetWithContent")
    KEY_LABEL_SET = hydra.core.Name("keyLabelSet")
    IMPLIED_CONTENT = hydra.core.Name("impliedContent")

class EdgeTypeImpliedContentLabelSet(Node["EdgeTypeLabelSet"]):
    ...

class EdgeTypeImpliedContentPropertyTypes(Node["EdgeTypePropertyTypes"]):
    ...

class EdgeTypeImpliedContentLabelSetWithProperties(Node["EdgeLabelSetWithProperties"]):
    ...

class _EdgeTypeImpliedContentMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeTypeImpliedContent(metaclass=_EdgeTypeImpliedContentMeta):
    r"""EdgeTypeImpliedContentLabelSet | EdgeTypeImpliedContentPropertyTypes | EdgeTypeImpliedContentLabelSetWithProperties"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypeImpliedContent")
    LABEL_SET = hydra.core.Name("labelSet")
    PROPERTY_TYPES = hydra.core.Name("propertyTypes")
    LABEL_SET_WITH_PROPERTIES = hydra.core.Name("labelSetWithProperties")

@dataclass(frozen=True)
class EdgeLabelSetWithProperties:
    label_set: EdgeTypeLabelSet
    property_types: EdgeTypePropertyTypes

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeLabelSetWithProperties")
    LABEL_SET = hydra.core.Name("labelSet")
    PROPERTY_TYPES = hydra.core.Name("propertyTypes")

EdgeTypeKeyLabelSet: TypeAlias = "Maybe[LabelSetPhrase]"

EdgeTypeLabelSet: TypeAlias = "LabelSetPhrase"

EdgeTypePropertyTypes: TypeAlias = "PropertyTypesSpecification"

class EdgeTypePatternDirectedPointingRight(Node["EdgeTypePatternPointingRight"]):
    ...

class EdgeTypePatternDirectedPointingLeft(Node["EdgeTypePatternPointingLeft"]):
    ...

class _EdgeTypePatternDirectedMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeTypePatternDirected(metaclass=_EdgeTypePatternDirectedMeta):
    r"""EdgeTypePatternDirectedPointingRight | EdgeTypePatternDirectedPointingLeft"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePatternDirected")
    POINTING_RIGHT = hydra.core.Name("pointingRight")
    POINTING_LEFT = hydra.core.Name("pointingLeft")

@dataclass(frozen=True)
class EdgeTypePatternPointingRight:
    source: SourceNodeTypeReference
    arc: ArcTypePointingRight
    destination: DestinationNodeTypeReference

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePatternPointingRight")
    SOURCE = hydra.core.Name("source")
    ARC = hydra.core.Name("arc")
    DESTINATION = hydra.core.Name("destination")

@dataclass(frozen=True)
class EdgeTypePatternPointingLeft:
    destination: DestinationNodeTypeReference
    arc: ArcTypePointingLeft
    source: SourceNodeTypeReference

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePatternPointingLeft")
    DESTINATION = hydra.core.Name("destination")
    ARC = hydra.core.Name("arc")
    SOURCE = hydra.core.Name("source")

@dataclass(frozen=True)
class EdgeTypePatternUndirected:
    source: SourceNodeTypeReference
    arc: ArcTypeUndirected
    destination: DestinationNodeTypeReference

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeTypePatternUndirected")
    SOURCE = hydra.core.Name("source")
    ARC = hydra.core.Name("arc")
    DESTINATION = hydra.core.Name("destination")

ArcTypePointingRight: TypeAlias = "EdgeTypeFiller"

ArcTypePointingLeft: TypeAlias = "EdgeTypeFiller"

ArcTypeUndirected: TypeAlias = "EdgeTypeFiller"

class SourceNodeTypeReferenceAlias(Node["SourceNodeTypeAlias"]):
    ...

class SourceNodeTypeReferenceFiller(Node["Maybe[NodeTypeFiller]"]):
    ...

class _SourceNodeTypeReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class SourceNodeTypeReference(metaclass=_SourceNodeTypeReferenceMeta):
    r"""SourceNodeTypeReferenceAlias | SourceNodeTypeReferenceFiller"""

    TYPE_ = hydra.core.Name("openGql.grammar.SourceNodeTypeReference")
    ALIAS = hydra.core.Name("alias")
    FILLER = hydra.core.Name("filler")

class DestinationNodeTypeReferenceAlias(Node["DestinationNodeTypeAlias"]):
    ...

class DestinationNodeTypeReferenceFiller(Node["Maybe[NodeTypeFiller]"]):
    ...

class _DestinationNodeTypeReferenceMeta(type):
    def __getitem__(cls, item):
        return object

class DestinationNodeTypeReference(metaclass=_DestinationNodeTypeReferenceMeta):
    r"""DestinationNodeTypeReferenceAlias | DestinationNodeTypeReferenceFiller"""

    TYPE_ = hydra.core.Name("openGql.grammar.DestinationNodeTypeReference")
    ALIAS = hydra.core.Name("alias")
    FILLER = hydra.core.Name("filler")

class EdgeKind(Enum):
    DIRECTED = hydra.core.Name("directed")

    UNDIRECTED = hydra.core.Name("undirected")

EdgeKind.TYPE_ = hydra.core.Name("openGql.grammar.EdgeKind")

EndpointPairPhrase: TypeAlias = "EndpointPair"

class EndpointPairDirectedPair(Node["EndpointPairDirected"]):
    ...

class EndpointPairUndirectedPair(Node["EndpointPairUndirected"]):
    ...

class _EndpointPairMeta(type):
    def __getitem__(cls, item):
        return object

class EndpointPair(metaclass=_EndpointPairMeta):
    r"""EndpointPairDirectedPair | EndpointPairUndirectedPair"""

    TYPE_ = hydra.core.Name("openGql.grammar.EndpointPair")
    DIRECTED_PAIR = hydra.core.Name("directedPair")
    UNDIRECTED_PAIR = hydra.core.Name("undirectedPair")

class EndpointPairDirectedPointingRight(Node["EndpointPairPointingRight"]):
    ...

class EndpointPairDirectedPointingLeft(Node["EndpointPairPointingLeft"]):
    ...

class _EndpointPairDirectedMeta(type):
    def __getitem__(cls, item):
        return object

class EndpointPairDirected(metaclass=_EndpointPairDirectedMeta):
    r"""EndpointPairDirectedPointingRight | EndpointPairDirectedPointingLeft"""

    TYPE_ = hydra.core.Name("openGql.grammar.EndpointPairDirected")
    POINTING_RIGHT = hydra.core.Name("pointingRight")
    POINTING_LEFT = hydra.core.Name("pointingLeft")

@dataclass(frozen=True)
class EndpointPairPointingRight:
    source_alias: SourceNodeTypeAlias
    connector: ConnectorPointingRight
    destination_alias: DestinationNodeTypeAlias

    TYPE_ = hydra.core.Name("openGql.grammar.EndpointPairPointingRight")
    SOURCE_ALIAS = hydra.core.Name("sourceAlias")
    CONNECTOR = hydra.core.Name("connector")
    DESTINATION_ALIAS = hydra.core.Name("destinationAlias")

@dataclass(frozen=True)
class EndpointPairPointingLeft:
    destination_alias: DestinationNodeTypeAlias
    source_alias: SourceNodeTypeAlias

    TYPE_ = hydra.core.Name("openGql.grammar.EndpointPairPointingLeft")
    DESTINATION_ALIAS = hydra.core.Name("destinationAlias")
    SOURCE_ALIAS = hydra.core.Name("sourceAlias")

@dataclass(frozen=True)
class EndpointPairUndirected:
    source_alias: SourceNodeTypeAlias
    connector: ConnectorUndirected
    destination_alias: DestinationNodeTypeAlias

    TYPE_ = hydra.core.Name("openGql.grammar.EndpointPairUndirected")
    SOURCE_ALIAS = hydra.core.Name("sourceAlias")
    CONNECTOR = hydra.core.Name("connector")
    DESTINATION_ALIAS = hydra.core.Name("destinationAlias")

class ConnectorPointingRight(Enum):
    TO = hydra.core.Name("to")

    RIGHT_ARROW = hydra.core.Name("rightArrow")

ConnectorPointingRight.TYPE_ = hydra.core.Name("openGql.grammar.ConnectorPointingRight")

class ConnectorUndirected(Enum):
    TO = hydra.core.Name("to")

    TILDE = hydra.core.Name("tilde")

ConnectorUndirected.TYPE_ = hydra.core.Name("openGql.grammar.ConnectorUndirected")

SourceNodeTypeAlias: TypeAlias = "str"

DestinationNodeTypeAlias: TypeAlias = "str"

class LabelSetPhraseSingleLabel(Node["LabelName"]):
    ...

class LabelSetPhraseMultipleLabels(Node["LabelSetSpecification"]):
    ...

class LabelSetPhraseIsOrColonWithLabels(Node["IsOrColonWithLabels"]):
    ...

class _LabelSetPhraseMeta(type):
    def __getitem__(cls, item):
        return object

class LabelSetPhrase(metaclass=_LabelSetPhraseMeta):
    r"""LabelSetPhraseSingleLabel | LabelSetPhraseMultipleLabels | LabelSetPhraseIsOrColonWithLabels"""

    TYPE_ = hydra.core.Name("openGql.grammar.LabelSetPhrase")
    SINGLE_LABEL = hydra.core.Name("singleLabel")
    MULTIPLE_LABELS = hydra.core.Name("multipleLabels")
    IS_OR_COLON_WITH_LABELS = hydra.core.Name("isOrColonWithLabels")

@dataclass(frozen=True)
class IsOrColonWithLabels:
    is_or_colon: IsOrColon
    labels: LabelSetSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.IsOrColonWithLabels")
    IS_OR_COLON = hydra.core.Name("isOrColon")
    LABELS = hydra.core.Name("labels")

LabelSetSpecification: TypeAlias = "frozenlist[LabelName]"

PropertyTypesSpecification: TypeAlias = "Maybe[PropertyTypeList]"

PropertyTypeList: TypeAlias = "frozenlist[PropertyType]"

@dataclass(frozen=True)
class PropertyType:
    name: PropertyName
    typed: Maybe[Typed]
    value_type: PropertyValueType

    TYPE_ = hydra.core.Name("openGql.grammar.PropertyType")
    NAME = hydra.core.Name("name")
    TYPED = hydra.core.Name("typed")
    VALUE_TYPE = hydra.core.Name("valueType")

PropertyValueType: TypeAlias = "ValueType"

@dataclass(frozen=True)
class BindingTableType:
    binding: bool
    field_types: FieldTypesSpecification

    TYPE_ = hydra.core.Name("openGql.grammar.BindingTableType")
    BINDING = hydra.core.Name("binding")
    FIELD_TYPES = hydra.core.Name("fieldTypes")

class ValueTypePredefinedType(Node["PredefinedType"]):
    ...

class ValueTypePathValueType(Node["PathValueType"]):
    ...

class ValueTypeListValueTypeAlt1(Node["ListValueTypeAlt1"]):
    ...

class ValueTypeListValueTypeAlt2(Node["ListValueTypeAlt2"]):
    ...

class ValueTypeListValueTypeAlt3(Node["ListValueTypeAlt3"]):
    ...

class ValueTypeRecordType(Node["RecordType"]):
    ...

class ValueTypeOpenDynamicUnionType(Node["OpenDynamicUnionType"]):
    ...

class ValueTypeDynamicPropertyValueType(Node["DynamicPropertyValueType"]):
    ...

class ValueTypeClosedDynamicUnionTypeAlt1(Node["ClosedDynamicUnionTypeAlt1"]):
    ...

class ValueTypeClosedDynamicUnionTypeAlt2(Node["ClosedDynamicUnionTypeAlt2"]):
    ...

class _ValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ValueType(metaclass=_ValueTypeMeta):
    r"""ValueTypePredefinedType | ValueTypePathValueType | ValueTypeListValueTypeAlt1 | ValueTypeListValueTypeAlt2 | ValueTypeListValueTypeAlt3 | ValueTypeRecordType | ValueTypeOpenDynamicUnionType | ValueTypeDynamicPropertyValueType | ValueTypeClosedDynamicUnionTypeAlt1 | ValueTypeClosedDynamicUnionTypeAlt2"""

    TYPE_ = hydra.core.Name("openGql.grammar.ValueType")
    PREDEFINED_TYPE = hydra.core.Name("predefinedType")
    PATH_VALUE_TYPE = hydra.core.Name("pathValueType")
    LIST_VALUE_TYPE_ALT1 = hydra.core.Name("listValueTypeAlt1")
    LIST_VALUE_TYPE_ALT2 = hydra.core.Name("listValueTypeAlt2")
    LIST_VALUE_TYPE_ALT3 = hydra.core.Name("listValueTypeAlt3")
    RECORD_TYPE = hydra.core.Name("recordType")
    OPEN_DYNAMIC_UNION_TYPE = hydra.core.Name("openDynamicUnionType")
    DYNAMIC_PROPERTY_VALUE_TYPE = hydra.core.Name("dynamicPropertyValueType")
    CLOSED_DYNAMIC_UNION_TYPE_ALT1 = hydra.core.Name("closedDynamicUnionTypeAlt1")
    CLOSED_DYNAMIC_UNION_TYPE_ALT2 = hydra.core.Name("closedDynamicUnionTypeAlt2")

@dataclass(frozen=True)
class ListValueTypeAlt1:
    type_name: ListValueTypeName
    value_type: ValueType
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ListValueTypeAlt1")
    TYPE_NAME = hydra.core.Name("typeName")
    VALUE_TYPE = hydra.core.Name("valueType")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class ListValueTypeAlt2:
    value_type: ValueType
    type_name: ListValueTypeName
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ListValueTypeAlt2")
    VALUE_TYPE = hydra.core.Name("valueType")
    TYPE_NAME = hydra.core.Name("typeName")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class ListValueTypeAlt3:
    type_name: ListValueTypeName
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ListValueTypeAlt3")
    TYPE_NAME = hydra.core.Name("typeName")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class OpenDynamicUnionType:
    value: bool
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.OpenDynamicUnionType")
    VALUE = hydra.core.Name("value")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class DynamicPropertyValueType:
    any: Maybe[bool]
    property: None
    value: None
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.DynamicPropertyValueType")
    ANY = hydra.core.Name("any")
    PROPERTY = hydra.core.Name("property")
    VALUE = hydra.core.Name("value")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class ClosedDynamicUnionTypeAlt1:
    any_value: Maybe[bool]
    value_types: frozenlist[ValueType]

    TYPE_ = hydra.core.Name("openGql.grammar.ClosedDynamicUnionTypeAlt1")
    ANY_VALUE = hydra.core.Name("anyValue")
    VALUE_TYPES = hydra.core.Name("valueTypes")

@dataclass(frozen=True)
class ClosedDynamicUnionTypeAlt2:
    value_types: frozenlist[ValueType]

    TYPE_ = hydra.core.Name("openGql.grammar.ClosedDynamicUnionTypeAlt2")
    VALUE_TYPES = hydra.core.Name("valueTypes")

Typed: TypeAlias = "None"

class PredefinedTypeBooleanType(Node["BooleanType"]):
    ...

class PredefinedTypeCharacterStringType(Node["CharacterStringType"]):
    ...

class PredefinedTypeByteStringType(Node["ByteStringType"]):
    ...

class PredefinedTypeNumericType(Node["NumericType"]):
    ...

class PredefinedTypeTemporalType(Node["TemporalType"]):
    ...

class PredefinedTypeReferenceValueType(Node["ReferenceValueType"]):
    ...

class PredefinedTypeImmaterialValueType(Node["ImmaterialValueType"]):
    ...

class _PredefinedTypeMeta(type):
    def __getitem__(cls, item):
        return object

class PredefinedType(metaclass=_PredefinedTypeMeta):
    r"""PredefinedTypeBooleanType | PredefinedTypeCharacterStringType | PredefinedTypeByteStringType | PredefinedTypeNumericType | PredefinedTypeTemporalType | PredefinedTypeReferenceValueType | PredefinedTypeImmaterialValueType"""

    TYPE_ = hydra.core.Name("openGql.grammar.PredefinedType")
    BOOLEAN_TYPE = hydra.core.Name("booleanType")
    CHARACTER_STRING_TYPE = hydra.core.Name("characterStringType")
    BYTE_STRING_TYPE = hydra.core.Name("byteStringType")
    NUMERIC_TYPE = hydra.core.Name("numericType")
    TEMPORAL_TYPE = hydra.core.Name("temporalType")
    REFERENCE_VALUE_TYPE = hydra.core.Name("referenceValueType")
    IMMATERIAL_VALUE_TYPE = hydra.core.Name("immaterialValueType")

@dataclass(frozen=True)
class BooleanType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.BooleanType")
    NOT_NULL = hydra.core.Name("notNull")

class CharacterStringTypeStringType(Node["StringType"]):
    ...

class CharacterStringTypeCharType(Node["CharType"]):
    ...

class CharacterStringTypeVarcharType(Node["VarcharType"]):
    ...

class _CharacterStringTypeMeta(type):
    def __getitem__(cls, item):
        return object

class CharacterStringType(metaclass=_CharacterStringTypeMeta):
    r"""CharacterStringTypeStringType | CharacterStringTypeCharType | CharacterStringTypeVarcharType"""

    TYPE_ = hydra.core.Name("openGql.grammar.CharacterStringType")
    STRING_TYPE = hydra.core.Name("stringType")
    CHAR_TYPE = hydra.core.Name("charType")
    VARCHAR_TYPE = hydra.core.Name("varcharType")

@dataclass(frozen=True)
class StringType:
    min_length: Maybe[MinLength]
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.StringType")
    MIN_LENGTH = hydra.core.Name("minLength")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class CharType:
    fixed_length: Maybe[FixedLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.CharType")
    FIXED_LENGTH = hydra.core.Name("fixedLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class VarcharType:
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.VarcharType")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

class ByteStringTypeBytesType(Node["BytesType"]):
    ...

class ByteStringTypeBinaryType(Node["BinaryType"]):
    ...

class ByteStringTypeVarbinaryType(Node["VarbinaryType"]):
    ...

class _ByteStringTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ByteStringType(metaclass=_ByteStringTypeMeta):
    r"""ByteStringTypeBytesType | ByteStringTypeBinaryType | ByteStringTypeVarbinaryType"""

    TYPE_ = hydra.core.Name("openGql.grammar.ByteStringType")
    BYTES_TYPE = hydra.core.Name("bytesType")
    BINARY_TYPE = hydra.core.Name("binaryType")
    VARBINARY_TYPE = hydra.core.Name("varbinaryType")

@dataclass(frozen=True)
class BytesType:
    min_length: Maybe[MinLength]
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.BytesType")
    MIN_LENGTH = hydra.core.Name("minLength")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class BinaryType:
    fixed_length: Maybe[FixedLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.BinaryType")
    FIXED_LENGTH = hydra.core.Name("fixedLength")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class VarbinaryType:
    max_length: Maybe[MaxLength]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.VarbinaryType")
    MAX_LENGTH = hydra.core.Name("maxLength")
    NOT_NULL = hydra.core.Name("notNull")

MinLength: TypeAlias = "UnsignedInteger"

FixedLength: TypeAlias = "UnsignedInteger"

MaxLength: TypeAlias = "UnsignedInteger"

class NumericTypeExact(Node["ExactNumericType"]):
    ...

class NumericTypeApproximate(Node["ApproximateNumericType"]):
    ...

class _NumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NumericType(metaclass=_NumericTypeMeta):
    r"""NumericTypeExact | NumericTypeApproximate"""

    TYPE_ = hydra.core.Name("openGql.grammar.NumericType")
    EXACT = hydra.core.Name("exact")
    APPROXIMATE = hydra.core.Name("approximate")

class ExactNumericTypeBinary(Node["BinaryExactNumericType"]):
    ...

class ExactNumericTypeDecimal(Node["DecimalExactNumericType"]):
    ...

class _ExactNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ExactNumericType(metaclass=_ExactNumericTypeMeta):
    r"""ExactNumericTypeBinary | ExactNumericTypeDecimal"""

    TYPE_ = hydra.core.Name("openGql.grammar.ExactNumericType")
    BINARY = hydra.core.Name("binary")
    DECIMAL = hydra.core.Name("decimal")

class BinaryExactNumericTypeSigned(Node["SignedBinaryExactNumericType"]):
    ...

class BinaryExactNumericTypeUnsigned(Node["UnsignedBinaryExactNumericType"]):
    ...

class _BinaryExactNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class BinaryExactNumericType(metaclass=_BinaryExactNumericTypeMeta):
    r"""BinaryExactNumericTypeSigned | BinaryExactNumericTypeUnsigned"""

    TYPE_ = hydra.core.Name("openGql.grammar.BinaryExactNumericType")
    SIGNED = hydra.core.Name("signed")
    UNSIGNED = hydra.core.Name("unsigned")

class SignedBinaryExactNumericTypeInt8(Node["Int8Type"]):
    ...

class SignedBinaryExactNumericTypeInt16(Node["Int16Type"]):
    ...

class SignedBinaryExactNumericTypeInt32(Node["Int32Type"]):
    ...

class SignedBinaryExactNumericTypeInt64(Node["Int64Type"]):
    ...

class SignedBinaryExactNumericTypeInt128(Node["Int128Type"]):
    ...

class SignedBinaryExactNumericTypeInt256(Node["Int256Type"]):
    ...

class SignedBinaryExactNumericTypeSmallInt(Node["SmallIntType"]):
    ...

class SignedBinaryExactNumericTypeIntWithPrecision(Node["IntWithPrecision"]):
    ...

class SignedBinaryExactNumericTypeBigInt(Node["BigIntType"]):
    ...

class SignedBinaryExactNumericTypeSignedVerboseType(Node["SignedVerboseBinaryExactNumericType"]):
    ...

class _SignedBinaryExactNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class SignedBinaryExactNumericType(metaclass=_SignedBinaryExactNumericTypeMeta):
    r"""SignedBinaryExactNumericTypeInt8 | SignedBinaryExactNumericTypeInt16 | SignedBinaryExactNumericTypeInt32 | SignedBinaryExactNumericTypeInt64 | SignedBinaryExactNumericTypeInt128 | SignedBinaryExactNumericTypeInt256 | SignedBinaryExactNumericTypeSmallInt | SignedBinaryExactNumericTypeIntWithPrecision | SignedBinaryExactNumericTypeBigInt | SignedBinaryExactNumericTypeSignedVerboseType"""

    TYPE_ = hydra.core.Name("openGql.grammar.SignedBinaryExactNumericType")
    INT8 = hydra.core.Name("int8")
    INT16 = hydra.core.Name("int16")
    INT32 = hydra.core.Name("int32")
    INT64 = hydra.core.Name("int64")
    INT128 = hydra.core.Name("int128")
    INT256 = hydra.core.Name("int256")
    SMALL_INT = hydra.core.Name("smallInt")
    INT_WITH_PRECISION = hydra.core.Name("intWithPrecision")
    BIG_INT = hydra.core.Name("bigInt")
    SIGNED_VERBOSE_TYPE = hydra.core.Name("signedVerboseType")

@dataclass(frozen=True)
class Int8Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Int8Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Int16Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Int16Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Int32Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Int32Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Int64Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Int64Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Int128Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Int128Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Int256Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Int256Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class SmallIntType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.SmallIntType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class BigIntType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.BigIntType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class IntWithPrecision:
    precision: Maybe[Precision]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.IntWithPrecision")
    PRECISION = hydra.core.Name("precision")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class SignedVerboseBinaryExactNumericType:
    signed: bool
    verbose_type: VerboseBinaryExactNumericType

    TYPE_ = hydra.core.Name("openGql.grammar.SignedVerboseBinaryExactNumericType")
    SIGNED = hydra.core.Name("signed")
    VERBOSE_TYPE = hydra.core.Name("verboseType")

class UnsignedBinaryExactNumericTypeUint8(Node["Uint8Type"]):
    ...

class UnsignedBinaryExactNumericTypeUint16(Node["Uint16Type"]):
    ...

class UnsignedBinaryExactNumericTypeUint32(Node["Uint32Type"]):
    ...

class UnsignedBinaryExactNumericTypeUint64(Node["Uint64Type"]):
    ...

class UnsignedBinaryExactNumericTypeUint128(Node["Uint128Type"]):
    ...

class UnsignedBinaryExactNumericTypeUint256(Node["Uint256Type"]):
    ...

class UnsignedBinaryExactNumericTypeUSmallInt(Node["USmallIntType"]):
    ...

class UnsignedBinaryExactNumericTypeUintWithPrecision(Node["UintWithPrecision"]):
    ...

class UnsignedBinaryExactNumericTypeUBigInt(Node["UBigIntType"]):
    ...

class UnsignedBinaryExactNumericTypeUnsigned(Node["VerboseBinaryExactNumericType"]):
    ...

class _UnsignedBinaryExactNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedBinaryExactNumericType(metaclass=_UnsignedBinaryExactNumericTypeMeta):
    r"""UnsignedBinaryExactNumericTypeUint8 | UnsignedBinaryExactNumericTypeUint16 | UnsignedBinaryExactNumericTypeUint32 | UnsignedBinaryExactNumericTypeUint64 | UnsignedBinaryExactNumericTypeUint128 | UnsignedBinaryExactNumericTypeUint256 | UnsignedBinaryExactNumericTypeUSmallInt | UnsignedBinaryExactNumericTypeUintWithPrecision | UnsignedBinaryExactNumericTypeUBigInt | UnsignedBinaryExactNumericTypeUnsigned"""

    TYPE_ = hydra.core.Name("openGql.grammar.UnsignedBinaryExactNumericType")
    UINT8 = hydra.core.Name("uint8")
    UINT16 = hydra.core.Name("uint16")
    UINT32 = hydra.core.Name("uint32")
    UINT64 = hydra.core.Name("uint64")
    UINT128 = hydra.core.Name("uint128")
    UINT256 = hydra.core.Name("uint256")
    U_SMALL_INT = hydra.core.Name("uSmallInt")
    UINT_WITH_PRECISION = hydra.core.Name("uintWithPrecision")
    U_BIG_INT = hydra.core.Name("uBigInt")
    UNSIGNED = hydra.core.Name("unsigned")

@dataclass(frozen=True)
class Uint8Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Uint8Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Uint16Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Uint16Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Uint32Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Uint32Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Uint64Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Uint64Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Uint128Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Uint128Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Uint256Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Uint256Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class USmallIntType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.USmallIntType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class UBigIntType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.UBigIntType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class UintWithPrecision:
    precision: Maybe[Precision]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.UintWithPrecision")
    PRECISION = hydra.core.Name("precision")
    NOT_NULL = hydra.core.Name("notNull")

class VerboseBinaryExactNumericTypeInteger8(Node["Integer8Type"]):
    ...

class VerboseBinaryExactNumericTypeInteger16(Node["Integer16Type"]):
    ...

class VerboseBinaryExactNumericTypeInteger32(Node["Integer32Type"]):
    ...

class VerboseBinaryExactNumericTypeInteger64(Node["Integer64Type"]):
    ...

class VerboseBinaryExactNumericTypeInteger128(Node["Integer128Type"]):
    ...

class VerboseBinaryExactNumericTypeInteger256(Node["Integer256Type"]):
    ...

class VerboseBinaryExactNumericTypeSmallInteger(Node["SmallIntegerType"]):
    ...

class VerboseBinaryExactNumericTypeIntegerWithPrecision(Node["IntegerWithPrecision"]):
    ...

class VerboseBinaryExactNumericTypeBigInteger(Node["BigIntegerType"]):
    ...

class _VerboseBinaryExactNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class VerboseBinaryExactNumericType(metaclass=_VerboseBinaryExactNumericTypeMeta):
    r"""VerboseBinaryExactNumericTypeInteger8 | VerboseBinaryExactNumericTypeInteger16 | VerboseBinaryExactNumericTypeInteger32 | VerboseBinaryExactNumericTypeInteger64 | VerboseBinaryExactNumericTypeInteger128 | VerboseBinaryExactNumericTypeInteger256 | VerboseBinaryExactNumericTypeSmallInteger | VerboseBinaryExactNumericTypeIntegerWithPrecision | VerboseBinaryExactNumericTypeBigInteger"""

    TYPE_ = hydra.core.Name("openGql.grammar.VerboseBinaryExactNumericType")
    INTEGER8 = hydra.core.Name("integer8")
    INTEGER16 = hydra.core.Name("integer16")
    INTEGER32 = hydra.core.Name("integer32")
    INTEGER64 = hydra.core.Name("integer64")
    INTEGER128 = hydra.core.Name("integer128")
    INTEGER256 = hydra.core.Name("integer256")
    SMALL_INTEGER = hydra.core.Name("smallInteger")
    INTEGER_WITH_PRECISION = hydra.core.Name("integerWithPrecision")
    BIG_INTEGER = hydra.core.Name("bigInteger")

@dataclass(frozen=True)
class Integer8Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Integer8Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Integer16Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Integer16Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Integer32Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Integer32Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Integer64Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Integer64Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Integer128Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Integer128Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Integer256Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Integer256Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class SmallIntegerType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.SmallIntegerType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class BigIntegerType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.BigIntegerType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class IntegerWithPrecision:
    precision: Maybe[Precision]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.IntegerWithPrecision")
    PRECISION = hydra.core.Name("precision")
    NOT_NULL = hydra.core.Name("notNull")

Precision: TypeAlias = "UnsignedDecimalInteger"

DecimalExactNumericType: TypeAlias = "Maybe[PrecisionAndScale]"

@dataclass(frozen=True)
class PrecisionAndScale:
    precision: Precision
    scale: Maybe[Scale]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.PrecisionAndScale")
    PRECISION = hydra.core.Name("precision")
    SCALE = hydra.core.Name("scale")
    NOT_NULL = hydra.core.Name("notNull")

Scale: TypeAlias = "UnsignedDecimalInteger"

class ApproximateNumericTypeFloat16(Node["Float16Type"]):
    ...

class ApproximateNumericTypeFloat32(Node["Float32Type"]):
    ...

class ApproximateNumericTypeFloat64(Node["Float64Type"]):
    ...

class ApproximateNumericTypeFloat128(Node["Float128Type"]):
    ...

class ApproximateNumericTypeFloat256(Node["Float256Type"]):
    ...

class ApproximateNumericTypeFloatWithPrecision(Node["FloatTypeWithPrecision"]):
    ...

class ApproximateNumericTypeReal(Node["RealType"]):
    ...

class ApproximateNumericTypeDoubleWithPrecision(Node["DoubleTypeWithPrecision"]):
    ...

class _ApproximateNumericTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ApproximateNumericType(metaclass=_ApproximateNumericTypeMeta):
    r"""ApproximateNumericTypeFloat16 | ApproximateNumericTypeFloat32 | ApproximateNumericTypeFloat64 | ApproximateNumericTypeFloat128 | ApproximateNumericTypeFloat256 | ApproximateNumericTypeFloatWithPrecision | ApproximateNumericTypeReal | ApproximateNumericTypeDoubleWithPrecision"""

    TYPE_ = hydra.core.Name("openGql.grammar.ApproximateNumericType")
    FLOAT16 = hydra.core.Name("float16")
    FLOAT32 = hydra.core.Name("float32")
    FLOAT64 = hydra.core.Name("float64")
    FLOAT128 = hydra.core.Name("float128")
    FLOAT256 = hydra.core.Name("float256")
    FLOAT_WITH_PRECISION = hydra.core.Name("floatWithPrecision")
    REAL = hydra.core.Name("real")
    DOUBLE_WITH_PRECISION = hydra.core.Name("doubleWithPrecision")

@dataclass(frozen=True)
class Float16Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Float16Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Float32Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Float32Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Float64Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Float64Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Float128Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Float128Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class Float256Type:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.Float256Type")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class FloatTypeWithPrecision:
    precision_and_scale: Maybe[PrecisionAndScale]
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.FloatTypeWithPrecision")
    PRECISION_AND_SCALE = hydra.core.Name("precisionAndScale")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class RealType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.RealType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class DoubleTypeWithPrecision:
    precision: bool
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.DoubleTypeWithPrecision")
    PRECISION = hydra.core.Name("precision")
    NOT_NULL = hydra.core.Name("notNull")

class TemporalTypeInstantType(Node["TemporalInstantType"]):
    ...

class TemporalTypeDurationType(Node["TemporalDurationType"]):
    ...

class _TemporalTypeMeta(type):
    def __getitem__(cls, item):
        return object

class TemporalType(metaclass=_TemporalTypeMeta):
    r"""TemporalTypeInstantType | TemporalTypeDurationType"""

    TYPE_ = hydra.core.Name("openGql.grammar.TemporalType")
    INSTANT_TYPE = hydra.core.Name("instantType")
    DURATION_TYPE = hydra.core.Name("durationType")

class TemporalInstantTypeDatetimeType(Node["DatetimeType"]):
    ...

class TemporalInstantTypeLocaldatetimeType(Node["LocaldatetimeType"]):
    ...

class TemporalInstantTypeDateType(Node["DateType"]):
    ...

class TemporalInstantTypeTimeType(Node["TimeType"]):
    ...

class TemporalInstantTypeLocaltimeType(Node["LocaltimeType"]):
    ...

class _TemporalInstantTypeMeta(type):
    def __getitem__(cls, item):
        return object

class TemporalInstantType(metaclass=_TemporalInstantTypeMeta):
    r"""TemporalInstantTypeDatetimeType | TemporalInstantTypeLocaldatetimeType | TemporalInstantTypeDateType | TemporalInstantTypeTimeType | TemporalInstantTypeLocaltimeType"""

    TYPE_ = hydra.core.Name("openGql.grammar.TemporalInstantType")
    DATETIME_TYPE = hydra.core.Name("datetimeType")
    LOCALDATETIME_TYPE = hydra.core.Name("localdatetimeType")
    DATE_TYPE = hydra.core.Name("dateType")
    TIME_TYPE = hydra.core.Name("timeType")
    LOCALTIME_TYPE = hydra.core.Name("localtimeType")

class DatetimeTypeZonedDatetime(Node["ZonedDatetimeType"]):
    ...

class DatetimeTypeTimestampWithTimeZone(Node["TimestampWithTimeZoneType"]):
    ...

class _DatetimeTypeMeta(type):
    def __getitem__(cls, item):
        return object

class DatetimeType(metaclass=_DatetimeTypeMeta):
    r"""DatetimeTypeZonedDatetime | DatetimeTypeTimestampWithTimeZone"""

    TYPE_ = hydra.core.Name("openGql.grammar.DatetimeType")
    ZONED_DATETIME = hydra.core.Name("zonedDatetime")
    TIMESTAMP_WITH_TIME_ZONE = hydra.core.Name("timestampWithTimeZone")

@dataclass(frozen=True)
class ZonedDatetimeType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ZonedDatetimeType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class TimestampWithTimeZoneType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.TimestampWithTimeZoneType")
    NOT_NULL = hydra.core.Name("notNull")

class LocaldatetimeTypeLocalDatetime(Node["LocalDatetimeType"]):
    ...

class LocaldatetimeTypeTimestampWithoutTimeZone(Node["TimestampWithoutTimeZoneType"]):
    ...

class _LocaldatetimeTypeMeta(type):
    def __getitem__(cls, item):
        return object

class LocaldatetimeType(metaclass=_LocaldatetimeTypeMeta):
    r"""LocaldatetimeTypeLocalDatetime | LocaldatetimeTypeTimestampWithoutTimeZone"""

    TYPE_ = hydra.core.Name("openGql.grammar.LocaldatetimeType")
    LOCAL_DATETIME = hydra.core.Name("localDatetime")
    TIMESTAMP_WITHOUT_TIME_ZONE = hydra.core.Name("timestampWithoutTimeZone")

@dataclass(frozen=True)
class LocalDatetimeType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.LocalDatetimeType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class TimestampWithoutTimeZoneType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.TimestampWithoutTimeZoneType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class DateType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.DateType")
    NOT_NULL = hydra.core.Name("notNull")

class TimeTypeZonedTime(Node["ZonedTimeType"]):
    ...

class TimeTypeTimeWithTimeZone(Node["TimeWithTimeZoneType"]):
    ...

class _TimeTypeMeta(type):
    def __getitem__(cls, item):
        return object

class TimeType(metaclass=_TimeTypeMeta):
    r"""TimeTypeZonedTime | TimeTypeTimeWithTimeZone"""

    TYPE_ = hydra.core.Name("openGql.grammar.TimeType")
    ZONED_TIME = hydra.core.Name("zonedTime")
    TIME_WITH_TIME_ZONE = hydra.core.Name("timeWithTimeZone")

@dataclass(frozen=True)
class ZonedTimeType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ZonedTimeType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class TimeWithTimeZoneType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.TimeWithTimeZoneType")
    NOT_NULL = hydra.core.Name("notNull")

class LocaltimeTypeLocalTime(Node["LocalTimeType"]):
    ...

class LocaltimeTypeTimeWithoutTimeZone(Node["TimeWithoutTimeZoneType"]):
    ...

class _LocaltimeTypeMeta(type):
    def __getitem__(cls, item):
        return object

class LocaltimeType(metaclass=_LocaltimeTypeMeta):
    r"""LocaltimeTypeLocalTime | LocaltimeTypeTimeWithoutTimeZone"""

    TYPE_ = hydra.core.Name("openGql.grammar.LocaltimeType")
    LOCAL_TIME = hydra.core.Name("localTime")
    TIME_WITHOUT_TIME_ZONE = hydra.core.Name("timeWithoutTimeZone")

@dataclass(frozen=True)
class LocalTimeType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.LocalTimeType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class TimeWithoutTimeZoneType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.TimeWithoutTimeZoneType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class TemporalDurationType:
    qualifier: TemporalDurationQualifier
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.TemporalDurationType")
    QUALIFIER = hydra.core.Name("qualifier")
    NOT_NULL = hydra.core.Name("notNull")

class TemporalDurationQualifier(Enum):
    YEAR_TO_MONTH = hydra.core.Name("yearToMonth")

    DAY_TO_SECOND = hydra.core.Name("dayToSecond")

TemporalDurationQualifier.TYPE_ = hydra.core.Name("openGql.grammar.TemporalDurationQualifier")

class ReferenceValueTypeGraph(Node["GraphReferenceValueType"]):
    ...

class ReferenceValueTypeBindingTable(Node["BindingTableReferenceValueType"]):
    ...

class ReferenceValueTypeNode(Node["NodeReferenceValueType"]):
    ...

class ReferenceValueTypeEdge(Node["EdgeReferenceValueType"]):
    ...

class _ReferenceValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ReferenceValueType(metaclass=_ReferenceValueTypeMeta):
    r"""ReferenceValueTypeGraph | ReferenceValueTypeBindingTable | ReferenceValueTypeNode | ReferenceValueTypeEdge"""

    TYPE_ = hydra.core.Name("openGql.grammar.ReferenceValueType")
    GRAPH = hydra.core.Name("graph")
    BINDING_TABLE = hydra.core.Name("bindingTable")
    NODE = hydra.core.Name("node")
    EDGE = hydra.core.Name("edge")

class ImmaterialValueTypeNullType(Node["NullType"]):
    ...

class ImmaterialValueTypeEmptyType(Node["EmptyType"]):
    ...

class _ImmaterialValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class ImmaterialValueType(metaclass=_ImmaterialValueTypeMeta):
    r"""ImmaterialValueTypeNullType | ImmaterialValueTypeEmptyType"""

    TYPE_ = hydra.core.Name("openGql.grammar.ImmaterialValueType")
    NULL_TYPE = hydra.core.Name("nullType")
    EMPTY_TYPE = hydra.core.Name("emptyType")

NullType: TypeAlias = "None"

EmptyType: TypeAlias = "None"

class GraphReferenceValueTypeOpen(Node["OpenGraphReferenceValueType"]):
    ...

class GraphReferenceValueTypeClosed(Node["ClosedGraphReferenceValueType"]):
    ...

class _GraphReferenceValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class GraphReferenceValueType(metaclass=_GraphReferenceValueTypeMeta):
    r"""GraphReferenceValueTypeOpen | GraphReferenceValueTypeClosed"""

    TYPE_ = hydra.core.Name("openGql.grammar.GraphReferenceValueType")
    OPEN = hydra.core.Name("open")
    CLOSED = hydra.core.Name("closed")

@dataclass(frozen=True)
class ClosedGraphReferenceValueType:
    property: bool
    nested_spec: NestedGraphTypeSpecification
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ClosedGraphReferenceValueType")
    PROPERTY = hydra.core.Name("property")
    NESTED_SPEC = hydra.core.Name("nestedSpec")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class OpenGraphReferenceValueType:
    any: Maybe[bool]
    property: bool
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.OpenGraphReferenceValueType")
    ANY = hydra.core.Name("any")
    PROPERTY = hydra.core.Name("property")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class BindingTableReferenceValueType:
    binding_table_type: BindingTableType
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.BindingTableReferenceValueType")
    BINDING_TABLE_TYPE = hydra.core.Name("bindingTableType")
    NOT_NULL = hydra.core.Name("notNull")

class NodeReferenceValueTypeOpen(Node["OpenNodeReferenceValueType"]):
    ...

class NodeReferenceValueTypeClosed(Node["ClosedNodeReferenceValueType"]):
    ...

class _NodeReferenceValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class NodeReferenceValueType(metaclass=_NodeReferenceValueTypeMeta):
    r"""NodeReferenceValueTypeOpen | NodeReferenceValueTypeClosed"""

    TYPE_ = hydra.core.Name("openGql.grammar.NodeReferenceValueType")
    OPEN = hydra.core.Name("open")
    CLOSED = hydra.core.Name("closed")

@dataclass(frozen=True)
class ClosedNodeReferenceValueType:
    node_type_spec: NodeTypeSpecification
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ClosedNodeReferenceValueType")
    NODE_TYPE_SPEC = hydra.core.Name("nodeTypeSpec")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class OpenNodeReferenceValueType:
    any: bool
    node_synonym: NodeSynonym
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.OpenNodeReferenceValueType")
    ANY = hydra.core.Name("any")
    NODE_SYNONYM = hydra.core.Name("nodeSynonym")
    NOT_NULL = hydra.core.Name("notNull")

class EdgeReferenceValueTypeOpen(Node["OpenEdgeReferenceValueType"]):
    ...

class EdgeReferenceValueTypeClosed(Node["ClosedEdgeReferenceValueType"]):
    ...

class _EdgeReferenceValueTypeMeta(type):
    def __getitem__(cls, item):
        return object

class EdgeReferenceValueType(metaclass=_EdgeReferenceValueTypeMeta):
    r"""EdgeReferenceValueTypeOpen | EdgeReferenceValueTypeClosed"""

    TYPE_ = hydra.core.Name("openGql.grammar.EdgeReferenceValueType")
    OPEN = hydra.core.Name("open")
    CLOSED = hydra.core.Name("closed")

@dataclass(frozen=True)
class ClosedEdgeReferenceValueType:
    edge_type_spec: EdgeTypeSpecification
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.ClosedEdgeReferenceValueType")
    EDGE_TYPE_SPEC = hydra.core.Name("edgeTypeSpec")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class OpenEdgeReferenceValueType:
    any: bool
    edge_synonym: EdgeSynonym
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.OpenEdgeReferenceValueType")
    ANY = hydra.core.Name("any")
    EDGE_SYNONYM = hydra.core.Name("edgeSynonym")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class PathValueType:
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.PathValueType")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class ListValueTypeName:
    group: bool
    synonym: ListValueTypeNameSynonym

    TYPE_ = hydra.core.Name("openGql.grammar.ListValueTypeName")
    GROUP = hydra.core.Name("group")
    SYNONYM = hydra.core.Name("synonym")

class ListValueTypeNameSynonym(Enum):
    LIST = hydra.core.Name("list")

    ARRAY = hydra.core.Name("array")

ListValueTypeNameSynonym.TYPE_ = hydra.core.Name("openGql.grammar.ListValueTypeNameSynonym")

class RecordTypeAnyRecord(Node["AnyRecordType"]):
    ...

class RecordTypeSpecifiedRecord(Node["SpecifiedRecordType"]):
    ...

class _RecordTypeMeta(type):
    def __getitem__(cls, item):
        return object

class RecordType(metaclass=_RecordTypeMeta):
    r"""RecordTypeAnyRecord | RecordTypeSpecifiedRecord"""

    TYPE_ = hydra.core.Name("openGql.grammar.RecordType")
    ANY_RECORD = hydra.core.Name("anyRecord")
    SPECIFIED_RECORD = hydra.core.Name("specifiedRecord")

@dataclass(frozen=True)
class AnyRecordType:
    any: bool
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.AnyRecordType")
    ANY = hydra.core.Name("any")
    NOT_NULL = hydra.core.Name("notNull")

@dataclass(frozen=True)
class SpecifiedRecordType:
    record: bool
    field_types: FieldTypesSpecification
    not_null: bool

    TYPE_ = hydra.core.Name("openGql.grammar.SpecifiedRecordType")
    RECORD = hydra.core.Name("record")
    FIELD_TYPES = hydra.core.Name("fieldTypes")
    NOT_NULL = hydra.core.Name("notNull")

FieldTypesSpecification: TypeAlias = "Maybe[FieldTypeList]"

FieldTypeList: TypeAlias = "frozenlist[FieldType]"

NotNull: TypeAlias = "None"

@dataclass(frozen=True)
class FieldType:
    field_name: FieldName
    typed: Maybe[Typed]
    value_type: ValueType

    TYPE_ = hydra.core.Name("openGql.grammar.FieldType")
    FIELD_NAME = hydra.core.Name("fieldName")
    TYPED = hydra.core.Name("typed")
    VALUE_TYPE = hydra.core.Name("valueType")

SearchCondition: TypeAlias = "BooleanValueExpression"

class PredicateExistsPredicate(Node["ExistsPredicate"]):
    ...

class PredicateNullPredicate(Node["NullPredicate"]):
    ...

class PredicateValueTypePredicate(Node["ValueTypePredicate"]):
    ...

class PredicateDirectedPredicate(Node["DirectedPredicate"]):
    ...

class PredicateLabeledPredicate(Node["LabeledPredicate"]):
    ...

class PredicateSourceDestinationPredicate(Node["SourceDestinationPredicate"]):
    ...

class PredicateAllDifferentPredicate(Node["AllDifferentPredicate"]):
    ...

class PredicateSamePredicate(Node["SamePredicate"]):
    ...

class PredicatePropertyExistsPredicate(Node["PropertyExistsPredicate"]):
    ...

class _PredicateMeta(type):
    def __getitem__(cls, item):
        return object

class Predicate(metaclass=_PredicateMeta):
    r"""PredicateExistsPredicate | PredicateNullPredicate | PredicateValueTypePredicate | PredicateDirectedPredicate | PredicateLabeledPredicate | PredicateSourceDestinationPredicate | PredicateAllDifferentPredicate | PredicateSamePredicate | PredicatePropertyExistsPredicate"""

    TYPE_ = hydra.core.Name("openGql.grammar.Predicate")
    EXISTS_PREDICATE = hydra.core.Name("existsPredicate")
    NULL_PREDICATE = hydra.core.Name("nullPredicate")
    VALUE_TYPE_PREDICATE = hydra.core.Name("valueTypePredicate")
    DIRECTED_PREDICATE = hydra.core.Name("directedPredicate")
    LABELED_PREDICATE = hydra.core.Name("labeledPredicate")
    SOURCE_DESTINATION_PREDICATE = hydra.core.Name("sourceDestinationPredicate")
    ALL_DIFFERENT_PREDICATE = hydra.core.Name("allDifferentPredicate")
    SAME_PREDICATE = hydra.core.Name("samePredicate")
    PROPERTY_EXISTS_PREDICATE = hydra.core.Name("propertyExistsPredicate")

@dataclass(frozen=True)
class ComparisonPredicatePart2:
    comp_op: CompOp
    value_expression: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.ComparisonPredicatePart2")
    COMP_OP = hydra.core.Name("compOp")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")

class CompOp(Enum):
    EQUALS = hydra.core.Name("equals")

    NOT_EQUALS = hydra.core.Name("notEquals")

    LESS_THAN = hydra.core.Name("lessThan")

    GREATER_THAN = hydra.core.Name("greaterThan")

    LESS_THAN_OR_EQUALS = hydra.core.Name("lessThanOrEquals")

    GREATER_THAN_OR_EQUALS = hydra.core.Name("greaterThanOrEquals")

CompOp.TYPE_ = hydra.core.Name("openGql.grammar.CompOp")

class ExistsPredicateGraphPatternBrace(Node["GraphPattern"]):
    ...

class ExistsPredicateGraphPatternParen(Node["GraphPattern"]):
    ...

class ExistsPredicateMatchBlockBrace(Node["MatchStatementBlock"]):
    ...

class ExistsPredicateMatchBlockParen(Node["MatchStatementBlock"]):
    ...

class ExistsPredicateNestedQuery(Node["NestedQuerySpecification"]):
    ...

class _ExistsPredicateMeta(type):
    def __getitem__(cls, item):
        return object

class ExistsPredicate(metaclass=_ExistsPredicateMeta):
    r"""ExistsPredicateGraphPatternBrace | ExistsPredicateGraphPatternParen | ExistsPredicateMatchBlockBrace | ExistsPredicateMatchBlockParen | ExistsPredicateNestedQuery"""

    TYPE_ = hydra.core.Name("openGql.grammar.ExistsPredicate")
    GRAPH_PATTERN_BRACE = hydra.core.Name("graphPatternBrace")
    GRAPH_PATTERN_PAREN = hydra.core.Name("graphPatternParen")
    MATCH_BLOCK_BRACE = hydra.core.Name("matchBlockBrace")
    MATCH_BLOCK_PAREN = hydra.core.Name("matchBlockParen")
    NESTED_QUERY = hydra.core.Name("nestedQuery")

@dataclass(frozen=True)
class NullPredicate:
    value_expression: PrimaryValueExpression
    null_part: NullPredicatePart2

    TYPE_ = hydra.core.Name("openGql.grammar.NullPredicate")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    NULL_PART = hydra.core.Name("nullPart")

@dataclass(frozen=True)
class NullPredicatePart2:
    not_: bool

    TYPE_ = hydra.core.Name("openGql.grammar.NullPredicatePart2")
    NOT = hydra.core.Name("not")

@dataclass(frozen=True)
class ValueTypePredicate:
    value_expression: PrimaryValueExpression
    value_type_part: ValueTypePredicatePart2

    TYPE_ = hydra.core.Name("openGql.grammar.ValueTypePredicate")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    VALUE_TYPE_PART = hydra.core.Name("valueTypePart")

@dataclass(frozen=True)
class ValueTypePredicatePart2:
    not_: bool
    typed: Typed
    value_type: ValueType

    TYPE_ = hydra.core.Name("openGql.grammar.ValueTypePredicatePart2")
    NOT = hydra.core.Name("not")
    TYPED = hydra.core.Name("typed")
    VALUE_TYPE = hydra.core.Name("valueType")

@dataclass(frozen=True)
class NormalizedPredicatePart2:
    not_: bool
    normal_form: Maybe[NormalForm]

    TYPE_ = hydra.core.Name("openGql.grammar.NormalizedPredicatePart2")
    NOT = hydra.core.Name("not")
    NORMAL_FORM = hydra.core.Name("normalForm")

@dataclass(frozen=True)
class DirectedPredicate:
    element_variable_reference: ElementVariableReference
    directed_part: DirectedPredicatePart2

    TYPE_ = hydra.core.Name("openGql.grammar.DirectedPredicate")
    ELEMENT_VARIABLE_REFERENCE = hydra.core.Name("elementVariableReference")
    DIRECTED_PART = hydra.core.Name("directedPart")

@dataclass(frozen=True)
class DirectedPredicatePart2:
    not_: bool

    TYPE_ = hydra.core.Name("openGql.grammar.DirectedPredicatePart2")
    NOT = hydra.core.Name("not")

@dataclass(frozen=True)
class LabeledPredicate:
    element_variable_reference: ElementVariableReference
    labeled_part: LabeledPredicatePart2

    TYPE_ = hydra.core.Name("openGql.grammar.LabeledPredicate")
    ELEMENT_VARIABLE_REFERENCE = hydra.core.Name("elementVariableReference")
    LABELED_PART = hydra.core.Name("labeledPart")

@dataclass(frozen=True)
class LabeledPredicatePart2:
    is_labeled_or_colon: IsLabeledOrColon
    label_expression: LabelExpression

    TYPE_ = hydra.core.Name("openGql.grammar.LabeledPredicatePart2")
    IS_LABELED_OR_COLON = hydra.core.Name("isLabeledOrColon")
    LABEL_EXPRESSION = hydra.core.Name("labelExpression")

class IsLabeledOrColonNot(Node[bool]):
    ...

class IsLabeledOrColonColon:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, IsLabeledOrColonColon)
    def __hash__(self):
        return hash("IsLabeledOrColonColon")

class _IsLabeledOrColonMeta(type):
    def __getitem__(cls, item):
        return object

class IsLabeledOrColon(metaclass=_IsLabeledOrColonMeta):
    r"""IsLabeledOrColonNot | IsLabeledOrColonColon"""

    TYPE_ = hydra.core.Name("openGql.grammar.IsLabeledOrColon")
    NOT = hydra.core.Name("not")
    COLON = hydra.core.Name("colon")

class SourceDestinationPredicateSourcePredicate(Node["SourcePredicate"]):
    ...

class SourceDestinationPredicateDestinationPredicate(Node["DestinationPredicate"]):
    ...

class _SourceDestinationPredicateMeta(type):
    def __getitem__(cls, item):
        return object

class SourceDestinationPredicate(metaclass=_SourceDestinationPredicateMeta):
    r"""SourceDestinationPredicateSourcePredicate | SourceDestinationPredicateDestinationPredicate"""

    TYPE_ = hydra.core.Name("openGql.grammar.SourceDestinationPredicate")
    SOURCE_PREDICATE = hydra.core.Name("sourcePredicate")
    DESTINATION_PREDICATE = hydra.core.Name("destinationPredicate")

NodeReference: TypeAlias = "ElementVariableReference"

@dataclass(frozen=True)
class SourcePredicate:
    not_: bool
    source_of: EdgeReference

    TYPE_ = hydra.core.Name("openGql.grammar.SourcePredicate")
    NOT = hydra.core.Name("not")
    SOURCE_OF = hydra.core.Name("sourceOf")

@dataclass(frozen=True)
class DestinationPredicate:
    node_reference: NodeReference
    not_: bool
    destination_of: EdgeReference

    TYPE_ = hydra.core.Name("openGql.grammar.DestinationPredicate")
    NODE_REFERENCE = hydra.core.Name("nodeReference")
    NOT = hydra.core.Name("not")
    DESTINATION_OF = hydra.core.Name("destinationOf")

EdgeReference: TypeAlias = "ElementVariableReference"

@dataclass(frozen=True)
class AllDifferentPredicate:
    references: frozenlist[ElementVariableReference]

    TYPE_ = hydra.core.Name("openGql.grammar.AllDifferentPredicate")
    REFERENCES = hydra.core.Name("references")

@dataclass(frozen=True)
class SamePredicate:
    references: frozenlist[ElementVariableReference]

    TYPE_ = hydra.core.Name("openGql.grammar.SamePredicate")
    REFERENCES = hydra.core.Name("references")

@dataclass(frozen=True)
class PropertyExistsPredicate:
    element_variable_reference: ElementVariableReference
    property_name: PropertyName

    TYPE_ = hydra.core.Name("openGql.grammar.PropertyExistsPredicate")
    ELEMENT_VARIABLE_REFERENCE = hydra.core.Name("elementVariableReference")
    PROPERTY_NAME = hydra.core.Name("propertyName")

class ValueExpressionSigned(Node["SignedExpr"]):
    ...

class ValueExpressionMultDiv(Node["MultDivExpr"]):
    ...

class ValueExpressionAddSubtract(Node["AddSubtractExpr"]):
    ...

class ValueExpressionConcatenation(Node["ConcatenationExpr"]):
    ...

class ValueExpressionNot(Node["NotExpr"]):
    ...

class ValueExpressionIsNot(Node["IsNotExpr"]):
    ...

class ValueExpressionConjunctive(Node["ConjunctiveExpr"]):
    ...

class ValueExpressionDisjunctive(Node["DisjunctiveExpr"]):
    ...

class ValueExpressionComparison(Node["ComparisonExpr"]):
    ...

class ValueExpressionPredicate(Node["Predicate"]):
    ...

class ValueExpressionNormalizedPredicate(Node["NormalizedPredicateExpr"]):
    ...

class ValueExpressionPropertyGraph(Node["GraphExpression"]):
    ...

class ValueExpressionBindingTable(Node["BindingTableExpression"]):
    ...

class ValueExpressionValueFunction(Node["ValueFunction"]):
    ...

class ValueExpressionPrimary(Node["PrimaryValueExpression"]):
    ...

class _ValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ValueExpression(metaclass=_ValueExpressionMeta):
    r"""ValueExpressionSigned | ValueExpressionMultDiv | ValueExpressionAddSubtract | ValueExpressionConcatenation | ValueExpressionNot | ValueExpressionIsNot | ValueExpressionConjunctive | ValueExpressionDisjunctive | ValueExpressionComparison | ValueExpressionPredicate | ValueExpressionNormalizedPredicate | ValueExpressionPropertyGraph | ValueExpressionBindingTable | ValueExpressionValueFunction | ValueExpressionPrimary"""

    TYPE_ = hydra.core.Name("openGql.grammar.ValueExpression")
    SIGNED = hydra.core.Name("signed")
    MULT_DIV = hydra.core.Name("multDiv")
    ADD_SUBTRACT = hydra.core.Name("addSubtract")
    CONCATENATION = hydra.core.Name("concatenation")
    NOT = hydra.core.Name("not")
    IS_NOT = hydra.core.Name("isNot")
    CONJUNCTIVE = hydra.core.Name("conjunctive")
    DISJUNCTIVE = hydra.core.Name("disjunctive")
    COMPARISON = hydra.core.Name("comparison")
    PREDICATE = hydra.core.Name("predicate")
    NORMALIZED_PREDICATE = hydra.core.Name("normalizedPredicate")
    PROPERTY_GRAPH = hydra.core.Name("propertyGraph")
    BINDING_TABLE = hydra.core.Name("bindingTable")
    VALUE_FUNCTION = hydra.core.Name("valueFunction")
    PRIMARY = hydra.core.Name("primary")

@dataclass(frozen=True)
class SignedExpr:
    sign: Sign
    value_expression: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.SignedExpr")
    SIGN = hydra.core.Name("sign")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")

@dataclass(frozen=True)
class MultDivExpr:
    left: ValueExpression
    operator: MultDivOperator
    right: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.MultDivExpr")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class AddSubtractExpr:
    left: ValueExpression
    operator: AddSubtractOperator
    right: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.AddSubtractExpr")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class ConcatenationExpr:
    left: ValueExpression
    right: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.ConcatenationExpr")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

NotExpr: TypeAlias = "ValueExpression"

@dataclass(frozen=True)
class IsNotExpr:
    value_expression: ValueExpression
    not_: bool
    truth_value: TruthValue

    TYPE_ = hydra.core.Name("openGql.grammar.IsNotExpr")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    NOT = hydra.core.Name("not")
    TRUTH_VALUE = hydra.core.Name("truthValue")

@dataclass(frozen=True)
class ConjunctiveExpr:
    left: ValueExpression
    right: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.ConjunctiveExpr")
    LEFT = hydra.core.Name("left")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class DisjunctiveExpr:
    left: ValueExpression
    operator: DisjunctiveOperator
    right: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.DisjunctiveExpr")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class ComparisonExpr:
    value_expression: ValueExpression
    comparison: ComparisonPredicatePart2

    TYPE_ = hydra.core.Name("openGql.grammar.ComparisonExpr")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    COMPARISON = hydra.core.Name("comparison")

@dataclass(frozen=True)
class NormalizedPredicateExpr:
    value_expression: ValueExpression
    normalized_predicate: NormalizedPredicatePart2

    TYPE_ = hydra.core.Name("openGql.grammar.NormalizedPredicateExpr")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    NORMALIZED_PREDICATE = hydra.core.Name("normalizedPredicate")

class Sign(Enum):
    PLUS = hydra.core.Name("plus")

    MINUS = hydra.core.Name("minus")

Sign.TYPE_ = hydra.core.Name("openGql.grammar.Sign")

class MultDivOperator(Enum):
    MULTIPLY = hydra.core.Name("multiply")

    DIVIDE = hydra.core.Name("divide")

MultDivOperator.TYPE_ = hydra.core.Name("openGql.grammar.MultDivOperator")

class AddSubtractOperator(Enum):
    ADD = hydra.core.Name("add")

    SUBTRACT = hydra.core.Name("subtract")

AddSubtractOperator.TYPE_ = hydra.core.Name("openGql.grammar.AddSubtractOperator")

class DisjunctiveOperator(Enum):
    OR = hydra.core.Name("or")

    XOR = hydra.core.Name("xor")

DisjunctiveOperator.TYPE_ = hydra.core.Name("openGql.grammar.DisjunctiveOperator")

class ValueFunctionNumeric(Node["NumericValueFunction"]):
    ...

class ValueFunctionDatetimeSubtraction(Node["DatetimeSubtraction"]):
    ...

class ValueFunctionDatetime(Node["DatetimeValueFunction"]):
    ...

class ValueFunctionDuration(Node["DurationValueFunction"]):
    ...

class ValueFunctionCharacterOrByteString(Node["CharacterOrByteStringFunction"]):
    ...

class ValueFunctionList(Node["ListValueFunction"]):
    ...

class _ValueFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class ValueFunction(metaclass=_ValueFunctionMeta):
    r"""ValueFunctionNumeric | ValueFunctionDatetimeSubtraction | ValueFunctionDatetime | ValueFunctionDuration | ValueFunctionCharacterOrByteString | ValueFunctionList"""

    TYPE_ = hydra.core.Name("openGql.grammar.ValueFunction")
    NUMERIC = hydra.core.Name("numeric")
    DATETIME_SUBTRACTION = hydra.core.Name("datetimeSubtraction")
    DATETIME = hydra.core.Name("datetime")
    DURATION = hydra.core.Name("duration")
    CHARACTER_OR_BYTE_STRING = hydra.core.Name("characterOrByteString")
    LIST = hydra.core.Name("list")

BooleanValueExpression: TypeAlias = "ValueExpression"

class CharacterOrByteStringFunctionSub(Node["SubCharacterOrByteString"]):
    ...

class CharacterOrByteStringFunctionTrimSingle(Node["TrimSingleCharacterOrByteString"]):
    ...

class CharacterOrByteStringFunctionFold(Node["FoldCharacterString"]):
    ...

class CharacterOrByteStringFunctionTrimMultiCharacter(Node["TrimMultiCharacterCharacterString"]):
    ...

class CharacterOrByteStringFunctionNormalize(Node["NormalizeCharacterString"]):
    ...

class _CharacterOrByteStringFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class CharacterOrByteStringFunction(metaclass=_CharacterOrByteStringFunctionMeta):
    r"""CharacterOrByteStringFunctionSub | CharacterOrByteStringFunctionTrimSingle | CharacterOrByteStringFunctionFold | CharacterOrByteStringFunctionTrimMultiCharacter | CharacterOrByteStringFunctionNormalize"""

    TYPE_ = hydra.core.Name("openGql.grammar.CharacterOrByteStringFunction")
    SUB = hydra.core.Name("sub")
    TRIM_SINGLE = hydra.core.Name("trimSingle")
    FOLD = hydra.core.Name("fold")
    TRIM_MULTI_CHARACTER = hydra.core.Name("trimMultiCharacter")
    NORMALIZE = hydra.core.Name("normalize")

@dataclass(frozen=True)
class SubCharacterOrByteString:
    side: Side
    value_expression: ValueExpression
    string_length: StringLength

    TYPE_ = hydra.core.Name("openGql.grammar.SubCharacterOrByteString")
    SIDE = hydra.core.Name("side")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    STRING_LENGTH = hydra.core.Name("stringLength")

class Side(Enum):
    LEFT = hydra.core.Name("left")

    RIGHT = hydra.core.Name("right")

Side.TYPE_ = hydra.core.Name("openGql.grammar.Side")

TrimSingleCharacterOrByteString: TypeAlias = "TrimOperands"

@dataclass(frozen=True)
class FoldCharacterString:
    case: Case
    value_expression: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.FoldCharacterString")
    CASE = hydra.core.Name("case")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")

class Case(Enum):
    UPPER = hydra.core.Name("upper")

    LOWER = hydra.core.Name("lower")

Case.TYPE_ = hydra.core.Name("openGql.grammar.Case")

@dataclass(frozen=True)
class TrimMultiCharacterCharacterString:
    trim_type: TrimType
    value_expression: ValueExpression
    optional_value_expression: Maybe[ValueExpression]

    TYPE_ = hydra.core.Name("openGql.grammar.TrimMultiCharacterCharacterString")
    TRIM_TYPE = hydra.core.Name("trimType")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    OPTIONAL_VALUE_EXPRESSION = hydra.core.Name("optionalValueExpression")

class TrimType(Enum):
    BTRIM = hydra.core.Name("btrim")

    LTRIM = hydra.core.Name("ltrim")

    RTRIM = hydra.core.Name("rtrim")

TrimType.TYPE_ = hydra.core.Name("openGql.grammar.TrimType")

@dataclass(frozen=True)
class NormalizeCharacterString:
    value_expression: ValueExpression
    normal_form: Maybe[NormalForm]

    TYPE_ = hydra.core.Name("openGql.grammar.NormalizeCharacterString")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    NORMAL_FORM = hydra.core.Name("normalForm")

NodeReferenceValueExpression: TypeAlias = "PrimaryValueExpression"

EdgeReferenceValueExpression: TypeAlias = "PrimaryValueExpression"

AggregatingValueExpression: TypeAlias = "ValueExpression"

class PrimaryValueExpressionParenthesized(Node["ParenthesizedValueExpression"]):
    ...

class PrimaryValueExpressionAggregateFunction(Node["AggregateFunction"]):
    ...

class PrimaryValueExpressionUnsignedValueSpecification(Node["UnsignedValueSpecification"]):
    ...

class PrimaryValueExpressionPathValueConstructor(Node["PathValueConstructor"]):
    ...

class PrimaryValueExpressionPropertyReference(Node["PropertyReference"]):
    ...

class PrimaryValueExpressionValueQueryExpression(Node["ValueQueryExpression"]):
    ...

class PrimaryValueExpressionCaseExpression(Node["CaseExpression"]):
    ...

class PrimaryValueExpressionCastSpecification(Node["CastSpecification"]):
    ...

class PrimaryValueExpressionElementIdFunction(Node["ElementIdFunction"]):
    ...

class PrimaryValueExpressionLetValueExpression(Node["LetValueExpression"]):
    ...

class PrimaryValueExpressionBindingVariableReference(Node["BindingVariableReference"]):
    ...

class _PrimaryValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class PrimaryValueExpression(metaclass=_PrimaryValueExpressionMeta):
    r"""PrimaryValueExpressionParenthesized | PrimaryValueExpressionAggregateFunction | PrimaryValueExpressionUnsignedValueSpecification | PrimaryValueExpressionPathValueConstructor | PrimaryValueExpressionPropertyReference | PrimaryValueExpressionValueQueryExpression | PrimaryValueExpressionCaseExpression | PrimaryValueExpressionCastSpecification | PrimaryValueExpressionElementIdFunction | PrimaryValueExpressionLetValueExpression | PrimaryValueExpressionBindingVariableReference"""

    TYPE_ = hydra.core.Name("openGql.grammar.PrimaryValueExpression")
    PARENTHESIZED = hydra.core.Name("parenthesized")
    AGGREGATE_FUNCTION = hydra.core.Name("aggregateFunction")
    UNSIGNED_VALUE_SPECIFICATION = hydra.core.Name("unsignedValueSpecification")
    PATH_VALUE_CONSTRUCTOR = hydra.core.Name("pathValueConstructor")
    PROPERTY_REFERENCE = hydra.core.Name("propertyReference")
    VALUE_QUERY_EXPRESSION = hydra.core.Name("valueQueryExpression")
    CASE_EXPRESSION = hydra.core.Name("caseExpression")
    CAST_SPECIFICATION = hydra.core.Name("castSpecification")
    ELEMENT_ID_FUNCTION = hydra.core.Name("elementIdFunction")
    LET_VALUE_EXPRESSION = hydra.core.Name("letValueExpression")
    BINDING_VARIABLE_REFERENCE = hydra.core.Name("bindingVariableReference")

ParenthesizedValueExpression: TypeAlias = "ValueExpression"

class NonParenthesizedPrimaryValueExpressionSpecial(Node["NonParenthesizedPrimaryValueExpressionSpecialCase"]):
    ...

class NonParenthesizedPrimaryValueExpressionBindingVariable(Node["BindingVariableReference"]):
    ...

class _NonParenthesizedPrimaryValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class NonParenthesizedPrimaryValueExpression(metaclass=_NonParenthesizedPrimaryValueExpressionMeta):
    r"""NonParenthesizedPrimaryValueExpressionSpecial | NonParenthesizedPrimaryValueExpressionBindingVariable"""

    TYPE_ = hydra.core.Name("openGql.grammar.NonParenthesizedPrimaryValueExpression")
    SPECIAL = hydra.core.Name("special")
    BINDING_VARIABLE = hydra.core.Name("bindingVariable")

class NonParenthesizedPrimaryValueExpressionSpecialCaseAggregateFunction(Node["AggregateFunction"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCaseUnsignedValueSpecification(Node["UnsignedValueSpecification"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCasePathValueConstructor(Node["PathValueConstructor"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCasePropertyReference(Node["PropertyReference"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCaseValueQueryExpression(Node["ValueQueryExpression"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCaseCaseExpression(Node["CaseExpression"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCaseCastSpecification(Node["CastSpecification"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCaseElementIdFunction(Node["ElementIdFunction"]):
    ...

class NonParenthesizedPrimaryValueExpressionSpecialCaseLetValueExpression(Node["LetValueExpression"]):
    ...

class _NonParenthesizedPrimaryValueExpressionSpecialCaseMeta(type):
    def __getitem__(cls, item):
        return object

class NonParenthesizedPrimaryValueExpressionSpecialCase(metaclass=_NonParenthesizedPrimaryValueExpressionSpecialCaseMeta):
    r"""NonParenthesizedPrimaryValueExpressionSpecialCaseAggregateFunction | NonParenthesizedPrimaryValueExpressionSpecialCaseUnsignedValueSpecification | NonParenthesizedPrimaryValueExpressionSpecialCasePathValueConstructor | NonParenthesizedPrimaryValueExpressionSpecialCasePropertyReference | NonParenthesizedPrimaryValueExpressionSpecialCaseValueQueryExpression | NonParenthesizedPrimaryValueExpressionSpecialCaseCaseExpression | NonParenthesizedPrimaryValueExpressionSpecialCaseCastSpecification | NonParenthesizedPrimaryValueExpressionSpecialCaseElementIdFunction | NonParenthesizedPrimaryValueExpressionSpecialCaseLetValueExpression"""

    TYPE_ = hydra.core.Name("openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase")
    AGGREGATE_FUNCTION = hydra.core.Name("aggregateFunction")
    UNSIGNED_VALUE_SPECIFICATION = hydra.core.Name("unsignedValueSpecification")
    PATH_VALUE_CONSTRUCTOR = hydra.core.Name("pathValueConstructor")
    PROPERTY_REFERENCE = hydra.core.Name("propertyReference")
    VALUE_QUERY_EXPRESSION = hydra.core.Name("valueQueryExpression")
    CASE_EXPRESSION = hydra.core.Name("caseExpression")
    CAST_SPECIFICATION = hydra.core.Name("castSpecification")
    ELEMENT_ID_FUNCTION = hydra.core.Name("elementIdFunction")
    LET_VALUE_EXPRESSION = hydra.core.Name("letValueExpression")

class UnsignedValueSpecificationUnsignedLiteral(Node["UnsignedLiteral"]):
    ...

class UnsignedValueSpecificationGeneralValueSpecification(Node["GeneralValueSpecification"]):
    ...

class _UnsignedValueSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedValueSpecification(metaclass=_UnsignedValueSpecificationMeta):
    r"""UnsignedValueSpecificationUnsignedLiteral | UnsignedValueSpecificationGeneralValueSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.UnsignedValueSpecification")
    UNSIGNED_LITERAL = hydra.core.Name("unsignedLiteral")
    GENERAL_VALUE_SPECIFICATION = hydra.core.Name("generalValueSpecification")

class NonNegativeIntegerSpecificationUnsignedInteger(Node["UnsignedInteger"]):
    ...

class NonNegativeIntegerSpecificationDynamicParameterSpecification(Node["DynamicParameterSpecification"]):
    ...

class _NonNegativeIntegerSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class NonNegativeIntegerSpecification(metaclass=_NonNegativeIntegerSpecificationMeta):
    r"""NonNegativeIntegerSpecificationUnsignedInteger | NonNegativeIntegerSpecificationDynamicParameterSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.NonNegativeIntegerSpecification")
    UNSIGNED_INTEGER = hydra.core.Name("unsignedInteger")
    DYNAMIC_PARAMETER_SPECIFICATION = hydra.core.Name("dynamicParameterSpecification")

class GeneralValueSpecificationDynamicParameterSpecification(Node["DynamicParameterSpecification"]):
    ...

class GeneralValueSpecificationSessionUser:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, GeneralValueSpecificationSessionUser)
    def __hash__(self):
        return hash("GeneralValueSpecificationSessionUser")

class _GeneralValueSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class GeneralValueSpecification(metaclass=_GeneralValueSpecificationMeta):
    r"""GeneralValueSpecificationDynamicParameterSpecification | GeneralValueSpecificationSessionUser"""

    TYPE_ = hydra.core.Name("openGql.grammar.GeneralValueSpecification")
    DYNAMIC_PARAMETER_SPECIFICATION = hydra.core.Name("dynamicParameterSpecification")
    SESSION_USER = hydra.core.Name("sessionUser")

DynamicParameterSpecification: TypeAlias = "ParameterName"

@dataclass(frozen=True)
class LetValueExpression:
    let_variables: LetVariableDefinitionList
    value_expression: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.LetValueExpression")
    LET_VARIABLES = hydra.core.Name("letVariables")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")

ValueQueryExpression: TypeAlias = "NestedQuerySpecification"

class CaseExpressionAbbreviation(Node["CaseAbbreviation"]):
    ...

class CaseExpressionSpecification(Node["CaseSpecification"]):
    ...

class _CaseExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class CaseExpression(metaclass=_CaseExpressionMeta):
    r"""CaseExpressionAbbreviation | CaseExpressionSpecification"""

    TYPE_ = hydra.core.Name("openGql.grammar.CaseExpression")
    ABBREVIATION = hydra.core.Name("abbreviation")
    SPECIFICATION = hydra.core.Name("specification")

class CaseAbbreviationNullIf(Node["NullIfAbbreviation"]):
    ...

class CaseAbbreviationCoalesce(Node["frozenlist[ValueExpression]"]):
    ...

class _CaseAbbreviationMeta(type):
    def __getitem__(cls, item):
        return object

class CaseAbbreviation(metaclass=_CaseAbbreviationMeta):
    r"""CaseAbbreviationNullIf | CaseAbbreviationCoalesce"""

    TYPE_ = hydra.core.Name("openGql.grammar.CaseAbbreviation")
    NULL_IF = hydra.core.Name("nullIf")
    COALESCE = hydra.core.Name("coalesce")

@dataclass(frozen=True)
class NullIfAbbreviation:
    first: ValueExpression
    second: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.NullIfAbbreviation")
    FIRST = hydra.core.Name("first")
    SECOND = hydra.core.Name("second")

class CaseSpecificationSimple(Node["SimpleCase"]):
    ...

class CaseSpecificationSearched(Node["SearchedCase"]):
    ...

class _CaseSpecificationMeta(type):
    def __getitem__(cls, item):
        return object

class CaseSpecification(metaclass=_CaseSpecificationMeta):
    r"""CaseSpecificationSimple | CaseSpecificationSearched"""

    TYPE_ = hydra.core.Name("openGql.grammar.CaseSpecification")
    SIMPLE = hydra.core.Name("simple")
    SEARCHED = hydra.core.Name("searched")

@dataclass(frozen=True)
class SimpleCase:
    case_operand: CaseOperand
    when_clauses: frozenlist[SimpleWhenClause]
    else_clause: Maybe[ElseClause]

    TYPE_ = hydra.core.Name("openGql.grammar.SimpleCase")
    CASE_OPERAND = hydra.core.Name("caseOperand")
    WHEN_CLAUSES = hydra.core.Name("whenClauses")
    ELSE_CLAUSE = hydra.core.Name("elseClause")

@dataclass(frozen=True)
class SearchedCase:
    when_clauses: frozenlist[SearchedWhenClause]
    else_clause: Maybe[ElseClause]

    TYPE_ = hydra.core.Name("openGql.grammar.SearchedCase")
    WHEN_CLAUSES = hydra.core.Name("whenClauses")
    ELSE_CLAUSE = hydra.core.Name("elseClause")

@dataclass(frozen=True)
class SimpleWhenClause:
    when_operands: WhenOperandList
    result: Result

    TYPE_ = hydra.core.Name("openGql.grammar.SimpleWhenClause")
    WHEN_OPERANDS = hydra.core.Name("whenOperands")
    RESULT = hydra.core.Name("result")

@dataclass(frozen=True)
class SearchedWhenClause:
    search_condition: SearchCondition
    result: Result

    TYPE_ = hydra.core.Name("openGql.grammar.SearchedWhenClause")
    SEARCH_CONDITION = hydra.core.Name("searchCondition")
    RESULT = hydra.core.Name("result")

ElseClause: TypeAlias = "Result"

class CaseOperandValueExpression(Node["NonParenthesizedPrimaryValueExpression"]):
    ...

class CaseOperandElementReference(Node["ElementVariableReference"]):
    ...

class _CaseOperandMeta(type):
    def __getitem__(cls, item):
        return object

class CaseOperand(metaclass=_CaseOperandMeta):
    r"""CaseOperandValueExpression | CaseOperandElementReference"""

    TYPE_ = hydra.core.Name("openGql.grammar.CaseOperand")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    ELEMENT_REFERENCE = hydra.core.Name("elementReference")

WhenOperandList: TypeAlias = "frozenlist[WhenOperand]"

class WhenOperandValueExpression(Node["NonParenthesizedPrimaryValueExpression"]):
    ...

class WhenOperandComparison(Node["ComparisonPredicatePart2"]):
    ...

class WhenOperandNullPredicate(Node["NullPredicatePart2"]):
    ...

class WhenOperandValueTypePredicate(Node["ValueTypePredicatePart2"]):
    ...

class WhenOperandNormalizedPredicate(Node["NormalizedPredicatePart2"]):
    ...

class WhenOperandDirectedPredicate(Node["DirectedPredicatePart2"]):
    ...

class WhenOperandLabeledPredicate(Node["LabeledPredicatePart2"]):
    ...

class WhenOperandSourcePredicate(Node["SourcePredicate"]):
    ...

class WhenOperandDestinationPredicate(Node["DestinationPredicate"]):
    ...

class _WhenOperandMeta(type):
    def __getitem__(cls, item):
        return object

class WhenOperand(metaclass=_WhenOperandMeta):
    r"""WhenOperandValueExpression | WhenOperandComparison | WhenOperandNullPredicate | WhenOperandValueTypePredicate | WhenOperandNormalizedPredicate | WhenOperandDirectedPredicate | WhenOperandLabeledPredicate | WhenOperandSourcePredicate | WhenOperandDestinationPredicate"""

    TYPE_ = hydra.core.Name("openGql.grammar.WhenOperand")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    COMPARISON = hydra.core.Name("comparison")
    NULL_PREDICATE = hydra.core.Name("nullPredicate")
    VALUE_TYPE_PREDICATE = hydra.core.Name("valueTypePredicate")
    NORMALIZED_PREDICATE = hydra.core.Name("normalizedPredicate")
    DIRECTED_PREDICATE = hydra.core.Name("directedPredicate")
    LABELED_PREDICATE = hydra.core.Name("labeledPredicate")
    SOURCE_PREDICATE = hydra.core.Name("sourcePredicate")
    DESTINATION_PREDICATE = hydra.core.Name("destinationPredicate")

class ResultSimple(Node["ResultExpression"]):
    ...

class ResultNullLiteral:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, ResultNullLiteral)
    def __hash__(self):
        return hash("ResultNullLiteral")

class _ResultMeta(type):
    def __getitem__(cls, item):
        return object

class Result(metaclass=_ResultMeta):
    r"""ResultSimple | ResultNullLiteral"""

    TYPE_ = hydra.core.Name("openGql.grammar.Result")
    SIMPLE = hydra.core.Name("simple")
    NULL_LITERAL = hydra.core.Name("nullLiteral")

ResultExpression: TypeAlias = "ValueExpression"

@dataclass(frozen=True)
class CastSpecification:
    operand: CastOperand
    target: CastTarget

    TYPE_ = hydra.core.Name("openGql.grammar.CastSpecification")
    OPERAND = hydra.core.Name("operand")
    TARGET = hydra.core.Name("target")

class CastOperandValueExpression(Node["ValueExpression"]):
    ...

class CastOperandNullLiteral:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, CastOperandNullLiteral)
    def __hash__(self):
        return hash("CastOperandNullLiteral")

class _CastOperandMeta(type):
    def __getitem__(cls, item):
        return object

class CastOperand(metaclass=_CastOperandMeta):
    r"""CastOperandValueExpression | CastOperandNullLiteral"""

    TYPE_ = hydra.core.Name("openGql.grammar.CastOperand")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    NULL_LITERAL = hydra.core.Name("nullLiteral")

CastTarget: TypeAlias = "ValueType"

class AggregateFunctionCountAll:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, AggregateFunctionCountAll)
    def __hash__(self):
        return hash("AggregateFunctionCountAll")

class AggregateFunctionGeneralSetFunction(Node["GeneralSetFunction"]):
    ...

class AggregateFunctionBinarySetFunction(Node["BinarySetFunction"]):
    ...

class _AggregateFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class AggregateFunction(metaclass=_AggregateFunctionMeta):
    r"""AggregateFunctionCountAll | AggregateFunctionGeneralSetFunction | AggregateFunctionBinarySetFunction"""

    TYPE_ = hydra.core.Name("openGql.grammar.AggregateFunction")
    COUNT_ALL = hydra.core.Name("countAll")
    GENERAL_SET_FUNCTION = hydra.core.Name("generalSetFunction")
    BINARY_SET_FUNCTION = hydra.core.Name("binarySetFunction")

@dataclass(frozen=True)
class GeneralSetFunction:
    function_type: GeneralSetFunctionType
    set_quantifier: Maybe[SetQuantifier]
    value_expression: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.GeneralSetFunction")
    FUNCTION_TYPE = hydra.core.Name("functionType")
    SET_QUANTIFIER = hydra.core.Name("setQuantifier")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")

@dataclass(frozen=True)
class BinarySetFunction:
    function_type: BinarySetFunctionType
    dependent_value: DependentValueExpression
    independent_value: IndependentValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.BinarySetFunction")
    FUNCTION_TYPE = hydra.core.Name("functionType")
    DEPENDENT_VALUE = hydra.core.Name("dependentValue")
    INDEPENDENT_VALUE = hydra.core.Name("independentValue")

class GeneralSetFunctionType(Enum):
    AVG = hydra.core.Name("avg")

    COUNT = hydra.core.Name("count")

    MAX = hydra.core.Name("max")

    MIN = hydra.core.Name("min")

    SUM = hydra.core.Name("sum")

    COLLECT_LIST = hydra.core.Name("collectList")

    STDDEV_SAMP = hydra.core.Name("stddevSamp")

    STDDEV_POP = hydra.core.Name("stddevPop")

GeneralSetFunctionType.TYPE_ = hydra.core.Name("openGql.grammar.GeneralSetFunctionType")

class SetQuantifier(Enum):
    DISTINCT = hydra.core.Name("distinct")

    ALL = hydra.core.Name("all")

SetQuantifier.TYPE_ = hydra.core.Name("openGql.grammar.SetQuantifier")

class BinarySetFunctionType(Enum):
    PERCENTILE_CONT = hydra.core.Name("percentileCont")

    PERCENTILE_DISC = hydra.core.Name("percentileDisc")

BinarySetFunctionType.TYPE_ = hydra.core.Name("openGql.grammar.BinarySetFunctionType")

@dataclass(frozen=True)
class DependentValueExpression:
    set_quantifier: Maybe[SetQuantifier]
    numeric_value: NumericValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.DependentValueExpression")
    SET_QUANTIFIER = hydra.core.Name("setQuantifier")
    NUMERIC_VALUE = hydra.core.Name("numericValue")

IndependentValueExpression: TypeAlias = "NumericValueExpression"

ElementIdFunction: TypeAlias = "ElementVariableReference"

@dataclass(frozen=True)
class PropertyReference:
    value_expression: PrimaryValueExpression
    property_name: PropertyName

    TYPE_ = hydra.core.Name("openGql.grammar.PropertyReference")
    VALUE_EXPRESSION = hydra.core.Name("valueExpression")
    PROPERTY_NAME = hydra.core.Name("propertyName")

BindingVariableReference: TypeAlias = "BindingVariable"

PathValueExpression: TypeAlias = "ValueExpression"

PathValueConstructor: TypeAlias = "PathValueConstructorByEnumeration"

PathValueConstructorByEnumeration: TypeAlias = "PathElementList"

@dataclass(frozen=True)
class PathElementList:
    start: PathElementListStart
    steps: frozenlist[PathElementListStep]

    TYPE_ = hydra.core.Name("openGql.grammar.PathElementList")
    START = hydra.core.Name("start")
    STEPS = hydra.core.Name("steps")

PathElementListStart: TypeAlias = "NodeReferenceValueExpression"

@dataclass(frozen=True)
class PathElementListStep:
    edge_reference: EdgeReferenceValueExpression
    node_reference: NodeReferenceValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.PathElementListStep")
    EDGE_REFERENCE = hydra.core.Name("edgeReference")
    NODE_REFERENCE = hydra.core.Name("nodeReference")

ListValueExpression: TypeAlias = "ValueExpression"

class ListValueFunctionTrim(Node["TrimListFunction"]):
    ...

class ListValueFunctionElements(Node["ElementsFunction"]):
    ...

class _ListValueFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class ListValueFunction(metaclass=_ListValueFunctionMeta):
    r"""ListValueFunctionTrim | ListValueFunctionElements"""

    TYPE_ = hydra.core.Name("openGql.grammar.ListValueFunction")
    TRIM = hydra.core.Name("trim")
    ELEMENTS = hydra.core.Name("elements")

@dataclass(frozen=True)
class TrimListFunction:
    list_value: ListValueExpression
    numeric_value: NumericValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.TrimListFunction")
    LIST_VALUE = hydra.core.Name("listValue")
    NUMERIC_VALUE = hydra.core.Name("numericValue")

ElementsFunction: TypeAlias = "PathValueExpression"

ListValueConstructor: TypeAlias = "ListValueConstructorByEnumeration"

@dataclass(frozen=True)
class ListValueConstructorByEnumeration:
    list_value_type_name: Maybe[ListValueTypeName]
    elements: Maybe[ListElementList]

    TYPE_ = hydra.core.Name("openGql.grammar.ListValueConstructorByEnumeration")
    LIST_VALUE_TYPE_NAME = hydra.core.Name("listValueTypeName")
    ELEMENTS = hydra.core.Name("elements")

ListElementList: TypeAlias = "frozenlist[ListElement]"

ListElement: TypeAlias = "ValueExpression"

RecordConstructor: TypeAlias = "FieldsSpecification"

FieldsSpecification: TypeAlias = "Maybe[FieldList]"

FieldList: TypeAlias = "frozenlist[Field]"

@dataclass(frozen=True)
class Field:
    name: FieldName
    value: ValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.Field")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

TruthValue: TypeAlias = "BooleanLiteral"

class NumericValueExpressionSigned(Node["SignedNumericValueExpression"]):
    ...

class NumericValueExpressionMultiplicationOrDivision(Node["MulDivNumericValueExpression"]):
    ...

class NumericValueExpressionAdditionOrSubtraction(Node["AddSubNumericValueExpression"]):
    ...

class NumericValueExpressionPrimary(Node["PrimaryValueExpression"]):
    ...

class NumericValueExpressionFunction(Node["NumericValueFunction"]):
    ...

class _NumericValueExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class NumericValueExpression(metaclass=_NumericValueExpressionMeta):
    r"""NumericValueExpressionSigned | NumericValueExpressionMultiplicationOrDivision | NumericValueExpressionAdditionOrSubtraction | NumericValueExpressionPrimary | NumericValueExpressionFunction"""

    TYPE_ = hydra.core.Name("openGql.grammar.NumericValueExpression")
    SIGNED = hydra.core.Name("signed")
    MULTIPLICATION_OR_DIVISION = hydra.core.Name("multiplicationOrDivision")
    ADDITION_OR_SUBTRACTION = hydra.core.Name("additionOrSubtraction")
    PRIMARY = hydra.core.Name("primary")
    FUNCTION = hydra.core.Name("function")

@dataclass(frozen=True)
class SignedNumericValueExpression:
    sign: Sign
    expression: NumericValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.SignedNumericValueExpression")
    SIGN = hydra.core.Name("sign")
    EXPRESSION = hydra.core.Name("expression")

@dataclass(frozen=True)
class MulDivNumericValueExpression:
    left: NumericValueExpression
    operator: MultDivOperator
    right: NumericValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.MulDivNumericValueExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class AddSubNumericValueExpression:
    left: NumericValueExpression
    operator: AddSubtractOperator
    right: NumericValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.AddSubNumericValueExpression")
    LEFT = hydra.core.Name("left")
    OPERATOR = hydra.core.Name("operator")
    RIGHT = hydra.core.Name("right")

class NumericValueFunctionLength(Node["LengthExpression"]):
    ...

class NumericValueFunctionCardinality(Node["CardinalityExpression"]):
    ...

class NumericValueFunctionAbsoluteValue(Node["AbsoluteValueExpression"]):
    ...

class NumericValueFunctionModulus(Node["ModulusExpression"]):
    ...

class NumericValueFunctionTrigonometric(Node["TrigonometricFunction"]):
    ...

class NumericValueFunctionLogarithm(Node["GeneralLogarithmFunction"]):
    ...

class NumericValueFunctionCommonLogarithm(Node["CommonLogarithm"]):
    ...

class NumericValueFunctionNaturalLogarithm(Node["NaturalLogarithm"]):
    ...

class NumericValueFunctionExponential(Node["ExponentialFunction"]):
    ...

class NumericValueFunctionPower(Node["PowerFunction"]):
    ...

class NumericValueFunctionSquareRoot(Node["SquareRoot"]):
    ...

class NumericValueFunctionFloor(Node["FloorFunction"]):
    ...

class NumericValueFunctionCeiling(Node["CeilingFunction"]):
    ...

class _NumericValueFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class NumericValueFunction(metaclass=_NumericValueFunctionMeta):
    r"""NumericValueFunctionLength | NumericValueFunctionCardinality | NumericValueFunctionAbsoluteValue | NumericValueFunctionModulus | NumericValueFunctionTrigonometric | NumericValueFunctionLogarithm | NumericValueFunctionCommonLogarithm | NumericValueFunctionNaturalLogarithm | NumericValueFunctionExponential | NumericValueFunctionPower | NumericValueFunctionSquareRoot | NumericValueFunctionFloor | NumericValueFunctionCeiling"""

    TYPE_ = hydra.core.Name("openGql.grammar.NumericValueFunction")
    LENGTH = hydra.core.Name("length")
    CARDINALITY = hydra.core.Name("cardinality")
    ABSOLUTE_VALUE = hydra.core.Name("absoluteValue")
    MODULUS = hydra.core.Name("modulus")
    TRIGONOMETRIC = hydra.core.Name("trigonometric")
    LOGARITHM = hydra.core.Name("logarithm")
    COMMON_LOGARITHM = hydra.core.Name("commonLogarithm")
    NATURAL_LOGARITHM = hydra.core.Name("naturalLogarithm")
    EXPONENTIAL = hydra.core.Name("exponential")
    POWER = hydra.core.Name("power")
    SQUARE_ROOT = hydra.core.Name("squareRoot")
    FLOOR = hydra.core.Name("floor")
    CEILING = hydra.core.Name("ceiling")

class LengthExpressionChar(Node["CharLengthExpression"]):
    ...

class LengthExpressionByte(Node["ByteLengthExpression"]):
    ...

class LengthExpressionPath(Node["PathLengthExpression"]):
    ...

class _LengthExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class LengthExpression(metaclass=_LengthExpressionMeta):
    r"""LengthExpressionChar | LengthExpressionByte | LengthExpressionPath"""

    TYPE_ = hydra.core.Name("openGql.grammar.LengthExpression")
    CHAR = hydra.core.Name("char")
    BYTE = hydra.core.Name("byte")
    PATH = hydra.core.Name("path")

class CardinalityExpressionCardinality(Node["CardinalityArgumentExpression"]):
    ...

class CardinalityExpressionSize(Node["ListValueExpression"]):
    ...

class _CardinalityExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class CardinalityExpression(metaclass=_CardinalityExpressionMeta):
    r"""CardinalityExpressionCardinality | CardinalityExpressionSize"""

    TYPE_ = hydra.core.Name("openGql.grammar.CardinalityExpression")
    CARDINALITY = hydra.core.Name("cardinality")
    SIZE = hydra.core.Name("size")

CardinalityArgumentExpression: TypeAlias = "ValueExpression"

CharLengthExpression: TypeAlias = "CharacterStringValueExpression"

ByteLengthExpression: TypeAlias = "ByteStringValueExpression"

PathLengthExpression: TypeAlias = "PathValueExpression"

AbsoluteValueExpression: TypeAlias = "ValueExpression"

@dataclass(frozen=True)
class ModulusExpression:
    dividend: NumericValueExpressionDividend
    divisor: NumericValueExpressionDivisor

    TYPE_ = hydra.core.Name("openGql.grammar.ModulusExpression")
    DIVIDEND = hydra.core.Name("dividend")
    DIVISOR = hydra.core.Name("divisor")

NumericValueExpressionDividend: TypeAlias = "NumericValueExpression"

NumericValueExpressionDivisor: TypeAlias = "NumericValueExpression"

@dataclass(frozen=True)
class TrigonometricFunction:
    name: TrigonometricFunctionName
    value: NumericValueExpression

    TYPE_ = hydra.core.Name("openGql.grammar.TrigonometricFunction")
    NAME = hydra.core.Name("name")
    VALUE = hydra.core.Name("value")

class TrigonometricFunctionName(Enum):
    SIN = hydra.core.Name("sin")

    COS = hydra.core.Name("cos")

    TAN = hydra.core.Name("tan")

    COT = hydra.core.Name("cot")

    SINH = hydra.core.Name("sinh")

    COSH = hydra.core.Name("cosh")

    TANH = hydra.core.Name("tanh")

    ASIN = hydra.core.Name("asin")

    ACOS = hydra.core.Name("acos")

    ATAN = hydra.core.Name("atan")

    DEGREES = hydra.core.Name("degrees")

    RADIANS = hydra.core.Name("radians")

TrigonometricFunctionName.TYPE_ = hydra.core.Name("openGql.grammar.TrigonometricFunctionName")

@dataclass(frozen=True)
class GeneralLogarithmFunction:
    base: GeneralLogarithmBase
    argument: GeneralLogarithmArgument

    TYPE_ = hydra.core.Name("openGql.grammar.GeneralLogarithmFunction")
    BASE = hydra.core.Name("base")
    ARGUMENT = hydra.core.Name("argument")

GeneralLogarithmBase: TypeAlias = "NumericValueExpression"

GeneralLogarithmArgument: TypeAlias = "NumericValueExpression"

CommonLogarithm: TypeAlias = "NumericValueExpression"

NaturalLogarithm: TypeAlias = "NumericValueExpression"

ExponentialFunction: TypeAlias = "NumericValueExpression"

@dataclass(frozen=True)
class PowerFunction:
    base: NumericValueExpressionBase
    exponent: NumericValueExpressionExponent

    TYPE_ = hydra.core.Name("openGql.grammar.PowerFunction")
    BASE = hydra.core.Name("base")
    EXPONENT = hydra.core.Name("exponent")

NumericValueExpressionBase: TypeAlias = "NumericValueExpression"

NumericValueExpressionExponent: TypeAlias = "NumericValueExpression"

SquareRoot: TypeAlias = "NumericValueExpression"

FloorFunction: TypeAlias = "NumericValueExpression"

CeilingFunction: TypeAlias = "NumericValueExpression"

CharacterStringValueExpression: TypeAlias = "ValueExpression"

ByteStringValueExpression: TypeAlias = "ValueExpression"

@dataclass(frozen=True)
class TrimOperands:
    specification: Maybe[TrimSpecification]
    character_or_byte_string: Maybe[TrimCharacterOrByteString]
    source: TrimCharacterOrByteStringSource

    TYPE_ = hydra.core.Name("openGql.grammar.TrimOperands")
    SPECIFICATION = hydra.core.Name("specification")
    CHARACTER_OR_BYTE_STRING = hydra.core.Name("characterOrByteString")
    SOURCE = hydra.core.Name("source")

TrimCharacterOrByteStringSource: TypeAlias = "ValueExpression"

class TrimSpecification(Enum):
    LEADING = hydra.core.Name("leading")

    TRAILING = hydra.core.Name("trailing")

    BOTH = hydra.core.Name("both")

TrimSpecification.TYPE_ = hydra.core.Name("openGql.grammar.TrimSpecification")

TrimCharacterOrByteString: TypeAlias = "ValueExpression"

class NormalForm(Enum):
    NFC = hydra.core.Name("nfc")

    NFD = hydra.core.Name("nfd")

    NFKC = hydra.core.Name("nfkc")

    NFKD = hydra.core.Name("nfkd")

NormalForm.TYPE_ = hydra.core.Name("openGql.grammar.NormalForm")

StringLength: TypeAlias = "NumericValueExpression"

DatetimeValueExpression: TypeAlias = "ValueExpression"

class DatetimeValueFunctionDateFunction(Node["DateFunction"]):
    ...

class DatetimeValueFunctionTimeFunction(Node["TimeFunction"]):
    ...

class DatetimeValueFunctionDatetimeFunction(Node["DatetimeFunction"]):
    ...

class DatetimeValueFunctionLocaltimeFunction(Node["LocaltimeFunction"]):
    ...

class DatetimeValueFunctionLocaldatetimeFunction(Node["LocaldatetimeFunction"]):
    ...

class _DatetimeValueFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class DatetimeValueFunction(metaclass=_DatetimeValueFunctionMeta):
    r"""DatetimeValueFunctionDateFunction | DatetimeValueFunctionTimeFunction | DatetimeValueFunctionDatetimeFunction | DatetimeValueFunctionLocaltimeFunction | DatetimeValueFunctionLocaldatetimeFunction"""

    TYPE_ = hydra.core.Name("openGql.grammar.DatetimeValueFunction")
    DATE_FUNCTION = hydra.core.Name("dateFunction")
    TIME_FUNCTION = hydra.core.Name("timeFunction")
    DATETIME_FUNCTION = hydra.core.Name("datetimeFunction")
    LOCALTIME_FUNCTION = hydra.core.Name("localtimeFunction")
    LOCALDATETIME_FUNCTION = hydra.core.Name("localdatetimeFunction")

class DateFunctionCurrentDate:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, DateFunctionCurrentDate)
    def __hash__(self):
        return hash("DateFunctionCurrentDate")

class DateFunctionDateWithParams(Node["Maybe[DateFunctionParameters]"]):
    ...

class _DateFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class DateFunction(metaclass=_DateFunctionMeta):
    r"""DateFunctionCurrentDate | DateFunctionDateWithParams"""

    TYPE_ = hydra.core.Name("openGql.grammar.DateFunction")
    CURRENT_DATE = hydra.core.Name("currentDate")
    DATE_WITH_PARAMS = hydra.core.Name("dateWithParams")

class TimeFunctionCurrentTime:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, TimeFunctionCurrentTime)
    def __hash__(self):
        return hash("TimeFunctionCurrentTime")

class TimeFunctionZonedTimeWithParams(Node["Maybe[TimeFunctionParameters]"]):
    ...

class _TimeFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class TimeFunction(metaclass=_TimeFunctionMeta):
    r"""TimeFunctionCurrentTime | TimeFunctionZonedTimeWithParams"""

    TYPE_ = hydra.core.Name("openGql.grammar.TimeFunction")
    CURRENT_TIME = hydra.core.Name("currentTime")
    ZONED_TIME_WITH_PARAMS = hydra.core.Name("zonedTimeWithParams")

LocaltimeFunction: TypeAlias = "Maybe[TimeFunctionParameters]"

class DatetimeFunctionCurrentTimestamp:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, DatetimeFunctionCurrentTimestamp)
    def __hash__(self):
        return hash("DatetimeFunctionCurrentTimestamp")

class DatetimeFunctionZonedDatetimeWithParams(Node["Maybe[DatetimeFunctionParameters]"]):
    ...

class _DatetimeFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class DatetimeFunction(metaclass=_DatetimeFunctionMeta):
    r"""DatetimeFunctionCurrentTimestamp | DatetimeFunctionZonedDatetimeWithParams"""

    TYPE_ = hydra.core.Name("openGql.grammar.DatetimeFunction")
    CURRENT_TIMESTAMP = hydra.core.Name("currentTimestamp")
    ZONED_DATETIME_WITH_PARAMS = hydra.core.Name("zonedDatetimeWithParams")

class LocaldatetimeFunctionLocalTimestamp:
    __slots__ = ()
    def __eq__(self, other):
        return isinstance(other, LocaldatetimeFunctionLocalTimestamp)
    def __hash__(self):
        return hash("LocaldatetimeFunctionLocalTimestamp")

class LocaldatetimeFunctionLocalDatetimeWithParams(Node["Maybe[DatetimeFunctionParameters]"]):
    ...

class _LocaldatetimeFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class LocaldatetimeFunction(metaclass=_LocaldatetimeFunctionMeta):
    r"""LocaldatetimeFunctionLocalTimestamp | LocaldatetimeFunctionLocalDatetimeWithParams"""

    TYPE_ = hydra.core.Name("openGql.grammar.LocaldatetimeFunction")
    LOCAL_TIMESTAMP = hydra.core.Name("localTimestamp")
    LOCAL_DATETIME_WITH_PARAMS = hydra.core.Name("localDatetimeWithParams")

class DateFunctionParametersDateString(Node["DateString"]):
    ...

class DateFunctionParametersRecordConstructor(Node["RecordConstructor"]):
    ...

class _DateFunctionParametersMeta(type):
    def __getitem__(cls, item):
        return object

class DateFunctionParameters(metaclass=_DateFunctionParametersMeta):
    r"""DateFunctionParametersDateString | DateFunctionParametersRecordConstructor"""

    TYPE_ = hydra.core.Name("openGql.grammar.DateFunctionParameters")
    DATE_STRING = hydra.core.Name("dateString")
    RECORD_CONSTRUCTOR = hydra.core.Name("recordConstructor")

class TimeFunctionParametersTimeString(Node["TimeString"]):
    ...

class TimeFunctionParametersRecordConstructor(Node["RecordConstructor"]):
    ...

class _TimeFunctionParametersMeta(type):
    def __getitem__(cls, item):
        return object

class TimeFunctionParameters(metaclass=_TimeFunctionParametersMeta):
    r"""TimeFunctionParametersTimeString | TimeFunctionParametersRecordConstructor"""

    TYPE_ = hydra.core.Name("openGql.grammar.TimeFunctionParameters")
    TIME_STRING = hydra.core.Name("timeString")
    RECORD_CONSTRUCTOR = hydra.core.Name("recordConstructor")

class DatetimeFunctionParametersDatetimeString(Node["DatetimeString"]):
    ...

class DatetimeFunctionParametersRecordConstructor(Node["RecordConstructor"]):
    ...

class _DatetimeFunctionParametersMeta(type):
    def __getitem__(cls, item):
        return object

class DatetimeFunctionParameters(metaclass=_DatetimeFunctionParametersMeta):
    r"""DatetimeFunctionParametersDatetimeString | DatetimeFunctionParametersRecordConstructor"""

    TYPE_ = hydra.core.Name("openGql.grammar.DatetimeFunctionParameters")
    DATETIME_STRING = hydra.core.Name("datetimeString")
    RECORD_CONSTRUCTOR = hydra.core.Name("recordConstructor")

DurationValueExpression: TypeAlias = "ValueExpression"

@dataclass(frozen=True)
class DatetimeSubtraction:
    parameters: DatetimeSubtractionParameters
    temporal_duration_qualifier: Maybe[TemporalDurationQualifier]

    TYPE_ = hydra.core.Name("openGql.grammar.DatetimeSubtraction")
    PARAMETERS = hydra.core.Name("parameters")
    TEMPORAL_DURATION_QUALIFIER = hydra.core.Name("temporalDurationQualifier")

@dataclass(frozen=True)
class DatetimeSubtractionParameters:
    expression1: DatetimeValueExpression1
    expression2: DatetimeValueExpression2

    TYPE_ = hydra.core.Name("openGql.grammar.DatetimeSubtractionParameters")
    EXPRESSION1 = hydra.core.Name("expression1")
    EXPRESSION2 = hydra.core.Name("expression2")

DatetimeValueExpression1: TypeAlias = "DatetimeValueExpression"

DatetimeValueExpression2: TypeAlias = "DatetimeValueExpression"

class DurationValueFunctionDurationFunction(Node["DurationFunction"]):
    ...

class DurationValueFunctionAbsoluteValue(Node["AbsoluteValueExpression"]):
    ...

class _DurationValueFunctionMeta(type):
    def __getitem__(cls, item):
        return object

class DurationValueFunction(metaclass=_DurationValueFunctionMeta):
    r"""DurationValueFunctionDurationFunction | DurationValueFunctionAbsoluteValue"""

    TYPE_ = hydra.core.Name("openGql.grammar.DurationValueFunction")
    DURATION_FUNCTION = hydra.core.Name("durationFunction")
    ABSOLUTE_VALUE = hydra.core.Name("absoluteValue")

DurationFunction: TypeAlias = "DurationFunctionParameters"

class DurationFunctionParametersDurationString(Node["DurationString"]):
    ...

class DurationFunctionParametersRecordConstructor(Node["RecordConstructor"]):
    ...

class _DurationFunctionParametersMeta(type):
    def __getitem__(cls, item):
        return object

class DurationFunctionParameters(metaclass=_DurationFunctionParametersMeta):
    r"""DurationFunctionParametersDurationString | DurationFunctionParametersRecordConstructor"""

    TYPE_ = hydra.core.Name("openGql.grammar.DurationFunctionParameters")
    DURATION_STRING = hydra.core.Name("durationString")
    RECORD_CONSTRUCTOR = hydra.core.Name("recordConstructor")

ObjectName: TypeAlias = "str"

ObjectNameOrBindingVariable: TypeAlias = "str"

DirectoryName: TypeAlias = "str"

SchemaName: TypeAlias = "str"

GraphName: TypeAlias = "str"

DelimitedGraphName: TypeAlias = "str"

GraphTypeName: TypeAlias = "str"

NodeTypeName: TypeAlias = "str"

EdgeTypeName: TypeAlias = "str"

class BindingTableNameRegularIdentifier(Node[str]):
    ...

class BindingTableNameDelimitedBindingTableName(Node["DelimitedBindingTableName"]):
    ...

class _BindingTableNameMeta(type):
    def __getitem__(cls, item):
        return object

class BindingTableName(metaclass=_BindingTableNameMeta):
    r"""BindingTableNameRegularIdentifier | BindingTableNameDelimitedBindingTableName"""

    TYPE_ = hydra.core.Name("openGql.grammar.BindingTableName")
    REGULAR_IDENTIFIER = hydra.core.Name("regularIdentifier")
    DELIMITED_BINDING_TABLE_NAME = hydra.core.Name("delimitedBindingTableName")

DelimitedBindingTableName: TypeAlias = "str"

ProcedureName: TypeAlias = "str"

LabelName: TypeAlias = "str"

PropertyName: TypeAlias = "str"

FieldName: TypeAlias = "str"

ElementVariable: TypeAlias = "BindingVariable"

PathVariable: TypeAlias = "BindingVariable"

SubpathVariable: TypeAlias = "str"

BindingVariable: TypeAlias = "str"

class UnsignedLiteralNumeric(Node["UnsignedNumericLiteral"]):
    ...

class UnsignedLiteralGeneral(Node["GeneralLiteral"]):
    ...

class _UnsignedLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedLiteral(metaclass=_UnsignedLiteralMeta):
    r"""UnsignedLiteralNumeric | UnsignedLiteralGeneral"""

    TYPE_ = hydra.core.Name("openGql.grammar.UnsignedLiteral")
    NUMERIC = hydra.core.Name("numeric")
    GENERAL = hydra.core.Name("general")

class GeneralLiteralBoolean(Node["BooleanLiteral"]):
    ...

class GeneralLiteralCharacterString(Node["CharacterStringLiteral"]):
    ...

class GeneralLiteralByteString(Node["ByteStringLiteral"]):
    ...

class GeneralLiteralTemporal(Node["TemporalLiteral"]):
    ...

class GeneralLiteralDuration(Node["DurationLiteral"]):
    ...

class GeneralLiteralNullLiteral(Node["NullLiteral"]):
    ...

class GeneralLiteralList(Node["ListLiteral"]):
    ...

class GeneralLiteralRecord(Node["RecordLiteral"]):
    ...

class _GeneralLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class GeneralLiteral(metaclass=_GeneralLiteralMeta):
    r"""GeneralLiteralBoolean | GeneralLiteralCharacterString | GeneralLiteralByteString | GeneralLiteralTemporal | GeneralLiteralDuration | GeneralLiteralNullLiteral | GeneralLiteralList | GeneralLiteralRecord"""

    TYPE_ = hydra.core.Name("openGql.grammar.GeneralLiteral")
    BOOLEAN = hydra.core.Name("boolean")
    CHARACTER_STRING = hydra.core.Name("characterString")
    BYTE_STRING = hydra.core.Name("byteString")
    TEMPORAL = hydra.core.Name("temporal")
    DURATION = hydra.core.Name("duration")
    NULL_LITERAL = hydra.core.Name("nullLiteral")
    LIST = hydra.core.Name("list")
    RECORD = hydra.core.Name("record")

class TemporalLiteralDate(Node["DateLiteral"]):
    ...

class TemporalLiteralTime(Node["TimeLiteral"]):
    ...

class TemporalLiteralDatetime(Node["DatetimeLiteral"]):
    ...

class _TemporalLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class TemporalLiteral(metaclass=_TemporalLiteralMeta):
    r"""TemporalLiteralDate | TemporalLiteralTime | TemporalLiteralDatetime"""

    TYPE_ = hydra.core.Name("openGql.grammar.TemporalLiteral")
    DATE = hydra.core.Name("date")
    TIME = hydra.core.Name("time")
    DATETIME = hydra.core.Name("datetime")

DateLiteral: TypeAlias = "DateString"

TimeLiteral: TypeAlias = "TimeString"

DatetimeLiteral: TypeAlias = "DatetimeString"

ListLiteral: TypeAlias = "ListValueConstructorByEnumeration"

RecordLiteral: TypeAlias = "RecordConstructor"

Identifier: TypeAlias = "str"

RegularIdentifier: TypeAlias = "str"

TimeZoneString: TypeAlias = "CharacterStringLiteral"

CharacterStringLiteral: TypeAlias = "str"

class UnsignedNumericLiteralExact(Node["ExactNumericLiteral"]):
    ...

class UnsignedNumericLiteralApproximate(Node["ApproximateNumericLiteral"]):
    ...

class _UnsignedNumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedNumericLiteral(metaclass=_UnsignedNumericLiteralMeta):
    r"""UnsignedNumericLiteralExact | UnsignedNumericLiteralApproximate"""

    TYPE_ = hydra.core.Name("openGql.grammar.UnsignedNumericLiteral")
    EXACT = hydra.core.Name("exact")
    APPROXIMATE = hydra.core.Name("approximate")

class ExactNumericLiteralScientificWithSuffix(Node[str]):
    ...

class ExactNumericLiteralCommonWithSuffix(Node[str]):
    ...

class ExactNumericLiteralCommonWithoutSuffix(Node[str]):
    ...

class ExactNumericLiteralIntegerWithSuffix(Node[str]):
    ...

class ExactNumericLiteralUnsignedInteger(Node["UnsignedInteger"]):
    ...

class _ExactNumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class ExactNumericLiteral(metaclass=_ExactNumericLiteralMeta):
    r"""ExactNumericLiteralScientificWithSuffix | ExactNumericLiteralCommonWithSuffix | ExactNumericLiteralCommonWithoutSuffix | ExactNumericLiteralIntegerWithSuffix | ExactNumericLiteralUnsignedInteger"""

    TYPE_ = hydra.core.Name("openGql.grammar.ExactNumericLiteral")
    SCIENTIFIC_WITH_SUFFIX = hydra.core.Name("scientificWithSuffix")
    COMMON_WITH_SUFFIX = hydra.core.Name("commonWithSuffix")
    COMMON_WITHOUT_SUFFIX = hydra.core.Name("commonWithoutSuffix")
    INTEGER_WITH_SUFFIX = hydra.core.Name("integerWithSuffix")
    UNSIGNED_INTEGER = hydra.core.Name("unsignedInteger")

class ApproximateNumericLiteralScientificWithSuffix(Node[str]):
    ...

class ApproximateNumericLiteralScientificWithoutSuffix(Node[str]):
    ...

class ApproximateNumericLiteralCommonWithSuffix(Node[str]):
    ...

class ApproximateNumericLiteralIntegerWithSuffix(Node[str]):
    ...

class _ApproximateNumericLiteralMeta(type):
    def __getitem__(cls, item):
        return object

class ApproximateNumericLiteral(metaclass=_ApproximateNumericLiteralMeta):
    r"""ApproximateNumericLiteralScientificWithSuffix | ApproximateNumericLiteralScientificWithoutSuffix | ApproximateNumericLiteralCommonWithSuffix | ApproximateNumericLiteralIntegerWithSuffix"""

    TYPE_ = hydra.core.Name("openGql.grammar.ApproximateNumericLiteral")
    SCIENTIFIC_WITH_SUFFIX = hydra.core.Name("scientificWithSuffix")
    SCIENTIFIC_WITHOUT_SUFFIX = hydra.core.Name("scientificWithoutSuffix")
    COMMON_WITH_SUFFIX = hydra.core.Name("commonWithSuffix")
    INTEGER_WITH_SUFFIX = hydra.core.Name("integerWithSuffix")

class UnsignedIntegerDecimal(Node[str]):
    ...

class UnsignedIntegerHexadecimal(Node[str]):
    ...

class UnsignedIntegerOctal(Node[str]):
    ...

class UnsignedIntegerBinary(Node[str]):
    ...

class _UnsignedIntegerMeta(type):
    def __getitem__(cls, item):
        return object

class UnsignedInteger(metaclass=_UnsignedIntegerMeta):
    r"""UnsignedIntegerDecimal | UnsignedIntegerHexadecimal | UnsignedIntegerOctal | UnsignedIntegerBinary"""

    TYPE_ = hydra.core.Name("openGql.grammar.UnsignedInteger")
    DECIMAL = hydra.core.Name("decimal")
    HEXADECIMAL = hydra.core.Name("hexadecimal")
    OCTAL = hydra.core.Name("octal")
    BINARY = hydra.core.Name("binary")

UnsignedDecimalInteger: TypeAlias = "str"

NullLiteral: TypeAlias = "None"

DateString: TypeAlias = "CharacterStringLiteral"

TimeString: TypeAlias = "CharacterStringLiteral"

DatetimeString: TypeAlias = "CharacterStringLiteral"

DurationLiteral: TypeAlias = "DurationString"

DurationString: TypeAlias = "CharacterStringLiteral"

class NodeSynonym(Enum):
    NODE = hydra.core.Name("node")

    VERTEX = hydra.core.Name("vertex")

NodeSynonym.TYPE_ = hydra.core.Name("openGql.grammar.NodeSynonym")

class EdgesSynonym(Enum):
    EDGES = hydra.core.Name("edges")

    RELATIONSHIPS = hydra.core.Name("relationships")

EdgesSynonym.TYPE_ = hydra.core.Name("openGql.grammar.EdgesSynonym")

class EdgeSynonym(Enum):
    EDGE = hydra.core.Name("edge")

    RELATIONSHIP = hydra.core.Name("relationship")

EdgeSynonym.TYPE_ = hydra.core.Name("openGql.grammar.EdgeSynonym")

class Implies(Enum):
    RIGHT_DOUBLE_ARROW = hydra.core.Name("rightDoubleArrow")

    IMPLIES = hydra.core.Name("implies")

Implies.TYPE_ = hydra.core.Name("openGql.grammar.Implies")

ParameterName: TypeAlias = "str"

class BooleanLiteral(Enum):
    TRUE = hydra.core.Name("true")

    FALSE = hydra.core.Name("false")

    UNKNOWN = hydra.core.Name("unknown")

BooleanLiteral.TYPE_ = hydra.core.Name("openGql.grammar.BooleanLiteral")

ByteStringLiteral: TypeAlias = "str"
