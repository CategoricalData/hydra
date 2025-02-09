-- | A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at: https://github.com/opengql/grammar/blob/main/GQL.g4

module Hydra.Ext.Gql.OpenGql where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data GqlProgram = 
  GqlProgramActivity (Maybe ProgramActivity) |
  GqlProgramClose (Maybe SessionCloseCommand)
  deriving (Eq, Ord, Read, Show)

_GqlProgram = (Core.Name "hydra/ext/gql/openGql.GqlProgram")

_GqlProgram_activity = (Core.Name "activity")

_GqlProgram_close = (Core.Name "close")

data ProgramActivity = 
  ProgramActivitySession SessionActivity |
  ProgramActivityTransaction TransactionActivity
  deriving (Eq, Ord, Read, Show)

_ProgramActivity = (Core.Name "hydra/ext/gql/openGql.ProgramActivity")

_ProgramActivity_session = (Core.Name "session")

_ProgramActivity_transaction = (Core.Name "transaction")

data SessionActivity = 
  SessionActivityReset [SessionResetCommand] |
  SessionActivitySetAndResetCommands SessionSetAndResetCommands
  deriving (Eq, Ord, Read, Show)

_SessionActivity = (Core.Name "hydra/ext/gql/openGql.SessionActivity")

_SessionActivity_reset = (Core.Name "reset")

_SessionActivity_setAndResetCommands = (Core.Name "setAndResetCommands")

data SessionSetAndResetCommands = 
  SessionSetAndResetCommands {
    sessionSetAndResetCommandsSet :: [SessionSetCommand],
    sessionSetAndResetCommandsReset :: [SessionResetCommand]}
  deriving (Eq, Ord, Read, Show)

_SessionSetAndResetCommands = (Core.Name "hydra/ext/gql/openGql.SessionSetAndResetCommands")

_SessionSetAndResetCommands_set = (Core.Name "set")

_SessionSetAndResetCommands_reset = (Core.Name "reset")

data TransactionActivity = 
  TransactionActivityStart StartAndMaybeProcedureAndMaybeEnd |
  TransactionActivityProcedure ProcedureAndMaybeEnd |
  TransactionActivityEnd EndTransactionCommand
  deriving (Eq, Ord, Read, Show)

_TransactionActivity = (Core.Name "hydra/ext/gql/openGql.TransactionActivity")

_TransactionActivity_start = (Core.Name "start")

_TransactionActivity_procedure = (Core.Name "procedure")

_TransactionActivity_end = (Core.Name "end")

data StartAndMaybeProcedureAndMaybeEnd = 
  StartAndMaybeProcedureAndMaybeEnd {
    startAndMaybeProcedureAndMaybeEndStart :: StartTransactionCommand,
    startAndMaybeProcedureAndMaybeEndProcedureAndEnd :: (Maybe ProcedureAndMaybeEnd)}
  deriving (Eq, Ord, Read, Show)

_StartAndMaybeProcedureAndMaybeEnd = (Core.Name "hydra/ext/gql/openGql.StartAndMaybeProcedureAndMaybeEnd")

_StartAndMaybeProcedureAndMaybeEnd_start = (Core.Name "start")

_StartAndMaybeProcedureAndMaybeEnd_procedureAndEnd = (Core.Name "procedureAndEnd")

data ProcedureAndMaybeEnd = 
  ProcedureAndMaybeEnd {
    procedureAndMaybeEndProcedure :: ProcedureSpecification,
    procedureAndMaybeEndEnd :: (Maybe EndTransactionCommand)}
  deriving (Eq, Ord, Read, Show)

_ProcedureAndMaybeEnd = (Core.Name "hydra/ext/gql/openGql.ProcedureAndMaybeEnd")

_ProcedureAndMaybeEnd_procedure = (Core.Name "procedure")

_ProcedureAndMaybeEnd_end = (Core.Name "end")

data EndTransactionCommand = 
  EndTransactionCommandRollback RollbackCommand |
  EndTransactionCommandCommit CommitCommand
  deriving (Eq, Ord, Read, Show)

_EndTransactionCommand = (Core.Name "hydra/ext/gql/openGql.EndTransactionCommand")

_EndTransactionCommand_rollback = (Core.Name "rollback")

_EndTransactionCommand_commit = (Core.Name "commit")

data SessionSetCommand = 
  SessionSetCommandSchema SessionSetSchemaClause |
  SessionSetCommandGraph SessionSetGraphClause |
  SessionSetCommandTimeZone SessionSetTimeZoneClause |
  SessionSetCommandParameter SessionSetParameterClause
  deriving (Eq, Ord, Read, Show)

_SessionSetCommand = (Core.Name "hydra/ext/gql/openGql.SessionSetCommand")

_SessionSetCommand_schema = (Core.Name "schema")

_SessionSetCommand_graph = (Core.Name "graph")

_SessionSetCommand_timeZone = (Core.Name "timeZone")

_SessionSetCommand_parameter = (Core.Name "parameter")

newtype SessionSetSchemaClause = 
  SessionSetSchemaClause {
    unSessionSetSchemaClause :: SchemaReference}
  deriving (Eq, Ord, Read, Show)

_SessionSetSchemaClause = (Core.Name "hydra/ext/gql/openGql.SessionSetSchemaClause")

newtype SessionSetGraphClause = 
  SessionSetGraphClause {
    unSessionSetGraphClause :: GraphExpression}
  deriving (Eq, Ord, Read, Show)

_SessionSetGraphClause = (Core.Name "hydra/ext/gql/openGql.SessionSetGraphClause")

newtype SessionSetTimeZoneClause = 
  SessionSetTimeZoneClause {
    unSessionSetTimeZoneClause :: SetTimeZoneValue}
  deriving (Eq, Ord, Read, Show)

_SessionSetTimeZoneClause = (Core.Name "hydra/ext/gql/openGql.SessionSetTimeZoneClause")

newtype SetTimeZoneValue = 
  SetTimeZoneValue {
    unSetTimeZoneValue :: TimeZoneString}
  deriving (Eq, Ord, Read, Show)

_SetTimeZoneValue = (Core.Name "hydra/ext/gql/openGql.SetTimeZoneValue")

data SessionSetParameterClause = 
  SessionSetParameterClauseGraph SessionSetGraphParameterClause |
  SessionSetParameterClauseBindings SessionSetBindingTableParameterClause |
  SessionSetParameterClauseValue SessionSetValueParameterClause
  deriving (Eq, Ord, Read, Show)

_SessionSetParameterClause = (Core.Name "hydra/ext/gql/openGql.SessionSetParameterClause")

_SessionSetParameterClause_graph = (Core.Name "graph")

_SessionSetParameterClause_bindings = (Core.Name "bindings")

_SessionSetParameterClause_value = (Core.Name "value")

data SessionSetGraphParameterClause = 
  SessionSetGraphParameterClause {
    sessionSetGraphParameterClauseGraph :: SessionSetParameterName,
    sessionSetGraphParameterClauseInitializer :: OptTypedGraphInitializer}
  deriving (Eq, Ord, Read, Show)

_SessionSetGraphParameterClause = (Core.Name "hydra/ext/gql/openGql.SessionSetGraphParameterClause")

_SessionSetGraphParameterClause_graph = (Core.Name "graph")

_SessionSetGraphParameterClause_initializer = (Core.Name "initializer")

data SessionSetBindingTableParameterClause = 
  SessionSetBindingTableParameterClause {
    sessionSetBindingTableParameterClauseBinding :: Bool,
    sessionSetBindingTableParameterClauseParam :: SessionSetParameterName,
    sessionSetBindingTableParameterClauseInit :: OptTypedBindingTableInitializer}
  deriving (Eq, Ord, Read, Show)

_SessionSetBindingTableParameterClause = (Core.Name "hydra/ext/gql/openGql.SessionSetBindingTableParameterClause")

_SessionSetBindingTableParameterClause_binding = (Core.Name "binding")

_SessionSetBindingTableParameterClause_param = (Core.Name "param")

_SessionSetBindingTableParameterClause_init = (Core.Name "init")

data SessionSetValueParameterClause = 
  SessionSetValueParameterClause {
    sessionSetValueParameterClauseValue :: SessionSetParameterName,
    sessionSetValueParameterClauseInitializer :: OptTypedValueInitializer}
  deriving (Eq, Ord, Read, Show)

_SessionSetValueParameterClause = (Core.Name "hydra/ext/gql/openGql.SessionSetValueParameterClause")

_SessionSetValueParameterClause_value = (Core.Name "value")

_SessionSetValueParameterClause_initializer = (Core.Name "initializer")

data SessionSetParameterName = 
  SessionSetParameterName {
    sessionSetParameterNameIfNotExists :: Bool,
    sessionSetParameterNameParameter :: SessionParameterSpecification}
  deriving (Eq, Ord, Read, Show)

_SessionSetParameterName = (Core.Name "hydra/ext/gql/openGql.SessionSetParameterName")

_SessionSetParameterName_ifNotExists = (Core.Name "ifNotExists")

_SessionSetParameterName_parameter = (Core.Name "parameter")

newtype SessionResetCommand = 
  SessionResetCommand {
    unSessionResetCommand :: (Maybe SessionResetArguments)}
  deriving (Eq, Ord, Read, Show)

_SessionResetCommand = (Core.Name "hydra/ext/gql/openGql.SessionResetCommand")

data SessionResetArguments = 
  SessionResetArgumentsParametersOrCharacteristics AllParametersOrCharacteristics |
  SessionResetArgumentsSchema  |
  SessionResetArgumentsGraph  |
  SessionResetArgumentsTimeZone  |
  SessionResetArgumentsParameterSessionSpecification ParameterSessionSpecification
  deriving (Eq, Ord, Read, Show)

_SessionResetArguments = (Core.Name "hydra/ext/gql/openGql.SessionResetArguments")

_SessionResetArguments_parametersOrCharacteristics = (Core.Name "parametersOrCharacteristics")

_SessionResetArguments_schema = (Core.Name "schema")

_SessionResetArguments_graph = (Core.Name "graph")

_SessionResetArguments_timeZone = (Core.Name "timeZone")

_SessionResetArguments_parameterSessionSpecification = (Core.Name "parameterSessionSpecification")

data AllParametersOrCharacteristics = 
  AllParametersOrCharacteristics {
    allParametersOrCharacteristicsAll :: Bool,
    allParametersOrCharacteristicsType :: ParametersOrCharacteristics}
  deriving (Eq, Ord, Read, Show)

_AllParametersOrCharacteristics = (Core.Name "hydra/ext/gql/openGql.AllParametersOrCharacteristics")

_AllParametersOrCharacteristics_all = (Core.Name "all")

_AllParametersOrCharacteristics_type = (Core.Name "type")

data ParametersOrCharacteristics = 
  ParametersOrCharacteristicsParameters  |
  ParametersOrCharacteristicsCharacteristics 
  deriving (Eq, Ord, Read, Show)

_ParametersOrCharacteristics = (Core.Name "hydra/ext/gql/openGql.ParametersOrCharacteristics")

_ParametersOrCharacteristics_parameters = (Core.Name "parameters")

_ParametersOrCharacteristics_characteristics = (Core.Name "characteristics")

data ParameterSessionSpecification = 
  ParameterSessionSpecification {
    parameterSessionSpecificationParameter :: Bool,
    parameterSessionSpecificationSessionParameterSpecification :: SessionParameterSpecification}
  deriving (Eq, Ord, Read, Show)

_ParameterSessionSpecification = (Core.Name "hydra/ext/gql/openGql.ParameterSessionSpecification")

_ParameterSessionSpecification_parameter = (Core.Name "parameter")

_ParameterSessionSpecification_sessionParameterSpecification = (Core.Name "sessionParameterSpecification")

data SessionCloseCommand = 
  SessionCloseCommand {}
  deriving (Eq, Ord, Read, Show)

_SessionCloseCommand = (Core.Name "hydra/ext/gql/openGql.SessionCloseCommand")

newtype SessionParameterSpecification = 
  SessionParameterSpecification {
    unSessionParameterSpecification :: ParameterName}
  deriving (Eq, Ord, Read, Show)

_SessionParameterSpecification = (Core.Name "hydra/ext/gql/openGql.SessionParameterSpecification")

newtype StartTransactionCommand = 
  StartTransactionCommand {
    unStartTransactionCommand :: (Maybe TransactionCharacteristics)}
  deriving (Eq, Ord, Read, Show)

_StartTransactionCommand = (Core.Name "hydra/ext/gql/openGql.StartTransactionCommand")

newtype TransactionCharacteristics = 
  TransactionCharacteristics {
    unTransactionCharacteristics :: [TransactionMode]}
  deriving (Eq, Ord, Read, Show)

_TransactionCharacteristics = (Core.Name "hydra/ext/gql/openGql.TransactionCharacteristics")

newtype TransactionMode = 
  TransactionMode {
    unTransactionMode :: TransactionAccessMode}
  deriving (Eq, Ord, Read, Show)

_TransactionMode = (Core.Name "hydra/ext/gql/openGql.TransactionMode")

data TransactionAccessMode = 
  TransactionAccessModeReadOnly  |
  TransactionAccessModeReadWrite 
  deriving (Eq, Ord, Read, Show)

_TransactionAccessMode = (Core.Name "hydra/ext/gql/openGql.TransactionAccessMode")

_TransactionAccessMode_readOnly = (Core.Name "readOnly")

_TransactionAccessMode_readWrite = (Core.Name "readWrite")

data RollbackCommand = 
  RollbackCommand {}
  deriving (Eq, Ord, Read, Show)

_RollbackCommand = (Core.Name "hydra/ext/gql/openGql.RollbackCommand")

data CommitCommand = 
  CommitCommand {}
  deriving (Eq, Ord, Read, Show)

_CommitCommand = (Core.Name "hydra/ext/gql/openGql.CommitCommand")

newtype NestedProcedureSpecification = 
  NestedProcedureSpecification {
    unNestedProcedureSpecification :: ProcedureSpecification}
  deriving (Eq, Ord, Read, Show)

_NestedProcedureSpecification = (Core.Name "hydra/ext/gql/openGql.NestedProcedureSpecification")

newtype ProcedureSpecification = 
  ProcedureSpecification {
    unProcedureSpecification :: ProcedureBody}
  deriving (Eq, Ord, Read, Show)

_ProcedureSpecification = (Core.Name "hydra/ext/gql/openGql.ProcedureSpecification")

newtype NestedDataModifyingProcedureSpecification = 
  NestedDataModifyingProcedureSpecification {
    unNestedDataModifyingProcedureSpecification :: ProcedureBody}
  deriving (Eq, Ord, Read, Show)

_NestedDataModifyingProcedureSpecification = (Core.Name "hydra/ext/gql/openGql.NestedDataModifyingProcedureSpecification")

newtype NestedQuerySpecification = 
  NestedQuerySpecification {
    unNestedQuerySpecification :: ProcedureBody}
  deriving (Eq, Ord, Read, Show)

_NestedQuerySpecification = (Core.Name "hydra/ext/gql/openGql.NestedQuerySpecification")

data ProcedureBody = 
  ProcedureBody {
    procedureBodyAtSchema :: (Maybe AtSchemaClause),
    procedureBodyBindings :: (Maybe BindingVariableDefinitionBlock),
    procedureBodyStatements :: StatementBlock}
  deriving (Eq, Ord, Read, Show)

_ProcedureBody = (Core.Name "hydra/ext/gql/openGql.ProcedureBody")

_ProcedureBody_atSchema = (Core.Name "atSchema")

_ProcedureBody_bindings = (Core.Name "bindings")

_ProcedureBody_statements = (Core.Name "statements")

newtype BindingVariableDefinitionBlock = 
  BindingVariableDefinitionBlock {
    unBindingVariableDefinitionBlock :: [BindingVariableDefinition]}
  deriving (Eq, Ord, Read, Show)

_BindingVariableDefinitionBlock = (Core.Name "hydra/ext/gql/openGql.BindingVariableDefinitionBlock")

data BindingVariableDefinition = 
  BindingVariableDefinitionGraph GraphVariableDefinition |
  BindingVariableDefinitionTable BindingTableVariableDefinition |
  BindingVariableDefinitionValue ValueVariableDefinition
  deriving (Eq, Ord, Read, Show)

_BindingVariableDefinition = (Core.Name "hydra/ext/gql/openGql.BindingVariableDefinition")

_BindingVariableDefinition_graph = (Core.Name "graph")

_BindingVariableDefinition_table = (Core.Name "table")

_BindingVariableDefinition_value = (Core.Name "value")

data StatementBlock = 
  StatementBlock {
    statementBlockStatement :: Statement,
    statementBlockNextStatements :: [NextStatement]}
  deriving (Eq, Ord, Read, Show)

_StatementBlock = (Core.Name "hydra/ext/gql/openGql.StatementBlock")

_StatementBlock_statement = (Core.Name "statement")

_StatementBlock_nextStatements = (Core.Name "nextStatements")

data Statement = 
  StatementLinearCatalogModifying LinearCatalogModifyingStatement |
  StatementLinearDataModifying LinearDataModifyingStatement |
  StatementCompositeQuery CompositeQueryStatement
  deriving (Eq, Ord, Read, Show)

_Statement = (Core.Name "hydra/ext/gql/openGql.Statement")

_Statement_linearCatalogModifying = (Core.Name "linearCatalogModifying")

_Statement_linearDataModifying = (Core.Name "linearDataModifying")

_Statement_compositeQuery = (Core.Name "compositeQuery")

data NextStatement = 
  NextStatement {
    nextStatementYieldClause :: (Maybe YieldClause),
    nextStatementStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_NextStatement = (Core.Name "hydra/ext/gql/openGql.NextStatement")

_NextStatement_yieldClause = (Core.Name "yieldClause")

_NextStatement_statement = (Core.Name "statement")

data GraphVariableDefinition = 
  GraphVariableDefinition {
    graphVariableDefinitionVariable :: BindingVariable,
    graphVariableDefinitionInitializer :: OptTypedGraphInitializer}
  deriving (Eq, Ord, Read, Show)

_GraphVariableDefinition = (Core.Name "hydra/ext/gql/openGql.GraphVariableDefinition")

_GraphVariableDefinition_variable = (Core.Name "variable")

_GraphVariableDefinition_initializer = (Core.Name "initializer")

data OptTypedGraphInitializer = 
  OptTypedGraphInitializer {
    optTypedGraphInitializerType :: (Maybe TypedGraphReferenceValueType),
    optTypedGraphInitializerInitializer :: GraphInitializer}
  deriving (Eq, Ord, Read, Show)

_OptTypedGraphInitializer = (Core.Name "hydra/ext/gql/openGql.OptTypedGraphInitializer")

_OptTypedGraphInitializer_type = (Core.Name "type")

_OptTypedGraphInitializer_initializer = (Core.Name "initializer")

data TypedGraphReferenceValueType = 
  TypedGraphReferenceValueType {
    typedGraphReferenceValueTypeTyped :: (Maybe Typed),
    typedGraphReferenceValueTypeValueType :: GraphReferenceValueType}
  deriving (Eq, Ord, Read, Show)

_TypedGraphReferenceValueType = (Core.Name "hydra/ext/gql/openGql.TypedGraphReferenceValueType")

_TypedGraphReferenceValueType_typed = (Core.Name "typed")

_TypedGraphReferenceValueType_valueType = (Core.Name "valueType")

newtype GraphInitializer = 
  GraphInitializer {
    unGraphInitializer :: GraphExpression}
  deriving (Eq, Ord, Read, Show)

_GraphInitializer = (Core.Name "hydra/ext/gql/openGql.GraphInitializer")

data BindingTableVariableDefinition = 
  BindingTableVariableDefinition {
    bindingTableVariableDefinitionBinding :: Bool,
    bindingTableVariableDefinitionVariable :: BindingVariable,
    bindingTableVariableDefinitionInitializer :: OptTypedBindingTableInitializer}
  deriving (Eq, Ord, Read, Show)

_BindingTableVariableDefinition = (Core.Name "hydra/ext/gql/openGql.BindingTableVariableDefinition")

_BindingTableVariableDefinition_binding = (Core.Name "binding")

_BindingTableVariableDefinition_variable = (Core.Name "variable")

_BindingTableVariableDefinition_initializer = (Core.Name "initializer")

data OptTypedBindingTableInitializer = 
  OptTypedBindingTableInitializer {
    optTypedBindingTableInitializerType :: (Maybe TypedBindingTableReferenceValueType),
    optTypedBindingTableInitializerInitializer :: BindingTableInitializer}
  deriving (Eq, Ord, Read, Show)

_OptTypedBindingTableInitializer = (Core.Name "hydra/ext/gql/openGql.OptTypedBindingTableInitializer")

_OptTypedBindingTableInitializer_type = (Core.Name "type")

_OptTypedBindingTableInitializer_initializer = (Core.Name "initializer")

data TypedBindingTableReferenceValueType = 
  TypedBindingTableReferenceValueType {
    typedBindingTableReferenceValueTypeTyped :: (Maybe Typed),
    typedBindingTableReferenceValueTypeValueType :: BindingTableReferenceValueType}
  deriving (Eq, Ord, Read, Show)

_TypedBindingTableReferenceValueType = (Core.Name "hydra/ext/gql/openGql.TypedBindingTableReferenceValueType")

_TypedBindingTableReferenceValueType_typed = (Core.Name "typed")

_TypedBindingTableReferenceValueType_valueType = (Core.Name "valueType")

newtype BindingTableInitializer = 
  BindingTableInitializer {
    unBindingTableInitializer :: BindingTableExpression}
  deriving (Eq, Ord, Read, Show)

_BindingTableInitializer = (Core.Name "hydra/ext/gql/openGql.BindingTableInitializer")

data ValueVariableDefinition = 
  ValueVariableDefinition {
    valueVariableDefinitionVariable :: BindingVariable,
    valueVariableDefinitionInitializer :: OptTypedValueInitializer}
  deriving (Eq, Ord, Read, Show)

_ValueVariableDefinition = (Core.Name "hydra/ext/gql/openGql.ValueVariableDefinition")

_ValueVariableDefinition_variable = (Core.Name "variable")

_ValueVariableDefinition_initializer = (Core.Name "initializer")

data OptTypedValueInitializer = 
  OptTypedValueInitializer {
    optTypedValueInitializerType :: (Maybe TypedValueType),
    optTypedValueInitializerInitializer :: ValueInitializer}
  deriving (Eq, Ord, Read, Show)

_OptTypedValueInitializer = (Core.Name "hydra/ext/gql/openGql.OptTypedValueInitializer")

_OptTypedValueInitializer_type = (Core.Name "type")

_OptTypedValueInitializer_initializer = (Core.Name "initializer")

data TypedValueType = 
  TypedValueType {
    typedValueTypeTyped :: (Maybe Typed),
    typedValueTypeValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_TypedValueType = (Core.Name "hydra/ext/gql/openGql.TypedValueType")

_TypedValueType_typed = (Core.Name "typed")

_TypedValueType_valueType = (Core.Name "valueType")

newtype ValueInitializer = 
  ValueInitializer {
    unValueInitializer :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ValueInitializer = (Core.Name "hydra/ext/gql/openGql.ValueInitializer")

data GraphExpression = 
  GraphExpressionObject ObjectExpressionPrimary |
  GraphExpressionReference GraphReference |
  GraphExpressionName ObjectNameOrBindingVariable |
  GraphExpressionCurrent CurrentGraph
  deriving (Eq, Ord, Read, Show)

_GraphExpression = (Core.Name "hydra/ext/gql/openGql.GraphExpression")

_GraphExpression_object = (Core.Name "object")

_GraphExpression_reference = (Core.Name "reference")

_GraphExpression_name = (Core.Name "name")

_GraphExpression_current = (Core.Name "current")

data CurrentGraph = 
  CurrentGraphGraph  |
  CurrentGraphPropertyGraph 
  deriving (Eq, Ord, Read, Show)

_CurrentGraph = (Core.Name "hydra/ext/gql/openGql.CurrentGraph")

_CurrentGraph_graph = (Core.Name "graph")

_CurrentGraph_propertyGraph = (Core.Name "propertyGraph")

data BindingTableExpression = 
  BindingTableExpressionNested NestedBindingTableQuerySpecification |
  BindingTableExpressionObject ObjectExpressionPrimary |
  BindingTableExpressionTable BindingTableReference |
  BindingTableExpressionName ObjectNameOrBindingVariable
  deriving (Eq, Ord, Read, Show)

_BindingTableExpression = (Core.Name "hydra/ext/gql/openGql.BindingTableExpression")

_BindingTableExpression_nested = (Core.Name "nested")

_BindingTableExpression_object = (Core.Name "object")

_BindingTableExpression_table = (Core.Name "table")

_BindingTableExpression_name = (Core.Name "name")

newtype NestedBindingTableQuerySpecification = 
  NestedBindingTableQuerySpecification {
    unNestedBindingTableQuerySpecification :: NestedQuerySpecification}
  deriving (Eq, Ord, Read, Show)

_NestedBindingTableQuerySpecification = (Core.Name "hydra/ext/gql/openGql.NestedBindingTableQuerySpecification")

data ObjectExpressionPrimary = 
  ObjectExpressionPrimaryVariable ValueExpressionPrimary |
  ObjectExpressionPrimaryParenthesized ParenthesizedValueExpression |
  ObjectExpressionPrimaryNonParenthesized NonParenthesizedValueExpressionPrimarySpecialCase
  deriving (Eq, Ord, Read, Show)

_ObjectExpressionPrimary = (Core.Name "hydra/ext/gql/openGql.ObjectExpressionPrimary")

_ObjectExpressionPrimary_variable = (Core.Name "variable")

_ObjectExpressionPrimary_parenthesized = (Core.Name "parenthesized")

_ObjectExpressionPrimary_nonParenthesized = (Core.Name "nonParenthesized")

newtype LinearCatalogModifyingStatement = 
  LinearCatalogModifyingStatement {
    unLinearCatalogModifyingStatement :: [SimpleCatalogModifyingStatement]}
  deriving (Eq, Ord, Read, Show)

_LinearCatalogModifyingStatement = (Core.Name "hydra/ext/gql/openGql.LinearCatalogModifyingStatement")

data SimpleCatalogModifyingStatement = 
  SimpleCatalogModifyingStatementPrimitive PrimitiveCatalogModifyingStatement |
  SimpleCatalogModifyingStatementCallProcedure CallCatalogModifyingProcedureStatement
  deriving (Eq, Ord, Read, Show)

_SimpleCatalogModifyingStatement = (Core.Name "hydra/ext/gql/openGql.SimpleCatalogModifyingStatement")

_SimpleCatalogModifyingStatement_primitive = (Core.Name "primitive")

_SimpleCatalogModifyingStatement_callProcedure = (Core.Name "callProcedure")

data PrimitiveCatalogModifyingStatement = 
  PrimitiveCatalogModifyingStatementCreateSchema CreateSchemaStatement |
  PrimitiveCatalogModifyingStatementDropSchema DropSchemaStatement |
  PrimitiveCatalogModifyingStatementCreateGraph CreateGraphStatement |
  PrimitiveCatalogModifyingStatementDropGraph DropGraphStatement |
  PrimitiveCatalogModifyingStatementCreateGraphType CreateGraphTypeStatement |
  PrimitiveCatalogModifyingStatementDropGraphType DropGraphTypeStatement
  deriving (Eq, Ord, Read, Show)

_PrimitiveCatalogModifyingStatement = (Core.Name "hydra/ext/gql/openGql.PrimitiveCatalogModifyingStatement")

_PrimitiveCatalogModifyingStatement_createSchema = (Core.Name "createSchema")

_PrimitiveCatalogModifyingStatement_dropSchema = (Core.Name "dropSchema")

_PrimitiveCatalogModifyingStatement_createGraph = (Core.Name "createGraph")

_PrimitiveCatalogModifyingStatement_dropGraph = (Core.Name "dropGraph")

_PrimitiveCatalogModifyingStatement_createGraphType = (Core.Name "createGraphType")

_PrimitiveCatalogModifyingStatement_dropGraphType = (Core.Name "dropGraphType")

data CreateSchemaStatement = 
  CreateSchemaStatement {
    createSchemaStatementIfNotExists :: Bool,
    createSchemaStatementParentAndName :: CatalogSchemaParentAndName}
  deriving (Eq, Ord, Read, Show)

_CreateSchemaStatement = (Core.Name "hydra/ext/gql/openGql.CreateSchemaStatement")

_CreateSchemaStatement_ifNotExists = (Core.Name "ifNotExists")

_CreateSchemaStatement_parentAndName = (Core.Name "parentAndName")

data DropSchemaStatement = 
  DropSchemaStatement {
    dropSchemaStatementIfExists :: Bool,
    dropSchemaStatementParentAndName :: CatalogSchemaParentAndName}
  deriving (Eq, Ord, Read, Show)

_DropSchemaStatement = (Core.Name "hydra/ext/gql/openGql.DropSchemaStatement")

_DropSchemaStatement_ifExists = (Core.Name "ifExists")

_DropSchemaStatement_parentAndName = (Core.Name "parentAndName")

data CreateGraphStatement = 
  CreateGraphStatement {
    createGraphStatementCreateOption :: CreateGraphOption,
    createGraphStatementParentAndName :: CatalogGraphParentAndName,
    createGraphStatementType :: GraphTypeOption,
    createGraphStatementSource :: (Maybe GraphSource)}
  deriving (Eq, Ord, Read, Show)

_CreateGraphStatement = (Core.Name "hydra/ext/gql/openGql.CreateGraphStatement")

_CreateGraphStatement_createOption = (Core.Name "createOption")

_CreateGraphStatement_parentAndName = (Core.Name "parentAndName")

_CreateGraphStatement_type = (Core.Name "type")

_CreateGraphStatement_source = (Core.Name "source")

data CreateGraphOption = 
  CreateGraphOptionGraphIfNotExists Bool |
  CreateGraphOptionOrReplace 
  deriving (Eq, Ord, Read, Show)

_CreateGraphOption = (Core.Name "hydra/ext/gql/openGql.CreateGraphOption")

_CreateGraphOption_graphIfNotExists = (Core.Name "graphIfNotExists")

_CreateGraphOption_orReplace = (Core.Name "orReplace")

data GraphTypeOption = 
  GraphTypeOptionOpenGraphType OpenGraphType |
  GraphTypeOptionOfGraphType OfGraphType
  deriving (Eq, Ord, Read, Show)

_GraphTypeOption = (Core.Name "hydra/ext/gql/openGql.GraphTypeOption")

_GraphTypeOption_openGraphType = (Core.Name "openGraphType")

_GraphTypeOption_ofGraphType = (Core.Name "ofGraphType")

data OpenGraphType = 
  OpenGraphType {
    openGraphTypeTyped :: (Maybe Typed),
    openGraphTypeGraph :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenGraphType = (Core.Name "hydra/ext/gql/openGql.OpenGraphType")

_OpenGraphType_typed = (Core.Name "typed")

_OpenGraphType_graph = (Core.Name "graph")

data OfGraphType = 
  OfGraphTypeLikeGraph GraphTypeLikeGraph |
  OfGraphTypeReference TypedGraphTypeReference |
  OfGraphTypeNested TypedNestedGraphTypeSpecification
  deriving (Eq, Ord, Read, Show)

_OfGraphType = (Core.Name "hydra/ext/gql/openGql.OfGraphType")

_OfGraphType_likeGraph = (Core.Name "likeGraph")

_OfGraphType_reference = (Core.Name "reference")

_OfGraphType_nested = (Core.Name "nested")

newtype GraphTypeLikeGraph = 
  GraphTypeLikeGraph {
    unGraphTypeLikeGraph :: GraphExpression}
  deriving (Eq, Ord, Read, Show)

_GraphTypeLikeGraph = (Core.Name "hydra/ext/gql/openGql.GraphTypeLikeGraph")

newtype GraphSource = 
  GraphSource {
    unGraphSource :: GraphExpression}
  deriving (Eq, Ord, Read, Show)

_GraphSource = (Core.Name "hydra/ext/gql/openGql.GraphSource")

data TypedGraphTypeReference = 
  TypedGraphTypeReference {
    typedGraphTypeReferenceTyped :: (Maybe Typed),
    typedGraphTypeReferenceReference :: GraphTypeReference}
  deriving (Eq, Ord, Read, Show)

_TypedGraphTypeReference = (Core.Name "hydra/ext/gql/openGql.TypedGraphTypeReference")

_TypedGraphTypeReference_typed = (Core.Name "typed")

_TypedGraphTypeReference_reference = (Core.Name "reference")

data TypedNestedGraphTypeSpecification = 
  TypedNestedGraphTypeSpecification {
    typedNestedGraphTypeSpecificationTyped :: (Maybe Typed),
    typedNestedGraphTypeSpecificationGraph :: Bool,
    typedNestedGraphTypeSpecificationSpecification :: NestedGraphTypeSpecification}
  deriving (Eq, Ord, Read, Show)

_TypedNestedGraphTypeSpecification = (Core.Name "hydra/ext/gql/openGql.TypedNestedGraphTypeSpecification")

_TypedNestedGraphTypeSpecification_typed = (Core.Name "typed")

_TypedNestedGraphTypeSpecification_graph = (Core.Name "graph")

_TypedNestedGraphTypeSpecification_specification = (Core.Name "specification")

data DropGraphStatement = 
  DropGraphStatement {
    dropGraphStatementIfExists :: Bool,
    dropGraphStatementParentAndName :: CatalogGraphParentAndName}
  deriving (Eq, Ord, Read, Show)

_DropGraphStatement = (Core.Name "hydra/ext/gql/openGql.DropGraphStatement")

_DropGraphStatement_ifExists = (Core.Name "ifExists")

_DropGraphStatement_parentAndName = (Core.Name "parentAndName")

data CreateGraphTypeStatement = 
  CreateGraphTypeStatement {
    createGraphTypeStatementCreateOption :: CreateGraphTypeOption,
    createGraphTypeStatementParentAndName :: CatalogGraphTypeParentAndName,
    createGraphTypeStatementSource :: GraphTypeSource}
  deriving (Eq, Ord, Read, Show)

_CreateGraphTypeStatement = (Core.Name "hydra/ext/gql/openGql.CreateGraphTypeStatement")

_CreateGraphTypeStatement_createOption = (Core.Name "createOption")

_CreateGraphTypeStatement_parentAndName = (Core.Name "parentAndName")

_CreateGraphTypeStatement_source = (Core.Name "source")

data CreateGraphTypeOption = 
  CreateGraphTypeOptionTypeIfNotExists Bool |
  CreateGraphTypeOptionOrReplace 
  deriving (Eq, Ord, Read, Show)

_CreateGraphTypeOption = (Core.Name "hydra/ext/gql/openGql.CreateGraphTypeOption")

_CreateGraphTypeOption_typeIfNotExists = (Core.Name "typeIfNotExists")

_CreateGraphTypeOption_orReplace = (Core.Name "orReplace")

data GraphTypeSource = 
  GraphTypeSourceCopyOf CopyOfGraphType |
  GraphTypeSourceLikeGraph GraphTypeLikeGraph |
  GraphTypeSourceNestedSpecification NestedGraphTypeSpecification
  deriving (Eq, Ord, Read, Show)

_GraphTypeSource = (Core.Name "hydra/ext/gql/openGql.GraphTypeSource")

_GraphTypeSource_copyOf = (Core.Name "copyOf")

_GraphTypeSource_likeGraph = (Core.Name "likeGraph")

_GraphTypeSource_nestedSpecification = (Core.Name "nestedSpecification")

newtype CopyOfGraphType = 
  CopyOfGraphType {
    unCopyOfGraphType :: GraphTypeReference}
  deriving (Eq, Ord, Read, Show)

_CopyOfGraphType = (Core.Name "hydra/ext/gql/openGql.CopyOfGraphType")

data DropGraphTypeStatement = 
  DropGraphTypeStatement {
    dropGraphTypeStatementIfExists :: Bool,
    dropGraphTypeStatementParentAndName :: CatalogGraphTypeParentAndName}
  deriving (Eq, Ord, Read, Show)

_DropGraphTypeStatement = (Core.Name "hydra/ext/gql/openGql.DropGraphTypeStatement")

_DropGraphTypeStatement_ifExists = (Core.Name "ifExists")

_DropGraphTypeStatement_parentAndName = (Core.Name "parentAndName")

newtype CallCatalogModifyingProcedureStatement = 
  CallCatalogModifyingProcedureStatement {
    unCallCatalogModifyingProcedureStatement :: CallProcedureStatement}
  deriving (Eq, Ord, Read, Show)

_CallCatalogModifyingProcedureStatement = (Core.Name "hydra/ext/gql/openGql.CallCatalogModifyingProcedureStatement")

data LinearDataModifyingStatement = 
  LinearDataModifyingStatementFocused FocusedLinearDataModifyingStatement |
  LinearDataModifyingStatementAmbient AmbientLinearDataModifyingStatement
  deriving (Eq, Ord, Read, Show)

_LinearDataModifyingStatement = (Core.Name "hydra/ext/gql/openGql.LinearDataModifyingStatement")

_LinearDataModifyingStatement_focused = (Core.Name "focused")

_LinearDataModifyingStatement_ambient = (Core.Name "ambient")

data FocusedLinearDataModifyingStatement = 
  FocusedLinearDataModifyingStatementSimple FocusedLinearDataModifyingStatementBody |
  FocusedLinearDataModifyingStatementNested FocusedNestedDataModifyingProcedureSpecification
  deriving (Eq, Ord, Read, Show)

_FocusedLinearDataModifyingStatement = (Core.Name "hydra/ext/gql/openGql.FocusedLinearDataModifyingStatement")

_FocusedLinearDataModifyingStatement_simple = (Core.Name "simple")

_FocusedLinearDataModifyingStatement_nested = (Core.Name "nested")

data FocusedLinearDataModifyingStatementBody = 
  FocusedLinearDataModifyingStatementBody {
    focusedLinearDataModifyingStatementBodyUseGraph :: UseGraphClause,
    focusedLinearDataModifyingStatementBodySimpleAccess :: SimpleLinearDataAccessingStatement,
    focusedLinearDataModifyingStatementBodyPrimitiveResult :: (Maybe PrimitiveResultStatement)}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearDataModifyingStatementBody = (Core.Name "hydra/ext/gql/openGql.FocusedLinearDataModifyingStatementBody")

_FocusedLinearDataModifyingStatementBody_useGraph = (Core.Name "useGraph")

_FocusedLinearDataModifyingStatementBody_simpleAccess = (Core.Name "simpleAccess")

_FocusedLinearDataModifyingStatementBody_primitiveResult = (Core.Name "primitiveResult")

data FocusedNestedDataModifyingProcedureSpecification = 
  FocusedNestedDataModifyingProcedureSpecification {
    focusedNestedDataModifyingProcedureSpecificationUseGraph :: UseGraphClause,
    focusedNestedDataModifyingProcedureSpecificationNestedSpec :: NestedDataModifyingProcedureSpecification}
  deriving (Eq, Ord, Read, Show)

_FocusedNestedDataModifyingProcedureSpecification = (Core.Name "hydra/ext/gql/openGql.FocusedNestedDataModifyingProcedureSpecification")

_FocusedNestedDataModifyingProcedureSpecification_useGraph = (Core.Name "useGraph")

_FocusedNestedDataModifyingProcedureSpecification_nestedSpec = (Core.Name "nestedSpec")

data AmbientLinearDataModifyingStatement = 
  AmbientLinearDataModifyingStatementSimple AmbientLinearDataModifyingStatementBody |
  AmbientLinearDataModifyingStatementNested NestedDataModifyingProcedureSpecification
  deriving (Eq, Ord, Read, Show)

_AmbientLinearDataModifyingStatement = (Core.Name "hydra/ext/gql/openGql.AmbientLinearDataModifyingStatement")

_AmbientLinearDataModifyingStatement_simple = (Core.Name "simple")

_AmbientLinearDataModifyingStatement_nested = (Core.Name "nested")

data AmbientLinearDataModifyingStatementBody = 
  AmbientLinearDataModifyingStatementBody {
    ambientLinearDataModifyingStatementBodySimpleAccess :: SimpleLinearDataAccessingStatement,
    ambientLinearDataModifyingStatementBodyPrimitiveResult :: (Maybe PrimitiveResultStatement)}
  deriving (Eq, Ord, Read, Show)

_AmbientLinearDataModifyingStatementBody = (Core.Name "hydra/ext/gql/openGql.AmbientLinearDataModifyingStatementBody")

_AmbientLinearDataModifyingStatementBody_simpleAccess = (Core.Name "simpleAccess")

_AmbientLinearDataModifyingStatementBody_primitiveResult = (Core.Name "primitiveResult")

newtype SimpleLinearDataAccessingStatement = 
  SimpleLinearDataAccessingStatement {
    unSimpleLinearDataAccessingStatement :: [SimpleDataAccessingStatement]}
  deriving (Eq, Ord, Read, Show)

_SimpleLinearDataAccessingStatement = (Core.Name "hydra/ext/gql/openGql.SimpleLinearDataAccessingStatement")

data SimpleDataAccessingStatement = 
  SimpleDataAccessingStatementQuery SimpleQueryStatement |
  SimpleDataAccessingStatementModifying SimpleDataModifyingStatement
  deriving (Eq, Ord, Read, Show)

_SimpleDataAccessingStatement = (Core.Name "hydra/ext/gql/openGql.SimpleDataAccessingStatement")

_SimpleDataAccessingStatement_query = (Core.Name "query")

_SimpleDataAccessingStatement_modifying = (Core.Name "modifying")

data SimpleDataModifyingStatement = 
  SimpleDataModifyingStatementPrimitive PrimitiveDataModifyingStatement |
  SimpleDataModifyingStatementCallProcedure CallDataModifyingProcedureStatement
  deriving (Eq, Ord, Read, Show)

_SimpleDataModifyingStatement = (Core.Name "hydra/ext/gql/openGql.SimpleDataModifyingStatement")

_SimpleDataModifyingStatement_primitive = (Core.Name "primitive")

_SimpleDataModifyingStatement_callProcedure = (Core.Name "callProcedure")

data PrimitiveDataModifyingStatement = 
  PrimitiveDataModifyingStatementInsert InsertStatement |
  PrimitiveDataModifyingStatementSet SetStatement |
  PrimitiveDataModifyingStatementRemove RemoveStatement |
  PrimitiveDataModifyingStatementDelete DeleteStatement
  deriving (Eq, Ord, Read, Show)

_PrimitiveDataModifyingStatement = (Core.Name "hydra/ext/gql/openGql.PrimitiveDataModifyingStatement")

_PrimitiveDataModifyingStatement_insert = (Core.Name "insert")

_PrimitiveDataModifyingStatement_set = (Core.Name "set")

_PrimitiveDataModifyingStatement_remove = (Core.Name "remove")

_PrimitiveDataModifyingStatement_delete = (Core.Name "delete")

newtype InsertStatement = 
  InsertStatement {
    unInsertStatement :: InsertGraphPattern}
  deriving (Eq, Ord, Read, Show)

_InsertStatement = (Core.Name "hydra/ext/gql/openGql.InsertStatement")

newtype SetStatement = 
  SetStatement {
    unSetStatement :: SetItemList}
  deriving (Eq, Ord, Read, Show)

_SetStatement = (Core.Name "hydra/ext/gql/openGql.SetStatement")

newtype SetItemList = 
  SetItemList {
    unSetItemList :: [SetItem]}
  deriving (Eq, Ord, Read, Show)

_SetItemList = (Core.Name "hydra/ext/gql/openGql.SetItemList")

data SetItem = 
  SetItemProperty SetPropertyItem |
  SetItemAllProperties SetAllPropertiesItem |
  SetItemLabel SetLabelItem
  deriving (Eq, Ord, Read, Show)

_SetItem = (Core.Name "hydra/ext/gql/openGql.SetItem")

_SetItem_property = (Core.Name "property")

_SetItem_allProperties = (Core.Name "allProperties")

_SetItem_label = (Core.Name "label")

data SetPropertyItem = 
  SetPropertyItem {
    setPropertyItemVariable :: BindingVariableReference,
    setPropertyItemPropertyName :: PropertyName,
    setPropertyItemValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_SetPropertyItem = (Core.Name "hydra/ext/gql/openGql.SetPropertyItem")

_SetPropertyItem_variable = (Core.Name "variable")

_SetPropertyItem_propertyName = (Core.Name "propertyName")

_SetPropertyItem_value = (Core.Name "value")

data SetAllPropertiesItem = 
  SetAllPropertiesItem {
    setAllPropertiesItemVariable :: BindingVariableReference,
    setAllPropertiesItemProperties :: (Maybe PropertyKeyValuePairList)}
  deriving (Eq, Ord, Read, Show)

_SetAllPropertiesItem = (Core.Name "hydra/ext/gql/openGql.SetAllPropertiesItem")

_SetAllPropertiesItem_variable = (Core.Name "variable")

_SetAllPropertiesItem_properties = (Core.Name "properties")

data SetLabelItem = 
  SetLabelItem {
    setLabelItemVariable :: BindingVariableReference,
    setLabelItemIsOrColon :: IsOrColon,
    setLabelItemLabel :: LabelName}
  deriving (Eq, Ord, Read, Show)

_SetLabelItem = (Core.Name "hydra/ext/gql/openGql.SetLabelItem")

_SetLabelItem_variable = (Core.Name "variable")

_SetLabelItem_isOrColon = (Core.Name "isOrColon")

_SetLabelItem_label = (Core.Name "label")

newtype RemoveStatement = 
  RemoveStatement {
    unRemoveStatement :: RemoveItemList}
  deriving (Eq, Ord, Read, Show)

_RemoveStatement = (Core.Name "hydra/ext/gql/openGql.RemoveStatement")

newtype RemoveItemList = 
  RemoveItemList {
    unRemoveItemList :: [RemoveItem]}
  deriving (Eq, Ord, Read, Show)

_RemoveItemList = (Core.Name "hydra/ext/gql/openGql.RemoveItemList")

data RemoveItem = 
  RemoveItemProperty RemovePropertyItem |
  RemoveItemLabel RemoveLabelItem
  deriving (Eq, Ord, Read, Show)

_RemoveItem = (Core.Name "hydra/ext/gql/openGql.RemoveItem")

_RemoveItem_property = (Core.Name "property")

_RemoveItem_label = (Core.Name "label")

data RemovePropertyItem = 
  RemovePropertyItem {
    removePropertyItemVariable :: BindingVariableReference,
    removePropertyItemPropertyName :: PropertyName}
  deriving (Eq, Ord, Read, Show)

_RemovePropertyItem = (Core.Name "hydra/ext/gql/openGql.RemovePropertyItem")

_RemovePropertyItem_variable = (Core.Name "variable")

_RemovePropertyItem_propertyName = (Core.Name "propertyName")

data RemoveLabelItem = 
  RemoveLabelItem {
    removeLabelItemVariable :: BindingVariableReference,
    removeLabelItemIsOrColon :: IsOrColon,
    removeLabelItemLabel :: LabelName}
  deriving (Eq, Ord, Read, Show)

_RemoveLabelItem = (Core.Name "hydra/ext/gql/openGql.RemoveLabelItem")

_RemoveLabelItem_variable = (Core.Name "variable")

_RemoveLabelItem_isOrColon = (Core.Name "isOrColon")

_RemoveLabelItem_label = (Core.Name "label")

data DeleteStatement = 
  DeleteStatement {
    deleteStatementDetach :: (Maybe DetachOption),
    deleteStatementItems :: DeleteItemList}
  deriving (Eq, Ord, Read, Show)

_DeleteStatement = (Core.Name "hydra/ext/gql/openGql.DeleteStatement")

_DeleteStatement_detach = (Core.Name "detach")

_DeleteStatement_items = (Core.Name "items")

data DetachOption = 
  DetachOptionDetach  |
  DetachOptionNoDetach 
  deriving (Eq, Ord, Read, Show)

_DetachOption = (Core.Name "hydra/ext/gql/openGql.DetachOption")

_DetachOption_detach = (Core.Name "detach")

_DetachOption_noDetach = (Core.Name "noDetach")

newtype DeleteItemList = 
  DeleteItemList {
    unDeleteItemList :: [DeleteItem]}
  deriving (Eq, Ord, Read, Show)

_DeleteItemList = (Core.Name "hydra/ext/gql/openGql.DeleteItemList")

newtype DeleteItem = 
  DeleteItem {
    unDeleteItem :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_DeleteItem = (Core.Name "hydra/ext/gql/openGql.DeleteItem")

newtype CallDataModifyingProcedureStatement = 
  CallDataModifyingProcedureStatement {
    unCallDataModifyingProcedureStatement :: CallProcedureStatement}
  deriving (Eq, Ord, Read, Show)

_CallDataModifyingProcedureStatement = (Core.Name "hydra/ext/gql/openGql.CallDataModifyingProcedureStatement")

newtype CompositeQueryStatement = 
  CompositeQueryStatement {
    unCompositeQueryStatement :: CompositeQueryExpression}
  deriving (Eq, Ord, Read, Show)

_CompositeQueryStatement = (Core.Name "hydra/ext/gql/openGql.CompositeQueryStatement")

data CompositeQueryExpression = 
  CompositeQueryExpressionSimple CompositeQueryExpressionConjunction |
  CompositeQueryExpressionPrimary CompositeQueryPrimary
  deriving (Eq, Ord, Read, Show)

_CompositeQueryExpression = (Core.Name "hydra/ext/gql/openGql.CompositeQueryExpression")

_CompositeQueryExpression_simple = (Core.Name "simple")

_CompositeQueryExpression_primary = (Core.Name "primary")

data CompositeQueryExpressionConjunction = 
  CompositeQueryExpressionConjunction {
    compositeQueryExpressionConjunctionLeft :: CompositeQueryExpression,
    compositeQueryExpressionConjunctionConjunction :: QueryConjunction,
    compositeQueryExpressionConjunctionRight :: CompositeQueryPrimary}
  deriving (Eq, Ord, Read, Show)

_CompositeQueryExpressionConjunction = (Core.Name "hydra/ext/gql/openGql.CompositeQueryExpressionConjunction")

_CompositeQueryExpressionConjunction_left = (Core.Name "left")

_CompositeQueryExpressionConjunction_conjunction = (Core.Name "conjunction")

_CompositeQueryExpressionConjunction_right = (Core.Name "right")

data QueryConjunction = 
  QueryConjunctionSetOperator SetOperator |
  QueryConjunctionOtherwise 
  deriving (Eq, Ord, Read, Show)

_QueryConjunction = (Core.Name "hydra/ext/gql/openGql.QueryConjunction")

_QueryConjunction_setOperator = (Core.Name "setOperator")

_QueryConjunction_otherwise = (Core.Name "otherwise")

data SetOperator = 
  SetOperator {
    setOperatorOperatorType :: SetOperatorType,
    setOperatorQuantifier :: (Maybe SetQuantifier)}
  deriving (Eq, Ord, Read, Show)

_SetOperator = (Core.Name "hydra/ext/gql/openGql.SetOperator")

_SetOperator_operatorType = (Core.Name "operatorType")

_SetOperator_quantifier = (Core.Name "quantifier")

data SetOperatorType = 
  SetOperatorTypeUnion  |
  SetOperatorTypeExcept  |
  SetOperatorTypeIntersect 
  deriving (Eq, Ord, Read, Show)

_SetOperatorType = (Core.Name "hydra/ext/gql/openGql.SetOperatorType")

_SetOperatorType_union = (Core.Name "union")

_SetOperatorType_except = (Core.Name "except")

_SetOperatorType_intersect = (Core.Name "intersect")

newtype CompositeQueryPrimary = 
  CompositeQueryPrimary {
    unCompositeQueryPrimary :: LinearQueryStatement}
  deriving (Eq, Ord, Read, Show)

_CompositeQueryPrimary = (Core.Name "hydra/ext/gql/openGql.CompositeQueryPrimary")

data LinearQueryStatement = 
  LinearQueryStatementFocused FocusedLinearQueryStatement |
  LinearQueryStatementAmbient AmbientLinearQueryStatement
  deriving (Eq, Ord, Read, Show)

_LinearQueryStatement = (Core.Name "hydra/ext/gql/openGql.LinearQueryStatement")

_LinearQueryStatement_focused = (Core.Name "focused")

_LinearQueryStatement_ambient = (Core.Name "ambient")

data FocusedLinearQueryStatement = 
  FocusedLinearQueryStatementParts FocusedLinearQueryStatementPartsAndResult |
  FocusedLinearQueryStatementPrimitive FocusedPrimitiveResultStatement |
  FocusedLinearQueryStatementNested FocusedNestedQuerySpecification |
  FocusedLinearQueryStatementSelect SelectStatement
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryStatement = (Core.Name "hydra/ext/gql/openGql.FocusedLinearQueryStatement")

_FocusedLinearQueryStatement_parts = (Core.Name "parts")

_FocusedLinearQueryStatement_primitive = (Core.Name "primitive")

_FocusedLinearQueryStatement_nested = (Core.Name "nested")

_FocusedLinearQueryStatement_select = (Core.Name "select")

data FocusedLinearQueryStatementPartsAndResult = 
  FocusedLinearQueryStatementPartsAndResult {
    focusedLinearQueryStatementPartsAndResultParts :: [FocusedLinearQueryStatementPart],
    focusedLinearQueryStatementPartsAndResultResult :: FocusedLinearQueryAndPrimitiveResultStatementPart}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryStatementPartsAndResult = (Core.Name "hydra/ext/gql/openGql.FocusedLinearQueryStatementPartsAndResult")

_FocusedLinearQueryStatementPartsAndResult_parts = (Core.Name "parts")

_FocusedLinearQueryStatementPartsAndResult_result = (Core.Name "result")

data FocusedLinearQueryStatementPart = 
  FocusedLinearQueryStatementPart {
    focusedLinearQueryStatementPartUseGraph :: UseGraphClause,
    focusedLinearQueryStatementPartSimple :: SimpleLinearQueryStatement}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryStatementPart = (Core.Name "hydra/ext/gql/openGql.FocusedLinearQueryStatementPart")

_FocusedLinearQueryStatementPart_useGraph = (Core.Name "useGraph")

_FocusedLinearQueryStatementPart_simple = (Core.Name "simple")

data FocusedLinearQueryAndPrimitiveResultStatementPart = 
  FocusedLinearQueryAndPrimitiveResultStatementPart {
    focusedLinearQueryAndPrimitiveResultStatementPartUseGraph :: UseGraphClause,
    focusedLinearQueryAndPrimitiveResultStatementPartSimple :: SimpleLinearQueryStatement,
    focusedLinearQueryAndPrimitiveResultStatementPartPrimitiveResult :: PrimitiveResultStatement}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryAndPrimitiveResultStatementPart = (Core.Name "hydra/ext/gql/openGql.FocusedLinearQueryAndPrimitiveResultStatementPart")

_FocusedLinearQueryAndPrimitiveResultStatementPart_useGraph = (Core.Name "useGraph")

_FocusedLinearQueryAndPrimitiveResultStatementPart_simple = (Core.Name "simple")

_FocusedLinearQueryAndPrimitiveResultStatementPart_primitiveResult = (Core.Name "primitiveResult")

data FocusedPrimitiveResultStatement = 
  FocusedPrimitiveResultStatement {
    focusedPrimitiveResultStatementUseGraph :: UseGraphClause,
    focusedPrimitiveResultStatementPrimitiveResult :: PrimitiveResultStatement}
  deriving (Eq, Ord, Read, Show)

_FocusedPrimitiveResultStatement = (Core.Name "hydra/ext/gql/openGql.FocusedPrimitiveResultStatement")

_FocusedPrimitiveResultStatement_useGraph = (Core.Name "useGraph")

_FocusedPrimitiveResultStatement_primitiveResult = (Core.Name "primitiveResult")

data FocusedNestedQuerySpecification = 
  FocusedNestedQuerySpecification {
    focusedNestedQuerySpecificationUseGraph :: UseGraphClause,
    focusedNestedQuerySpecificationNested :: NestedQuerySpecification}
  deriving (Eq, Ord, Read, Show)

_FocusedNestedQuerySpecification = (Core.Name "hydra/ext/gql/openGql.FocusedNestedQuerySpecification")

_FocusedNestedQuerySpecification_useGraph = (Core.Name "useGraph")

_FocusedNestedQuerySpecification_nested = (Core.Name "nested")

data AmbientLinearQueryStatement = 
  AmbientLinearQueryStatementSimple AmbientLinearQueryStatementSimpleAndPrimitiveResult |
  AmbientLinearQueryStatementNested NestedQuerySpecification
  deriving (Eq, Ord, Read, Show)

_AmbientLinearQueryStatement = (Core.Name "hydra/ext/gql/openGql.AmbientLinearQueryStatement")

_AmbientLinearQueryStatement_simple = (Core.Name "simple")

_AmbientLinearQueryStatement_nested = (Core.Name "nested")

data AmbientLinearQueryStatementSimpleAndPrimitiveResult = 
  AmbientLinearQueryStatementSimpleAndPrimitiveResult {
    ambientLinearQueryStatementSimpleAndPrimitiveResultSimple :: (Maybe SimpleLinearQueryStatement),
    ambientLinearQueryStatementSimpleAndPrimitiveResultPrimitiveResult :: PrimitiveResultStatement}
  deriving (Eq, Ord, Read, Show)

_AmbientLinearQueryStatementSimpleAndPrimitiveResult = (Core.Name "hydra/ext/gql/openGql.AmbientLinearQueryStatementSimpleAndPrimitiveResult")

_AmbientLinearQueryStatementSimpleAndPrimitiveResult_simple = (Core.Name "simple")

_AmbientLinearQueryStatementSimpleAndPrimitiveResult_primitiveResult = (Core.Name "primitiveResult")

newtype SimpleLinearQueryStatement = 
  SimpleLinearQueryStatement {
    unSimpleLinearQueryStatement :: [SimpleQueryStatement]}
  deriving (Eq, Ord, Read, Show)

_SimpleLinearQueryStatement = (Core.Name "hydra/ext/gql/openGql.SimpleLinearQueryStatement")

data SimpleQueryStatement = 
  SimpleQueryStatementPrimitive PrimitiveQueryStatement |
  SimpleQueryStatementCall CallQueryStatement
  deriving (Eq, Ord, Read, Show)

_SimpleQueryStatement = (Core.Name "hydra/ext/gql/openGql.SimpleQueryStatement")

_SimpleQueryStatement_primitive = (Core.Name "primitive")

_SimpleQueryStatement_call = (Core.Name "call")

data PrimitiveQueryStatement = 
  PrimitiveQueryStatementMatch MatchStatement |
  PrimitiveQueryStatementLet LetStatement |
  PrimitiveQueryStatementFor ForStatement |
  PrimitiveQueryStatementFilter FilterStatement |
  PrimitiveQueryStatementOrderByAndPage OrderByAndPageStatement
  deriving (Eq, Ord, Read, Show)

_PrimitiveQueryStatement = (Core.Name "hydra/ext/gql/openGql.PrimitiveQueryStatement")

_PrimitiveQueryStatement_match = (Core.Name "match")

_PrimitiveQueryStatement_let = (Core.Name "let")

_PrimitiveQueryStatement_for = (Core.Name "for")

_PrimitiveQueryStatement_filter = (Core.Name "filter")

_PrimitiveQueryStatement_orderByAndPage = (Core.Name "orderByAndPage")

data MatchStatement = 
  MatchStatementSimple SimpleMatchStatement |
  MatchStatementOptional OptionalMatchStatement
  deriving (Eq, Ord, Read, Show)

_MatchStatement = (Core.Name "hydra/ext/gql/openGql.MatchStatement")

_MatchStatement_simple = (Core.Name "simple")

_MatchStatement_optional = (Core.Name "optional")

newtype SimpleMatchStatement = 
  SimpleMatchStatement {
    unSimpleMatchStatement :: GraphPatternBindingTable}
  deriving (Eq, Ord, Read, Show)

_SimpleMatchStatement = (Core.Name "hydra/ext/gql/openGql.SimpleMatchStatement")

newtype OptionalMatchStatement = 
  OptionalMatchStatement {
    unOptionalMatchStatement :: OptionalOperand}
  deriving (Eq, Ord, Read, Show)

_OptionalMatchStatement = (Core.Name "hydra/ext/gql/openGql.OptionalMatchStatement")

data OptionalOperand = 
  OptionalOperandSimple SimpleMatchStatement |
  OptionalOperandBraceBlock MatchStatementBlock |
  OptionalOperandParenBlock MatchStatementBlock
  deriving (Eq, Ord, Read, Show)

_OptionalOperand = (Core.Name "hydra/ext/gql/openGql.OptionalOperand")

_OptionalOperand_simple = (Core.Name "simple")

_OptionalOperand_braceBlock = (Core.Name "braceBlock")

_OptionalOperand_parenBlock = (Core.Name "parenBlock")

newtype MatchStatementBlock = 
  MatchStatementBlock {
    unMatchStatementBlock :: [MatchStatement]}
  deriving (Eq, Ord, Read, Show)

_MatchStatementBlock = (Core.Name "hydra/ext/gql/openGql.MatchStatementBlock")

newtype CallQueryStatement = 
  CallQueryStatement {
    unCallQueryStatement :: CallProcedureStatement}
  deriving (Eq, Ord, Read, Show)

_CallQueryStatement = (Core.Name "hydra/ext/gql/openGql.CallQueryStatement")

data FilterStatement = 
  FilterStatementWhereClause WhereClause |
  FilterStatementSearchCondition SearchCondition
  deriving (Eq, Ord, Read, Show)

_FilterStatement = (Core.Name "hydra/ext/gql/openGql.FilterStatement")

_FilterStatement_whereClause = (Core.Name "whereClause")

_FilterStatement_searchCondition = (Core.Name "searchCondition")

newtype LetStatement = 
  LetStatement {
    unLetStatement :: LetVariableDefinitionList}
  deriving (Eq, Ord, Read, Show)

_LetStatement = (Core.Name "hydra/ext/gql/openGql.LetStatement")

newtype LetVariableDefinitionList = 
  LetVariableDefinitionList {
    unLetVariableDefinitionList :: [LetVariableDefinition]}
  deriving (Eq, Ord, Read, Show)

_LetVariableDefinitionList = (Core.Name "hydra/ext/gql/openGql.LetVariableDefinitionList")

data LetVariableDefinition = 
  LetVariableDefinitionValueVariable ValueVariableDefinition |
  LetVariableDefinitionBindingEqualsValue BindingEqualsValue
  deriving (Eq, Ord, Read, Show)

_LetVariableDefinition = (Core.Name "hydra/ext/gql/openGql.LetVariableDefinition")

_LetVariableDefinition_valueVariable = (Core.Name "valueVariable")

_LetVariableDefinition_bindingEqualsValue = (Core.Name "bindingEqualsValue")

data BindingEqualsValue = 
  BindingEqualsValue {
    bindingEqualsValueBinding :: BindingVariable,
    bindingEqualsValueValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_BindingEqualsValue = (Core.Name "hydra/ext/gql/openGql.BindingEqualsValue")

_BindingEqualsValue_binding = (Core.Name "binding")

_BindingEqualsValue_value = (Core.Name "value")

data ForStatement = 
  ForStatement {
    forStatementItem :: ForItem,
    forStatementOrdinalityOrOffset :: (Maybe ForOrdinalityOrOffset)}
  deriving (Eq, Ord, Read, Show)

_ForStatement = (Core.Name "hydra/ext/gql/openGql.ForStatement")

_ForStatement_item = (Core.Name "item")

_ForStatement_ordinalityOrOffset = (Core.Name "ordinalityOrOffset")

data ForItem = 
  ForItem {
    forItemAlias :: ForItemAlias,
    forItemSource :: ForItemSource}
  deriving (Eq, Ord, Read, Show)

_ForItem = (Core.Name "hydra/ext/gql/openGql.ForItem")

_ForItem_alias = (Core.Name "alias")

_ForItem_source = (Core.Name "source")

newtype ForItemAlias = 
  ForItemAlias {
    unForItemAlias :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_ForItemAlias = (Core.Name "hydra/ext/gql/openGql.ForItemAlias")

newtype ForItemSource = 
  ForItemSource {
    unForItemSource :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ForItemSource = (Core.Name "hydra/ext/gql/openGql.ForItemSource")

data ForOrdinalityOrOffset = 
  ForOrdinalityOrOffset {
    forOrdinalityOrOffsetType :: OrdinalityOrOffsetType,
    forOrdinalityOrOffsetVariable :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_ForOrdinalityOrOffset = (Core.Name "hydra/ext/gql/openGql.ForOrdinalityOrOffset")

_ForOrdinalityOrOffset_type = (Core.Name "type")

_ForOrdinalityOrOffset_variable = (Core.Name "variable")

data OrdinalityOrOffsetType = 
  OrdinalityOrOffsetTypeOrdinality  |
  OrdinalityOrOffsetTypeOffset 
  deriving (Eq, Ord, Read, Show)

_OrdinalityOrOffsetType = (Core.Name "hydra/ext/gql/openGql.OrdinalityOrOffsetType")

_OrdinalityOrOffsetType_ordinality = (Core.Name "ordinality")

_OrdinalityOrOffsetType_offset = (Core.Name "offset")

data OrderByAndPageStatement = 
  OrderByAndPageStatementOrderByAndOptionalOffsetAndLimit OrderByAndOptionalOffsetAndLimit |
  OrderByAndPageStatementOffsetAndOptionalLimit OffsetAndOptionalLimit |
  OrderByAndPageStatementLimitOnly LimitClause
  deriving (Eq, Ord, Read, Show)

_OrderByAndPageStatement = (Core.Name "hydra/ext/gql/openGql.OrderByAndPageStatement")

_OrderByAndPageStatement_orderByAndOptionalOffsetAndLimit = (Core.Name "orderByAndOptionalOffsetAndLimit")

_OrderByAndPageStatement_offsetAndOptionalLimit = (Core.Name "offsetAndOptionalLimit")

_OrderByAndPageStatement_limitOnly = (Core.Name "limitOnly")

data OrderByAndOptionalOffsetAndLimit = 
  OrderByAndOptionalOffsetAndLimit {
    orderByAndOptionalOffsetAndLimitOrderBy :: OrderByClause,
    orderByAndOptionalOffsetAndLimitOffset :: (Maybe OffsetClause),
    orderByAndOptionalOffsetAndLimitLimit :: (Maybe LimitClause)}
  deriving (Eq, Ord, Read, Show)

_OrderByAndOptionalOffsetAndLimit = (Core.Name "hydra/ext/gql/openGql.OrderByAndOptionalOffsetAndLimit")

_OrderByAndOptionalOffsetAndLimit_orderBy = (Core.Name "orderBy")

_OrderByAndOptionalOffsetAndLimit_offset = (Core.Name "offset")

_OrderByAndOptionalOffsetAndLimit_limit = (Core.Name "limit")

data OffsetAndOptionalLimit = 
  OffsetAndOptionalLimit {
    offsetAndOptionalLimitOffset :: OffsetClause,
    offsetAndOptionalLimitLimit :: (Maybe LimitClause)}
  deriving (Eq, Ord, Read, Show)

_OffsetAndOptionalLimit = (Core.Name "hydra/ext/gql/openGql.OffsetAndOptionalLimit")

_OffsetAndOptionalLimit_offset = (Core.Name "offset")

_OffsetAndOptionalLimit_limit = (Core.Name "limit")

data PrimitiveResultStatement = 
  PrimitiveResultStatementReturnAndOptionalOrderBy ReturnAndOptionalOrderByAndPage |
  PrimitiveResultStatementFinish 
  deriving (Eq, Ord, Read, Show)

_PrimitiveResultStatement = (Core.Name "hydra/ext/gql/openGql.PrimitiveResultStatement")

_PrimitiveResultStatement_returnAndOptionalOrderBy = (Core.Name "returnAndOptionalOrderBy")

_PrimitiveResultStatement_finish = (Core.Name "finish")

data ReturnAndOptionalOrderByAndPage = 
  ReturnAndOptionalOrderByAndPage {
    returnAndOptionalOrderByAndPageReturn :: ReturnStatement,
    returnAndOptionalOrderByAndPageOrderByAndPage :: (Maybe OrderByAndPageStatement)}
  deriving (Eq, Ord, Read, Show)

_ReturnAndOptionalOrderByAndPage = (Core.Name "hydra/ext/gql/openGql.ReturnAndOptionalOrderByAndPage")

_ReturnAndOptionalOrderByAndPage_return = (Core.Name "return")

_ReturnAndOptionalOrderByAndPage_orderByAndPage = (Core.Name "orderByAndPage")

newtype ReturnStatement = 
  ReturnStatement {
    unReturnStatement :: ReturnStatementBody}
  deriving (Eq, Ord, Read, Show)

_ReturnStatement = (Core.Name "hydra/ext/gql/openGql.ReturnStatement")

data ReturnStatementBody = 
  ReturnStatementBodyItems ReturnItemsAndGroupBy |
  ReturnStatementBodyNoBindings 
  deriving (Eq, Ord, Read, Show)

_ReturnStatementBody = (Core.Name "hydra/ext/gql/openGql.ReturnStatementBody")

_ReturnStatementBody_items = (Core.Name "items")

_ReturnStatementBody_noBindings = (Core.Name "noBindings")

data ReturnItemsAndGroupBy = 
  ReturnItemsAndGroupBy {
    returnItemsAndGroupByQuantifier :: (Maybe SetQuantifier),
    returnItemsAndGroupByItems :: ReturnItems,
    returnItemsAndGroupByGroupBy :: (Maybe GroupByClause)}
  deriving (Eq, Ord, Read, Show)

_ReturnItemsAndGroupBy = (Core.Name "hydra/ext/gql/openGql.ReturnItemsAndGroupBy")

_ReturnItemsAndGroupBy_quantifier = (Core.Name "quantifier")

_ReturnItemsAndGroupBy_items = (Core.Name "items")

_ReturnItemsAndGroupBy_groupBy = (Core.Name "groupBy")

data ReturnItems = 
  ReturnItemsAsterisk  |
  ReturnItemsItemList ReturnItemList
  deriving (Eq, Ord, Read, Show)

_ReturnItems = (Core.Name "hydra/ext/gql/openGql.ReturnItems")

_ReturnItems_asterisk = (Core.Name "asterisk")

_ReturnItems_itemList = (Core.Name "itemList")

newtype ReturnItemList = 
  ReturnItemList {
    unReturnItemList :: [ReturnItem]}
  deriving (Eq, Ord, Read, Show)

_ReturnItemList = (Core.Name "hydra/ext/gql/openGql.ReturnItemList")

data ReturnItem = 
  ReturnItem {
    returnItemExpression :: AggregatingValueExpression,
    returnItemAlias :: (Maybe ReturnItemAlias)}
  deriving (Eq, Ord, Read, Show)

_ReturnItem = (Core.Name "hydra/ext/gql/openGql.ReturnItem")

_ReturnItem_expression = (Core.Name "expression")

_ReturnItem_alias = (Core.Name "alias")

newtype ReturnItemAlias = 
  ReturnItemAlias {
    unReturnItemAlias :: String}
  deriving (Eq, Ord, Read, Show)

_ReturnItemAlias = (Core.Name "hydra/ext/gql/openGql.ReturnItemAlias")

data SelectStatement = 
  SelectStatement {
    selectStatementQuantifier :: (Maybe SetQuantifier),
    selectStatementItems :: SelectItems,
    selectStatementBody :: (Maybe SelectStatementBodyAndClauses)}
  deriving (Eq, Ord, Read, Show)

_SelectStatement = (Core.Name "hydra/ext/gql/openGql.SelectStatement")

_SelectStatement_quantifier = (Core.Name "quantifier")

_SelectStatement_items = (Core.Name "items")

_SelectStatement_body = (Core.Name "body")

data SelectItems = 
  SelectItemsAsterisk  |
  SelectItemsItemList SelectItemList
  deriving (Eq, Ord, Read, Show)

_SelectItems = (Core.Name "hydra/ext/gql/openGql.SelectItems")

_SelectItems_asterisk = (Core.Name "asterisk")

_SelectItems_itemList = (Core.Name "itemList")

data SelectStatementBodyAndClauses = 
  SelectStatementBodyAndClauses {
    selectStatementBodyAndClausesBody :: SelectStatementBody,
    selectStatementBodyAndClausesWhere :: (Maybe WhereClause),
    selectStatementBodyAndClausesGroupBy :: (Maybe GroupByClause),
    selectStatementBodyAndClausesHaving :: (Maybe HavingClause),
    selectStatementBodyAndClausesOrderBy :: (Maybe OrderByClause),
    selectStatementBodyAndClausesOffset :: (Maybe OffsetClause),
    selectStatementBodyAndClausesLimit :: (Maybe LimitClause)}
  deriving (Eq, Ord, Read, Show)

_SelectStatementBodyAndClauses = (Core.Name "hydra/ext/gql/openGql.SelectStatementBodyAndClauses")

_SelectStatementBodyAndClauses_body = (Core.Name "body")

_SelectStatementBodyAndClauses_where = (Core.Name "where")

_SelectStatementBodyAndClauses_groupBy = (Core.Name "groupBy")

_SelectStatementBodyAndClauses_having = (Core.Name "having")

_SelectStatementBodyAndClauses_orderBy = (Core.Name "orderBy")

_SelectStatementBodyAndClauses_offset = (Core.Name "offset")

_SelectStatementBodyAndClauses_limit = (Core.Name "limit")

newtype SelectItemList = 
  SelectItemList {
    unSelectItemList :: [SelectItem]}
  deriving (Eq, Ord, Read, Show)

_SelectItemList = (Core.Name "hydra/ext/gql/openGql.SelectItemList")

data SelectItem = 
  SelectItem {
    selectItemExpression :: AggregatingValueExpression,
    selectItemAlias :: (Maybe SelectItemAlias)}
  deriving (Eq, Ord, Read, Show)

_SelectItem = (Core.Name "hydra/ext/gql/openGql.SelectItem")

_SelectItem_expression = (Core.Name "expression")

_SelectItem_alias = (Core.Name "alias")

newtype SelectItemAlias = 
  SelectItemAlias {
    unSelectItemAlias :: String}
  deriving (Eq, Ord, Read, Show)

_SelectItemAlias = (Core.Name "hydra/ext/gql/openGql.SelectItemAlias")

newtype HavingClause = 
  HavingClause {
    unHavingClause :: SearchCondition}
  deriving (Eq, Ord, Read, Show)

_HavingClause = (Core.Name "hydra/ext/gql/openGql.HavingClause")

data SelectStatementBody = 
  SelectStatementBodyGraphMatchList SelectGraphMatchList |
  SelectStatementBodyQuerySpecification SelectQuerySpecification
  deriving (Eq, Ord, Read, Show)

_SelectStatementBody = (Core.Name "hydra/ext/gql/openGql.SelectStatementBody")

_SelectStatementBody_graphMatchList = (Core.Name "graphMatchList")

_SelectStatementBody_querySpecification = (Core.Name "querySpecification")

newtype SelectGraphMatchList = 
  SelectGraphMatchList {
    unSelectGraphMatchList :: [SelectGraphMatch]}
  deriving (Eq, Ord, Read, Show)

_SelectGraphMatchList = (Core.Name "hydra/ext/gql/openGql.SelectGraphMatchList")

data SelectGraphMatch = 
  SelectGraphMatch {
    selectGraphMatchGraphExpression :: GraphExpression,
    selectGraphMatchMatchStatement :: MatchStatement}
  deriving (Eq, Ord, Read, Show)

_SelectGraphMatch = (Core.Name "hydra/ext/gql/openGql.SelectGraphMatch")

_SelectGraphMatch_graphExpression = (Core.Name "graphExpression")

_SelectGraphMatch_matchStatement = (Core.Name "matchStatement")

data SelectQuerySpecification = 
  SelectQuerySpecificationNested NestedQuerySpecification |
  SelectQuerySpecificationGraphAndNested GraphAndNestedQuerySpecification
  deriving (Eq, Ord, Read, Show)

_SelectQuerySpecification = (Core.Name "hydra/ext/gql/openGql.SelectQuerySpecification")

_SelectQuerySpecification_nested = (Core.Name "nested")

_SelectQuerySpecification_graphAndNested = (Core.Name "graphAndNested")

data GraphAndNestedQuerySpecification = 
  GraphAndNestedQuerySpecification {
    graphAndNestedQuerySpecificationGraphExpression :: GraphExpression,
    graphAndNestedQuerySpecificationNested :: NestedQuerySpecification}
  deriving (Eq, Ord, Read, Show)

_GraphAndNestedQuerySpecification = (Core.Name "hydra/ext/gql/openGql.GraphAndNestedQuerySpecification")

_GraphAndNestedQuerySpecification_graphExpression = (Core.Name "graphExpression")

_GraphAndNestedQuerySpecification_nested = (Core.Name "nested")

data CallProcedureStatement = 
  CallProcedureStatement {
    callProcedureStatementOptional :: Bool,
    callProcedureStatementCall :: ProcedureCall}
  deriving (Eq, Ord, Read, Show)

_CallProcedureStatement = (Core.Name "hydra/ext/gql/openGql.CallProcedureStatement")

_CallProcedureStatement_optional = (Core.Name "optional")

_CallProcedureStatement_call = (Core.Name "call")

data ProcedureCall = 
  ProcedureCallInline InlineProcedureCall |
  ProcedureCallNamed NamedProcedureCall
  deriving (Eq, Ord, Read, Show)

_ProcedureCall = (Core.Name "hydra/ext/gql/openGql.ProcedureCall")

_ProcedureCall_inline = (Core.Name "inline")

_ProcedureCall_named = (Core.Name "named")

data InlineProcedureCall = 
  InlineProcedureCall {
    inlineProcedureCallScope :: (Maybe VariableScopeClause),
    inlineProcedureCallNested :: NestedProcedureSpecification}
  deriving (Eq, Ord, Read, Show)

_InlineProcedureCall = (Core.Name "hydra/ext/gql/openGql.InlineProcedureCall")

_InlineProcedureCall_scope = (Core.Name "scope")

_InlineProcedureCall_nested = (Core.Name "nested")

newtype VariableScopeClause = 
  VariableScopeClause {
    unVariableScopeClause :: (Maybe BindingVariableReferenceList)}
  deriving (Eq, Ord, Read, Show)

_VariableScopeClause = (Core.Name "hydra/ext/gql/openGql.VariableScopeClause")

newtype BindingVariableReferenceList = 
  BindingVariableReferenceList {
    unBindingVariableReferenceList :: [BindingVariableReference]}
  deriving (Eq, Ord, Read, Show)

_BindingVariableReferenceList = (Core.Name "hydra/ext/gql/openGql.BindingVariableReferenceList")

data NamedProcedureCall = 
  NamedProcedureCall {
    namedProcedureCallReference :: ProcedureReference,
    namedProcedureCallArguments :: (Maybe ProcedureArgumentList),
    namedProcedureCallYield :: (Maybe YieldClause)}
  deriving (Eq, Ord, Read, Show)

_NamedProcedureCall = (Core.Name "hydra/ext/gql/openGql.NamedProcedureCall")

_NamedProcedureCall_reference = (Core.Name "reference")

_NamedProcedureCall_arguments = (Core.Name "arguments")

_NamedProcedureCall_yield = (Core.Name "yield")

newtype ProcedureArgumentList = 
  ProcedureArgumentList {
    unProcedureArgumentList :: [ProcedureArgument]}
  deriving (Eq, Ord, Read, Show)

_ProcedureArgumentList = (Core.Name "hydra/ext/gql/openGql.ProcedureArgumentList")

newtype ProcedureArgument = 
  ProcedureArgument {
    unProcedureArgument :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ProcedureArgument = (Core.Name "hydra/ext/gql/openGql.ProcedureArgument")

newtype AtSchemaClause = 
  AtSchemaClause {
    unAtSchemaClause :: SchemaReference}
  deriving (Eq, Ord, Read, Show)

_AtSchemaClause = (Core.Name "hydra/ext/gql/openGql.AtSchemaClause")

newtype UseGraphClause = 
  UseGraphClause {
    unUseGraphClause :: GraphExpression}
  deriving (Eq, Ord, Read, Show)

_UseGraphClause = (Core.Name "hydra/ext/gql/openGql.UseGraphClause")

data GraphPatternBindingTable = 
  GraphPatternBindingTable {
    graphPatternBindingTablePattern :: GraphPattern,
    graphPatternBindingTableYieldClause :: (Maybe GraphPatternYieldClause)}
  deriving (Eq, Ord, Read, Show)

_GraphPatternBindingTable = (Core.Name "hydra/ext/gql/openGql.GraphPatternBindingTable")

_GraphPatternBindingTable_pattern = (Core.Name "pattern")

_GraphPatternBindingTable_yieldClause = (Core.Name "yieldClause")

newtype GraphPatternYieldClause = 
  GraphPatternYieldClause {
    unGraphPatternYieldClause :: GraphPatternYieldItemList}
  deriving (Eq, Ord, Read, Show)

_GraphPatternYieldClause = (Core.Name "hydra/ext/gql/openGql.GraphPatternYieldClause")

data GraphPatternYieldItemList = 
  GraphPatternYieldItemListItems [GraphPatternYieldItem] |
  GraphPatternYieldItemListNoBindings 
  deriving (Eq, Ord, Read, Show)

_GraphPatternYieldItemList = (Core.Name "hydra/ext/gql/openGql.GraphPatternYieldItemList")

_GraphPatternYieldItemList_items = (Core.Name "items")

_GraphPatternYieldItemList_noBindings = (Core.Name "noBindings")

newtype GraphPatternYieldItem = 
  GraphPatternYieldItem {
    unGraphPatternYieldItem :: BindingVariableReference}
  deriving (Eq, Ord, Read, Show)

_GraphPatternYieldItem = (Core.Name "hydra/ext/gql/openGql.GraphPatternYieldItem")

data GraphPattern = 
  GraphPattern {
    graphPatternMatchMode :: (Maybe MatchMode),
    graphPatternPathPatterns :: PathPatternList,
    graphPatternKeepClause :: (Maybe KeepClause),
    graphPatternWhereClause :: (Maybe GraphPatternWhereClause)}
  deriving (Eq, Ord, Read, Show)

_GraphPattern = (Core.Name "hydra/ext/gql/openGql.GraphPattern")

_GraphPattern_matchMode = (Core.Name "matchMode")

_GraphPattern_pathPatterns = (Core.Name "pathPatterns")

_GraphPattern_keepClause = (Core.Name "keepClause")

_GraphPattern_whereClause = (Core.Name "whereClause")

data MatchMode = 
  MatchModeRepeatableElements RepeatableElementsMatchMode |
  MatchModeDifferentEdges DifferentEdgesMatchMode
  deriving (Eq, Ord, Read, Show)

_MatchMode = (Core.Name "hydra/ext/gql/openGql.MatchMode")

_MatchMode_repeatableElements = (Core.Name "repeatableElements")

_MatchMode_differentEdges = (Core.Name "differentEdges")

newtype RepeatableElementsMatchMode = 
  RepeatableElementsMatchMode {
    unRepeatableElementsMatchMode :: ElementBindingsOrElements}
  deriving (Eq, Ord, Read, Show)

_RepeatableElementsMatchMode = (Core.Name "hydra/ext/gql/openGql.RepeatableElementsMatchMode")

newtype DifferentEdgesMatchMode = 
  DifferentEdgesMatchMode {
    unDifferentEdgesMatchMode :: EdgeBindingsOrEdges}
  deriving (Eq, Ord, Read, Show)

_DifferentEdgesMatchMode = (Core.Name "hydra/ext/gql/openGql.DifferentEdgesMatchMode")

data ElementBindingsOrElements = 
  ElementBindingsOrElementsElementBindings Bool |
  ElementBindingsOrElementsElements 
  deriving (Eq, Ord, Read, Show)

_ElementBindingsOrElements = (Core.Name "hydra/ext/gql/openGql.ElementBindingsOrElements")

_ElementBindingsOrElements_elementBindings = (Core.Name "elementBindings")

_ElementBindingsOrElements_elements = (Core.Name "elements")

data EdgeBindingsOrEdges = 
  EdgeBindingsOrEdgesEdgeBindings Bool |
  EdgeBindingsOrEdgesEdges 
  deriving (Eq, Ord, Read, Show)

_EdgeBindingsOrEdges = (Core.Name "hydra/ext/gql/openGql.EdgeBindingsOrEdges")

_EdgeBindingsOrEdges_edgeBindings = (Core.Name "edgeBindings")

_EdgeBindingsOrEdges_edges = (Core.Name "edges")

newtype PathPatternList = 
  PathPatternList {
    unPathPatternList :: [PathPattern]}
  deriving (Eq, Ord, Read, Show)

_PathPatternList = (Core.Name "hydra/ext/gql/openGql.PathPatternList")

data PathPattern = 
  PathPattern {
    pathPatternVariableDeclaration :: (Maybe PathVariableDeclaration),
    pathPatternPrefix :: (Maybe PathPatternPrefix),
    pathPatternExpression :: PathPatternExpression}
  deriving (Eq, Ord, Read, Show)

_PathPattern = (Core.Name "hydra/ext/gql/openGql.PathPattern")

_PathPattern_variableDeclaration = (Core.Name "variableDeclaration")

_PathPattern_prefix = (Core.Name "prefix")

_PathPattern_expression = (Core.Name "expression")

newtype PathVariableDeclaration = 
  PathVariableDeclaration {
    unPathVariableDeclaration :: PathVariable}
  deriving (Eq, Ord, Read, Show)

_PathVariableDeclaration = (Core.Name "hydra/ext/gql/openGql.PathVariableDeclaration")

newtype KeepClause = 
  KeepClause {
    unKeepClause :: PathPatternPrefix}
  deriving (Eq, Ord, Read, Show)

_KeepClause = (Core.Name "hydra/ext/gql/openGql.KeepClause")

newtype GraphPatternWhereClause = 
  GraphPatternWhereClause {
    unGraphPatternWhereClause :: SearchCondition}
  deriving (Eq, Ord, Read, Show)

_GraphPatternWhereClause = (Core.Name "hydra/ext/gql/openGql.GraphPatternWhereClause")

newtype InsertGraphPattern = 
  InsertGraphPattern {
    unInsertGraphPattern :: InsertPathPatternList}
  deriving (Eq, Ord, Read, Show)

_InsertGraphPattern = (Core.Name "hydra/ext/gql/openGql.InsertGraphPattern")

newtype InsertPathPatternList = 
  InsertPathPatternList {
    unInsertPathPatternList :: [InsertPathPattern]}
  deriving (Eq, Ord, Read, Show)

_InsertPathPatternList = (Core.Name "hydra/ext/gql/openGql.InsertPathPatternList")

data InsertPathPattern = 
  InsertPathPattern {
    insertPathPatternStartNode :: InsertNodePattern,
    insertPathPatternEdgesAndNodes :: [InsertEdgeAndNode]}
  deriving (Eq, Ord, Read, Show)

_InsertPathPattern = (Core.Name "hydra/ext/gql/openGql.InsertPathPattern")

_InsertPathPattern_startNode = (Core.Name "startNode")

_InsertPathPattern_edgesAndNodes = (Core.Name "edgesAndNodes")

data InsertEdgeAndNode = 
  InsertEdgeAndNode {
    insertEdgeAndNodeEdge :: InsertEdgePattern,
    insertEdgeAndNodeNode :: InsertNodePattern}
  deriving (Eq, Ord, Read, Show)

_InsertEdgeAndNode = (Core.Name "hydra/ext/gql/openGql.InsertEdgeAndNode")

_InsertEdgeAndNode_edge = (Core.Name "edge")

_InsertEdgeAndNode_node = (Core.Name "node")

newtype InsertNodePattern = 
  InsertNodePattern {
    unInsertNodePattern :: (Maybe InsertElementPatternFiller)}
  deriving (Eq, Ord, Read, Show)

_InsertNodePattern = (Core.Name "hydra/ext/gql/openGql.InsertNodePattern")

data InsertEdgePattern = 
  InsertEdgePatternPointingLeft InsertEdgePointingLeft |
  InsertEdgePatternPointingRight InsertEdgePointingRight |
  InsertEdgePatternUndirected InsertEdgeUndirected
  deriving (Eq, Ord, Read, Show)

_InsertEdgePattern = (Core.Name "hydra/ext/gql/openGql.InsertEdgePattern")

_InsertEdgePattern_pointingLeft = (Core.Name "pointingLeft")

_InsertEdgePattern_pointingRight = (Core.Name "pointingRight")

_InsertEdgePattern_undirected = (Core.Name "undirected")

newtype InsertEdgePointingLeft = 
  InsertEdgePointingLeft {
    unInsertEdgePointingLeft :: (Maybe InsertElementPatternFiller)}
  deriving (Eq, Ord, Read, Show)

_InsertEdgePointingLeft = (Core.Name "hydra/ext/gql/openGql.InsertEdgePointingLeft")

newtype InsertEdgePointingRight = 
  InsertEdgePointingRight {
    unInsertEdgePointingRight :: (Maybe InsertElementPatternFiller)}
  deriving (Eq, Ord, Read, Show)

_InsertEdgePointingRight = (Core.Name "hydra/ext/gql/openGql.InsertEdgePointingRight")

newtype InsertEdgeUndirected = 
  InsertEdgeUndirected {
    unInsertEdgeUndirected :: (Maybe InsertElementPatternFiller)}
  deriving (Eq, Ord, Read, Show)

_InsertEdgeUndirected = (Core.Name "hydra/ext/gql/openGql.InsertEdgeUndirected")

data InsertElementPatternFiller = 
  InsertElementPatternFiller {
    insertElementPatternFillerVariableDeclaration :: (Maybe ElementVariableDeclaration),
    insertElementPatternFillerLabelAndProperties :: (Maybe LabelAndPropertySetSpecification)}
  deriving (Eq, Ord, Read, Show)

_InsertElementPatternFiller = (Core.Name "hydra/ext/gql/openGql.InsertElementPatternFiller")

_InsertElementPatternFiller_variableDeclaration = (Core.Name "variableDeclaration")

_InsertElementPatternFiller_labelAndProperties = (Core.Name "labelAndProperties")

data LabelAndPropertySetSpecification = 
  LabelAndPropertySetSpecification {
    labelAndPropertySetSpecificationIsOrColon :: (Maybe IsOrColon),
    labelAndPropertySetSpecificationLabelSet :: (Maybe LabelSetSpecification),
    labelAndPropertySetSpecificationPropertySpecification :: (Maybe ElementPropertySpecification)}
  deriving (Eq, Ord, Read, Show)

_LabelAndPropertySetSpecification = (Core.Name "hydra/ext/gql/openGql.LabelAndPropertySetSpecification")

_LabelAndPropertySetSpecification_isOrColon = (Core.Name "isOrColon")

_LabelAndPropertySetSpecification_labelSet = (Core.Name "labelSet")

_LabelAndPropertySetSpecification_propertySpecification = (Core.Name "propertySpecification")

data PathPatternPrefix = 
  PathPatternPrefixModePrefix PathModePrefix |
  PathPatternPrefixSearchPrefix PathSearchPrefix
  deriving (Eq, Ord, Read, Show)

_PathPatternPrefix = (Core.Name "hydra/ext/gql/openGql.PathPatternPrefix")

_PathPatternPrefix_modePrefix = (Core.Name "modePrefix")

_PathPatternPrefix_searchPrefix = (Core.Name "searchPrefix")

data PathModePrefix = 
  PathModePrefix {
    pathModePrefixMode :: PathMode,
    pathModePrefixOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_PathModePrefix = (Core.Name "hydra/ext/gql/openGql.PathModePrefix")

_PathModePrefix_mode = (Core.Name "mode")

_PathModePrefix_orPaths = (Core.Name "orPaths")

data PathMode = 
  PathModeWalk  |
  PathModeTrail  |
  PathModeSimple  |
  PathModeAcyclic 
  deriving (Eq, Ord, Read, Show)

_PathMode = (Core.Name "hydra/ext/gql/openGql.PathMode")

_PathMode_walk = (Core.Name "walk")

_PathMode_trail = (Core.Name "trail")

_PathMode_simple = (Core.Name "simple")

_PathMode_acyclic = (Core.Name "acyclic")

data PathSearchPrefix = 
  PathSearchPrefixAll AllPathSearch |
  PathSearchPrefixAny AnyPathSearch |
  PathSearchPrefixShortest ShortestPathSearch
  deriving (Eq, Ord, Read, Show)

_PathSearchPrefix = (Core.Name "hydra/ext/gql/openGql.PathSearchPrefix")

_PathSearchPrefix_all = (Core.Name "all")

_PathSearchPrefix_any = (Core.Name "any")

_PathSearchPrefix_shortest = (Core.Name "shortest")

data AllPathSearch = 
  AllPathSearch {
    allPathSearchMode :: (Maybe PathMode),
    allPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AllPathSearch = (Core.Name "hydra/ext/gql/openGql.AllPathSearch")

_AllPathSearch_mode = (Core.Name "mode")

_AllPathSearch_orPaths = (Core.Name "orPaths")

data PathOrPaths = 
  PathOrPathsPath  |
  PathOrPathsPaths 
  deriving (Eq, Ord, Read, Show)

_PathOrPaths = (Core.Name "hydra/ext/gql/openGql.PathOrPaths")

_PathOrPaths_path = (Core.Name "path")

_PathOrPaths_paths = (Core.Name "paths")

data AnyPathSearch = 
  AnyPathSearch {
    anyPathSearchNumberOfPaths :: (Maybe NumberOfPaths),
    anyPathSearchMode :: (Maybe PathMode),
    anyPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AnyPathSearch = (Core.Name "hydra/ext/gql/openGql.AnyPathSearch")

_AnyPathSearch_numberOfPaths = (Core.Name "numberOfPaths")

_AnyPathSearch_mode = (Core.Name "mode")

_AnyPathSearch_orPaths = (Core.Name "orPaths")

newtype NumberOfPaths = 
  NumberOfPaths {
    unNumberOfPaths :: NonNegativeIntegerSpecification}
  deriving (Eq, Ord, Read, Show)

_NumberOfPaths = (Core.Name "hydra/ext/gql/openGql.NumberOfPaths")

data ShortestPathSearch = 
  ShortestPathSearchAllShortest AllShortestPathSearch |
  ShortestPathSearchAnyShortest AnyShortestPathSearch |
  ShortestPathSearchCountedShortest CountedShortestPathSearch |
  ShortestPathSearchCountedShortestGroup CountedShortestGroupSearch
  deriving (Eq, Ord, Read, Show)

_ShortestPathSearch = (Core.Name "hydra/ext/gql/openGql.ShortestPathSearch")

_ShortestPathSearch_allShortest = (Core.Name "allShortest")

_ShortestPathSearch_anyShortest = (Core.Name "anyShortest")

_ShortestPathSearch_countedShortest = (Core.Name "countedShortest")

_ShortestPathSearch_countedShortestGroup = (Core.Name "countedShortestGroup")

data AllShortestPathSearch = 
  AllShortestPathSearch {
    allShortestPathSearchMode :: (Maybe PathMode),
    allShortestPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AllShortestPathSearch = (Core.Name "hydra/ext/gql/openGql.AllShortestPathSearch")

_AllShortestPathSearch_mode = (Core.Name "mode")

_AllShortestPathSearch_orPaths = (Core.Name "orPaths")

data AnyShortestPathSearch = 
  AnyShortestPathSearch {
    anyShortestPathSearchMode :: (Maybe PathMode),
    anyShortestPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AnyShortestPathSearch = (Core.Name "hydra/ext/gql/openGql.AnyShortestPathSearch")

_AnyShortestPathSearch_mode = (Core.Name "mode")

_AnyShortestPathSearch_orPaths = (Core.Name "orPaths")

data CountedShortestPathSearch = 
  CountedShortestPathSearch {
    countedShortestPathSearchNumberOfPaths :: NumberOfPaths,
    countedShortestPathSearchMode :: (Maybe PathMode),
    countedShortestPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_CountedShortestPathSearch = (Core.Name "hydra/ext/gql/openGql.CountedShortestPathSearch")

_CountedShortestPathSearch_numberOfPaths = (Core.Name "numberOfPaths")

_CountedShortestPathSearch_mode = (Core.Name "mode")

_CountedShortestPathSearch_orPaths = (Core.Name "orPaths")

data CountedShortestGroupSearch = 
  CountedShortestGroupSearch {
    countedShortestGroupSearchNumberOfGroups :: (Maybe NumberOfGroups),
    countedShortestGroupSearchMode :: (Maybe PathMode),
    countedShortestGroupSearchOrPaths :: (Maybe PathOrPaths),
    countedShortestGroupSearchGroups :: Bool}
  deriving (Eq, Ord, Read, Show)

_CountedShortestGroupSearch = (Core.Name "hydra/ext/gql/openGql.CountedShortestGroupSearch")

_CountedShortestGroupSearch_numberOfGroups = (Core.Name "numberOfGroups")

_CountedShortestGroupSearch_mode = (Core.Name "mode")

_CountedShortestGroupSearch_orPaths = (Core.Name "orPaths")

_CountedShortestGroupSearch_groups = (Core.Name "groups")

newtype NumberOfGroups = 
  NumberOfGroups {
    unNumberOfGroups :: NonNegativeIntegerSpecification}
  deriving (Eq, Ord, Read, Show)

_NumberOfGroups = (Core.Name "hydra/ext/gql/openGql.NumberOfGroups")

data PathPatternExpression = 
  PathPatternExpressionTerm PathTerm |
  PathPatternExpressionMultisetAlternation [PathTerm] |
  PathPatternExpressionPatternUnion [PathTerm]
  deriving (Eq, Ord, Read, Show)

_PathPatternExpression = (Core.Name "hydra/ext/gql/openGql.PathPatternExpression")

_PathPatternExpression_term = (Core.Name "term")

_PathPatternExpression_multisetAlternation = (Core.Name "multisetAlternation")

_PathPatternExpression_patternUnion = (Core.Name "patternUnion")

newtype PathTerm = 
  PathTerm {
    unPathTerm :: [PathFactor]}
  deriving (Eq, Ord, Read, Show)

_PathTerm = (Core.Name "hydra/ext/gql/openGql.PathTerm")

data PathFactor = 
  PathFactorPrimary PathPrimary |
  PathFactorQuantifiedPrimary QuantifiedPathPrimary |
  PathFactorQuestionedPrimary QuestionedPathPrimary
  deriving (Eq, Ord, Read, Show)

_PathFactor = (Core.Name "hydra/ext/gql/openGql.PathFactor")

_PathFactor_primary = (Core.Name "primary")

_PathFactor_quantifiedPrimary = (Core.Name "quantifiedPrimary")

_PathFactor_questionedPrimary = (Core.Name "questionedPrimary")

data QuantifiedPathPrimary = 
  QuantifiedPathPrimary {
    quantifiedPathPrimaryPrimary :: PathPrimary,
    quantifiedPathPrimaryQuantifier :: GraphPatternQuantifier}
  deriving (Eq, Ord, Read, Show)

_QuantifiedPathPrimary = (Core.Name "hydra/ext/gql/openGql.QuantifiedPathPrimary")

_QuantifiedPathPrimary_primary = (Core.Name "primary")

_QuantifiedPathPrimary_quantifier = (Core.Name "quantifier")

newtype QuestionedPathPrimary = 
  QuestionedPathPrimary {
    unQuestionedPathPrimary :: PathPrimary}
  deriving (Eq, Ord, Read, Show)

_QuestionedPathPrimary = (Core.Name "hydra/ext/gql/openGql.QuestionedPathPrimary")

data PathPrimary = 
  PathPrimaryElementPattern ElementPattern |
  PathPrimaryParenthesizedExpression ParenthesizedPathPatternExpression |
  PathPrimarySimplifiedExpression SimplifiedPathPatternExpression
  deriving (Eq, Ord, Read, Show)

_PathPrimary = (Core.Name "hydra/ext/gql/openGql.PathPrimary")

_PathPrimary_elementPattern = (Core.Name "elementPattern")

_PathPrimary_parenthesizedExpression = (Core.Name "parenthesizedExpression")

_PathPrimary_simplifiedExpression = (Core.Name "simplifiedExpression")

data ElementPattern = 
  ElementPatternNode NodePattern |
  ElementPatternEdge EdgePattern
  deriving (Eq, Ord, Read, Show)

_ElementPattern = (Core.Name "hydra/ext/gql/openGql.ElementPattern")

_ElementPattern_node = (Core.Name "node")

_ElementPattern_edge = (Core.Name "edge")

newtype NodePattern = 
  NodePattern {
    unNodePattern :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_NodePattern = (Core.Name "hydra/ext/gql/openGql.NodePattern")

data ElementPatternFiller = 
  ElementPatternFiller {
    elementPatternFillerVariableDeclaration :: (Maybe ElementVariableDeclaration),
    elementPatternFillerIsLabelExpression :: (Maybe IsLabelExpression),
    elementPatternFillerPredicate :: (Maybe ElementPatternPredicate)}
  deriving (Eq, Ord, Read, Show)

_ElementPatternFiller = (Core.Name "hydra/ext/gql/openGql.ElementPatternFiller")

_ElementPatternFiller_variableDeclaration = (Core.Name "variableDeclaration")

_ElementPatternFiller_isLabelExpression = (Core.Name "isLabelExpression")

_ElementPatternFiller_predicate = (Core.Name "predicate")

data ElementVariableDeclaration = 
  ElementVariableDeclaration {
    elementVariableDeclarationTemp :: (Maybe Bool),
    elementVariableDeclarationVariable :: ElementVariable}
  deriving (Eq, Ord, Read, Show)

_ElementVariableDeclaration = (Core.Name "hydra/ext/gql/openGql.ElementVariableDeclaration")

_ElementVariableDeclaration_temp = (Core.Name "temp")

_ElementVariableDeclaration_variable = (Core.Name "variable")

data IsLabelExpression = 
  IsLabelExpression {
    isLabelExpressionIsOrColon :: IsOrColon,
    isLabelExpressionLabel :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_IsLabelExpression = (Core.Name "hydra/ext/gql/openGql.IsLabelExpression")

_IsLabelExpression_isOrColon = (Core.Name "isOrColon")

_IsLabelExpression_label = (Core.Name "label")

data IsOrColon = 
  IsOrColonIs  |
  IsOrColonColon 
  deriving (Eq, Ord, Read, Show)

_IsOrColon = (Core.Name "hydra/ext/gql/openGql.IsOrColon")

_IsOrColon_is = (Core.Name "is")

_IsOrColon_colon = (Core.Name "colon")

data ElementPatternPredicate = 
  ElementPatternPredicateWhereClause ElementPatternWhereClause |
  ElementPatternPredicatePropertySpecification ElementPropertySpecification
  deriving (Eq, Ord, Read, Show)

_ElementPatternPredicate = (Core.Name "hydra/ext/gql/openGql.ElementPatternPredicate")

_ElementPatternPredicate_whereClause = (Core.Name "whereClause")

_ElementPatternPredicate_propertySpecification = (Core.Name "propertySpecification")

newtype ElementPatternWhereClause = 
  ElementPatternWhereClause {
    unElementPatternWhereClause :: SearchCondition}
  deriving (Eq, Ord, Read, Show)

_ElementPatternWhereClause = (Core.Name "hydra/ext/gql/openGql.ElementPatternWhereClause")

newtype ElementPropertySpecification = 
  ElementPropertySpecification {
    unElementPropertySpecification :: PropertyKeyValuePairList}
  deriving (Eq, Ord, Read, Show)

_ElementPropertySpecification = (Core.Name "hydra/ext/gql/openGql.ElementPropertySpecification")

newtype PropertyKeyValuePairList = 
  PropertyKeyValuePairList {
    unPropertyKeyValuePairList :: [PropertyKeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyValuePairList = (Core.Name "hydra/ext/gql/openGql.PropertyKeyValuePairList")

data PropertyKeyValuePair = 
  PropertyKeyValuePair {
    propertyKeyValuePairName :: PropertyName,
    propertyKeyValuePairValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyValuePair = (Core.Name "hydra/ext/gql/openGql.PropertyKeyValuePair")

_PropertyKeyValuePair_name = (Core.Name "name")

_PropertyKeyValuePair_value = (Core.Name "value")

data EdgePattern = 
  EdgePatternFullEdge FullEdgePattern |
  EdgePatternAbbreviatedEdge AbbreviatedEdgePattern
  deriving (Eq, Ord, Read, Show)

_EdgePattern = (Core.Name "hydra/ext/gql/openGql.EdgePattern")

_EdgePattern_fullEdge = (Core.Name "fullEdge")

_EdgePattern_abbreviatedEdge = (Core.Name "abbreviatedEdge")

data FullEdgePattern = 
  FullEdgePatternPointingLeft FullEdgePointingLeft |
  FullEdgePatternUndirected FullEdgeUndirected |
  FullEdgePatternPointingRight FullEdgePointingRight |
  FullEdgePatternLeftOrUndirected FullEdgeLeftOrUndirected |
  FullEdgePatternUndirectedOrRight FullEdgeUndirectedOrRight |
  FullEdgePatternLeftOrRight FullEdgeLeftOrRight |
  FullEdgePatternAnyDirection FullEdgeAnyDirection
  deriving (Eq, Ord, Read, Show)

_FullEdgePattern = (Core.Name "hydra/ext/gql/openGql.FullEdgePattern")

_FullEdgePattern_pointingLeft = (Core.Name "pointingLeft")

_FullEdgePattern_undirected = (Core.Name "undirected")

_FullEdgePattern_pointingRight = (Core.Name "pointingRight")

_FullEdgePattern_leftOrUndirected = (Core.Name "leftOrUndirected")

_FullEdgePattern_undirectedOrRight = (Core.Name "undirectedOrRight")

_FullEdgePattern_leftOrRight = (Core.Name "leftOrRight")

_FullEdgePattern_anyDirection = (Core.Name "anyDirection")

newtype FullEdgePointingLeft = 
  FullEdgePointingLeft {
    unFullEdgePointingLeft :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgePointingLeft = (Core.Name "hydra/ext/gql/openGql.FullEdgePointingLeft")

newtype FullEdgeUndirected = 
  FullEdgeUndirected {
    unFullEdgeUndirected :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgeUndirected = (Core.Name "hydra/ext/gql/openGql.FullEdgeUndirected")

newtype FullEdgePointingRight = 
  FullEdgePointingRight {
    unFullEdgePointingRight :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgePointingRight = (Core.Name "hydra/ext/gql/openGql.FullEdgePointingRight")

newtype FullEdgeLeftOrUndirected = 
  FullEdgeLeftOrUndirected {
    unFullEdgeLeftOrUndirected :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgeLeftOrUndirected = (Core.Name "hydra/ext/gql/openGql.FullEdgeLeftOrUndirected")

newtype FullEdgeUndirectedOrRight = 
  FullEdgeUndirectedOrRight {
    unFullEdgeUndirectedOrRight :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgeUndirectedOrRight = (Core.Name "hydra/ext/gql/openGql.FullEdgeUndirectedOrRight")

newtype FullEdgeLeftOrRight = 
  FullEdgeLeftOrRight {
    unFullEdgeLeftOrRight :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgeLeftOrRight = (Core.Name "hydra/ext/gql/openGql.FullEdgeLeftOrRight")

newtype FullEdgeAnyDirection = 
  FullEdgeAnyDirection {
    unFullEdgeAnyDirection :: ElementPatternFiller}
  deriving (Eq, Ord, Read, Show)

_FullEdgeAnyDirection = (Core.Name "hydra/ext/gql/openGql.FullEdgeAnyDirection")

data AbbreviatedEdgePattern = 
  AbbreviatedEdgePatternLeftArrow  |
  AbbreviatedEdgePatternTilde  |
  AbbreviatedEdgePatternRightArrow  |
  AbbreviatedEdgePatternLeftArrowTilde  |
  AbbreviatedEdgePatternTildeRightArrow  |
  AbbreviatedEdgePatternLeftMinusRight  |
  AbbreviatedEdgePatternMinusSign 
  deriving (Eq, Ord, Read, Show)

_AbbreviatedEdgePattern = (Core.Name "hydra/ext/gql/openGql.AbbreviatedEdgePattern")

_AbbreviatedEdgePattern_leftArrow = (Core.Name "leftArrow")

_AbbreviatedEdgePattern_tilde = (Core.Name "tilde")

_AbbreviatedEdgePattern_rightArrow = (Core.Name "rightArrow")

_AbbreviatedEdgePattern_leftArrowTilde = (Core.Name "leftArrowTilde")

_AbbreviatedEdgePattern_tildeRightArrow = (Core.Name "tildeRightArrow")

_AbbreviatedEdgePattern_leftMinusRight = (Core.Name "leftMinusRight")

_AbbreviatedEdgePattern_minusSign = (Core.Name "minusSign")

data ParenthesizedPathPatternExpression = 
  ParenthesizedPathPatternExpression {
    parenthesizedPathPatternExpressionSubpathDeclaration :: (Maybe SubpathVariableDeclaration),
    parenthesizedPathPatternExpressionPathMode :: (Maybe PathModePrefix),
    parenthesizedPathPatternExpressionExpression :: PathPatternExpression,
    parenthesizedPathPatternExpressionWhereClause :: (Maybe ParenthesizedPathPatternWhereClause)}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedPathPatternExpression = (Core.Name "hydra/ext/gql/openGql.ParenthesizedPathPatternExpression")

_ParenthesizedPathPatternExpression_subpathDeclaration = (Core.Name "subpathDeclaration")

_ParenthesizedPathPatternExpression_pathMode = (Core.Name "pathMode")

_ParenthesizedPathPatternExpression_expression = (Core.Name "expression")

_ParenthesizedPathPatternExpression_whereClause = (Core.Name "whereClause")

newtype SubpathVariableDeclaration = 
  SubpathVariableDeclaration {
    unSubpathVariableDeclaration :: SubpathVariable}
  deriving (Eq, Ord, Read, Show)

_SubpathVariableDeclaration = (Core.Name "hydra/ext/gql/openGql.SubpathVariableDeclaration")

newtype ParenthesizedPathPatternWhereClause = 
  ParenthesizedPathPatternWhereClause {
    unParenthesizedPathPatternWhereClause :: SearchCondition}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedPathPatternWhereClause = (Core.Name "hydra/ext/gql/openGql.ParenthesizedPathPatternWhereClause")

data LabelExpression = 
  LabelExpressionNegation LabelExpression |
  LabelExpressionConjunction ConjunctionLabelExpression |
  LabelExpressionDisjunction DisjunctionLabelExpression |
  LabelExpressionName LabelName |
  LabelExpressionWildcard  |
  LabelExpressionParenthesized LabelExpression
  deriving (Eq, Ord, Read, Show)

_LabelExpression = (Core.Name "hydra/ext/gql/openGql.LabelExpression")

_LabelExpression_negation = (Core.Name "negation")

_LabelExpression_conjunction = (Core.Name "conjunction")

_LabelExpression_disjunction = (Core.Name "disjunction")

_LabelExpression_name = (Core.Name "name")

_LabelExpression_wildcard = (Core.Name "wildcard")

_LabelExpression_parenthesized = (Core.Name "parenthesized")

data ConjunctionLabelExpression = 
  ConjunctionLabelExpression {
    conjunctionLabelExpressionLeft :: LabelExpression,
    conjunctionLabelExpressionRight :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_ConjunctionLabelExpression = (Core.Name "hydra/ext/gql/openGql.ConjunctionLabelExpression")

_ConjunctionLabelExpression_left = (Core.Name "left")

_ConjunctionLabelExpression_right = (Core.Name "right")

data DisjunctionLabelExpression = 
  DisjunctionLabelExpression {
    disjunctionLabelExpressionLeft :: LabelExpression,
    disjunctionLabelExpressionRight :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_DisjunctionLabelExpression = (Core.Name "hydra/ext/gql/openGql.DisjunctionLabelExpression")

_DisjunctionLabelExpression_left = (Core.Name "left")

_DisjunctionLabelExpression_right = (Core.Name "right")

newtype PathVariableReference = 
  PathVariableReference {
    unPathVariableReference :: BindingVariableReference}
  deriving (Eq, Ord, Read, Show)

_PathVariableReference = (Core.Name "hydra/ext/gql/openGql.PathVariableReference")

newtype ElementVariableReference = 
  ElementVariableReference {
    unElementVariableReference :: BindingVariableReference}
  deriving (Eq, Ord, Read, Show)

_ElementVariableReference = (Core.Name "hydra/ext/gql/openGql.ElementVariableReference")

data GraphPatternQuantifier = 
  GraphPatternQuantifierAsterisk  |
  GraphPatternQuantifierPlusSign  |
  GraphPatternQuantifierFixed FixedQuantifier |
  GraphPatternQuantifierGeneral GeneralQuantifier
  deriving (Eq, Ord, Read, Show)

_GraphPatternQuantifier = (Core.Name "hydra/ext/gql/openGql.GraphPatternQuantifier")

_GraphPatternQuantifier_asterisk = (Core.Name "asterisk")

_GraphPatternQuantifier_plusSign = (Core.Name "plusSign")

_GraphPatternQuantifier_fixed = (Core.Name "fixed")

_GraphPatternQuantifier_general = (Core.Name "general")

newtype FixedQuantifier = 
  FixedQuantifier {
    unFixedQuantifier :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_FixedQuantifier = (Core.Name "hydra/ext/gql/openGql.FixedQuantifier")

data GeneralQuantifier = 
  GeneralQuantifier {
    generalQuantifierLowerBound :: (Maybe LowerBound),
    generalQuantifierUpperBound :: (Maybe UpperBound)}
  deriving (Eq, Ord, Read, Show)

_GeneralQuantifier = (Core.Name "hydra/ext/gql/openGql.GeneralQuantifier")

_GeneralQuantifier_lowerBound = (Core.Name "lowerBound")

_GeneralQuantifier_upperBound = (Core.Name "upperBound")

newtype LowerBound = 
  LowerBound {
    unLowerBound :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_LowerBound = (Core.Name "hydra/ext/gql/openGql.LowerBound")

newtype UpperBound = 
  UpperBound {
    unUpperBound :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_UpperBound = (Core.Name "hydra/ext/gql/openGql.UpperBound")

data SimplifiedPathPatternExpression = 
  SimplifiedPathPatternExpressionLeft SimplifiedDefaultingLeft |
  SimplifiedPathPatternExpressionUndirected SimplifiedDefaultingUndirected |
  SimplifiedPathPatternExpressionRight SimplifiedDefaultingRight |
  SimplifiedPathPatternExpressionLeftOrUndirected SimplifiedDefaultingLeftOrUndirected |
  SimplifiedPathPatternExpressionUndirectedOrRight SimplifiedDefaultingUndirectedOrRight |
  SimplifiedPathPatternExpressionLeftOrRight SimplifiedDefaultingLeftOrRight |
  SimplifiedPathPatternExpressionAnyDirection SimplifiedDefaultingAnyDirection
  deriving (Eq, Ord, Read, Show)

_SimplifiedPathPatternExpression = (Core.Name "hydra/ext/gql/openGql.SimplifiedPathPatternExpression")

_SimplifiedPathPatternExpression_left = (Core.Name "left")

_SimplifiedPathPatternExpression_undirected = (Core.Name "undirected")

_SimplifiedPathPatternExpression_right = (Core.Name "right")

_SimplifiedPathPatternExpression_leftOrUndirected = (Core.Name "leftOrUndirected")

_SimplifiedPathPatternExpression_undirectedOrRight = (Core.Name "undirectedOrRight")

_SimplifiedPathPatternExpression_leftOrRight = (Core.Name "leftOrRight")

_SimplifiedPathPatternExpression_anyDirection = (Core.Name "anyDirection")

newtype SimplifiedDefaultingLeft = 
  SimplifiedDefaultingLeft {
    unSimplifiedDefaultingLeft :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingLeft = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingLeft")

newtype SimplifiedDefaultingUndirected = 
  SimplifiedDefaultingUndirected {
    unSimplifiedDefaultingUndirected :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingUndirected = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingUndirected")

newtype SimplifiedDefaultingRight = 
  SimplifiedDefaultingRight {
    unSimplifiedDefaultingRight :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingRight = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingRight")

newtype SimplifiedDefaultingLeftOrUndirected = 
  SimplifiedDefaultingLeftOrUndirected {
    unSimplifiedDefaultingLeftOrUndirected :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingLeftOrUndirected = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingLeftOrUndirected")

newtype SimplifiedDefaultingUndirectedOrRight = 
  SimplifiedDefaultingUndirectedOrRight {
    unSimplifiedDefaultingUndirectedOrRight :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingUndirectedOrRight = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingUndirectedOrRight")

newtype SimplifiedDefaultingLeftOrRight = 
  SimplifiedDefaultingLeftOrRight {
    unSimplifiedDefaultingLeftOrRight :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingLeftOrRight = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingLeftOrRight")

newtype SimplifiedDefaultingAnyDirection = 
  SimplifiedDefaultingAnyDirection {
    unSimplifiedDefaultingAnyDirection :: SimplifiedContents}
  deriving (Eq, Ord, Read, Show)

_SimplifiedDefaultingAnyDirection = (Core.Name "hydra/ext/gql/openGql.SimplifiedDefaultingAnyDirection")

data SimplifiedContents = 
  SimplifiedContentsTerm SimplifiedTerm |
  SimplifiedContentsPathUnion SimplifiedPathUnion |
  SimplifiedContentsMultisetAlternation SimplifiedMultisetAlternation
  deriving (Eq, Ord, Read, Show)

_SimplifiedContents = (Core.Name "hydra/ext/gql/openGql.SimplifiedContents")

_SimplifiedContents_term = (Core.Name "term")

_SimplifiedContents_pathUnion = (Core.Name "pathUnion")

_SimplifiedContents_multisetAlternation = (Core.Name "multisetAlternation")

newtype SimplifiedPathUnion = 
  SimplifiedPathUnion {
    unSimplifiedPathUnion :: [SimplifiedTerm]}
  deriving (Eq, Ord, Read, Show)

_SimplifiedPathUnion = (Core.Name "hydra/ext/gql/openGql.SimplifiedPathUnion")

newtype SimplifiedMultisetAlternation = 
  SimplifiedMultisetAlternation {
    unSimplifiedMultisetAlternation :: [SimplifiedTerm]}
  deriving (Eq, Ord, Read, Show)

_SimplifiedMultisetAlternation = (Core.Name "hydra/ext/gql/openGql.SimplifiedMultisetAlternation")

data SimplifiedTerm = 
  SimplifiedTermFactorLow SimplifiedFactorLow |
  SimplifiedTermConcatenation SimplifiedConcatenation
  deriving (Eq, Ord, Read, Show)

_SimplifiedTerm = (Core.Name "hydra/ext/gql/openGql.SimplifiedTerm")

_SimplifiedTerm_factorLow = (Core.Name "factorLow")

_SimplifiedTerm_concatenation = (Core.Name "concatenation")

data SimplifiedConcatenation = 
  SimplifiedConcatenation {
    simplifiedConcatenationInitialTerm :: SimplifiedTerm,
    simplifiedConcatenationNextFactor :: SimplifiedFactorLow}
  deriving (Eq, Ord, Read, Show)

_SimplifiedConcatenation = (Core.Name "hydra/ext/gql/openGql.SimplifiedConcatenation")

_SimplifiedConcatenation_initialTerm = (Core.Name "initialTerm")

_SimplifiedConcatenation_nextFactor = (Core.Name "nextFactor")

data SimplifiedFactorLow = 
  SimplifiedFactorLowFactorHigh SimplifiedFactorHigh |
  SimplifiedFactorLowConjunction SimplifiedConjunction
  deriving (Eq, Ord, Read, Show)

_SimplifiedFactorLow = (Core.Name "hydra/ext/gql/openGql.SimplifiedFactorLow")

_SimplifiedFactorLow_factorHigh = (Core.Name "factorHigh")

_SimplifiedFactorLow_conjunction = (Core.Name "conjunction")

data SimplifiedConjunction = 
  SimplifiedConjunction {
    simplifiedConjunctionLeft :: SimplifiedFactorLow,
    simplifiedConjunctionRight :: SimplifiedFactorHigh}
  deriving (Eq, Ord, Read, Show)

_SimplifiedConjunction = (Core.Name "hydra/ext/gql/openGql.SimplifiedConjunction")

_SimplifiedConjunction_left = (Core.Name "left")

_SimplifiedConjunction_right = (Core.Name "right")

data SimplifiedFactorHigh = 
  SimplifiedFactorHighTertiary SimplifiedTertiary |
  SimplifiedFactorHighQuantified SimplifiedQuantified |
  SimplifiedFactorHighQuestioned SimplifiedQuestioned
  deriving (Eq, Ord, Read, Show)

_SimplifiedFactorHigh = (Core.Name "hydra/ext/gql/openGql.SimplifiedFactorHigh")

_SimplifiedFactorHigh_tertiary = (Core.Name "tertiary")

_SimplifiedFactorHigh_quantified = (Core.Name "quantified")

_SimplifiedFactorHigh_questioned = (Core.Name "questioned")

data SimplifiedQuantified = 
  SimplifiedQuantified {
    simplifiedQuantifiedTertiary :: SimplifiedTertiary,
    simplifiedQuantifiedQuantifier :: GraphPatternQuantifier}
  deriving (Eq, Ord, Read, Show)

_SimplifiedQuantified = (Core.Name "hydra/ext/gql/openGql.SimplifiedQuantified")

_SimplifiedQuantified_tertiary = (Core.Name "tertiary")

_SimplifiedQuantified_quantifier = (Core.Name "quantifier")

newtype SimplifiedQuestioned = 
  SimplifiedQuestioned {
    unSimplifiedQuestioned :: SimplifiedTertiary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedQuestioned = (Core.Name "hydra/ext/gql/openGql.SimplifiedQuestioned")

data SimplifiedTertiary = 
  SimplifiedTertiaryDirectionOverride SimplifiedDirectionOverride |
  SimplifiedTertiarySecondary SimplifiedSecondary
  deriving (Eq, Ord, Read, Show)

_SimplifiedTertiary = (Core.Name "hydra/ext/gql/openGql.SimplifiedTertiary")

_SimplifiedTertiary_directionOverride = (Core.Name "directionOverride")

_SimplifiedTertiary_secondary = (Core.Name "secondary")

data SimplifiedDirectionOverride = 
  SimplifiedDirectionOverrideOverrideLeft SimplifiedOverrideLeft |
  SimplifiedDirectionOverrideOverrideUndirected SimplifiedOverrideUndirected |
  SimplifiedDirectionOverrideOverrideRight SimplifiedOverrideRight |
  SimplifiedDirectionOverrideOverrideLeftOrUndirected SimplifiedOverrideLeftOrUndirected |
  SimplifiedDirectionOverrideOverrideUndirectedOrRight SimplifiedOverrideUndirectedOrRight |
  SimplifiedDirectionOverrideOverrideLeftOrRight SimplifiedOverrideLeftOrRight |
  SimplifiedDirectionOverrideOverrideAnyDirection SimplifiedOverrideAnyDirection
  deriving (Eq, Ord, Read, Show)

_SimplifiedDirectionOverride = (Core.Name "hydra/ext/gql/openGql.SimplifiedDirectionOverride")

_SimplifiedDirectionOverride_overrideLeft = (Core.Name "overrideLeft")

_SimplifiedDirectionOverride_overrideUndirected = (Core.Name "overrideUndirected")

_SimplifiedDirectionOverride_overrideRight = (Core.Name "overrideRight")

_SimplifiedDirectionOverride_overrideLeftOrUndirected = (Core.Name "overrideLeftOrUndirected")

_SimplifiedDirectionOverride_overrideUndirectedOrRight = (Core.Name "overrideUndirectedOrRight")

_SimplifiedDirectionOverride_overrideLeftOrRight = (Core.Name "overrideLeftOrRight")

_SimplifiedDirectionOverride_overrideAnyDirection = (Core.Name "overrideAnyDirection")

newtype SimplifiedOverrideLeft = 
  SimplifiedOverrideLeft {
    unSimplifiedOverrideLeft :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideLeft = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideLeft")

newtype SimplifiedOverrideUndirected = 
  SimplifiedOverrideUndirected {
    unSimplifiedOverrideUndirected :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideUndirected = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideUndirected")

newtype SimplifiedOverrideRight = 
  SimplifiedOverrideRight {
    unSimplifiedOverrideRight :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideRight = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideRight")

newtype SimplifiedOverrideLeftOrUndirected = 
  SimplifiedOverrideLeftOrUndirected {
    unSimplifiedOverrideLeftOrUndirected :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideLeftOrUndirected = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideLeftOrUndirected")

newtype SimplifiedOverrideUndirectedOrRight = 
  SimplifiedOverrideUndirectedOrRight {
    unSimplifiedOverrideUndirectedOrRight :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideUndirectedOrRight = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideUndirectedOrRight")

newtype SimplifiedOverrideLeftOrRight = 
  SimplifiedOverrideLeftOrRight {
    unSimplifiedOverrideLeftOrRight :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideLeftOrRight = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideLeftOrRight")

newtype SimplifiedOverrideAnyDirection = 
  SimplifiedOverrideAnyDirection {
    unSimplifiedOverrideAnyDirection :: SimplifiedSecondary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedOverrideAnyDirection = (Core.Name "hydra/ext/gql/openGql.SimplifiedOverrideAnyDirection")

data SimplifiedSecondary = 
  SimplifiedSecondaryPrimary SimplifiedPrimary |
  SimplifiedSecondaryNegation SimplifiedNegation
  deriving (Eq, Ord, Read, Show)

_SimplifiedSecondary = (Core.Name "hydra/ext/gql/openGql.SimplifiedSecondary")

_SimplifiedSecondary_primary = (Core.Name "primary")

_SimplifiedSecondary_negation = (Core.Name "negation")

newtype SimplifiedNegation = 
  SimplifiedNegation {
    unSimplifiedNegation :: SimplifiedPrimary}
  deriving (Eq, Ord, Read, Show)

_SimplifiedNegation = (Core.Name "hydra/ext/gql/openGql.SimplifiedNegation")

data SimplifiedPrimary = 
  SimplifiedPrimaryLabelName LabelName |
  SimplifiedPrimaryParenthesizedContents SimplifiedContents
  deriving (Eq, Ord, Read, Show)

_SimplifiedPrimary = (Core.Name "hydra/ext/gql/openGql.SimplifiedPrimary")

_SimplifiedPrimary_labelName = (Core.Name "labelName")

_SimplifiedPrimary_parenthesizedContents = (Core.Name "parenthesizedContents")

newtype WhereClause = 
  WhereClause {
    unWhereClause :: SearchCondition}
  deriving (Eq, Ord, Read, Show)

_WhereClause = (Core.Name "hydra/ext/gql/openGql.WhereClause")

newtype YieldClause = 
  YieldClause {
    unYieldClause :: YieldItemList}
  deriving (Eq, Ord, Read, Show)

_YieldClause = (Core.Name "hydra/ext/gql/openGql.YieldClause")

newtype YieldItemList = 
  YieldItemList {
    unYieldItemList :: [YieldItem]}
  deriving (Eq, Ord, Read, Show)

_YieldItemList = (Core.Name "hydra/ext/gql/openGql.YieldItemList")

data YieldItem = 
  YieldItem {
    yieldItemName :: YieldItemName,
    yieldItemAlias :: (Maybe YieldItemAlias)}
  deriving (Eq, Ord, Read, Show)

_YieldItem = (Core.Name "hydra/ext/gql/openGql.YieldItem")

_YieldItem_name = (Core.Name "name")

_YieldItem_alias = (Core.Name "alias")

newtype YieldItemName = 
  YieldItemName {
    unYieldItemName :: FieldName}
  deriving (Eq, Ord, Read, Show)

_YieldItemName = (Core.Name "hydra/ext/gql/openGql.YieldItemName")

newtype YieldItemAlias = 
  YieldItemAlias {
    unYieldItemAlias :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_YieldItemAlias = (Core.Name "hydra/ext/gql/openGql.YieldItemAlias")

newtype GroupByClause = 
  GroupByClause {
    unGroupByClause :: GroupingElementList}
  deriving (Eq, Ord, Read, Show)

_GroupByClause = (Core.Name "hydra/ext/gql/openGql.GroupByClause")

data GroupingElementList = 
  GroupingElementListElements [GroupingElement] |
  GroupingElementListEmptySet 
  deriving (Eq, Ord, Read, Show)

_GroupingElementList = (Core.Name "hydra/ext/gql/openGql.GroupingElementList")

_GroupingElementList_elements = (Core.Name "elements")

_GroupingElementList_emptySet = (Core.Name "emptySet")

newtype GroupingElement = 
  GroupingElement {
    unGroupingElement :: BindingVariableReference}
  deriving (Eq, Ord, Read, Show)

_GroupingElement = (Core.Name "hydra/ext/gql/openGql.GroupingElement")

newtype OrderByClause = 
  OrderByClause {
    unOrderByClause :: SortSpecificationList}
  deriving (Eq, Ord, Read, Show)

_OrderByClause = (Core.Name "hydra/ext/gql/openGql.OrderByClause")

newtype SortSpecificationList = 
  SortSpecificationList {
    unSortSpecificationList :: [SortSpecification]}
  deriving (Eq, Ord, Read, Show)

_SortSpecificationList = (Core.Name "hydra/ext/gql/openGql.SortSpecificationList")

data SortSpecification = 
  SortSpecification {
    sortSpecificationSortKey :: SortKey,
    sortSpecificationOrdering :: (Maybe OrderingSpecification),
    sortSpecificationNullOrdering :: (Maybe NullOrdering)}
  deriving (Eq, Ord, Read, Show)

_SortSpecification = (Core.Name "hydra/ext/gql/openGql.SortSpecification")

_SortSpecification_sortKey = (Core.Name "sortKey")

_SortSpecification_ordering = (Core.Name "ordering")

_SortSpecification_nullOrdering = (Core.Name "nullOrdering")

newtype SortKey = 
  SortKey {
    unSortKey :: AggregatingValueExpression}
  deriving (Eq, Ord, Read, Show)

_SortKey = (Core.Name "hydra/ext/gql/openGql.SortKey")

data OrderingSpecification = 
  OrderingSpecificationAscending  |
  OrderingSpecificationDescending 
  deriving (Eq, Ord, Read, Show)

_OrderingSpecification = (Core.Name "hydra/ext/gql/openGql.OrderingSpecification")

_OrderingSpecification_ascending = (Core.Name "ascending")

_OrderingSpecification_descending = (Core.Name "descending")

data NullOrdering = 
  NullOrderingNullsFirst  |
  NullOrderingNullsLast 
  deriving (Eq, Ord, Read, Show)

_NullOrdering = (Core.Name "hydra/ext/gql/openGql.NullOrdering")

_NullOrdering_nullsFirst = (Core.Name "nullsFirst")

_NullOrdering_nullsLast = (Core.Name "nullsLast")

newtype LimitClause = 
  LimitClause {
    unLimitClause :: NonNegativeIntegerSpecification}
  deriving (Eq, Ord, Read, Show)

_LimitClause = (Core.Name "hydra/ext/gql/openGql.LimitClause")

data OffsetClause = 
  OffsetClause {
    offsetClauseSynonym :: OffsetSynonym,
    offsetClauseValue :: NonNegativeIntegerSpecification}
  deriving (Eq, Ord, Read, Show)

_OffsetClause = (Core.Name "hydra/ext/gql/openGql.OffsetClause")

_OffsetClause_synonym = (Core.Name "synonym")

_OffsetClause_value = (Core.Name "value")

data OffsetSynonym = 
  OffsetSynonymOffset  |
  OffsetSynonymSkipReservedWord 
  deriving (Eq, Ord, Read, Show)

_OffsetSynonym = (Core.Name "hydra/ext/gql/openGql.OffsetSynonym")

_OffsetSynonym_offset = (Core.Name "offset")

_OffsetSynonym_skipReservedWord = (Core.Name "skipReservedWord")

data SchemaReference = 
  SchemaReferenceAbsoluteReference AbsoluteCatalogSchemaReference |
  SchemaReferenceRelativeReference RelativeCatalogSchemaReference |
  SchemaReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_SchemaReference = (Core.Name "hydra/ext/gql/openGql.SchemaReference")

_SchemaReference_absoluteReference = (Core.Name "absoluteReference")

_SchemaReference_relativeReference = (Core.Name "relativeReference")

_SchemaReference_parameterSpecification = (Core.Name "parameterSpecification")

data AbsoluteCatalogSchemaReference = 
  AbsoluteCatalogSchemaReferenceRoot  |
  AbsoluteCatalogSchemaReferenceDirectoryAndSchema AbsoluteDirectoryAndSchema
  deriving (Eq, Ord, Read, Show)

_AbsoluteCatalogSchemaReference = (Core.Name "hydra/ext/gql/openGql.AbsoluteCatalogSchemaReference")

_AbsoluteCatalogSchemaReference_root = (Core.Name "root")

_AbsoluteCatalogSchemaReference_directoryAndSchema = (Core.Name "directoryAndSchema")

data AbsoluteDirectoryAndSchema = 
  AbsoluteDirectoryAndSchema {
    absoluteDirectoryAndSchemaDirectoryPath :: AbsoluteDirectoryPath,
    absoluteDirectoryAndSchemaSchemaName :: SchemaName}
  deriving (Eq, Ord, Read, Show)

_AbsoluteDirectoryAndSchema = (Core.Name "hydra/ext/gql/openGql.AbsoluteDirectoryAndSchema")

_AbsoluteDirectoryAndSchema_directoryPath = (Core.Name "directoryPath")

_AbsoluteDirectoryAndSchema_schemaName = (Core.Name "schemaName")

newtype CatalogSchemaParentAndName = 
  CatalogSchemaParentAndName {
    unCatalogSchemaParentAndName :: AbsoluteDirectoryAndSchema}
  deriving (Eq, Ord, Read, Show)

_CatalogSchemaParentAndName = (Core.Name "hydra/ext/gql/openGql.CatalogSchemaParentAndName")

data RelativeCatalogSchemaReference = 
  RelativeCatalogSchemaReferencePredefinedReference PredefinedSchemaReference |
  RelativeCatalogSchemaReferenceDirectoryAndSchema RelativeDirectoryAndSchema
  deriving (Eq, Ord, Read, Show)

_RelativeCatalogSchemaReference = (Core.Name "hydra/ext/gql/openGql.RelativeCatalogSchemaReference")

_RelativeCatalogSchemaReference_predefinedReference = (Core.Name "predefinedReference")

_RelativeCatalogSchemaReference_directoryAndSchema = (Core.Name "directoryAndSchema")

data RelativeDirectoryAndSchema = 
  RelativeDirectoryAndSchema {
    relativeDirectoryAndSchemaDirectoryPath :: RelativeDirectoryPath,
    relativeDirectoryAndSchemaSchemaName :: SchemaName}
  deriving (Eq, Ord, Read, Show)

_RelativeDirectoryAndSchema = (Core.Name "hydra/ext/gql/openGql.RelativeDirectoryAndSchema")

_RelativeDirectoryAndSchema_directoryPath = (Core.Name "directoryPath")

_RelativeDirectoryAndSchema_schemaName = (Core.Name "schemaName")

data PredefinedSchemaReference = 
  PredefinedSchemaReferenceHomeSchema  |
  PredefinedSchemaReferenceCurrentSchema  |
  PredefinedSchemaReferencePeriod 
  deriving (Eq, Ord, Read, Show)

_PredefinedSchemaReference = (Core.Name "hydra/ext/gql/openGql.PredefinedSchemaReference")

_PredefinedSchemaReference_homeSchema = (Core.Name "homeSchema")

_PredefinedSchemaReference_currentSchema = (Core.Name "currentSchema")

_PredefinedSchemaReference_period = (Core.Name "period")

newtype AbsoluteDirectoryPath = 
  AbsoluteDirectoryPath {
    unAbsoluteDirectoryPath :: (Maybe SimpleDirectoryPath)}
  deriving (Eq, Ord, Read, Show)

_AbsoluteDirectoryPath = (Core.Name "hydra/ext/gql/openGql.AbsoluteDirectoryPath")

data RelativeDirectoryPath = 
  RelativeDirectoryPath {
    relativeDirectoryPathParentDirectories :: Int,
    relativeDirectoryPathSimplePath :: (Maybe SimpleDirectoryPath)}
  deriving (Eq, Ord, Read, Show)

_RelativeDirectoryPath = (Core.Name "hydra/ext/gql/openGql.RelativeDirectoryPath")

_RelativeDirectoryPath_parentDirectories = (Core.Name "parentDirectories")

_RelativeDirectoryPath_simplePath = (Core.Name "simplePath")

newtype SimpleDirectoryPath = 
  SimpleDirectoryPath {
    unSimpleDirectoryPath :: [DirectoryName]}
  deriving (Eq, Ord, Read, Show)

_SimpleDirectoryPath = (Core.Name "hydra/ext/gql/openGql.SimpleDirectoryPath")

data GraphReference = 
  GraphReferenceParentAndGraphName ParentAndGraphName |
  GraphReferenceDelimitedGraphName DelimitedGraphName |
  GraphReferenceHomeGraph HomeGraph |
  GraphReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_GraphReference = (Core.Name "hydra/ext/gql/openGql.GraphReference")

_GraphReference_parentAndGraphName = (Core.Name "parentAndGraphName")

_GraphReference_delimitedGraphName = (Core.Name "delimitedGraphName")

_GraphReference_homeGraph = (Core.Name "homeGraph")

_GraphReference_parameterSpecification = (Core.Name "parameterSpecification")

data ParentAndGraphName = 
  ParentAndGraphName {
    parentAndGraphNameParentReference :: CatalogObjectParentReference,
    parentAndGraphNameGraphName :: GraphName}
  deriving (Eq, Ord, Read, Show)

_ParentAndGraphName = (Core.Name "hydra/ext/gql/openGql.ParentAndGraphName")

_ParentAndGraphName_parentReference = (Core.Name "parentReference")

_ParentAndGraphName_graphName = (Core.Name "graphName")

data CatalogGraphParentAndName = 
  CatalogGraphParentAndName {
    catalogGraphParentAndNameParentReference :: (Maybe CatalogObjectParentReference),
    catalogGraphParentAndNameGraphName :: GraphName}
  deriving (Eq, Ord, Read, Show)

_CatalogGraphParentAndName = (Core.Name "hydra/ext/gql/openGql.CatalogGraphParentAndName")

_CatalogGraphParentAndName_parentReference = (Core.Name "parentReference")

_CatalogGraphParentAndName_graphName = (Core.Name "graphName")

data HomeGraph = 
  HomeGraphHomePropertyGraph  |
  HomeGraphHomeGraph 
  deriving (Eq, Ord, Read, Show)

_HomeGraph = (Core.Name "hydra/ext/gql/openGql.HomeGraph")

_HomeGraph_homePropertyGraph = (Core.Name "homePropertyGraph")

_HomeGraph_homeGraph = (Core.Name "homeGraph")

data GraphTypeReference = 
  GraphTypeReferenceParentAndTypeName CatalogGraphTypeParentAndName |
  GraphTypeReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_GraphTypeReference = (Core.Name "hydra/ext/gql/openGql.GraphTypeReference")

_GraphTypeReference_parentAndTypeName = (Core.Name "parentAndTypeName")

_GraphTypeReference_parameterSpecification = (Core.Name "parameterSpecification")

data CatalogGraphTypeParentAndName = 
  CatalogGraphTypeParentAndName {
    catalogGraphTypeParentAndNameParentReference :: (Maybe CatalogObjectParentReference),
    catalogGraphTypeParentAndNameGraphTypeName :: GraphTypeName}
  deriving (Eq, Ord, Read, Show)

_CatalogGraphTypeParentAndName = (Core.Name "hydra/ext/gql/openGql.CatalogGraphTypeParentAndName")

_CatalogGraphTypeParentAndName_parentReference = (Core.Name "parentReference")

_CatalogGraphTypeParentAndName_graphTypeName = (Core.Name "graphTypeName")

data BindingTableReference = 
  BindingTableReferenceParentAndTableName ParentAndTableName |
  BindingTableReferenceDelimitedBindingTableName DelimitedBindingTableName |
  BindingTableReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_BindingTableReference = (Core.Name "hydra/ext/gql/openGql.BindingTableReference")

_BindingTableReference_parentAndTableName = (Core.Name "parentAndTableName")

_BindingTableReference_delimitedBindingTableName = (Core.Name "delimitedBindingTableName")

_BindingTableReference_parameterSpecification = (Core.Name "parameterSpecification")

data ParentAndTableName = 
  ParentAndTableName {
    parentAndTableNameParentReference :: CatalogObjectParentReference,
    parentAndTableNameTableName :: BindingTableName}
  deriving (Eq, Ord, Read, Show)

_ParentAndTableName = (Core.Name "hydra/ext/gql/openGql.ParentAndTableName")

_ParentAndTableName_parentReference = (Core.Name "parentReference")

_ParentAndTableName_tableName = (Core.Name "tableName")

data ProcedureReference = 
  ProcedureReferenceParentAndProcedureName CatalogProcedureParentAndName |
  ProcedureReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_ProcedureReference = (Core.Name "hydra/ext/gql/openGql.ProcedureReference")

_ProcedureReference_parentAndProcedureName = (Core.Name "parentAndProcedureName")

_ProcedureReference_parameterSpecification = (Core.Name "parameterSpecification")

data CatalogProcedureParentAndName = 
  CatalogProcedureParentAndName {
    catalogProcedureParentAndNameParentReference :: (Maybe CatalogObjectParentReference),
    catalogProcedureParentAndNameProcedureName :: ProcedureName}
  deriving (Eq, Ord, Read, Show)

_CatalogProcedureParentAndName = (Core.Name "hydra/ext/gql/openGql.CatalogProcedureParentAndName")

_CatalogProcedureParentAndName_parentReference = (Core.Name "parentReference")

_CatalogProcedureParentAndName_procedureName = (Core.Name "procedureName")

data CatalogObjectParentReference = 
  CatalogObjectParentReferenceSchemaAndObjects SchemaAndObjects |
  CatalogObjectParentReferenceObjectsOnly [ObjectName]
  deriving (Eq, Ord, Read, Show)

_CatalogObjectParentReference = (Core.Name "hydra/ext/gql/openGql.CatalogObjectParentReference")

_CatalogObjectParentReference_schemaAndObjects = (Core.Name "schemaAndObjects")

_CatalogObjectParentReference_objectsOnly = (Core.Name "objectsOnly")

data SchemaAndObjects = 
  SchemaAndObjects {
    schemaAndObjectsSchemaReference :: SchemaReference,
    schemaAndObjectsObjects :: [ObjectName]}
  deriving (Eq, Ord, Read, Show)

_SchemaAndObjects = (Core.Name "hydra/ext/gql/openGql.SchemaAndObjects")

_SchemaAndObjects_schemaReference = (Core.Name "schemaReference")

_SchemaAndObjects_objects = (Core.Name "objects")

data ReferenceParameterSpecification = 
  ReferenceParameterSpecification {}
  deriving (Eq, Ord, Read, Show)

_ReferenceParameterSpecification = (Core.Name "hydra/ext/gql/openGql.ReferenceParameterSpecification")

newtype NestedGraphTypeSpecification = 
  NestedGraphTypeSpecification {
    unNestedGraphTypeSpecification :: GraphTypeSpecificationBody}
  deriving (Eq, Ord, Read, Show)

_NestedGraphTypeSpecification = (Core.Name "hydra/ext/gql/openGql.NestedGraphTypeSpecification")

newtype GraphTypeSpecificationBody = 
  GraphTypeSpecificationBody {
    unGraphTypeSpecificationBody :: ElementTypeList}
  deriving (Eq, Ord, Read, Show)

_GraphTypeSpecificationBody = (Core.Name "hydra/ext/gql/openGql.GraphTypeSpecificationBody")

newtype ElementTypeList = 
  ElementTypeList {
    unElementTypeList :: [ElementTypeSpecification]}
  deriving (Eq, Ord, Read, Show)

_ElementTypeList = (Core.Name "hydra/ext/gql/openGql.ElementTypeList")

data ElementTypeSpecification = 
  ElementTypeSpecificationNodeType NodeTypeSpecification |
  ElementTypeSpecificationEdgeType EdgeTypeSpecification
  deriving (Eq, Ord, Read, Show)

_ElementTypeSpecification = (Core.Name "hydra/ext/gql/openGql.ElementTypeSpecification")

_ElementTypeSpecification_nodeType = (Core.Name "nodeType")

_ElementTypeSpecification_edgeType = (Core.Name "edgeType")

data NodeTypeSpecification = 
  NodeTypeSpecificationPattern NodeTypePattern |
  NodeTypeSpecificationPhrase NodeTypePhrase
  deriving (Eq, Ord, Read, Show)

_NodeTypeSpecification = (Core.Name "hydra/ext/gql/openGql.NodeTypeSpecification")

_NodeTypeSpecification_pattern = (Core.Name "pattern")

_NodeTypeSpecification_phrase = (Core.Name "phrase")

data NodeTypePattern = 
  NodeTypePattern {
    nodeTypePatternSynonymAndTypeName :: (Maybe NodeSynonymAndTypeName),
    nodeTypePatternAlias :: (Maybe LocalNodeTypeAlias),
    nodeTypePatternFiller :: (Maybe NodeTypeFiller)}
  deriving (Eq, Ord, Read, Show)

_NodeTypePattern = (Core.Name "hydra/ext/gql/openGql.NodeTypePattern")

_NodeTypePattern_synonymAndTypeName = (Core.Name "synonymAndTypeName")

_NodeTypePattern_alias = (Core.Name "alias")

_NodeTypePattern_filler = (Core.Name "filler")

data NodeSynonymAndTypeName = 
  NodeSynonymAndTypeName {
    nodeSynonymAndTypeNameNodeSynonym :: NodeSynonym,
    nodeSynonymAndTypeNameTypeName :: (Maybe NodeTypeName)}
  deriving (Eq, Ord, Read, Show)

_NodeSynonymAndTypeName = (Core.Name "hydra/ext/gql/openGql.NodeSynonymAndTypeName")

_NodeSynonymAndTypeName_nodeSynonym = (Core.Name "nodeSynonym")

_NodeSynonymAndTypeName_typeName = (Core.Name "typeName")

data NodeTypePhrase = 
  NodeTypePhrase {
    nodeTypePhraseSynonym :: NodeSynonym,
    nodeTypePhraseTypePhraseFiller :: NodeTypePhraseFiller,
    nodeTypePhraseAlias :: (Maybe LocalNodeTypeAlias)}
  deriving (Eq, Ord, Read, Show)

_NodeTypePhrase = (Core.Name "hydra/ext/gql/openGql.NodeTypePhrase")

_NodeTypePhrase_synonym = (Core.Name "synonym")

_NodeTypePhrase_typePhraseFiller = (Core.Name "typePhraseFiller")

_NodeTypePhrase_alias = (Core.Name "alias")

data NodeTypePhraseFiller = 
  NodeTypePhraseFillerTypeName NodeTypeNameWithFiller |
  NodeTypePhraseFillerFillerOnly NodeTypeFiller
  deriving (Eq, Ord, Read, Show)

_NodeTypePhraseFiller = (Core.Name "hydra/ext/gql/openGql.NodeTypePhraseFiller")

_NodeTypePhraseFiller_typeName = (Core.Name "typeName")

_NodeTypePhraseFiller_fillerOnly = (Core.Name "fillerOnly")

data NodeTypeNameWithFiller = 
  NodeTypeNameWithFiller {
    nodeTypeNameWithFillerTypeName :: NodeTypeName,
    nodeTypeNameWithFillerFiller :: (Maybe NodeTypeFiller)}
  deriving (Eq, Ord, Read, Show)

_NodeTypeNameWithFiller = (Core.Name "hydra/ext/gql/openGql.NodeTypeNameWithFiller")

_NodeTypeNameWithFiller_typeName = (Core.Name "typeName")

_NodeTypeNameWithFiller_filler = (Core.Name "filler")

data NodeTypeFiller = 
  NodeTypeFillerKeyLabelSet NodeKeyLabelSetWithContent |
  NodeTypeFillerImpliedContent NodeTypeImpliedContent
  deriving (Eq, Ord, Read, Show)

_NodeTypeFiller = (Core.Name "hydra/ext/gql/openGql.NodeTypeFiller")

_NodeTypeFiller_keyLabelSet = (Core.Name "keyLabelSet")

_NodeTypeFiller_impliedContent = (Core.Name "impliedContent")

data NodeKeyLabelSetWithContent = 
  NodeKeyLabelSetWithContent {
    nodeKeyLabelSetWithContentKeyLabelSet :: NodeTypeKeyLabelSet,
    nodeKeyLabelSetWithContentImpliedContent :: (Maybe NodeTypeImpliedContent)}
  deriving (Eq, Ord, Read, Show)

_NodeKeyLabelSetWithContent = (Core.Name "hydra/ext/gql/openGql.NodeKeyLabelSetWithContent")

_NodeKeyLabelSetWithContent_keyLabelSet = (Core.Name "keyLabelSet")

_NodeKeyLabelSetWithContent_impliedContent = (Core.Name "impliedContent")

newtype LocalNodeTypeAlias = 
  LocalNodeTypeAlias {
    unLocalNodeTypeAlias :: String}
  deriving (Eq, Ord, Read, Show)

_LocalNodeTypeAlias = (Core.Name "hydra/ext/gql/openGql.LocalNodeTypeAlias")

data NodeTypeImpliedContent = 
  NodeTypeImpliedContentLabelSet NodeTypeLabelSet |
  NodeTypeImpliedContentPropertyTypes NodeTypePropertyTypes |
  NodeTypeImpliedContentLabelSetWithProperties NodeLabelSetWithProperties
  deriving (Eq, Ord, Read, Show)

_NodeTypeImpliedContent = (Core.Name "hydra/ext/gql/openGql.NodeTypeImpliedContent")

_NodeTypeImpliedContent_labelSet = (Core.Name "labelSet")

_NodeTypeImpliedContent_propertyTypes = (Core.Name "propertyTypes")

_NodeTypeImpliedContent_labelSetWithProperties = (Core.Name "labelSetWithProperties")

data NodeLabelSetWithProperties = 
  NodeLabelSetWithProperties {
    nodeLabelSetWithPropertiesLabelSet :: NodeTypeLabelSet,
    nodeLabelSetWithPropertiesPropertyTypes :: NodeTypePropertyTypes}
  deriving (Eq, Ord, Read, Show)

_NodeLabelSetWithProperties = (Core.Name "hydra/ext/gql/openGql.NodeLabelSetWithProperties")

_NodeLabelSetWithProperties_labelSet = (Core.Name "labelSet")

_NodeLabelSetWithProperties_propertyTypes = (Core.Name "propertyTypes")

newtype NodeTypeKeyLabelSet = 
  NodeTypeKeyLabelSet {
    unNodeTypeKeyLabelSet :: (Maybe LabelSetPhrase)}
  deriving (Eq, Ord, Read, Show)

_NodeTypeKeyLabelSet = (Core.Name "hydra/ext/gql/openGql.NodeTypeKeyLabelSet")

newtype NodeTypeLabelSet = 
  NodeTypeLabelSet {
    unNodeTypeLabelSet :: LabelSetPhrase}
  deriving (Eq, Ord, Read, Show)

_NodeTypeLabelSet = (Core.Name "hydra/ext/gql/openGql.NodeTypeLabelSet")

newtype NodeTypePropertyTypes = 
  NodeTypePropertyTypes {
    unNodeTypePropertyTypes :: PropertyTypesSpecification}
  deriving (Eq, Ord, Read, Show)

_NodeTypePropertyTypes = (Core.Name "hydra/ext/gql/openGql.NodeTypePropertyTypes")

data EdgeTypeSpecification = 
  EdgeTypeSpecificationPattern EdgeTypePattern |
  EdgeTypeSpecificationPhrase EdgeTypePhrase
  deriving (Eq, Ord, Read, Show)

_EdgeTypeSpecification = (Core.Name "hydra/ext/gql/openGql.EdgeTypeSpecification")

_EdgeTypeSpecification_pattern = (Core.Name "pattern")

_EdgeTypeSpecification_phrase = (Core.Name "phrase")

data EdgeTypePattern = 
  EdgeTypePattern {
    edgeTypePatternKindAndSynonym :: (Maybe EdgeKindAndSynonym),
    edgeTypePatternPatternType :: EdgeTypePatternType}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePattern = (Core.Name "hydra/ext/gql/openGql.EdgeTypePattern")

_EdgeTypePattern_kindAndSynonym = (Core.Name "kindAndSynonym")

_EdgeTypePattern_patternType = (Core.Name "patternType")

data EdgeKindAndSynonym = 
  EdgeKindAndSynonym {
    edgeKindAndSynonymKind :: (Maybe EdgeKind),
    edgeKindAndSynonymSynonym :: EdgeSynonym,
    edgeKindAndSynonymTypeName :: (Maybe EdgeTypeName)}
  deriving (Eq, Ord, Read, Show)

_EdgeKindAndSynonym = (Core.Name "hydra/ext/gql/openGql.EdgeKindAndSynonym")

_EdgeKindAndSynonym_kind = (Core.Name "kind")

_EdgeKindAndSynonym_synonym = (Core.Name "synonym")

_EdgeKindAndSynonym_typeName = (Core.Name "typeName")

data EdgeTypePatternType = 
  EdgeTypePatternTypeDirected EdgeTypePatternDirected |
  EdgeTypePatternTypeUndirected EdgeTypePatternUndirected
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternType = (Core.Name "hydra/ext/gql/openGql.EdgeTypePatternType")

_EdgeTypePatternType_directed = (Core.Name "directed")

_EdgeTypePatternType_undirected = (Core.Name "undirected")

data EdgeTypePhrase = 
  EdgeTypePhrase {
    edgeTypePhraseKind :: EdgeKind,
    edgeTypePhraseSynonym :: EdgeSynonym,
    edgeTypePhraseTypeNameAndFiller :: EdgeTypePhraseFiller,
    edgeTypePhraseEndpointPair :: EndpointPairPhrase}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePhrase = (Core.Name "hydra/ext/gql/openGql.EdgeTypePhrase")

_EdgeTypePhrase_kind = (Core.Name "kind")

_EdgeTypePhrase_synonym = (Core.Name "synonym")

_EdgeTypePhrase_typeNameAndFiller = (Core.Name "typeNameAndFiller")

_EdgeTypePhrase_endpointPair = (Core.Name "endpointPair")

data EdgeTypePhraseFiller = 
  EdgeTypePhraseFillerTypeNameWithFiller EdgeTypeNameWithFiller |
  EdgeTypePhraseFillerFillerOnly EdgeTypeFiller
  deriving (Eq, Ord, Read, Show)

_EdgeTypePhraseFiller = (Core.Name "hydra/ext/gql/openGql.EdgeTypePhraseFiller")

_EdgeTypePhraseFiller_typeNameWithFiller = (Core.Name "typeNameWithFiller")

_EdgeTypePhraseFiller_fillerOnly = (Core.Name "fillerOnly")

data EdgeTypeNameWithFiller = 
  EdgeTypeNameWithFiller {
    edgeTypeNameWithFillerTypeName :: EdgeTypeName,
    edgeTypeNameWithFillerFiller :: (Maybe EdgeTypeFiller)}
  deriving (Eq, Ord, Read, Show)

_EdgeTypeNameWithFiller = (Core.Name "hydra/ext/gql/openGql.EdgeTypeNameWithFiller")

_EdgeTypeNameWithFiller_typeName = (Core.Name "typeName")

_EdgeTypeNameWithFiller_filler = (Core.Name "filler")

data EdgeTypeFiller = 
  EdgeTypeFillerKeyLabelSetWithContent EdgeKeyLabelSetWithContent |
  EdgeTypeFillerImpliedContent EdgeTypeImpliedContent
  deriving (Eq, Ord, Read, Show)

_EdgeTypeFiller = (Core.Name "hydra/ext/gql/openGql.EdgeTypeFiller")

_EdgeTypeFiller_keyLabelSetWithContent = (Core.Name "keyLabelSetWithContent")

_EdgeTypeFiller_impliedContent = (Core.Name "impliedContent")

data EdgeKeyLabelSetWithContent = 
  EdgeKeyLabelSetWithContent {
    edgeKeyLabelSetWithContentKeyLabelSet :: EdgeTypeKeyLabelSet,
    edgeKeyLabelSetWithContentImpliedContent :: (Maybe EdgeTypeImpliedContent)}
  deriving (Eq, Ord, Read, Show)

_EdgeKeyLabelSetWithContent = (Core.Name "hydra/ext/gql/openGql.EdgeKeyLabelSetWithContent")

_EdgeKeyLabelSetWithContent_keyLabelSet = (Core.Name "keyLabelSet")

_EdgeKeyLabelSetWithContent_impliedContent = (Core.Name "impliedContent")

data EdgeTypeImpliedContent = 
  EdgeTypeImpliedContentLabelSet EdgeTypeLabelSet |
  EdgeTypeImpliedContentPropertyTypes EdgeTypePropertyTypes |
  EdgeTypeImpliedContentLabelSetWithProperties EdgeLabelSetWithProperties
  deriving (Eq, Ord, Read, Show)

_EdgeTypeImpliedContent = (Core.Name "hydra/ext/gql/openGql.EdgeTypeImpliedContent")

_EdgeTypeImpliedContent_labelSet = (Core.Name "labelSet")

_EdgeTypeImpliedContent_propertyTypes = (Core.Name "propertyTypes")

_EdgeTypeImpliedContent_labelSetWithProperties = (Core.Name "labelSetWithProperties")

data EdgeLabelSetWithProperties = 
  EdgeLabelSetWithProperties {
    edgeLabelSetWithPropertiesLabelSet :: EdgeTypeLabelSet,
    edgeLabelSetWithPropertiesPropertyTypes :: EdgeTypePropertyTypes}
  deriving (Eq, Ord, Read, Show)

_EdgeLabelSetWithProperties = (Core.Name "hydra/ext/gql/openGql.EdgeLabelSetWithProperties")

_EdgeLabelSetWithProperties_labelSet = (Core.Name "labelSet")

_EdgeLabelSetWithProperties_propertyTypes = (Core.Name "propertyTypes")

newtype EdgeTypeKeyLabelSet = 
  EdgeTypeKeyLabelSet {
    unEdgeTypeKeyLabelSet :: (Maybe LabelSetPhrase)}
  deriving (Eq, Ord, Read, Show)

_EdgeTypeKeyLabelSet = (Core.Name "hydra/ext/gql/openGql.EdgeTypeKeyLabelSet")

newtype EdgeTypeLabelSet = 
  EdgeTypeLabelSet {
    unEdgeTypeLabelSet :: LabelSetPhrase}
  deriving (Eq, Ord, Read, Show)

_EdgeTypeLabelSet = (Core.Name "hydra/ext/gql/openGql.EdgeTypeLabelSet")

newtype EdgeTypePropertyTypes = 
  EdgeTypePropertyTypes {
    unEdgeTypePropertyTypes :: PropertyTypesSpecification}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePropertyTypes = (Core.Name "hydra/ext/gql/openGql.EdgeTypePropertyTypes")

data EdgeTypePatternDirected = 
  EdgeTypePatternDirectedPointingRight EdgeTypePatternPointingRight |
  EdgeTypePatternDirectedPointingLeft EdgeTypePatternPointingLeft
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternDirected = (Core.Name "hydra/ext/gql/openGql.EdgeTypePatternDirected")

_EdgeTypePatternDirected_pointingRight = (Core.Name "pointingRight")

_EdgeTypePatternDirected_pointingLeft = (Core.Name "pointingLeft")

data EdgeTypePatternPointingRight = 
  EdgeTypePatternPointingRight {
    edgeTypePatternPointingRightSource :: SourceNodeTypeReference,
    edgeTypePatternPointingRightArc :: ArcTypePointingRight,
    edgeTypePatternPointingRightDestination :: DestinationNodeTypeReference}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternPointingRight = (Core.Name "hydra/ext/gql/openGql.EdgeTypePatternPointingRight")

_EdgeTypePatternPointingRight_source = (Core.Name "source")

_EdgeTypePatternPointingRight_arc = (Core.Name "arc")

_EdgeTypePatternPointingRight_destination = (Core.Name "destination")

data EdgeTypePatternPointingLeft = 
  EdgeTypePatternPointingLeft {
    edgeTypePatternPointingLeftDestination :: DestinationNodeTypeReference,
    edgeTypePatternPointingLeftArc :: ArcTypePointingLeft,
    edgeTypePatternPointingLeftSource :: SourceNodeTypeReference}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternPointingLeft = (Core.Name "hydra/ext/gql/openGql.EdgeTypePatternPointingLeft")

_EdgeTypePatternPointingLeft_destination = (Core.Name "destination")

_EdgeTypePatternPointingLeft_arc = (Core.Name "arc")

_EdgeTypePatternPointingLeft_source = (Core.Name "source")

data EdgeTypePatternUndirected = 
  EdgeTypePatternUndirected {
    edgeTypePatternUndirectedSource :: SourceNodeTypeReference,
    edgeTypePatternUndirectedArc :: ArcTypeUndirected,
    edgeTypePatternUndirectedDestination :: DestinationNodeTypeReference}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternUndirected = (Core.Name "hydra/ext/gql/openGql.EdgeTypePatternUndirected")

_EdgeTypePatternUndirected_source = (Core.Name "source")

_EdgeTypePatternUndirected_arc = (Core.Name "arc")

_EdgeTypePatternUndirected_destination = (Core.Name "destination")

newtype ArcTypePointingRight = 
  ArcTypePointingRight {
    unArcTypePointingRight :: EdgeTypeFiller}
  deriving (Eq, Ord, Read, Show)

_ArcTypePointingRight = (Core.Name "hydra/ext/gql/openGql.ArcTypePointingRight")

newtype ArcTypePointingLeft = 
  ArcTypePointingLeft {
    unArcTypePointingLeft :: EdgeTypeFiller}
  deriving (Eq, Ord, Read, Show)

_ArcTypePointingLeft = (Core.Name "hydra/ext/gql/openGql.ArcTypePointingLeft")

newtype ArcTypeUndirected = 
  ArcTypeUndirected {
    unArcTypeUndirected :: EdgeTypeFiller}
  deriving (Eq, Ord, Read, Show)

_ArcTypeUndirected = (Core.Name "hydra/ext/gql/openGql.ArcTypeUndirected")

data SourceNodeTypeReference = 
  SourceNodeTypeReferenceAlias SourceNodeTypeAlias |
  SourceNodeTypeReferenceFiller (Maybe NodeTypeFiller)
  deriving (Eq, Ord, Read, Show)

_SourceNodeTypeReference = (Core.Name "hydra/ext/gql/openGql.SourceNodeTypeReference")

_SourceNodeTypeReference_alias = (Core.Name "alias")

_SourceNodeTypeReference_filler = (Core.Name "filler")

data DestinationNodeTypeReference = 
  DestinationNodeTypeReferenceAlias DestinationNodeTypeAlias |
  DestinationNodeTypeReferenceFiller (Maybe NodeTypeFiller)
  deriving (Eq, Ord, Read, Show)

_DestinationNodeTypeReference = (Core.Name "hydra/ext/gql/openGql.DestinationNodeTypeReference")

_DestinationNodeTypeReference_alias = (Core.Name "alias")

_DestinationNodeTypeReference_filler = (Core.Name "filler")

data EdgeKind = 
  EdgeKindDirected  |
  EdgeKindUndirected 
  deriving (Eq, Ord, Read, Show)

_EdgeKind = (Core.Name "hydra/ext/gql/openGql.EdgeKind")

_EdgeKind_directed = (Core.Name "directed")

_EdgeKind_undirected = (Core.Name "undirected")

newtype EndpointPairPhrase = 
  EndpointPairPhrase {
    unEndpointPairPhrase :: EndpointPair}
  deriving (Eq, Ord, Read, Show)

_EndpointPairPhrase = (Core.Name "hydra/ext/gql/openGql.EndpointPairPhrase")

data EndpointPair = 
  EndpointPairDirected_ EndpointPairDirected |
  EndpointPairUndirected_ EndpointPairUndirected
  deriving (Eq, Ord, Read, Show)

_EndpointPair = (Core.Name "hydra/ext/gql/openGql.EndpointPair")

_EndpointPair_directed = (Core.Name "directed")

_EndpointPair_undirected = (Core.Name "undirected")

data EndpointPairDirected = 
  EndpointPairDirectedPointingRight EndpointPairPointingRight |
  EndpointPairDirectedPointingLeft EndpointPairPointingLeft
  deriving (Eq, Ord, Read, Show)

_EndpointPairDirected = (Core.Name "hydra/ext/gql/openGql.EndpointPairDirected")

_EndpointPairDirected_pointingRight = (Core.Name "pointingRight")

_EndpointPairDirected_pointingLeft = (Core.Name "pointingLeft")

data EndpointPairPointingRight = 
  EndpointPairPointingRight {
    endpointPairPointingRightSourceAlias :: SourceNodeTypeAlias,
    endpointPairPointingRightConnector :: ConnectorPointingRight,
    endpointPairPointingRightDestinationAlias :: DestinationNodeTypeAlias}
  deriving (Eq, Ord, Read, Show)

_EndpointPairPointingRight = (Core.Name "hydra/ext/gql/openGql.EndpointPairPointingRight")

_EndpointPairPointingRight_sourceAlias = (Core.Name "sourceAlias")

_EndpointPairPointingRight_connector = (Core.Name "connector")

_EndpointPairPointingRight_destinationAlias = (Core.Name "destinationAlias")

data EndpointPairPointingLeft = 
  EndpointPairPointingLeft {
    endpointPairPointingLeftDestinationAlias :: DestinationNodeTypeAlias,
    endpointPairPointingLeftSourceAlias :: SourceNodeTypeAlias}
  deriving (Eq, Ord, Read, Show)

_EndpointPairPointingLeft = (Core.Name "hydra/ext/gql/openGql.EndpointPairPointingLeft")

_EndpointPairPointingLeft_destinationAlias = (Core.Name "destinationAlias")

_EndpointPairPointingLeft_sourceAlias = (Core.Name "sourceAlias")

data EndpointPairUndirected = 
  EndpointPairUndirected {
    endpointPairUndirectedSourceAlias :: SourceNodeTypeAlias,
    endpointPairUndirectedConnector :: ConnectorUndirected,
    endpointPairUndirectedDestinationAlias :: DestinationNodeTypeAlias}
  deriving (Eq, Ord, Read, Show)

_EndpointPairUndirected = (Core.Name "hydra/ext/gql/openGql.EndpointPairUndirected")

_EndpointPairUndirected_sourceAlias = (Core.Name "sourceAlias")

_EndpointPairUndirected_connector = (Core.Name "connector")

_EndpointPairUndirected_destinationAlias = (Core.Name "destinationAlias")

data ConnectorPointingRight = 
  ConnectorPointingRightTo  |
  ConnectorPointingRightRightArrow 
  deriving (Eq, Ord, Read, Show)

_ConnectorPointingRight = (Core.Name "hydra/ext/gql/openGql.ConnectorPointingRight")

_ConnectorPointingRight_to = (Core.Name "to")

_ConnectorPointingRight_rightArrow = (Core.Name "rightArrow")

data ConnectorUndirected = 
  ConnectorUndirectedTo  |
  ConnectorUndirectedTilde 
  deriving (Eq, Ord, Read, Show)

_ConnectorUndirected = (Core.Name "hydra/ext/gql/openGql.ConnectorUndirected")

_ConnectorUndirected_to = (Core.Name "to")

_ConnectorUndirected_tilde = (Core.Name "tilde")

newtype SourceNodeTypeAlias = 
  SourceNodeTypeAlias {
    unSourceNodeTypeAlias :: String}
  deriving (Eq, Ord, Read, Show)

_SourceNodeTypeAlias = (Core.Name "hydra/ext/gql/openGql.SourceNodeTypeAlias")

newtype DestinationNodeTypeAlias = 
  DestinationNodeTypeAlias {
    unDestinationNodeTypeAlias :: String}
  deriving (Eq, Ord, Read, Show)

_DestinationNodeTypeAlias = (Core.Name "hydra/ext/gql/openGql.DestinationNodeTypeAlias")

data LabelSetPhrase = 
  LabelSetPhraseSingleLabel LabelName |
  LabelSetPhraseMultipleLabels LabelSetSpecification |
  LabelSetPhraseIsOrColonWithLabels IsOrColonWithLabels
  deriving (Eq, Ord, Read, Show)

_LabelSetPhrase = (Core.Name "hydra/ext/gql/openGql.LabelSetPhrase")

_LabelSetPhrase_singleLabel = (Core.Name "singleLabel")

_LabelSetPhrase_multipleLabels = (Core.Name "multipleLabels")

_LabelSetPhrase_isOrColonWithLabels = (Core.Name "isOrColonWithLabels")

data IsOrColonWithLabels = 
  IsOrColonWithLabels {
    isOrColonWithLabelsIsOrColon :: IsOrColon,
    isOrColonWithLabelsLabels :: LabelSetSpecification}
  deriving (Eq, Ord, Read, Show)

_IsOrColonWithLabels = (Core.Name "hydra/ext/gql/openGql.IsOrColonWithLabels")

_IsOrColonWithLabels_isOrColon = (Core.Name "isOrColon")

_IsOrColonWithLabels_labels = (Core.Name "labels")

newtype LabelSetSpecification = 
  LabelSetSpecification {
    unLabelSetSpecification :: [LabelName]}
  deriving (Eq, Ord, Read, Show)

_LabelSetSpecification = (Core.Name "hydra/ext/gql/openGql.LabelSetSpecification")

newtype PropertyTypesSpecification = 
  PropertyTypesSpecification {
    unPropertyTypesSpecification :: (Maybe PropertyTypeList)}
  deriving (Eq, Ord, Read, Show)

_PropertyTypesSpecification = (Core.Name "hydra/ext/gql/openGql.PropertyTypesSpecification")

newtype PropertyTypeList = 
  PropertyTypeList {
    unPropertyTypeList :: [PropertyType]}
  deriving (Eq, Ord, Read, Show)

_PropertyTypeList = (Core.Name "hydra/ext/gql/openGql.PropertyTypeList")

data PropertyType = 
  PropertyType {
    propertyTypeName :: PropertyName,
    propertyTypeTyped :: (Maybe Typed),
    propertyTypeValueType :: PropertyValueType}
  deriving (Eq, Ord, Read, Show)

_PropertyType = (Core.Name "hydra/ext/gql/openGql.PropertyType")

_PropertyType_name = (Core.Name "name")

_PropertyType_typed = (Core.Name "typed")

_PropertyType_valueType = (Core.Name "valueType")

newtype PropertyValueType = 
  PropertyValueType {
    unPropertyValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_PropertyValueType = (Core.Name "hydra/ext/gql/openGql.PropertyValueType")

data BindingTableType = 
  BindingTableType {
    bindingTableTypeBinding :: Bool,
    bindingTableTypeFieldTypes :: FieldTypesSpecification}
  deriving (Eq, Ord, Read, Show)

_BindingTableType = (Core.Name "hydra/ext/gql/openGql.BindingTableType")

_BindingTableType_binding = (Core.Name "binding")

_BindingTableType_fieldTypes = (Core.Name "fieldTypes")

data ValueType = 
  ValueTypePredefinedType PredefinedType |
  ValueTypePathValueType PathValueType |
  ValueTypeListValueTypeAlt1 ListValueTypeAlt1 |
  ValueTypeListValueTypeAlt2 ListValueTypeAlt2 |
  ValueTypeListValueTypeAlt3 ListValueTypeAlt3 |
  ValueTypeRecordType RecordType |
  ValueTypeOpenDynamicUnionType OpenDynamicUnionType |
  ValueTypeDynamicPropertyValueType DynamicPropertyValueType |
  ValueTypeClosedDynamicUnionTypeAlt1 ClosedDynamicUnionTypeAlt1 |
  ValueTypeClosedDynamicUnionTypeAlt2 ClosedDynamicUnionTypeAlt2
  deriving (Eq, Ord, Read, Show)

_ValueType = (Core.Name "hydra/ext/gql/openGql.ValueType")

_ValueType_predefinedType = (Core.Name "predefinedType")

_ValueType_pathValueType = (Core.Name "pathValueType")

_ValueType_listValueTypeAlt1 = (Core.Name "listValueTypeAlt1")

_ValueType_listValueTypeAlt2 = (Core.Name "listValueTypeAlt2")

_ValueType_listValueTypeAlt3 = (Core.Name "listValueTypeAlt3")

_ValueType_recordType = (Core.Name "recordType")

_ValueType_openDynamicUnionType = (Core.Name "openDynamicUnionType")

_ValueType_dynamicPropertyValueType = (Core.Name "dynamicPropertyValueType")

_ValueType_closedDynamicUnionTypeAlt1 = (Core.Name "closedDynamicUnionTypeAlt1")

_ValueType_closedDynamicUnionTypeAlt2 = (Core.Name "closedDynamicUnionTypeAlt2")

data ListValueTypeAlt1 = 
  ListValueTypeAlt1 {
    listValueTypeAlt1TypeName :: ListValueTypeName,
    listValueTypeAlt1ValueType :: ValueType,
    listValueTypeAlt1MaxLength :: (Maybe MaxLength),
    listValueTypeAlt1NotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeAlt1 = (Core.Name "hydra/ext/gql/openGql.ListValueTypeAlt1")

_ListValueTypeAlt1_typeName = (Core.Name "typeName")

_ListValueTypeAlt1_valueType = (Core.Name "valueType")

_ListValueTypeAlt1_maxLength = (Core.Name "maxLength")

_ListValueTypeAlt1_notNull = (Core.Name "notNull")

data ListValueTypeAlt2 = 
  ListValueTypeAlt2 {
    listValueTypeAlt2ValueType :: ValueType,
    listValueTypeAlt2TypeName :: ListValueTypeName,
    listValueTypeAlt2MaxLength :: (Maybe MaxLength),
    listValueTypeAlt2NotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeAlt2 = (Core.Name "hydra/ext/gql/openGql.ListValueTypeAlt2")

_ListValueTypeAlt2_valueType = (Core.Name "valueType")

_ListValueTypeAlt2_typeName = (Core.Name "typeName")

_ListValueTypeAlt2_maxLength = (Core.Name "maxLength")

_ListValueTypeAlt2_notNull = (Core.Name "notNull")

data ListValueTypeAlt3 = 
  ListValueTypeAlt3 {
    listValueTypeAlt3TypeName :: ListValueTypeName,
    listValueTypeAlt3MaxLength :: (Maybe MaxLength),
    listValueTypeAlt3NotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeAlt3 = (Core.Name "hydra/ext/gql/openGql.ListValueTypeAlt3")

_ListValueTypeAlt3_typeName = (Core.Name "typeName")

_ListValueTypeAlt3_maxLength = (Core.Name "maxLength")

_ListValueTypeAlt3_notNull = (Core.Name "notNull")

data OpenDynamicUnionType = 
  OpenDynamicUnionType {
    openDynamicUnionTypeValue :: Bool,
    openDynamicUnionTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenDynamicUnionType = (Core.Name "hydra/ext/gql/openGql.OpenDynamicUnionType")

_OpenDynamicUnionType_value = (Core.Name "value")

_OpenDynamicUnionType_notNull = (Core.Name "notNull")

data DynamicPropertyValueType = 
  DynamicPropertyValueType {
    dynamicPropertyValueTypeAny :: (Maybe Bool),
    dynamicPropertyValueTypeProperty :: (),
    dynamicPropertyValueTypeValue :: (),
    dynamicPropertyValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_DynamicPropertyValueType = (Core.Name "hydra/ext/gql/openGql.DynamicPropertyValueType")

_DynamicPropertyValueType_any = (Core.Name "any")

_DynamicPropertyValueType_property = (Core.Name "property")

_DynamicPropertyValueType_value = (Core.Name "value")

_DynamicPropertyValueType_notNull = (Core.Name "notNull")

data ClosedDynamicUnionTypeAlt1 = 
  ClosedDynamicUnionTypeAlt1 {
    closedDynamicUnionTypeAlt1AnyValue :: (Maybe Bool),
    closedDynamicUnionTypeAlt1ValueTypes :: [ValueType]}
  deriving (Eq, Ord, Read, Show)

_ClosedDynamicUnionTypeAlt1 = (Core.Name "hydra/ext/gql/openGql.ClosedDynamicUnionTypeAlt1")

_ClosedDynamicUnionTypeAlt1_anyValue = (Core.Name "anyValue")

_ClosedDynamicUnionTypeAlt1_valueTypes = (Core.Name "valueTypes")

data ClosedDynamicUnionTypeAlt2 = 
  ClosedDynamicUnionTypeAlt2 {
    closedDynamicUnionTypeAlt2ValueTypes :: [ValueType]}
  deriving (Eq, Ord, Read, Show)

_ClosedDynamicUnionTypeAlt2 = (Core.Name "hydra/ext/gql/openGql.ClosedDynamicUnionTypeAlt2")

_ClosedDynamicUnionTypeAlt2_valueTypes = (Core.Name "valueTypes")

data Typed = 
  Typed {}
  deriving (Eq, Ord, Read, Show)

_Typed = (Core.Name "hydra/ext/gql/openGql.Typed")

data PredefinedType = 
  PredefinedTypeBooleanType BooleanType |
  PredefinedTypeCharacterStringType CharacterStringType |
  PredefinedTypeByteStringType ByteStringType |
  PredefinedTypeNumericType NumericType |
  PredefinedTypeTemporalType TemporalType |
  PredefinedTypeReferenceValueType ReferenceValueType |
  PredefinedTypeImmaterialValueType ImmaterialValueType
  deriving (Eq, Ord, Read, Show)

_PredefinedType = (Core.Name "hydra/ext/gql/openGql.PredefinedType")

_PredefinedType_booleanType = (Core.Name "booleanType")

_PredefinedType_characterStringType = (Core.Name "characterStringType")

_PredefinedType_byteStringType = (Core.Name "byteStringType")

_PredefinedType_numericType = (Core.Name "numericType")

_PredefinedType_temporalType = (Core.Name "temporalType")

_PredefinedType_referenceValueType = (Core.Name "referenceValueType")

_PredefinedType_immaterialValueType = (Core.Name "immaterialValueType")

data BooleanType = 
  BooleanType {
    booleanTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BooleanType = (Core.Name "hydra/ext/gql/openGql.BooleanType")

_BooleanType_notNull = (Core.Name "notNull")

data CharacterStringType = 
  CharacterStringTypeStringType StringType |
  CharacterStringTypeCharType CharType |
  CharacterStringTypeVarcharType VarcharType
  deriving (Eq, Ord, Read, Show)

_CharacterStringType = (Core.Name "hydra/ext/gql/openGql.CharacterStringType")

_CharacterStringType_stringType = (Core.Name "stringType")

_CharacterStringType_charType = (Core.Name "charType")

_CharacterStringType_varcharType = (Core.Name "varcharType")

data StringType = 
  StringType {
    stringTypeMinLength :: (Maybe MinLength),
    stringTypeMaxLength :: (Maybe MaxLength),
    stringTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringType = (Core.Name "hydra/ext/gql/openGql.StringType")

_StringType_minLength = (Core.Name "minLength")

_StringType_maxLength = (Core.Name "maxLength")

_StringType_notNull = (Core.Name "notNull")

data CharType = 
  CharType {
    charTypeFixedLength :: (Maybe FixedLength),
    charTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_CharType = (Core.Name "hydra/ext/gql/openGql.CharType")

_CharType_fixedLength = (Core.Name "fixedLength")

_CharType_notNull = (Core.Name "notNull")

data VarcharType = 
  VarcharType {
    varcharTypeMaxLength :: (Maybe MaxLength),
    varcharTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_VarcharType = (Core.Name "hydra/ext/gql/openGql.VarcharType")

_VarcharType_maxLength = (Core.Name "maxLength")

_VarcharType_notNull = (Core.Name "notNull")

data ByteStringType = 
  ByteStringTypeBytesType BytesType |
  ByteStringTypeBinaryType BinaryType |
  ByteStringTypeVarbinaryType VarbinaryType
  deriving (Eq, Ord, Read, Show)

_ByteStringType = (Core.Name "hydra/ext/gql/openGql.ByteStringType")

_ByteStringType_bytesType = (Core.Name "bytesType")

_ByteStringType_binaryType = (Core.Name "binaryType")

_ByteStringType_varbinaryType = (Core.Name "varbinaryType")

data BytesType = 
  BytesType {
    bytesTypeMinLength :: (Maybe MinLength),
    bytesTypeMaxLength :: (Maybe MaxLength),
    bytesTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BytesType = (Core.Name "hydra/ext/gql/openGql.BytesType")

_BytesType_minLength = (Core.Name "minLength")

_BytesType_maxLength = (Core.Name "maxLength")

_BytesType_notNull = (Core.Name "notNull")

data BinaryType = 
  BinaryType {
    binaryTypeFixedLength :: (Maybe FixedLength),
    binaryTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BinaryType = (Core.Name "hydra/ext/gql/openGql.BinaryType")

_BinaryType_fixedLength = (Core.Name "fixedLength")

_BinaryType_notNull = (Core.Name "notNull")

data VarbinaryType = 
  VarbinaryType {
    varbinaryTypeMaxLength :: (Maybe MaxLength),
    varbinaryTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_VarbinaryType = (Core.Name "hydra/ext/gql/openGql.VarbinaryType")

_VarbinaryType_maxLength = (Core.Name "maxLength")

_VarbinaryType_notNull = (Core.Name "notNull")

newtype MinLength = 
  MinLength {
    unMinLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_MinLength = (Core.Name "hydra/ext/gql/openGql.MinLength")

newtype FixedLength = 
  FixedLength {
    unFixedLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_FixedLength = (Core.Name "hydra/ext/gql/openGql.FixedLength")

newtype MaxLength = 
  MaxLength {
    unMaxLength :: UnsignedInteger}
  deriving (Eq, Ord, Read, Show)

_MaxLength = (Core.Name "hydra/ext/gql/openGql.MaxLength")

data NumericType = 
  NumericTypeExact ExactNumericType |
  NumericTypeApproximate ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = (Core.Name "hydra/ext/gql/openGql.NumericType")

_NumericType_exact = (Core.Name "exact")

_NumericType_approximate = (Core.Name "approximate")

data ExactNumericType = 
  ExactNumericTypeBinary BinaryExactNumericType |
  ExactNumericTypeDecimal DecimalExactNumericType
  deriving (Eq, Ord, Read, Show)

_ExactNumericType = (Core.Name "hydra/ext/gql/openGql.ExactNumericType")

_ExactNumericType_binary = (Core.Name "binary")

_ExactNumericType_decimal = (Core.Name "decimal")

data BinaryExactNumericType = 
  BinaryExactNumericTypeSigned SignedBinaryExactNumericType |
  BinaryExactNumericTypeUnsigned UnsignedBinaryExactNumericType
  deriving (Eq, Ord, Read, Show)

_BinaryExactNumericType = (Core.Name "hydra/ext/gql/openGql.BinaryExactNumericType")

_BinaryExactNumericType_signed = (Core.Name "signed")

_BinaryExactNumericType_unsigned = (Core.Name "unsigned")

data SignedBinaryExactNumericType = 
  SignedBinaryExactNumericTypeInt8 Int8Type |
  SignedBinaryExactNumericTypeInt16 Int16Type |
  SignedBinaryExactNumericTypeInt32 Int32Type |
  SignedBinaryExactNumericTypeInt64 Int64Type |
  SignedBinaryExactNumericTypeInt128 Int128Type |
  SignedBinaryExactNumericTypeInt256 Int256Type |
  SignedBinaryExactNumericTypeSmallInt SmallIntType |
  SignedBinaryExactNumericTypeIntWithPrecision IntWithPrecision |
  SignedBinaryExactNumericTypeBigInt BigIntType |
  SignedBinaryExactNumericTypeSignedVerboseType SignedVerboseBinaryExactNumericType
  deriving (Eq, Ord, Read, Show)

_SignedBinaryExactNumericType = (Core.Name "hydra/ext/gql/openGql.SignedBinaryExactNumericType")

_SignedBinaryExactNumericType_int8 = (Core.Name "int8")

_SignedBinaryExactNumericType_int16 = (Core.Name "int16")

_SignedBinaryExactNumericType_int32 = (Core.Name "int32")

_SignedBinaryExactNumericType_int64 = (Core.Name "int64")

_SignedBinaryExactNumericType_int128 = (Core.Name "int128")

_SignedBinaryExactNumericType_int256 = (Core.Name "int256")

_SignedBinaryExactNumericType_smallInt = (Core.Name "smallInt")

_SignedBinaryExactNumericType_intWithPrecision = (Core.Name "intWithPrecision")

_SignedBinaryExactNumericType_bigInt = (Core.Name "bigInt")

_SignedBinaryExactNumericType_signedVerboseType = (Core.Name "signedVerboseType")

data Int8Type = 
  Int8Type {
    int8TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int8Type = (Core.Name "hydra/ext/gql/openGql.Int8Type")

_Int8Type_notNull = (Core.Name "notNull")

data Int16Type = 
  Int16Type {
    int16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int16Type = (Core.Name "hydra/ext/gql/openGql.Int16Type")

_Int16Type_notNull = (Core.Name "notNull")

data Int32Type = 
  Int32Type {
    int32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int32Type = (Core.Name "hydra/ext/gql/openGql.Int32Type")

_Int32Type_notNull = (Core.Name "notNull")

data Int64Type = 
  Int64Type {
    int64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int64Type = (Core.Name "hydra/ext/gql/openGql.Int64Type")

_Int64Type_notNull = (Core.Name "notNull")

data Int128Type = 
  Int128Type {
    int128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int128Type = (Core.Name "hydra/ext/gql/openGql.Int128Type")

_Int128Type_notNull = (Core.Name "notNull")

data Int256Type = 
  Int256Type {
    int256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int256Type = (Core.Name "hydra/ext/gql/openGql.Int256Type")

_Int256Type_notNull = (Core.Name "notNull")

data SmallIntType = 
  SmallIntType {
    smallIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_SmallIntType = (Core.Name "hydra/ext/gql/openGql.SmallIntType")

_SmallIntType_notNull = (Core.Name "notNull")

data BigIntType = 
  BigIntType {
    bigIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BigIntType = (Core.Name "hydra/ext/gql/openGql.BigIntType")

_BigIntType_notNull = (Core.Name "notNull")

data IntWithPrecision = 
  IntWithPrecision {
    intWithPrecisionPrecision :: (Maybe Precision),
    intWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_IntWithPrecision = (Core.Name "hydra/ext/gql/openGql.IntWithPrecision")

_IntWithPrecision_precision = (Core.Name "precision")

_IntWithPrecision_notNull = (Core.Name "notNull")

data SignedVerboseBinaryExactNumericType = 
  SignedVerboseBinaryExactNumericType {
    signedVerboseBinaryExactNumericTypeSigned :: Bool,
    signedVerboseBinaryExactNumericTypeVerboseType :: VerboseBinaryExactNumericType}
  deriving (Eq, Ord, Read, Show)

_SignedVerboseBinaryExactNumericType = (Core.Name "hydra/ext/gql/openGql.SignedVerboseBinaryExactNumericType")

_SignedVerboseBinaryExactNumericType_signed = (Core.Name "signed")

_SignedVerboseBinaryExactNumericType_verboseType = (Core.Name "verboseType")

data UnsignedBinaryExactNumericType = 
  UnsignedBinaryExactNumericTypeUint8 Uint8Type |
  UnsignedBinaryExactNumericTypeUint16 Uint16Type |
  UnsignedBinaryExactNumericTypeUint32 Uint32Type |
  UnsignedBinaryExactNumericTypeUint64 Uint64Type |
  UnsignedBinaryExactNumericTypeUint128 Uint128Type |
  UnsignedBinaryExactNumericTypeUint256 Uint256Type |
  UnsignedBinaryExactNumericTypeUSmallInt USmallIntType |
  UnsignedBinaryExactNumericTypeUintWithPrecision UintWithPrecision |
  UnsignedBinaryExactNumericTypeUBigInt UBigIntType |
  UnsignedBinaryExactNumericTypeUnsigned VerboseBinaryExactNumericType
  deriving (Eq, Ord, Read, Show)

_UnsignedBinaryExactNumericType = (Core.Name "hydra/ext/gql/openGql.UnsignedBinaryExactNumericType")

_UnsignedBinaryExactNumericType_uint8 = (Core.Name "uint8")

_UnsignedBinaryExactNumericType_uint16 = (Core.Name "uint16")

_UnsignedBinaryExactNumericType_uint32 = (Core.Name "uint32")

_UnsignedBinaryExactNumericType_uint64 = (Core.Name "uint64")

_UnsignedBinaryExactNumericType_uint128 = (Core.Name "uint128")

_UnsignedBinaryExactNumericType_uint256 = (Core.Name "uint256")

_UnsignedBinaryExactNumericType_uSmallInt = (Core.Name "uSmallInt")

_UnsignedBinaryExactNumericType_uintWithPrecision = (Core.Name "uintWithPrecision")

_UnsignedBinaryExactNumericType_uBigInt = (Core.Name "uBigInt")

_UnsignedBinaryExactNumericType_unsigned = (Core.Name "unsigned")

data Uint8Type = 
  Uint8Type {
    uint8TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint8Type = (Core.Name "hydra/ext/gql/openGql.Uint8Type")

_Uint8Type_notNull = (Core.Name "notNull")

data Uint16Type = 
  Uint16Type {
    uint16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint16Type = (Core.Name "hydra/ext/gql/openGql.Uint16Type")

_Uint16Type_notNull = (Core.Name "notNull")

data Uint32Type = 
  Uint32Type {
    uint32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint32Type = (Core.Name "hydra/ext/gql/openGql.Uint32Type")

_Uint32Type_notNull = (Core.Name "notNull")

data Uint64Type = 
  Uint64Type {
    uint64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint64Type = (Core.Name "hydra/ext/gql/openGql.Uint64Type")

_Uint64Type_notNull = (Core.Name "notNull")

data Uint128Type = 
  Uint128Type {
    uint128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint128Type = (Core.Name "hydra/ext/gql/openGql.Uint128Type")

_Uint128Type_notNull = (Core.Name "notNull")

data Uint256Type = 
  Uint256Type {
    uint256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint256Type = (Core.Name "hydra/ext/gql/openGql.Uint256Type")

_Uint256Type_notNull = (Core.Name "notNull")

data USmallIntType = 
  USmallIntType {
    uSmallIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_USmallIntType = (Core.Name "hydra/ext/gql/openGql.USmallIntType")

_USmallIntType_notNull = (Core.Name "notNull")

data UBigIntType = 
  UBigIntType {
    uBigIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_UBigIntType = (Core.Name "hydra/ext/gql/openGql.UBigIntType")

_UBigIntType_notNull = (Core.Name "notNull")

data UintWithPrecision = 
  UintWithPrecision {
    uintWithPrecisionPrecision :: (Maybe Precision),
    uintWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_UintWithPrecision = (Core.Name "hydra/ext/gql/openGql.UintWithPrecision")

_UintWithPrecision_precision = (Core.Name "precision")

_UintWithPrecision_notNull = (Core.Name "notNull")

data VerboseBinaryExactNumericType = 
  VerboseBinaryExactNumericTypeInteger8 Integer8Type |
  VerboseBinaryExactNumericTypeInteger16 Integer16Type |
  VerboseBinaryExactNumericTypeInteger32 Integer32Type |
  VerboseBinaryExactNumericTypeInteger64 Integer64Type |
  VerboseBinaryExactNumericTypeInteger128 Integer128Type |
  VerboseBinaryExactNumericTypeInteger256 Integer256Type |
  VerboseBinaryExactNumericTypeSmallInteger SmallIntegerType |
  VerboseBinaryExactNumericTypeIntegerWithPrecision IntegerWithPrecision |
  VerboseBinaryExactNumericTypeBigInteger BigIntegerType
  deriving (Eq, Ord, Read, Show)

_VerboseBinaryExactNumericType = (Core.Name "hydra/ext/gql/openGql.VerboseBinaryExactNumericType")

_VerboseBinaryExactNumericType_integer8 = (Core.Name "integer8")

_VerboseBinaryExactNumericType_integer16 = (Core.Name "integer16")

_VerboseBinaryExactNumericType_integer32 = (Core.Name "integer32")

_VerboseBinaryExactNumericType_integer64 = (Core.Name "integer64")

_VerboseBinaryExactNumericType_integer128 = (Core.Name "integer128")

_VerboseBinaryExactNumericType_integer256 = (Core.Name "integer256")

_VerboseBinaryExactNumericType_smallInteger = (Core.Name "smallInteger")

_VerboseBinaryExactNumericType_integerWithPrecision = (Core.Name "integerWithPrecision")

_VerboseBinaryExactNumericType_bigInteger = (Core.Name "bigInteger")

data Integer8Type = 
  Integer8Type {
    integer8TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer8Type = (Core.Name "hydra/ext/gql/openGql.Integer8Type")

_Integer8Type_notNull = (Core.Name "notNull")

data Integer16Type = 
  Integer16Type {
    integer16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer16Type = (Core.Name "hydra/ext/gql/openGql.Integer16Type")

_Integer16Type_notNull = (Core.Name "notNull")

data Integer32Type = 
  Integer32Type {
    integer32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer32Type = (Core.Name "hydra/ext/gql/openGql.Integer32Type")

_Integer32Type_notNull = (Core.Name "notNull")

data Integer64Type = 
  Integer64Type {
    integer64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer64Type = (Core.Name "hydra/ext/gql/openGql.Integer64Type")

_Integer64Type_notNull = (Core.Name "notNull")

data Integer128Type = 
  Integer128Type {
    integer128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer128Type = (Core.Name "hydra/ext/gql/openGql.Integer128Type")

_Integer128Type_notNull = (Core.Name "notNull")

data Integer256Type = 
  Integer256Type {
    integer256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer256Type = (Core.Name "hydra/ext/gql/openGql.Integer256Type")

_Integer256Type_notNull = (Core.Name "notNull")

data SmallIntegerType = 
  SmallIntegerType {
    smallIntegerTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_SmallIntegerType = (Core.Name "hydra/ext/gql/openGql.SmallIntegerType")

_SmallIntegerType_notNull = (Core.Name "notNull")

data BigIntegerType = 
  BigIntegerType {
    bigIntegerTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BigIntegerType = (Core.Name "hydra/ext/gql/openGql.BigIntegerType")

_BigIntegerType_notNull = (Core.Name "notNull")

data IntegerWithPrecision = 
  IntegerWithPrecision {
    integerWithPrecisionPrecision :: (Maybe Precision),
    integerWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_IntegerWithPrecision = (Core.Name "hydra/ext/gql/openGql.IntegerWithPrecision")

_IntegerWithPrecision_precision = (Core.Name "precision")

_IntegerWithPrecision_notNull = (Core.Name "notNull")

newtype Precision = 
  Precision {
    unPrecision :: UnsignedDecimalInteger}
  deriving (Eq, Ord, Read, Show)

_Precision = (Core.Name "hydra/ext/gql/openGql.Precision")

newtype DecimalExactNumericType = 
  DecimalExactNumericType {
    unDecimalExactNumericType :: (Maybe PrecisionAndScale)}
  deriving (Eq, Ord, Read, Show)

_DecimalExactNumericType = (Core.Name "hydra/ext/gql/openGql.DecimalExactNumericType")

data PrecisionAndScale = 
  PrecisionAndScale {
    precisionAndScalePrecision :: Precision,
    precisionAndScaleScale :: (Maybe Scale),
    precisionAndScaleNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_PrecisionAndScale = (Core.Name "hydra/ext/gql/openGql.PrecisionAndScale")

_PrecisionAndScale_precision = (Core.Name "precision")

_PrecisionAndScale_scale = (Core.Name "scale")

_PrecisionAndScale_notNull = (Core.Name "notNull")

newtype Scale = 
  Scale {
    unScale :: UnsignedDecimalInteger}
  deriving (Eq, Ord, Read, Show)

_Scale = (Core.Name "hydra/ext/gql/openGql.Scale")

data ApproximateNumericType = 
  ApproximateNumericTypeFloat16 Float16Type |
  ApproximateNumericTypeFloat32 Float32Type |
  ApproximateNumericTypeFloat64 Float64Type |
  ApproximateNumericTypeFloat128 Float128Type |
  ApproximateNumericTypeFloat256 Float256Type |
  ApproximateNumericTypeFloatWithPrecision FloatTypeWithPrecision |
  ApproximateNumericTypeReal RealType |
  ApproximateNumericTypeDoubleWithPrecision DoubleTypeWithPrecision
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericType = (Core.Name "hydra/ext/gql/openGql.ApproximateNumericType")

_ApproximateNumericType_float16 = (Core.Name "float16")

_ApproximateNumericType_float32 = (Core.Name "float32")

_ApproximateNumericType_float64 = (Core.Name "float64")

_ApproximateNumericType_float128 = (Core.Name "float128")

_ApproximateNumericType_float256 = (Core.Name "float256")

_ApproximateNumericType_floatWithPrecision = (Core.Name "floatWithPrecision")

_ApproximateNumericType_real = (Core.Name "real")

_ApproximateNumericType_doubleWithPrecision = (Core.Name "doubleWithPrecision")

data Float16Type = 
  Float16Type {
    float16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float16Type = (Core.Name "hydra/ext/gql/openGql.Float16Type")

_Float16Type_notNull = (Core.Name "notNull")

data Float32Type = 
  Float32Type {
    float32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float32Type = (Core.Name "hydra/ext/gql/openGql.Float32Type")

_Float32Type_notNull = (Core.Name "notNull")

data Float64Type = 
  Float64Type {
    float64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float64Type = (Core.Name "hydra/ext/gql/openGql.Float64Type")

_Float64Type_notNull = (Core.Name "notNull")

data Float128Type = 
  Float128Type {
    float128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float128Type = (Core.Name "hydra/ext/gql/openGql.Float128Type")

_Float128Type_notNull = (Core.Name "notNull")

data Float256Type = 
  Float256Type {
    float256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float256Type = (Core.Name "hydra/ext/gql/openGql.Float256Type")

_Float256Type_notNull = (Core.Name "notNull")

data FloatTypeWithPrecision = 
  FloatTypeWithPrecision {
    floatTypeWithPrecisionPrecisionAndScale :: (Maybe PrecisionAndScale),
    floatTypeWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_FloatTypeWithPrecision = (Core.Name "hydra/ext/gql/openGql.FloatTypeWithPrecision")

_FloatTypeWithPrecision_precisionAndScale = (Core.Name "precisionAndScale")

_FloatTypeWithPrecision_notNull = (Core.Name "notNull")

data RealType = 
  RealType {
    realTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_RealType = (Core.Name "hydra/ext/gql/openGql.RealType")

_RealType_notNull = (Core.Name "notNull")

data DoubleTypeWithPrecision = 
  DoubleTypeWithPrecision {
    doubleTypeWithPrecisionPrecision :: Bool,
    doubleTypeWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_DoubleTypeWithPrecision = (Core.Name "hydra/ext/gql/openGql.DoubleTypeWithPrecision")

_DoubleTypeWithPrecision_precision = (Core.Name "precision")

_DoubleTypeWithPrecision_notNull = (Core.Name "notNull")

data TemporalType = 
  TemporalTypeInstantType TemporalInstantType |
  TemporalTypeDurationType TemporalDurationType
  deriving (Eq, Ord, Read, Show)

_TemporalType = (Core.Name "hydra/ext/gql/openGql.TemporalType")

_TemporalType_instantType = (Core.Name "instantType")

_TemporalType_durationType = (Core.Name "durationType")

data TemporalInstantType = 
  TemporalInstantTypeDatetimeType DatetimeType |
  TemporalInstantTypeLocaldatetimeType LocaldatetimeType |
  TemporalInstantTypeDateType DateType |
  TemporalInstantTypeTimeType TimeType |
  TemporalInstantTypeLocaltimeType LocaltimeType
  deriving (Eq, Ord, Read, Show)

_TemporalInstantType = (Core.Name "hydra/ext/gql/openGql.TemporalInstantType")

_TemporalInstantType_datetimeType = (Core.Name "datetimeType")

_TemporalInstantType_localdatetimeType = (Core.Name "localdatetimeType")

_TemporalInstantType_dateType = (Core.Name "dateType")

_TemporalInstantType_timeType = (Core.Name "timeType")

_TemporalInstantType_localtimeType = (Core.Name "localtimeType")

data DatetimeType = 
  DatetimeTypeZonedDatetime ZonedDatetimeType |
  DatetimeTypeTimestampWithTimeZone TimestampWithTimeZoneType
  deriving (Eq, Ord, Read, Show)

_DatetimeType = (Core.Name "hydra/ext/gql/openGql.DatetimeType")

_DatetimeType_zonedDatetime = (Core.Name "zonedDatetime")

_DatetimeType_timestampWithTimeZone = (Core.Name "timestampWithTimeZone")

data ZonedDatetimeType = 
  ZonedDatetimeType {
    zonedDatetimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ZonedDatetimeType = (Core.Name "hydra/ext/gql/openGql.ZonedDatetimeType")

_ZonedDatetimeType_notNull = (Core.Name "notNull")

data TimestampWithTimeZoneType = 
  TimestampWithTimeZoneType {
    timestampWithTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimestampWithTimeZoneType = (Core.Name "hydra/ext/gql/openGql.TimestampWithTimeZoneType")

_TimestampWithTimeZoneType_notNull = (Core.Name "notNull")

data LocaldatetimeType = 
  LocaldatetimeTypeLocalDatetime LocalDatetimeType |
  LocaldatetimeTypeTimestampWithoutTimeZone TimestampWithoutTimeZoneType
  deriving (Eq, Ord, Read, Show)

_LocaldatetimeType = (Core.Name "hydra/ext/gql/openGql.LocaldatetimeType")

_LocaldatetimeType_localDatetime = (Core.Name "localDatetime")

_LocaldatetimeType_timestampWithoutTimeZone = (Core.Name "timestampWithoutTimeZone")

data LocalDatetimeType = 
  LocalDatetimeType {
    localDatetimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_LocalDatetimeType = (Core.Name "hydra/ext/gql/openGql.LocalDatetimeType")

_LocalDatetimeType_notNull = (Core.Name "notNull")

data TimestampWithoutTimeZoneType = 
  TimestampWithoutTimeZoneType {
    timestampWithoutTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimestampWithoutTimeZoneType = (Core.Name "hydra/ext/gql/openGql.TimestampWithoutTimeZoneType")

_TimestampWithoutTimeZoneType_notNull = (Core.Name "notNull")

data DateType = 
  DateType {
    dateTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_DateType = (Core.Name "hydra/ext/gql/openGql.DateType")

_DateType_notNull = (Core.Name "notNull")

data TimeType = 
  TimeTypeZonedTime ZonedTimeType |
  TimeTypeTimeWithTimeZone TimeWithTimeZoneType
  deriving (Eq, Ord, Read, Show)

_TimeType = (Core.Name "hydra/ext/gql/openGql.TimeType")

_TimeType_zonedTime = (Core.Name "zonedTime")

_TimeType_timeWithTimeZone = (Core.Name "timeWithTimeZone")

data ZonedTimeType = 
  ZonedTimeType {
    zonedTimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ZonedTimeType = (Core.Name "hydra/ext/gql/openGql.ZonedTimeType")

_ZonedTimeType_notNull = (Core.Name "notNull")

data TimeWithTimeZoneType = 
  TimeWithTimeZoneType {
    timeWithTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimeWithTimeZoneType = (Core.Name "hydra/ext/gql/openGql.TimeWithTimeZoneType")

_TimeWithTimeZoneType_notNull = (Core.Name "notNull")

data LocaltimeType = 
  LocaltimeTypeLocalTime LocalTimeType |
  LocaltimeTypeTimeWithoutTimeZone TimeWithoutTimeZoneType
  deriving (Eq, Ord, Read, Show)

_LocaltimeType = (Core.Name "hydra/ext/gql/openGql.LocaltimeType")

_LocaltimeType_localTime = (Core.Name "localTime")

_LocaltimeType_timeWithoutTimeZone = (Core.Name "timeWithoutTimeZone")

data LocalTimeType = 
  LocalTimeType {
    localTimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_LocalTimeType = (Core.Name "hydra/ext/gql/openGql.LocalTimeType")

_LocalTimeType_notNull = (Core.Name "notNull")

data TimeWithoutTimeZoneType = 
  TimeWithoutTimeZoneType {
    timeWithoutTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimeWithoutTimeZoneType = (Core.Name "hydra/ext/gql/openGql.TimeWithoutTimeZoneType")

_TimeWithoutTimeZoneType_notNull = (Core.Name "notNull")

data TemporalDurationType = 
  TemporalDurationType {
    temporalDurationTypeQualifier :: TemporalDurationQualifier,
    temporalDurationTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TemporalDurationType = (Core.Name "hydra/ext/gql/openGql.TemporalDurationType")

_TemporalDurationType_qualifier = (Core.Name "qualifier")

_TemporalDurationType_notNull = (Core.Name "notNull")

data TemporalDurationQualifier = 
  TemporalDurationQualifierYearToMonth  |
  TemporalDurationQualifierDayToSecond 
  deriving (Eq, Ord, Read, Show)

_TemporalDurationQualifier = (Core.Name "hydra/ext/gql/openGql.TemporalDurationQualifier")

_TemporalDurationQualifier_yearToMonth = (Core.Name "yearToMonth")

_TemporalDurationQualifier_dayToSecond = (Core.Name "dayToSecond")

data ReferenceValueType = 
  ReferenceValueTypeGraph GraphReferenceValueType |
  ReferenceValueTypeBindingTable BindingTableReferenceValueType |
  ReferenceValueTypeNode NodeReferenceValueType |
  ReferenceValueTypeEdge EdgeReferenceValueType
  deriving (Eq, Ord, Read, Show)

_ReferenceValueType = (Core.Name "hydra/ext/gql/openGql.ReferenceValueType")

_ReferenceValueType_graph = (Core.Name "graph")

_ReferenceValueType_bindingTable = (Core.Name "bindingTable")

_ReferenceValueType_node = (Core.Name "node")

_ReferenceValueType_edge = (Core.Name "edge")

data ImmaterialValueType = 
  ImmaterialValueTypeNullType NullType |
  ImmaterialValueTypeEmptyType EmptyType
  deriving (Eq, Ord, Read, Show)

_ImmaterialValueType = (Core.Name "hydra/ext/gql/openGql.ImmaterialValueType")

_ImmaterialValueType_nullType = (Core.Name "nullType")

_ImmaterialValueType_emptyType = (Core.Name "emptyType")

data NullType = 
  NullType {}
  deriving (Eq, Ord, Read, Show)

_NullType = (Core.Name "hydra/ext/gql/openGql.NullType")

data EmptyType = 
  EmptyType {}
  deriving (Eq, Ord, Read, Show)

_EmptyType = (Core.Name "hydra/ext/gql/openGql.EmptyType")

data GraphReferenceValueType = 
  GraphReferenceValueTypeOpen OpenGraphReferenceValueType |
  GraphReferenceValueTypeClosed ClosedGraphReferenceValueType
  deriving (Eq, Ord, Read, Show)

_GraphReferenceValueType = (Core.Name "hydra/ext/gql/openGql.GraphReferenceValueType")

_GraphReferenceValueType_open = (Core.Name "open")

_GraphReferenceValueType_closed = (Core.Name "closed")

data ClosedGraphReferenceValueType = 
  ClosedGraphReferenceValueType {
    closedGraphReferenceValueTypeProperty :: Bool,
    closedGraphReferenceValueTypeNestedSpec :: NestedGraphTypeSpecification,
    closedGraphReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ClosedGraphReferenceValueType = (Core.Name "hydra/ext/gql/openGql.ClosedGraphReferenceValueType")

_ClosedGraphReferenceValueType_property = (Core.Name "property")

_ClosedGraphReferenceValueType_nestedSpec = (Core.Name "nestedSpec")

_ClosedGraphReferenceValueType_notNull = (Core.Name "notNull")

data OpenGraphReferenceValueType = 
  OpenGraphReferenceValueType {
    openGraphReferenceValueTypeAny :: (Maybe Bool),
    openGraphReferenceValueTypeProperty :: Bool,
    openGraphReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenGraphReferenceValueType = (Core.Name "hydra/ext/gql/openGql.OpenGraphReferenceValueType")

_OpenGraphReferenceValueType_any = (Core.Name "any")

_OpenGraphReferenceValueType_property = (Core.Name "property")

_OpenGraphReferenceValueType_notNull = (Core.Name "notNull")

data BindingTableReferenceValueType = 
  BindingTableReferenceValueType {
    bindingTableReferenceValueTypeBindingTableType :: BindingTableType,
    bindingTableReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BindingTableReferenceValueType = (Core.Name "hydra/ext/gql/openGql.BindingTableReferenceValueType")

_BindingTableReferenceValueType_bindingTableType = (Core.Name "bindingTableType")

_BindingTableReferenceValueType_notNull = (Core.Name "notNull")

data NodeReferenceValueType = 
  NodeReferenceValueTypeOpen OpenNodeReferenceValueType |
  NodeReferenceValueTypeClosed ClosedNodeReferenceValueType
  deriving (Eq, Ord, Read, Show)

_NodeReferenceValueType = (Core.Name "hydra/ext/gql/openGql.NodeReferenceValueType")

_NodeReferenceValueType_open = (Core.Name "open")

_NodeReferenceValueType_closed = (Core.Name "closed")

data ClosedNodeReferenceValueType = 
  ClosedNodeReferenceValueType {
    closedNodeReferenceValueTypeNodeTypeSpec :: NodeTypeSpecification,
    closedNodeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ClosedNodeReferenceValueType = (Core.Name "hydra/ext/gql/openGql.ClosedNodeReferenceValueType")

_ClosedNodeReferenceValueType_nodeTypeSpec = (Core.Name "nodeTypeSpec")

_ClosedNodeReferenceValueType_notNull = (Core.Name "notNull")

data OpenNodeReferenceValueType = 
  OpenNodeReferenceValueType {
    openNodeReferenceValueTypeAny :: Bool,
    openNodeReferenceValueTypeNodeSynonym :: NodeSynonym,
    openNodeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenNodeReferenceValueType = (Core.Name "hydra/ext/gql/openGql.OpenNodeReferenceValueType")

_OpenNodeReferenceValueType_any = (Core.Name "any")

_OpenNodeReferenceValueType_nodeSynonym = (Core.Name "nodeSynonym")

_OpenNodeReferenceValueType_notNull = (Core.Name "notNull")

data EdgeReferenceValueType = 
  EdgeReferenceValueTypeOpen OpenEdgeReferenceValueType |
  EdgeReferenceValueTypeClosed ClosedEdgeReferenceValueType
  deriving (Eq, Ord, Read, Show)

_EdgeReferenceValueType = (Core.Name "hydra/ext/gql/openGql.EdgeReferenceValueType")

_EdgeReferenceValueType_open = (Core.Name "open")

_EdgeReferenceValueType_closed = (Core.Name "closed")

data ClosedEdgeReferenceValueType = 
  ClosedEdgeReferenceValueType {
    closedEdgeReferenceValueTypeEdgeTypeSpec :: EdgeTypeSpecification,
    closedEdgeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ClosedEdgeReferenceValueType = (Core.Name "hydra/ext/gql/openGql.ClosedEdgeReferenceValueType")

_ClosedEdgeReferenceValueType_edgeTypeSpec = (Core.Name "edgeTypeSpec")

_ClosedEdgeReferenceValueType_notNull = (Core.Name "notNull")

data OpenEdgeReferenceValueType = 
  OpenEdgeReferenceValueType {
    openEdgeReferenceValueTypeAny :: Bool,
    openEdgeReferenceValueTypeEdgeSynonym :: EdgeSynonym,
    openEdgeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenEdgeReferenceValueType = (Core.Name "hydra/ext/gql/openGql.OpenEdgeReferenceValueType")

_OpenEdgeReferenceValueType_any = (Core.Name "any")

_OpenEdgeReferenceValueType_edgeSynonym = (Core.Name "edgeSynonym")

_OpenEdgeReferenceValueType_notNull = (Core.Name "notNull")

data PathValueType = 
  PathValueType {
    pathValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_PathValueType = (Core.Name "hydra/ext/gql/openGql.PathValueType")

_PathValueType_notNull = (Core.Name "notNull")

data ListValueTypeName = 
  ListValueTypeName {
    listValueTypeNameGroup :: Bool,
    listValueTypeNameSynonym :: ListValueTypeNameSynonym}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeName = (Core.Name "hydra/ext/gql/openGql.ListValueTypeName")

_ListValueTypeName_group = (Core.Name "group")

_ListValueTypeName_synonym = (Core.Name "synonym")

data ListValueTypeNameSynonym = 
  ListValueTypeNameSynonymList  |
  ListValueTypeNameSynonymArray 
  deriving (Eq, Ord, Read, Show)

_ListValueTypeNameSynonym = (Core.Name "hydra/ext/gql/openGql.ListValueTypeNameSynonym")

_ListValueTypeNameSynonym_list = (Core.Name "list")

_ListValueTypeNameSynonym_array = (Core.Name "array")

data RecordType = 
  RecordTypeAnyRecord AnyRecordType |
  RecordTypeSpecifiedRecord SpecifiedRecordType
  deriving (Eq, Ord, Read, Show)

_RecordType = (Core.Name "hydra/ext/gql/openGql.RecordType")

_RecordType_anyRecord = (Core.Name "anyRecord")

_RecordType_specifiedRecord = (Core.Name "specifiedRecord")

data AnyRecordType = 
  AnyRecordType {
    anyRecordTypeAny :: Bool,
    anyRecordTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_AnyRecordType = (Core.Name "hydra/ext/gql/openGql.AnyRecordType")

_AnyRecordType_any = (Core.Name "any")

_AnyRecordType_notNull = (Core.Name "notNull")

data SpecifiedRecordType = 
  SpecifiedRecordType {
    specifiedRecordTypeRecord :: Bool,
    specifiedRecordTypeFieldTypes :: FieldTypesSpecification,
    specifiedRecordTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_SpecifiedRecordType = (Core.Name "hydra/ext/gql/openGql.SpecifiedRecordType")

_SpecifiedRecordType_record = (Core.Name "record")

_SpecifiedRecordType_fieldTypes = (Core.Name "fieldTypes")

_SpecifiedRecordType_notNull = (Core.Name "notNull")

newtype FieldTypesSpecification = 
  FieldTypesSpecification {
    unFieldTypesSpecification :: (Maybe FieldTypeList)}
  deriving (Eq, Ord, Read, Show)

_FieldTypesSpecification = (Core.Name "hydra/ext/gql/openGql.FieldTypesSpecification")

newtype FieldTypeList = 
  FieldTypeList {
    unFieldTypeList :: [FieldType]}
  deriving (Eq, Ord, Read, Show)

_FieldTypeList = (Core.Name "hydra/ext/gql/openGql.FieldTypeList")

data NotNull = 
  NotNull {}
  deriving (Eq, Ord, Read, Show)

_NotNull = (Core.Name "hydra/ext/gql/openGql.NotNull")

data FieldType = 
  FieldType {
    fieldTypeFieldName :: FieldName,
    fieldTypeTyped :: (Maybe Typed),
    fieldTypeValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_FieldType = (Core.Name "hydra/ext/gql/openGql.FieldType")

_FieldType_fieldName = (Core.Name "fieldName")

_FieldType_typed = (Core.Name "typed")

_FieldType_valueType = (Core.Name "valueType")

newtype SearchCondition = 
  SearchCondition {
    unSearchCondition :: BooleanValueExpression}
  deriving (Eq, Ord, Read, Show)

_SearchCondition = (Core.Name "hydra/ext/gql/openGql.SearchCondition")

data Predicate = 
  PredicateExistsPredicate ExistsPredicate |
  PredicateNullPredicate NullPredicate |
  PredicateValueTypePredicate ValueTypePredicate |
  PredicateDirectedPredicate DirectedPredicate |
  PredicateLabeledPredicate LabeledPredicate |
  PredicateSourceDestinationPredicate SourceDestinationPredicate |
  PredicateAllDifferentPredicate AllDifferentPredicate |
  PredicateSamePredicate SamePredicate |
  PredicatePropertyExistsPredicate PropertyExistsPredicate
  deriving (Eq, Ord, Read, Show)

_Predicate = (Core.Name "hydra/ext/gql/openGql.Predicate")

_Predicate_existsPredicate = (Core.Name "existsPredicate")

_Predicate_nullPredicate = (Core.Name "nullPredicate")

_Predicate_valueTypePredicate = (Core.Name "valueTypePredicate")

_Predicate_directedPredicate = (Core.Name "directedPredicate")

_Predicate_labeledPredicate = (Core.Name "labeledPredicate")

_Predicate_sourceDestinationPredicate = (Core.Name "sourceDestinationPredicate")

_Predicate_allDifferentPredicate = (Core.Name "allDifferentPredicate")

_Predicate_samePredicate = (Core.Name "samePredicate")

_Predicate_propertyExistsPredicate = (Core.Name "propertyExistsPredicate")

data ComparisonPredicatePart2 = 
  ComparisonPredicatePart2 {
    comparisonPredicatePart2CompOp :: CompOp,
    comparisonPredicatePart2ValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ComparisonPredicatePart2 = (Core.Name "hydra/ext/gql/openGql.ComparisonPredicatePart2")

_ComparisonPredicatePart2_compOp = (Core.Name "compOp")

_ComparisonPredicatePart2_valueExpression = (Core.Name "valueExpression")

data CompOp = 
  CompOpEquals  |
  CompOpNotEquals  |
  CompOpLessThan  |
  CompOpGreaterThan  |
  CompOpLessThanOrEquals  |
  CompOpGreaterThanOrEquals 
  deriving (Eq, Ord, Read, Show)

_CompOp = (Core.Name "hydra/ext/gql/openGql.CompOp")

_CompOp_equals = (Core.Name "equals")

_CompOp_notEquals = (Core.Name "notEquals")

_CompOp_lessThan = (Core.Name "lessThan")

_CompOp_greaterThan = (Core.Name "greaterThan")

_CompOp_lessThanOrEquals = (Core.Name "lessThanOrEquals")

_CompOp_greaterThanOrEquals = (Core.Name "greaterThanOrEquals")

data ExistsPredicate = 
  ExistsPredicateGraphPatternBrace GraphPattern |
  ExistsPredicateGraphPatternParen GraphPattern |
  ExistsPredicateMatchBlockBrace MatchStatementBlock |
  ExistsPredicateMatchBlockParen MatchStatementBlock |
  ExistsPredicateNestedQuery NestedQuerySpecification
  deriving (Eq, Ord, Read, Show)

_ExistsPredicate = (Core.Name "hydra/ext/gql/openGql.ExistsPredicate")

_ExistsPredicate_graphPatternBrace = (Core.Name "graphPatternBrace")

_ExistsPredicate_graphPatternParen = (Core.Name "graphPatternParen")

_ExistsPredicate_matchBlockBrace = (Core.Name "matchBlockBrace")

_ExistsPredicate_matchBlockParen = (Core.Name "matchBlockParen")

_ExistsPredicate_nestedQuery = (Core.Name "nestedQuery")

data NullPredicate = 
  NullPredicate {
    nullPredicateValueExpression :: ValueExpressionPrimary,
    nullPredicateNullPart :: NullPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_NullPredicate = (Core.Name "hydra/ext/gql/openGql.NullPredicate")

_NullPredicate_valueExpression = (Core.Name "valueExpression")

_NullPredicate_nullPart = (Core.Name "nullPart")

data NullPredicatePart2 = 
  NullPredicatePart2 {
    nullPredicatePart2Not :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullPredicatePart2 = (Core.Name "hydra/ext/gql/openGql.NullPredicatePart2")

_NullPredicatePart2_not = (Core.Name "not")

data ValueTypePredicate = 
  ValueTypePredicate {
    valueTypePredicateValueExpression :: ValueExpressionPrimary,
    valueTypePredicateValueTypePart :: ValueTypePredicatePart2}
  deriving (Eq, Ord, Read, Show)

_ValueTypePredicate = (Core.Name "hydra/ext/gql/openGql.ValueTypePredicate")

_ValueTypePredicate_valueExpression = (Core.Name "valueExpression")

_ValueTypePredicate_valueTypePart = (Core.Name "valueTypePart")

data ValueTypePredicatePart2 = 
  ValueTypePredicatePart2 {
    valueTypePredicatePart2Not :: Bool,
    valueTypePredicatePart2Typed :: Typed,
    valueTypePredicatePart2ValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_ValueTypePredicatePart2 = (Core.Name "hydra/ext/gql/openGql.ValueTypePredicatePart2")

_ValueTypePredicatePart2_not = (Core.Name "not")

_ValueTypePredicatePart2_typed = (Core.Name "typed")

_ValueTypePredicatePart2_valueType = (Core.Name "valueType")

data NormalizedPredicatePart2 = 
  NormalizedPredicatePart2 {
    normalizedPredicatePart2Not :: Bool,
    normalizedPredicatePart2NormalForm :: (Maybe NormalForm)}
  deriving (Eq, Ord, Read, Show)

_NormalizedPredicatePart2 = (Core.Name "hydra/ext/gql/openGql.NormalizedPredicatePart2")

_NormalizedPredicatePart2_not = (Core.Name "not")

_NormalizedPredicatePart2_normalForm = (Core.Name "normalForm")

data DirectedPredicate = 
  DirectedPredicate {
    directedPredicateElementVariableReference :: ElementVariableReference,
    directedPredicateDirectedPart :: DirectedPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_DirectedPredicate = (Core.Name "hydra/ext/gql/openGql.DirectedPredicate")

_DirectedPredicate_elementVariableReference = (Core.Name "elementVariableReference")

_DirectedPredicate_directedPart = (Core.Name "directedPart")

data DirectedPredicatePart2 = 
  DirectedPredicatePart2 {
    directedPredicatePart2Not :: Bool}
  deriving (Eq, Ord, Read, Show)

_DirectedPredicatePart2 = (Core.Name "hydra/ext/gql/openGql.DirectedPredicatePart2")

_DirectedPredicatePart2_not = (Core.Name "not")

data LabeledPredicate = 
  LabeledPredicate {
    labeledPredicateElementVariableReference :: ElementVariableReference,
    labeledPredicateLabeledPart :: LabeledPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_LabeledPredicate = (Core.Name "hydra/ext/gql/openGql.LabeledPredicate")

_LabeledPredicate_elementVariableReference = (Core.Name "elementVariableReference")

_LabeledPredicate_labeledPart = (Core.Name "labeledPart")

data LabeledPredicatePart2 = 
  LabeledPredicatePart2 {
    labeledPredicatePart2IsLabeledOrColon :: IsLabeledOrColon,
    labeledPredicatePart2LabelExpression :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_LabeledPredicatePart2 = (Core.Name "hydra/ext/gql/openGql.LabeledPredicatePart2")

_LabeledPredicatePart2_isLabeledOrColon = (Core.Name "isLabeledOrColon")

_LabeledPredicatePart2_labelExpression = (Core.Name "labelExpression")

data IsLabeledOrColon = 
  IsLabeledOrColonNot Bool |
  IsLabeledOrColonColon 
  deriving (Eq, Ord, Read, Show)

_IsLabeledOrColon = (Core.Name "hydra/ext/gql/openGql.IsLabeledOrColon")

_IsLabeledOrColon_not = (Core.Name "not")

_IsLabeledOrColon_colon = (Core.Name "colon")

data SourceDestinationPredicate = 
  SourceDestinationPredicateSourcePredicate SourcePredicate |
  SourceDestinationPredicateDestinationPredicate DestinationPredicate
  deriving (Eq, Ord, Read, Show)

_SourceDestinationPredicate = (Core.Name "hydra/ext/gql/openGql.SourceDestinationPredicate")

_SourceDestinationPredicate_sourcePredicate = (Core.Name "sourcePredicate")

_SourceDestinationPredicate_destinationPredicate = (Core.Name "destinationPredicate")

newtype NodeReference = 
  NodeReference {
    unNodeReference :: ElementVariableReference}
  deriving (Eq, Ord, Read, Show)

_NodeReference = (Core.Name "hydra/ext/gql/openGql.NodeReference")

data SourcePredicate = 
  SourcePredicate {
    sourcePredicateNot :: Bool,
    sourcePredicateSourceOf :: EdgeReference}
  deriving (Eq, Ord, Read, Show)

_SourcePredicate = (Core.Name "hydra/ext/gql/openGql.SourcePredicate")

_SourcePredicate_not = (Core.Name "not")

_SourcePredicate_sourceOf = (Core.Name "sourceOf")

data DestinationPredicate = 
  DestinationPredicate {
    destinationPredicateNodeReference :: NodeReference,
    destinationPredicateNot :: Bool,
    destinationPredicateDestinationOf :: EdgeReference}
  deriving (Eq, Ord, Read, Show)

_DestinationPredicate = (Core.Name "hydra/ext/gql/openGql.DestinationPredicate")

_DestinationPredicate_nodeReference = (Core.Name "nodeReference")

_DestinationPredicate_not = (Core.Name "not")

_DestinationPredicate_destinationOf = (Core.Name "destinationOf")

newtype EdgeReference = 
  EdgeReference {
    unEdgeReference :: ElementVariableReference}
  deriving (Eq, Ord, Read, Show)

_EdgeReference = (Core.Name "hydra/ext/gql/openGql.EdgeReference")

data AllDifferentPredicate = 
  AllDifferentPredicate {
    allDifferentPredicateReferences :: [ElementVariableReference]}
  deriving (Eq, Ord, Read, Show)

_AllDifferentPredicate = (Core.Name "hydra/ext/gql/openGql.AllDifferentPredicate")

_AllDifferentPredicate_references = (Core.Name "references")

data SamePredicate = 
  SamePredicate {
    samePredicateReferences :: [ElementVariableReference]}
  deriving (Eq, Ord, Read, Show)

_SamePredicate = (Core.Name "hydra/ext/gql/openGql.SamePredicate")

_SamePredicate_references = (Core.Name "references")

data PropertyExistsPredicate = 
  PropertyExistsPredicate {
    propertyExistsPredicateElementVariableReference :: ElementVariableReference,
    propertyExistsPredicatePropertyName :: PropertyName}
  deriving (Eq, Ord, Read, Show)

_PropertyExistsPredicate = (Core.Name "hydra/ext/gql/openGql.PropertyExistsPredicate")

_PropertyExistsPredicate_elementVariableReference = (Core.Name "elementVariableReference")

_PropertyExistsPredicate_propertyName = (Core.Name "propertyName")

data ValueExpression = 
  ValueExpressionSigned SignedExpr |
  ValueExpressionMultDiv MultDivExpr |
  ValueExpressionAddSubtract AddSubtractExpr |
  ValueExpressionConcatenation ConcatenationExpr |
  ValueExpressionNot NotExpr |
  ValueExpressionIsNot IsNotExpr |
  ValueExpressionConjunctive ConjunctiveExpr |
  ValueExpressionDisjunctive DisjunctiveExpr |
  ValueExpressionComparison ComparisonExpr |
  ValueExpressionPredicate Predicate |
  ValueExpressionNormalizedPredicate NormalizedPredicateExpr |
  ValueExpressionPropertyGraph GraphExpression |
  ValueExpressionBindingTable BindingTableExpression |
  ValueExpressionValueFunction ValueFunction |
  ValueExpressionPrimary_ ValueExpressionPrimary
  deriving (Eq, Ord, Read, Show)

_ValueExpression = (Core.Name "hydra/ext/gql/openGql.ValueExpression")

_ValueExpression_signed = (Core.Name "signed")

_ValueExpression_multDiv = (Core.Name "multDiv")

_ValueExpression_addSubtract = (Core.Name "addSubtract")

_ValueExpression_concatenation = (Core.Name "concatenation")

_ValueExpression_not = (Core.Name "not")

_ValueExpression_isNot = (Core.Name "isNot")

_ValueExpression_conjunctive = (Core.Name "conjunctive")

_ValueExpression_disjunctive = (Core.Name "disjunctive")

_ValueExpression_comparison = (Core.Name "comparison")

_ValueExpression_predicate = (Core.Name "predicate")

_ValueExpression_normalizedPredicate = (Core.Name "normalizedPredicate")

_ValueExpression_propertyGraph = (Core.Name "propertyGraph")

_ValueExpression_bindingTable = (Core.Name "bindingTable")

_ValueExpression_valueFunction = (Core.Name "valueFunction")

_ValueExpression_primary = (Core.Name "primary")

data SignedExpr = 
  SignedExpr {
    signedExprSign :: Sign,
    signedExprValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_SignedExpr = (Core.Name "hydra/ext/gql/openGql.SignedExpr")

_SignedExpr_sign = (Core.Name "sign")

_SignedExpr_valueExpression = (Core.Name "valueExpression")

data MultDivExpr = 
  MultDivExpr {
    multDivExprLeft :: ValueExpression,
    multDivExprOperator :: MultDivOperator,
    multDivExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_MultDivExpr = (Core.Name "hydra/ext/gql/openGql.MultDivExpr")

_MultDivExpr_left = (Core.Name "left")

_MultDivExpr_operator = (Core.Name "operator")

_MultDivExpr_right = (Core.Name "right")

data AddSubtractExpr = 
  AddSubtractExpr {
    addSubtractExprLeft :: ValueExpression,
    addSubtractExprOperator :: AddSubtractOperator,
    addSubtractExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_AddSubtractExpr = (Core.Name "hydra/ext/gql/openGql.AddSubtractExpr")

_AddSubtractExpr_left = (Core.Name "left")

_AddSubtractExpr_operator = (Core.Name "operator")

_AddSubtractExpr_right = (Core.Name "right")

data ConcatenationExpr = 
  ConcatenationExpr {
    concatenationExprLeft :: ValueExpression,
    concatenationExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ConcatenationExpr = (Core.Name "hydra/ext/gql/openGql.ConcatenationExpr")

_ConcatenationExpr_left = (Core.Name "left")

_ConcatenationExpr_right = (Core.Name "right")

newtype NotExpr = 
  NotExpr {
    unNotExpr :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_NotExpr = (Core.Name "hydra/ext/gql/openGql.NotExpr")

data IsNotExpr = 
  IsNotExpr {
    isNotExprValueExpression :: ValueExpression,
    isNotExprNot :: Bool,
    isNotExprTruthValue :: TruthValue}
  deriving (Eq, Ord, Read, Show)

_IsNotExpr = (Core.Name "hydra/ext/gql/openGql.IsNotExpr")

_IsNotExpr_valueExpression = (Core.Name "valueExpression")

_IsNotExpr_not = (Core.Name "not")

_IsNotExpr_truthValue = (Core.Name "truthValue")

data ConjunctiveExpr = 
  ConjunctiveExpr {
    conjunctiveExprLeft :: ValueExpression,
    conjunctiveExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ConjunctiveExpr = (Core.Name "hydra/ext/gql/openGql.ConjunctiveExpr")

_ConjunctiveExpr_left = (Core.Name "left")

_ConjunctiveExpr_right = (Core.Name "right")

data DisjunctiveExpr = 
  DisjunctiveExpr {
    disjunctiveExprLeft :: ValueExpression,
    disjunctiveExprOperator :: DisjunctiveOperator,
    disjunctiveExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_DisjunctiveExpr = (Core.Name "hydra/ext/gql/openGql.DisjunctiveExpr")

_DisjunctiveExpr_left = (Core.Name "left")

_DisjunctiveExpr_operator = (Core.Name "operator")

_DisjunctiveExpr_right = (Core.Name "right")

data ComparisonExpr = 
  ComparisonExpr {
    comparisonExprValueExpression :: ValueExpression,
    comparisonExprComparison :: ComparisonPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_ComparisonExpr = (Core.Name "hydra/ext/gql/openGql.ComparisonExpr")

_ComparisonExpr_valueExpression = (Core.Name "valueExpression")

_ComparisonExpr_comparison = (Core.Name "comparison")

data NormalizedPredicateExpr = 
  NormalizedPredicateExpr {
    normalizedPredicateExprValueExpression :: ValueExpression,
    normalizedPredicateExprNormalizedPredicate :: NormalizedPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_NormalizedPredicateExpr = (Core.Name "hydra/ext/gql/openGql.NormalizedPredicateExpr")

_NormalizedPredicateExpr_valueExpression = (Core.Name "valueExpression")

_NormalizedPredicateExpr_normalizedPredicate = (Core.Name "normalizedPredicate")

data Sign = 
  SignPlus  |
  SignMinus 
  deriving (Eq, Ord, Read, Show)

_Sign = (Core.Name "hydra/ext/gql/openGql.Sign")

_Sign_plus = (Core.Name "plus")

_Sign_minus = (Core.Name "minus")

data MultDivOperator = 
  MultDivOperatorMultiply  |
  MultDivOperatorDivide 
  deriving (Eq, Ord, Read, Show)

_MultDivOperator = (Core.Name "hydra/ext/gql/openGql.MultDivOperator")

_MultDivOperator_multiply = (Core.Name "multiply")

_MultDivOperator_divide = (Core.Name "divide")

data AddSubtractOperator = 
  AddSubtractOperatorAdd  |
  AddSubtractOperatorSubtract 
  deriving (Eq, Ord, Read, Show)

_AddSubtractOperator = (Core.Name "hydra/ext/gql/openGql.AddSubtractOperator")

_AddSubtractOperator_add = (Core.Name "add")

_AddSubtractOperator_subtract = (Core.Name "subtract")

data DisjunctiveOperator = 
  DisjunctiveOperatorOr  |
  DisjunctiveOperatorXor 
  deriving (Eq, Ord, Read, Show)

_DisjunctiveOperator = (Core.Name "hydra/ext/gql/openGql.DisjunctiveOperator")

_DisjunctiveOperator_or = (Core.Name "or")

_DisjunctiveOperator_xor = (Core.Name "xor")

data ValueFunction = 
  ValueFunctionNumeric NumericValueFunction |
  ValueFunctionDatetimeSubtraction DatetimeSubtraction |
  ValueFunctionDatetime DatetimeValueFunction |
  ValueFunctionDuration DurationValueFunction |
  ValueFunctionCharacterOrByteString CharacterOrByteStringFunction |
  ValueFunctionList ListValueFunction
  deriving (Eq, Ord, Read, Show)

_ValueFunction = (Core.Name "hydra/ext/gql/openGql.ValueFunction")

_ValueFunction_numeric = (Core.Name "numeric")

_ValueFunction_datetimeSubtraction = (Core.Name "datetimeSubtraction")

_ValueFunction_datetime = (Core.Name "datetime")

_ValueFunction_duration = (Core.Name "duration")

_ValueFunction_characterOrByteString = (Core.Name "characterOrByteString")

_ValueFunction_list = (Core.Name "list")

newtype BooleanValueExpression = 
  BooleanValueExpression {
    unBooleanValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_BooleanValueExpression = (Core.Name "hydra/ext/gql/openGql.BooleanValueExpression")

data CharacterOrByteStringFunction = 
  CharacterOrByteStringFunctionSub SubCharacterOrByteString |
  CharacterOrByteStringFunctionTrimSingle TrimSingleCharacterOrByteString |
  CharacterOrByteStringFunctionFold FoldCharacterString |
  CharacterOrByteStringFunctionTrimMultiCharacter TrimMultiCharacterCharacterString |
  CharacterOrByteStringFunctionNormalize NormalizeCharacterString
  deriving (Eq, Ord, Read, Show)

_CharacterOrByteStringFunction = (Core.Name "hydra/ext/gql/openGql.CharacterOrByteStringFunction")

_CharacterOrByteStringFunction_sub = (Core.Name "sub")

_CharacterOrByteStringFunction_trimSingle = (Core.Name "trimSingle")

_CharacterOrByteStringFunction_fold = (Core.Name "fold")

_CharacterOrByteStringFunction_trimMultiCharacter = (Core.Name "trimMultiCharacter")

_CharacterOrByteStringFunction_normalize = (Core.Name "normalize")

data SubCharacterOrByteString = 
  SubCharacterOrByteString {
    subCharacterOrByteStringSide :: Side,
    subCharacterOrByteStringValueExpression :: ValueExpression,
    subCharacterOrByteStringStringLength :: StringLength}
  deriving (Eq, Ord, Read, Show)

_SubCharacterOrByteString = (Core.Name "hydra/ext/gql/openGql.SubCharacterOrByteString")

_SubCharacterOrByteString_side = (Core.Name "side")

_SubCharacterOrByteString_valueExpression = (Core.Name "valueExpression")

_SubCharacterOrByteString_stringLength = (Core.Name "stringLength")

data Side = 
  SideLeft  |
  SideRight 
  deriving (Eq, Ord, Read, Show)

_Side = (Core.Name "hydra/ext/gql/openGql.Side")

_Side_left = (Core.Name "left")

_Side_right = (Core.Name "right")

newtype TrimSingleCharacterOrByteString = 
  TrimSingleCharacterOrByteString {
    unTrimSingleCharacterOrByteString :: TrimOperands}
  deriving (Eq, Ord, Read, Show)

_TrimSingleCharacterOrByteString = (Core.Name "hydra/ext/gql/openGql.TrimSingleCharacterOrByteString")

data FoldCharacterString = 
  FoldCharacterString {
    foldCharacterStringCase :: Case,
    foldCharacterStringValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_FoldCharacterString = (Core.Name "hydra/ext/gql/openGql.FoldCharacterString")

_FoldCharacterString_case = (Core.Name "case")

_FoldCharacterString_valueExpression = (Core.Name "valueExpression")

data Case = 
  CaseUpper  |
  CaseLower 
  deriving (Eq, Ord, Read, Show)

_Case = (Core.Name "hydra/ext/gql/openGql.Case")

_Case_upper = (Core.Name "upper")

_Case_lower = (Core.Name "lower")

data TrimMultiCharacterCharacterString = 
  TrimMultiCharacterCharacterString {
    trimMultiCharacterCharacterStringTrimType :: TrimType,
    trimMultiCharacterCharacterStringValueExpression :: ValueExpression,
    trimMultiCharacterCharacterStringOptionalValueExpression :: (Maybe ValueExpression)}
  deriving (Eq, Ord, Read, Show)

_TrimMultiCharacterCharacterString = (Core.Name "hydra/ext/gql/openGql.TrimMultiCharacterCharacterString")

_TrimMultiCharacterCharacterString_trimType = (Core.Name "trimType")

_TrimMultiCharacterCharacterString_valueExpression = (Core.Name "valueExpression")

_TrimMultiCharacterCharacterString_optionalValueExpression = (Core.Name "optionalValueExpression")

data TrimType = 
  TrimTypeBtrim  |
  TrimTypeLtrim  |
  TrimTypeRtrim 
  deriving (Eq, Ord, Read, Show)

_TrimType = (Core.Name "hydra/ext/gql/openGql.TrimType")

_TrimType_btrim = (Core.Name "btrim")

_TrimType_ltrim = (Core.Name "ltrim")

_TrimType_rtrim = (Core.Name "rtrim")

data NormalizeCharacterString = 
  NormalizeCharacterString {
    normalizeCharacterStringValueExpression :: ValueExpression,
    normalizeCharacterStringNormalForm :: (Maybe NormalForm)}
  deriving (Eq, Ord, Read, Show)

_NormalizeCharacterString = (Core.Name "hydra/ext/gql/openGql.NormalizeCharacterString")

_NormalizeCharacterString_valueExpression = (Core.Name "valueExpression")

_NormalizeCharacterString_normalForm = (Core.Name "normalForm")

newtype NodeReferenceValueExpression = 
  NodeReferenceValueExpression {
    unNodeReferenceValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_NodeReferenceValueExpression = (Core.Name "hydra/ext/gql/openGql.NodeReferenceValueExpression")

newtype EdgeReferenceValueExpression = 
  EdgeReferenceValueExpression {
    unEdgeReferenceValueExpression :: ValueExpressionPrimary}
  deriving (Eq, Ord, Read, Show)

_EdgeReferenceValueExpression = (Core.Name "hydra/ext/gql/openGql.EdgeReferenceValueExpression")

newtype AggregatingValueExpression = 
  AggregatingValueExpression {
    unAggregatingValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_AggregatingValueExpression = (Core.Name "hydra/ext/gql/openGql.AggregatingValueExpression")

data ValueExpressionPrimary = 
  ValueExpressionPrimaryParenthesized ParenthesizedValueExpression |
  ValueExpressionPrimaryAggregateFunction AggregateFunction |
  ValueExpressionPrimaryUnsignedValueSpecification UnsignedValueSpecification |
  ValueExpressionPrimaryPathValueConstructor PathValueConstructor |
  ValueExpressionPrimaryPropertyReference PropertyReference |
  ValueExpressionPrimaryValueQueryExpression ValueQueryExpression |
  ValueExpressionPrimaryCaseExpression CaseExpression |
  ValueExpressionPrimaryCastSpecification CastSpecification |
  ValueExpressionPrimaryElementIdFunction ElementIdFunction |
  ValueExpressionPrimaryLetValueExpression LetValueExpression |
  ValueExpressionPrimaryBindingVariableReference BindingVariableReference
  deriving (Eq, Ord, Read, Show)

_ValueExpressionPrimary = (Core.Name "hydra/ext/gql/openGql.ValueExpressionPrimary")

_ValueExpressionPrimary_parenthesized = (Core.Name "parenthesized")

_ValueExpressionPrimary_aggregateFunction = (Core.Name "aggregateFunction")

_ValueExpressionPrimary_unsignedValueSpecification = (Core.Name "unsignedValueSpecification")

_ValueExpressionPrimary_pathValueConstructor = (Core.Name "pathValueConstructor")

_ValueExpressionPrimary_propertyReference = (Core.Name "propertyReference")

_ValueExpressionPrimary_valueQueryExpression = (Core.Name "valueQueryExpression")

_ValueExpressionPrimary_caseExpression = (Core.Name "caseExpression")

_ValueExpressionPrimary_castSpecification = (Core.Name "castSpecification")

_ValueExpressionPrimary_elementIdFunction = (Core.Name "elementIdFunction")

_ValueExpressionPrimary_letValueExpression = (Core.Name "letValueExpression")

_ValueExpressionPrimary_bindingVariableReference = (Core.Name "bindingVariableReference")

newtype ParenthesizedValueExpression = 
  ParenthesizedValueExpression {
    unParenthesizedValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedValueExpression = (Core.Name "hydra/ext/gql/openGql.ParenthesizedValueExpression")

data NonParenthesizedValueExpressionPrimary = 
  NonParenthesizedValueExpressionPrimarySpecialCase_ NonParenthesizedValueExpressionPrimarySpecialCase |
  NonParenthesizedValueExpressionPrimaryBindingVariableReference BindingVariableReference
  deriving (Eq, Ord, Read, Show)

_NonParenthesizedValueExpressionPrimary = (Core.Name "hydra/ext/gql/openGql.NonParenthesizedValueExpressionPrimary")

_NonParenthesizedValueExpressionPrimary_specialCase = (Core.Name "specialCase")

_NonParenthesizedValueExpressionPrimary_bindingVariableReference = (Core.Name "bindingVariableReference")

data NonParenthesizedValueExpressionPrimarySpecialCase = 
  NonParenthesizedValueExpressionPrimarySpecialCaseAggregateFunction AggregateFunction |
  NonParenthesizedValueExpressionPrimarySpecialCaseUnsignedValueSpecification UnsignedValueSpecification |
  NonParenthesizedValueExpressionPrimarySpecialCasePathValueConstructor PathValueConstructor |
  NonParenthesizedValueExpressionPrimarySpecialCasePropertyReference PropertyReference |
  NonParenthesizedValueExpressionPrimarySpecialCaseValueQueryExpression ValueQueryExpression |
  NonParenthesizedValueExpressionPrimarySpecialCaseCaseExpression CaseExpression |
  NonParenthesizedValueExpressionPrimarySpecialCaseCastSpecification CastSpecification |
  NonParenthesizedValueExpressionPrimarySpecialCaseElementIdFunction ElementIdFunction |
  NonParenthesizedValueExpressionPrimarySpecialCaseLetValueExpression LetValueExpression
  deriving (Eq, Ord, Read, Show)

_NonParenthesizedValueExpressionPrimarySpecialCase = (Core.Name "hydra/ext/gql/openGql.NonParenthesizedValueExpressionPrimarySpecialCase")

_NonParenthesizedValueExpressionPrimarySpecialCase_aggregateFunction = (Core.Name "aggregateFunction")

_NonParenthesizedValueExpressionPrimarySpecialCase_unsignedValueSpecification = (Core.Name "unsignedValueSpecification")

_NonParenthesizedValueExpressionPrimarySpecialCase_pathValueConstructor = (Core.Name "pathValueConstructor")

_NonParenthesizedValueExpressionPrimarySpecialCase_propertyReference = (Core.Name "propertyReference")

_NonParenthesizedValueExpressionPrimarySpecialCase_valueQueryExpression = (Core.Name "valueQueryExpression")

_NonParenthesizedValueExpressionPrimarySpecialCase_caseExpression = (Core.Name "caseExpression")

_NonParenthesizedValueExpressionPrimarySpecialCase_castSpecification = (Core.Name "castSpecification")

_NonParenthesizedValueExpressionPrimarySpecialCase_elementIdFunction = (Core.Name "elementIdFunction")

_NonParenthesizedValueExpressionPrimarySpecialCase_letValueExpression = (Core.Name "letValueExpression")

data UnsignedValueSpecification = 
  UnsignedValueSpecificationUnsignedLiteral UnsignedLiteral |
  UnsignedValueSpecificationGeneralValueSpecification GeneralValueSpecification
  deriving (Eq, Ord, Read, Show)

_UnsignedValueSpecification = (Core.Name "hydra/ext/gql/openGql.UnsignedValueSpecification")

_UnsignedValueSpecification_unsignedLiteral = (Core.Name "unsignedLiteral")

_UnsignedValueSpecification_generalValueSpecification = (Core.Name "generalValueSpecification")

data NonNegativeIntegerSpecification = 
  NonNegativeIntegerSpecificationUnsignedInteger UnsignedInteger |
  NonNegativeIntegerSpecificationDynamicParameterSpecification DynamicParameterSpecification
  deriving (Eq, Ord, Read, Show)

_NonNegativeIntegerSpecification = (Core.Name "hydra/ext/gql/openGql.NonNegativeIntegerSpecification")

_NonNegativeIntegerSpecification_unsignedInteger = (Core.Name "unsignedInteger")

_NonNegativeIntegerSpecification_dynamicParameterSpecification = (Core.Name "dynamicParameterSpecification")

data GeneralValueSpecification = 
  GeneralValueSpecificationDynamicParameterSpecification DynamicParameterSpecification |
  GeneralValueSpecificationSessionUser 
  deriving (Eq, Ord, Read, Show)

_GeneralValueSpecification = (Core.Name "hydra/ext/gql/openGql.GeneralValueSpecification")

_GeneralValueSpecification_dynamicParameterSpecification = (Core.Name "dynamicParameterSpecification")

_GeneralValueSpecification_sessionUser = (Core.Name "sessionUser")

newtype DynamicParameterSpecification = 
  DynamicParameterSpecification {
    unDynamicParameterSpecification :: ParameterName}
  deriving (Eq, Ord, Read, Show)

_DynamicParameterSpecification = (Core.Name "hydra/ext/gql/openGql.DynamicParameterSpecification")

data LetValueExpression = 
  LetValueExpression {
    letValueExpressionLetVariables :: LetVariableDefinitionList,
    letValueExpressionValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_LetValueExpression = (Core.Name "hydra/ext/gql/openGql.LetValueExpression")

_LetValueExpression_letVariables = (Core.Name "letVariables")

_LetValueExpression_valueExpression = (Core.Name "valueExpression")

newtype ValueQueryExpression = 
  ValueQueryExpression {
    unValueQueryExpression :: NestedQuerySpecification}
  deriving (Eq, Ord, Read, Show)

_ValueQueryExpression = (Core.Name "hydra/ext/gql/openGql.ValueQueryExpression")

data CaseExpression = 
  CaseExpressionAbbreviation CaseAbbreviation |
  CaseExpressionSpecification CaseSpecification
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra/ext/gql/openGql.CaseExpression")

_CaseExpression_abbreviation = (Core.Name "abbreviation")

_CaseExpression_specification = (Core.Name "specification")

data CaseAbbreviation = 
  CaseAbbreviationNullIf NullIfAbbreviation |
  CaseAbbreviationCoalesce [ValueExpression]
  deriving (Eq, Ord, Read, Show)

_CaseAbbreviation = (Core.Name "hydra/ext/gql/openGql.CaseAbbreviation")

_CaseAbbreviation_nullIf = (Core.Name "nullIf")

_CaseAbbreviation_coalesce = (Core.Name "coalesce")

data NullIfAbbreviation = 
  NullIfAbbreviation {
    nullIfAbbreviationFirst :: ValueExpression,
    nullIfAbbreviationSecond :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_NullIfAbbreviation = (Core.Name "hydra/ext/gql/openGql.NullIfAbbreviation")

_NullIfAbbreviation_first = (Core.Name "first")

_NullIfAbbreviation_second = (Core.Name "second")

data CaseSpecification = 
  CaseSpecificationSimple SimpleCase |
  CaseSpecificationSearched SearchedCase
  deriving (Eq, Ord, Read, Show)

_CaseSpecification = (Core.Name "hydra/ext/gql/openGql.CaseSpecification")

_CaseSpecification_simple = (Core.Name "simple")

_CaseSpecification_searched = (Core.Name "searched")

data SimpleCase = 
  SimpleCase {
    simpleCaseCaseOperand :: CaseOperand,
    simpleCaseWhenClauses :: [SimpleWhenClause],
    simpleCaseElseClause :: (Maybe ElseClause)}
  deriving (Eq, Ord, Read, Show)

_SimpleCase = (Core.Name "hydra/ext/gql/openGql.SimpleCase")

_SimpleCase_caseOperand = (Core.Name "caseOperand")

_SimpleCase_whenClauses = (Core.Name "whenClauses")

_SimpleCase_elseClause = (Core.Name "elseClause")

data SearchedCase = 
  SearchedCase {
    searchedCaseWhenClauses :: [SearchedWhenClause],
    searchedCaseElseClause :: (Maybe ElseClause)}
  deriving (Eq, Ord, Read, Show)

_SearchedCase = (Core.Name "hydra/ext/gql/openGql.SearchedCase")

_SearchedCase_whenClauses = (Core.Name "whenClauses")

_SearchedCase_elseClause = (Core.Name "elseClause")

data SimpleWhenClause = 
  SimpleWhenClause {
    simpleWhenClauseWhenOperands :: WhenOperandList,
    simpleWhenClauseResult :: Result}
  deriving (Eq, Ord, Read, Show)

_SimpleWhenClause = (Core.Name "hydra/ext/gql/openGql.SimpleWhenClause")

_SimpleWhenClause_whenOperands = (Core.Name "whenOperands")

_SimpleWhenClause_result = (Core.Name "result")

data SearchedWhenClause = 
  SearchedWhenClause {
    searchedWhenClauseSearchCondition :: SearchCondition,
    searchedWhenClauseResult :: Result}
  deriving (Eq, Ord, Read, Show)

_SearchedWhenClause = (Core.Name "hydra/ext/gql/openGql.SearchedWhenClause")

_SearchedWhenClause_searchCondition = (Core.Name "searchCondition")

_SearchedWhenClause_result = (Core.Name "result")

newtype ElseClause = 
  ElseClause {
    unElseClause :: Result}
  deriving (Eq, Ord, Read, Show)

_ElseClause = (Core.Name "hydra/ext/gql/openGql.ElseClause")

data CaseOperand = 
  CaseOperandValueExpression NonParenthesizedValueExpressionPrimary |
  CaseOperandElementReference ElementVariableReference
  deriving (Eq, Ord, Read, Show)

_CaseOperand = (Core.Name "hydra/ext/gql/openGql.CaseOperand")

_CaseOperand_valueExpression = (Core.Name "valueExpression")

_CaseOperand_elementReference = (Core.Name "elementReference")

newtype WhenOperandList = 
  WhenOperandList {
    unWhenOperandList :: [WhenOperand]}
  deriving (Eq, Ord, Read, Show)

_WhenOperandList = (Core.Name "hydra/ext/gql/openGql.WhenOperandList")

data WhenOperand = 
  WhenOperandValueExpression NonParenthesizedValueExpressionPrimary |
  WhenOperandComparison ComparisonPredicatePart2 |
  WhenOperandNullPredicate NullPredicatePart2 |
  WhenOperandValueTypePredicate ValueTypePredicatePart2 |
  WhenOperandNormalizedPredicate NormalizedPredicatePart2 |
  WhenOperandDirectedPredicate DirectedPredicatePart2 |
  WhenOperandLabeledPredicate LabeledPredicatePart2 |
  WhenOperandSourcePredicate SourcePredicate |
  WhenOperandDestinationPredicate DestinationPredicate
  deriving (Eq, Ord, Read, Show)

_WhenOperand = (Core.Name "hydra/ext/gql/openGql.WhenOperand")

_WhenOperand_valueExpression = (Core.Name "valueExpression")

_WhenOperand_comparison = (Core.Name "comparison")

_WhenOperand_nullPredicate = (Core.Name "nullPredicate")

_WhenOperand_valueTypePredicate = (Core.Name "valueTypePredicate")

_WhenOperand_normalizedPredicate = (Core.Name "normalizedPredicate")

_WhenOperand_directedPredicate = (Core.Name "directedPredicate")

_WhenOperand_labeledPredicate = (Core.Name "labeledPredicate")

_WhenOperand_sourcePredicate = (Core.Name "sourcePredicate")

_WhenOperand_destinationPredicate = (Core.Name "destinationPredicate")

data Result = 
  ResultExpression_ ResultExpression |
  ResultNullLiteral 
  deriving (Eq, Ord, Read, Show)

_Result = (Core.Name "hydra/ext/gql/openGql.Result")

_Result_expression = (Core.Name "expression")

_Result_nullLiteral = (Core.Name "nullLiteral")

newtype ResultExpression = 
  ResultExpression {
    unResultExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ResultExpression = (Core.Name "hydra/ext/gql/openGql.ResultExpression")

data CastSpecification = 
  CastSpecification {
    castSpecificationOperand :: CastOperand,
    castSpecificationTarget :: CastTarget}
  deriving (Eq, Ord, Read, Show)

_CastSpecification = (Core.Name "hydra/ext/gql/openGql.CastSpecification")

_CastSpecification_operand = (Core.Name "operand")

_CastSpecification_target = (Core.Name "target")

data CastOperand = 
  CastOperandValueExpression ValueExpression |
  CastOperandNullLiteral 
  deriving (Eq, Ord, Read, Show)

_CastOperand = (Core.Name "hydra/ext/gql/openGql.CastOperand")

_CastOperand_valueExpression = (Core.Name "valueExpression")

_CastOperand_nullLiteral = (Core.Name "nullLiteral")

newtype CastTarget = 
  CastTarget {
    unCastTarget :: ValueType}
  deriving (Eq, Ord, Read, Show)

_CastTarget = (Core.Name "hydra/ext/gql/openGql.CastTarget")

data AggregateFunction = 
  AggregateFunctionCountAll  |
  AggregateFunctionGeneralSetFunction GeneralSetFunction |
  AggregateFunctionBinarySetFunction BinarySetFunction
  deriving (Eq, Ord, Read, Show)

_AggregateFunction = (Core.Name "hydra/ext/gql/openGql.AggregateFunction")

_AggregateFunction_countAll = (Core.Name "countAll")

_AggregateFunction_generalSetFunction = (Core.Name "generalSetFunction")

_AggregateFunction_binarySetFunction = (Core.Name "binarySetFunction")

data GeneralSetFunction = 
  GeneralSetFunction {
    generalSetFunctionFunctionType :: GeneralSetFunctionType,
    generalSetFunctionSetQuantifier :: (Maybe SetQuantifier),
    generalSetFunctionValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_GeneralSetFunction = (Core.Name "hydra/ext/gql/openGql.GeneralSetFunction")

_GeneralSetFunction_functionType = (Core.Name "functionType")

_GeneralSetFunction_setQuantifier = (Core.Name "setQuantifier")

_GeneralSetFunction_valueExpression = (Core.Name "valueExpression")

data BinarySetFunction = 
  BinarySetFunction {
    binarySetFunctionFunctionType :: BinarySetFunctionType,
    binarySetFunctionDependentValue :: DependentValueExpression,
    binarySetFunctionIndependentValue :: IndependentValueExpression}
  deriving (Eq, Ord, Read, Show)

_BinarySetFunction = (Core.Name "hydra/ext/gql/openGql.BinarySetFunction")

_BinarySetFunction_functionType = (Core.Name "functionType")

_BinarySetFunction_dependentValue = (Core.Name "dependentValue")

_BinarySetFunction_independentValue = (Core.Name "independentValue")

data GeneralSetFunctionType = 
  GeneralSetFunctionTypeAvg  |
  GeneralSetFunctionTypeCount  |
  GeneralSetFunctionTypeMax  |
  GeneralSetFunctionTypeMin  |
  GeneralSetFunctionTypeSum  |
  GeneralSetFunctionTypeCollectList  |
  GeneralSetFunctionTypeStddevSamp  |
  GeneralSetFunctionTypeStddevPop 
  deriving (Eq, Ord, Read, Show)

_GeneralSetFunctionType = (Core.Name "hydra/ext/gql/openGql.GeneralSetFunctionType")

_GeneralSetFunctionType_avg = (Core.Name "avg")

_GeneralSetFunctionType_count = (Core.Name "count")

_GeneralSetFunctionType_max = (Core.Name "max")

_GeneralSetFunctionType_min = (Core.Name "min")

_GeneralSetFunctionType_sum = (Core.Name "sum")

_GeneralSetFunctionType_collectList = (Core.Name "collectList")

_GeneralSetFunctionType_stddevSamp = (Core.Name "stddevSamp")

_GeneralSetFunctionType_stddevPop = (Core.Name "stddevPop")

data SetQuantifier = 
  SetQuantifierDistinct  |
  SetQuantifierAll 
  deriving (Eq, Ord, Read, Show)

_SetQuantifier = (Core.Name "hydra/ext/gql/openGql.SetQuantifier")

_SetQuantifier_distinct = (Core.Name "distinct")

_SetQuantifier_all = (Core.Name "all")

data BinarySetFunctionType = 
  BinarySetFunctionTypePercentileCont  |
  BinarySetFunctionTypePercentileDisc 
  deriving (Eq, Ord, Read, Show)

_BinarySetFunctionType = (Core.Name "hydra/ext/gql/openGql.BinarySetFunctionType")

_BinarySetFunctionType_percentileCont = (Core.Name "percentileCont")

_BinarySetFunctionType_percentileDisc = (Core.Name "percentileDisc")

data DependentValueExpression = 
  DependentValueExpression {
    dependentValueExpressionSetQuantifier :: (Maybe SetQuantifier),
    dependentValueExpressionNumericValue :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_DependentValueExpression = (Core.Name "hydra/ext/gql/openGql.DependentValueExpression")

_DependentValueExpression_setQuantifier = (Core.Name "setQuantifier")

_DependentValueExpression_numericValue = (Core.Name "numericValue")

newtype IndependentValueExpression = 
  IndependentValueExpression {
    unIndependentValueExpression :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_IndependentValueExpression = (Core.Name "hydra/ext/gql/openGql.IndependentValueExpression")

newtype ElementIdFunction = 
  ElementIdFunction {
    unElementIdFunction :: ElementVariableReference}
  deriving (Eq, Ord, Read, Show)

_ElementIdFunction = (Core.Name "hydra/ext/gql/openGql.ElementIdFunction")

data PropertyReference = 
  PropertyReference {
    propertyReferenceValueExpression :: ValueExpressionPrimary,
    propertyReferencePropertyName :: PropertyName}
  deriving (Eq, Ord, Read, Show)

_PropertyReference = (Core.Name "hydra/ext/gql/openGql.PropertyReference")

_PropertyReference_valueExpression = (Core.Name "valueExpression")

_PropertyReference_propertyName = (Core.Name "propertyName")

newtype BindingVariableReference = 
  BindingVariableReference {
    unBindingVariableReference :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_BindingVariableReference = (Core.Name "hydra/ext/gql/openGql.BindingVariableReference")

newtype PathValueExpression = 
  PathValueExpression {
    unPathValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_PathValueExpression = (Core.Name "hydra/ext/gql/openGql.PathValueExpression")

newtype PathValueConstructor = 
  PathValueConstructor {
    unPathValueConstructor :: PathValueConstructorByEnumeration}
  deriving (Eq, Ord, Read, Show)

_PathValueConstructor = (Core.Name "hydra/ext/gql/openGql.PathValueConstructor")

newtype PathValueConstructorByEnumeration = 
  PathValueConstructorByEnumeration {
    unPathValueConstructorByEnumeration :: PathElementList}
  deriving (Eq, Ord, Read, Show)

_PathValueConstructorByEnumeration = (Core.Name "hydra/ext/gql/openGql.PathValueConstructorByEnumeration")

data PathElementList = 
  PathElementList {
    pathElementListStart :: PathElementListStart,
    pathElementListSteps :: [PathElementListStep]}
  deriving (Eq, Ord, Read, Show)

_PathElementList = (Core.Name "hydra/ext/gql/openGql.PathElementList")

_PathElementList_start = (Core.Name "start")

_PathElementList_steps = (Core.Name "steps")

newtype PathElementListStart = 
  PathElementListStart {
    unPathElementListStart :: NodeReferenceValueExpression}
  deriving (Eq, Ord, Read, Show)

_PathElementListStart = (Core.Name "hydra/ext/gql/openGql.PathElementListStart")

data PathElementListStep = 
  PathElementListStep {
    pathElementListStepEdgeReference :: EdgeReferenceValueExpression,
    pathElementListStepNodeReference :: NodeReferenceValueExpression}
  deriving (Eq, Ord, Read, Show)

_PathElementListStep = (Core.Name "hydra/ext/gql/openGql.PathElementListStep")

_PathElementListStep_edgeReference = (Core.Name "edgeReference")

_PathElementListStep_nodeReference = (Core.Name "nodeReference")

newtype ListValueExpression = 
  ListValueExpression {
    unListValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ListValueExpression = (Core.Name "hydra/ext/gql/openGql.ListValueExpression")

data ListValueFunction = 
  ListValueFunctionTrim TrimListFunction |
  ListValueFunctionElements ElementsFunction
  deriving (Eq, Ord, Read, Show)

_ListValueFunction = (Core.Name "hydra/ext/gql/openGql.ListValueFunction")

_ListValueFunction_trim = (Core.Name "trim")

_ListValueFunction_elements = (Core.Name "elements")

data TrimListFunction = 
  TrimListFunction {
    trimListFunctionListValue :: ListValueExpression,
    trimListFunctionNumericValue :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_TrimListFunction = (Core.Name "hydra/ext/gql/openGql.TrimListFunction")

_TrimListFunction_listValue = (Core.Name "listValue")

_TrimListFunction_numericValue = (Core.Name "numericValue")

newtype ElementsFunction = 
  ElementsFunction {
    unElementsFunction :: PathValueExpression}
  deriving (Eq, Ord, Read, Show)

_ElementsFunction = (Core.Name "hydra/ext/gql/openGql.ElementsFunction")

newtype ListValueConstructor = 
  ListValueConstructor {
    unListValueConstructor :: ListValueConstructorByEnumeration}
  deriving (Eq, Ord, Read, Show)

_ListValueConstructor = (Core.Name "hydra/ext/gql/openGql.ListValueConstructor")

data ListValueConstructorByEnumeration = 
  ListValueConstructorByEnumeration {
    listValueConstructorByEnumerationListValueTypeName :: (Maybe ListValueTypeName),
    listValueConstructorByEnumerationElements :: (Maybe ListElementList)}
  deriving (Eq, Ord, Read, Show)

_ListValueConstructorByEnumeration = (Core.Name "hydra/ext/gql/openGql.ListValueConstructorByEnumeration")

_ListValueConstructorByEnumeration_listValueTypeName = (Core.Name "listValueTypeName")

_ListValueConstructorByEnumeration_elements = (Core.Name "elements")

newtype ListElementList = 
  ListElementList {
    unListElementList :: [ListElement]}
  deriving (Eq, Ord, Read, Show)

_ListElementList = (Core.Name "hydra/ext/gql/openGql.ListElementList")

newtype ListElement = 
  ListElement {
    unListElement :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ListElement = (Core.Name "hydra/ext/gql/openGql.ListElement")

newtype RecordConstructor = 
  RecordConstructor {
    unRecordConstructor :: FieldsSpecification}
  deriving (Eq, Ord, Read, Show)

_RecordConstructor = (Core.Name "hydra/ext/gql/openGql.RecordConstructor")

newtype FieldsSpecification = 
  FieldsSpecification {
    unFieldsSpecification :: (Maybe FieldList)}
  deriving (Eq, Ord, Read, Show)

_FieldsSpecification = (Core.Name "hydra/ext/gql/openGql.FieldsSpecification")

newtype FieldList = 
  FieldList {
    unFieldList :: [Field]}
  deriving (Eq, Ord, Read, Show)

_FieldList = (Core.Name "hydra/ext/gql/openGql.FieldList")

data Field = 
  Field {
    fieldName :: FieldName,
    fieldValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_Field = (Core.Name "hydra/ext/gql/openGql.Field")

_Field_name = (Core.Name "name")

_Field_value = (Core.Name "value")

newtype TruthValue = 
  TruthValue {
    unTruthValue :: BooleanLiteral}
  deriving (Eq, Ord, Read, Show)

_TruthValue = (Core.Name "hydra/ext/gql/openGql.TruthValue")

data NumericValueExpression = 
  NumericValueExpressionSigned SignedNumericValueExpression |
  NumericValueExpressionMultiplicationOrDivision MulDivNumericValueExpression |
  NumericValueExpressionAdditionOrSubtraction AddSubNumericValueExpression |
  NumericValueExpressionPrimary ValueExpressionPrimary |
  NumericValueExpressionFunction NumericValueFunction
  deriving (Eq, Ord, Read, Show)

_NumericValueExpression = (Core.Name "hydra/ext/gql/openGql.NumericValueExpression")

_NumericValueExpression_signed = (Core.Name "signed")

_NumericValueExpression_multiplicationOrDivision = (Core.Name "multiplicationOrDivision")

_NumericValueExpression_additionOrSubtraction = (Core.Name "additionOrSubtraction")

_NumericValueExpression_primary = (Core.Name "primary")

_NumericValueExpression_function = (Core.Name "function")

data SignedNumericValueExpression = 
  SignedNumericValueExpression {
    signedNumericValueExpressionSign :: Sign,
    signedNumericValueExpressionExpression :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_SignedNumericValueExpression = (Core.Name "hydra/ext/gql/openGql.SignedNumericValueExpression")

_SignedNumericValueExpression_sign = (Core.Name "sign")

_SignedNumericValueExpression_expression = (Core.Name "expression")

data MulDivNumericValueExpression = 
  MulDivNumericValueExpression {
    mulDivNumericValueExpressionLeft :: NumericValueExpression,
    mulDivNumericValueExpressionOperator :: MultDivOperator,
    mulDivNumericValueExpressionRight :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_MulDivNumericValueExpression = (Core.Name "hydra/ext/gql/openGql.MulDivNumericValueExpression")

_MulDivNumericValueExpression_left = (Core.Name "left")

_MulDivNumericValueExpression_operator = (Core.Name "operator")

_MulDivNumericValueExpression_right = (Core.Name "right")

data AddSubNumericValueExpression = 
  AddSubNumericValueExpression {
    addSubNumericValueExpressionLeft :: NumericValueExpression,
    addSubNumericValueExpressionOperator :: AddSubtractOperator,
    addSubNumericValueExpressionRight :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_AddSubNumericValueExpression = (Core.Name "hydra/ext/gql/openGql.AddSubNumericValueExpression")

_AddSubNumericValueExpression_left = (Core.Name "left")

_AddSubNumericValueExpression_operator = (Core.Name "operator")

_AddSubNumericValueExpression_right = (Core.Name "right")

data NumericValueFunction = 
  NumericValueFunctionLength LengthExpression |
  NumericValueFunctionCardinality CardinalityExpression |
  NumericValueFunctionAbsoluteValue AbsoluteValueExpression |
  NumericValueFunctionModulus ModulusExpression |
  NumericValueFunctionTrigonometric TrigonometricFunction |
  NumericValueFunctionLogarithm GeneralLogarithmFunction |
  NumericValueFunctionCommonLogarithm CommonLogarithm |
  NumericValueFunctionNaturalLogarithm NaturalLogarithm |
  NumericValueFunctionExponential ExponentialFunction |
  NumericValueFunctionPower PowerFunction |
  NumericValueFunctionSquareRoot SquareRoot |
  NumericValueFunctionFloor FloorFunction |
  NumericValueFunctionCeiling CeilingFunction
  deriving (Eq, Ord, Read, Show)

_NumericValueFunction = (Core.Name "hydra/ext/gql/openGql.NumericValueFunction")

_NumericValueFunction_length = (Core.Name "length")

_NumericValueFunction_cardinality = (Core.Name "cardinality")

_NumericValueFunction_absoluteValue = (Core.Name "absoluteValue")

_NumericValueFunction_modulus = (Core.Name "modulus")

_NumericValueFunction_trigonometric = (Core.Name "trigonometric")

_NumericValueFunction_logarithm = (Core.Name "logarithm")

_NumericValueFunction_commonLogarithm = (Core.Name "commonLogarithm")

_NumericValueFunction_naturalLogarithm = (Core.Name "naturalLogarithm")

_NumericValueFunction_exponential = (Core.Name "exponential")

_NumericValueFunction_power = (Core.Name "power")

_NumericValueFunction_squareRoot = (Core.Name "squareRoot")

_NumericValueFunction_floor = (Core.Name "floor")

_NumericValueFunction_ceiling = (Core.Name "ceiling")

data LengthExpression = 
  LengthExpressionChar CharLengthExpression |
  LengthExpressionByte ByteLengthExpression |
  LengthExpressionPath PathLengthExpression
  deriving (Eq, Ord, Read, Show)

_LengthExpression = (Core.Name "hydra/ext/gql/openGql.LengthExpression")

_LengthExpression_char = (Core.Name "char")

_LengthExpression_byte = (Core.Name "byte")

_LengthExpression_path = (Core.Name "path")

data CardinalityExpression = 
  CardinalityExpressionCardinality CardinalityArgumentExpression |
  CardinalityExpressionSize ListValueExpression
  deriving (Eq, Ord, Read, Show)

_CardinalityExpression = (Core.Name "hydra/ext/gql/openGql.CardinalityExpression")

_CardinalityExpression_cardinality = (Core.Name "cardinality")

_CardinalityExpression_size = (Core.Name "size")

newtype CardinalityArgumentExpression = 
  CardinalityArgumentExpression {
    unCardinalityArgumentExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_CardinalityArgumentExpression = (Core.Name "hydra/ext/gql/openGql.CardinalityArgumentExpression")

newtype CharLengthExpression = 
  CharLengthExpression {
    unCharLengthExpression :: CharacterStringValueExpression}
  deriving (Eq, Ord, Read, Show)

_CharLengthExpression = (Core.Name "hydra/ext/gql/openGql.CharLengthExpression")

newtype ByteLengthExpression = 
  ByteLengthExpression {
    unByteLengthExpression :: ByteStringValueExpression}
  deriving (Eq, Ord, Read, Show)

_ByteLengthExpression = (Core.Name "hydra/ext/gql/openGql.ByteLengthExpression")

newtype PathLengthExpression = 
  PathLengthExpression {
    unPathLengthExpression :: PathValueExpression}
  deriving (Eq, Ord, Read, Show)

_PathLengthExpression = (Core.Name "hydra/ext/gql/openGql.PathLengthExpression")

newtype AbsoluteValueExpression = 
  AbsoluteValueExpression {
    unAbsoluteValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_AbsoluteValueExpression = (Core.Name "hydra/ext/gql/openGql.AbsoluteValueExpression")

data ModulusExpression = 
  ModulusExpression {
    modulusExpressionDividend :: NumericValueExpressionDividend,
    modulusExpressionDivisor :: NumericValueExpressionDivisor}
  deriving (Eq, Ord, Read, Show)

_ModulusExpression = (Core.Name "hydra/ext/gql/openGql.ModulusExpression")

_ModulusExpression_dividend = (Core.Name "dividend")

_ModulusExpression_divisor = (Core.Name "divisor")

newtype NumericValueExpressionDividend = 
  NumericValueExpressionDividend {
    unNumericValueExpressionDividend :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpressionDividend = (Core.Name "hydra/ext/gql/openGql.NumericValueExpressionDividend")

newtype NumericValueExpressionDivisor = 
  NumericValueExpressionDivisor {
    unNumericValueExpressionDivisor :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpressionDivisor = (Core.Name "hydra/ext/gql/openGql.NumericValueExpressionDivisor")

data TrigonometricFunction = 
  TrigonometricFunction {
    trigonometricFunctionName :: TrigonometricFunctionName,
    trigonometricFunctionValue :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_TrigonometricFunction = (Core.Name "hydra/ext/gql/openGql.TrigonometricFunction")

_TrigonometricFunction_name = (Core.Name "name")

_TrigonometricFunction_value = (Core.Name "value")

data TrigonometricFunctionName = 
  TrigonometricFunctionNameSin  |
  TrigonometricFunctionNameCos  |
  TrigonometricFunctionNameTan  |
  TrigonometricFunctionNameCot  |
  TrigonometricFunctionNameSinh  |
  TrigonometricFunctionNameCosh  |
  TrigonometricFunctionNameTanh  |
  TrigonometricFunctionNameAsin  |
  TrigonometricFunctionNameAcos  |
  TrigonometricFunctionNameAtan  |
  TrigonometricFunctionNameDegrees  |
  TrigonometricFunctionNameRadians 
  deriving (Eq, Ord, Read, Show)

_TrigonometricFunctionName = (Core.Name "hydra/ext/gql/openGql.TrigonometricFunctionName")

_TrigonometricFunctionName_sin = (Core.Name "sin")

_TrigonometricFunctionName_cos = (Core.Name "cos")

_TrigonometricFunctionName_tan = (Core.Name "tan")

_TrigonometricFunctionName_cot = (Core.Name "cot")

_TrigonometricFunctionName_sinh = (Core.Name "sinh")

_TrigonometricFunctionName_cosh = (Core.Name "cosh")

_TrigonometricFunctionName_tanh = (Core.Name "tanh")

_TrigonometricFunctionName_asin = (Core.Name "asin")

_TrigonometricFunctionName_acos = (Core.Name "acos")

_TrigonometricFunctionName_atan = (Core.Name "atan")

_TrigonometricFunctionName_degrees = (Core.Name "degrees")

_TrigonometricFunctionName_radians = (Core.Name "radians")

data GeneralLogarithmFunction = 
  GeneralLogarithmFunction {
    generalLogarithmFunctionBase :: GeneralLogarithmBase,
    generalLogarithmFunctionArgument :: GeneralLogarithmArgument}
  deriving (Eq, Ord, Read, Show)

_GeneralLogarithmFunction = (Core.Name "hydra/ext/gql/openGql.GeneralLogarithmFunction")

_GeneralLogarithmFunction_base = (Core.Name "base")

_GeneralLogarithmFunction_argument = (Core.Name "argument")

newtype GeneralLogarithmBase = 
  GeneralLogarithmBase {
    unGeneralLogarithmBase :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_GeneralLogarithmBase = (Core.Name "hydra/ext/gql/openGql.GeneralLogarithmBase")

newtype GeneralLogarithmArgument = 
  GeneralLogarithmArgument {
    unGeneralLogarithmArgument :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_GeneralLogarithmArgument = (Core.Name "hydra/ext/gql/openGql.GeneralLogarithmArgument")

newtype CommonLogarithm = 
  CommonLogarithm {
    unCommonLogarithm :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_CommonLogarithm = (Core.Name "hydra/ext/gql/openGql.CommonLogarithm")

newtype NaturalLogarithm = 
  NaturalLogarithm {
    unNaturalLogarithm :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_NaturalLogarithm = (Core.Name "hydra/ext/gql/openGql.NaturalLogarithm")

newtype ExponentialFunction = 
  ExponentialFunction {
    unExponentialFunction :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_ExponentialFunction = (Core.Name "hydra/ext/gql/openGql.ExponentialFunction")

data PowerFunction = 
  PowerFunction {
    powerFunctionBase :: NumericValueExpressionBase,
    powerFunctionExponent :: NumericValueExpressionExponent}
  deriving (Eq, Ord, Read, Show)

_PowerFunction = (Core.Name "hydra/ext/gql/openGql.PowerFunction")

_PowerFunction_base = (Core.Name "base")

_PowerFunction_exponent = (Core.Name "exponent")

newtype NumericValueExpressionBase = 
  NumericValueExpressionBase {
    unNumericValueExpressionBase :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpressionBase = (Core.Name "hydra/ext/gql/openGql.NumericValueExpressionBase")

newtype NumericValueExpressionExponent = 
  NumericValueExpressionExponent {
    unNumericValueExpressionExponent :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_NumericValueExpressionExponent = (Core.Name "hydra/ext/gql/openGql.NumericValueExpressionExponent")

newtype SquareRoot = 
  SquareRoot {
    unSquareRoot :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_SquareRoot = (Core.Name "hydra/ext/gql/openGql.SquareRoot")

newtype FloorFunction = 
  FloorFunction {
    unFloorFunction :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_FloorFunction = (Core.Name "hydra/ext/gql/openGql.FloorFunction")

newtype CeilingFunction = 
  CeilingFunction {
    unCeilingFunction :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_CeilingFunction = (Core.Name "hydra/ext/gql/openGql.CeilingFunction")

newtype CharacterStringValueExpression = 
  CharacterStringValueExpression {
    unCharacterStringValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_CharacterStringValueExpression = (Core.Name "hydra/ext/gql/openGql.CharacterStringValueExpression")

newtype ByteStringValueExpression = 
  ByteStringValueExpression {
    unByteStringValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ByteStringValueExpression = (Core.Name "hydra/ext/gql/openGql.ByteStringValueExpression")

data TrimOperands = 
  TrimOperands {
    trimOperandsSpecification :: (Maybe TrimSpecification),
    trimOperandsCharacterOrByteString :: (Maybe TrimCharacterOrByteString),
    trimOperandsSource :: TrimCharacterOrByteStringSource}
  deriving (Eq, Ord, Read, Show)

_TrimOperands = (Core.Name "hydra/ext/gql/openGql.TrimOperands")

_TrimOperands_specification = (Core.Name "specification")

_TrimOperands_characterOrByteString = (Core.Name "characterOrByteString")

_TrimOperands_source = (Core.Name "source")

newtype TrimCharacterOrByteStringSource = 
  TrimCharacterOrByteStringSource {
    unTrimCharacterOrByteStringSource :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_TrimCharacterOrByteStringSource = (Core.Name "hydra/ext/gql/openGql.TrimCharacterOrByteStringSource")

data TrimSpecification = 
  TrimSpecificationLeading  |
  TrimSpecificationTrailing  |
  TrimSpecificationBoth 
  deriving (Eq, Ord, Read, Show)

_TrimSpecification = (Core.Name "hydra/ext/gql/openGql.TrimSpecification")

_TrimSpecification_leading = (Core.Name "leading")

_TrimSpecification_trailing = (Core.Name "trailing")

_TrimSpecification_both = (Core.Name "both")

newtype TrimCharacterOrByteString = 
  TrimCharacterOrByteString {
    unTrimCharacterOrByteString :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_TrimCharacterOrByteString = (Core.Name "hydra/ext/gql/openGql.TrimCharacterOrByteString")

data NormalForm = 
  NormalFormNfc  |
  NormalFormNfd  |
  NormalFormNfkc  |
  NormalFormNfkd 
  deriving (Eq, Ord, Read, Show)

_NormalForm = (Core.Name "hydra/ext/gql/openGql.NormalForm")

_NormalForm_nfc = (Core.Name "nfc")

_NormalForm_nfd = (Core.Name "nfd")

_NormalForm_nfkc = (Core.Name "nfkc")

_NormalForm_nfkd = (Core.Name "nfkd")

newtype StringLength = 
  StringLength {
    unStringLength :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_StringLength = (Core.Name "hydra/ext/gql/openGql.StringLength")

newtype DatetimeValueExpression = 
  DatetimeValueExpression {
    unDatetimeValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_DatetimeValueExpression = (Core.Name "hydra/ext/gql/openGql.DatetimeValueExpression")

data DatetimeValueFunction = 
  DatetimeValueFunctionDateFunction DateFunction |
  DatetimeValueFunctionTimeFunction TimeFunction |
  DatetimeValueFunctionDatetimeFunction DatetimeFunction |
  DatetimeValueFunctionLocaltimeFunction LocaltimeFunction |
  DatetimeValueFunctionLocaldatetimeFunction LocaldatetimeFunction
  deriving (Eq, Ord, Read, Show)

_DatetimeValueFunction = (Core.Name "hydra/ext/gql/openGql.DatetimeValueFunction")

_DatetimeValueFunction_dateFunction = (Core.Name "dateFunction")

_DatetimeValueFunction_timeFunction = (Core.Name "timeFunction")

_DatetimeValueFunction_datetimeFunction = (Core.Name "datetimeFunction")

_DatetimeValueFunction_localtimeFunction = (Core.Name "localtimeFunction")

_DatetimeValueFunction_localdatetimeFunction = (Core.Name "localdatetimeFunction")

data DateFunction = 
  DateFunctionCurrentDate  |
  DateFunctionDateWithParams (Maybe DateFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_DateFunction = (Core.Name "hydra/ext/gql/openGql.DateFunction")

_DateFunction_currentDate = (Core.Name "currentDate")

_DateFunction_dateWithParams = (Core.Name "dateWithParams")

data TimeFunction = 
  TimeFunctionCurrentTime  |
  TimeFunctionZonedTimeWithParams (Maybe TimeFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_TimeFunction = (Core.Name "hydra/ext/gql/openGql.TimeFunction")

_TimeFunction_currentTime = (Core.Name "currentTime")

_TimeFunction_zonedTimeWithParams = (Core.Name "zonedTimeWithParams")

newtype LocaltimeFunction = 
  LocaltimeFunction {
    unLocaltimeFunction :: (Maybe TimeFunctionParameters)}
  deriving (Eq, Ord, Read, Show)

_LocaltimeFunction = (Core.Name "hydra/ext/gql/openGql.LocaltimeFunction")

data DatetimeFunction = 
  DatetimeFunctionCurrentTimestamp  |
  DatetimeFunctionZonedDatetimeWithParams (Maybe DatetimeFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_DatetimeFunction = (Core.Name "hydra/ext/gql/openGql.DatetimeFunction")

_DatetimeFunction_currentTimestamp = (Core.Name "currentTimestamp")

_DatetimeFunction_zonedDatetimeWithParams = (Core.Name "zonedDatetimeWithParams")

data LocaldatetimeFunction = 
  LocaldatetimeFunctionLocalTimestamp  |
  LocaldatetimeFunctionLocalDatetimeWithParams (Maybe DatetimeFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_LocaldatetimeFunction = (Core.Name "hydra/ext/gql/openGql.LocaldatetimeFunction")

_LocaldatetimeFunction_localTimestamp = (Core.Name "localTimestamp")

_LocaldatetimeFunction_localDatetimeWithParams = (Core.Name "localDatetimeWithParams")

data DateFunctionParameters = 
  DateFunctionParametersDateString DateString |
  DateFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_DateFunctionParameters = (Core.Name "hydra/ext/gql/openGql.DateFunctionParameters")

_DateFunctionParameters_dateString = (Core.Name "dateString")

_DateFunctionParameters_recordConstructor = (Core.Name "recordConstructor")

data TimeFunctionParameters = 
  TimeFunctionParametersTimeString TimeString |
  TimeFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_TimeFunctionParameters = (Core.Name "hydra/ext/gql/openGql.TimeFunctionParameters")

_TimeFunctionParameters_timeString = (Core.Name "timeString")

_TimeFunctionParameters_recordConstructor = (Core.Name "recordConstructor")

data DatetimeFunctionParameters = 
  DatetimeFunctionParametersDatetimeString DatetimeString |
  DatetimeFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_DatetimeFunctionParameters = (Core.Name "hydra/ext/gql/openGql.DatetimeFunctionParameters")

_DatetimeFunctionParameters_datetimeString = (Core.Name "datetimeString")

_DatetimeFunctionParameters_recordConstructor = (Core.Name "recordConstructor")

newtype DurationValueExpression = 
  DurationValueExpression {
    unDurationValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_DurationValueExpression = (Core.Name "hydra/ext/gql/openGql.DurationValueExpression")

data DatetimeSubtraction = 
  DatetimeSubtraction {
    datetimeSubtractionParameters :: DatetimeSubtractionParameters,
    datetimeSubtractionTemporalDurationQualifier :: (Maybe TemporalDurationQualifier)}
  deriving (Eq, Ord, Read, Show)

_DatetimeSubtraction = (Core.Name "hydra/ext/gql/openGql.DatetimeSubtraction")

_DatetimeSubtraction_parameters = (Core.Name "parameters")

_DatetimeSubtraction_temporalDurationQualifier = (Core.Name "temporalDurationQualifier")

data DatetimeSubtractionParameters = 
  DatetimeSubtractionParameters {
    datetimeSubtractionParametersExpression1 :: DatetimeValueExpression1,
    datetimeSubtractionParametersExpression2 :: DatetimeValueExpression2}
  deriving (Eq, Ord, Read, Show)

_DatetimeSubtractionParameters = (Core.Name "hydra/ext/gql/openGql.DatetimeSubtractionParameters")

_DatetimeSubtractionParameters_expression1 = (Core.Name "expression1")

_DatetimeSubtractionParameters_expression2 = (Core.Name "expression2")

newtype DatetimeValueExpression1 = 
  DatetimeValueExpression1 {
    unDatetimeValueExpression1 :: DatetimeValueExpression}
  deriving (Eq, Ord, Read, Show)

_DatetimeValueExpression1 = (Core.Name "hydra/ext/gql/openGql.DatetimeValueExpression1")

newtype DatetimeValueExpression2 = 
  DatetimeValueExpression2 {
    unDatetimeValueExpression2 :: DatetimeValueExpression}
  deriving (Eq, Ord, Read, Show)

_DatetimeValueExpression2 = (Core.Name "hydra/ext/gql/openGql.DatetimeValueExpression2")

data DurationValueFunction = 
  DurationValueFunctionDurationFunction DurationFunction |
  DurationValueFunctionAbsoluteValue AbsoluteValueExpression
  deriving (Eq, Ord, Read, Show)

_DurationValueFunction = (Core.Name "hydra/ext/gql/openGql.DurationValueFunction")

_DurationValueFunction_durationFunction = (Core.Name "durationFunction")

_DurationValueFunction_absoluteValue = (Core.Name "absoluteValue")

newtype DurationFunction = 
  DurationFunction {
    unDurationFunction :: DurationFunctionParameters}
  deriving (Eq, Ord, Read, Show)

_DurationFunction = (Core.Name "hydra/ext/gql/openGql.DurationFunction")

data DurationFunctionParameters = 
  DurationFunctionParametersDurationString DurationString |
  DurationFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_DurationFunctionParameters = (Core.Name "hydra/ext/gql/openGql.DurationFunctionParameters")

_DurationFunctionParameters_durationString = (Core.Name "durationString")

_DurationFunctionParameters_recordConstructor = (Core.Name "recordConstructor")

newtype ObjectName = 
  ObjectName {
    unObjectName :: String}
  deriving (Eq, Ord, Read, Show)

_ObjectName = (Core.Name "hydra/ext/gql/openGql.ObjectName")

newtype ObjectNameOrBindingVariable = 
  ObjectNameOrBindingVariable {
    unObjectNameOrBindingVariable :: String}
  deriving (Eq, Ord, Read, Show)

_ObjectNameOrBindingVariable = (Core.Name "hydra/ext/gql/openGql.ObjectNameOrBindingVariable")

newtype DirectoryName = 
  DirectoryName {
    unDirectoryName :: String}
  deriving (Eq, Ord, Read, Show)

_DirectoryName = (Core.Name "hydra/ext/gql/openGql.DirectoryName")

newtype SchemaName = 
  SchemaName {
    unSchemaName :: String}
  deriving (Eq, Ord, Read, Show)

_SchemaName = (Core.Name "hydra/ext/gql/openGql.SchemaName")

newtype GraphName = 
  GraphName {
    unGraphName :: String}
  deriving (Eq, Ord, Read, Show)

_GraphName = (Core.Name "hydra/ext/gql/openGql.GraphName")

newtype DelimitedGraphName = 
  DelimitedGraphName {
    unDelimitedGraphName :: String}
  deriving (Eq, Ord, Read, Show)

_DelimitedGraphName = (Core.Name "hydra/ext/gql/openGql.DelimitedGraphName")

newtype GraphTypeName = 
  GraphTypeName {
    unGraphTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_GraphTypeName = (Core.Name "hydra/ext/gql/openGql.GraphTypeName")

newtype NodeTypeName = 
  NodeTypeName {
    unNodeTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_NodeTypeName = (Core.Name "hydra/ext/gql/openGql.NodeTypeName")

newtype EdgeTypeName = 
  EdgeTypeName {
    unEdgeTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_EdgeTypeName = (Core.Name "hydra/ext/gql/openGql.EdgeTypeName")

data BindingTableName = 
  BindingTableNameRegularIdentifier String |
  BindingTableNameDelimitedBindingTableName DelimitedBindingTableName
  deriving (Eq, Ord, Read, Show)

_BindingTableName = (Core.Name "hydra/ext/gql/openGql.BindingTableName")

_BindingTableName_regularIdentifier = (Core.Name "regularIdentifier")

_BindingTableName_delimitedBindingTableName = (Core.Name "delimitedBindingTableName")

newtype DelimitedBindingTableName = 
  DelimitedBindingTableName {
    unDelimitedBindingTableName :: String}
  deriving (Eq, Ord, Read, Show)

_DelimitedBindingTableName = (Core.Name "hydra/ext/gql/openGql.DelimitedBindingTableName")

newtype ProcedureName = 
  ProcedureName {
    unProcedureName :: String}
  deriving (Eq, Ord, Read, Show)

_ProcedureName = (Core.Name "hydra/ext/gql/openGql.ProcedureName")

newtype LabelName = 
  LabelName {
    unLabelName :: String}
  deriving (Eq, Ord, Read, Show)

_LabelName = (Core.Name "hydra/ext/gql/openGql.LabelName")

newtype PropertyName = 
  PropertyName {
    unPropertyName :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyName = (Core.Name "hydra/ext/gql/openGql.PropertyName")

newtype FieldName = 
  FieldName {
    unFieldName :: String}
  deriving (Eq, Ord, Read, Show)

_FieldName = (Core.Name "hydra/ext/gql/openGql.FieldName")

newtype ElementVariable = 
  ElementVariable {
    unElementVariable :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_ElementVariable = (Core.Name "hydra/ext/gql/openGql.ElementVariable")

newtype PathVariable = 
  PathVariable {
    unPathVariable :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_PathVariable = (Core.Name "hydra/ext/gql/openGql.PathVariable")

newtype SubpathVariable = 
  SubpathVariable {
    unSubpathVariable :: String}
  deriving (Eq, Ord, Read, Show)

_SubpathVariable = (Core.Name "hydra/ext/gql/openGql.SubpathVariable")

newtype BindingVariable = 
  BindingVariable {
    unBindingVariable :: String}
  deriving (Eq, Ord, Read, Show)

_BindingVariable = (Core.Name "hydra/ext/gql/openGql.BindingVariable")

data UnsignedLiteral = 
  UnsignedLiteralNumeric UnsignedNumericLiteral |
  UnsignedLiteralGeneral GeneralLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedLiteral = (Core.Name "hydra/ext/gql/openGql.UnsignedLiteral")

_UnsignedLiteral_numeric = (Core.Name "numeric")

_UnsignedLiteral_general = (Core.Name "general")

data GeneralLiteral = 
  GeneralLiteralBoolean BooleanLiteral |
  GeneralLiteralCharacterString CharacterStringLiteral |
  GeneralLiteralByteString ByteStringLiteral |
  GeneralLiteralTemporal TemporalLiteral |
  GeneralLiteralDuration DurationLiteral |
  GeneralLiteralNullLiteral NullLiteral |
  GeneralLiteralList ListLiteral |
  GeneralLiteralRecord RecordLiteral
  deriving (Eq, Ord, Read, Show)

_GeneralLiteral = (Core.Name "hydra/ext/gql/openGql.GeneralLiteral")

_GeneralLiteral_boolean = (Core.Name "boolean")

_GeneralLiteral_characterString = (Core.Name "characterString")

_GeneralLiteral_byteString = (Core.Name "byteString")

_GeneralLiteral_temporal = (Core.Name "temporal")

_GeneralLiteral_duration = (Core.Name "duration")

_GeneralLiteral_nullLiteral = (Core.Name "nullLiteral")

_GeneralLiteral_list = (Core.Name "list")

_GeneralLiteral_record = (Core.Name "record")

data TemporalLiteral = 
  TemporalLiteralDate DateLiteral |
  TemporalLiteralTime TimeLiteral |
  TemporalLiteralDatetime DatetimeLiteral
  deriving (Eq, Ord, Read, Show)

_TemporalLiteral = (Core.Name "hydra/ext/gql/openGql.TemporalLiteral")

_TemporalLiteral_date = (Core.Name "date")

_TemporalLiteral_time = (Core.Name "time")

_TemporalLiteral_datetime = (Core.Name "datetime")

newtype DateLiteral = 
  DateLiteral {
    unDateLiteral :: DateString}
  deriving (Eq, Ord, Read, Show)

_DateLiteral = (Core.Name "hydra/ext/gql/openGql.DateLiteral")

newtype TimeLiteral = 
  TimeLiteral {
    unTimeLiteral :: TimeString}
  deriving (Eq, Ord, Read, Show)

_TimeLiteral = (Core.Name "hydra/ext/gql/openGql.TimeLiteral")

newtype DatetimeLiteral = 
  DatetimeLiteral {
    unDatetimeLiteral :: DatetimeString}
  deriving (Eq, Ord, Read, Show)

_DatetimeLiteral = (Core.Name "hydra/ext/gql/openGql.DatetimeLiteral")

newtype ListLiteral = 
  ListLiteral {
    unListLiteral :: ListValueConstructorByEnumeration}
  deriving (Eq, Ord, Read, Show)

_ListLiteral = (Core.Name "hydra/ext/gql/openGql.ListLiteral")

newtype RecordLiteral = 
  RecordLiteral {
    unRecordLiteral :: RecordConstructor}
  deriving (Eq, Ord, Read, Show)

_RecordLiteral = (Core.Name "hydra/ext/gql/openGql.RecordLiteral")

newtype Identifier = 
  Identifier {
    unIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_Identifier = (Core.Name "hydra/ext/gql/openGql.Identifier")

newtype RegularIdentifier = 
  RegularIdentifier {
    unRegularIdentifier :: String}
  deriving (Eq, Ord, Read, Show)

_RegularIdentifier = (Core.Name "hydra/ext/gql/openGql.RegularIdentifier")

newtype TimeZoneString = 
  TimeZoneString {
    unTimeZoneString :: CharacterStringLiteral}
  deriving (Eq, Ord, Read, Show)

_TimeZoneString = (Core.Name "hydra/ext/gql/openGql.TimeZoneString")

newtype CharacterStringLiteral = 
  CharacterStringLiteral {
    unCharacterStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_CharacterStringLiteral = (Core.Name "hydra/ext/gql/openGql.CharacterStringLiteral")

data UnsignedNumericLiteral = 
  UnsignedNumericLiteralExact ExactNumericLiteral |
  UnsignedNumericLiteralApproximate ApproximateNumericLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedNumericLiteral = (Core.Name "hydra/ext/gql/openGql.UnsignedNumericLiteral")

_UnsignedNumericLiteral_exact = (Core.Name "exact")

_UnsignedNumericLiteral_approximate = (Core.Name "approximate")

data ExactNumericLiteral = 
  ExactNumericLiteralScientificWithSuffix String |
  ExactNumericLiteralCommonWithSuffix String |
  ExactNumericLiteralCommonWithoutSuffix String |
  ExactNumericLiteralIntegerWithSuffix String |
  ExactNumericLiteralUnsignedInteger UnsignedInteger
  deriving (Eq, Ord, Read, Show)

_ExactNumericLiteral = (Core.Name "hydra/ext/gql/openGql.ExactNumericLiteral")

_ExactNumericLiteral_scientificWithSuffix = (Core.Name "scientificWithSuffix")

_ExactNumericLiteral_commonWithSuffix = (Core.Name "commonWithSuffix")

_ExactNumericLiteral_commonWithoutSuffix = (Core.Name "commonWithoutSuffix")

_ExactNumericLiteral_integerWithSuffix = (Core.Name "integerWithSuffix")

_ExactNumericLiteral_unsignedInteger = (Core.Name "unsignedInteger")

data ApproximateNumericLiteral = 
  ApproximateNumericLiteralScientificWithSuffix String |
  ApproximateNumericLiteralScientificWithoutSuffix String |
  ApproximateNumericLiteralCommonWithSuffix String |
  ApproximateNumericLiteralIntegerWithSuffix String
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericLiteral = (Core.Name "hydra/ext/gql/openGql.ApproximateNumericLiteral")

_ApproximateNumericLiteral_scientificWithSuffix = (Core.Name "scientificWithSuffix")

_ApproximateNumericLiteral_scientificWithoutSuffix = (Core.Name "scientificWithoutSuffix")

_ApproximateNumericLiteral_commonWithSuffix = (Core.Name "commonWithSuffix")

_ApproximateNumericLiteral_integerWithSuffix = (Core.Name "integerWithSuffix")

data UnsignedInteger = 
  UnsignedIntegerDecimal String |
  UnsignedIntegerHexadecimal String |
  UnsignedIntegerOctal String |
  UnsignedIntegerBinary String
  deriving (Eq, Ord, Read, Show)

_UnsignedInteger = (Core.Name "hydra/ext/gql/openGql.UnsignedInteger")

_UnsignedInteger_decimal = (Core.Name "decimal")

_UnsignedInteger_hexadecimal = (Core.Name "hexadecimal")

_UnsignedInteger_octal = (Core.Name "octal")

_UnsignedInteger_binary = (Core.Name "binary")

newtype UnsignedDecimalInteger = 
  UnsignedDecimalInteger {
    unUnsignedDecimalInteger :: String}
  deriving (Eq, Ord, Read, Show)

_UnsignedDecimalInteger = (Core.Name "hydra/ext/gql/openGql.UnsignedDecimalInteger")

data NullLiteral = 
  NullLiteral {}
  deriving (Eq, Ord, Read, Show)

_NullLiteral = (Core.Name "hydra/ext/gql/openGql.NullLiteral")

newtype DateString = 
  DateString {
    unDateString :: CharacterStringLiteral}
  deriving (Eq, Ord, Read, Show)

_DateString = (Core.Name "hydra/ext/gql/openGql.DateString")

newtype TimeString = 
  TimeString {
    unTimeString :: CharacterStringLiteral}
  deriving (Eq, Ord, Read, Show)

_TimeString = (Core.Name "hydra/ext/gql/openGql.TimeString")

newtype DatetimeString = 
  DatetimeString {
    unDatetimeString :: CharacterStringLiteral}
  deriving (Eq, Ord, Read, Show)

_DatetimeString = (Core.Name "hydra/ext/gql/openGql.DatetimeString")

newtype DurationLiteral = 
  DurationLiteral {
    unDurationLiteral :: DurationString}
  deriving (Eq, Ord, Read, Show)

_DurationLiteral = (Core.Name "hydra/ext/gql/openGql.DurationLiteral")

newtype DurationString = 
  DurationString {
    unDurationString :: CharacterStringLiteral}
  deriving (Eq, Ord, Read, Show)

_DurationString = (Core.Name "hydra/ext/gql/openGql.DurationString")

data NodeSynonym = 
  NodeSynonymNode  |
  NodeSynonymVertex 
  deriving (Eq, Ord, Read, Show)

_NodeSynonym = (Core.Name "hydra/ext/gql/openGql.NodeSynonym")

_NodeSynonym_node = (Core.Name "node")

_NodeSynonym_vertex = (Core.Name "vertex")

data EdgesSynonym = 
  EdgesSynonymEdges  |
  EdgesSynonymRelationships 
  deriving (Eq, Ord, Read, Show)

_EdgesSynonym = (Core.Name "hydra/ext/gql/openGql.EdgesSynonym")

_EdgesSynonym_edges = (Core.Name "edges")

_EdgesSynonym_relationships = (Core.Name "relationships")

data EdgeSynonym = 
  EdgeSynonymEdge  |
  EdgeSynonymRelationship 
  deriving (Eq, Ord, Read, Show)

_EdgeSynonym = (Core.Name "hydra/ext/gql/openGql.EdgeSynonym")

_EdgeSynonym_edge = (Core.Name "edge")

_EdgeSynonym_relationship = (Core.Name "relationship")

data Implies = 
  ImpliesRightDoubleArrow  |
  ImpliesImplies 
  deriving (Eq, Ord, Read, Show)

_Implies = (Core.Name "hydra/ext/gql/openGql.Implies")

_Implies_rightDoubleArrow = (Core.Name "rightDoubleArrow")

_Implies_implies = (Core.Name "implies")

newtype ParameterName = 
  ParameterName {
    unParameterName :: String}
  deriving (Eq, Ord, Read, Show)

_ParameterName = (Core.Name "hydra/ext/gql/openGql.ParameterName")

data BooleanLiteral = 
  BooleanLiteralTrue  |
  BooleanLiteralFalse  |
  BooleanLiteralUnknown 
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = (Core.Name "hydra/ext/gql/openGql.BooleanLiteral")

_BooleanLiteral_true = (Core.Name "true")

_BooleanLiteral_false = (Core.Name "false")

_BooleanLiteral_unknown = (Core.Name "unknown")

newtype ByteStringLiteral = 
  ByteStringLiteral {
    unByteStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_ByteStringLiteral = (Core.Name "hydra/ext/gql/openGql.ByteStringLiteral")
