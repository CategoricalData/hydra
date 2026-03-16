-- Note: this is an automatically generated file. Do not edit.

-- | A GQL model based on the OpenGQL ANTLR grammar, version 15b256b (2024-09-05), available at: https://github.com/opengql/grammar/blob/main/GQL.g4

module OpenGql.Grammar where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data GqlProgram = 
  GqlProgram {
    gqlProgramActivity :: (Maybe ProgramActivity),
    gqlProgramClose :: (Maybe SessionCloseCommand)}
  deriving (Eq, Ord, Read, Show)

_GqlProgram = Core.Name "openGql.grammar.GqlProgram"

_GqlProgram_activity = Core.Name "activity"

_GqlProgram_close = Core.Name "close"

data ProgramActivity = 
  ProgramActivitySession SessionActivity |
  ProgramActivityTransaction TransactionActivity
  deriving (Eq, Ord, Read, Show)

_ProgramActivity = Core.Name "openGql.grammar.ProgramActivity"

_ProgramActivity_session = Core.Name "session"

_ProgramActivity_transaction = Core.Name "transaction"

data SessionActivity = 
  SessionActivityReset [SessionResetCommand] |
  SessionActivitySetAndResetCommands SessionSetAndResetCommands
  deriving (Eq, Ord, Read, Show)

_SessionActivity = Core.Name "openGql.grammar.SessionActivity"

_SessionActivity_reset = Core.Name "reset"

_SessionActivity_setAndResetCommands = Core.Name "setAndResetCommands"

data SessionSetAndResetCommands = 
  SessionSetAndResetCommands {
    sessionSetAndResetCommandsSet :: [SessionSetCommand],
    sessionSetAndResetCommandsReset :: [SessionResetCommand]}
  deriving (Eq, Ord, Read, Show)

_SessionSetAndResetCommands = Core.Name "openGql.grammar.SessionSetAndResetCommands"

_SessionSetAndResetCommands_set = Core.Name "set"

_SessionSetAndResetCommands_reset = Core.Name "reset"

data TransactionActivity = 
  TransactionActivityStart StartAndMaybeProcedureAndMaybeEnd |
  TransactionActivityProcedure ProcedureAndMaybeEnd |
  TransactionActivityEnd EndTransactionCommand
  deriving (Eq, Ord, Read, Show)

_TransactionActivity = Core.Name "openGql.grammar.TransactionActivity"

_TransactionActivity_start = Core.Name "start"

_TransactionActivity_procedure = Core.Name "procedure"

_TransactionActivity_end = Core.Name "end"

data StartAndMaybeProcedureAndMaybeEnd = 
  StartAndMaybeProcedureAndMaybeEnd {
    startAndMaybeProcedureAndMaybeEndStart :: StartTransactionCommand,
    startAndMaybeProcedureAndMaybeEndProcedureAndEnd :: (Maybe ProcedureAndMaybeEnd)}
  deriving (Eq, Ord, Read, Show)

_StartAndMaybeProcedureAndMaybeEnd = Core.Name "openGql.grammar.StartAndMaybeProcedureAndMaybeEnd"

_StartAndMaybeProcedureAndMaybeEnd_start = Core.Name "start"

_StartAndMaybeProcedureAndMaybeEnd_procedureAndEnd = Core.Name "procedureAndEnd"

data ProcedureAndMaybeEnd = 
  ProcedureAndMaybeEnd {
    procedureAndMaybeEndProcedure :: ProcedureSpecification,
    procedureAndMaybeEndEnd :: (Maybe EndTransactionCommand)}
  deriving (Eq, Ord, Read, Show)

_ProcedureAndMaybeEnd = Core.Name "openGql.grammar.ProcedureAndMaybeEnd"

_ProcedureAndMaybeEnd_procedure = Core.Name "procedure"

_ProcedureAndMaybeEnd_end = Core.Name "end"

data EndTransactionCommand = 
  EndTransactionCommandRollback RollbackCommand |
  EndTransactionCommandCommit CommitCommand
  deriving (Eq, Ord, Read, Show)

_EndTransactionCommand = Core.Name "openGql.grammar.EndTransactionCommand"

_EndTransactionCommand_rollback = Core.Name "rollback"

_EndTransactionCommand_commit = Core.Name "commit"

data SessionSetCommand = 
  SessionSetCommandSchema SessionSetSchemaClause |
  SessionSetCommandGraph SessionSetGraphClause |
  SessionSetCommandTimeZone SessionSetTimeZoneClause |
  SessionSetCommandParameter SessionSetParameterClause
  deriving (Eq, Ord, Read, Show)

_SessionSetCommand = Core.Name "openGql.grammar.SessionSetCommand"

_SessionSetCommand_schema = Core.Name "schema"

_SessionSetCommand_graph = Core.Name "graph"

_SessionSetCommand_timeZone = Core.Name "timeZone"

_SessionSetCommand_parameter = Core.Name "parameter"

type SessionSetSchemaClause = SchemaReference

_SessionSetSchemaClause = Core.Name "openGql.grammar.SessionSetSchemaClause"

type SessionSetGraphClause = GraphExpression

_SessionSetGraphClause = Core.Name "openGql.grammar.SessionSetGraphClause"

type SessionSetTimeZoneClause = SetTimeZoneValue

_SessionSetTimeZoneClause = Core.Name "openGql.grammar.SessionSetTimeZoneClause"

type SetTimeZoneValue = TimeZoneString

_SetTimeZoneValue = Core.Name "openGql.grammar.SetTimeZoneValue"

data SessionSetParameterClause = 
  SessionSetParameterClauseGraph SessionSetGraphParameterClause |
  SessionSetParameterClauseBindings SessionSetBindingTableParameterClause |
  SessionSetParameterClauseValue SessionSetValueParameterClause
  deriving (Eq, Ord, Read, Show)

_SessionSetParameterClause = Core.Name "openGql.grammar.SessionSetParameterClause"

_SessionSetParameterClause_graph = Core.Name "graph"

_SessionSetParameterClause_bindings = Core.Name "bindings"

_SessionSetParameterClause_value = Core.Name "value"

data SessionSetGraphParameterClause = 
  SessionSetGraphParameterClause {
    sessionSetGraphParameterClauseGraph :: SessionSetParameterName,
    sessionSetGraphParameterClauseInitializer :: OptTypedGraphInitializer}
  deriving (Eq, Ord, Read, Show)

_SessionSetGraphParameterClause = Core.Name "openGql.grammar.SessionSetGraphParameterClause"

_SessionSetGraphParameterClause_graph = Core.Name "graph"

_SessionSetGraphParameterClause_initializer = Core.Name "initializer"

data SessionSetBindingTableParameterClause = 
  SessionSetBindingTableParameterClause {
    sessionSetBindingTableParameterClauseBinding :: Bool,
    sessionSetBindingTableParameterClauseParam :: SessionSetParameterName,
    sessionSetBindingTableParameterClauseInit :: OptTypedBindingTableInitializer}
  deriving (Eq, Ord, Read, Show)

_SessionSetBindingTableParameterClause = Core.Name "openGql.grammar.SessionSetBindingTableParameterClause"

_SessionSetBindingTableParameterClause_binding = Core.Name "binding"

_SessionSetBindingTableParameterClause_param = Core.Name "param"

_SessionSetBindingTableParameterClause_init = Core.Name "init"

data SessionSetValueParameterClause = 
  SessionSetValueParameterClause {
    sessionSetValueParameterClauseValue :: SessionSetParameterName,
    sessionSetValueParameterClauseInitializer :: OptTypedValueInitializer}
  deriving (Eq, Ord, Read, Show)

_SessionSetValueParameterClause = Core.Name "openGql.grammar.SessionSetValueParameterClause"

_SessionSetValueParameterClause_value = Core.Name "value"

_SessionSetValueParameterClause_initializer = Core.Name "initializer"

data SessionSetParameterName = 
  SessionSetParameterName {
    sessionSetParameterNameIfNotExists :: Bool,
    sessionSetParameterNameParameter :: SessionParameterSpecification}
  deriving (Eq, Ord, Read, Show)

_SessionSetParameterName = Core.Name "openGql.grammar.SessionSetParameterName"

_SessionSetParameterName_ifNotExists = Core.Name "ifNotExists"

_SessionSetParameterName_parameter = Core.Name "parameter"

type SessionResetCommand = (Maybe SessionResetArguments)

_SessionResetCommand = Core.Name "openGql.grammar.SessionResetCommand"

data SessionResetArguments = 
  SessionResetArgumentsParametersOrCharacteristics AllParametersOrCharacteristics |
  SessionResetArgumentsSchema  |
  SessionResetArgumentsGraph  |
  SessionResetArgumentsTimeZone  |
  SessionResetArgumentsParameterSessionSpecification ParameterSessionSpecification
  deriving (Eq, Ord, Read, Show)

_SessionResetArguments = Core.Name "openGql.grammar.SessionResetArguments"

_SessionResetArguments_parametersOrCharacteristics = Core.Name "parametersOrCharacteristics"

_SessionResetArguments_schema = Core.Name "schema"

_SessionResetArguments_graph = Core.Name "graph"

_SessionResetArguments_timeZone = Core.Name "timeZone"

_SessionResetArguments_parameterSessionSpecification = Core.Name "parameterSessionSpecification"

data AllParametersOrCharacteristics = 
  AllParametersOrCharacteristics {
    allParametersOrCharacteristicsAll :: Bool,
    allParametersOrCharacteristicsType :: ParametersOrCharacteristics}
  deriving (Eq, Ord, Read, Show)

_AllParametersOrCharacteristics = Core.Name "openGql.grammar.AllParametersOrCharacteristics"

_AllParametersOrCharacteristics_all = Core.Name "all"

_AllParametersOrCharacteristics_type = Core.Name "type"

data ParametersOrCharacteristics = 
  ParametersOrCharacteristicsParameters  |
  ParametersOrCharacteristicsCharacteristics 
  deriving (Eq, Ord, Read, Show)

_ParametersOrCharacteristics = Core.Name "openGql.grammar.ParametersOrCharacteristics"

_ParametersOrCharacteristics_parameters = Core.Name "parameters"

_ParametersOrCharacteristics_characteristics = Core.Name "characteristics"

data ParameterSessionSpecification = 
  ParameterSessionSpecification {
    parameterSessionSpecificationParameter :: Bool,
    parameterSessionSpecificationSessionParameterSpecification :: SessionParameterSpecification}
  deriving (Eq, Ord, Read, Show)

_ParameterSessionSpecification = Core.Name "openGql.grammar.ParameterSessionSpecification"

_ParameterSessionSpecification_parameter = Core.Name "parameter"

_ParameterSessionSpecification_sessionParameterSpecification = Core.Name "sessionParameterSpecification"

type SessionCloseCommand = ()

_SessionCloseCommand = Core.Name "openGql.grammar.SessionCloseCommand"

type SessionParameterSpecification = ParameterName

_SessionParameterSpecification = Core.Name "openGql.grammar.SessionParameterSpecification"

type StartTransactionCommand = (Maybe TransactionCharacteristics)

_StartTransactionCommand = Core.Name "openGql.grammar.StartTransactionCommand"

type TransactionCharacteristics = [TransactionMode]

_TransactionCharacteristics = Core.Name "openGql.grammar.TransactionCharacteristics"

type TransactionMode = TransactionAccessMode

_TransactionMode = Core.Name "openGql.grammar.TransactionMode"

data TransactionAccessMode = 
  TransactionAccessModeReadOnly  |
  TransactionAccessModeReadWrite 
  deriving (Eq, Ord, Read, Show)

_TransactionAccessMode = Core.Name "openGql.grammar.TransactionAccessMode"

_TransactionAccessMode_readOnly = Core.Name "readOnly"

_TransactionAccessMode_readWrite = Core.Name "readWrite"

type RollbackCommand = ()

_RollbackCommand = Core.Name "openGql.grammar.RollbackCommand"

type CommitCommand = ()

_CommitCommand = Core.Name "openGql.grammar.CommitCommand"

type NestedProcedureSpecification = ProcedureSpecification

_NestedProcedureSpecification = Core.Name "openGql.grammar.NestedProcedureSpecification"

type ProcedureSpecification = ProcedureBody

_ProcedureSpecification = Core.Name "openGql.grammar.ProcedureSpecification"

type NestedDataModifyingProcedureSpecification = ProcedureBody

_NestedDataModifyingProcedureSpecification = Core.Name "openGql.grammar.NestedDataModifyingProcedureSpecification"

type NestedQuerySpecification = ProcedureBody

_NestedQuerySpecification = Core.Name "openGql.grammar.NestedQuerySpecification"

data ProcedureBody = 
  ProcedureBody {
    procedureBodyAtSchema :: (Maybe AtSchemaClause),
    procedureBodyBindings :: (Maybe BindingVariableDefinitionBlock),
    procedureBodyStatements :: StatementBlock}
  deriving (Eq, Ord, Read, Show)

_ProcedureBody = Core.Name "openGql.grammar.ProcedureBody"

_ProcedureBody_atSchema = Core.Name "atSchema"

_ProcedureBody_bindings = Core.Name "bindings"

_ProcedureBody_statements = Core.Name "statements"

type BindingVariableDefinitionBlock = [BindingVariableDefinition]

_BindingVariableDefinitionBlock = Core.Name "openGql.grammar.BindingVariableDefinitionBlock"

data BindingVariableDefinition = 
  BindingVariableDefinitionGraph GraphVariableDefinition |
  BindingVariableDefinitionTable BindingTableVariableDefinition |
  BindingVariableDefinitionValue ValueVariableDefinition
  deriving (Eq, Ord, Read, Show)

_BindingVariableDefinition = Core.Name "openGql.grammar.BindingVariableDefinition"

_BindingVariableDefinition_graph = Core.Name "graph"

_BindingVariableDefinition_table = Core.Name "table"

_BindingVariableDefinition_value = Core.Name "value"

data StatementBlock = 
  StatementBlock {
    statementBlockStatement :: Statement,
    statementBlockNextStatements :: [NextStatement]}
  deriving (Eq, Ord, Read, Show)

_StatementBlock = Core.Name "openGql.grammar.StatementBlock"

_StatementBlock_statement = Core.Name "statement"

_StatementBlock_nextStatements = Core.Name "nextStatements"

data Statement = 
  StatementLinearCatalogModifying LinearCatalogModifyingStatement |
  StatementLinearDataModifying LinearDataModifyingStatement |
  StatementCompositeQuery CompositeQueryStatement
  deriving (Eq, Ord, Read, Show)

_Statement = Core.Name "openGql.grammar.Statement"

_Statement_linearCatalogModifying = Core.Name "linearCatalogModifying"

_Statement_linearDataModifying = Core.Name "linearDataModifying"

_Statement_compositeQuery = Core.Name "compositeQuery"

data NextStatement = 
  NextStatement {
    nextStatementYieldClause :: (Maybe YieldClause),
    nextStatementStatement :: Statement}
  deriving (Eq, Ord, Read, Show)

_NextStatement = Core.Name "openGql.grammar.NextStatement"

_NextStatement_yieldClause = Core.Name "yieldClause"

_NextStatement_statement = Core.Name "statement"

data GraphVariableDefinition = 
  GraphVariableDefinition {
    graphVariableDefinitionVariable :: BindingVariable,
    graphVariableDefinitionInitializer :: OptTypedGraphInitializer}
  deriving (Eq, Ord, Read, Show)

_GraphVariableDefinition = Core.Name "openGql.grammar.GraphVariableDefinition"

_GraphVariableDefinition_variable = Core.Name "variable"

_GraphVariableDefinition_initializer = Core.Name "initializer"

data OptTypedGraphInitializer = 
  OptTypedGraphInitializer {
    optTypedGraphInitializerType :: (Maybe TypedGraphReferenceValueType),
    optTypedGraphInitializerInitializer :: GraphInitializer}
  deriving (Eq, Ord, Read, Show)

_OptTypedGraphInitializer = Core.Name "openGql.grammar.OptTypedGraphInitializer"

_OptTypedGraphInitializer_type = Core.Name "type"

_OptTypedGraphInitializer_initializer = Core.Name "initializer"

data TypedGraphReferenceValueType = 
  TypedGraphReferenceValueType {
    typedGraphReferenceValueTypeTyped :: (Maybe Typed),
    typedGraphReferenceValueTypeValueType :: GraphReferenceValueType}
  deriving (Eq, Ord, Read, Show)

_TypedGraphReferenceValueType = Core.Name "openGql.grammar.TypedGraphReferenceValueType"

_TypedGraphReferenceValueType_typed = Core.Name "typed"

_TypedGraphReferenceValueType_valueType = Core.Name "valueType"

type GraphInitializer = ()

_GraphInitializer = Core.Name "openGql.grammar.GraphInitializer"

data BindingTableVariableDefinition = 
  BindingTableVariableDefinition {
    bindingTableVariableDefinitionBinding :: Bool,
    bindingTableVariableDefinitionVariable :: BindingVariable,
    bindingTableVariableDefinitionInitializer :: OptTypedBindingTableInitializer}
  deriving (Eq, Ord, Read, Show)

_BindingTableVariableDefinition = Core.Name "openGql.grammar.BindingTableVariableDefinition"

_BindingTableVariableDefinition_binding = Core.Name "binding"

_BindingTableVariableDefinition_variable = Core.Name "variable"

_BindingTableVariableDefinition_initializer = Core.Name "initializer"

data OptTypedBindingTableInitializer = 
  OptTypedBindingTableInitializer {
    optTypedBindingTableInitializerType :: (Maybe TypedBindingTableReferenceValueType),
    optTypedBindingTableInitializerInitializer :: BindingTableInitializer}
  deriving (Eq, Ord, Read, Show)

_OptTypedBindingTableInitializer = Core.Name "openGql.grammar.OptTypedBindingTableInitializer"

_OptTypedBindingTableInitializer_type = Core.Name "type"

_OptTypedBindingTableInitializer_initializer = Core.Name "initializer"

data TypedBindingTableReferenceValueType = 
  TypedBindingTableReferenceValueType {
    typedBindingTableReferenceValueTypeTyped :: (Maybe Typed),
    typedBindingTableReferenceValueTypeValueType :: BindingTableReferenceValueType}
  deriving (Eq, Ord, Read, Show)

_TypedBindingTableReferenceValueType = Core.Name "openGql.grammar.TypedBindingTableReferenceValueType"

_TypedBindingTableReferenceValueType_typed = Core.Name "typed"

_TypedBindingTableReferenceValueType_valueType = Core.Name "valueType"

type BindingTableInitializer = ()

_BindingTableInitializer = Core.Name "openGql.grammar.BindingTableInitializer"

data ValueVariableDefinition = 
  ValueVariableDefinition {
    valueVariableDefinitionVariable :: BindingVariable,
    valueVariableDefinitionInitializer :: OptTypedValueInitializer}
  deriving (Eq, Ord, Read, Show)

_ValueVariableDefinition = Core.Name "openGql.grammar.ValueVariableDefinition"

_ValueVariableDefinition_variable = Core.Name "variable"

_ValueVariableDefinition_initializer = Core.Name "initializer"

data OptTypedValueInitializer = 
  OptTypedValueInitializer {
    optTypedValueInitializerType :: (Maybe TypedValueType),
    optTypedValueInitializerInitializer :: ValueInitializer}
  deriving (Eq, Ord, Read, Show)

_OptTypedValueInitializer = Core.Name "openGql.grammar.OptTypedValueInitializer"

_OptTypedValueInitializer_type = Core.Name "type"

_OptTypedValueInitializer_initializer = Core.Name "initializer"

data TypedValueType = 
  TypedValueType {
    typedValueTypeTyped :: (Maybe Typed),
    typedValueTypeValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_TypedValueType = Core.Name "openGql.grammar.TypedValueType"

_TypedValueType_typed = Core.Name "typed"

_TypedValueType_valueType = Core.Name "valueType"

type ValueInitializer = ()

_ValueInitializer = Core.Name "openGql.grammar.ValueInitializer"

data GraphExpression = 
  GraphExpressionObject ObjectExpressionPrimary |
  GraphExpressionReference GraphReference |
  GraphExpressionName ObjectNameOrBindingVariable |
  GraphExpressionCurrent CurrentGraph
  deriving (Eq, Ord, Read, Show)

_GraphExpression = Core.Name "openGql.grammar.GraphExpression"

_GraphExpression_object = Core.Name "object"

_GraphExpression_reference = Core.Name "reference"

_GraphExpression_name = Core.Name "name"

_GraphExpression_current = Core.Name "current"

data CurrentGraph = 
  CurrentGraphGraph  |
  CurrentGraphPropertyGraph 
  deriving (Eq, Ord, Read, Show)

_CurrentGraph = Core.Name "openGql.grammar.CurrentGraph"

_CurrentGraph_graph = Core.Name "graph"

_CurrentGraph_propertyGraph = Core.Name "propertyGraph"

data BindingTableExpression = 
  BindingTableExpressionNested NestedBindingTableQuerySpecification |
  BindingTableExpressionObject ObjectExpressionPrimary |
  BindingTableExpressionTable BindingTableReference |
  BindingTableExpressionName ObjectNameOrBindingVariable
  deriving (Eq, Ord, Read, Show)

_BindingTableExpression = Core.Name "openGql.grammar.BindingTableExpression"

_BindingTableExpression_nested = Core.Name "nested"

_BindingTableExpression_object = Core.Name "object"

_BindingTableExpression_table = Core.Name "table"

_BindingTableExpression_name = Core.Name "name"

type NestedBindingTableQuerySpecification = ()

_NestedBindingTableQuerySpecification = Core.Name "openGql.grammar.NestedBindingTableQuerySpecification"

data ObjectExpressionPrimary = 
  ObjectExpressionPrimaryVariable PrimaryValueExpression |
  ObjectExpressionPrimaryParenthesized ParenthesizedValueExpression |
  ObjectExpressionPrimaryNonParenthesized NonParenthesizedPrimaryValueExpressionSpecialCase
  deriving (Eq, Ord, Read, Show)

_ObjectExpressionPrimary = Core.Name "openGql.grammar.ObjectExpressionPrimary"

_ObjectExpressionPrimary_variable = Core.Name "variable"

_ObjectExpressionPrimary_parenthesized = Core.Name "parenthesized"

_ObjectExpressionPrimary_nonParenthesized = Core.Name "nonParenthesized"

type LinearCatalogModifyingStatement = [SimpleCatalogModifyingStatement]

_LinearCatalogModifyingStatement = Core.Name "openGql.grammar.LinearCatalogModifyingStatement"

data SimpleCatalogModifyingStatement = 
  SimpleCatalogModifyingStatementPrimitive PrimitiveCatalogModifyingStatement |
  SimpleCatalogModifyingStatementCallProcedure CallCatalogModifyingProcedureStatement
  deriving (Eq, Ord, Read, Show)

_SimpleCatalogModifyingStatement = Core.Name "openGql.grammar.SimpleCatalogModifyingStatement"

_SimpleCatalogModifyingStatement_primitive = Core.Name "primitive"

_SimpleCatalogModifyingStatement_callProcedure = Core.Name "callProcedure"

data PrimitiveCatalogModifyingStatement = 
  PrimitiveCatalogModifyingStatementCreateSchema CreateSchemaStatement |
  PrimitiveCatalogModifyingStatementDropSchema DropSchemaStatement |
  PrimitiveCatalogModifyingStatementCreateGraph CreateGraphStatement |
  PrimitiveCatalogModifyingStatementDropGraph DropGraphStatement |
  PrimitiveCatalogModifyingStatementCreateGraphType CreateGraphTypeStatement |
  PrimitiveCatalogModifyingStatementDropGraphType DropGraphTypeStatement
  deriving (Eq, Ord, Read, Show)

_PrimitiveCatalogModifyingStatement = Core.Name "openGql.grammar.PrimitiveCatalogModifyingStatement"

_PrimitiveCatalogModifyingStatement_createSchema = Core.Name "createSchema"

_PrimitiveCatalogModifyingStatement_dropSchema = Core.Name "dropSchema"

_PrimitiveCatalogModifyingStatement_createGraph = Core.Name "createGraph"

_PrimitiveCatalogModifyingStatement_dropGraph = Core.Name "dropGraph"

_PrimitiveCatalogModifyingStatement_createGraphType = Core.Name "createGraphType"

_PrimitiveCatalogModifyingStatement_dropGraphType = Core.Name "dropGraphType"

data CreateSchemaStatement = 
  CreateSchemaStatement {
    createSchemaStatementIfNotExists :: Bool,
    createSchemaStatementParentAndName :: CatalogSchemaParentAndName}
  deriving (Eq, Ord, Read, Show)

_CreateSchemaStatement = Core.Name "openGql.grammar.CreateSchemaStatement"

_CreateSchemaStatement_ifNotExists = Core.Name "ifNotExists"

_CreateSchemaStatement_parentAndName = Core.Name "parentAndName"

data DropSchemaStatement = 
  DropSchemaStatement {
    dropSchemaStatementIfExists :: Bool,
    dropSchemaStatementParentAndName :: CatalogSchemaParentAndName}
  deriving (Eq, Ord, Read, Show)

_DropSchemaStatement = Core.Name "openGql.grammar.DropSchemaStatement"

_DropSchemaStatement_ifExists = Core.Name "ifExists"

_DropSchemaStatement_parentAndName = Core.Name "parentAndName"

data CreateGraphStatement = 
  CreateGraphStatement {
    createGraphStatementCreateOption :: CreateGraphOption,
    createGraphStatementParentAndName :: CatalogGraphParentAndName,
    createGraphStatementType :: GraphTypeOption,
    createGraphStatementSource :: (Maybe GraphSource)}
  deriving (Eq, Ord, Read, Show)

_CreateGraphStatement = Core.Name "openGql.grammar.CreateGraphStatement"

_CreateGraphStatement_createOption = Core.Name "createOption"

_CreateGraphStatement_parentAndName = Core.Name "parentAndName"

_CreateGraphStatement_type = Core.Name "type"

_CreateGraphStatement_source = Core.Name "source"

data CreateGraphOption = 
  CreateGraphOptionGraphIfNotExists Bool |
  CreateGraphOptionOrReplace 
  deriving (Eq, Ord, Read, Show)

_CreateGraphOption = Core.Name "openGql.grammar.CreateGraphOption"

_CreateGraphOption_graphIfNotExists = Core.Name "graphIfNotExists"

_CreateGraphOption_orReplace = Core.Name "orReplace"

data GraphTypeOption = 
  GraphTypeOptionOpenGraphType OpenGraphType |
  GraphTypeOptionOfGraphType OfGraphType
  deriving (Eq, Ord, Read, Show)

_GraphTypeOption = Core.Name "openGql.grammar.GraphTypeOption"

_GraphTypeOption_openGraphType = Core.Name "openGraphType"

_GraphTypeOption_ofGraphType = Core.Name "ofGraphType"

data OpenGraphType = 
  OpenGraphType {
    openGraphTypeTyped :: (Maybe Typed),
    openGraphTypeGraph :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenGraphType = Core.Name "openGql.grammar.OpenGraphType"

_OpenGraphType_typed = Core.Name "typed"

_OpenGraphType_graph = Core.Name "graph"

data OfGraphType = 
  OfGraphTypeLikeGraph GraphTypeLikeGraph |
  OfGraphTypeReference TypedGraphTypeReference |
  OfGraphTypeNested TypedNestedGraphTypeSpecification
  deriving (Eq, Ord, Read, Show)

_OfGraphType = Core.Name "openGql.grammar.OfGraphType"

_OfGraphType_likeGraph = Core.Name "likeGraph"

_OfGraphType_reference = Core.Name "reference"

_OfGraphType_nested = Core.Name "nested"

type GraphTypeLikeGraph = GraphExpression

_GraphTypeLikeGraph = Core.Name "openGql.grammar.GraphTypeLikeGraph"

type GraphSource = GraphExpression

_GraphSource = Core.Name "openGql.grammar.GraphSource"

data TypedGraphTypeReference = 
  TypedGraphTypeReference {
    typedGraphTypeReferenceTyped :: (Maybe Typed),
    typedGraphTypeReferenceReference :: GraphTypeReference}
  deriving (Eq, Ord, Read, Show)

_TypedGraphTypeReference = Core.Name "openGql.grammar.TypedGraphTypeReference"

_TypedGraphTypeReference_typed = Core.Name "typed"

_TypedGraphTypeReference_reference = Core.Name "reference"

data TypedNestedGraphTypeSpecification = 
  TypedNestedGraphTypeSpecification {
    typedNestedGraphTypeSpecificationTyped :: (Maybe Typed),
    typedNestedGraphTypeSpecificationGraph :: Bool,
    typedNestedGraphTypeSpecificationSpecification :: NestedGraphTypeSpecification}
  deriving (Eq, Ord, Read, Show)

_TypedNestedGraphTypeSpecification = Core.Name "openGql.grammar.TypedNestedGraphTypeSpecification"

_TypedNestedGraphTypeSpecification_typed = Core.Name "typed"

_TypedNestedGraphTypeSpecification_graph = Core.Name "graph"

_TypedNestedGraphTypeSpecification_specification = Core.Name "specification"

data DropGraphStatement = 
  DropGraphStatement {
    dropGraphStatementIfExists :: Bool,
    dropGraphStatementParentAndName :: CatalogGraphParentAndName}
  deriving (Eq, Ord, Read, Show)

_DropGraphStatement = Core.Name "openGql.grammar.DropGraphStatement"

_DropGraphStatement_ifExists = Core.Name "ifExists"

_DropGraphStatement_parentAndName = Core.Name "parentAndName"

data CreateGraphTypeStatement = 
  CreateGraphTypeStatement {
    createGraphTypeStatementCreateOption :: CreateGraphTypeOption,
    createGraphTypeStatementParentAndName :: CatalogGraphTypeParentAndName,
    createGraphTypeStatementSource :: GraphTypeSource}
  deriving (Eq, Ord, Read, Show)

_CreateGraphTypeStatement = Core.Name "openGql.grammar.CreateGraphTypeStatement"

_CreateGraphTypeStatement_createOption = Core.Name "createOption"

_CreateGraphTypeStatement_parentAndName = Core.Name "parentAndName"

_CreateGraphTypeStatement_source = Core.Name "source"

data CreateGraphTypeOption = 
  CreateGraphTypeOptionTypeIfNotExists Bool |
  CreateGraphTypeOptionOrReplace 
  deriving (Eq, Ord, Read, Show)

_CreateGraphTypeOption = Core.Name "openGql.grammar.CreateGraphTypeOption"

_CreateGraphTypeOption_typeIfNotExists = Core.Name "typeIfNotExists"

_CreateGraphTypeOption_orReplace = Core.Name "orReplace"

data GraphTypeSource = 
  GraphTypeSourceCopyOf CopyOfGraphType |
  GraphTypeSourceLikeGraph GraphTypeLikeGraph |
  GraphTypeSourceNestedSpecification NestedGraphTypeSpecification
  deriving (Eq, Ord, Read, Show)

_GraphTypeSource = Core.Name "openGql.grammar.GraphTypeSource"

_GraphTypeSource_copyOf = Core.Name "copyOf"

_GraphTypeSource_likeGraph = Core.Name "likeGraph"

_GraphTypeSource_nestedSpecification = Core.Name "nestedSpecification"

type CopyOfGraphType = GraphTypeReference

_CopyOfGraphType = Core.Name "openGql.grammar.CopyOfGraphType"

data DropGraphTypeStatement = 
  DropGraphTypeStatement {
    dropGraphTypeStatementIfExists :: Bool,
    dropGraphTypeStatementParentAndName :: CatalogGraphTypeParentAndName}
  deriving (Eq, Ord, Read, Show)

_DropGraphTypeStatement = Core.Name "openGql.grammar.DropGraphTypeStatement"

_DropGraphTypeStatement_ifExists = Core.Name "ifExists"

_DropGraphTypeStatement_parentAndName = Core.Name "parentAndName"

type CallCatalogModifyingProcedureStatement = CallProcedureStatement

_CallCatalogModifyingProcedureStatement = Core.Name "openGql.grammar.CallCatalogModifyingProcedureStatement"

data LinearDataModifyingStatement = 
  LinearDataModifyingStatementFocused FocusedLinearDataModifyingStatement |
  LinearDataModifyingStatementAmbient AmbientLinearDataModifyingStatement
  deriving (Eq, Ord, Read, Show)

_LinearDataModifyingStatement = Core.Name "openGql.grammar.LinearDataModifyingStatement"

_LinearDataModifyingStatement_focused = Core.Name "focused"

_LinearDataModifyingStatement_ambient = Core.Name "ambient"

data FocusedLinearDataModifyingStatement = 
  FocusedLinearDataModifyingStatementSimple FocusedLinearDataModifyingStatementBody |
  FocusedLinearDataModifyingStatementNested FocusedNestedDataModifyingProcedureSpecification
  deriving (Eq, Ord, Read, Show)

_FocusedLinearDataModifyingStatement = Core.Name "openGql.grammar.FocusedLinearDataModifyingStatement"

_FocusedLinearDataModifyingStatement_simple = Core.Name "simple"

_FocusedLinearDataModifyingStatement_nested = Core.Name "nested"

data FocusedLinearDataModifyingStatementBody = 
  FocusedLinearDataModifyingStatementBody {
    focusedLinearDataModifyingStatementBodyUseGraph :: UseGraphClause,
    focusedLinearDataModifyingStatementBodySimpleAccess :: SimpleLinearDataAccessingStatement,
    focusedLinearDataModifyingStatementBodyPrimitiveResult :: (Maybe PrimitiveResultStatement)}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearDataModifyingStatementBody = Core.Name "openGql.grammar.FocusedLinearDataModifyingStatementBody"

_FocusedLinearDataModifyingStatementBody_useGraph = Core.Name "useGraph"

_FocusedLinearDataModifyingStatementBody_simpleAccess = Core.Name "simpleAccess"

_FocusedLinearDataModifyingStatementBody_primitiveResult = Core.Name "primitiveResult"

data FocusedNestedDataModifyingProcedureSpecification = 
  FocusedNestedDataModifyingProcedureSpecification {
    focusedNestedDataModifyingProcedureSpecificationUseGraph :: UseGraphClause,
    focusedNestedDataModifyingProcedureSpecificationNestedSpec :: NestedDataModifyingProcedureSpecification}
  deriving (Eq, Ord, Read, Show)

_FocusedNestedDataModifyingProcedureSpecification =
    Core.Name "openGql.grammar.FocusedNestedDataModifyingProcedureSpecification"

_FocusedNestedDataModifyingProcedureSpecification_useGraph = Core.Name "useGraph"

_FocusedNestedDataModifyingProcedureSpecification_nestedSpec = Core.Name "nestedSpec"

data AmbientLinearDataModifyingStatement = 
  AmbientLinearDataModifyingStatementSimple AmbientLinearDataModifyingStatementBody |
  AmbientLinearDataModifyingStatementNested NestedDataModifyingProcedureSpecification
  deriving (Eq, Ord, Read, Show)

_AmbientLinearDataModifyingStatement = Core.Name "openGql.grammar.AmbientLinearDataModifyingStatement"

_AmbientLinearDataModifyingStatement_simple = Core.Name "simple"

_AmbientLinearDataModifyingStatement_nested = Core.Name "nested"

data AmbientLinearDataModifyingStatementBody = 
  AmbientLinearDataModifyingStatementBody {
    ambientLinearDataModifyingStatementBodySimpleAccess :: SimpleLinearDataAccessingStatement,
    ambientLinearDataModifyingStatementBodyPrimitiveResult :: (Maybe PrimitiveResultStatement)}
  deriving (Eq, Ord, Read, Show)

_AmbientLinearDataModifyingStatementBody = Core.Name "openGql.grammar.AmbientLinearDataModifyingStatementBody"

_AmbientLinearDataModifyingStatementBody_simpleAccess = Core.Name "simpleAccess"

_AmbientLinearDataModifyingStatementBody_primitiveResult = Core.Name "primitiveResult"

type SimpleLinearDataAccessingStatement = [SimpleDataAccessingStatement]

_SimpleLinearDataAccessingStatement = Core.Name "openGql.grammar.SimpleLinearDataAccessingStatement"

data SimpleDataAccessingStatement = 
  SimpleDataAccessingStatementQuery SimpleQueryStatement |
  SimpleDataAccessingStatementModifying SimpleDataModifyingStatement
  deriving (Eq, Ord, Read, Show)

_SimpleDataAccessingStatement = Core.Name "openGql.grammar.SimpleDataAccessingStatement"

_SimpleDataAccessingStatement_query = Core.Name "query"

_SimpleDataAccessingStatement_modifying = Core.Name "modifying"

data SimpleDataModifyingStatement = 
  SimpleDataModifyingStatementPrimitive PrimitiveDataModifyingStatement |
  SimpleDataModifyingStatementCallProcedure CallDataModifyingProcedureStatement
  deriving (Eq, Ord, Read, Show)

_SimpleDataModifyingStatement = Core.Name "openGql.grammar.SimpleDataModifyingStatement"

_SimpleDataModifyingStatement_primitive = Core.Name "primitive"

_SimpleDataModifyingStatement_callProcedure = Core.Name "callProcedure"

data PrimitiveDataModifyingStatement = 
  PrimitiveDataModifyingStatementInsert InsertStatement |
  PrimitiveDataModifyingStatementSet SetStatement |
  PrimitiveDataModifyingStatementRemove RemoveStatement |
  PrimitiveDataModifyingStatementDelete DeleteStatement
  deriving (Eq, Ord, Read, Show)

_PrimitiveDataModifyingStatement = Core.Name "openGql.grammar.PrimitiveDataModifyingStatement"

_PrimitiveDataModifyingStatement_insert = Core.Name "insert"

_PrimitiveDataModifyingStatement_set = Core.Name "set"

_PrimitiveDataModifyingStatement_remove = Core.Name "remove"

_PrimitiveDataModifyingStatement_delete = Core.Name "delete"

type InsertStatement = InsertGraphPattern

_InsertStatement = Core.Name "openGql.grammar.InsertStatement"

type SetStatement = SetItemList

_SetStatement = Core.Name "openGql.grammar.SetStatement"

type SetItemList = [SetItem]

_SetItemList = Core.Name "openGql.grammar.SetItemList"

data SetItem = 
  SetItemProperty SetPropertyItem |
  SetItemAllProperties SetAllPropertiesItem |
  SetItemLabel SetLabelItem
  deriving (Eq, Ord, Read, Show)

_SetItem = Core.Name "openGql.grammar.SetItem"

_SetItem_property = Core.Name "property"

_SetItem_allProperties = Core.Name "allProperties"

_SetItem_label = Core.Name "label"

data SetPropertyItem = 
  SetPropertyItem {
    setPropertyItemVariable :: BindingVariableReference,
    setPropertyItemPropertyName :: PropertyName,
    setPropertyItemValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_SetPropertyItem = Core.Name "openGql.grammar.SetPropertyItem"

_SetPropertyItem_variable = Core.Name "variable"

_SetPropertyItem_propertyName = Core.Name "propertyName"

_SetPropertyItem_value = Core.Name "value"

data SetAllPropertiesItem = 
  SetAllPropertiesItem {
    setAllPropertiesItemVariable :: BindingVariableReference,
    setAllPropertiesItemProperties :: (Maybe PropertyKeyValuePairList)}
  deriving (Eq, Ord, Read, Show)

_SetAllPropertiesItem = Core.Name "openGql.grammar.SetAllPropertiesItem"

_SetAllPropertiesItem_variable = Core.Name "variable"

_SetAllPropertiesItem_properties = Core.Name "properties"

data SetLabelItem = 
  SetLabelItem {
    setLabelItemVariable :: BindingVariableReference,
    setLabelItemIsOrColon :: IsOrColon,
    setLabelItemLabel :: LabelName}
  deriving (Eq, Ord, Read, Show)

_SetLabelItem = Core.Name "openGql.grammar.SetLabelItem"

_SetLabelItem_variable = Core.Name "variable"

_SetLabelItem_isOrColon = Core.Name "isOrColon"

_SetLabelItem_label = Core.Name "label"

type RemoveStatement = RemoveItemList

_RemoveStatement = Core.Name "openGql.grammar.RemoveStatement"

type RemoveItemList = [RemoveItem]

_RemoveItemList = Core.Name "openGql.grammar.RemoveItemList"

data RemoveItem = 
  RemoveItemProperty RemovePropertyItem |
  RemoveItemLabel RemoveLabelItem
  deriving (Eq, Ord, Read, Show)

_RemoveItem = Core.Name "openGql.grammar.RemoveItem"

_RemoveItem_property = Core.Name "property"

_RemoveItem_label = Core.Name "label"

data RemovePropertyItem = 
  RemovePropertyItem {
    removePropertyItemVariable :: BindingVariableReference,
    removePropertyItemPropertyName :: PropertyName}
  deriving (Eq, Ord, Read, Show)

_RemovePropertyItem = Core.Name "openGql.grammar.RemovePropertyItem"

_RemovePropertyItem_variable = Core.Name "variable"

_RemovePropertyItem_propertyName = Core.Name "propertyName"

data RemoveLabelItem = 
  RemoveLabelItem {
    removeLabelItemVariable :: BindingVariableReference,
    removeLabelItemIsOrColon :: IsOrColon,
    removeLabelItemLabel :: LabelName}
  deriving (Eq, Ord, Read, Show)

_RemoveLabelItem = Core.Name "openGql.grammar.RemoveLabelItem"

_RemoveLabelItem_variable = Core.Name "variable"

_RemoveLabelItem_isOrColon = Core.Name "isOrColon"

_RemoveLabelItem_label = Core.Name "label"

data DeleteStatement = 
  DeleteStatement {
    deleteStatementDetach :: (Maybe DetachOption),
    deleteStatementItems :: DeleteItemList}
  deriving (Eq, Ord, Read, Show)

_DeleteStatement = Core.Name "openGql.grammar.DeleteStatement"

_DeleteStatement_detach = Core.Name "detach"

_DeleteStatement_items = Core.Name "items"

data DetachOption = 
  DetachOptionDetach  |
  DetachOptionNoDetach 
  deriving (Eq, Ord, Read, Show)

_DetachOption = Core.Name "openGql.grammar.DetachOption"

_DetachOption_detach = Core.Name "detach"

_DetachOption_noDetach = Core.Name "noDetach"

type DeleteItemList = [DeleteItem]

_DeleteItemList = Core.Name "openGql.grammar.DeleteItemList"

type DeleteItem = ValueExpression

_DeleteItem = Core.Name "openGql.grammar.DeleteItem"

type CallDataModifyingProcedureStatement = CallProcedureStatement

_CallDataModifyingProcedureStatement = Core.Name "openGql.grammar.CallDataModifyingProcedureStatement"

type CompositeQueryStatement = CompositeQueryExpression

_CompositeQueryStatement = Core.Name "openGql.grammar.CompositeQueryStatement"

data CompositeQueryExpression = 
  CompositeQueryExpressionSimple CompositeQueryExpressionConjunction |
  CompositeQueryExpressionPrimary CompositeQueryPrimary
  deriving (Eq, Ord, Read, Show)

_CompositeQueryExpression = Core.Name "openGql.grammar.CompositeQueryExpression"

_CompositeQueryExpression_simple = Core.Name "simple"

_CompositeQueryExpression_primary = Core.Name "primary"

data CompositeQueryExpressionConjunction = 
  CompositeQueryExpressionConjunction {
    compositeQueryExpressionConjunctionLeft :: CompositeQueryExpression,
    compositeQueryExpressionConjunctionConjunction :: QueryConjunction,
    compositeQueryExpressionConjunctionRight :: CompositeQueryPrimary}
  deriving (Eq, Ord, Read, Show)

_CompositeQueryExpressionConjunction = Core.Name "openGql.grammar.CompositeQueryExpressionConjunction"

_CompositeQueryExpressionConjunction_left = Core.Name "left"

_CompositeQueryExpressionConjunction_conjunction = Core.Name "conjunction"

_CompositeQueryExpressionConjunction_right = Core.Name "right"

data QueryConjunction = 
  QueryConjunctionSetOperator SetOperator |
  QueryConjunctionOtherwise 
  deriving (Eq, Ord, Read, Show)

_QueryConjunction = Core.Name "openGql.grammar.QueryConjunction"

_QueryConjunction_setOperator = Core.Name "setOperator"

_QueryConjunction_otherwise = Core.Name "otherwise"

data SetOperator = 
  SetOperator {
    setOperatorOperatorType :: SetOperatorType,
    setOperatorQuantifier :: (Maybe SetQuantifier)}
  deriving (Eq, Ord, Read, Show)

_SetOperator = Core.Name "openGql.grammar.SetOperator"

_SetOperator_operatorType = Core.Name "operatorType"

_SetOperator_quantifier = Core.Name "quantifier"

data SetOperatorType = 
  SetOperatorTypeUnion  |
  SetOperatorTypeExcept  |
  SetOperatorTypeIntersect 
  deriving (Eq, Ord, Read, Show)

_SetOperatorType = Core.Name "openGql.grammar.SetOperatorType"

_SetOperatorType_union = Core.Name "union"

_SetOperatorType_except = Core.Name "except"

_SetOperatorType_intersect = Core.Name "intersect"

type CompositeQueryPrimary = LinearQueryStatement

_CompositeQueryPrimary = Core.Name "openGql.grammar.CompositeQueryPrimary"

data LinearQueryStatement = 
  LinearQueryStatementFocused FocusedLinearQueryStatement |
  LinearQueryStatementAmbient AmbientLinearQueryStatement
  deriving (Eq, Ord, Read, Show)

_LinearQueryStatement = Core.Name "openGql.grammar.LinearQueryStatement"

_LinearQueryStatement_focused = Core.Name "focused"

_LinearQueryStatement_ambient = Core.Name "ambient"

data FocusedLinearQueryStatement = 
  FocusedLinearQueryStatementParts FocusedLinearQueryStatementPartsAndResult |
  FocusedLinearQueryStatementPrimitive FocusedPrimitiveResultStatement |
  FocusedLinearQueryStatementNested FocusedNestedQuerySpecification |
  FocusedLinearQueryStatementSelect SelectStatement
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryStatement = Core.Name "openGql.grammar.FocusedLinearQueryStatement"

_FocusedLinearQueryStatement_parts = Core.Name "parts"

_FocusedLinearQueryStatement_primitive = Core.Name "primitive"

_FocusedLinearQueryStatement_nested = Core.Name "nested"

_FocusedLinearQueryStatement_select = Core.Name "select"

data FocusedLinearQueryStatementPartsAndResult = 
  FocusedLinearQueryStatementPartsAndResult {
    focusedLinearQueryStatementPartsAndResultParts :: [FocusedLinearQueryStatementPart],
    focusedLinearQueryStatementPartsAndResultResult :: FocusedLinearQueryAndPrimitiveResultStatementPart}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryStatementPartsAndResult = Core.Name "openGql.grammar.FocusedLinearQueryStatementPartsAndResult"

_FocusedLinearQueryStatementPartsAndResult_parts = Core.Name "parts"

_FocusedLinearQueryStatementPartsAndResult_result = Core.Name "result"

data FocusedLinearQueryStatementPart = 
  FocusedLinearQueryStatementPart {
    focusedLinearQueryStatementPartUseGraph :: UseGraphClause,
    focusedLinearQueryStatementPartSimple :: SimpleLinearQueryStatement}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryStatementPart = Core.Name "openGql.grammar.FocusedLinearQueryStatementPart"

_FocusedLinearQueryStatementPart_useGraph = Core.Name "useGraph"

_FocusedLinearQueryStatementPart_simple = Core.Name "simple"

data FocusedLinearQueryAndPrimitiveResultStatementPart = 
  FocusedLinearQueryAndPrimitiveResultStatementPart {
    focusedLinearQueryAndPrimitiveResultStatementPartUseGraph :: UseGraphClause,
    focusedLinearQueryAndPrimitiveResultStatementPartSimple :: SimpleLinearQueryStatement,
    focusedLinearQueryAndPrimitiveResultStatementPartPrimitiveResult :: PrimitiveResultStatement}
  deriving (Eq, Ord, Read, Show)

_FocusedLinearQueryAndPrimitiveResultStatementPart =
    Core.Name "openGql.grammar.FocusedLinearQueryAndPrimitiveResultStatementPart"

_FocusedLinearQueryAndPrimitiveResultStatementPart_useGraph = Core.Name "useGraph"

_FocusedLinearQueryAndPrimitiveResultStatementPart_simple = Core.Name "simple"

_FocusedLinearQueryAndPrimitiveResultStatementPart_primitiveResult = Core.Name "primitiveResult"

data FocusedPrimitiveResultStatement = 
  FocusedPrimitiveResultStatement {
    focusedPrimitiveResultStatementUseGraph :: UseGraphClause,
    focusedPrimitiveResultStatementPrimitiveResult :: PrimitiveResultStatement}
  deriving (Eq, Ord, Read, Show)

_FocusedPrimitiveResultStatement = Core.Name "openGql.grammar.FocusedPrimitiveResultStatement"

_FocusedPrimitiveResultStatement_useGraph = Core.Name "useGraph"

_FocusedPrimitiveResultStatement_primitiveResult = Core.Name "primitiveResult"

data FocusedNestedQuerySpecification = 
  FocusedNestedQuerySpecification {
    focusedNestedQuerySpecificationUseGraph :: UseGraphClause,
    focusedNestedQuerySpecificationNested :: NestedQuerySpecification}
  deriving (Eq, Ord, Read, Show)

_FocusedNestedQuerySpecification = Core.Name "openGql.grammar.FocusedNestedQuerySpecification"

_FocusedNestedQuerySpecification_useGraph = Core.Name "useGraph"

_FocusedNestedQuerySpecification_nested = Core.Name "nested"

data AmbientLinearQueryStatement = 
  AmbientLinearQueryStatementSimple AmbientLinearQueryStatementSimpleAndPrimitiveResult |
  AmbientLinearQueryStatementNested NestedQuerySpecification
  deriving (Eq, Ord, Read, Show)

_AmbientLinearQueryStatement = Core.Name "openGql.grammar.AmbientLinearQueryStatement"

_AmbientLinearQueryStatement_simple = Core.Name "simple"

_AmbientLinearQueryStatement_nested = Core.Name "nested"

data AmbientLinearQueryStatementSimpleAndPrimitiveResult = 
  AmbientLinearQueryStatementSimpleAndPrimitiveResult {
    ambientLinearQueryStatementSimpleAndPrimitiveResultSimple :: (Maybe SimpleLinearQueryStatement),
    ambientLinearQueryStatementSimpleAndPrimitiveResultPrimitiveResult :: PrimitiveResultStatement}
  deriving (Eq, Ord, Read, Show)

_AmbientLinearQueryStatementSimpleAndPrimitiveResult =
    Core.Name "openGql.grammar.AmbientLinearQueryStatementSimpleAndPrimitiveResult"

_AmbientLinearQueryStatementSimpleAndPrimitiveResult_simple = Core.Name "simple"

_AmbientLinearQueryStatementSimpleAndPrimitiveResult_primitiveResult = Core.Name "primitiveResult"

type SimpleLinearQueryStatement = [SimpleQueryStatement]

_SimpleLinearQueryStatement = Core.Name "openGql.grammar.SimpleLinearQueryStatement"

data SimpleQueryStatement = 
  SimpleQueryStatementPrimitive PrimitiveQueryStatement |
  SimpleQueryStatementCall CallQueryStatement
  deriving (Eq, Ord, Read, Show)

_SimpleQueryStatement = Core.Name "openGql.grammar.SimpleQueryStatement"

_SimpleQueryStatement_primitive = Core.Name "primitive"

_SimpleQueryStatement_call = Core.Name "call"

data PrimitiveQueryStatement = 
  PrimitiveQueryStatementMatch MatchStatement |
  PrimitiveQueryStatementLet LetStatement |
  PrimitiveQueryStatementFor ForStatement |
  PrimitiveQueryStatementFilter FilterStatement |
  PrimitiveQueryStatementOrderByAndPage OrderByAndPageStatement
  deriving (Eq, Ord, Read, Show)

_PrimitiveQueryStatement = Core.Name "openGql.grammar.PrimitiveQueryStatement"

_PrimitiveQueryStatement_match = Core.Name "match"

_PrimitiveQueryStatement_let = Core.Name "let"

_PrimitiveQueryStatement_for = Core.Name "for"

_PrimitiveQueryStatement_filter = Core.Name "filter"

_PrimitiveQueryStatement_orderByAndPage = Core.Name "orderByAndPage"

data MatchStatement = 
  MatchStatementSimple SimpleMatchStatement |
  MatchStatementOptional OptionalMatchStatement
  deriving (Eq, Ord, Read, Show)

_MatchStatement = Core.Name "openGql.grammar.MatchStatement"

_MatchStatement_simple = Core.Name "simple"

_MatchStatement_optional = Core.Name "optional"

type SimpleMatchStatement = GraphPatternBindingTable

_SimpleMatchStatement = Core.Name "openGql.grammar.SimpleMatchStatement"

type OptionalMatchStatement = OptionalOperand

_OptionalMatchStatement = Core.Name "openGql.grammar.OptionalMatchStatement"

data OptionalOperand = 
  OptionalOperandSimple SimpleMatchStatement |
  OptionalOperandBraceBlock MatchStatementBlock |
  OptionalOperandParenBlock MatchStatementBlock
  deriving (Eq, Ord, Read, Show)

_OptionalOperand = Core.Name "openGql.grammar.OptionalOperand"

_OptionalOperand_simple = Core.Name "simple"

_OptionalOperand_braceBlock = Core.Name "braceBlock"

_OptionalOperand_parenBlock = Core.Name "parenBlock"

type MatchStatementBlock = [MatchStatement]

_MatchStatementBlock = Core.Name "openGql.grammar.MatchStatementBlock"

type CallQueryStatement = CallProcedureStatement

_CallQueryStatement = Core.Name "openGql.grammar.CallQueryStatement"

data FilterStatement = 
  FilterStatementWhereClause WhereClause |
  FilterStatementSearchCondition SearchCondition
  deriving (Eq, Ord, Read, Show)

_FilterStatement = Core.Name "openGql.grammar.FilterStatement"

_FilterStatement_whereClause = Core.Name "whereClause"

_FilterStatement_searchCondition = Core.Name "searchCondition"

type LetStatement = LetVariableDefinitionList

_LetStatement = Core.Name "openGql.grammar.LetStatement"

type LetVariableDefinitionList = [LetVariableDefinition]

_LetVariableDefinitionList = Core.Name "openGql.grammar.LetVariableDefinitionList"

data LetVariableDefinition = 
  LetVariableDefinitionValueVariable ValueVariableDefinition |
  LetVariableDefinitionBindingEqualsValue BindingEqualsValue
  deriving (Eq, Ord, Read, Show)

_LetVariableDefinition = Core.Name "openGql.grammar.LetVariableDefinition"

_LetVariableDefinition_valueVariable = Core.Name "valueVariable"

_LetVariableDefinition_bindingEqualsValue = Core.Name "bindingEqualsValue"

data BindingEqualsValue = 
  BindingEqualsValue {
    bindingEqualsValueBinding :: BindingVariable,
    bindingEqualsValueValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_BindingEqualsValue = Core.Name "openGql.grammar.BindingEqualsValue"

_BindingEqualsValue_binding = Core.Name "binding"

_BindingEqualsValue_value = Core.Name "value"

data ForStatement = 
  ForStatement {
    forStatementItem :: ForItem,
    forStatementOrdinalityOrOffset :: (Maybe ForOrdinalityOrOffset)}
  deriving (Eq, Ord, Read, Show)

_ForStatement = Core.Name "openGql.grammar.ForStatement"

_ForStatement_item = Core.Name "item"

_ForStatement_ordinalityOrOffset = Core.Name "ordinalityOrOffset"

data ForItem = 
  ForItem {
    forItemAlias :: ForItemAlias,
    forItemSource :: ForItemSource}
  deriving (Eq, Ord, Read, Show)

_ForItem = Core.Name "openGql.grammar.ForItem"

_ForItem_alias = Core.Name "alias"

_ForItem_source = Core.Name "source"

type ForItemAlias = BindingVariable

_ForItemAlias = Core.Name "openGql.grammar.ForItemAlias"

type ForItemSource = ValueExpression

_ForItemSource = Core.Name "openGql.grammar.ForItemSource"

data ForOrdinalityOrOffset = 
  ForOrdinalityOrOffset {
    forOrdinalityOrOffsetType :: OrdinalityOrOffsetType,
    forOrdinalityOrOffsetVariable :: BindingVariable}
  deriving (Eq, Ord, Read, Show)

_ForOrdinalityOrOffset = Core.Name "openGql.grammar.ForOrdinalityOrOffset"

_ForOrdinalityOrOffset_type = Core.Name "type"

_ForOrdinalityOrOffset_variable = Core.Name "variable"

data OrdinalityOrOffsetType = 
  OrdinalityOrOffsetTypeOrdinality  |
  OrdinalityOrOffsetTypeOffset 
  deriving (Eq, Ord, Read, Show)

_OrdinalityOrOffsetType = Core.Name "openGql.grammar.OrdinalityOrOffsetType"

_OrdinalityOrOffsetType_ordinality = Core.Name "ordinality"

_OrdinalityOrOffsetType_offset = Core.Name "offset"

data OrderByAndPageStatement = 
  OrderByAndPageStatementOrderByAndOptionalOffsetAndLimit OrderByAndOptionalOffsetAndLimit |
  OrderByAndPageStatementOffsetAndOptionalLimit OffsetAndOptionalLimit |
  OrderByAndPageStatementLimitOnly LimitClause
  deriving (Eq, Ord, Read, Show)

_OrderByAndPageStatement = Core.Name "openGql.grammar.OrderByAndPageStatement"

_OrderByAndPageStatement_orderByAndOptionalOffsetAndLimit = Core.Name "orderByAndOptionalOffsetAndLimit"

_OrderByAndPageStatement_offsetAndOptionalLimit = Core.Name "offsetAndOptionalLimit"

_OrderByAndPageStatement_limitOnly = Core.Name "limitOnly"

data OrderByAndOptionalOffsetAndLimit = 
  OrderByAndOptionalOffsetAndLimit {
    orderByAndOptionalOffsetAndLimitOrderBy :: OrderByClause,
    orderByAndOptionalOffsetAndLimitOffset :: (Maybe OffsetClause),
    orderByAndOptionalOffsetAndLimitLimit :: (Maybe LimitClause)}
  deriving (Eq, Ord, Read, Show)

_OrderByAndOptionalOffsetAndLimit = Core.Name "openGql.grammar.OrderByAndOptionalOffsetAndLimit"

_OrderByAndOptionalOffsetAndLimit_orderBy = Core.Name "orderBy"

_OrderByAndOptionalOffsetAndLimit_offset = Core.Name "offset"

_OrderByAndOptionalOffsetAndLimit_limit = Core.Name "limit"

data OffsetAndOptionalLimit = 
  OffsetAndOptionalLimit {
    offsetAndOptionalLimitOffset :: OffsetClause,
    offsetAndOptionalLimitLimit :: (Maybe LimitClause)}
  deriving (Eq, Ord, Read, Show)

_OffsetAndOptionalLimit = Core.Name "openGql.grammar.OffsetAndOptionalLimit"

_OffsetAndOptionalLimit_offset = Core.Name "offset"

_OffsetAndOptionalLimit_limit = Core.Name "limit"

data PrimitiveResultStatement = 
  PrimitiveResultStatementReturnAndOptionalOrderBy ReturnAndOptionalOrderByAndPage |
  PrimitiveResultStatementFinish 
  deriving (Eq, Ord, Read, Show)

_PrimitiveResultStatement = Core.Name "openGql.grammar.PrimitiveResultStatement"

_PrimitiveResultStatement_returnAndOptionalOrderBy = Core.Name "returnAndOptionalOrderBy"

_PrimitiveResultStatement_finish = Core.Name "finish"

data ReturnAndOptionalOrderByAndPage = 
  ReturnAndOptionalOrderByAndPage {
    returnAndOptionalOrderByAndPageReturn :: ReturnStatement,
    returnAndOptionalOrderByAndPageOrderByAndPage :: (Maybe OrderByAndPageStatement)}
  deriving (Eq, Ord, Read, Show)

_ReturnAndOptionalOrderByAndPage = Core.Name "openGql.grammar.ReturnAndOptionalOrderByAndPage"

_ReturnAndOptionalOrderByAndPage_return = Core.Name "return"

_ReturnAndOptionalOrderByAndPage_orderByAndPage = Core.Name "orderByAndPage"

type ReturnStatement = ReturnStatementBody

_ReturnStatement = Core.Name "openGql.grammar.ReturnStatement"

data ReturnStatementBody = 
  ReturnStatementBodyItems ReturnItemsAndGroupBy |
  ReturnStatementBodyNoBindings 
  deriving (Eq, Ord, Read, Show)

_ReturnStatementBody = Core.Name "openGql.grammar.ReturnStatementBody"

_ReturnStatementBody_items = Core.Name "items"

_ReturnStatementBody_noBindings = Core.Name "noBindings"

data ReturnItemsAndGroupBy = 
  ReturnItemsAndGroupBy {
    returnItemsAndGroupByQuantifier :: (Maybe SetQuantifier),
    returnItemsAndGroupByItems :: ReturnItems,
    returnItemsAndGroupByGroupBy :: (Maybe GroupByClause)}
  deriving (Eq, Ord, Read, Show)

_ReturnItemsAndGroupBy = Core.Name "openGql.grammar.ReturnItemsAndGroupBy"

_ReturnItemsAndGroupBy_quantifier = Core.Name "quantifier"

_ReturnItemsAndGroupBy_items = Core.Name "items"

_ReturnItemsAndGroupBy_groupBy = Core.Name "groupBy"

data ReturnItems = 
  ReturnItemsAsterisk  |
  ReturnItemsItemList ReturnItemList
  deriving (Eq, Ord, Read, Show)

_ReturnItems = Core.Name "openGql.grammar.ReturnItems"

_ReturnItems_asterisk = Core.Name "asterisk"

_ReturnItems_itemList = Core.Name "itemList"

type ReturnItemList = [ReturnItem]

_ReturnItemList = Core.Name "openGql.grammar.ReturnItemList"

data ReturnItem = 
  ReturnItem {
    returnItemExpression :: AggregatingValueExpression,
    returnItemAlias :: (Maybe ReturnItemAlias)}
  deriving (Eq, Ord, Read, Show)

_ReturnItem = Core.Name "openGql.grammar.ReturnItem"

_ReturnItem_expression = Core.Name "expression"

_ReturnItem_alias = Core.Name "alias"

type ReturnItemAlias = String

_ReturnItemAlias = Core.Name "openGql.grammar.ReturnItemAlias"

data SelectStatement = 
  SelectStatement {
    selectStatementQuantifier :: (Maybe SetQuantifier),
    selectStatementItems :: SelectItems,
    selectStatementBody :: (Maybe SelectStatementBodyAndClauses)}
  deriving (Eq, Ord, Read, Show)

_SelectStatement = Core.Name "openGql.grammar.SelectStatement"

_SelectStatement_quantifier = Core.Name "quantifier"

_SelectStatement_items = Core.Name "items"

_SelectStatement_body = Core.Name "body"

data SelectItems = 
  SelectItemsAsterisk  |
  SelectItemsItemList SelectItemList
  deriving (Eq, Ord, Read, Show)

_SelectItems = Core.Name "openGql.grammar.SelectItems"

_SelectItems_asterisk = Core.Name "asterisk"

_SelectItems_itemList = Core.Name "itemList"

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

_SelectStatementBodyAndClauses = Core.Name "openGql.grammar.SelectStatementBodyAndClauses"

_SelectStatementBodyAndClauses_body = Core.Name "body"

_SelectStatementBodyAndClauses_where = Core.Name "where"

_SelectStatementBodyAndClauses_groupBy = Core.Name "groupBy"

_SelectStatementBodyAndClauses_having = Core.Name "having"

_SelectStatementBodyAndClauses_orderBy = Core.Name "orderBy"

_SelectStatementBodyAndClauses_offset = Core.Name "offset"

_SelectStatementBodyAndClauses_limit = Core.Name "limit"

type SelectItemList = [SelectItem]

_SelectItemList = Core.Name "openGql.grammar.SelectItemList"

data SelectItem = 
  SelectItem {
    selectItemExpression :: AggregatingValueExpression,
    selectItemAlias :: (Maybe SelectItemAlias)}
  deriving (Eq, Ord, Read, Show)

_SelectItem = Core.Name "openGql.grammar.SelectItem"

_SelectItem_expression = Core.Name "expression"

_SelectItem_alias = Core.Name "alias"

type SelectItemAlias = String

_SelectItemAlias = Core.Name "openGql.grammar.SelectItemAlias"

type HavingClause = SearchCondition

_HavingClause = Core.Name "openGql.grammar.HavingClause"

data SelectStatementBody = 
  SelectStatementBodyGraphMatchList SelectGraphMatchList |
  SelectStatementBodyQuerySpecification SelectQuerySpecification
  deriving (Eq, Ord, Read, Show)

_SelectStatementBody = Core.Name "openGql.grammar.SelectStatementBody"

_SelectStatementBody_graphMatchList = Core.Name "graphMatchList"

_SelectStatementBody_querySpecification = Core.Name "querySpecification"

type SelectGraphMatchList = [SelectGraphMatch]

_SelectGraphMatchList = Core.Name "openGql.grammar.SelectGraphMatchList"

data SelectGraphMatch = 
  SelectGraphMatch {
    selectGraphMatchGraphExpression :: GraphExpression,
    selectGraphMatchMatchStatement :: MatchStatement}
  deriving (Eq, Ord, Read, Show)

_SelectGraphMatch = Core.Name "openGql.grammar.SelectGraphMatch"

_SelectGraphMatch_graphExpression = Core.Name "graphExpression"

_SelectGraphMatch_matchStatement = Core.Name "matchStatement"

data SelectQuerySpecification = 
  SelectQuerySpecificationNested NestedQuerySpecification |
  SelectQuerySpecificationGraphAndNested GraphAndNestedQuerySpecification
  deriving (Eq, Ord, Read, Show)

_SelectQuerySpecification = Core.Name "openGql.grammar.SelectQuerySpecification"

_SelectQuerySpecification_nested = Core.Name "nested"

_SelectQuerySpecification_graphAndNested = Core.Name "graphAndNested"

data GraphAndNestedQuerySpecification = 
  GraphAndNestedQuerySpecification {
    graphAndNestedQuerySpecificationGraphExpression :: GraphExpression,
    graphAndNestedQuerySpecificationNested :: NestedQuerySpecification}
  deriving (Eq, Ord, Read, Show)

_GraphAndNestedQuerySpecification = Core.Name "openGql.grammar.GraphAndNestedQuerySpecification"

_GraphAndNestedQuerySpecification_graphExpression = Core.Name "graphExpression"

_GraphAndNestedQuerySpecification_nested = Core.Name "nested"

data CallProcedureStatement = 
  CallProcedureStatement {
    callProcedureStatementOptional :: Bool,
    callProcedureStatementCall :: ProcedureCall}
  deriving (Eq, Ord, Read, Show)

_CallProcedureStatement = Core.Name "openGql.grammar.CallProcedureStatement"

_CallProcedureStatement_optional = Core.Name "optional"

_CallProcedureStatement_call = Core.Name "call"

data ProcedureCall = 
  ProcedureCallInline InlineProcedureCall |
  ProcedureCallNamed NamedProcedureCall
  deriving (Eq, Ord, Read, Show)

_ProcedureCall = Core.Name "openGql.grammar.ProcedureCall"

_ProcedureCall_inline = Core.Name "inline"

_ProcedureCall_named = Core.Name "named"

data InlineProcedureCall = 
  InlineProcedureCall {
    inlineProcedureCallScope :: (Maybe VariableScopeClause),
    inlineProcedureCallNested :: NestedProcedureSpecification}
  deriving (Eq, Ord, Read, Show)

_InlineProcedureCall = Core.Name "openGql.grammar.InlineProcedureCall"

_InlineProcedureCall_scope = Core.Name "scope"

_InlineProcedureCall_nested = Core.Name "nested"

type VariableScopeClause = (Maybe BindingVariableReferenceList)

_VariableScopeClause = Core.Name "openGql.grammar.VariableScopeClause"

type BindingVariableReferenceList = [BindingVariableReference]

_BindingVariableReferenceList = Core.Name "openGql.grammar.BindingVariableReferenceList"

data NamedProcedureCall = 
  NamedProcedureCall {
    namedProcedureCallReference :: ProcedureReference,
    namedProcedureCallArguments :: (Maybe ProcedureArgumentList),
    namedProcedureCallYield :: (Maybe YieldClause)}
  deriving (Eq, Ord, Read, Show)

_NamedProcedureCall = Core.Name "openGql.grammar.NamedProcedureCall"

_NamedProcedureCall_reference = Core.Name "reference"

_NamedProcedureCall_arguments = Core.Name "arguments"

_NamedProcedureCall_yield = Core.Name "yield"

type ProcedureArgumentList = [ProcedureArgument]

_ProcedureArgumentList = Core.Name "openGql.grammar.ProcedureArgumentList"

type ProcedureArgument = ValueExpression

_ProcedureArgument = Core.Name "openGql.grammar.ProcedureArgument"

type AtSchemaClause = SchemaReference

_AtSchemaClause = Core.Name "openGql.grammar.AtSchemaClause"

type UseGraphClause = GraphExpression

_UseGraphClause = Core.Name "openGql.grammar.UseGraphClause"

data GraphPatternBindingTable = 
  GraphPatternBindingTable {
    graphPatternBindingTablePattern :: GraphPattern,
    graphPatternBindingTableYieldClause :: (Maybe GraphPatternYieldClause)}
  deriving (Eq, Ord, Read, Show)

_GraphPatternBindingTable = Core.Name "openGql.grammar.GraphPatternBindingTable"

_GraphPatternBindingTable_pattern = Core.Name "pattern"

_GraphPatternBindingTable_yieldClause = Core.Name "yieldClause"

type GraphPatternYieldClause = GraphPatternYieldItemList

_GraphPatternYieldClause = Core.Name "openGql.grammar.GraphPatternYieldClause"

data GraphPatternYieldItemList = 
  GraphPatternYieldItemListItems [GraphPatternYieldItem] |
  GraphPatternYieldItemListNoBindings 
  deriving (Eq, Ord, Read, Show)

_GraphPatternYieldItemList = Core.Name "openGql.grammar.GraphPatternYieldItemList"

_GraphPatternYieldItemList_items = Core.Name "items"

_GraphPatternYieldItemList_noBindings = Core.Name "noBindings"

type GraphPatternYieldItem = BindingVariableReference

_GraphPatternYieldItem = Core.Name "openGql.grammar.GraphPatternYieldItem"

data GraphPattern = 
  GraphPattern {
    graphPatternMatchMode :: (Maybe MatchMode),
    graphPatternPathPatterns :: PathPatternList,
    graphPatternKeepClause :: (Maybe KeepClause),
    graphPatternWhereClause :: (Maybe GraphPatternWhereClause)}
  deriving (Eq, Ord, Read, Show)

_GraphPattern = Core.Name "openGql.grammar.GraphPattern"

_GraphPattern_matchMode = Core.Name "matchMode"

_GraphPattern_pathPatterns = Core.Name "pathPatterns"

_GraphPattern_keepClause = Core.Name "keepClause"

_GraphPattern_whereClause = Core.Name "whereClause"

data MatchMode = 
  MatchModeRepeatableElements RepeatableElementsMatchMode |
  MatchModeDifferentEdges DifferentEdgesMatchMode
  deriving (Eq, Ord, Read, Show)

_MatchMode = Core.Name "openGql.grammar.MatchMode"

_MatchMode_repeatableElements = Core.Name "repeatableElements"

_MatchMode_differentEdges = Core.Name "differentEdges"

type RepeatableElementsMatchMode = ElementBindingsOrElements

_RepeatableElementsMatchMode = Core.Name "openGql.grammar.RepeatableElementsMatchMode"

type DifferentEdgesMatchMode = EdgeBindingsOrEdges

_DifferentEdgesMatchMode = Core.Name "openGql.grammar.DifferentEdgesMatchMode"

data ElementBindingsOrElements = 
  ElementBindingsOrElementsElementBindings Bool |
  ElementBindingsOrElementsElements 
  deriving (Eq, Ord, Read, Show)

_ElementBindingsOrElements = Core.Name "openGql.grammar.ElementBindingsOrElements"

_ElementBindingsOrElements_elementBindings = Core.Name "elementBindings"

_ElementBindingsOrElements_elements = Core.Name "elements"

data EdgeBindingsOrEdges = 
  EdgeBindingsOrEdgesEdgeBindings Bool |
  EdgeBindingsOrEdgesEdges 
  deriving (Eq, Ord, Read, Show)

_EdgeBindingsOrEdges = Core.Name "openGql.grammar.EdgeBindingsOrEdges"

_EdgeBindingsOrEdges_edgeBindings = Core.Name "edgeBindings"

_EdgeBindingsOrEdges_edges = Core.Name "edges"

type PathPatternList = [PathPattern]

_PathPatternList = Core.Name "openGql.grammar.PathPatternList"

data PathPattern = 
  PathPattern {
    pathPatternVariableDeclaration :: (Maybe PathVariableDeclaration),
    pathPatternPrefix :: (Maybe PathPatternPrefix),
    pathPatternExpression :: PathPatternExpression}
  deriving (Eq, Ord, Read, Show)

_PathPattern = Core.Name "openGql.grammar.PathPattern"

_PathPattern_variableDeclaration = Core.Name "variableDeclaration"

_PathPattern_prefix = Core.Name "prefix"

_PathPattern_expression = Core.Name "expression"

type PathVariableDeclaration = PathVariable

_PathVariableDeclaration = Core.Name "openGql.grammar.PathVariableDeclaration"

type KeepClause = PathPatternPrefix

_KeepClause = Core.Name "openGql.grammar.KeepClause"

type GraphPatternWhereClause = SearchCondition

_GraphPatternWhereClause = Core.Name "openGql.grammar.GraphPatternWhereClause"

type InsertGraphPattern = InsertPathPatternList

_InsertGraphPattern = Core.Name "openGql.grammar.InsertGraphPattern"

type InsertPathPatternList = [InsertPathPattern]

_InsertPathPatternList = Core.Name "openGql.grammar.InsertPathPatternList"

data InsertPathPattern = 
  InsertPathPattern {
    insertPathPatternStartNode :: InsertNodePattern,
    insertPathPatternEdgesAndNodes :: [InsertEdgeAndNode]}
  deriving (Eq, Ord, Read, Show)

_InsertPathPattern = Core.Name "openGql.grammar.InsertPathPattern"

_InsertPathPattern_startNode = Core.Name "startNode"

_InsertPathPattern_edgesAndNodes = Core.Name "edgesAndNodes"

data InsertEdgeAndNode = 
  InsertEdgeAndNode {
    insertEdgeAndNodeEdge :: InsertEdgePattern,
    insertEdgeAndNodeNode :: InsertNodePattern}
  deriving (Eq, Ord, Read, Show)

_InsertEdgeAndNode = Core.Name "openGql.grammar.InsertEdgeAndNode"

_InsertEdgeAndNode_edge = Core.Name "edge"

_InsertEdgeAndNode_node = Core.Name "node"

type InsertNodePattern = (Maybe InsertElementPatternFiller)

_InsertNodePattern = Core.Name "openGql.grammar.InsertNodePattern"

data InsertEdgePattern = 
  InsertEdgePatternPointingLeft InsertEdgePointingLeft |
  InsertEdgePatternPointingRight InsertEdgePointingRight |
  InsertEdgePatternUndirected InsertEdgeUndirected
  deriving (Eq, Ord, Read, Show)

_InsertEdgePattern = Core.Name "openGql.grammar.InsertEdgePattern"

_InsertEdgePattern_pointingLeft = Core.Name "pointingLeft"

_InsertEdgePattern_pointingRight = Core.Name "pointingRight"

_InsertEdgePattern_undirected = Core.Name "undirected"

type InsertEdgePointingLeft = (Maybe InsertElementPatternFiller)

_InsertEdgePointingLeft = Core.Name "openGql.grammar.InsertEdgePointingLeft"

type InsertEdgePointingRight = (Maybe InsertElementPatternFiller)

_InsertEdgePointingRight = Core.Name "openGql.grammar.InsertEdgePointingRight"

type InsertEdgeUndirected = (Maybe InsertElementPatternFiller)

_InsertEdgeUndirected = Core.Name "openGql.grammar.InsertEdgeUndirected"

data InsertElementPatternFiller = 
  InsertElementPatternFiller {
    insertElementPatternFillerVariableDeclaration :: (Maybe ElementVariableDeclaration),
    insertElementPatternFillerLabelAndProperties :: (Maybe LabelAndPropertySetSpecification)}
  deriving (Eq, Ord, Read, Show)

_InsertElementPatternFiller = Core.Name "openGql.grammar.InsertElementPatternFiller"

_InsertElementPatternFiller_variableDeclaration = Core.Name "variableDeclaration"

_InsertElementPatternFiller_labelAndProperties = Core.Name "labelAndProperties"

data LabelAndPropertySetSpecification = 
  LabelAndPropertySetSpecification {
    labelAndPropertySetSpecificationIsOrColon :: (Maybe IsOrColon),
    labelAndPropertySetSpecificationLabelSet :: (Maybe LabelSetSpecification),
    labelAndPropertySetSpecificationPropertySpecification :: (Maybe ElementPropertySpecification)}
  deriving (Eq, Ord, Read, Show)

_LabelAndPropertySetSpecification = Core.Name "openGql.grammar.LabelAndPropertySetSpecification"

_LabelAndPropertySetSpecification_isOrColon = Core.Name "isOrColon"

_LabelAndPropertySetSpecification_labelSet = Core.Name "labelSet"

_LabelAndPropertySetSpecification_propertySpecification = Core.Name "propertySpecification"

data PathPatternPrefix = 
  PathPatternPrefixModePrefix PathModePrefix |
  PathPatternPrefixSearchPrefix PathSearchPrefix
  deriving (Eq, Ord, Read, Show)

_PathPatternPrefix = Core.Name "openGql.grammar.PathPatternPrefix"

_PathPatternPrefix_modePrefix = Core.Name "modePrefix"

_PathPatternPrefix_searchPrefix = Core.Name "searchPrefix"

data PathModePrefix = 
  PathModePrefix {
    pathModePrefixMode :: PathMode,
    pathModePrefixOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_PathModePrefix = Core.Name "openGql.grammar.PathModePrefix"

_PathModePrefix_mode = Core.Name "mode"

_PathModePrefix_orPaths = Core.Name "orPaths"

data PathMode = 
  PathModeWalk  |
  PathModeTrail  |
  PathModeSimple  |
  PathModeAcyclic 
  deriving (Eq, Ord, Read, Show)

_PathMode = Core.Name "openGql.grammar.PathMode"

_PathMode_walk = Core.Name "walk"

_PathMode_trail = Core.Name "trail"

_PathMode_simple = Core.Name "simple"

_PathMode_acyclic = Core.Name "acyclic"

data PathSearchPrefix = 
  PathSearchPrefixAll AllPathSearch |
  PathSearchPrefixAny AnyPathSearch |
  PathSearchPrefixShortest ShortestPathSearch
  deriving (Eq, Ord, Read, Show)

_PathSearchPrefix = Core.Name "openGql.grammar.PathSearchPrefix"

_PathSearchPrefix_all = Core.Name "all"

_PathSearchPrefix_any = Core.Name "any"

_PathSearchPrefix_shortest = Core.Name "shortest"

data AllPathSearch = 
  AllPathSearch {
    allPathSearchMode :: (Maybe PathMode),
    allPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AllPathSearch = Core.Name "openGql.grammar.AllPathSearch"

_AllPathSearch_mode = Core.Name "mode"

_AllPathSearch_orPaths = Core.Name "orPaths"

data PathOrPaths = 
  PathOrPathsPath  |
  PathOrPathsPaths 
  deriving (Eq, Ord, Read, Show)

_PathOrPaths = Core.Name "openGql.grammar.PathOrPaths"

_PathOrPaths_path = Core.Name "path"

_PathOrPaths_paths = Core.Name "paths"

data AnyPathSearch = 
  AnyPathSearch {
    anyPathSearchNumberOfPaths :: (Maybe NumberOfPaths),
    anyPathSearchMode :: (Maybe PathMode),
    anyPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AnyPathSearch = Core.Name "openGql.grammar.AnyPathSearch"

_AnyPathSearch_numberOfPaths = Core.Name "numberOfPaths"

_AnyPathSearch_mode = Core.Name "mode"

_AnyPathSearch_orPaths = Core.Name "orPaths"

type NumberOfPaths = NonNegativeIntegerSpecification

_NumberOfPaths = Core.Name "openGql.grammar.NumberOfPaths"

data ShortestPathSearch = 
  ShortestPathSearchAllShortest AllShortestPathSearch |
  ShortestPathSearchAnyShortest AnyShortestPathSearch |
  ShortestPathSearchCountedShortest CountedShortestPathSearch |
  ShortestPathSearchCountedShortestGroup CountedShortestGroupSearch
  deriving (Eq, Ord, Read, Show)

_ShortestPathSearch = Core.Name "openGql.grammar.ShortestPathSearch"

_ShortestPathSearch_allShortest = Core.Name "allShortest"

_ShortestPathSearch_anyShortest = Core.Name "anyShortest"

_ShortestPathSearch_countedShortest = Core.Name "countedShortest"

_ShortestPathSearch_countedShortestGroup = Core.Name "countedShortestGroup"

data AllShortestPathSearch = 
  AllShortestPathSearch {
    allShortestPathSearchMode :: (Maybe PathMode),
    allShortestPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AllShortestPathSearch = Core.Name "openGql.grammar.AllShortestPathSearch"

_AllShortestPathSearch_mode = Core.Name "mode"

_AllShortestPathSearch_orPaths = Core.Name "orPaths"

data AnyShortestPathSearch = 
  AnyShortestPathSearch {
    anyShortestPathSearchMode :: (Maybe PathMode),
    anyShortestPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_AnyShortestPathSearch = Core.Name "openGql.grammar.AnyShortestPathSearch"

_AnyShortestPathSearch_mode = Core.Name "mode"

_AnyShortestPathSearch_orPaths = Core.Name "orPaths"

data CountedShortestPathSearch = 
  CountedShortestPathSearch {
    countedShortestPathSearchNumberOfPaths :: NumberOfPaths,
    countedShortestPathSearchMode :: (Maybe PathMode),
    countedShortestPathSearchOrPaths :: (Maybe PathOrPaths)}
  deriving (Eq, Ord, Read, Show)

_CountedShortestPathSearch = Core.Name "openGql.grammar.CountedShortestPathSearch"

_CountedShortestPathSearch_numberOfPaths = Core.Name "numberOfPaths"

_CountedShortestPathSearch_mode = Core.Name "mode"

_CountedShortestPathSearch_orPaths = Core.Name "orPaths"

data CountedShortestGroupSearch = 
  CountedShortestGroupSearch {
    countedShortestGroupSearchNumberOfGroups :: (Maybe NumberOfGroups),
    countedShortestGroupSearchMode :: (Maybe PathMode),
    countedShortestGroupSearchOrPaths :: (Maybe PathOrPaths),
    countedShortestGroupSearchGroups :: Bool}
  deriving (Eq, Ord, Read, Show)

_CountedShortestGroupSearch = Core.Name "openGql.grammar.CountedShortestGroupSearch"

_CountedShortestGroupSearch_numberOfGroups = Core.Name "numberOfGroups"

_CountedShortestGroupSearch_mode = Core.Name "mode"

_CountedShortestGroupSearch_orPaths = Core.Name "orPaths"

_CountedShortestGroupSearch_groups = Core.Name "groups"

type NumberOfGroups = NonNegativeIntegerSpecification

_NumberOfGroups = Core.Name "openGql.grammar.NumberOfGroups"

data PathPatternExpression = 
  PathPatternExpressionTerm PathTerm |
  PathPatternExpressionMultisetAlternation [PathTerm] |
  PathPatternExpressionPatternUnion [PathTerm]
  deriving (Eq, Ord, Read, Show)

_PathPatternExpression = Core.Name "openGql.grammar.PathPatternExpression"

_PathPatternExpression_term = Core.Name "term"

_PathPatternExpression_multisetAlternation = Core.Name "multisetAlternation"

_PathPatternExpression_patternUnion = Core.Name "patternUnion"

type PathTerm = [PathFactor]

_PathTerm = Core.Name "openGql.grammar.PathTerm"

data PathFactor = 
  PathFactorPrimary PathPrimary |
  PathFactorQuantifiedPrimary QuantifiedPathPrimary |
  PathFactorQuestionedPrimary QuestionedPathPrimary
  deriving (Eq, Ord, Read, Show)

_PathFactor = Core.Name "openGql.grammar.PathFactor"

_PathFactor_primary = Core.Name "primary"

_PathFactor_quantifiedPrimary = Core.Name "quantifiedPrimary"

_PathFactor_questionedPrimary = Core.Name "questionedPrimary"

data QuantifiedPathPrimary = 
  QuantifiedPathPrimary {
    quantifiedPathPrimaryPrimary :: PathPrimary,
    quantifiedPathPrimaryQuantifier :: GraphPatternQuantifier}
  deriving (Eq, Ord, Read, Show)

_QuantifiedPathPrimary = Core.Name "openGql.grammar.QuantifiedPathPrimary"

_QuantifiedPathPrimary_primary = Core.Name "primary"

_QuantifiedPathPrimary_quantifier = Core.Name "quantifier"

type QuestionedPathPrimary = PathPrimary

_QuestionedPathPrimary = Core.Name "openGql.grammar.QuestionedPathPrimary"

data PathPrimary = 
  PathPrimaryElementPattern ElementPattern |
  PathPrimaryParenthesizedExpression ParenthesizedPathPatternExpression |
  PathPrimarySimplifiedExpression SimplifiedPathPatternExpression
  deriving (Eq, Ord, Read, Show)

_PathPrimary = Core.Name "openGql.grammar.PathPrimary"

_PathPrimary_elementPattern = Core.Name "elementPattern"

_PathPrimary_parenthesizedExpression = Core.Name "parenthesizedExpression"

_PathPrimary_simplifiedExpression = Core.Name "simplifiedExpression"

data ElementPattern = 
  ElementPatternNode NodePattern |
  ElementPatternEdge EdgePattern
  deriving (Eq, Ord, Read, Show)

_ElementPattern = Core.Name "openGql.grammar.ElementPattern"

_ElementPattern_node = Core.Name "node"

_ElementPattern_edge = Core.Name "edge"

type NodePattern = ElementPatternFiller

_NodePattern = Core.Name "openGql.grammar.NodePattern"

data ElementPatternFiller = 
  ElementPatternFiller {
    elementPatternFillerVariableDeclaration :: (Maybe ElementVariableDeclaration),
    elementPatternFillerIsLabelExpression :: (Maybe IsLabelExpression),
    elementPatternFillerPredicate :: (Maybe ElementPatternPredicate)}
  deriving (Eq, Ord, Read, Show)

_ElementPatternFiller = Core.Name "openGql.grammar.ElementPatternFiller"

_ElementPatternFiller_variableDeclaration = Core.Name "variableDeclaration"

_ElementPatternFiller_isLabelExpression = Core.Name "isLabelExpression"

_ElementPatternFiller_predicate = Core.Name "predicate"

data ElementVariableDeclaration = 
  ElementVariableDeclaration {
    elementVariableDeclarationTemp :: (Maybe Bool),
    elementVariableDeclarationVariable :: ElementVariable}
  deriving (Eq, Ord, Read, Show)

_ElementVariableDeclaration = Core.Name "openGql.grammar.ElementVariableDeclaration"

_ElementVariableDeclaration_temp = Core.Name "temp"

_ElementVariableDeclaration_variable = Core.Name "variable"

data IsLabelExpression = 
  IsLabelExpression {
    isLabelExpressionIsOrColon :: IsOrColon,
    isLabelExpressionLabel :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_IsLabelExpression = Core.Name "openGql.grammar.IsLabelExpression"

_IsLabelExpression_isOrColon = Core.Name "isOrColon"

_IsLabelExpression_label = Core.Name "label"

data IsOrColon = 
  IsOrColonIs  |
  IsOrColonColon 
  deriving (Eq, Ord, Read, Show)

_IsOrColon = Core.Name "openGql.grammar.IsOrColon"

_IsOrColon_is = Core.Name "is"

_IsOrColon_colon = Core.Name "colon"

data ElementPatternPredicate = 
  ElementPatternPredicateWhereClause ElementPatternWhereClause |
  ElementPatternPredicatePropertySpecification ElementPropertySpecification
  deriving (Eq, Ord, Read, Show)

_ElementPatternPredicate = Core.Name "openGql.grammar.ElementPatternPredicate"

_ElementPatternPredicate_whereClause = Core.Name "whereClause"

_ElementPatternPredicate_propertySpecification = Core.Name "propertySpecification"

type ElementPatternWhereClause = SearchCondition

_ElementPatternWhereClause = Core.Name "openGql.grammar.ElementPatternWhereClause"

type ElementPropertySpecification = PropertyKeyValuePairList

_ElementPropertySpecification = Core.Name "openGql.grammar.ElementPropertySpecification"

type PropertyKeyValuePairList = [PropertyKeyValuePair]

_PropertyKeyValuePairList = Core.Name "openGql.grammar.PropertyKeyValuePairList"

data PropertyKeyValuePair = 
  PropertyKeyValuePair {
    propertyKeyValuePairName :: PropertyName,
    propertyKeyValuePairValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyValuePair = Core.Name "openGql.grammar.PropertyKeyValuePair"

_PropertyKeyValuePair_name = Core.Name "name"

_PropertyKeyValuePair_value = Core.Name "value"

data EdgePattern = 
  EdgePatternFullEdge FullEdgePattern |
  EdgePatternAbbreviatedEdge AbbreviatedEdgePattern
  deriving (Eq, Ord, Read, Show)

_EdgePattern = Core.Name "openGql.grammar.EdgePattern"

_EdgePattern_fullEdge = Core.Name "fullEdge"

_EdgePattern_abbreviatedEdge = Core.Name "abbreviatedEdge"

data FullEdgePattern = 
  FullEdgePatternPointingLeft FullEdgePointingLeft |
  FullEdgePatternUndirected FullEdgeUndirected |
  FullEdgePatternPointingRight FullEdgePointingRight |
  FullEdgePatternLeftOrUndirected FullEdgeLeftOrUndirected |
  FullEdgePatternUndirectedOrRight FullEdgeUndirectedOrRight |
  FullEdgePatternLeftOrRight FullEdgeLeftOrRight |
  FullEdgePatternAnyDirection FullEdgeAnyDirection
  deriving (Eq, Ord, Read, Show)

_FullEdgePattern = Core.Name "openGql.grammar.FullEdgePattern"

_FullEdgePattern_pointingLeft = Core.Name "pointingLeft"

_FullEdgePattern_undirected = Core.Name "undirected"

_FullEdgePattern_pointingRight = Core.Name "pointingRight"

_FullEdgePattern_leftOrUndirected = Core.Name "leftOrUndirected"

_FullEdgePattern_undirectedOrRight = Core.Name "undirectedOrRight"

_FullEdgePattern_leftOrRight = Core.Name "leftOrRight"

_FullEdgePattern_anyDirection = Core.Name "anyDirection"

type FullEdgePointingLeft = ElementPatternFiller

_FullEdgePointingLeft = Core.Name "openGql.grammar.FullEdgePointingLeft"

type FullEdgeUndirected = ElementPatternFiller

_FullEdgeUndirected = Core.Name "openGql.grammar.FullEdgeUndirected"

type FullEdgePointingRight = ElementPatternFiller

_FullEdgePointingRight = Core.Name "openGql.grammar.FullEdgePointingRight"

type FullEdgeLeftOrUndirected = ElementPatternFiller

_FullEdgeLeftOrUndirected = Core.Name "openGql.grammar.FullEdgeLeftOrUndirected"

type FullEdgeUndirectedOrRight = ElementPatternFiller

_FullEdgeUndirectedOrRight = Core.Name "openGql.grammar.FullEdgeUndirectedOrRight"

type FullEdgeLeftOrRight = ElementPatternFiller

_FullEdgeLeftOrRight = Core.Name "openGql.grammar.FullEdgeLeftOrRight"

type FullEdgeAnyDirection = ElementPatternFiller

_FullEdgeAnyDirection = Core.Name "openGql.grammar.FullEdgeAnyDirection"

data AbbreviatedEdgePattern = 
  AbbreviatedEdgePatternLeftArrow  |
  AbbreviatedEdgePatternTilde  |
  AbbreviatedEdgePatternRightArrow  |
  AbbreviatedEdgePatternLeftArrowTilde  |
  AbbreviatedEdgePatternTildeRightArrow  |
  AbbreviatedEdgePatternLeftMinusRight  |
  AbbreviatedEdgePatternMinusSign 
  deriving (Eq, Ord, Read, Show)

_AbbreviatedEdgePattern = Core.Name "openGql.grammar.AbbreviatedEdgePattern"

_AbbreviatedEdgePattern_leftArrow = Core.Name "leftArrow"

_AbbreviatedEdgePattern_tilde = Core.Name "tilde"

_AbbreviatedEdgePattern_rightArrow = Core.Name "rightArrow"

_AbbreviatedEdgePattern_leftArrowTilde = Core.Name "leftArrowTilde"

_AbbreviatedEdgePattern_tildeRightArrow = Core.Name "tildeRightArrow"

_AbbreviatedEdgePattern_leftMinusRight = Core.Name "leftMinusRight"

_AbbreviatedEdgePattern_minusSign = Core.Name "minusSign"

data ParenthesizedPathPatternExpression = 
  ParenthesizedPathPatternExpression {
    parenthesizedPathPatternExpressionSubpathDeclaration :: (Maybe SubpathVariableDeclaration),
    parenthesizedPathPatternExpressionPathMode :: (Maybe PathModePrefix),
    parenthesizedPathPatternExpressionExpression :: PathPatternExpression,
    parenthesizedPathPatternExpressionWhereClause :: (Maybe ParenthesizedPathPatternWhereClause)}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedPathPatternExpression = Core.Name "openGql.grammar.ParenthesizedPathPatternExpression"

_ParenthesizedPathPatternExpression_subpathDeclaration = Core.Name "subpathDeclaration"

_ParenthesizedPathPatternExpression_pathMode = Core.Name "pathMode"

_ParenthesizedPathPatternExpression_expression = Core.Name "expression"

_ParenthesizedPathPatternExpression_whereClause = Core.Name "whereClause"

type SubpathVariableDeclaration = SubpathVariable

_SubpathVariableDeclaration = Core.Name "openGql.grammar.SubpathVariableDeclaration"

type ParenthesizedPathPatternWhereClause = SearchCondition

_ParenthesizedPathPatternWhereClause = Core.Name "openGql.grammar.ParenthesizedPathPatternWhereClause"

data LabelExpression = 
  LabelExpressionNegation LabelExpression |
  LabelExpressionConjunction ConjunctionLabelExpression |
  LabelExpressionDisjunction DisjunctionLabelExpression |
  LabelExpressionName LabelName |
  LabelExpressionWildcard  |
  LabelExpressionParenthesized LabelExpression
  deriving (Eq, Ord, Read, Show)

_LabelExpression = Core.Name "openGql.grammar.LabelExpression"

_LabelExpression_negation = Core.Name "negation"

_LabelExpression_conjunction = Core.Name "conjunction"

_LabelExpression_disjunction = Core.Name "disjunction"

_LabelExpression_name = Core.Name "name"

_LabelExpression_wildcard = Core.Name "wildcard"

_LabelExpression_parenthesized = Core.Name "parenthesized"

data ConjunctionLabelExpression = 
  ConjunctionLabelExpression {
    conjunctionLabelExpressionLeft :: LabelExpression,
    conjunctionLabelExpressionRight :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_ConjunctionLabelExpression = Core.Name "openGql.grammar.ConjunctionLabelExpression"

_ConjunctionLabelExpression_left = Core.Name "left"

_ConjunctionLabelExpression_right = Core.Name "right"

data DisjunctionLabelExpression = 
  DisjunctionLabelExpression {
    disjunctionLabelExpressionLeft :: LabelExpression,
    disjunctionLabelExpressionRight :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_DisjunctionLabelExpression = Core.Name "openGql.grammar.DisjunctionLabelExpression"

_DisjunctionLabelExpression_left = Core.Name "left"

_DisjunctionLabelExpression_right = Core.Name "right"

type PathVariableReference = BindingVariableReference

_PathVariableReference = Core.Name "openGql.grammar.PathVariableReference"

type ElementVariableReference = BindingVariableReference

_ElementVariableReference = Core.Name "openGql.grammar.ElementVariableReference"

data GraphPatternQuantifier = 
  GraphPatternQuantifierAsterisk  |
  GraphPatternQuantifierPlusSign  |
  GraphPatternQuantifierFixed FixedQuantifier |
  GraphPatternQuantifierGeneral GeneralQuantifier
  deriving (Eq, Ord, Read, Show)

_GraphPatternQuantifier = Core.Name "openGql.grammar.GraphPatternQuantifier"

_GraphPatternQuantifier_asterisk = Core.Name "asterisk"

_GraphPatternQuantifier_plusSign = Core.Name "plusSign"

_GraphPatternQuantifier_fixed = Core.Name "fixed"

_GraphPatternQuantifier_general = Core.Name "general"

type FixedQuantifier = UnsignedInteger

_FixedQuantifier = Core.Name "openGql.grammar.FixedQuantifier"

data GeneralQuantifier = 
  GeneralQuantifier {
    generalQuantifierLowerBound :: (Maybe LowerBound),
    generalQuantifierUpperBound :: (Maybe UpperBound)}
  deriving (Eq, Ord, Read, Show)

_GeneralQuantifier = Core.Name "openGql.grammar.GeneralQuantifier"

_GeneralQuantifier_lowerBound = Core.Name "lowerBound"

_GeneralQuantifier_upperBound = Core.Name "upperBound"

type LowerBound = UnsignedInteger

_LowerBound = Core.Name "openGql.grammar.LowerBound"

type UpperBound = UnsignedInteger

_UpperBound = Core.Name "openGql.grammar.UpperBound"

data SimplifiedPathPatternExpression = 
  SimplifiedPathPatternExpressionLeft SimplifiedDefaultingLeft |
  SimplifiedPathPatternExpressionUndirected SimplifiedDefaultingUndirected |
  SimplifiedPathPatternExpressionRight SimplifiedDefaultingRight |
  SimplifiedPathPatternExpressionLeftOrUndirected SimplifiedDefaultingLeftOrUndirected |
  SimplifiedPathPatternExpressionUndirectedOrRight SimplifiedDefaultingUndirectedOrRight |
  SimplifiedPathPatternExpressionLeftOrRight SimplifiedDefaultingLeftOrRight |
  SimplifiedPathPatternExpressionAnyDirection SimplifiedDefaultingAnyDirection
  deriving (Eq, Ord, Read, Show)

_SimplifiedPathPatternExpression = Core.Name "openGql.grammar.SimplifiedPathPatternExpression"

_SimplifiedPathPatternExpression_left = Core.Name "left"

_SimplifiedPathPatternExpression_undirected = Core.Name "undirected"

_SimplifiedPathPatternExpression_right = Core.Name "right"

_SimplifiedPathPatternExpression_leftOrUndirected = Core.Name "leftOrUndirected"

_SimplifiedPathPatternExpression_undirectedOrRight = Core.Name "undirectedOrRight"

_SimplifiedPathPatternExpression_leftOrRight = Core.Name "leftOrRight"

_SimplifiedPathPatternExpression_anyDirection = Core.Name "anyDirection"

type SimplifiedDefaultingLeft = SimplifiedContents

_SimplifiedDefaultingLeft = Core.Name "openGql.grammar.SimplifiedDefaultingLeft"

type SimplifiedDefaultingUndirected = SimplifiedContents

_SimplifiedDefaultingUndirected = Core.Name "openGql.grammar.SimplifiedDefaultingUndirected"

type SimplifiedDefaultingRight = SimplifiedContents

_SimplifiedDefaultingRight = Core.Name "openGql.grammar.SimplifiedDefaultingRight"

type SimplifiedDefaultingLeftOrUndirected = SimplifiedContents

_SimplifiedDefaultingLeftOrUndirected = Core.Name "openGql.grammar.SimplifiedDefaultingLeftOrUndirected"

type SimplifiedDefaultingUndirectedOrRight = SimplifiedContents

_SimplifiedDefaultingUndirectedOrRight = Core.Name "openGql.grammar.SimplifiedDefaultingUndirectedOrRight"

type SimplifiedDefaultingLeftOrRight = SimplifiedContents

_SimplifiedDefaultingLeftOrRight = Core.Name "openGql.grammar.SimplifiedDefaultingLeftOrRight"

type SimplifiedDefaultingAnyDirection = SimplifiedContents

_SimplifiedDefaultingAnyDirection = Core.Name "openGql.grammar.SimplifiedDefaultingAnyDirection"

data SimplifiedContents = 
  SimplifiedContentsTerm SimplifiedTerm |
  SimplifiedContentsPathUnion SimplifiedPathUnion |
  SimplifiedContentsMultisetAlternation SimplifiedMultisetAlternation
  deriving (Eq, Ord, Read, Show)

_SimplifiedContents = Core.Name "openGql.grammar.SimplifiedContents"

_SimplifiedContents_term = Core.Name "term"

_SimplifiedContents_pathUnion = Core.Name "pathUnion"

_SimplifiedContents_multisetAlternation = Core.Name "multisetAlternation"

type SimplifiedPathUnion = [SimplifiedTerm]

_SimplifiedPathUnion = Core.Name "openGql.grammar.SimplifiedPathUnion"

type SimplifiedMultisetAlternation = [SimplifiedTerm]

_SimplifiedMultisetAlternation = Core.Name "openGql.grammar.SimplifiedMultisetAlternation"

data SimplifiedTerm = 
  SimplifiedTermFactorLow SimplifiedFactorLow |
  SimplifiedTermConcatenation SimplifiedConcatenation
  deriving (Eq, Ord, Read, Show)

_SimplifiedTerm = Core.Name "openGql.grammar.SimplifiedTerm"

_SimplifiedTerm_factorLow = Core.Name "factorLow"

_SimplifiedTerm_concatenation = Core.Name "concatenation"

data SimplifiedConcatenation = 
  SimplifiedConcatenation {
    simplifiedConcatenationInitialTerm :: SimplifiedTerm,
    simplifiedConcatenationNextFactor :: SimplifiedFactorLow}
  deriving (Eq, Ord, Read, Show)

_SimplifiedConcatenation = Core.Name "openGql.grammar.SimplifiedConcatenation"

_SimplifiedConcatenation_initialTerm = Core.Name "initialTerm"

_SimplifiedConcatenation_nextFactor = Core.Name "nextFactor"

data SimplifiedFactorLow = 
  SimplifiedFactorLowFactorHigh SimplifiedFactorHigh |
  SimplifiedFactorLowConjunction SimplifiedConjunction
  deriving (Eq, Ord, Read, Show)

_SimplifiedFactorLow = Core.Name "openGql.grammar.SimplifiedFactorLow"

_SimplifiedFactorLow_factorHigh = Core.Name "factorHigh"

_SimplifiedFactorLow_conjunction = Core.Name "conjunction"

data SimplifiedConjunction = 
  SimplifiedConjunction {
    simplifiedConjunctionLeft :: SimplifiedFactorLow,
    simplifiedConjunctionRight :: SimplifiedFactorHigh}
  deriving (Eq, Ord, Read, Show)

_SimplifiedConjunction = Core.Name "openGql.grammar.SimplifiedConjunction"

_SimplifiedConjunction_left = Core.Name "left"

_SimplifiedConjunction_right = Core.Name "right"

data SimplifiedFactorHigh = 
  SimplifiedFactorHighTertiary SimplifiedTertiary |
  SimplifiedFactorHighQuantified SimplifiedQuantified |
  SimplifiedFactorHighQuestioned SimplifiedQuestioned
  deriving (Eq, Ord, Read, Show)

_SimplifiedFactorHigh = Core.Name "openGql.grammar.SimplifiedFactorHigh"

_SimplifiedFactorHigh_tertiary = Core.Name "tertiary"

_SimplifiedFactorHigh_quantified = Core.Name "quantified"

_SimplifiedFactorHigh_questioned = Core.Name "questioned"

data SimplifiedQuantified = 
  SimplifiedQuantified {
    simplifiedQuantifiedTertiary :: SimplifiedTertiary,
    simplifiedQuantifiedQuantifier :: GraphPatternQuantifier}
  deriving (Eq, Ord, Read, Show)

_SimplifiedQuantified = Core.Name "openGql.grammar.SimplifiedQuantified"

_SimplifiedQuantified_tertiary = Core.Name "tertiary"

_SimplifiedQuantified_quantifier = Core.Name "quantifier"

type SimplifiedQuestioned = SimplifiedTertiary

_SimplifiedQuestioned = Core.Name "openGql.grammar.SimplifiedQuestioned"

data SimplifiedTertiary = 
  SimplifiedTertiaryDirectionOverride SimplifiedDirectionOverride |
  SimplifiedTertiarySecondary SimplifiedSecondary
  deriving (Eq, Ord, Read, Show)

_SimplifiedTertiary = Core.Name "openGql.grammar.SimplifiedTertiary"

_SimplifiedTertiary_directionOverride = Core.Name "directionOverride"

_SimplifiedTertiary_secondary = Core.Name "secondary"

data SimplifiedDirectionOverride = 
  SimplifiedDirectionOverrideOverrideLeft SimplifiedOverrideLeft |
  SimplifiedDirectionOverrideOverrideUndirected SimplifiedOverrideUndirected |
  SimplifiedDirectionOverrideOverrideRight SimplifiedOverrideRight |
  SimplifiedDirectionOverrideOverrideLeftOrUndirected SimplifiedOverrideLeftOrUndirected |
  SimplifiedDirectionOverrideOverrideUndirectedOrRight SimplifiedOverrideUndirectedOrRight |
  SimplifiedDirectionOverrideOverrideLeftOrRight SimplifiedOverrideLeftOrRight |
  SimplifiedDirectionOverrideOverrideAnyDirection SimplifiedOverrideAnyDirection
  deriving (Eq, Ord, Read, Show)

_SimplifiedDirectionOverride = Core.Name "openGql.grammar.SimplifiedDirectionOverride"

_SimplifiedDirectionOverride_overrideLeft = Core.Name "overrideLeft"

_SimplifiedDirectionOverride_overrideUndirected = Core.Name "overrideUndirected"

_SimplifiedDirectionOverride_overrideRight = Core.Name "overrideRight"

_SimplifiedDirectionOverride_overrideLeftOrUndirected = Core.Name "overrideLeftOrUndirected"

_SimplifiedDirectionOverride_overrideUndirectedOrRight = Core.Name "overrideUndirectedOrRight"

_SimplifiedDirectionOverride_overrideLeftOrRight = Core.Name "overrideLeftOrRight"

_SimplifiedDirectionOverride_overrideAnyDirection = Core.Name "overrideAnyDirection"

type SimplifiedOverrideLeft = SimplifiedSecondary

_SimplifiedOverrideLeft = Core.Name "openGql.grammar.SimplifiedOverrideLeft"

type SimplifiedOverrideUndirected = SimplifiedSecondary

_SimplifiedOverrideUndirected = Core.Name "openGql.grammar.SimplifiedOverrideUndirected"

type SimplifiedOverrideRight = SimplifiedSecondary

_SimplifiedOverrideRight = Core.Name "openGql.grammar.SimplifiedOverrideRight"

type SimplifiedOverrideLeftOrUndirected = SimplifiedSecondary

_SimplifiedOverrideLeftOrUndirected = Core.Name "openGql.grammar.SimplifiedOverrideLeftOrUndirected"

type SimplifiedOverrideUndirectedOrRight = SimplifiedSecondary

_SimplifiedOverrideUndirectedOrRight = Core.Name "openGql.grammar.SimplifiedOverrideUndirectedOrRight"

type SimplifiedOverrideLeftOrRight = SimplifiedSecondary

_SimplifiedOverrideLeftOrRight = Core.Name "openGql.grammar.SimplifiedOverrideLeftOrRight"

type SimplifiedOverrideAnyDirection = SimplifiedSecondary

_SimplifiedOverrideAnyDirection = Core.Name "openGql.grammar.SimplifiedOverrideAnyDirection"

data SimplifiedSecondary = 
  SimplifiedSecondaryPrimary SimplifiedPrimary |
  SimplifiedSecondaryNegation SimplifiedNegation
  deriving (Eq, Ord, Read, Show)

_SimplifiedSecondary = Core.Name "openGql.grammar.SimplifiedSecondary"

_SimplifiedSecondary_primary = Core.Name "primary"

_SimplifiedSecondary_negation = Core.Name "negation"

type SimplifiedNegation = SimplifiedPrimary

_SimplifiedNegation = Core.Name "openGql.grammar.SimplifiedNegation"

data SimplifiedPrimary = 
  SimplifiedPrimaryLabelName LabelName |
  SimplifiedPrimaryParenthesizedContents SimplifiedContents
  deriving (Eq, Ord, Read, Show)

_SimplifiedPrimary = Core.Name "openGql.grammar.SimplifiedPrimary"

_SimplifiedPrimary_labelName = Core.Name "labelName"

_SimplifiedPrimary_parenthesizedContents = Core.Name "parenthesizedContents"

type WhereClause = SearchCondition

_WhereClause = Core.Name "openGql.grammar.WhereClause"

type YieldClause = YieldItemList

_YieldClause = Core.Name "openGql.grammar.YieldClause"

type YieldItemList = [YieldItem]

_YieldItemList = Core.Name "openGql.grammar.YieldItemList"

data YieldItem = 
  YieldItem {
    yieldItemName :: YieldItemName,
    yieldItemAlias :: (Maybe YieldItemAlias)}
  deriving (Eq, Ord, Read, Show)

_YieldItem = Core.Name "openGql.grammar.YieldItem"

_YieldItem_name = Core.Name "name"

_YieldItem_alias = Core.Name "alias"

type YieldItemName = FieldName

_YieldItemName = Core.Name "openGql.grammar.YieldItemName"

type YieldItemAlias = BindingVariable

_YieldItemAlias = Core.Name "openGql.grammar.YieldItemAlias"

type GroupByClause = GroupingElementList

_GroupByClause = Core.Name "openGql.grammar.GroupByClause"

data GroupingElementList = 
  GroupingElementListElements [GroupingElement] |
  GroupingElementListEmptySet 
  deriving (Eq, Ord, Read, Show)

_GroupingElementList = Core.Name "openGql.grammar.GroupingElementList"

_GroupingElementList_elements = Core.Name "elements"

_GroupingElementList_emptySet = Core.Name "emptySet"

type GroupingElement = BindingVariableReference

_GroupingElement = Core.Name "openGql.grammar.GroupingElement"

type OrderByClause = SortSpecificationList

_OrderByClause = Core.Name "openGql.grammar.OrderByClause"

type SortSpecificationList = [SortSpecification]

_SortSpecificationList = Core.Name "openGql.grammar.SortSpecificationList"

data SortSpecification = 
  SortSpecification {
    sortSpecificationSortKey :: SortKey,
    sortSpecificationOrdering :: (Maybe OrderingSpecification),
    sortSpecificationNullOrdering :: (Maybe NullOrdering)}
  deriving (Eq, Ord, Read, Show)

_SortSpecification = Core.Name "openGql.grammar.SortSpecification"

_SortSpecification_sortKey = Core.Name "sortKey"

_SortSpecification_ordering = Core.Name "ordering"

_SortSpecification_nullOrdering = Core.Name "nullOrdering"

type SortKey = AggregatingValueExpression

_SortKey = Core.Name "openGql.grammar.SortKey"

data OrderingSpecification = 
  OrderingSpecificationAscending  |
  OrderingSpecificationDescending 
  deriving (Eq, Ord, Read, Show)

_OrderingSpecification = Core.Name "openGql.grammar.OrderingSpecification"

_OrderingSpecification_ascending = Core.Name "ascending"

_OrderingSpecification_descending = Core.Name "descending"

data NullOrdering = 
  NullOrderingNullsFirst  |
  NullOrderingNullsLast 
  deriving (Eq, Ord, Read, Show)

_NullOrdering = Core.Name "openGql.grammar.NullOrdering"

_NullOrdering_nullsFirst = Core.Name "nullsFirst"

_NullOrdering_nullsLast = Core.Name "nullsLast"

type LimitClause = NonNegativeIntegerSpecification

_LimitClause = Core.Name "openGql.grammar.LimitClause"

data OffsetClause = 
  OffsetClause {
    offsetClauseSynonym :: OffsetSynonym,
    offsetClauseValue :: NonNegativeIntegerSpecification}
  deriving (Eq, Ord, Read, Show)

_OffsetClause = Core.Name "openGql.grammar.OffsetClause"

_OffsetClause_synonym = Core.Name "synonym"

_OffsetClause_value = Core.Name "value"

data OffsetSynonym = 
  OffsetSynonymOffset  |
  OffsetSynonymSkipReservedWord 
  deriving (Eq, Ord, Read, Show)

_OffsetSynonym = Core.Name "openGql.grammar.OffsetSynonym"

_OffsetSynonym_offset = Core.Name "offset"

_OffsetSynonym_skipReservedWord = Core.Name "skipReservedWord"

data SchemaReference = 
  SchemaReferenceAbsoluteReference AbsoluteCatalogSchemaReference |
  SchemaReferenceRelativeReference RelativeCatalogSchemaReference |
  SchemaReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_SchemaReference = Core.Name "openGql.grammar.SchemaReference"

_SchemaReference_absoluteReference = Core.Name "absoluteReference"

_SchemaReference_relativeReference = Core.Name "relativeReference"

_SchemaReference_parameterSpecification = Core.Name "parameterSpecification"

data AbsoluteCatalogSchemaReference = 
  AbsoluteCatalogSchemaReferenceRoot  |
  AbsoluteCatalogSchemaReferenceDirectoryAndSchema AbsoluteDirectoryAndSchema
  deriving (Eq, Ord, Read, Show)

_AbsoluteCatalogSchemaReference = Core.Name "openGql.grammar.AbsoluteCatalogSchemaReference"

_AbsoluteCatalogSchemaReference_root = Core.Name "root"

_AbsoluteCatalogSchemaReference_directoryAndSchema = Core.Name "directoryAndSchema"

data AbsoluteDirectoryAndSchema = 
  AbsoluteDirectoryAndSchema {
    absoluteDirectoryAndSchemaDirectoryPath :: AbsoluteDirectoryPath,
    absoluteDirectoryAndSchemaSchemaName :: SchemaName}
  deriving (Eq, Ord, Read, Show)

_AbsoluteDirectoryAndSchema = Core.Name "openGql.grammar.AbsoluteDirectoryAndSchema"

_AbsoluteDirectoryAndSchema_directoryPath = Core.Name "directoryPath"

_AbsoluteDirectoryAndSchema_schemaName = Core.Name "schemaName"

type CatalogSchemaParentAndName = AbsoluteDirectoryAndSchema

_CatalogSchemaParentAndName = Core.Name "openGql.grammar.CatalogSchemaParentAndName"

data RelativeCatalogSchemaReference = 
  RelativeCatalogSchemaReferencePredefinedReference PredefinedSchemaReference |
  RelativeCatalogSchemaReferenceDirectoryAndSchema RelativeDirectoryAndSchema
  deriving (Eq, Ord, Read, Show)

_RelativeCatalogSchemaReference = Core.Name "openGql.grammar.RelativeCatalogSchemaReference"

_RelativeCatalogSchemaReference_predefinedReference = Core.Name "predefinedReference"

_RelativeCatalogSchemaReference_directoryAndSchema = Core.Name "directoryAndSchema"

data RelativeDirectoryAndSchema = 
  RelativeDirectoryAndSchema {
    relativeDirectoryAndSchemaDirectoryPath :: RelativeDirectoryPath,
    relativeDirectoryAndSchemaSchemaName :: SchemaName}
  deriving (Eq, Ord, Read, Show)

_RelativeDirectoryAndSchema = Core.Name "openGql.grammar.RelativeDirectoryAndSchema"

_RelativeDirectoryAndSchema_directoryPath = Core.Name "directoryPath"

_RelativeDirectoryAndSchema_schemaName = Core.Name "schemaName"

data PredefinedSchemaReference = 
  PredefinedSchemaReferenceHomeSchema  |
  PredefinedSchemaReferenceCurrentSchema  |
  PredefinedSchemaReferencePeriod 
  deriving (Eq, Ord, Read, Show)

_PredefinedSchemaReference = Core.Name "openGql.grammar.PredefinedSchemaReference"

_PredefinedSchemaReference_homeSchema = Core.Name "homeSchema"

_PredefinedSchemaReference_currentSchema = Core.Name "currentSchema"

_PredefinedSchemaReference_period = Core.Name "period"

type AbsoluteDirectoryPath = (Maybe SimpleDirectoryPath)

_AbsoluteDirectoryPath = Core.Name "openGql.grammar.AbsoluteDirectoryPath"

data RelativeDirectoryPath = 
  RelativeDirectoryPath {
    relativeDirectoryPathParentDirectories :: Int,
    relativeDirectoryPathSimplePath :: (Maybe SimpleDirectoryPath)}
  deriving (Eq, Ord, Read, Show)

_RelativeDirectoryPath = Core.Name "openGql.grammar.RelativeDirectoryPath"

_RelativeDirectoryPath_parentDirectories = Core.Name "parentDirectories"

_RelativeDirectoryPath_simplePath = Core.Name "simplePath"

type SimpleDirectoryPath = [DirectoryName]

_SimpleDirectoryPath = Core.Name "openGql.grammar.SimpleDirectoryPath"

data GraphReference = 
  GraphReferenceParentAndGraphName ParentAndGraphName |
  GraphReferenceDelimitedGraphName DelimitedGraphName |
  GraphReferenceHomeGraph HomeGraph |
  GraphReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_GraphReference = Core.Name "openGql.grammar.GraphReference"

_GraphReference_parentAndGraphName = Core.Name "parentAndGraphName"

_GraphReference_delimitedGraphName = Core.Name "delimitedGraphName"

_GraphReference_homeGraph = Core.Name "homeGraph"

_GraphReference_parameterSpecification = Core.Name "parameterSpecification"

data ParentAndGraphName = 
  ParentAndGraphName {
    parentAndGraphNameParentReference :: CatalogObjectParentReference,
    parentAndGraphNameGraphName :: GraphName}
  deriving (Eq, Ord, Read, Show)

_ParentAndGraphName = Core.Name "openGql.grammar.ParentAndGraphName"

_ParentAndGraphName_parentReference = Core.Name "parentReference"

_ParentAndGraphName_graphName = Core.Name "graphName"

data CatalogGraphParentAndName = 
  CatalogGraphParentAndName {
    catalogGraphParentAndNameParentReference :: (Maybe CatalogObjectParentReference),
    catalogGraphParentAndNameGraphName :: GraphName}
  deriving (Eq, Ord, Read, Show)

_CatalogGraphParentAndName = Core.Name "openGql.grammar.CatalogGraphParentAndName"

_CatalogGraphParentAndName_parentReference = Core.Name "parentReference"

_CatalogGraphParentAndName_graphName = Core.Name "graphName"

data HomeGraph = 
  HomeGraphHomePropertyGraph  |
  HomeGraphHomeGraph 
  deriving (Eq, Ord, Read, Show)

_HomeGraph = Core.Name "openGql.grammar.HomeGraph"

_HomeGraph_homePropertyGraph = Core.Name "homePropertyGraph"

_HomeGraph_homeGraph = Core.Name "homeGraph"

data GraphTypeReference = 
  GraphTypeReferenceParentAndTypeName CatalogGraphTypeParentAndName |
  GraphTypeReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_GraphTypeReference = Core.Name "openGql.grammar.GraphTypeReference"

_GraphTypeReference_parentAndTypeName = Core.Name "parentAndTypeName"

_GraphTypeReference_parameterSpecification = Core.Name "parameterSpecification"

data CatalogGraphTypeParentAndName = 
  CatalogGraphTypeParentAndName {
    catalogGraphTypeParentAndNameParentReference :: (Maybe CatalogObjectParentReference),
    catalogGraphTypeParentAndNameGraphTypeName :: GraphTypeName}
  deriving (Eq, Ord, Read, Show)

_CatalogGraphTypeParentAndName = Core.Name "openGql.grammar.CatalogGraphTypeParentAndName"

_CatalogGraphTypeParentAndName_parentReference = Core.Name "parentReference"

_CatalogGraphTypeParentAndName_graphTypeName = Core.Name "graphTypeName"

data BindingTableReference = 
  BindingTableReferenceParentAndTableName ParentAndTableName |
  BindingTableReferenceDelimitedBindingTableName DelimitedBindingTableName |
  BindingTableReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_BindingTableReference = Core.Name "openGql.grammar.BindingTableReference"

_BindingTableReference_parentAndTableName = Core.Name "parentAndTableName"

_BindingTableReference_delimitedBindingTableName = Core.Name "delimitedBindingTableName"

_BindingTableReference_parameterSpecification = Core.Name "parameterSpecification"

data ParentAndTableName = 
  ParentAndTableName {
    parentAndTableNameParentReference :: CatalogObjectParentReference,
    parentAndTableNameTableName :: BindingTableName}
  deriving (Eq, Ord, Read, Show)

_ParentAndTableName = Core.Name "openGql.grammar.ParentAndTableName"

_ParentAndTableName_parentReference = Core.Name "parentReference"

_ParentAndTableName_tableName = Core.Name "tableName"

data ProcedureReference = 
  ProcedureReferenceParentAndProcedureName CatalogProcedureParentAndName |
  ProcedureReferenceParameterSpecification ReferenceParameterSpecification
  deriving (Eq, Ord, Read, Show)

_ProcedureReference = Core.Name "openGql.grammar.ProcedureReference"

_ProcedureReference_parentAndProcedureName = Core.Name "parentAndProcedureName"

_ProcedureReference_parameterSpecification = Core.Name "parameterSpecification"

data CatalogProcedureParentAndName = 
  CatalogProcedureParentAndName {
    catalogProcedureParentAndNameParentReference :: (Maybe CatalogObjectParentReference),
    catalogProcedureParentAndNameProcedureName :: ProcedureName}
  deriving (Eq, Ord, Read, Show)

_CatalogProcedureParentAndName = Core.Name "openGql.grammar.CatalogProcedureParentAndName"

_CatalogProcedureParentAndName_parentReference = Core.Name "parentReference"

_CatalogProcedureParentAndName_procedureName = Core.Name "procedureName"

data CatalogObjectParentReference = 
  CatalogObjectParentReferenceSchemaAndObjects SchemaAndObjects |
  CatalogObjectParentReferenceObjectsOnly [ObjectName]
  deriving (Eq, Ord, Read, Show)

_CatalogObjectParentReference = Core.Name "openGql.grammar.CatalogObjectParentReference"

_CatalogObjectParentReference_schemaAndObjects = Core.Name "schemaAndObjects"

_CatalogObjectParentReference_objectsOnly = Core.Name "objectsOnly"

data SchemaAndObjects = 
  SchemaAndObjects {
    schemaAndObjectsSchemaReference :: SchemaReference,
    schemaAndObjectsObjects :: [ObjectName]}
  deriving (Eq, Ord, Read, Show)

_SchemaAndObjects = Core.Name "openGql.grammar.SchemaAndObjects"

_SchemaAndObjects_schemaReference = Core.Name "schemaReference"

_SchemaAndObjects_objects = Core.Name "objects"

type ReferenceParameterSpecification = ()

_ReferenceParameterSpecification = Core.Name "openGql.grammar.ReferenceParameterSpecification"

type NestedGraphTypeSpecification = GraphTypeSpecificationBody

_NestedGraphTypeSpecification = Core.Name "openGql.grammar.NestedGraphTypeSpecification"

type GraphTypeSpecificationBody = ElementTypeList

_GraphTypeSpecificationBody = Core.Name "openGql.grammar.GraphTypeSpecificationBody"

type ElementTypeList = [ElementTypeSpecification]

_ElementTypeList = Core.Name "openGql.grammar.ElementTypeList"

data ElementTypeSpecification = 
  ElementTypeSpecificationNodeType NodeTypeSpecification |
  ElementTypeSpecificationEdgeType EdgeTypeSpecification
  deriving (Eq, Ord, Read, Show)

_ElementTypeSpecification = Core.Name "openGql.grammar.ElementTypeSpecification"

_ElementTypeSpecification_nodeType = Core.Name "nodeType"

_ElementTypeSpecification_edgeType = Core.Name "edgeType"

data NodeTypeSpecification = 
  NodeTypeSpecificationPattern NodeTypePattern |
  NodeTypeSpecificationPhrase NodeTypePhrase
  deriving (Eq, Ord, Read, Show)

_NodeTypeSpecification = Core.Name "openGql.grammar.NodeTypeSpecification"

_NodeTypeSpecification_pattern = Core.Name "pattern"

_NodeTypeSpecification_phrase = Core.Name "phrase"

data NodeTypePattern = 
  NodeTypePattern {
    nodeTypePatternSynonymAndTypeName :: (Maybe NodeSynonymAndTypeName),
    nodeTypePatternAlias :: (Maybe LocalNodeTypeAlias),
    nodeTypePatternFiller :: (Maybe NodeTypeFiller)}
  deriving (Eq, Ord, Read, Show)

_NodeTypePattern = Core.Name "openGql.grammar.NodeTypePattern"

_NodeTypePattern_synonymAndTypeName = Core.Name "synonymAndTypeName"

_NodeTypePattern_alias = Core.Name "alias"

_NodeTypePattern_filler = Core.Name "filler"

data NodeSynonymAndTypeName = 
  NodeSynonymAndTypeName {
    nodeSynonymAndTypeNameNodeSynonym :: NodeSynonym,
    nodeSynonymAndTypeNameTypeName :: (Maybe NodeTypeName)}
  deriving (Eq, Ord, Read, Show)

_NodeSynonymAndTypeName = Core.Name "openGql.grammar.NodeSynonymAndTypeName"

_NodeSynonymAndTypeName_nodeSynonym = Core.Name "nodeSynonym"

_NodeSynonymAndTypeName_typeName = Core.Name "typeName"

data NodeTypePhrase = 
  NodeTypePhrase {
    nodeTypePhraseSynonym :: NodeSynonym,
    nodeTypePhraseTypePhraseFiller :: NodeTypePhraseFiller,
    nodeTypePhraseAlias :: (Maybe LocalNodeTypeAlias)}
  deriving (Eq, Ord, Read, Show)

_NodeTypePhrase = Core.Name "openGql.grammar.NodeTypePhrase"

_NodeTypePhrase_synonym = Core.Name "synonym"

_NodeTypePhrase_typePhraseFiller = Core.Name "typePhraseFiller"

_NodeTypePhrase_alias = Core.Name "alias"

data NodeTypePhraseFiller = 
  NodeTypePhraseFillerTypeName NodeTypeNameWithFiller |
  NodeTypePhraseFillerFillerOnly NodeTypeFiller
  deriving (Eq, Ord, Read, Show)

_NodeTypePhraseFiller = Core.Name "openGql.grammar.NodeTypePhraseFiller"

_NodeTypePhraseFiller_typeName = Core.Name "typeName"

_NodeTypePhraseFiller_fillerOnly = Core.Name "fillerOnly"

data NodeTypeNameWithFiller = 
  NodeTypeNameWithFiller {
    nodeTypeNameWithFillerTypeName :: NodeTypeName,
    nodeTypeNameWithFillerFiller :: (Maybe NodeTypeFiller)}
  deriving (Eq, Ord, Read, Show)

_NodeTypeNameWithFiller = Core.Name "openGql.grammar.NodeTypeNameWithFiller"

_NodeTypeNameWithFiller_typeName = Core.Name "typeName"

_NodeTypeNameWithFiller_filler = Core.Name "filler"

data NodeTypeFiller = 
  NodeTypeFillerKeyLabelSet NodeKeyLabelSetWithContent |
  NodeTypeFillerImpliedContent NodeTypeImpliedContent
  deriving (Eq, Ord, Read, Show)

_NodeTypeFiller = Core.Name "openGql.grammar.NodeTypeFiller"

_NodeTypeFiller_keyLabelSet = Core.Name "keyLabelSet"

_NodeTypeFiller_impliedContent = Core.Name "impliedContent"

data NodeKeyLabelSetWithContent = 
  NodeKeyLabelSetWithContent {
    nodeKeyLabelSetWithContentKeyLabelSet :: NodeTypeKeyLabelSet,
    nodeKeyLabelSetWithContentImpliedContent :: (Maybe NodeTypeImpliedContent)}
  deriving (Eq, Ord, Read, Show)

_NodeKeyLabelSetWithContent = Core.Name "openGql.grammar.NodeKeyLabelSetWithContent"

_NodeKeyLabelSetWithContent_keyLabelSet = Core.Name "keyLabelSet"

_NodeKeyLabelSetWithContent_impliedContent = Core.Name "impliedContent"

type LocalNodeTypeAlias = String

_LocalNodeTypeAlias = Core.Name "openGql.grammar.LocalNodeTypeAlias"

data NodeTypeImpliedContent = 
  NodeTypeImpliedContentLabelSet NodeTypeLabelSet |
  NodeTypeImpliedContentPropertyTypes NodeTypePropertyTypes |
  NodeTypeImpliedContentLabelSetWithProperties NodeLabelSetWithProperties
  deriving (Eq, Ord, Read, Show)

_NodeTypeImpliedContent = Core.Name "openGql.grammar.NodeTypeImpliedContent"

_NodeTypeImpliedContent_labelSet = Core.Name "labelSet"

_NodeTypeImpliedContent_propertyTypes = Core.Name "propertyTypes"

_NodeTypeImpliedContent_labelSetWithProperties = Core.Name "labelSetWithProperties"

data NodeLabelSetWithProperties = 
  NodeLabelSetWithProperties {
    nodeLabelSetWithPropertiesLabelSet :: NodeTypeLabelSet,
    nodeLabelSetWithPropertiesPropertyTypes :: NodeTypePropertyTypes}
  deriving (Eq, Ord, Read, Show)

_NodeLabelSetWithProperties = Core.Name "openGql.grammar.NodeLabelSetWithProperties"

_NodeLabelSetWithProperties_labelSet = Core.Name "labelSet"

_NodeLabelSetWithProperties_propertyTypes = Core.Name "propertyTypes"

type NodeTypeKeyLabelSet = (Maybe LabelSetPhrase)

_NodeTypeKeyLabelSet = Core.Name "openGql.grammar.NodeTypeKeyLabelSet"

type NodeTypeLabelSet = LabelSetPhrase

_NodeTypeLabelSet = Core.Name "openGql.grammar.NodeTypeLabelSet"

type NodeTypePropertyTypes = PropertyTypesSpecification

_NodeTypePropertyTypes = Core.Name "openGql.grammar.NodeTypePropertyTypes"

data EdgeTypeSpecification = 
  EdgeTypeSpecificationPattern EdgeTypePattern |
  EdgeTypeSpecificationPhrase EdgeTypePhrase
  deriving (Eq, Ord, Read, Show)

_EdgeTypeSpecification = Core.Name "openGql.grammar.EdgeTypeSpecification"

_EdgeTypeSpecification_pattern = Core.Name "pattern"

_EdgeTypeSpecification_phrase = Core.Name "phrase"

data EdgeTypePattern = 
  EdgeTypePattern {
    edgeTypePatternKindAndSynonym :: (Maybe EdgeKindAndSynonym),
    edgeTypePatternPatternType :: EdgeTypePatternType}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePattern = Core.Name "openGql.grammar.EdgeTypePattern"

_EdgeTypePattern_kindAndSynonym = Core.Name "kindAndSynonym"

_EdgeTypePattern_patternType = Core.Name "patternType"

data EdgeKindAndSynonym = 
  EdgeKindAndSynonym {
    edgeKindAndSynonymKind :: (Maybe EdgeKind),
    edgeKindAndSynonymSynonym :: EdgeSynonym,
    edgeKindAndSynonymTypeName :: (Maybe EdgeTypeName)}
  deriving (Eq, Ord, Read, Show)

_EdgeKindAndSynonym = Core.Name "openGql.grammar.EdgeKindAndSynonym"

_EdgeKindAndSynonym_kind = Core.Name "kind"

_EdgeKindAndSynonym_synonym = Core.Name "synonym"

_EdgeKindAndSynonym_typeName = Core.Name "typeName"

data EdgeTypePatternType = 
  EdgeTypePatternTypeDirected EdgeTypePatternDirected |
  EdgeTypePatternTypeUndirected EdgeTypePatternUndirected
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternType = Core.Name "openGql.grammar.EdgeTypePatternType"

_EdgeTypePatternType_directed = Core.Name "directed"

_EdgeTypePatternType_undirected = Core.Name "undirected"

data EdgeTypePhrase = 
  EdgeTypePhrase {
    edgeTypePhraseKind :: EdgeKind,
    edgeTypePhraseSynonym :: EdgeSynonym,
    edgeTypePhraseTypeNameAndFiller :: EdgeTypePhraseFiller,
    edgeTypePhraseEndpointPair :: EndpointPairPhrase}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePhrase = Core.Name "openGql.grammar.EdgeTypePhrase"

_EdgeTypePhrase_kind = Core.Name "kind"

_EdgeTypePhrase_synonym = Core.Name "synonym"

_EdgeTypePhrase_typeNameAndFiller = Core.Name "typeNameAndFiller"

_EdgeTypePhrase_endpointPair = Core.Name "endpointPair"

data EdgeTypePhraseFiller = 
  EdgeTypePhraseFillerTypeNameWithFiller EdgeTypeNameWithFiller |
  EdgeTypePhraseFillerFillerOnly EdgeTypeFiller
  deriving (Eq, Ord, Read, Show)

_EdgeTypePhraseFiller = Core.Name "openGql.grammar.EdgeTypePhraseFiller"

_EdgeTypePhraseFiller_typeNameWithFiller = Core.Name "typeNameWithFiller"

_EdgeTypePhraseFiller_fillerOnly = Core.Name "fillerOnly"

data EdgeTypeNameWithFiller = 
  EdgeTypeNameWithFiller {
    edgeTypeNameWithFillerTypeName :: EdgeTypeName,
    edgeTypeNameWithFillerFiller :: (Maybe EdgeTypeFiller)}
  deriving (Eq, Ord, Read, Show)

_EdgeTypeNameWithFiller = Core.Name "openGql.grammar.EdgeTypeNameWithFiller"

_EdgeTypeNameWithFiller_typeName = Core.Name "typeName"

_EdgeTypeNameWithFiller_filler = Core.Name "filler"

data EdgeTypeFiller = 
  EdgeTypeFillerKeyLabelSetWithContent EdgeKeyLabelSetWithContent |
  EdgeTypeFillerImpliedContent EdgeTypeImpliedContent
  deriving (Eq, Ord, Read, Show)

_EdgeTypeFiller = Core.Name "openGql.grammar.EdgeTypeFiller"

_EdgeTypeFiller_keyLabelSetWithContent = Core.Name "keyLabelSetWithContent"

_EdgeTypeFiller_impliedContent = Core.Name "impliedContent"

data EdgeKeyLabelSetWithContent = 
  EdgeKeyLabelSetWithContent {
    edgeKeyLabelSetWithContentKeyLabelSet :: EdgeTypeKeyLabelSet,
    edgeKeyLabelSetWithContentImpliedContent :: (Maybe EdgeTypeImpliedContent)}
  deriving (Eq, Ord, Read, Show)

_EdgeKeyLabelSetWithContent = Core.Name "openGql.grammar.EdgeKeyLabelSetWithContent"

_EdgeKeyLabelSetWithContent_keyLabelSet = Core.Name "keyLabelSet"

_EdgeKeyLabelSetWithContent_impliedContent = Core.Name "impliedContent"

data EdgeTypeImpliedContent = 
  EdgeTypeImpliedContentLabelSet EdgeTypeLabelSet |
  EdgeTypeImpliedContentPropertyTypes EdgeTypePropertyTypes |
  EdgeTypeImpliedContentLabelSetWithProperties EdgeLabelSetWithProperties
  deriving (Eq, Ord, Read, Show)

_EdgeTypeImpliedContent = Core.Name "openGql.grammar.EdgeTypeImpliedContent"

_EdgeTypeImpliedContent_labelSet = Core.Name "labelSet"

_EdgeTypeImpliedContent_propertyTypes = Core.Name "propertyTypes"

_EdgeTypeImpliedContent_labelSetWithProperties = Core.Name "labelSetWithProperties"

data EdgeLabelSetWithProperties = 
  EdgeLabelSetWithProperties {
    edgeLabelSetWithPropertiesLabelSet :: EdgeTypeLabelSet,
    edgeLabelSetWithPropertiesPropertyTypes :: EdgeTypePropertyTypes}
  deriving (Eq, Ord, Read, Show)

_EdgeLabelSetWithProperties = Core.Name "openGql.grammar.EdgeLabelSetWithProperties"

_EdgeLabelSetWithProperties_labelSet = Core.Name "labelSet"

_EdgeLabelSetWithProperties_propertyTypes = Core.Name "propertyTypes"

type EdgeTypeKeyLabelSet = (Maybe LabelSetPhrase)

_EdgeTypeKeyLabelSet = Core.Name "openGql.grammar.EdgeTypeKeyLabelSet"

type EdgeTypeLabelSet = LabelSetPhrase

_EdgeTypeLabelSet = Core.Name "openGql.grammar.EdgeTypeLabelSet"

type EdgeTypePropertyTypes = PropertyTypesSpecification

_EdgeTypePropertyTypes = Core.Name "openGql.grammar.EdgeTypePropertyTypes"

data EdgeTypePatternDirected = 
  EdgeTypePatternDirectedPointingRight EdgeTypePatternPointingRight |
  EdgeTypePatternDirectedPointingLeft EdgeTypePatternPointingLeft
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternDirected = Core.Name "openGql.grammar.EdgeTypePatternDirected"

_EdgeTypePatternDirected_pointingRight = Core.Name "pointingRight"

_EdgeTypePatternDirected_pointingLeft = Core.Name "pointingLeft"

data EdgeTypePatternPointingRight = 
  EdgeTypePatternPointingRight {
    edgeTypePatternPointingRightSource :: SourceNodeTypeReference,
    edgeTypePatternPointingRightArc :: ArcTypePointingRight,
    edgeTypePatternPointingRightDestination :: DestinationNodeTypeReference}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternPointingRight = Core.Name "openGql.grammar.EdgeTypePatternPointingRight"

_EdgeTypePatternPointingRight_source = Core.Name "source"

_EdgeTypePatternPointingRight_arc = Core.Name "arc"

_EdgeTypePatternPointingRight_destination = Core.Name "destination"

data EdgeTypePatternPointingLeft = 
  EdgeTypePatternPointingLeft {
    edgeTypePatternPointingLeftDestination :: DestinationNodeTypeReference,
    edgeTypePatternPointingLeftArc :: ArcTypePointingLeft,
    edgeTypePatternPointingLeftSource :: SourceNodeTypeReference}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternPointingLeft = Core.Name "openGql.grammar.EdgeTypePatternPointingLeft"

_EdgeTypePatternPointingLeft_destination = Core.Name "destination"

_EdgeTypePatternPointingLeft_arc = Core.Name "arc"

_EdgeTypePatternPointingLeft_source = Core.Name "source"

data EdgeTypePatternUndirected = 
  EdgeTypePatternUndirected {
    edgeTypePatternUndirectedSource :: SourceNodeTypeReference,
    edgeTypePatternUndirectedArc :: ArcTypeUndirected,
    edgeTypePatternUndirectedDestination :: DestinationNodeTypeReference}
  deriving (Eq, Ord, Read, Show)

_EdgeTypePatternUndirected = Core.Name "openGql.grammar.EdgeTypePatternUndirected"

_EdgeTypePatternUndirected_source = Core.Name "source"

_EdgeTypePatternUndirected_arc = Core.Name "arc"

_EdgeTypePatternUndirected_destination = Core.Name "destination"

type ArcTypePointingRight = EdgeTypeFiller

_ArcTypePointingRight = Core.Name "openGql.grammar.ArcTypePointingRight"

type ArcTypePointingLeft = EdgeTypeFiller

_ArcTypePointingLeft = Core.Name "openGql.grammar.ArcTypePointingLeft"

type ArcTypeUndirected = EdgeTypeFiller

_ArcTypeUndirected = Core.Name "openGql.grammar.ArcTypeUndirected"

data SourceNodeTypeReference = 
  SourceNodeTypeReferenceAlias SourceNodeTypeAlias |
  SourceNodeTypeReferenceFiller (Maybe NodeTypeFiller)
  deriving (Eq, Ord, Read, Show)

_SourceNodeTypeReference = Core.Name "openGql.grammar.SourceNodeTypeReference"

_SourceNodeTypeReference_alias = Core.Name "alias"

_SourceNodeTypeReference_filler = Core.Name "filler"

data DestinationNodeTypeReference = 
  DestinationNodeTypeReferenceAlias DestinationNodeTypeAlias |
  DestinationNodeTypeReferenceFiller (Maybe NodeTypeFiller)
  deriving (Eq, Ord, Read, Show)

_DestinationNodeTypeReference = Core.Name "openGql.grammar.DestinationNodeTypeReference"

_DestinationNodeTypeReference_alias = Core.Name "alias"

_DestinationNodeTypeReference_filler = Core.Name "filler"

data EdgeKind = 
  EdgeKindDirected  |
  EdgeKindUndirected 
  deriving (Eq, Ord, Read, Show)

_EdgeKind = Core.Name "openGql.grammar.EdgeKind"

_EdgeKind_directed = Core.Name "directed"

_EdgeKind_undirected = Core.Name "undirected"

type EndpointPairPhrase = EndpointPair

_EndpointPairPhrase = Core.Name "openGql.grammar.EndpointPairPhrase"

data EndpointPair = 
  EndpointPairDirectedPair EndpointPairDirected |
  EndpointPairUndirectedPair EndpointPairUndirected
  deriving (Eq, Ord, Read, Show)

_EndpointPair = Core.Name "openGql.grammar.EndpointPair"

_EndpointPair_directedPair = Core.Name "directedPair"

_EndpointPair_undirectedPair = Core.Name "undirectedPair"

data EndpointPairDirected = 
  EndpointPairDirectedPointingRight EndpointPairPointingRight |
  EndpointPairDirectedPointingLeft EndpointPairPointingLeft
  deriving (Eq, Ord, Read, Show)

_EndpointPairDirected = Core.Name "openGql.grammar.EndpointPairDirected"

_EndpointPairDirected_pointingRight = Core.Name "pointingRight"

_EndpointPairDirected_pointingLeft = Core.Name "pointingLeft"

data EndpointPairPointingRight = 
  EndpointPairPointingRight {
    endpointPairPointingRightSourceAlias :: SourceNodeTypeAlias,
    endpointPairPointingRightConnector :: ConnectorPointingRight,
    endpointPairPointingRightDestinationAlias :: DestinationNodeTypeAlias}
  deriving (Eq, Ord, Read, Show)

_EndpointPairPointingRight = Core.Name "openGql.grammar.EndpointPairPointingRight"

_EndpointPairPointingRight_sourceAlias = Core.Name "sourceAlias"

_EndpointPairPointingRight_connector = Core.Name "connector"

_EndpointPairPointingRight_destinationAlias = Core.Name "destinationAlias"

data EndpointPairPointingLeft = 
  EndpointPairPointingLeft {
    endpointPairPointingLeftDestinationAlias :: DestinationNodeTypeAlias,
    endpointPairPointingLeftSourceAlias :: SourceNodeTypeAlias}
  deriving (Eq, Ord, Read, Show)

_EndpointPairPointingLeft = Core.Name "openGql.grammar.EndpointPairPointingLeft"

_EndpointPairPointingLeft_destinationAlias = Core.Name "destinationAlias"

_EndpointPairPointingLeft_sourceAlias = Core.Name "sourceAlias"

data EndpointPairUndirected = 
  EndpointPairUndirected {
    endpointPairUndirectedSourceAlias :: SourceNodeTypeAlias,
    endpointPairUndirectedConnector :: ConnectorUndirected,
    endpointPairUndirectedDestinationAlias :: DestinationNodeTypeAlias}
  deriving (Eq, Ord, Read, Show)

_EndpointPairUndirected = Core.Name "openGql.grammar.EndpointPairUndirected"

_EndpointPairUndirected_sourceAlias = Core.Name "sourceAlias"

_EndpointPairUndirected_connector = Core.Name "connector"

_EndpointPairUndirected_destinationAlias = Core.Name "destinationAlias"

data ConnectorPointingRight = 
  ConnectorPointingRightTo  |
  ConnectorPointingRightRightArrow 
  deriving (Eq, Ord, Read, Show)

_ConnectorPointingRight = Core.Name "openGql.grammar.ConnectorPointingRight"

_ConnectorPointingRight_to = Core.Name "to"

_ConnectorPointingRight_rightArrow = Core.Name "rightArrow"

data ConnectorUndirected = 
  ConnectorUndirectedTo  |
  ConnectorUndirectedTilde 
  deriving (Eq, Ord, Read, Show)

_ConnectorUndirected = Core.Name "openGql.grammar.ConnectorUndirected"

_ConnectorUndirected_to = Core.Name "to"

_ConnectorUndirected_tilde = Core.Name "tilde"

type SourceNodeTypeAlias = String

_SourceNodeTypeAlias = Core.Name "openGql.grammar.SourceNodeTypeAlias"

type DestinationNodeTypeAlias = String

_DestinationNodeTypeAlias = Core.Name "openGql.grammar.DestinationNodeTypeAlias"

data LabelSetPhrase = 
  LabelSetPhraseSingleLabel LabelName |
  LabelSetPhraseMultipleLabels LabelSetSpecification |
  LabelSetPhraseIsOrColonWithLabels IsOrColonWithLabels
  deriving (Eq, Ord, Read, Show)

_LabelSetPhrase = Core.Name "openGql.grammar.LabelSetPhrase"

_LabelSetPhrase_singleLabel = Core.Name "singleLabel"

_LabelSetPhrase_multipleLabels = Core.Name "multipleLabels"

_LabelSetPhrase_isOrColonWithLabels = Core.Name "isOrColonWithLabels"

data IsOrColonWithLabels = 
  IsOrColonWithLabels {
    isOrColonWithLabelsIsOrColon :: IsOrColon,
    isOrColonWithLabelsLabels :: LabelSetSpecification}
  deriving (Eq, Ord, Read, Show)

_IsOrColonWithLabels = Core.Name "openGql.grammar.IsOrColonWithLabels"

_IsOrColonWithLabels_isOrColon = Core.Name "isOrColon"

_IsOrColonWithLabels_labels = Core.Name "labels"

type LabelSetSpecification = [LabelName]

_LabelSetSpecification = Core.Name "openGql.grammar.LabelSetSpecification"

type PropertyTypesSpecification = (Maybe PropertyTypeList)

_PropertyTypesSpecification = Core.Name "openGql.grammar.PropertyTypesSpecification"

type PropertyTypeList = [PropertyType]

_PropertyTypeList = Core.Name "openGql.grammar.PropertyTypeList"

data PropertyType = 
  PropertyType {
    propertyTypeName :: PropertyName,
    propertyTypeTyped :: (Maybe Typed),
    propertyTypeValueType :: PropertyValueType}
  deriving (Eq, Ord, Read, Show)

_PropertyType = Core.Name "openGql.grammar.PropertyType"

_PropertyType_name = Core.Name "name"

_PropertyType_typed = Core.Name "typed"

_PropertyType_valueType = Core.Name "valueType"

type PropertyValueType = ValueType

_PropertyValueType = Core.Name "openGql.grammar.PropertyValueType"

data BindingTableType = 
  BindingTableType {
    bindingTableTypeBinding :: Bool,
    bindingTableTypeFieldTypes :: FieldTypesSpecification}
  deriving (Eq, Ord, Read, Show)

_BindingTableType = Core.Name "openGql.grammar.BindingTableType"

_BindingTableType_binding = Core.Name "binding"

_BindingTableType_fieldTypes = Core.Name "fieldTypes"

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

_ValueType = Core.Name "openGql.grammar.ValueType"

_ValueType_predefinedType = Core.Name "predefinedType"

_ValueType_pathValueType = Core.Name "pathValueType"

_ValueType_listValueTypeAlt1 = Core.Name "listValueTypeAlt1"

_ValueType_listValueTypeAlt2 = Core.Name "listValueTypeAlt2"

_ValueType_listValueTypeAlt3 = Core.Name "listValueTypeAlt3"

_ValueType_recordType = Core.Name "recordType"

_ValueType_openDynamicUnionType = Core.Name "openDynamicUnionType"

_ValueType_dynamicPropertyValueType = Core.Name "dynamicPropertyValueType"

_ValueType_closedDynamicUnionTypeAlt1 = Core.Name "closedDynamicUnionTypeAlt1"

_ValueType_closedDynamicUnionTypeAlt2 = Core.Name "closedDynamicUnionTypeAlt2"

data ListValueTypeAlt1 = 
  ListValueTypeAlt1 {
    listValueTypeAlt1TypeName :: ListValueTypeName,
    listValueTypeAlt1ValueType :: ValueType,
    listValueTypeAlt1MaxLength :: (Maybe MaxLength),
    listValueTypeAlt1NotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeAlt1 = Core.Name "openGql.grammar.ListValueTypeAlt1"

_ListValueTypeAlt1_typeName = Core.Name "typeName"

_ListValueTypeAlt1_valueType = Core.Name "valueType"

_ListValueTypeAlt1_maxLength = Core.Name "maxLength"

_ListValueTypeAlt1_notNull = Core.Name "notNull"

data ListValueTypeAlt2 = 
  ListValueTypeAlt2 {
    listValueTypeAlt2ValueType :: ValueType,
    listValueTypeAlt2TypeName :: ListValueTypeName,
    listValueTypeAlt2MaxLength :: (Maybe MaxLength),
    listValueTypeAlt2NotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeAlt2 = Core.Name "openGql.grammar.ListValueTypeAlt2"

_ListValueTypeAlt2_valueType = Core.Name "valueType"

_ListValueTypeAlt2_typeName = Core.Name "typeName"

_ListValueTypeAlt2_maxLength = Core.Name "maxLength"

_ListValueTypeAlt2_notNull = Core.Name "notNull"

data ListValueTypeAlt3 = 
  ListValueTypeAlt3 {
    listValueTypeAlt3TypeName :: ListValueTypeName,
    listValueTypeAlt3MaxLength :: (Maybe MaxLength),
    listValueTypeAlt3NotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeAlt3 = Core.Name "openGql.grammar.ListValueTypeAlt3"

_ListValueTypeAlt3_typeName = Core.Name "typeName"

_ListValueTypeAlt3_maxLength = Core.Name "maxLength"

_ListValueTypeAlt3_notNull = Core.Name "notNull"

data OpenDynamicUnionType = 
  OpenDynamicUnionType {
    openDynamicUnionTypeValue :: Bool,
    openDynamicUnionTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenDynamicUnionType = Core.Name "openGql.grammar.OpenDynamicUnionType"

_OpenDynamicUnionType_value = Core.Name "value"

_OpenDynamicUnionType_notNull = Core.Name "notNull"

data DynamicPropertyValueType = 
  DynamicPropertyValueType {
    dynamicPropertyValueTypeAny :: (Maybe Bool),
    dynamicPropertyValueTypeProperty :: (),
    dynamicPropertyValueTypeValue :: (),
    dynamicPropertyValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_DynamicPropertyValueType = Core.Name "openGql.grammar.DynamicPropertyValueType"

_DynamicPropertyValueType_any = Core.Name "any"

_DynamicPropertyValueType_property = Core.Name "property"

_DynamicPropertyValueType_value = Core.Name "value"

_DynamicPropertyValueType_notNull = Core.Name "notNull"

data ClosedDynamicUnionTypeAlt1 = 
  ClosedDynamicUnionTypeAlt1 {
    closedDynamicUnionTypeAlt1AnyValue :: (Maybe Bool),
    closedDynamicUnionTypeAlt1ValueTypes :: [ValueType]}
  deriving (Eq, Ord, Read, Show)

_ClosedDynamicUnionTypeAlt1 = Core.Name "openGql.grammar.ClosedDynamicUnionTypeAlt1"

_ClosedDynamicUnionTypeAlt1_anyValue = Core.Name "anyValue"

_ClosedDynamicUnionTypeAlt1_valueTypes = Core.Name "valueTypes"

data ClosedDynamicUnionTypeAlt2 = 
  ClosedDynamicUnionTypeAlt2 {
    closedDynamicUnionTypeAlt2ValueTypes :: [ValueType]}
  deriving (Eq, Ord, Read, Show)

_ClosedDynamicUnionTypeAlt2 = Core.Name "openGql.grammar.ClosedDynamicUnionTypeAlt2"

_ClosedDynamicUnionTypeAlt2_valueTypes = Core.Name "valueTypes"

type Typed = ()

_Typed = Core.Name "openGql.grammar.Typed"

data PredefinedType = 
  PredefinedTypeBooleanType BooleanType |
  PredefinedTypeCharacterStringType CharacterStringType |
  PredefinedTypeByteStringType ByteStringType |
  PredefinedTypeNumericType NumericType |
  PredefinedTypeTemporalType TemporalType |
  PredefinedTypeReferenceValueType ReferenceValueType |
  PredefinedTypeImmaterialValueType ImmaterialValueType
  deriving (Eq, Ord, Read, Show)

_PredefinedType = Core.Name "openGql.grammar.PredefinedType"

_PredefinedType_booleanType = Core.Name "booleanType"

_PredefinedType_characterStringType = Core.Name "characterStringType"

_PredefinedType_byteStringType = Core.Name "byteStringType"

_PredefinedType_numericType = Core.Name "numericType"

_PredefinedType_temporalType = Core.Name "temporalType"

_PredefinedType_referenceValueType = Core.Name "referenceValueType"

_PredefinedType_immaterialValueType = Core.Name "immaterialValueType"

data BooleanType = 
  BooleanType {
    booleanTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BooleanType = Core.Name "openGql.grammar.BooleanType"

_BooleanType_notNull = Core.Name "notNull"

data CharacterStringType = 
  CharacterStringTypeStringType StringType |
  CharacterStringTypeCharType CharType |
  CharacterStringTypeVarcharType VarcharType
  deriving (Eq, Ord, Read, Show)

_CharacterStringType = Core.Name "openGql.grammar.CharacterStringType"

_CharacterStringType_stringType = Core.Name "stringType"

_CharacterStringType_charType = Core.Name "charType"

_CharacterStringType_varcharType = Core.Name "varcharType"

data StringType = 
  StringType {
    stringTypeMinLength :: (Maybe MinLength),
    stringTypeMaxLength :: (Maybe MaxLength),
    stringTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringType = Core.Name "openGql.grammar.StringType"

_StringType_minLength = Core.Name "minLength"

_StringType_maxLength = Core.Name "maxLength"

_StringType_notNull = Core.Name "notNull"

data CharType = 
  CharType {
    charTypeFixedLength :: (Maybe FixedLength),
    charTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_CharType = Core.Name "openGql.grammar.CharType"

_CharType_fixedLength = Core.Name "fixedLength"

_CharType_notNull = Core.Name "notNull"

data VarcharType = 
  VarcharType {
    varcharTypeMaxLength :: (Maybe MaxLength),
    varcharTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_VarcharType = Core.Name "openGql.grammar.VarcharType"

_VarcharType_maxLength = Core.Name "maxLength"

_VarcharType_notNull = Core.Name "notNull"

data ByteStringType = 
  ByteStringTypeBytesType BytesType |
  ByteStringTypeBinaryType BinaryType |
  ByteStringTypeVarbinaryType VarbinaryType
  deriving (Eq, Ord, Read, Show)

_ByteStringType = Core.Name "openGql.grammar.ByteStringType"

_ByteStringType_bytesType = Core.Name "bytesType"

_ByteStringType_binaryType = Core.Name "binaryType"

_ByteStringType_varbinaryType = Core.Name "varbinaryType"

data BytesType = 
  BytesType {
    bytesTypeMinLength :: (Maybe MinLength),
    bytesTypeMaxLength :: (Maybe MaxLength),
    bytesTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BytesType = Core.Name "openGql.grammar.BytesType"

_BytesType_minLength = Core.Name "minLength"

_BytesType_maxLength = Core.Name "maxLength"

_BytesType_notNull = Core.Name "notNull"

data BinaryType = 
  BinaryType {
    binaryTypeFixedLength :: (Maybe FixedLength),
    binaryTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BinaryType = Core.Name "openGql.grammar.BinaryType"

_BinaryType_fixedLength = Core.Name "fixedLength"

_BinaryType_notNull = Core.Name "notNull"

data VarbinaryType = 
  VarbinaryType {
    varbinaryTypeMaxLength :: (Maybe MaxLength),
    varbinaryTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_VarbinaryType = Core.Name "openGql.grammar.VarbinaryType"

_VarbinaryType_maxLength = Core.Name "maxLength"

_VarbinaryType_notNull = Core.Name "notNull"

type MinLength = UnsignedInteger

_MinLength = Core.Name "openGql.grammar.MinLength"

type FixedLength = UnsignedInteger

_FixedLength = Core.Name "openGql.grammar.FixedLength"

type MaxLength = UnsignedInteger

_MaxLength = Core.Name "openGql.grammar.MaxLength"

data NumericType = 
  NumericTypeExact ExactNumericType |
  NumericTypeApproximate ApproximateNumericType
  deriving (Eq, Ord, Read, Show)

_NumericType = Core.Name "openGql.grammar.NumericType"

_NumericType_exact = Core.Name "exact"

_NumericType_approximate = Core.Name "approximate"

data ExactNumericType = 
  ExactNumericTypeBinary BinaryExactNumericType |
  ExactNumericTypeDecimal DecimalExactNumericType
  deriving (Eq, Ord, Read, Show)

_ExactNumericType = Core.Name "openGql.grammar.ExactNumericType"

_ExactNumericType_binary = Core.Name "binary"

_ExactNumericType_decimal = Core.Name "decimal"

data BinaryExactNumericType = 
  BinaryExactNumericTypeSigned SignedBinaryExactNumericType |
  BinaryExactNumericTypeUnsigned UnsignedBinaryExactNumericType
  deriving (Eq, Ord, Read, Show)

_BinaryExactNumericType = Core.Name "openGql.grammar.BinaryExactNumericType"

_BinaryExactNumericType_signed = Core.Name "signed"

_BinaryExactNumericType_unsigned = Core.Name "unsigned"

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

_SignedBinaryExactNumericType = Core.Name "openGql.grammar.SignedBinaryExactNumericType"

_SignedBinaryExactNumericType_int8 = Core.Name "int8"

_SignedBinaryExactNumericType_int16 = Core.Name "int16"

_SignedBinaryExactNumericType_int32 = Core.Name "int32"

_SignedBinaryExactNumericType_int64 = Core.Name "int64"

_SignedBinaryExactNumericType_int128 = Core.Name "int128"

_SignedBinaryExactNumericType_int256 = Core.Name "int256"

_SignedBinaryExactNumericType_smallInt = Core.Name "smallInt"

_SignedBinaryExactNumericType_intWithPrecision = Core.Name "intWithPrecision"

_SignedBinaryExactNumericType_bigInt = Core.Name "bigInt"

_SignedBinaryExactNumericType_signedVerboseType = Core.Name "signedVerboseType"

data Int8Type = 
  Int8Type {
    int8TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int8Type = Core.Name "openGql.grammar.Int8Type"

_Int8Type_notNull = Core.Name "notNull"

data Int16Type = 
  Int16Type {
    int16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int16Type = Core.Name "openGql.grammar.Int16Type"

_Int16Type_notNull = Core.Name "notNull"

data Int32Type = 
  Int32Type {
    int32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int32Type = Core.Name "openGql.grammar.Int32Type"

_Int32Type_notNull = Core.Name "notNull"

data Int64Type = 
  Int64Type {
    int64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int64Type = Core.Name "openGql.grammar.Int64Type"

_Int64Type_notNull = Core.Name "notNull"

data Int128Type = 
  Int128Type {
    int128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int128Type = Core.Name "openGql.grammar.Int128Type"

_Int128Type_notNull = Core.Name "notNull"

data Int256Type = 
  Int256Type {
    int256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Int256Type = Core.Name "openGql.grammar.Int256Type"

_Int256Type_notNull = Core.Name "notNull"

data SmallIntType = 
  SmallIntType {
    smallIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_SmallIntType = Core.Name "openGql.grammar.SmallIntType"

_SmallIntType_notNull = Core.Name "notNull"

data BigIntType = 
  BigIntType {
    bigIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BigIntType = Core.Name "openGql.grammar.BigIntType"

_BigIntType_notNull = Core.Name "notNull"

data IntWithPrecision = 
  IntWithPrecision {
    intWithPrecisionPrecision :: (Maybe Precision),
    intWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_IntWithPrecision = Core.Name "openGql.grammar.IntWithPrecision"

_IntWithPrecision_precision = Core.Name "precision"

_IntWithPrecision_notNull = Core.Name "notNull"

data SignedVerboseBinaryExactNumericType = 
  SignedVerboseBinaryExactNumericType {
    signedVerboseBinaryExactNumericTypeSigned :: Bool,
    signedVerboseBinaryExactNumericTypeVerboseType :: VerboseBinaryExactNumericType}
  deriving (Eq, Ord, Read, Show)

_SignedVerboseBinaryExactNumericType = Core.Name "openGql.grammar.SignedVerboseBinaryExactNumericType"

_SignedVerboseBinaryExactNumericType_signed = Core.Name "signed"

_SignedVerboseBinaryExactNumericType_verboseType = Core.Name "verboseType"

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

_UnsignedBinaryExactNumericType = Core.Name "openGql.grammar.UnsignedBinaryExactNumericType"

_UnsignedBinaryExactNumericType_uint8 = Core.Name "uint8"

_UnsignedBinaryExactNumericType_uint16 = Core.Name "uint16"

_UnsignedBinaryExactNumericType_uint32 = Core.Name "uint32"

_UnsignedBinaryExactNumericType_uint64 = Core.Name "uint64"

_UnsignedBinaryExactNumericType_uint128 = Core.Name "uint128"

_UnsignedBinaryExactNumericType_uint256 = Core.Name "uint256"

_UnsignedBinaryExactNumericType_uSmallInt = Core.Name "uSmallInt"

_UnsignedBinaryExactNumericType_uintWithPrecision = Core.Name "uintWithPrecision"

_UnsignedBinaryExactNumericType_uBigInt = Core.Name "uBigInt"

_UnsignedBinaryExactNumericType_unsigned = Core.Name "unsigned"

data Uint8Type = 
  Uint8Type {
    uint8TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint8Type = Core.Name "openGql.grammar.Uint8Type"

_Uint8Type_notNull = Core.Name "notNull"

data Uint16Type = 
  Uint16Type {
    uint16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint16Type = Core.Name "openGql.grammar.Uint16Type"

_Uint16Type_notNull = Core.Name "notNull"

data Uint32Type = 
  Uint32Type {
    uint32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint32Type = Core.Name "openGql.grammar.Uint32Type"

_Uint32Type_notNull = Core.Name "notNull"

data Uint64Type = 
  Uint64Type {
    uint64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint64Type = Core.Name "openGql.grammar.Uint64Type"

_Uint64Type_notNull = Core.Name "notNull"

data Uint128Type = 
  Uint128Type {
    uint128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint128Type = Core.Name "openGql.grammar.Uint128Type"

_Uint128Type_notNull = Core.Name "notNull"

data Uint256Type = 
  Uint256Type {
    uint256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Uint256Type = Core.Name "openGql.grammar.Uint256Type"

_Uint256Type_notNull = Core.Name "notNull"

data USmallIntType = 
  USmallIntType {
    uSmallIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_USmallIntType = Core.Name "openGql.grammar.USmallIntType"

_USmallIntType_notNull = Core.Name "notNull"

data UBigIntType = 
  UBigIntType {
    uBigIntTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_UBigIntType = Core.Name "openGql.grammar.UBigIntType"

_UBigIntType_notNull = Core.Name "notNull"

data UintWithPrecision = 
  UintWithPrecision {
    uintWithPrecisionPrecision :: (Maybe Precision),
    uintWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_UintWithPrecision = Core.Name "openGql.grammar.UintWithPrecision"

_UintWithPrecision_precision = Core.Name "precision"

_UintWithPrecision_notNull = Core.Name "notNull"

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

_VerboseBinaryExactNumericType = Core.Name "openGql.grammar.VerboseBinaryExactNumericType"

_VerboseBinaryExactNumericType_integer8 = Core.Name "integer8"

_VerboseBinaryExactNumericType_integer16 = Core.Name "integer16"

_VerboseBinaryExactNumericType_integer32 = Core.Name "integer32"

_VerboseBinaryExactNumericType_integer64 = Core.Name "integer64"

_VerboseBinaryExactNumericType_integer128 = Core.Name "integer128"

_VerboseBinaryExactNumericType_integer256 = Core.Name "integer256"

_VerboseBinaryExactNumericType_smallInteger = Core.Name "smallInteger"

_VerboseBinaryExactNumericType_integerWithPrecision = Core.Name "integerWithPrecision"

_VerboseBinaryExactNumericType_bigInteger = Core.Name "bigInteger"

data Integer8Type = 
  Integer8Type {
    integer8TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer8Type = Core.Name "openGql.grammar.Integer8Type"

_Integer8Type_notNull = Core.Name "notNull"

data Integer16Type = 
  Integer16Type {
    integer16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer16Type = Core.Name "openGql.grammar.Integer16Type"

_Integer16Type_notNull = Core.Name "notNull"

data Integer32Type = 
  Integer32Type {
    integer32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer32Type = Core.Name "openGql.grammar.Integer32Type"

_Integer32Type_notNull = Core.Name "notNull"

data Integer64Type = 
  Integer64Type {
    integer64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer64Type = Core.Name "openGql.grammar.Integer64Type"

_Integer64Type_notNull = Core.Name "notNull"

data Integer128Type = 
  Integer128Type {
    integer128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer128Type = Core.Name "openGql.grammar.Integer128Type"

_Integer128Type_notNull = Core.Name "notNull"

data Integer256Type = 
  Integer256Type {
    integer256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Integer256Type = Core.Name "openGql.grammar.Integer256Type"

_Integer256Type_notNull = Core.Name "notNull"

data SmallIntegerType = 
  SmallIntegerType {
    smallIntegerTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_SmallIntegerType = Core.Name "openGql.grammar.SmallIntegerType"

_SmallIntegerType_notNull = Core.Name "notNull"

data BigIntegerType = 
  BigIntegerType {
    bigIntegerTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BigIntegerType = Core.Name "openGql.grammar.BigIntegerType"

_BigIntegerType_notNull = Core.Name "notNull"

data IntegerWithPrecision = 
  IntegerWithPrecision {
    integerWithPrecisionPrecision :: (Maybe Precision),
    integerWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_IntegerWithPrecision = Core.Name "openGql.grammar.IntegerWithPrecision"

_IntegerWithPrecision_precision = Core.Name "precision"

_IntegerWithPrecision_notNull = Core.Name "notNull"

type Precision = UnsignedDecimalInteger

_Precision = Core.Name "openGql.grammar.Precision"

type DecimalExactNumericType = (Maybe PrecisionAndScale)

_DecimalExactNumericType = Core.Name "openGql.grammar.DecimalExactNumericType"

data PrecisionAndScale = 
  PrecisionAndScale {
    precisionAndScalePrecision :: Precision,
    precisionAndScaleScale :: (Maybe Scale),
    precisionAndScaleNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_PrecisionAndScale = Core.Name "openGql.grammar.PrecisionAndScale"

_PrecisionAndScale_precision = Core.Name "precision"

_PrecisionAndScale_scale = Core.Name "scale"

_PrecisionAndScale_notNull = Core.Name "notNull"

type Scale = UnsignedDecimalInteger

_Scale = Core.Name "openGql.grammar.Scale"

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

_ApproximateNumericType = Core.Name "openGql.grammar.ApproximateNumericType"

_ApproximateNumericType_float16 = Core.Name "float16"

_ApproximateNumericType_float32 = Core.Name "float32"

_ApproximateNumericType_float64 = Core.Name "float64"

_ApproximateNumericType_float128 = Core.Name "float128"

_ApproximateNumericType_float256 = Core.Name "float256"

_ApproximateNumericType_floatWithPrecision = Core.Name "floatWithPrecision"

_ApproximateNumericType_real = Core.Name "real"

_ApproximateNumericType_doubleWithPrecision = Core.Name "doubleWithPrecision"

data Float16Type = 
  Float16Type {
    float16TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float16Type = Core.Name "openGql.grammar.Float16Type"

_Float16Type_notNull = Core.Name "notNull"

data Float32Type = 
  Float32Type {
    float32TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float32Type = Core.Name "openGql.grammar.Float32Type"

_Float32Type_notNull = Core.Name "notNull"

data Float64Type = 
  Float64Type {
    float64TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float64Type = Core.Name "openGql.grammar.Float64Type"

_Float64Type_notNull = Core.Name "notNull"

data Float128Type = 
  Float128Type {
    float128TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float128Type = Core.Name "openGql.grammar.Float128Type"

_Float128Type_notNull = Core.Name "notNull"

data Float256Type = 
  Float256Type {
    float256TypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_Float256Type = Core.Name "openGql.grammar.Float256Type"

_Float256Type_notNull = Core.Name "notNull"

data FloatTypeWithPrecision = 
  FloatTypeWithPrecision {
    floatTypeWithPrecisionPrecisionAndScale :: (Maybe PrecisionAndScale),
    floatTypeWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_FloatTypeWithPrecision = Core.Name "openGql.grammar.FloatTypeWithPrecision"

_FloatTypeWithPrecision_precisionAndScale = Core.Name "precisionAndScale"

_FloatTypeWithPrecision_notNull = Core.Name "notNull"

data RealType = 
  RealType {
    realTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_RealType = Core.Name "openGql.grammar.RealType"

_RealType_notNull = Core.Name "notNull"

data DoubleTypeWithPrecision = 
  DoubleTypeWithPrecision {
    doubleTypeWithPrecisionPrecision :: Bool,
    doubleTypeWithPrecisionNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_DoubleTypeWithPrecision = Core.Name "openGql.grammar.DoubleTypeWithPrecision"

_DoubleTypeWithPrecision_precision = Core.Name "precision"

_DoubleTypeWithPrecision_notNull = Core.Name "notNull"

data TemporalType = 
  TemporalTypeInstantType TemporalInstantType |
  TemporalTypeDurationType TemporalDurationType
  deriving (Eq, Ord, Read, Show)

_TemporalType = Core.Name "openGql.grammar.TemporalType"

_TemporalType_instantType = Core.Name "instantType"

_TemporalType_durationType = Core.Name "durationType"

data TemporalInstantType = 
  TemporalInstantTypeDatetimeType DatetimeType |
  TemporalInstantTypeLocaldatetimeType LocaldatetimeType |
  TemporalInstantTypeDateType DateType |
  TemporalInstantTypeTimeType TimeType |
  TemporalInstantTypeLocaltimeType LocaltimeType
  deriving (Eq, Ord, Read, Show)

_TemporalInstantType = Core.Name "openGql.grammar.TemporalInstantType"

_TemporalInstantType_datetimeType = Core.Name "datetimeType"

_TemporalInstantType_localdatetimeType = Core.Name "localdatetimeType"

_TemporalInstantType_dateType = Core.Name "dateType"

_TemporalInstantType_timeType = Core.Name "timeType"

_TemporalInstantType_localtimeType = Core.Name "localtimeType"

data DatetimeType = 
  DatetimeTypeZonedDatetime ZonedDatetimeType |
  DatetimeTypeTimestampWithTimeZone TimestampWithTimeZoneType
  deriving (Eq, Ord, Read, Show)

_DatetimeType = Core.Name "openGql.grammar.DatetimeType"

_DatetimeType_zonedDatetime = Core.Name "zonedDatetime"

_DatetimeType_timestampWithTimeZone = Core.Name "timestampWithTimeZone"

data ZonedDatetimeType = 
  ZonedDatetimeType {
    zonedDatetimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ZonedDatetimeType = Core.Name "openGql.grammar.ZonedDatetimeType"

_ZonedDatetimeType_notNull = Core.Name "notNull"

data TimestampWithTimeZoneType = 
  TimestampWithTimeZoneType {
    timestampWithTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimestampWithTimeZoneType = Core.Name "openGql.grammar.TimestampWithTimeZoneType"

_TimestampWithTimeZoneType_notNull = Core.Name "notNull"

data LocaldatetimeType = 
  LocaldatetimeTypeLocalDatetime LocalDatetimeType |
  LocaldatetimeTypeTimestampWithoutTimeZone TimestampWithoutTimeZoneType
  deriving (Eq, Ord, Read, Show)

_LocaldatetimeType = Core.Name "openGql.grammar.LocaldatetimeType"

_LocaldatetimeType_localDatetime = Core.Name "localDatetime"

_LocaldatetimeType_timestampWithoutTimeZone = Core.Name "timestampWithoutTimeZone"

data LocalDatetimeType = 
  LocalDatetimeType {
    localDatetimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_LocalDatetimeType = Core.Name "openGql.grammar.LocalDatetimeType"

_LocalDatetimeType_notNull = Core.Name "notNull"

data TimestampWithoutTimeZoneType = 
  TimestampWithoutTimeZoneType {
    timestampWithoutTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimestampWithoutTimeZoneType = Core.Name "openGql.grammar.TimestampWithoutTimeZoneType"

_TimestampWithoutTimeZoneType_notNull = Core.Name "notNull"

data DateType = 
  DateType {
    dateTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_DateType = Core.Name "openGql.grammar.DateType"

_DateType_notNull = Core.Name "notNull"

data TimeType = 
  TimeTypeZonedTime ZonedTimeType |
  TimeTypeTimeWithTimeZone TimeWithTimeZoneType
  deriving (Eq, Ord, Read, Show)

_TimeType = Core.Name "openGql.grammar.TimeType"

_TimeType_zonedTime = Core.Name "zonedTime"

_TimeType_timeWithTimeZone = Core.Name "timeWithTimeZone"

data ZonedTimeType = 
  ZonedTimeType {
    zonedTimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ZonedTimeType = Core.Name "openGql.grammar.ZonedTimeType"

_ZonedTimeType_notNull = Core.Name "notNull"

data TimeWithTimeZoneType = 
  TimeWithTimeZoneType {
    timeWithTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimeWithTimeZoneType = Core.Name "openGql.grammar.TimeWithTimeZoneType"

_TimeWithTimeZoneType_notNull = Core.Name "notNull"

data LocaltimeType = 
  LocaltimeTypeLocalTime LocalTimeType |
  LocaltimeTypeTimeWithoutTimeZone TimeWithoutTimeZoneType
  deriving (Eq, Ord, Read, Show)

_LocaltimeType = Core.Name "openGql.grammar.LocaltimeType"

_LocaltimeType_localTime = Core.Name "localTime"

_LocaltimeType_timeWithoutTimeZone = Core.Name "timeWithoutTimeZone"

data LocalTimeType = 
  LocalTimeType {
    localTimeTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_LocalTimeType = Core.Name "openGql.grammar.LocalTimeType"

_LocalTimeType_notNull = Core.Name "notNull"

data TimeWithoutTimeZoneType = 
  TimeWithoutTimeZoneType {
    timeWithoutTimeZoneTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TimeWithoutTimeZoneType = Core.Name "openGql.grammar.TimeWithoutTimeZoneType"

_TimeWithoutTimeZoneType_notNull = Core.Name "notNull"

data TemporalDurationType = 
  TemporalDurationType {
    temporalDurationTypeQualifier :: TemporalDurationQualifier,
    temporalDurationTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_TemporalDurationType = Core.Name "openGql.grammar.TemporalDurationType"

_TemporalDurationType_qualifier = Core.Name "qualifier"

_TemporalDurationType_notNull = Core.Name "notNull"

data TemporalDurationQualifier = 
  TemporalDurationQualifierYearToMonth  |
  TemporalDurationQualifierDayToSecond 
  deriving (Eq, Ord, Read, Show)

_TemporalDurationQualifier = Core.Name "openGql.grammar.TemporalDurationQualifier"

_TemporalDurationQualifier_yearToMonth = Core.Name "yearToMonth"

_TemporalDurationQualifier_dayToSecond = Core.Name "dayToSecond"

data ReferenceValueType = 
  ReferenceValueTypeGraph GraphReferenceValueType |
  ReferenceValueTypeBindingTable BindingTableReferenceValueType |
  ReferenceValueTypeNode NodeReferenceValueType |
  ReferenceValueTypeEdge EdgeReferenceValueType
  deriving (Eq, Ord, Read, Show)

_ReferenceValueType = Core.Name "openGql.grammar.ReferenceValueType"

_ReferenceValueType_graph = Core.Name "graph"

_ReferenceValueType_bindingTable = Core.Name "bindingTable"

_ReferenceValueType_node = Core.Name "node"

_ReferenceValueType_edge = Core.Name "edge"

data ImmaterialValueType = 
  ImmaterialValueTypeNullType NullType |
  ImmaterialValueTypeEmptyType EmptyType
  deriving (Eq, Ord, Read, Show)

_ImmaterialValueType = Core.Name "openGql.grammar.ImmaterialValueType"

_ImmaterialValueType_nullType = Core.Name "nullType"

_ImmaterialValueType_emptyType = Core.Name "emptyType"

type NullType = ()

_NullType = Core.Name "openGql.grammar.NullType"

type EmptyType = ()

_EmptyType = Core.Name "openGql.grammar.EmptyType"

data GraphReferenceValueType = 
  GraphReferenceValueTypeOpen OpenGraphReferenceValueType |
  GraphReferenceValueTypeClosed ClosedGraphReferenceValueType
  deriving (Eq, Ord, Read, Show)

_GraphReferenceValueType = Core.Name "openGql.grammar.GraphReferenceValueType"

_GraphReferenceValueType_open = Core.Name "open"

_GraphReferenceValueType_closed = Core.Name "closed"

data ClosedGraphReferenceValueType = 
  ClosedGraphReferenceValueType {
    closedGraphReferenceValueTypeProperty :: Bool,
    closedGraphReferenceValueTypeNestedSpec :: NestedGraphTypeSpecification,
    closedGraphReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ClosedGraphReferenceValueType = Core.Name "openGql.grammar.ClosedGraphReferenceValueType"

_ClosedGraphReferenceValueType_property = Core.Name "property"

_ClosedGraphReferenceValueType_nestedSpec = Core.Name "nestedSpec"

_ClosedGraphReferenceValueType_notNull = Core.Name "notNull"

data OpenGraphReferenceValueType = 
  OpenGraphReferenceValueType {
    openGraphReferenceValueTypeAny :: (Maybe Bool),
    openGraphReferenceValueTypeProperty :: Bool,
    openGraphReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenGraphReferenceValueType = Core.Name "openGql.grammar.OpenGraphReferenceValueType"

_OpenGraphReferenceValueType_any = Core.Name "any"

_OpenGraphReferenceValueType_property = Core.Name "property"

_OpenGraphReferenceValueType_notNull = Core.Name "notNull"

data BindingTableReferenceValueType = 
  BindingTableReferenceValueType {
    bindingTableReferenceValueTypeBindingTableType :: BindingTableType,
    bindingTableReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_BindingTableReferenceValueType = Core.Name "openGql.grammar.BindingTableReferenceValueType"

_BindingTableReferenceValueType_bindingTableType = Core.Name "bindingTableType"

_BindingTableReferenceValueType_notNull = Core.Name "notNull"

data NodeReferenceValueType = 
  NodeReferenceValueTypeOpen OpenNodeReferenceValueType |
  NodeReferenceValueTypeClosed ClosedNodeReferenceValueType
  deriving (Eq, Ord, Read, Show)

_NodeReferenceValueType = Core.Name "openGql.grammar.NodeReferenceValueType"

_NodeReferenceValueType_open = Core.Name "open"

_NodeReferenceValueType_closed = Core.Name "closed"

data ClosedNodeReferenceValueType = 
  ClosedNodeReferenceValueType {
    closedNodeReferenceValueTypeNodeTypeSpec :: NodeTypeSpecification,
    closedNodeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ClosedNodeReferenceValueType = Core.Name "openGql.grammar.ClosedNodeReferenceValueType"

_ClosedNodeReferenceValueType_nodeTypeSpec = Core.Name "nodeTypeSpec"

_ClosedNodeReferenceValueType_notNull = Core.Name "notNull"

data OpenNodeReferenceValueType = 
  OpenNodeReferenceValueType {
    openNodeReferenceValueTypeAny :: Bool,
    openNodeReferenceValueTypeNodeSynonym :: NodeSynonym,
    openNodeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenNodeReferenceValueType = Core.Name "openGql.grammar.OpenNodeReferenceValueType"

_OpenNodeReferenceValueType_any = Core.Name "any"

_OpenNodeReferenceValueType_nodeSynonym = Core.Name "nodeSynonym"

_OpenNodeReferenceValueType_notNull = Core.Name "notNull"

data EdgeReferenceValueType = 
  EdgeReferenceValueTypeOpen OpenEdgeReferenceValueType |
  EdgeReferenceValueTypeClosed ClosedEdgeReferenceValueType
  deriving (Eq, Ord, Read, Show)

_EdgeReferenceValueType = Core.Name "openGql.grammar.EdgeReferenceValueType"

_EdgeReferenceValueType_open = Core.Name "open"

_EdgeReferenceValueType_closed = Core.Name "closed"

data ClosedEdgeReferenceValueType = 
  ClosedEdgeReferenceValueType {
    closedEdgeReferenceValueTypeEdgeTypeSpec :: EdgeTypeSpecification,
    closedEdgeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_ClosedEdgeReferenceValueType = Core.Name "openGql.grammar.ClosedEdgeReferenceValueType"

_ClosedEdgeReferenceValueType_edgeTypeSpec = Core.Name "edgeTypeSpec"

_ClosedEdgeReferenceValueType_notNull = Core.Name "notNull"

data OpenEdgeReferenceValueType = 
  OpenEdgeReferenceValueType {
    openEdgeReferenceValueTypeAny :: Bool,
    openEdgeReferenceValueTypeEdgeSynonym :: EdgeSynonym,
    openEdgeReferenceValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_OpenEdgeReferenceValueType = Core.Name "openGql.grammar.OpenEdgeReferenceValueType"

_OpenEdgeReferenceValueType_any = Core.Name "any"

_OpenEdgeReferenceValueType_edgeSynonym = Core.Name "edgeSynonym"

_OpenEdgeReferenceValueType_notNull = Core.Name "notNull"

data PathValueType = 
  PathValueType {
    pathValueTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_PathValueType = Core.Name "openGql.grammar.PathValueType"

_PathValueType_notNull = Core.Name "notNull"

data ListValueTypeName = 
  ListValueTypeName {
    listValueTypeNameGroup :: Bool,
    listValueTypeNameSynonym :: ListValueTypeNameSynonym}
  deriving (Eq, Ord, Read, Show)

_ListValueTypeName = Core.Name "openGql.grammar.ListValueTypeName"

_ListValueTypeName_group = Core.Name "group"

_ListValueTypeName_synonym = Core.Name "synonym"

data ListValueTypeNameSynonym = 
  ListValueTypeNameSynonymList  |
  ListValueTypeNameSynonymArray 
  deriving (Eq, Ord, Read, Show)

_ListValueTypeNameSynonym = Core.Name "openGql.grammar.ListValueTypeNameSynonym"

_ListValueTypeNameSynonym_list = Core.Name "list"

_ListValueTypeNameSynonym_array = Core.Name "array"

data RecordType = 
  RecordTypeAnyRecord AnyRecordType |
  RecordTypeSpecifiedRecord SpecifiedRecordType
  deriving (Eq, Ord, Read, Show)

_RecordType = Core.Name "openGql.grammar.RecordType"

_RecordType_anyRecord = Core.Name "anyRecord"

_RecordType_specifiedRecord = Core.Name "specifiedRecord"

data AnyRecordType = 
  AnyRecordType {
    anyRecordTypeAny :: Bool,
    anyRecordTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_AnyRecordType = Core.Name "openGql.grammar.AnyRecordType"

_AnyRecordType_any = Core.Name "any"

_AnyRecordType_notNull = Core.Name "notNull"

data SpecifiedRecordType = 
  SpecifiedRecordType {
    specifiedRecordTypeRecord :: Bool,
    specifiedRecordTypeFieldTypes :: FieldTypesSpecification,
    specifiedRecordTypeNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_SpecifiedRecordType = Core.Name "openGql.grammar.SpecifiedRecordType"

_SpecifiedRecordType_record = Core.Name "record"

_SpecifiedRecordType_fieldTypes = Core.Name "fieldTypes"

_SpecifiedRecordType_notNull = Core.Name "notNull"

type FieldTypesSpecification = (Maybe FieldTypeList)

_FieldTypesSpecification = Core.Name "openGql.grammar.FieldTypesSpecification"

type FieldTypeList = [FieldType]

_FieldTypeList = Core.Name "openGql.grammar.FieldTypeList"

type NotNull = ()

_NotNull = Core.Name "openGql.grammar.NotNull"

data FieldType = 
  FieldType {
    fieldTypeFieldName :: FieldName,
    fieldTypeTyped :: (Maybe Typed),
    fieldTypeValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_FieldType = Core.Name "openGql.grammar.FieldType"

_FieldType_fieldName = Core.Name "fieldName"

_FieldType_typed = Core.Name "typed"

_FieldType_valueType = Core.Name "valueType"

type SearchCondition = BooleanValueExpression

_SearchCondition = Core.Name "openGql.grammar.SearchCondition"

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

_Predicate = Core.Name "openGql.grammar.Predicate"

_Predicate_existsPredicate = Core.Name "existsPredicate"

_Predicate_nullPredicate = Core.Name "nullPredicate"

_Predicate_valueTypePredicate = Core.Name "valueTypePredicate"

_Predicate_directedPredicate = Core.Name "directedPredicate"

_Predicate_labeledPredicate = Core.Name "labeledPredicate"

_Predicate_sourceDestinationPredicate = Core.Name "sourceDestinationPredicate"

_Predicate_allDifferentPredicate = Core.Name "allDifferentPredicate"

_Predicate_samePredicate = Core.Name "samePredicate"

_Predicate_propertyExistsPredicate = Core.Name "propertyExistsPredicate"

data ComparisonPredicatePart2 = 
  ComparisonPredicatePart2 {
    comparisonPredicatePart2CompOp :: CompOp,
    comparisonPredicatePart2ValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ComparisonPredicatePart2 = Core.Name "openGql.grammar.ComparisonPredicatePart2"

_ComparisonPredicatePart2_compOp = Core.Name "compOp"

_ComparisonPredicatePart2_valueExpression = Core.Name "valueExpression"

data CompOp = 
  CompOpEquals  |
  CompOpNotEquals  |
  CompOpLessThan  |
  CompOpGreaterThan  |
  CompOpLessThanOrEquals  |
  CompOpGreaterThanOrEquals 
  deriving (Eq, Ord, Read, Show)

_CompOp = Core.Name "openGql.grammar.CompOp"

_CompOp_equals = Core.Name "equals"

_CompOp_notEquals = Core.Name "notEquals"

_CompOp_lessThan = Core.Name "lessThan"

_CompOp_greaterThan = Core.Name "greaterThan"

_CompOp_lessThanOrEquals = Core.Name "lessThanOrEquals"

_CompOp_greaterThanOrEquals = Core.Name "greaterThanOrEquals"

data ExistsPredicate = 
  ExistsPredicateGraphPatternBrace GraphPattern |
  ExistsPredicateGraphPatternParen GraphPattern |
  ExistsPredicateMatchBlockBrace MatchStatementBlock |
  ExistsPredicateMatchBlockParen MatchStatementBlock |
  ExistsPredicateNestedQuery NestedQuerySpecification
  deriving (Eq, Ord, Read, Show)

_ExistsPredicate = Core.Name "openGql.grammar.ExistsPredicate"

_ExistsPredicate_graphPatternBrace = Core.Name "graphPatternBrace"

_ExistsPredicate_graphPatternParen = Core.Name "graphPatternParen"

_ExistsPredicate_matchBlockBrace = Core.Name "matchBlockBrace"

_ExistsPredicate_matchBlockParen = Core.Name "matchBlockParen"

_ExistsPredicate_nestedQuery = Core.Name "nestedQuery"

data NullPredicate = 
  NullPredicate {
    nullPredicateValueExpression :: PrimaryValueExpression,
    nullPredicateNullPart :: NullPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_NullPredicate = Core.Name "openGql.grammar.NullPredicate"

_NullPredicate_valueExpression = Core.Name "valueExpression"

_NullPredicate_nullPart = Core.Name "nullPart"

data NullPredicatePart2 = 
  NullPredicatePart2 {
    nullPredicatePart2Not :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullPredicatePart2 = Core.Name "openGql.grammar.NullPredicatePart2"

_NullPredicatePart2_not = Core.Name "not"

data ValueTypePredicate = 
  ValueTypePredicate {
    valueTypePredicateValueExpression :: PrimaryValueExpression,
    valueTypePredicateValueTypePart :: ValueTypePredicatePart2}
  deriving (Eq, Ord, Read, Show)

_ValueTypePredicate = Core.Name "openGql.grammar.ValueTypePredicate"

_ValueTypePredicate_valueExpression = Core.Name "valueExpression"

_ValueTypePredicate_valueTypePart = Core.Name "valueTypePart"

data ValueTypePredicatePart2 = 
  ValueTypePredicatePart2 {
    valueTypePredicatePart2Not :: Bool,
    valueTypePredicatePart2Typed :: Typed,
    valueTypePredicatePart2ValueType :: ValueType}
  deriving (Eq, Ord, Read, Show)

_ValueTypePredicatePart2 = Core.Name "openGql.grammar.ValueTypePredicatePart2"

_ValueTypePredicatePart2_not = Core.Name "not"

_ValueTypePredicatePart2_typed = Core.Name "typed"

_ValueTypePredicatePart2_valueType = Core.Name "valueType"

data NormalizedPredicatePart2 = 
  NormalizedPredicatePart2 {
    normalizedPredicatePart2Not :: Bool,
    normalizedPredicatePart2NormalForm :: (Maybe NormalForm)}
  deriving (Eq, Ord, Read, Show)

_NormalizedPredicatePart2 = Core.Name "openGql.grammar.NormalizedPredicatePart2"

_NormalizedPredicatePart2_not = Core.Name "not"

_NormalizedPredicatePart2_normalForm = Core.Name "normalForm"

data DirectedPredicate = 
  DirectedPredicate {
    directedPredicateElementVariableReference :: ElementVariableReference,
    directedPredicateDirectedPart :: DirectedPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_DirectedPredicate = Core.Name "openGql.grammar.DirectedPredicate"

_DirectedPredicate_elementVariableReference = Core.Name "elementVariableReference"

_DirectedPredicate_directedPart = Core.Name "directedPart"

data DirectedPredicatePart2 = 
  DirectedPredicatePart2 {
    directedPredicatePart2Not :: Bool}
  deriving (Eq, Ord, Read, Show)

_DirectedPredicatePart2 = Core.Name "openGql.grammar.DirectedPredicatePart2"

_DirectedPredicatePart2_not = Core.Name "not"

data LabeledPredicate = 
  LabeledPredicate {
    labeledPredicateElementVariableReference :: ElementVariableReference,
    labeledPredicateLabeledPart :: LabeledPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_LabeledPredicate = Core.Name "openGql.grammar.LabeledPredicate"

_LabeledPredicate_elementVariableReference = Core.Name "elementVariableReference"

_LabeledPredicate_labeledPart = Core.Name "labeledPart"

data LabeledPredicatePart2 = 
  LabeledPredicatePart2 {
    labeledPredicatePart2IsLabeledOrColon :: IsLabeledOrColon,
    labeledPredicatePart2LabelExpression :: LabelExpression}
  deriving (Eq, Ord, Read, Show)

_LabeledPredicatePart2 = Core.Name "openGql.grammar.LabeledPredicatePart2"

_LabeledPredicatePart2_isLabeledOrColon = Core.Name "isLabeledOrColon"

_LabeledPredicatePart2_labelExpression = Core.Name "labelExpression"

data IsLabeledOrColon = 
  IsLabeledOrColonNot Bool |
  IsLabeledOrColonColon 
  deriving (Eq, Ord, Read, Show)

_IsLabeledOrColon = Core.Name "openGql.grammar.IsLabeledOrColon"

_IsLabeledOrColon_not = Core.Name "not"

_IsLabeledOrColon_colon = Core.Name "colon"

data SourceDestinationPredicate = 
  SourceDestinationPredicateSourcePredicate SourcePredicate |
  SourceDestinationPredicateDestinationPredicate DestinationPredicate
  deriving (Eq, Ord, Read, Show)

_SourceDestinationPredicate = Core.Name "openGql.grammar.SourceDestinationPredicate"

_SourceDestinationPredicate_sourcePredicate = Core.Name "sourcePredicate"

_SourceDestinationPredicate_destinationPredicate = Core.Name "destinationPredicate"

type NodeReference = ElementVariableReference

_NodeReference = Core.Name "openGql.grammar.NodeReference"

data SourcePredicate = 
  SourcePredicate {
    sourcePredicateNot :: Bool,
    sourcePredicateSourceOf :: EdgeReference}
  deriving (Eq, Ord, Read, Show)

_SourcePredicate = Core.Name "openGql.grammar.SourcePredicate"

_SourcePredicate_not = Core.Name "not"

_SourcePredicate_sourceOf = Core.Name "sourceOf"

data DestinationPredicate = 
  DestinationPredicate {
    destinationPredicateNodeReference :: NodeReference,
    destinationPredicateNot :: Bool,
    destinationPredicateDestinationOf :: EdgeReference}
  deriving (Eq, Ord, Read, Show)

_DestinationPredicate = Core.Name "openGql.grammar.DestinationPredicate"

_DestinationPredicate_nodeReference = Core.Name "nodeReference"

_DestinationPredicate_not = Core.Name "not"

_DestinationPredicate_destinationOf = Core.Name "destinationOf"

type EdgeReference = ElementVariableReference

_EdgeReference = Core.Name "openGql.grammar.EdgeReference"

data AllDifferentPredicate = 
  AllDifferentPredicate {
    allDifferentPredicateReferences :: [ElementVariableReference]}
  deriving (Eq, Ord, Read, Show)

_AllDifferentPredicate = Core.Name "openGql.grammar.AllDifferentPredicate"

_AllDifferentPredicate_references = Core.Name "references"

data SamePredicate = 
  SamePredicate {
    samePredicateReferences :: [ElementVariableReference]}
  deriving (Eq, Ord, Read, Show)

_SamePredicate = Core.Name "openGql.grammar.SamePredicate"

_SamePredicate_references = Core.Name "references"

data PropertyExistsPredicate = 
  PropertyExistsPredicate {
    propertyExistsPredicateElementVariableReference :: ElementVariableReference,
    propertyExistsPredicatePropertyName :: PropertyName}
  deriving (Eq, Ord, Read, Show)

_PropertyExistsPredicate = Core.Name "openGql.grammar.PropertyExistsPredicate"

_PropertyExistsPredicate_elementVariableReference = Core.Name "elementVariableReference"

_PropertyExistsPredicate_propertyName = Core.Name "propertyName"

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
  ValueExpressionPrimary PrimaryValueExpression
  deriving (Eq, Ord, Read, Show)

_ValueExpression = Core.Name "openGql.grammar.ValueExpression"

_ValueExpression_signed = Core.Name "signed"

_ValueExpression_multDiv = Core.Name "multDiv"

_ValueExpression_addSubtract = Core.Name "addSubtract"

_ValueExpression_concatenation = Core.Name "concatenation"

_ValueExpression_not = Core.Name "not"

_ValueExpression_isNot = Core.Name "isNot"

_ValueExpression_conjunctive = Core.Name "conjunctive"

_ValueExpression_disjunctive = Core.Name "disjunctive"

_ValueExpression_comparison = Core.Name "comparison"

_ValueExpression_predicate = Core.Name "predicate"

_ValueExpression_normalizedPredicate = Core.Name "normalizedPredicate"

_ValueExpression_propertyGraph = Core.Name "propertyGraph"

_ValueExpression_bindingTable = Core.Name "bindingTable"

_ValueExpression_valueFunction = Core.Name "valueFunction"

_ValueExpression_primary = Core.Name "primary"

data SignedExpr = 
  SignedExpr {
    signedExprSign :: Sign,
    signedExprValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_SignedExpr = Core.Name "openGql.grammar.SignedExpr"

_SignedExpr_sign = Core.Name "sign"

_SignedExpr_valueExpression = Core.Name "valueExpression"

data MultDivExpr = 
  MultDivExpr {
    multDivExprLeft :: ValueExpression,
    multDivExprOperator :: MultDivOperator,
    multDivExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_MultDivExpr = Core.Name "openGql.grammar.MultDivExpr"

_MultDivExpr_left = Core.Name "left"

_MultDivExpr_operator = Core.Name "operator"

_MultDivExpr_right = Core.Name "right"

data AddSubtractExpr = 
  AddSubtractExpr {
    addSubtractExprLeft :: ValueExpression,
    addSubtractExprOperator :: AddSubtractOperator,
    addSubtractExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_AddSubtractExpr = Core.Name "openGql.grammar.AddSubtractExpr"

_AddSubtractExpr_left = Core.Name "left"

_AddSubtractExpr_operator = Core.Name "operator"

_AddSubtractExpr_right = Core.Name "right"

data ConcatenationExpr = 
  ConcatenationExpr {
    concatenationExprLeft :: ValueExpression,
    concatenationExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ConcatenationExpr = Core.Name "openGql.grammar.ConcatenationExpr"

_ConcatenationExpr_left = Core.Name "left"

_ConcatenationExpr_right = Core.Name "right"

type NotExpr = ValueExpression

_NotExpr = Core.Name "openGql.grammar.NotExpr"

data IsNotExpr = 
  IsNotExpr {
    isNotExprValueExpression :: ValueExpression,
    isNotExprNot :: Bool,
    isNotExprTruthValue :: TruthValue}
  deriving (Eq, Ord, Read, Show)

_IsNotExpr = Core.Name "openGql.grammar.IsNotExpr"

_IsNotExpr_valueExpression = Core.Name "valueExpression"

_IsNotExpr_not = Core.Name "not"

_IsNotExpr_truthValue = Core.Name "truthValue"

data ConjunctiveExpr = 
  ConjunctiveExpr {
    conjunctiveExprLeft :: ValueExpression,
    conjunctiveExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_ConjunctiveExpr = Core.Name "openGql.grammar.ConjunctiveExpr"

_ConjunctiveExpr_left = Core.Name "left"

_ConjunctiveExpr_right = Core.Name "right"

data DisjunctiveExpr = 
  DisjunctiveExpr {
    disjunctiveExprLeft :: ValueExpression,
    disjunctiveExprOperator :: DisjunctiveOperator,
    disjunctiveExprRight :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_DisjunctiveExpr = Core.Name "openGql.grammar.DisjunctiveExpr"

_DisjunctiveExpr_left = Core.Name "left"

_DisjunctiveExpr_operator = Core.Name "operator"

_DisjunctiveExpr_right = Core.Name "right"

data ComparisonExpr = 
  ComparisonExpr {
    comparisonExprValueExpression :: ValueExpression,
    comparisonExprComparison :: ComparisonPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_ComparisonExpr = Core.Name "openGql.grammar.ComparisonExpr"

_ComparisonExpr_valueExpression = Core.Name "valueExpression"

_ComparisonExpr_comparison = Core.Name "comparison"

data NormalizedPredicateExpr = 
  NormalizedPredicateExpr {
    normalizedPredicateExprValueExpression :: ValueExpression,
    normalizedPredicateExprNormalizedPredicate :: NormalizedPredicatePart2}
  deriving (Eq, Ord, Read, Show)

_NormalizedPredicateExpr = Core.Name "openGql.grammar.NormalizedPredicateExpr"

_NormalizedPredicateExpr_valueExpression = Core.Name "valueExpression"

_NormalizedPredicateExpr_normalizedPredicate = Core.Name "normalizedPredicate"

data Sign = 
  SignPlus  |
  SignMinus 
  deriving (Eq, Ord, Read, Show)

_Sign = Core.Name "openGql.grammar.Sign"

_Sign_plus = Core.Name "plus"

_Sign_minus = Core.Name "minus"

data MultDivOperator = 
  MultDivOperatorMultiply  |
  MultDivOperatorDivide 
  deriving (Eq, Ord, Read, Show)

_MultDivOperator = Core.Name "openGql.grammar.MultDivOperator"

_MultDivOperator_multiply = Core.Name "multiply"

_MultDivOperator_divide = Core.Name "divide"

data AddSubtractOperator = 
  AddSubtractOperatorAdd  |
  AddSubtractOperatorSubtract 
  deriving (Eq, Ord, Read, Show)

_AddSubtractOperator = Core.Name "openGql.grammar.AddSubtractOperator"

_AddSubtractOperator_add = Core.Name "add"

_AddSubtractOperator_subtract = Core.Name "subtract"

data DisjunctiveOperator = 
  DisjunctiveOperatorOr  |
  DisjunctiveOperatorXor 
  deriving (Eq, Ord, Read, Show)

_DisjunctiveOperator = Core.Name "openGql.grammar.DisjunctiveOperator"

_DisjunctiveOperator_or = Core.Name "or"

_DisjunctiveOperator_xor = Core.Name "xor"

data ValueFunction = 
  ValueFunctionNumeric NumericValueFunction |
  ValueFunctionDatetimeSubtraction DatetimeSubtraction |
  ValueFunctionDatetime DatetimeValueFunction |
  ValueFunctionDuration DurationValueFunction |
  ValueFunctionCharacterOrByteString CharacterOrByteStringFunction |
  ValueFunctionList ListValueFunction
  deriving (Eq, Ord, Read, Show)

_ValueFunction = Core.Name "openGql.grammar.ValueFunction"

_ValueFunction_numeric = Core.Name "numeric"

_ValueFunction_datetimeSubtraction = Core.Name "datetimeSubtraction"

_ValueFunction_datetime = Core.Name "datetime"

_ValueFunction_duration = Core.Name "duration"

_ValueFunction_characterOrByteString = Core.Name "characterOrByteString"

_ValueFunction_list = Core.Name "list"

type BooleanValueExpression = ValueExpression

_BooleanValueExpression = Core.Name "openGql.grammar.BooleanValueExpression"

data CharacterOrByteStringFunction = 
  CharacterOrByteStringFunctionSub SubCharacterOrByteString |
  CharacterOrByteStringFunctionTrimSingle TrimSingleCharacterOrByteString |
  CharacterOrByteStringFunctionFold FoldCharacterString |
  CharacterOrByteStringFunctionTrimMultiCharacter TrimMultiCharacterCharacterString |
  CharacterOrByteStringFunctionNormalize NormalizeCharacterString
  deriving (Eq, Ord, Read, Show)

_CharacterOrByteStringFunction = Core.Name "openGql.grammar.CharacterOrByteStringFunction"

_CharacterOrByteStringFunction_sub = Core.Name "sub"

_CharacterOrByteStringFunction_trimSingle = Core.Name "trimSingle"

_CharacterOrByteStringFunction_fold = Core.Name "fold"

_CharacterOrByteStringFunction_trimMultiCharacter = Core.Name "trimMultiCharacter"

_CharacterOrByteStringFunction_normalize = Core.Name "normalize"

data SubCharacterOrByteString = 
  SubCharacterOrByteString {
    subCharacterOrByteStringSide :: Side,
    subCharacterOrByteStringValueExpression :: ValueExpression,
    subCharacterOrByteStringStringLength :: StringLength}
  deriving (Eq, Ord, Read, Show)

_SubCharacterOrByteString = Core.Name "openGql.grammar.SubCharacterOrByteString"

_SubCharacterOrByteString_side = Core.Name "side"

_SubCharacterOrByteString_valueExpression = Core.Name "valueExpression"

_SubCharacterOrByteString_stringLength = Core.Name "stringLength"

data Side = 
  SideLeft  |
  SideRight 
  deriving (Eq, Ord, Read, Show)

_Side = Core.Name "openGql.grammar.Side"

_Side_left = Core.Name "left"

_Side_right = Core.Name "right"

type TrimSingleCharacterOrByteString = TrimOperands

_TrimSingleCharacterOrByteString = Core.Name "openGql.grammar.TrimSingleCharacterOrByteString"

data FoldCharacterString = 
  FoldCharacterString {
    foldCharacterStringCase :: Case,
    foldCharacterStringValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_FoldCharacterString = Core.Name "openGql.grammar.FoldCharacterString"

_FoldCharacterString_case = Core.Name "case"

_FoldCharacterString_valueExpression = Core.Name "valueExpression"

data Case = 
  CaseUpper  |
  CaseLower 
  deriving (Eq, Ord, Read, Show)

_Case = Core.Name "openGql.grammar.Case"

_Case_upper = Core.Name "upper"

_Case_lower = Core.Name "lower"

data TrimMultiCharacterCharacterString = 
  TrimMultiCharacterCharacterString {
    trimMultiCharacterCharacterStringTrimType :: TrimType,
    trimMultiCharacterCharacterStringValueExpression :: ValueExpression,
    trimMultiCharacterCharacterStringOptionalValueExpression :: (Maybe ValueExpression)}
  deriving (Eq, Ord, Read, Show)

_TrimMultiCharacterCharacterString = Core.Name "openGql.grammar.TrimMultiCharacterCharacterString"

_TrimMultiCharacterCharacterString_trimType = Core.Name "trimType"

_TrimMultiCharacterCharacterString_valueExpression = Core.Name "valueExpression"

_TrimMultiCharacterCharacterString_optionalValueExpression = Core.Name "optionalValueExpression"

data TrimType = 
  TrimTypeBtrim  |
  TrimTypeLtrim  |
  TrimTypeRtrim 
  deriving (Eq, Ord, Read, Show)

_TrimType = Core.Name "openGql.grammar.TrimType"

_TrimType_btrim = Core.Name "btrim"

_TrimType_ltrim = Core.Name "ltrim"

_TrimType_rtrim = Core.Name "rtrim"

data NormalizeCharacterString = 
  NormalizeCharacterString {
    normalizeCharacterStringValueExpression :: ValueExpression,
    normalizeCharacterStringNormalForm :: (Maybe NormalForm)}
  deriving (Eq, Ord, Read, Show)

_NormalizeCharacterString = Core.Name "openGql.grammar.NormalizeCharacterString"

_NormalizeCharacterString_valueExpression = Core.Name "valueExpression"

_NormalizeCharacterString_normalForm = Core.Name "normalForm"

type NodeReferenceValueExpression = PrimaryValueExpression

_NodeReferenceValueExpression = Core.Name "openGql.grammar.NodeReferenceValueExpression"

type EdgeReferenceValueExpression = PrimaryValueExpression

_EdgeReferenceValueExpression = Core.Name "openGql.grammar.EdgeReferenceValueExpression"

type AggregatingValueExpression = ValueExpression

_AggregatingValueExpression = Core.Name "openGql.grammar.AggregatingValueExpression"

data PrimaryValueExpression = 
  PrimaryValueExpressionParenthesized ParenthesizedValueExpression |
  PrimaryValueExpressionAggregateFunction AggregateFunction |
  PrimaryValueExpressionUnsignedValueSpecification UnsignedValueSpecification |
  PrimaryValueExpressionPathValueConstructor PathValueConstructor |
  PrimaryValueExpressionPropertyReference PropertyReference |
  PrimaryValueExpressionValueQueryExpression ValueQueryExpression |
  PrimaryValueExpressionCaseExpression CaseExpression |
  PrimaryValueExpressionCastSpecification CastSpecification |
  PrimaryValueExpressionElementIdFunction ElementIdFunction |
  PrimaryValueExpressionLetValueExpression LetValueExpression |
  PrimaryValueExpressionBindingVariableReference BindingVariableReference
  deriving (Eq, Ord, Read, Show)

_PrimaryValueExpression = Core.Name "openGql.grammar.PrimaryValueExpression"

_PrimaryValueExpression_parenthesized = Core.Name "parenthesized"

_PrimaryValueExpression_aggregateFunction = Core.Name "aggregateFunction"

_PrimaryValueExpression_unsignedValueSpecification = Core.Name "unsignedValueSpecification"

_PrimaryValueExpression_pathValueConstructor = Core.Name "pathValueConstructor"

_PrimaryValueExpression_propertyReference = Core.Name "propertyReference"

_PrimaryValueExpression_valueQueryExpression = Core.Name "valueQueryExpression"

_PrimaryValueExpression_caseExpression = Core.Name "caseExpression"

_PrimaryValueExpression_castSpecification = Core.Name "castSpecification"

_PrimaryValueExpression_elementIdFunction = Core.Name "elementIdFunction"

_PrimaryValueExpression_letValueExpression = Core.Name "letValueExpression"

_PrimaryValueExpression_bindingVariableReference = Core.Name "bindingVariableReference"

type ParenthesizedValueExpression = ValueExpression

_ParenthesizedValueExpression = Core.Name "openGql.grammar.ParenthesizedValueExpression"

data NonParenthesizedPrimaryValueExpression = 
  NonParenthesizedPrimaryValueExpressionSpecial NonParenthesizedPrimaryValueExpressionSpecialCase |
  NonParenthesizedPrimaryValueExpressionBindingVariable BindingVariableReference
  deriving (Eq, Ord, Read, Show)

_NonParenthesizedPrimaryValueExpression = Core.Name "openGql.grammar.NonParenthesizedPrimaryValueExpression"

_NonParenthesizedPrimaryValueExpression_special = Core.Name "special"

_NonParenthesizedPrimaryValueExpression_bindingVariable = Core.Name "bindingVariable"

data NonParenthesizedPrimaryValueExpressionSpecialCase = 
  NonParenthesizedPrimaryValueExpressionSpecialCaseAggregateFunction AggregateFunction |
  NonParenthesizedPrimaryValueExpressionSpecialCaseUnsignedValueSpecification UnsignedValueSpecification |
  NonParenthesizedPrimaryValueExpressionSpecialCasePathValueConstructor PathValueConstructor |
  NonParenthesizedPrimaryValueExpressionSpecialCasePropertyReference PropertyReference |
  NonParenthesizedPrimaryValueExpressionSpecialCaseValueQueryExpression ValueQueryExpression |
  NonParenthesizedPrimaryValueExpressionSpecialCaseCaseExpression CaseExpression |
  NonParenthesizedPrimaryValueExpressionSpecialCaseCastSpecification CastSpecification |
  NonParenthesizedPrimaryValueExpressionSpecialCaseElementIdFunction ElementIdFunction |
  NonParenthesizedPrimaryValueExpressionSpecialCaseLetValueExpression LetValueExpression
  deriving (Eq, Ord, Read, Show)

_NonParenthesizedPrimaryValueExpressionSpecialCase =
    Core.Name "openGql.grammar.NonParenthesizedPrimaryValueExpressionSpecialCase"

_NonParenthesizedPrimaryValueExpressionSpecialCase_aggregateFunction = Core.Name "aggregateFunction"

_NonParenthesizedPrimaryValueExpressionSpecialCase_unsignedValueSpecification = Core.Name "unsignedValueSpecification"

_NonParenthesizedPrimaryValueExpressionSpecialCase_pathValueConstructor = Core.Name "pathValueConstructor"

_NonParenthesizedPrimaryValueExpressionSpecialCase_propertyReference = Core.Name "propertyReference"

_NonParenthesizedPrimaryValueExpressionSpecialCase_valueQueryExpression = Core.Name "valueQueryExpression"

_NonParenthesizedPrimaryValueExpressionSpecialCase_caseExpression = Core.Name "caseExpression"

_NonParenthesizedPrimaryValueExpressionSpecialCase_castSpecification = Core.Name "castSpecification"

_NonParenthesizedPrimaryValueExpressionSpecialCase_elementIdFunction = Core.Name "elementIdFunction"

_NonParenthesizedPrimaryValueExpressionSpecialCase_letValueExpression = Core.Name "letValueExpression"

data UnsignedValueSpecification = 
  UnsignedValueSpecificationUnsignedLiteral UnsignedLiteral |
  UnsignedValueSpecificationGeneralValueSpecification GeneralValueSpecification
  deriving (Eq, Ord, Read, Show)

_UnsignedValueSpecification = Core.Name "openGql.grammar.UnsignedValueSpecification"

_UnsignedValueSpecification_unsignedLiteral = Core.Name "unsignedLiteral"

_UnsignedValueSpecification_generalValueSpecification = Core.Name "generalValueSpecification"

data NonNegativeIntegerSpecification = 
  NonNegativeIntegerSpecificationUnsignedInteger UnsignedInteger |
  NonNegativeIntegerSpecificationDynamicParameterSpecification DynamicParameterSpecification
  deriving (Eq, Ord, Read, Show)

_NonNegativeIntegerSpecification = Core.Name "openGql.grammar.NonNegativeIntegerSpecification"

_NonNegativeIntegerSpecification_unsignedInteger = Core.Name "unsignedInteger"

_NonNegativeIntegerSpecification_dynamicParameterSpecification = Core.Name "dynamicParameterSpecification"

data GeneralValueSpecification = 
  GeneralValueSpecificationDynamicParameterSpecification DynamicParameterSpecification |
  GeneralValueSpecificationSessionUser 
  deriving (Eq, Ord, Read, Show)

_GeneralValueSpecification = Core.Name "openGql.grammar.GeneralValueSpecification"

_GeneralValueSpecification_dynamicParameterSpecification = Core.Name "dynamicParameterSpecification"

_GeneralValueSpecification_sessionUser = Core.Name "sessionUser"

type DynamicParameterSpecification = ParameterName

_DynamicParameterSpecification = Core.Name "openGql.grammar.DynamicParameterSpecification"

data LetValueExpression = 
  LetValueExpression {
    letValueExpressionLetVariables :: LetVariableDefinitionList,
    letValueExpressionValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_LetValueExpression = Core.Name "openGql.grammar.LetValueExpression"

_LetValueExpression_letVariables = Core.Name "letVariables"

_LetValueExpression_valueExpression = Core.Name "valueExpression"

type ValueQueryExpression = NestedQuerySpecification

_ValueQueryExpression = Core.Name "openGql.grammar.ValueQueryExpression"

data CaseExpression = 
  CaseExpressionAbbreviation CaseAbbreviation |
  CaseExpressionSpecification CaseSpecification
  deriving (Eq, Ord, Read, Show)

_CaseExpression = Core.Name "openGql.grammar.CaseExpression"

_CaseExpression_abbreviation = Core.Name "abbreviation"

_CaseExpression_specification = Core.Name "specification"

data CaseAbbreviation = 
  CaseAbbreviationNullIf NullIfAbbreviation |
  CaseAbbreviationCoalesce [ValueExpression]
  deriving (Eq, Ord, Read, Show)

_CaseAbbreviation = Core.Name "openGql.grammar.CaseAbbreviation"

_CaseAbbreviation_nullIf = Core.Name "nullIf"

_CaseAbbreviation_coalesce = Core.Name "coalesce"

data NullIfAbbreviation = 
  NullIfAbbreviation {
    nullIfAbbreviationFirst :: ValueExpression,
    nullIfAbbreviationSecond :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_NullIfAbbreviation = Core.Name "openGql.grammar.NullIfAbbreviation"

_NullIfAbbreviation_first = Core.Name "first"

_NullIfAbbreviation_second = Core.Name "second"

data CaseSpecification = 
  CaseSpecificationSimple SimpleCase |
  CaseSpecificationSearched SearchedCase
  deriving (Eq, Ord, Read, Show)

_CaseSpecification = Core.Name "openGql.grammar.CaseSpecification"

_CaseSpecification_simple = Core.Name "simple"

_CaseSpecification_searched = Core.Name "searched"

data SimpleCase = 
  SimpleCase {
    simpleCaseCaseOperand :: CaseOperand,
    simpleCaseWhenClauses :: [SimpleWhenClause],
    simpleCaseElseClause :: (Maybe ElseClause)}
  deriving (Eq, Ord, Read, Show)

_SimpleCase = Core.Name "openGql.grammar.SimpleCase"

_SimpleCase_caseOperand = Core.Name "caseOperand"

_SimpleCase_whenClauses = Core.Name "whenClauses"

_SimpleCase_elseClause = Core.Name "elseClause"

data SearchedCase = 
  SearchedCase {
    searchedCaseWhenClauses :: [SearchedWhenClause],
    searchedCaseElseClause :: (Maybe ElseClause)}
  deriving (Eq, Ord, Read, Show)

_SearchedCase = Core.Name "openGql.grammar.SearchedCase"

_SearchedCase_whenClauses = Core.Name "whenClauses"

_SearchedCase_elseClause = Core.Name "elseClause"

data SimpleWhenClause = 
  SimpleWhenClause {
    simpleWhenClauseWhenOperands :: WhenOperandList,
    simpleWhenClauseResult :: Result}
  deriving (Eq, Ord, Read, Show)

_SimpleWhenClause = Core.Name "openGql.grammar.SimpleWhenClause"

_SimpleWhenClause_whenOperands = Core.Name "whenOperands"

_SimpleWhenClause_result = Core.Name "result"

data SearchedWhenClause = 
  SearchedWhenClause {
    searchedWhenClauseSearchCondition :: SearchCondition,
    searchedWhenClauseResult :: Result}
  deriving (Eq, Ord, Read, Show)

_SearchedWhenClause = Core.Name "openGql.grammar.SearchedWhenClause"

_SearchedWhenClause_searchCondition = Core.Name "searchCondition"

_SearchedWhenClause_result = Core.Name "result"

type ElseClause = Result

_ElseClause = Core.Name "openGql.grammar.ElseClause"

data CaseOperand = 
  CaseOperandValueExpression NonParenthesizedPrimaryValueExpression |
  CaseOperandElementReference ElementVariableReference
  deriving (Eq, Ord, Read, Show)

_CaseOperand = Core.Name "openGql.grammar.CaseOperand"

_CaseOperand_valueExpression = Core.Name "valueExpression"

_CaseOperand_elementReference = Core.Name "elementReference"

type WhenOperandList = [WhenOperand]

_WhenOperandList = Core.Name "openGql.grammar.WhenOperandList"

data WhenOperand = 
  WhenOperandValueExpression NonParenthesizedPrimaryValueExpression |
  WhenOperandComparison ComparisonPredicatePart2 |
  WhenOperandNullPredicate NullPredicatePart2 |
  WhenOperandValueTypePredicate ValueTypePredicatePart2 |
  WhenOperandNormalizedPredicate NormalizedPredicatePart2 |
  WhenOperandDirectedPredicate DirectedPredicatePart2 |
  WhenOperandLabeledPredicate LabeledPredicatePart2 |
  WhenOperandSourcePredicate SourcePredicate |
  WhenOperandDestinationPredicate DestinationPredicate
  deriving (Eq, Ord, Read, Show)

_WhenOperand = Core.Name "openGql.grammar.WhenOperand"

_WhenOperand_valueExpression = Core.Name "valueExpression"

_WhenOperand_comparison = Core.Name "comparison"

_WhenOperand_nullPredicate = Core.Name "nullPredicate"

_WhenOperand_valueTypePredicate = Core.Name "valueTypePredicate"

_WhenOperand_normalizedPredicate = Core.Name "normalizedPredicate"

_WhenOperand_directedPredicate = Core.Name "directedPredicate"

_WhenOperand_labeledPredicate = Core.Name "labeledPredicate"

_WhenOperand_sourcePredicate = Core.Name "sourcePredicate"

_WhenOperand_destinationPredicate = Core.Name "destinationPredicate"

data Result = 
  ResultSimple ResultExpression |
  ResultNullLiteral 
  deriving (Eq, Ord, Read, Show)

_Result = Core.Name "openGql.grammar.Result"

_Result_simple = Core.Name "simple"

_Result_nullLiteral = Core.Name "nullLiteral"

type ResultExpression = ValueExpression

_ResultExpression = Core.Name "openGql.grammar.ResultExpression"

data CastSpecification = 
  CastSpecification {
    castSpecificationOperand :: CastOperand,
    castSpecificationTarget :: CastTarget}
  deriving (Eq, Ord, Read, Show)

_CastSpecification = Core.Name "openGql.grammar.CastSpecification"

_CastSpecification_operand = Core.Name "operand"

_CastSpecification_target = Core.Name "target"

data CastOperand = 
  CastOperandValueExpression ValueExpression |
  CastOperandNullLiteral 
  deriving (Eq, Ord, Read, Show)

_CastOperand = Core.Name "openGql.grammar.CastOperand"

_CastOperand_valueExpression = Core.Name "valueExpression"

_CastOperand_nullLiteral = Core.Name "nullLiteral"

type CastTarget = ValueType

_CastTarget = Core.Name "openGql.grammar.CastTarget"

data AggregateFunction = 
  AggregateFunctionCountAll  |
  AggregateFunctionGeneralSetFunction GeneralSetFunction |
  AggregateFunctionBinarySetFunction BinarySetFunction
  deriving (Eq, Ord, Read, Show)

_AggregateFunction = Core.Name "openGql.grammar.AggregateFunction"

_AggregateFunction_countAll = Core.Name "countAll"

_AggregateFunction_generalSetFunction = Core.Name "generalSetFunction"

_AggregateFunction_binarySetFunction = Core.Name "binarySetFunction"

data GeneralSetFunction = 
  GeneralSetFunction {
    generalSetFunctionFunctionType :: GeneralSetFunctionType,
    generalSetFunctionSetQuantifier :: (Maybe SetQuantifier),
    generalSetFunctionValueExpression :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_GeneralSetFunction = Core.Name "openGql.grammar.GeneralSetFunction"

_GeneralSetFunction_functionType = Core.Name "functionType"

_GeneralSetFunction_setQuantifier = Core.Name "setQuantifier"

_GeneralSetFunction_valueExpression = Core.Name "valueExpression"

data BinarySetFunction = 
  BinarySetFunction {
    binarySetFunctionFunctionType :: BinarySetFunctionType,
    binarySetFunctionDependentValue :: DependentValueExpression,
    binarySetFunctionIndependentValue :: IndependentValueExpression}
  deriving (Eq, Ord, Read, Show)

_BinarySetFunction = Core.Name "openGql.grammar.BinarySetFunction"

_BinarySetFunction_functionType = Core.Name "functionType"

_BinarySetFunction_dependentValue = Core.Name "dependentValue"

_BinarySetFunction_independentValue = Core.Name "independentValue"

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

_GeneralSetFunctionType = Core.Name "openGql.grammar.GeneralSetFunctionType"

_GeneralSetFunctionType_avg = Core.Name "avg"

_GeneralSetFunctionType_count = Core.Name "count"

_GeneralSetFunctionType_max = Core.Name "max"

_GeneralSetFunctionType_min = Core.Name "min"

_GeneralSetFunctionType_sum = Core.Name "sum"

_GeneralSetFunctionType_collectList = Core.Name "collectList"

_GeneralSetFunctionType_stddevSamp = Core.Name "stddevSamp"

_GeneralSetFunctionType_stddevPop = Core.Name "stddevPop"

data SetQuantifier = 
  SetQuantifierDistinct  |
  SetQuantifierAll 
  deriving (Eq, Ord, Read, Show)

_SetQuantifier = Core.Name "openGql.grammar.SetQuantifier"

_SetQuantifier_distinct = Core.Name "distinct"

_SetQuantifier_all = Core.Name "all"

data BinarySetFunctionType = 
  BinarySetFunctionTypePercentileCont  |
  BinarySetFunctionTypePercentileDisc 
  deriving (Eq, Ord, Read, Show)

_BinarySetFunctionType = Core.Name "openGql.grammar.BinarySetFunctionType"

_BinarySetFunctionType_percentileCont = Core.Name "percentileCont"

_BinarySetFunctionType_percentileDisc = Core.Name "percentileDisc"

data DependentValueExpression = 
  DependentValueExpression {
    dependentValueExpressionSetQuantifier :: (Maybe SetQuantifier),
    dependentValueExpressionNumericValue :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_DependentValueExpression = Core.Name "openGql.grammar.DependentValueExpression"

_DependentValueExpression_setQuantifier = Core.Name "setQuantifier"

_DependentValueExpression_numericValue = Core.Name "numericValue"

type IndependentValueExpression = NumericValueExpression

_IndependentValueExpression = Core.Name "openGql.grammar.IndependentValueExpression"

type ElementIdFunction = ElementVariableReference

_ElementIdFunction = Core.Name "openGql.grammar.ElementIdFunction"

data PropertyReference = 
  PropertyReference {
    propertyReferenceValueExpression :: PrimaryValueExpression,
    propertyReferencePropertyName :: PropertyName}
  deriving (Eq, Ord, Read, Show)

_PropertyReference = Core.Name "openGql.grammar.PropertyReference"

_PropertyReference_valueExpression = Core.Name "valueExpression"

_PropertyReference_propertyName = Core.Name "propertyName"

type BindingVariableReference = BindingVariable

_BindingVariableReference = Core.Name "openGql.grammar.BindingVariableReference"

type PathValueExpression = ValueExpression

_PathValueExpression = Core.Name "openGql.grammar.PathValueExpression"

type PathValueConstructor = PathValueConstructorByEnumeration

_PathValueConstructor = Core.Name "openGql.grammar.PathValueConstructor"

type PathValueConstructorByEnumeration = PathElementList

_PathValueConstructorByEnumeration = Core.Name "openGql.grammar.PathValueConstructorByEnumeration"

data PathElementList = 
  PathElementList {
    pathElementListStart :: PathElementListStart,
    pathElementListSteps :: [PathElementListStep]}
  deriving (Eq, Ord, Read, Show)

_PathElementList = Core.Name "openGql.grammar.PathElementList"

_PathElementList_start = Core.Name "start"

_PathElementList_steps = Core.Name "steps"

type PathElementListStart = NodeReferenceValueExpression

_PathElementListStart = Core.Name "openGql.grammar.PathElementListStart"

data PathElementListStep = 
  PathElementListStep {
    pathElementListStepEdgeReference :: EdgeReferenceValueExpression,
    pathElementListStepNodeReference :: NodeReferenceValueExpression}
  deriving (Eq, Ord, Read, Show)

_PathElementListStep = Core.Name "openGql.grammar.PathElementListStep"

_PathElementListStep_edgeReference = Core.Name "edgeReference"

_PathElementListStep_nodeReference = Core.Name "nodeReference"

type ListValueExpression = ValueExpression

_ListValueExpression = Core.Name "openGql.grammar.ListValueExpression"

data ListValueFunction = 
  ListValueFunctionTrim TrimListFunction |
  ListValueFunctionElements ElementsFunction
  deriving (Eq, Ord, Read, Show)

_ListValueFunction = Core.Name "openGql.grammar.ListValueFunction"

_ListValueFunction_trim = Core.Name "trim"

_ListValueFunction_elements = Core.Name "elements"

data TrimListFunction = 
  TrimListFunction {
    trimListFunctionListValue :: ListValueExpression,
    trimListFunctionNumericValue :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_TrimListFunction = Core.Name "openGql.grammar.TrimListFunction"

_TrimListFunction_listValue = Core.Name "listValue"

_TrimListFunction_numericValue = Core.Name "numericValue"

type ElementsFunction = PathValueExpression

_ElementsFunction = Core.Name "openGql.grammar.ElementsFunction"

type ListValueConstructor = ListValueConstructorByEnumeration

_ListValueConstructor = Core.Name "openGql.grammar.ListValueConstructor"

data ListValueConstructorByEnumeration = 
  ListValueConstructorByEnumeration {
    listValueConstructorByEnumerationListValueTypeName :: (Maybe ListValueTypeName),
    listValueConstructorByEnumerationElements :: (Maybe ListElementList)}
  deriving (Eq, Ord, Read, Show)

_ListValueConstructorByEnumeration = Core.Name "openGql.grammar.ListValueConstructorByEnumeration"

_ListValueConstructorByEnumeration_listValueTypeName = Core.Name "listValueTypeName"

_ListValueConstructorByEnumeration_elements = Core.Name "elements"

type ListElementList = [ListElement]

_ListElementList = Core.Name "openGql.grammar.ListElementList"

type ListElement = ValueExpression

_ListElement = Core.Name "openGql.grammar.ListElement"

type RecordConstructor = FieldsSpecification

_RecordConstructor = Core.Name "openGql.grammar.RecordConstructor"

type FieldsSpecification = (Maybe FieldList)

_FieldsSpecification = Core.Name "openGql.grammar.FieldsSpecification"

type FieldList = [Field]

_FieldList = Core.Name "openGql.grammar.FieldList"

data Field = 
  Field {
    fieldName :: FieldName,
    fieldValue :: ValueExpression}
  deriving (Eq, Ord, Read, Show)

_Field = Core.Name "openGql.grammar.Field"

_Field_name = Core.Name "name"

_Field_value = Core.Name "value"

type TruthValue = BooleanLiteral

_TruthValue = Core.Name "openGql.grammar.TruthValue"

data NumericValueExpression = 
  NumericValueExpressionSigned SignedNumericValueExpression |
  NumericValueExpressionMultiplicationOrDivision MulDivNumericValueExpression |
  NumericValueExpressionAdditionOrSubtraction AddSubNumericValueExpression |
  NumericValueExpressionPrimary PrimaryValueExpression |
  NumericValueExpressionFunction NumericValueFunction
  deriving (Eq, Ord, Read, Show)

_NumericValueExpression = Core.Name "openGql.grammar.NumericValueExpression"

_NumericValueExpression_signed = Core.Name "signed"

_NumericValueExpression_multiplicationOrDivision = Core.Name "multiplicationOrDivision"

_NumericValueExpression_additionOrSubtraction = Core.Name "additionOrSubtraction"

_NumericValueExpression_primary = Core.Name "primary"

_NumericValueExpression_function = Core.Name "function"

data SignedNumericValueExpression = 
  SignedNumericValueExpression {
    signedNumericValueExpressionSign :: Sign,
    signedNumericValueExpressionExpression :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_SignedNumericValueExpression = Core.Name "openGql.grammar.SignedNumericValueExpression"

_SignedNumericValueExpression_sign = Core.Name "sign"

_SignedNumericValueExpression_expression = Core.Name "expression"

data MulDivNumericValueExpression = 
  MulDivNumericValueExpression {
    mulDivNumericValueExpressionLeft :: NumericValueExpression,
    mulDivNumericValueExpressionOperator :: MultDivOperator,
    mulDivNumericValueExpressionRight :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_MulDivNumericValueExpression = Core.Name "openGql.grammar.MulDivNumericValueExpression"

_MulDivNumericValueExpression_left = Core.Name "left"

_MulDivNumericValueExpression_operator = Core.Name "operator"

_MulDivNumericValueExpression_right = Core.Name "right"

data AddSubNumericValueExpression = 
  AddSubNumericValueExpression {
    addSubNumericValueExpressionLeft :: NumericValueExpression,
    addSubNumericValueExpressionOperator :: AddSubtractOperator,
    addSubNumericValueExpressionRight :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_AddSubNumericValueExpression = Core.Name "openGql.grammar.AddSubNumericValueExpression"

_AddSubNumericValueExpression_left = Core.Name "left"

_AddSubNumericValueExpression_operator = Core.Name "operator"

_AddSubNumericValueExpression_right = Core.Name "right"

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

_NumericValueFunction = Core.Name "openGql.grammar.NumericValueFunction"

_NumericValueFunction_length = Core.Name "length"

_NumericValueFunction_cardinality = Core.Name "cardinality"

_NumericValueFunction_absoluteValue = Core.Name "absoluteValue"

_NumericValueFunction_modulus = Core.Name "modulus"

_NumericValueFunction_trigonometric = Core.Name "trigonometric"

_NumericValueFunction_logarithm = Core.Name "logarithm"

_NumericValueFunction_commonLogarithm = Core.Name "commonLogarithm"

_NumericValueFunction_naturalLogarithm = Core.Name "naturalLogarithm"

_NumericValueFunction_exponential = Core.Name "exponential"

_NumericValueFunction_power = Core.Name "power"

_NumericValueFunction_squareRoot = Core.Name "squareRoot"

_NumericValueFunction_floor = Core.Name "floor"

_NumericValueFunction_ceiling = Core.Name "ceiling"

data LengthExpression = 
  LengthExpressionChar CharLengthExpression |
  LengthExpressionByte ByteLengthExpression |
  LengthExpressionPath PathLengthExpression
  deriving (Eq, Ord, Read, Show)

_LengthExpression = Core.Name "openGql.grammar.LengthExpression"

_LengthExpression_char = Core.Name "char"

_LengthExpression_byte = Core.Name "byte"

_LengthExpression_path = Core.Name "path"

data CardinalityExpression = 
  CardinalityExpressionCardinality CardinalityArgumentExpression |
  CardinalityExpressionSize ListValueExpression
  deriving (Eq, Ord, Read, Show)

_CardinalityExpression = Core.Name "openGql.grammar.CardinalityExpression"

_CardinalityExpression_cardinality = Core.Name "cardinality"

_CardinalityExpression_size = Core.Name "size"

type CardinalityArgumentExpression = ValueExpression

_CardinalityArgumentExpression = Core.Name "openGql.grammar.CardinalityArgumentExpression"

type CharLengthExpression = CharacterStringValueExpression

_CharLengthExpression = Core.Name "openGql.grammar.CharLengthExpression"

type ByteLengthExpression = ByteStringValueExpression

_ByteLengthExpression = Core.Name "openGql.grammar.ByteLengthExpression"

type PathLengthExpression = PathValueExpression

_PathLengthExpression = Core.Name "openGql.grammar.PathLengthExpression"

type AbsoluteValueExpression = ValueExpression

_AbsoluteValueExpression = Core.Name "openGql.grammar.AbsoluteValueExpression"

data ModulusExpression = 
  ModulusExpression {
    modulusExpressionDividend :: NumericValueExpressionDividend,
    modulusExpressionDivisor :: NumericValueExpressionDivisor}
  deriving (Eq, Ord, Read, Show)

_ModulusExpression = Core.Name "openGql.grammar.ModulusExpression"

_ModulusExpression_dividend = Core.Name "dividend"

_ModulusExpression_divisor = Core.Name "divisor"

type NumericValueExpressionDividend = NumericValueExpression

_NumericValueExpressionDividend = Core.Name "openGql.grammar.NumericValueExpressionDividend"

type NumericValueExpressionDivisor = NumericValueExpression

_NumericValueExpressionDivisor = Core.Name "openGql.grammar.NumericValueExpressionDivisor"

data TrigonometricFunction = 
  TrigonometricFunction {
    trigonometricFunctionName :: TrigonometricFunctionName,
    trigonometricFunctionValue :: NumericValueExpression}
  deriving (Eq, Ord, Read, Show)

_TrigonometricFunction = Core.Name "openGql.grammar.TrigonometricFunction"

_TrigonometricFunction_name = Core.Name "name"

_TrigonometricFunction_value = Core.Name "value"

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

_TrigonometricFunctionName = Core.Name "openGql.grammar.TrigonometricFunctionName"

_TrigonometricFunctionName_sin = Core.Name "sin"

_TrigonometricFunctionName_cos = Core.Name "cos"

_TrigonometricFunctionName_tan = Core.Name "tan"

_TrigonometricFunctionName_cot = Core.Name "cot"

_TrigonometricFunctionName_sinh = Core.Name "sinh"

_TrigonometricFunctionName_cosh = Core.Name "cosh"

_TrigonometricFunctionName_tanh = Core.Name "tanh"

_TrigonometricFunctionName_asin = Core.Name "asin"

_TrigonometricFunctionName_acos = Core.Name "acos"

_TrigonometricFunctionName_atan = Core.Name "atan"

_TrigonometricFunctionName_degrees = Core.Name "degrees"

_TrigonometricFunctionName_radians = Core.Name "radians"

data GeneralLogarithmFunction = 
  GeneralLogarithmFunction {
    generalLogarithmFunctionBase :: GeneralLogarithmBase,
    generalLogarithmFunctionArgument :: GeneralLogarithmArgument}
  deriving (Eq, Ord, Read, Show)

_GeneralLogarithmFunction = Core.Name "openGql.grammar.GeneralLogarithmFunction"

_GeneralLogarithmFunction_base = Core.Name "base"

_GeneralLogarithmFunction_argument = Core.Name "argument"

type GeneralLogarithmBase = NumericValueExpression

_GeneralLogarithmBase = Core.Name "openGql.grammar.GeneralLogarithmBase"

type GeneralLogarithmArgument = NumericValueExpression

_GeneralLogarithmArgument = Core.Name "openGql.grammar.GeneralLogarithmArgument"

type CommonLogarithm = NumericValueExpression

_CommonLogarithm = Core.Name "openGql.grammar.CommonLogarithm"

type NaturalLogarithm = NumericValueExpression

_NaturalLogarithm = Core.Name "openGql.grammar.NaturalLogarithm"

type ExponentialFunction = NumericValueExpression

_ExponentialFunction = Core.Name "openGql.grammar.ExponentialFunction"

data PowerFunction = 
  PowerFunction {
    powerFunctionBase :: NumericValueExpressionBase,
    powerFunctionExponent :: NumericValueExpressionExponent}
  deriving (Eq, Ord, Read, Show)

_PowerFunction = Core.Name "openGql.grammar.PowerFunction"

_PowerFunction_base = Core.Name "base"

_PowerFunction_exponent = Core.Name "exponent"

type NumericValueExpressionBase = NumericValueExpression

_NumericValueExpressionBase = Core.Name "openGql.grammar.NumericValueExpressionBase"

type NumericValueExpressionExponent = NumericValueExpression

_NumericValueExpressionExponent = Core.Name "openGql.grammar.NumericValueExpressionExponent"

type SquareRoot = NumericValueExpression

_SquareRoot = Core.Name "openGql.grammar.SquareRoot"

type FloorFunction = NumericValueExpression

_FloorFunction = Core.Name "openGql.grammar.FloorFunction"

type CeilingFunction = NumericValueExpression

_CeilingFunction = Core.Name "openGql.grammar.CeilingFunction"

type CharacterStringValueExpression = ValueExpression

_CharacterStringValueExpression = Core.Name "openGql.grammar.CharacterStringValueExpression"

type ByteStringValueExpression = ValueExpression

_ByteStringValueExpression = Core.Name "openGql.grammar.ByteStringValueExpression"

data TrimOperands = 
  TrimOperands {
    trimOperandsSpecification :: (Maybe TrimSpecification),
    trimOperandsCharacterOrByteString :: (Maybe TrimCharacterOrByteString),
    trimOperandsSource :: TrimCharacterOrByteStringSource}
  deriving (Eq, Ord, Read, Show)

_TrimOperands = Core.Name "openGql.grammar.TrimOperands"

_TrimOperands_specification = Core.Name "specification"

_TrimOperands_characterOrByteString = Core.Name "characterOrByteString"

_TrimOperands_source = Core.Name "source"

type TrimCharacterOrByteStringSource = ValueExpression

_TrimCharacterOrByteStringSource = Core.Name "openGql.grammar.TrimCharacterOrByteStringSource"

data TrimSpecification = 
  TrimSpecificationLeading  |
  TrimSpecificationTrailing  |
  TrimSpecificationBoth 
  deriving (Eq, Ord, Read, Show)

_TrimSpecification = Core.Name "openGql.grammar.TrimSpecification"

_TrimSpecification_leading = Core.Name "leading"

_TrimSpecification_trailing = Core.Name "trailing"

_TrimSpecification_both = Core.Name "both"

type TrimCharacterOrByteString = ValueExpression

_TrimCharacterOrByteString = Core.Name "openGql.grammar.TrimCharacterOrByteString"

data NormalForm = 
  NormalFormNfc  |
  NormalFormNfd  |
  NormalFormNfkc  |
  NormalFormNfkd 
  deriving (Eq, Ord, Read, Show)

_NormalForm = Core.Name "openGql.grammar.NormalForm"

_NormalForm_nfc = Core.Name "nfc"

_NormalForm_nfd = Core.Name "nfd"

_NormalForm_nfkc = Core.Name "nfkc"

_NormalForm_nfkd = Core.Name "nfkd"

type StringLength = NumericValueExpression

_StringLength = Core.Name "openGql.grammar.StringLength"

type DatetimeValueExpression = ValueExpression

_DatetimeValueExpression = Core.Name "openGql.grammar.DatetimeValueExpression"

data DatetimeValueFunction = 
  DatetimeValueFunctionDateFunction DateFunction |
  DatetimeValueFunctionTimeFunction TimeFunction |
  DatetimeValueFunctionDatetimeFunction DatetimeFunction |
  DatetimeValueFunctionLocaltimeFunction LocaltimeFunction |
  DatetimeValueFunctionLocaldatetimeFunction LocaldatetimeFunction
  deriving (Eq, Ord, Read, Show)

_DatetimeValueFunction = Core.Name "openGql.grammar.DatetimeValueFunction"

_DatetimeValueFunction_dateFunction = Core.Name "dateFunction"

_DatetimeValueFunction_timeFunction = Core.Name "timeFunction"

_DatetimeValueFunction_datetimeFunction = Core.Name "datetimeFunction"

_DatetimeValueFunction_localtimeFunction = Core.Name "localtimeFunction"

_DatetimeValueFunction_localdatetimeFunction = Core.Name "localdatetimeFunction"

data DateFunction = 
  DateFunctionCurrentDate  |
  DateFunctionDateWithParams (Maybe DateFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_DateFunction = Core.Name "openGql.grammar.DateFunction"

_DateFunction_currentDate = Core.Name "currentDate"

_DateFunction_dateWithParams = Core.Name "dateWithParams"

data TimeFunction = 
  TimeFunctionCurrentTime  |
  TimeFunctionZonedTimeWithParams (Maybe TimeFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_TimeFunction = Core.Name "openGql.grammar.TimeFunction"

_TimeFunction_currentTime = Core.Name "currentTime"

_TimeFunction_zonedTimeWithParams = Core.Name "zonedTimeWithParams"

type LocaltimeFunction = (Maybe TimeFunctionParameters)

_LocaltimeFunction = Core.Name "openGql.grammar.LocaltimeFunction"

data DatetimeFunction = 
  DatetimeFunctionCurrentTimestamp  |
  DatetimeFunctionZonedDatetimeWithParams (Maybe DatetimeFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_DatetimeFunction = Core.Name "openGql.grammar.DatetimeFunction"

_DatetimeFunction_currentTimestamp = Core.Name "currentTimestamp"

_DatetimeFunction_zonedDatetimeWithParams = Core.Name "zonedDatetimeWithParams"

data LocaldatetimeFunction = 
  LocaldatetimeFunctionLocalTimestamp  |
  LocaldatetimeFunctionLocalDatetimeWithParams (Maybe DatetimeFunctionParameters)
  deriving (Eq, Ord, Read, Show)

_LocaldatetimeFunction = Core.Name "openGql.grammar.LocaldatetimeFunction"

_LocaldatetimeFunction_localTimestamp = Core.Name "localTimestamp"

_LocaldatetimeFunction_localDatetimeWithParams = Core.Name "localDatetimeWithParams"

data DateFunctionParameters = 
  DateFunctionParametersDateString DateString |
  DateFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_DateFunctionParameters = Core.Name "openGql.grammar.DateFunctionParameters"

_DateFunctionParameters_dateString = Core.Name "dateString"

_DateFunctionParameters_recordConstructor = Core.Name "recordConstructor"

data TimeFunctionParameters = 
  TimeFunctionParametersTimeString TimeString |
  TimeFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_TimeFunctionParameters = Core.Name "openGql.grammar.TimeFunctionParameters"

_TimeFunctionParameters_timeString = Core.Name "timeString"

_TimeFunctionParameters_recordConstructor = Core.Name "recordConstructor"

data DatetimeFunctionParameters = 
  DatetimeFunctionParametersDatetimeString DatetimeString |
  DatetimeFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_DatetimeFunctionParameters = Core.Name "openGql.grammar.DatetimeFunctionParameters"

_DatetimeFunctionParameters_datetimeString = Core.Name "datetimeString"

_DatetimeFunctionParameters_recordConstructor = Core.Name "recordConstructor"

type DurationValueExpression = ValueExpression

_DurationValueExpression = Core.Name "openGql.grammar.DurationValueExpression"

data DatetimeSubtraction = 
  DatetimeSubtraction {
    datetimeSubtractionParameters :: DatetimeSubtractionParameters,
    datetimeSubtractionTemporalDurationQualifier :: (Maybe TemporalDurationQualifier)}
  deriving (Eq, Ord, Read, Show)

_DatetimeSubtraction = Core.Name "openGql.grammar.DatetimeSubtraction"

_DatetimeSubtraction_parameters = Core.Name "parameters"

_DatetimeSubtraction_temporalDurationQualifier = Core.Name "temporalDurationQualifier"

data DatetimeSubtractionParameters = 
  DatetimeSubtractionParameters {
    datetimeSubtractionParametersExpression1 :: DatetimeValueExpression1,
    datetimeSubtractionParametersExpression2 :: DatetimeValueExpression2}
  deriving (Eq, Ord, Read, Show)

_DatetimeSubtractionParameters = Core.Name "openGql.grammar.DatetimeSubtractionParameters"

_DatetimeSubtractionParameters_expression1 = Core.Name "expression1"

_DatetimeSubtractionParameters_expression2 = Core.Name "expression2"

type DatetimeValueExpression1 = DatetimeValueExpression

_DatetimeValueExpression1 = Core.Name "openGql.grammar.DatetimeValueExpression1"

type DatetimeValueExpression2 = DatetimeValueExpression

_DatetimeValueExpression2 = Core.Name "openGql.grammar.DatetimeValueExpression2"

data DurationValueFunction = 
  DurationValueFunctionDurationFunction DurationFunction |
  DurationValueFunctionAbsoluteValue AbsoluteValueExpression
  deriving (Eq, Ord, Read, Show)

_DurationValueFunction = Core.Name "openGql.grammar.DurationValueFunction"

_DurationValueFunction_durationFunction = Core.Name "durationFunction"

_DurationValueFunction_absoluteValue = Core.Name "absoluteValue"

type DurationFunction = DurationFunctionParameters

_DurationFunction = Core.Name "openGql.grammar.DurationFunction"

data DurationFunctionParameters = 
  DurationFunctionParametersDurationString DurationString |
  DurationFunctionParametersRecordConstructor RecordConstructor
  deriving (Eq, Ord, Read, Show)

_DurationFunctionParameters = Core.Name "openGql.grammar.DurationFunctionParameters"

_DurationFunctionParameters_durationString = Core.Name "durationString"

_DurationFunctionParameters_recordConstructor = Core.Name "recordConstructor"

type ObjectName = String

_ObjectName = Core.Name "openGql.grammar.ObjectName"

type ObjectNameOrBindingVariable = String

_ObjectNameOrBindingVariable = Core.Name "openGql.grammar.ObjectNameOrBindingVariable"

type DirectoryName = String

_DirectoryName = Core.Name "openGql.grammar.DirectoryName"

type SchemaName = String

_SchemaName = Core.Name "openGql.grammar.SchemaName"

type GraphName = String

_GraphName = Core.Name "openGql.grammar.GraphName"

type DelimitedGraphName = String

_DelimitedGraphName = Core.Name "openGql.grammar.DelimitedGraphName"

type GraphTypeName = String

_GraphTypeName = Core.Name "openGql.grammar.GraphTypeName"

type NodeTypeName = String

_NodeTypeName = Core.Name "openGql.grammar.NodeTypeName"

type EdgeTypeName = String

_EdgeTypeName = Core.Name "openGql.grammar.EdgeTypeName"

data BindingTableName = 
  BindingTableNameRegularIdentifier String |
  BindingTableNameDelimitedBindingTableName DelimitedBindingTableName
  deriving (Eq, Ord, Read, Show)

_BindingTableName = Core.Name "openGql.grammar.BindingTableName"

_BindingTableName_regularIdentifier = Core.Name "regularIdentifier"

_BindingTableName_delimitedBindingTableName = Core.Name "delimitedBindingTableName"

type DelimitedBindingTableName = String

_DelimitedBindingTableName = Core.Name "openGql.grammar.DelimitedBindingTableName"

type ProcedureName = String

_ProcedureName = Core.Name "openGql.grammar.ProcedureName"

type LabelName = String

_LabelName = Core.Name "openGql.grammar.LabelName"

type PropertyName = String

_PropertyName = Core.Name "openGql.grammar.PropertyName"

type FieldName = String

_FieldName = Core.Name "openGql.grammar.FieldName"

type ElementVariable = BindingVariable

_ElementVariable = Core.Name "openGql.grammar.ElementVariable"

type PathVariable = BindingVariable

_PathVariable = Core.Name "openGql.grammar.PathVariable"

type SubpathVariable = String

_SubpathVariable = Core.Name "openGql.grammar.SubpathVariable"

type BindingVariable = String

_BindingVariable = Core.Name "openGql.grammar.BindingVariable"

data UnsignedLiteral = 
  UnsignedLiteralNumeric UnsignedNumericLiteral |
  UnsignedLiteralGeneral GeneralLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedLiteral = Core.Name "openGql.grammar.UnsignedLiteral"

_UnsignedLiteral_numeric = Core.Name "numeric"

_UnsignedLiteral_general = Core.Name "general"

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

_GeneralLiteral = Core.Name "openGql.grammar.GeneralLiteral"

_GeneralLiteral_boolean = Core.Name "boolean"

_GeneralLiteral_characterString = Core.Name "characterString"

_GeneralLiteral_byteString = Core.Name "byteString"

_GeneralLiteral_temporal = Core.Name "temporal"

_GeneralLiteral_duration = Core.Name "duration"

_GeneralLiteral_nullLiteral = Core.Name "nullLiteral"

_GeneralLiteral_list = Core.Name "list"

_GeneralLiteral_record = Core.Name "record"

data TemporalLiteral = 
  TemporalLiteralDate DateLiteral |
  TemporalLiteralTime TimeLiteral |
  TemporalLiteralDatetime DatetimeLiteral
  deriving (Eq, Ord, Read, Show)

_TemporalLiteral = Core.Name "openGql.grammar.TemporalLiteral"

_TemporalLiteral_date = Core.Name "date"

_TemporalLiteral_time = Core.Name "time"

_TemporalLiteral_datetime = Core.Name "datetime"

type DateLiteral = DateString

_DateLiteral = Core.Name "openGql.grammar.DateLiteral"

type TimeLiteral = TimeString

_TimeLiteral = Core.Name "openGql.grammar.TimeLiteral"

type DatetimeLiteral = DatetimeString

_DatetimeLiteral = Core.Name "openGql.grammar.DatetimeLiteral"

type ListLiteral = ListValueConstructorByEnumeration

_ListLiteral = Core.Name "openGql.grammar.ListLiteral"

type RecordLiteral = RecordConstructor

_RecordLiteral = Core.Name "openGql.grammar.RecordLiteral"

type Identifier = String

_Identifier = Core.Name "openGql.grammar.Identifier"

type RegularIdentifier = String

_RegularIdentifier = Core.Name "openGql.grammar.RegularIdentifier"

type TimeZoneString = CharacterStringLiteral

_TimeZoneString = Core.Name "openGql.grammar.TimeZoneString"

type CharacterStringLiteral = String

_CharacterStringLiteral = Core.Name "openGql.grammar.CharacterStringLiteral"

data UnsignedNumericLiteral = 
  UnsignedNumericLiteralExact ExactNumericLiteral |
  UnsignedNumericLiteralApproximate ApproximateNumericLiteral
  deriving (Eq, Ord, Read, Show)

_UnsignedNumericLiteral = Core.Name "openGql.grammar.UnsignedNumericLiteral"

_UnsignedNumericLiteral_exact = Core.Name "exact"

_UnsignedNumericLiteral_approximate = Core.Name "approximate"

data ExactNumericLiteral = 
  ExactNumericLiteralScientificWithSuffix String |
  ExactNumericLiteralCommonWithSuffix String |
  ExactNumericLiteralCommonWithoutSuffix String |
  ExactNumericLiteralIntegerWithSuffix String |
  ExactNumericLiteralUnsignedInteger UnsignedInteger
  deriving (Eq, Ord, Read, Show)

_ExactNumericLiteral = Core.Name "openGql.grammar.ExactNumericLiteral"

_ExactNumericLiteral_scientificWithSuffix = Core.Name "scientificWithSuffix"

_ExactNumericLiteral_commonWithSuffix = Core.Name "commonWithSuffix"

_ExactNumericLiteral_commonWithoutSuffix = Core.Name "commonWithoutSuffix"

_ExactNumericLiteral_integerWithSuffix = Core.Name "integerWithSuffix"

_ExactNumericLiteral_unsignedInteger = Core.Name "unsignedInteger"

data ApproximateNumericLiteral = 
  ApproximateNumericLiteralScientificWithSuffix String |
  ApproximateNumericLiteralScientificWithoutSuffix String |
  ApproximateNumericLiteralCommonWithSuffix String |
  ApproximateNumericLiteralIntegerWithSuffix String
  deriving (Eq, Ord, Read, Show)

_ApproximateNumericLiteral = Core.Name "openGql.grammar.ApproximateNumericLiteral"

_ApproximateNumericLiteral_scientificWithSuffix = Core.Name "scientificWithSuffix"

_ApproximateNumericLiteral_scientificWithoutSuffix = Core.Name "scientificWithoutSuffix"

_ApproximateNumericLiteral_commonWithSuffix = Core.Name "commonWithSuffix"

_ApproximateNumericLiteral_integerWithSuffix = Core.Name "integerWithSuffix"

data UnsignedInteger = 
  UnsignedIntegerDecimal String |
  UnsignedIntegerHexadecimal String |
  UnsignedIntegerOctal String |
  UnsignedIntegerBinary String
  deriving (Eq, Ord, Read, Show)

_UnsignedInteger = Core.Name "openGql.grammar.UnsignedInteger"

_UnsignedInteger_decimal = Core.Name "decimal"

_UnsignedInteger_hexadecimal = Core.Name "hexadecimal"

_UnsignedInteger_octal = Core.Name "octal"

_UnsignedInteger_binary = Core.Name "binary"

type UnsignedDecimalInteger = String

_UnsignedDecimalInteger = Core.Name "openGql.grammar.UnsignedDecimalInteger"

type NullLiteral = ()

_NullLiteral = Core.Name "openGql.grammar.NullLiteral"

type DateString = CharacterStringLiteral

_DateString = Core.Name "openGql.grammar.DateString"

type TimeString = CharacterStringLiteral

_TimeString = Core.Name "openGql.grammar.TimeString"

type DatetimeString = CharacterStringLiteral

_DatetimeString = Core.Name "openGql.grammar.DatetimeString"

type DurationLiteral = DurationString

_DurationLiteral = Core.Name "openGql.grammar.DurationLiteral"

type DurationString = CharacterStringLiteral

_DurationString = Core.Name "openGql.grammar.DurationString"

data NodeSynonym = 
  NodeSynonymNode  |
  NodeSynonymVertex 
  deriving (Eq, Ord, Read, Show)

_NodeSynonym = Core.Name "openGql.grammar.NodeSynonym"

_NodeSynonym_node = Core.Name "node"

_NodeSynonym_vertex = Core.Name "vertex"

data EdgesSynonym = 
  EdgesSynonymEdges  |
  EdgesSynonymRelationships 
  deriving (Eq, Ord, Read, Show)

_EdgesSynonym = Core.Name "openGql.grammar.EdgesSynonym"

_EdgesSynonym_edges = Core.Name "edges"

_EdgesSynonym_relationships = Core.Name "relationships"

data EdgeSynonym = 
  EdgeSynonymEdge  |
  EdgeSynonymRelationship 
  deriving (Eq, Ord, Read, Show)

_EdgeSynonym = Core.Name "openGql.grammar.EdgeSynonym"

_EdgeSynonym_edge = Core.Name "edge"

_EdgeSynonym_relationship = Core.Name "relationship"

data Implies = 
  ImpliesRightDoubleArrow  |
  ImpliesImplies 
  deriving (Eq, Ord, Read, Show)

_Implies = Core.Name "openGql.grammar.Implies"

_Implies_rightDoubleArrow = Core.Name "rightDoubleArrow"

_Implies_implies = Core.Name "implies"

type ParameterName = String

_ParameterName = Core.Name "openGql.grammar.ParameterName"

data BooleanLiteral = 
  BooleanLiteralTrue  |
  BooleanLiteralFalse  |
  BooleanLiteralUnknown 
  deriving (Eq, Ord, Read, Show)

_BooleanLiteral = Core.Name "openGql.grammar.BooleanLiteral"

_BooleanLiteral_true = Core.Name "true"

_BooleanLiteral_false = Core.Name "false"

_BooleanLiteral_unknown = Core.Name "unknown"

type ByteStringLiteral = String

_ByteStringLiteral = Core.Name "openGql.grammar.ByteStringLiteral"
