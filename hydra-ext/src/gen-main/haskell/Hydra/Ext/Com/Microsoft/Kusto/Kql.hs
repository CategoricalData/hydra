-- | A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative.

module Hydra.Ext.Com.Microsoft.Kusto.Kql where

import qualified Hydra.Core as Core
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data BetweenExpression = 
  BetweenExpression {
    betweenExpressionNot :: Bool,
    betweenExpressionExpression :: Expression,
    betweenExpressionLowerBound :: Expression,
    betweenExpressionUpperBound :: Expression}
  deriving (Eq, Ord, Read, Show)

_BetweenExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.BetweenExpression")

_BetweenExpression_not = (Core.Name "not")

_BetweenExpression_expression = (Core.Name "expression")

_BetweenExpression_lowerBound = (Core.Name "lowerBound")

_BetweenExpression_upperBound = (Core.Name "upperBound")

data BinaryExpression = 
  BinaryExpression {
    binaryExpressionLeft :: Expression,
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.BinaryExpression")

_BinaryExpression_left = (Core.Name "left")

_BinaryExpression_operator = (Core.Name "operator")

_BinaryExpression_right = (Core.Name "right")

data BinaryOperator = 
  BinaryOperatorCaseInsensitiveEqual  |
  BinaryOperatorContains  |
  BinaryOperatorDivide  |
  BinaryOperatorEndsWith  |
  BinaryOperatorEqual  |
  BinaryOperatorGreater  |
  BinaryOperatorGreaterOrEqual  |
  BinaryOperatorHas  |
  BinaryOperatorHasPrefix  |
  BinaryOperatorHasSuffix  |
  BinaryOperatorLess  |
  BinaryOperatorLessOrEqual  |
  BinaryOperatorMatchesRegex  |
  BinaryOperatorMinus  |
  BinaryOperatorNotEqual  |
  BinaryOperatorPlus  |
  BinaryOperatorStartsWith  |
  BinaryOperatorTimes 
  deriving (Eq, Ord, Read, Show)

_BinaryOperator = (Core.Name "hydra.ext.com.microsoft.kusto.kql.BinaryOperator")

_BinaryOperator_caseInsensitiveEqual = (Core.Name "caseInsensitiveEqual")

_BinaryOperator_contains = (Core.Name "contains")

_BinaryOperator_divide = (Core.Name "divide")

_BinaryOperator_endsWith = (Core.Name "endsWith")

_BinaryOperator_equal = (Core.Name "equal")

_BinaryOperator_greater = (Core.Name "greater")

_BinaryOperator_greaterOrEqual = (Core.Name "greaterOrEqual")

_BinaryOperator_has = (Core.Name "has")

_BinaryOperator_hasPrefix = (Core.Name "hasPrefix")

_BinaryOperator_hasSuffix = (Core.Name "hasSuffix")

_BinaryOperator_less = (Core.Name "less")

_BinaryOperator_lessOrEqual = (Core.Name "lessOrEqual")

_BinaryOperator_matchesRegex = (Core.Name "matchesRegex")

_BinaryOperator_minus = (Core.Name "minus")

_BinaryOperator_notEqual = (Core.Name "notEqual")

_BinaryOperator_plus = (Core.Name "plus")

_BinaryOperator_startsWith = (Core.Name "startsWith")

_BinaryOperator_times = (Core.Name "times")

data BuiltInFunction = 
  BuiltInFunctionAgo  |
  BuiltInFunctionBin  |
  BuiltInFunctionCount  |
  BuiltInFunctionDcount  |
  BuiltInFunctionEndofday  |
  BuiltInFunctionExtract  |
  BuiltInFunctionFormat_datetime  |
  BuiltInFunctionMaterialize  |
  BuiltInFunctionNow  |
  BuiltInFunctionRange  |
  BuiltInFunctionStartofday  |
  BuiltInFunctionStrcat  |
  BuiltInFunctionTodynamic 
  deriving (Eq, Ord, Read, Show)

_BuiltInFunction = (Core.Name "hydra.ext.com.microsoft.kusto.kql.BuiltInFunction")

_BuiltInFunction_ago = (Core.Name "ago")

_BuiltInFunction_bin = (Core.Name "bin")

_BuiltInFunction_count = (Core.Name "count")

_BuiltInFunction_dcount = (Core.Name "dcount")

_BuiltInFunction_endofday = (Core.Name "endofday")

_BuiltInFunction_extract = (Core.Name "extract")

_BuiltInFunction_format_datetime = (Core.Name "format_datetime")

_BuiltInFunction_materialize = (Core.Name "materialize")

_BuiltInFunction_now = (Core.Name "now")

_BuiltInFunction_range = (Core.Name "range")

_BuiltInFunction_startofday = (Core.Name "startofday")

_BuiltInFunction_strcat = (Core.Name "strcat")

_BuiltInFunction_todynamic = (Core.Name "todynamic")

data ColumnAlias = 
  ColumnAlias {
    columnAliasColumn :: ColumnName,
    columnAliasAlias :: ColumnName}
  deriving (Eq, Ord, Read, Show)

_ColumnAlias = (Core.Name "hydra.ext.com.microsoft.kusto.kql.ColumnAlias")

_ColumnAlias_column = (Core.Name "column")

_ColumnAlias_alias = (Core.Name "alias")

data ColumnAssignment = 
  ColumnAssignment {
    columnAssignmentColumn :: ColumnName,
    columnAssignmentExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ColumnAssignment = (Core.Name "hydra.ext.com.microsoft.kusto.kql.ColumnAssignment")

_ColumnAssignment_column = (Core.Name "column")

_ColumnAssignment_expression = (Core.Name "expression")

newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra.ext.com.microsoft.kusto.kql.ColumnName")

data Columns = 
  ColumnsAll  |
  ColumnsSingle ColumnName
  deriving (Eq, Ord, Read, Show)

_Columns = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Columns")

_Columns_all = (Core.Name "all")

_Columns_single = (Core.Name "single")

data Command = 
  CommandCount  |
  -- | See https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/distinct-operator
  CommandDistinct [ColumnName] |
  CommandExtend [ColumnAssignment] |
  CommandJoin JoinCommand |
  CommandLimit Int |
  CommandMvexpand ColumnName |
  CommandOrderBy [SortBy] |
  CommandParse ParseCommand |
  CommandPrint PrintCommand |
  CommandProject [Projection] |
  CommandProjectAway [ColumnName] |
  CommandProjectRename [ColumnAlias] |
  CommandRender String |
  CommandSearch SearchCommand |
  CommandSortBy [SortBy] |
  CommandSummarize SummarizeCommand |
  -- | Limit a search to a specified number of results
  CommandTake Int |
  CommandTop TopCommand |
  CommandUnion UnionCommand |
  CommandWhere Expression
  deriving (Eq, Ord, Read, Show)

_Command = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Command")

_Command_count = (Core.Name "count")

_Command_distinct = (Core.Name "distinct")

_Command_extend = (Core.Name "extend")

_Command_join = (Core.Name "join")

_Command_limit = (Core.Name "limit")

_Command_mvexpand = (Core.Name "mvexpand")

_Command_orderBy = (Core.Name "orderBy")

_Command_parse = (Core.Name "parse")

_Command_print = (Core.Name "print")

_Command_project = (Core.Name "project")

_Command_projectAway = (Core.Name "projectAway")

_Command_projectRename = (Core.Name "projectRename")

_Command_render = (Core.Name "render")

_Command_search = (Core.Name "search")

_Command_sortBy = (Core.Name "sortBy")

_Command_summarize = (Core.Name "summarize")

_Command_take = (Core.Name "take")

_Command_top = (Core.Name "top")

_Command_union = (Core.Name "union")

_Command_where = (Core.Name "where")

newtype Datetime = 
  Datetime {
    unDatetime :: String}
  deriving (Eq, Ord, Read, Show)

_Datetime = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Datetime")

data Duration = 
  Duration {
    durationValue :: Int,
    durationUnit :: DurationUnit}
  deriving (Eq, Ord, Read, Show)

_Duration = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Duration")

_Duration_value = (Core.Name "value")

_Duration_unit = (Core.Name "unit")

data DurationUnit = 
  DurationUnitSecond  |
  DurationUnitMinute  |
  DurationUnitHour 
  deriving (Eq, Ord, Read, Show)

_DurationUnit = (Core.Name "hydra.ext.com.microsoft.kusto.kql.DurationUnit")

_DurationUnit_second = (Core.Name "second")

_DurationUnit_minute = (Core.Name "minute")

_DurationUnit_hour = (Core.Name "hour")

data Expression = 
  ExpressionAnd [Expression] |
  ExpressionAny  |
  ExpressionBetween BetweenExpression |
  ExpressionBinary BinaryExpression |
  ExpressionBraces Expression |
  ExpressionColumn ColumnName |
  ExpressionDataset TableName |
  ExpressionIndex IndexExpression |
  ExpressionList [Expression] |
  ExpressionLiteral Literal |
  ExpressionOr [Expression] |
  ExpressionParentheses Expression |
  ExpressionProperty PropertyExpression |
  ExpressionUnary UnaryExpression
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Expression")

_Expression_and = (Core.Name "and")

_Expression_any = (Core.Name "any")

_Expression_between = (Core.Name "between")

_Expression_binary = (Core.Name "binary")

_Expression_braces = (Core.Name "braces")

_Expression_column = (Core.Name "column")

_Expression_dataset = (Core.Name "dataset")

_Expression_index = (Core.Name "index")

_Expression_list = (Core.Name "list")

_Expression_literal = (Core.Name "literal")

_Expression_or = (Core.Name "or")

_Expression_parentheses = (Core.Name "parentheses")

_Expression_property = (Core.Name "property")

_Expression_unary = (Core.Name "unary")

data Function = 
  FunctionBuiltIn BuiltInFunction |
  FunctionCustom FunctionName
  deriving (Eq, Ord, Read, Show)

_Function = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Function")

_Function_builtIn = (Core.Name "builtIn")

_Function_custom = (Core.Name "custom")

data FunctionExpression = 
  FunctionExpression {
    functionExpressionFunction :: Function,
    functionExpressionArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.FunctionExpression")

_FunctionExpression_function = (Core.Name "function")

_FunctionExpression_arguments = (Core.Name "arguments")

newtype FunctionName = 
  FunctionName {
    unFunctionName :: String}
  deriving (Eq, Ord, Read, Show)

_FunctionName = (Core.Name "hydra.ext.com.microsoft.kusto.kql.FunctionName")

data IndexExpression = 
  IndexExpression {
    indexExpressionExpression :: Expression,
    indexExpressionIndex :: String}
  deriving (Eq, Ord, Read, Show)

_IndexExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.IndexExpression")

_IndexExpression_expression = (Core.Name "expression")

_IndexExpression_index = (Core.Name "index")

data JoinCommand = 
  JoinCommand {
    joinCommandKind :: JoinKind,
    joinCommandExpression :: TableName,
    joinCommandOn :: Expression}
  deriving (Eq, Ord, Read, Show)

_JoinCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.JoinCommand")

_JoinCommand_kind = (Core.Name "kind")

_JoinCommand_expression = (Core.Name "expression")

_JoinCommand_on = (Core.Name "on")

data JoinKind = 
  JoinKindLeftouter  |
  JoinKindLeftsemi  |
  JoinKindLeftanti  |
  JoinKindFullouter  |
  JoinKindInner  |
  JoinKindInnerunique  |
  JoinKindRightouter  |
  JoinKindRightsemi  |
  JoinKindRightanti 
  deriving (Eq, Ord, Read, Show)

_JoinKind = (Core.Name "hydra.ext.com.microsoft.kusto.kql.JoinKind")

_JoinKind_leftouter = (Core.Name "leftouter")

_JoinKind_leftsemi = (Core.Name "leftsemi")

_JoinKind_leftanti = (Core.Name "leftanti")

_JoinKind_fullouter = (Core.Name "fullouter")

_JoinKind_inner = (Core.Name "inner")

_JoinKind_innerunique = (Core.Name "innerunique")

_JoinKind_rightouter = (Core.Name "rightouter")

_JoinKind_rightsemi = (Core.Name "rightsemi")

_JoinKind_rightanti = (Core.Name "rightanti")

data KeyValuePair = 
  KeyValuePair {
    keyValuePairKey :: String,
    keyValuePairValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_KeyValuePair = (Core.Name "hydra.ext.com.microsoft.kusto.kql.KeyValuePair")

_KeyValuePair_key = (Core.Name "key")

_KeyValuePair_value = (Core.Name "value")

data LetBinding = 
  LetBinding {
    letBindingName :: ColumnName,
    letBindingExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_LetBinding = (Core.Name "hydra.ext.com.microsoft.kusto.kql.LetBinding")

_LetBinding_name = (Core.Name "name")

_LetBinding_expression = (Core.Name "expression")

data LetExpression = 
  LetExpression {
    letExpressionBindings :: [LetBinding],
    letExpressionExpression :: TabularExpression}
  deriving (Eq, Ord, Read, Show)

_LetExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.LetExpression")

_LetExpression_bindings = (Core.Name "bindings")

_LetExpression_expression = (Core.Name "expression")

data Literal = 
  LiteralDuration Duration |
  LiteralDatetime Datetime |
  LiteralString String |
  LiteralInt Int |
  LiteralLong I.Int64 |
  LiteralDouble Double |
  LiteralBoolean Bool
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Literal")

_Literal_duration = (Core.Name "duration")

_Literal_datetime = (Core.Name "datetime")

_Literal_string = (Core.Name "string")

_Literal_int = (Core.Name "int")

_Literal_long = (Core.Name "long")

_Literal_double = (Core.Name "double")

_Literal_boolean = (Core.Name "boolean")

data Order = 
  OrderAscending  |
  OrderDescending 
  deriving (Eq, Ord, Read, Show)

_Order = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Order")

_Order_ascending = (Core.Name "ascending")

_Order_descending = (Core.Name "descending")

data Parameter = 
  Parameter {
    parameterKey :: String,
    parameterValue :: Literal}
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Parameter")

_Parameter_key = (Core.Name "key")

_Parameter_value = (Core.Name "value")

data ParseCommand = 
  ParseCommand {
    parseCommandColumn :: ColumnName,
    parseCommandPairs :: [KeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_ParseCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.ParseCommand")

_ParseCommand_column = (Core.Name "column")

_ParseCommand_pairs = (Core.Name "pairs")

newtype PipelineExpression = 
  PipelineExpression {
    unPipelineExpression :: [TabularExpression]}
  deriving (Eq, Ord, Read, Show)

_PipelineExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.PipelineExpression")

data PrintCommand = 
  PrintCommand {
    printCommandColumn :: (Maybe ColumnName),
    printCommandExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_PrintCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.PrintCommand")

_PrintCommand_column = (Core.Name "column")

_PrintCommand_expression = (Core.Name "expression")

data Projection = 
  Projection {
    projectionExpression :: Expression,
    projectionAlias :: (Maybe ColumnName)}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Projection")

_Projection_expression = (Core.Name "expression")

_Projection_alias = (Core.Name "alias")

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionExpression :: Expression,
    propertyExpressionProperty :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.PropertyExpression")

_PropertyExpression_expression = (Core.Name "expression")

_PropertyExpression_property = (Core.Name "property")

newtype Query = 
  Query {
    unQuery :: TabularExpression}
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra.ext.com.microsoft.kusto.kql.Query")

-- | Search across all datasets and columns or, if provided, specific datasets and/or columns
data SearchCommand = 
  SearchCommand {
    searchCommandDatasets :: [TableName],
    searchCommandPattern :: Expression}
  deriving (Eq, Ord, Read, Show)

_SearchCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.SearchCommand")

_SearchCommand_datasets = (Core.Name "datasets")

_SearchCommand_pattern = (Core.Name "pattern")

data SummarizeCommand = 
  SummarizeCommand {
    summarizeCommandColumns :: [ColumnAssignment],
    summarizeCommandBy :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_SummarizeCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.SummarizeCommand")

_SummarizeCommand_columns = (Core.Name "columns")

_SummarizeCommand_by = (Core.Name "by")

newtype TableName = 
  TableName {
    unTableName :: String}
  deriving (Eq, Ord, Read, Show)

_TableName = (Core.Name "hydra.ext.com.microsoft.kusto.kql.TableName")

data TopCommand = 
  TopCommand {
    topCommandCount :: Int,
    topCommandSort :: [SortBy]}
  deriving (Eq, Ord, Read, Show)

_TopCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.TopCommand")

_TopCommand_count = (Core.Name "count")

_TopCommand_sort = (Core.Name "sort")

data SortBy = 
  SortBy {
    sortByColumn :: ColumnName,
    sortByOrder :: (Maybe Order)}
  deriving (Eq, Ord, Read, Show)

_SortBy = (Core.Name "hydra.ext.com.microsoft.kusto.kql.SortBy")

_SortBy_column = (Core.Name "column")

_SortBy_order = (Core.Name "order")

data TabularExpression = 
  TabularExpressionCommand Command |
  TabularExpressionPipeline PipelineExpression |
  TabularExpressionLet LetExpression |
  TabularExpressionTable TableName
  deriving (Eq, Ord, Read, Show)

_TabularExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.TabularExpression")

_TabularExpression_command = (Core.Name "command")

_TabularExpression_pipeline = (Core.Name "pipeline")

_TabularExpression_let = (Core.Name "let")

_TabularExpression_table = (Core.Name "table")

data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra.ext.com.microsoft.kusto.kql.UnaryExpression")

_UnaryExpression_operator = (Core.Name "operator")

_UnaryExpression_expression = (Core.Name "expression")

data UnaryOperator = 
  UnaryOperatorNot 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra.ext.com.microsoft.kusto.kql.UnaryOperator")

_UnaryOperator_not = (Core.Name "not")

data UnionCommand = 
  UnionCommand {
    unionCommandParameters :: [Parameter],
    unionCommandKind :: (Maybe UnionKind),
    unionCommandWithSource :: (Maybe ColumnName),
    unionCommandIsFuzzy :: (Maybe Bool),
    unionCommandTables :: [TableName]}
  deriving (Eq, Ord, Read, Show)

_UnionCommand = (Core.Name "hydra.ext.com.microsoft.kusto.kql.UnionCommand")

_UnionCommand_parameters = (Core.Name "parameters")

_UnionCommand_kind = (Core.Name "kind")

_UnionCommand_withSource = (Core.Name "withSource")

_UnionCommand_isFuzzy = (Core.Name "isFuzzy")

_UnionCommand_tables = (Core.Name "tables")

data UnionKind = 
  UnionKindInner  |
  UnionKindOuter 
  deriving (Eq, Ord, Read, Show)

_UnionKind = (Core.Name "hydra.ext.com.microsoft.kusto.kql.UnionKind")

_UnionKind_inner = (Core.Name "inner")

_UnionKind_outer = (Core.Name "outer")
