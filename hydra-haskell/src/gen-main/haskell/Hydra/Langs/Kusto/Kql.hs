-- | A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative.

module Hydra.Langs.Kusto.Kql where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype AndExpression = 
  AndExpression {
    unAndExpression :: [BasicExpression]}
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra/langs/kusto/kql.AndExpression")

data BasicExpression = 
  BasicExpressionAny  |
  BasicExpressionBetween BetweenExpression |
  BasicExpressionBinary BinaryExpression |
  BasicExpressionColumn ColumnName |
  BasicExpressionIndex IndexExpression |
  BasicExpressionList [Expression] |
  BasicExpressionLiteral Literal |
  BasicExpressionNotBetween BetweenExpression |
  BasicExpressionParentheses Expression |
  BasicExpressionPipeline PipelineExpression |
  BasicExpressionProperty PropertyExpression |
  BasicExpressionUnary UnaryExpression |
  BasicExpressionUnion UnionExpression
  deriving (Eq, Ord, Read, Show)

_BasicExpression = (Core.Name "hydra/langs/kusto/kql.BasicExpression")

_BasicExpression_any = (Core.FieldName "any")

_BasicExpression_between = (Core.FieldName "between")

_BasicExpression_binary = (Core.FieldName "binary")

_BasicExpression_column = (Core.FieldName "column")

_BasicExpression_index = (Core.FieldName "index")

_BasicExpression_list = (Core.FieldName "list")

_BasicExpression_literal = (Core.FieldName "literal")

_BasicExpression_notBetween = (Core.FieldName "notBetween")

_BasicExpression_parentheses = (Core.FieldName "parentheses")

_BasicExpression_pipeline = (Core.FieldName "pipeline")

_BasicExpression_property = (Core.FieldName "property")

_BasicExpression_unary = (Core.FieldName "unary")

_BasicExpression_union = (Core.FieldName "union")

data BetweenExpression = 
  BetweenExpression {
    betweenExpressionExpression :: Expression,
    betweenExpressionLowerBound :: Expression,
    betweenExpressionUpperBound :: Expression}
  deriving (Eq, Ord, Read, Show)

_BetweenExpression = (Core.Name "hydra/langs/kusto/kql.BetweenExpression")

_BetweenExpression_expression = (Core.FieldName "expression")

_BetweenExpression_lowerBound = (Core.FieldName "lowerBound")

_BetweenExpression_upperBound = (Core.FieldName "upperBound")

data BinaryExpression = 
  BinaryExpression {
    binaryExpressionLeft :: Expression,
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra/langs/kusto/kql.BinaryExpression")

_BinaryExpression_left = (Core.FieldName "left")

_BinaryExpression_operator = (Core.FieldName "operator")

_BinaryExpression_right = (Core.FieldName "right")

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

_BinaryOperator = (Core.Name "hydra/langs/kusto/kql.BinaryOperator")

_BinaryOperator_caseInsensitiveEqual = (Core.FieldName "caseInsensitiveEqual")

_BinaryOperator_contains = (Core.FieldName "contains")

_BinaryOperator_divide = (Core.FieldName "divide")

_BinaryOperator_endsWith = (Core.FieldName "endsWith")

_BinaryOperator_equal = (Core.FieldName "equal")

_BinaryOperator_greater = (Core.FieldName "greater")

_BinaryOperator_greaterOrEqual = (Core.FieldName "greaterOrEqual")

_BinaryOperator_has = (Core.FieldName "has")

_BinaryOperator_hasPrefix = (Core.FieldName "hasPrefix")

_BinaryOperator_hasSuffix = (Core.FieldName "hasSuffix")

_BinaryOperator_less = (Core.FieldName "less")

_BinaryOperator_lessOrEqual = (Core.FieldName "lessOrEqual")

_BinaryOperator_matchesRegex = (Core.FieldName "matchesRegex")

_BinaryOperator_minus = (Core.FieldName "minus")

_BinaryOperator_notEqual = (Core.FieldName "notEqual")

_BinaryOperator_plus = (Core.FieldName "plus")

_BinaryOperator_startsWith = (Core.FieldName "startsWith")

_BinaryOperator_times = (Core.FieldName "times")

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

_BuiltInFunction = (Core.Name "hydra/langs/kusto/kql.BuiltInFunction")

_BuiltInFunction_ago = (Core.FieldName "ago")

_BuiltInFunction_bin = (Core.FieldName "bin")

_BuiltInFunction_count = (Core.FieldName "count")

_BuiltInFunction_dcount = (Core.FieldName "dcount")

_BuiltInFunction_endofday = (Core.FieldName "endofday")

_BuiltInFunction_extract = (Core.FieldName "extract")

_BuiltInFunction_format_datetime = (Core.FieldName "format_datetime")

_BuiltInFunction_materialize = (Core.FieldName "materialize")

_BuiltInFunction_now = (Core.FieldName "now")

_BuiltInFunction_range = (Core.FieldName "range")

_BuiltInFunction_startofday = (Core.FieldName "startofday")

_BuiltInFunction_strcat = (Core.FieldName "strcat")

_BuiltInFunction_todynamic = (Core.FieldName "todynamic")

data ColumnAlias = 
  ColumnAlias {
    columnAliasColumn :: ColumnName,
    columnAliasAlias :: ColumnName}
  deriving (Eq, Ord, Read, Show)

_ColumnAlias = (Core.Name "hydra/langs/kusto/kql.ColumnAlias")

_ColumnAlias_column = (Core.FieldName "column")

_ColumnAlias_alias = (Core.FieldName "alias")

data ColumnAssignment = 
  ColumnAssignment {
    columnAssignmentColumn :: ColumnName,
    columnAssignmentExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ColumnAssignment = (Core.Name "hydra/langs/kusto/kql.ColumnAssignment")

_ColumnAssignment_column = (Core.FieldName "column")

_ColumnAssignment_expression = (Core.FieldName "expression")

newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra/langs/kusto/kql.ColumnName")

data Columns = 
  ColumnsAll  |
  ColumnsSingle ColumnName
  deriving (Eq, Ord, Read, Show)

_Columns = (Core.Name "hydra/langs/kusto/kql.Columns")

_Columns_all = (Core.FieldName "all")

_Columns_single = (Core.FieldName "single")

data Command = 
  CommandCount  |
  CommandDistinct [ColumnName] |
  CommandExtend [ColumnAssignment] |
  CommandJoin JoinCommand |
  CommandLet [LetBinding] |
  CommandLimit Int |
  CommandMvexpand ColumnName |
  CommandOrder_by [SortBy] |
  CommandParse ParseCommand |
  CommandPrint PrintCommand |
  CommandProject [Projection] |
  CommandProjectAway [ColumnName] |
  CommandProjectRename [ColumnAlias] |
  CommandRender  |
  CommandSearch SearchCommand |
  CommandSort [SortBy] |
  CommandSummarize SummarizeCommand |
  -- | Limit a search to a specified number of results
  CommandTake Int |
  CommandTop TopCommand |
  CommandUnion UnionExpression |
  CommandWhere Expression
  deriving (Eq, Ord, Read, Show)

_Command = (Core.Name "hydra/langs/kusto/kql.Command")

_Command_count = (Core.FieldName "count")

_Command_distinct = (Core.FieldName "distinct")

_Command_extend = (Core.FieldName "extend")

_Command_join = (Core.FieldName "join")

_Command_let = (Core.FieldName "let")

_Command_limit = (Core.FieldName "limit")

_Command_mvexpand = (Core.FieldName "mvexpand")

_Command_order_by = (Core.FieldName "order by")

_Command_parse = (Core.FieldName "parse")

_Command_print = (Core.FieldName "print")

_Command_project = (Core.FieldName "project")

_Command_projectAway = (Core.FieldName "projectAway")

_Command_projectRename = (Core.FieldName "projectRename")

_Command_render = (Core.FieldName "render")

_Command_search = (Core.FieldName "search")

_Command_sort = (Core.FieldName "sort")

_Command_summarize = (Core.FieldName "summarize")

_Command_take = (Core.FieldName "take")

_Command_top = (Core.FieldName "top")

_Command_union = (Core.FieldName "union")

_Command_where = (Core.FieldName "where")

newtype DatasetName = 
  DatasetName {
    unDatasetName :: String}
  deriving (Eq, Ord, Read, Show)

_DatasetName = (Core.Name "hydra/langs/kusto/kql.DatasetName")

data Duration = 
  Duration {
    durationValue :: Int,
    durationUnit :: DurationUnit}
  deriving (Eq, Ord, Read, Show)

_Duration = (Core.Name "hydra/langs/kusto/kql.Duration")

_Duration_value = (Core.FieldName "value")

_Duration_unit = (Core.FieldName "unit")

data DurationUnit = 
  DurationUnitSecond  |
  DurationUnitMinute  |
  DurationUnitHour 
  deriving (Eq, Ord, Read, Show)

_DurationUnit = (Core.Name "hydra/langs/kusto/kql.DurationUnit")

_DurationUnit_second = (Core.FieldName "second")

_DurationUnit_minute = (Core.FieldName "minute")

_DurationUnit_hour = (Core.FieldName "hour")

newtype Expression = 
  Expression {
    unExpression :: [OrExpression]}
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/kusto/kql.Expression")

data Function = 
  FunctionBuiltIn BuiltInFunction |
  FunctionCustom FunctionName
  deriving (Eq, Ord, Read, Show)

_Function = (Core.Name "hydra/langs/kusto/kql.Function")

_Function_builtIn = (Core.FieldName "builtIn")

_Function_custom = (Core.FieldName "custom")

data FunctionExpression = 
  FunctionExpression {
    functionExpressionFunction :: Function,
    functionExpressionArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionExpression = (Core.Name "hydra/langs/kusto/kql.FunctionExpression")

_FunctionExpression_function = (Core.FieldName "function")

_FunctionExpression_arguments = (Core.FieldName "arguments")

newtype FunctionName = 
  FunctionName {
    unFunctionName :: String}
  deriving (Eq, Ord, Read, Show)

_FunctionName = (Core.Name "hydra/langs/kusto/kql.FunctionName")

data IndexExpression = 
  IndexExpression {
    indexExpressionExpression :: Expression,
    indexExpressionIndex :: String}
  deriving (Eq, Ord, Read, Show)

_IndexExpression = (Core.Name "hydra/langs/kusto/kql.IndexExpression")

_IndexExpression_expression = (Core.FieldName "expression")

_IndexExpression_index = (Core.FieldName "index")

data JoinCommand = 
  JoinCommand {
    joinCommandKind :: JoinKind,
    joinCommandExpression :: DatasetName,
    joinCommandOn :: Expression}
  deriving (Eq, Ord, Read, Show)

_JoinCommand = (Core.Name "hydra/langs/kusto/kql.JoinCommand")

_JoinCommand_kind = (Core.FieldName "kind")

_JoinCommand_expression = (Core.FieldName "expression")

_JoinCommand_on = (Core.FieldName "on")

data JoinKind = 
  JoinKindInner  |
  JoinKindLeftanti  |
  JoinKindLeftantisemi  |
  JoinKindLeftsemi  |
  JoinKindOuter  |
  JoinKindRightanti  |
  JoinKindRightantisemi  |
  JoinKindRightsemi 
  deriving (Eq, Ord, Read, Show)

_JoinKind = (Core.Name "hydra/langs/kusto/kql.JoinKind")

_JoinKind_inner = (Core.FieldName "inner")

_JoinKind_leftanti = (Core.FieldName "leftanti")

_JoinKind_leftantisemi = (Core.FieldName "leftantisemi")

_JoinKind_leftsemi = (Core.FieldName "leftsemi")

_JoinKind_outer = (Core.FieldName "outer")

_JoinKind_rightanti = (Core.FieldName "rightanti")

_JoinKind_rightantisemi = (Core.FieldName "rightantisemi")

_JoinKind_rightsemi = (Core.FieldName "rightsemi")

data KeyValuePair = 
  KeyValuePair {
    keyValuePairKey :: String,
    keyValuePairValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_KeyValuePair = (Core.Name "hydra/langs/kusto/kql.KeyValuePair")

_KeyValuePair_key = (Core.FieldName "key")

_KeyValuePair_value = (Core.FieldName "value")

data LetBinding = 
  LetBinding {
    letBindingName :: ColumnName,
    letBindingExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_LetBinding = (Core.Name "hydra/langs/kusto/kql.LetBinding")

_LetBinding_name = (Core.FieldName "name")

_LetBinding_expression = (Core.FieldName "expression")

data Literal = 
  LiteralDuration Duration |
  LiteralDatetime String |
  LiteralString String |
  LiteralInt Int |
  LiteralLong Int64 |
  LiteralDouble Double |
  LiteralBoolean Bool
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/kusto/kql.Literal")

_Literal_duration = (Core.FieldName "duration")

_Literal_datetime = (Core.FieldName "datetime")

_Literal_string = (Core.FieldName "string")

_Literal_int = (Core.FieldName "int")

_Literal_long = (Core.FieldName "long")

_Literal_double = (Core.FieldName "double")

_Literal_boolean = (Core.FieldName "boolean")

newtype OrExpression = 
  OrExpression {
    unOrExpression :: [AndExpression]}
  deriving (Eq, Ord, Read, Show)

_OrExpression = (Core.Name "hydra/langs/kusto/kql.OrExpression")

data Order = 
  OrderAscending  |
  OrderDescending 
  deriving (Eq, Ord, Read, Show)

_Order = (Core.Name "hydra/langs/kusto/kql.Order")

_Order_ascending = (Core.FieldName "ascending")

_Order_descending = (Core.FieldName "descending")

data ParseCommand = 
  ParseCommand {
    parseCommandColumn :: ColumnName,
    parseCommandPairs :: [KeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_ParseCommand = (Core.Name "hydra/langs/kusto/kql.ParseCommand")

_ParseCommand_column = (Core.FieldName "column")

_ParseCommand_pairs = (Core.FieldName "pairs")

data PipelineExpression = 
  PipelineExpression {
    pipelineExpressionDataset :: DatasetName,
    pipelineExpressionCommands :: [Command]}
  deriving (Eq, Ord, Read, Show)

_PipelineExpression = (Core.Name "hydra/langs/kusto/kql.PipelineExpression")

_PipelineExpression_dataset = (Core.FieldName "dataset")

_PipelineExpression_commands = (Core.FieldName "commands")

data PrintCommand = 
  PrintCommand {
    printCommandColumn :: (Maybe ColumnName),
    printCommandExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_PrintCommand = (Core.Name "hydra/langs/kusto/kql.PrintCommand")

_PrintCommand_column = (Core.FieldName "column")

_PrintCommand_expression = (Core.FieldName "expression")

data Projection = 
  Projection {
    projectionExpression :: Expression,
    projectionAlias :: (Maybe ColumnName)}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "hydra/langs/kusto/kql.Projection")

_Projection_expression = (Core.FieldName "expression")

_Projection_alias = (Core.FieldName "alias")

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionExpression :: Expression,
    propertyExpressionProperty :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyExpression = (Core.Name "hydra/langs/kusto/kql.PropertyExpression")

_PropertyExpression_expression = (Core.FieldName "expression")

_PropertyExpression_property = (Core.FieldName "property")

newtype Query = 
  Query {
    unQuery :: PipelineExpression}
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/kusto/kql.Query")

-- | Search across all datasets and columns or, if provided, specific datasets and/or columns
data SearchCommand = 
  SearchCommand {
    searchCommandDatasets :: [DatasetName],
    searchCommandPattern :: Expression}
  deriving (Eq, Ord, Read, Show)

_SearchCommand = (Core.Name "hydra/langs/kusto/kql.SearchCommand")

_SearchCommand_datasets = (Core.FieldName "datasets")

_SearchCommand_pattern = (Core.FieldName "pattern")

data SummarizeCommand = 
  SummarizeCommand {
    summarizeCommandColumns :: [ColumnAssignment],
    summarizeCommandBy :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_SummarizeCommand = (Core.Name "hydra/langs/kusto/kql.SummarizeCommand")

_SummarizeCommand_columns = (Core.FieldName "columns")

_SummarizeCommand_by = (Core.FieldName "by")

data TopCommand = 
  TopCommand {
    topCommandCount :: Int,
    topCommandSort :: [SortBy]}
  deriving (Eq, Ord, Read, Show)

_TopCommand = (Core.Name "hydra/langs/kusto/kql.TopCommand")

_TopCommand_count = (Core.FieldName "count")

_TopCommand_sort = (Core.FieldName "sort")

data SortBy = 
  SortBy {
    sortByColumn :: ColumnName,
    sortByOrder :: (Maybe Order)}
  deriving (Eq, Ord, Read, Show)

_SortBy = (Core.Name "hydra/langs/kusto/kql.SortBy")

_SortBy_column = (Core.FieldName "column")

_SortBy_order = (Core.FieldName "order")

data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra/langs/kusto/kql.UnaryExpression")

_UnaryExpression_operator = (Core.FieldName "operator")

_UnaryExpression_expression = (Core.FieldName "expression")

data UnaryOperator = 
  UnaryOperatorNot 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra/langs/kusto/kql.UnaryOperator")

_UnaryOperator_not = (Core.FieldName "not")

newtype UnionExpression = 
  UnionExpression {
    unUnionExpression :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_UnionExpression = (Core.Name "hydra/langs/kusto/kql.UnionExpression")