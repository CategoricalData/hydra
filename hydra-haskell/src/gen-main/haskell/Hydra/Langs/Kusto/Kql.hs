-- | A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative.

module Hydra.Langs.Kusto.Kql where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data BetweenExpression = 
  BetweenExpression {
    betweenExpressionNot :: Bool,
    betweenExpressionExpression :: Expression,
    betweenExpressionLowerBound :: Expression,
    betweenExpressionUpperBound :: Expression}
  deriving (Eq, Ord, Read, Show)

_BetweenExpression = (Core.Name "hydra/langs/kusto/kql.BetweenExpression")

_BetweenExpression_not = (Core.Name "not")

_BetweenExpression_expression = (Core.Name "expression")

_BetweenExpression_lowerBound = (Core.Name "lowerBound")

_BetweenExpression_upperBound = (Core.Name "upperBound")

_BetweenExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.BetweenExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lowerBound"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "upperBound"),
      Core.fieldTypeType = _Expression_type_}]}))

data BinaryExpression = 
  BinaryExpression {
    binaryExpressionLeft :: Expression,
    binaryExpressionOperator :: BinaryOperator,
    binaryExpressionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_BinaryExpression = (Core.Name "hydra/langs/kusto/kql.BinaryExpression")

_BinaryExpression_left = (Core.Name "left")

_BinaryExpression_operator = (Core.Name "operator")

_BinaryExpression_right = (Core.Name "right")

_BinaryExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.BinaryExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _BinaryOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _Expression_type_}]}))

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

_BinaryOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.BinaryOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "caseInsensitiveEqual"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "contains"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "divide"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "endsWith"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equal"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "greater"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "greaterOrEqual"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "has"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasPrefix"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasSuffix"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "less"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessOrEqual"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "matchesRegex"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minus"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "notEqual"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "plus"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "startsWith"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "times"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

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

_BuiltInFunction_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.BuiltInFunction"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ago"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bin"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "count"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dcount"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "endofday"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "extract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "format_datetime"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "materialize"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "now"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "startofday"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "strcat"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "todynamic"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data ColumnAlias = 
  ColumnAlias {
    columnAliasColumn :: ColumnName,
    columnAliasAlias :: ColumnName}
  deriving (Eq, Ord, Read, Show)

_ColumnAlias = (Core.Name "hydra/langs/kusto/kql.ColumnAlias")

_ColumnAlias_column = (Core.Name "column")

_ColumnAlias_alias = (Core.Name "alias")

_ColumnAlias_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.ColumnAlias"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alias"),
      Core.fieldTypeType = _ColumnName_type_}]}))

data ColumnAssignment = 
  ColumnAssignment {
    columnAssignmentColumn :: ColumnName,
    columnAssignmentExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ColumnAssignment = (Core.Name "hydra/langs/kusto/kql.ColumnAssignment")

_ColumnAssignment_column = (Core.Name "column")

_ColumnAssignment_expression = (Core.Name "expression")

_ColumnAssignment_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.ColumnAssignment"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

newtype ColumnName = 
  ColumnName {
    unColumnName :: String}
  deriving (Eq, Ord, Read, Show)

_ColumnName = (Core.Name "hydra/langs/kusto/kql.ColumnName")

_ColumnName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Columns = 
  ColumnsAll  |
  ColumnsSingle ColumnName
  deriving (Eq, Ord, Read, Show)

_Columns = (Core.Name "hydra/langs/kusto/kql.Columns")

_Columns_all = (Core.Name "all")

_Columns_single = (Core.Name "single")

_Columns_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Columns"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "single"),
      Core.fieldTypeType = _ColumnName_type_}]}))

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

_Command = (Core.Name "hydra/langs/kusto/kql.Command")

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

_Command_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Command"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "count"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "distinct"),
      Core.fieldTypeType = (Core.TypeList _ColumnName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "extend"),
      Core.fieldTypeType = (Core.TypeList _ColumnAssignment_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "join"),
      Core.fieldTypeType = _JoinCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "limit"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "mvexpand"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "orderBy"),
      Core.fieldTypeType = (Core.TypeList _SortBy_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parse"),
      Core.fieldTypeType = _ParseCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "print"),
      Core.fieldTypeType = _PrintCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "project"),
      Core.fieldTypeType = (Core.TypeList _Projection_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "projectAway"),
      Core.fieldTypeType = (Core.TypeList _ColumnName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "projectRename"),
      Core.fieldTypeType = (Core.TypeList _ColumnAlias_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "render"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "search"),
      Core.fieldTypeType = _SearchCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sortBy"),
      Core.fieldTypeType = (Core.TypeList _SortBy_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "summarize"),
      Core.fieldTypeType = _SummarizeCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "take"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "top"),
      Core.fieldTypeType = _TopCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "union"),
      Core.fieldTypeType = _UnionCommand_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = _Expression_type_}]}))

newtype Datetime = 
  Datetime {
    unDatetime :: String}
  deriving (Eq, Ord, Read, Show)

_Datetime = (Core.Name "hydra/langs/kusto/kql.Datetime")

_Datetime_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Duration = 
  Duration {
    durationValue :: Int,
    durationUnit :: DurationUnit}
  deriving (Eq, Ord, Read, Show)

_Duration = (Core.Name "hydra/langs/kusto/kql.Duration")

_Duration_value = (Core.Name "value")

_Duration_unit = (Core.Name "unit")

_Duration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Duration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unit"),
      Core.fieldTypeType = _DurationUnit_type_}]}))

data DurationUnit = 
  DurationUnitSecond  |
  DurationUnitMinute  |
  DurationUnitHour 
  deriving (Eq, Ord, Read, Show)

_DurationUnit = (Core.Name "hydra/langs/kusto/kql.DurationUnit")

_DurationUnit_second = (Core.Name "second")

_DurationUnit_minute = (Core.Name "minute")

_DurationUnit_hour = (Core.Name "hour")

_DurationUnit_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.DurationUnit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "second"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minute"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hour"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

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

_Expression = (Core.Name "hydra/langs/kusto/kql.Expression")

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

_Expression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Expression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "any"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "between"),
      Core.fieldTypeType = _BetweenExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "binary"),
      Core.fieldTypeType = _BinaryExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "braces"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataset"),
      Core.fieldTypeType = _TableName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "index"),
      Core.fieldTypeType = _IndexExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parentheses"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unary"),
      Core.fieldTypeType = _UnaryExpression_type_}]}))

data Function = 
  FunctionBuiltIn BuiltInFunction |
  FunctionCustom FunctionName
  deriving (Eq, Ord, Read, Show)

_Function = (Core.Name "hydra/langs/kusto/kql.Function")

_Function_builtIn = (Core.Name "builtIn")

_Function_custom = (Core.Name "custom")

_Function_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Function"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "builtIn"),
      Core.fieldTypeType = _BuiltInFunction_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "custom"),
      Core.fieldTypeType = _FunctionName_type_}]}))

data FunctionExpression = 
  FunctionExpression {
    functionExpressionFunction :: Function,
    functionExpressionArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionExpression = (Core.Name "hydra/langs/kusto/kql.FunctionExpression")

_FunctionExpression_function = (Core.Name "function")

_FunctionExpression_arguments = (Core.Name "arguments")

_FunctionExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.FunctionExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "function"),
      Core.fieldTypeType = _Function_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)}]}))

newtype FunctionName = 
  FunctionName {
    unFunctionName :: String}
  deriving (Eq, Ord, Read, Show)

_FunctionName = (Core.Name "hydra/langs/kusto/kql.FunctionName")

_FunctionName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data IndexExpression = 
  IndexExpression {
    indexExpressionExpression :: Expression,
    indexExpressionIndex :: String}
  deriving (Eq, Ord, Read, Show)

_IndexExpression = (Core.Name "hydra/langs/kusto/kql.IndexExpression")

_IndexExpression_expression = (Core.Name "expression")

_IndexExpression_index = (Core.Name "index")

_IndexExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.IndexExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "index"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

data JoinCommand = 
  JoinCommand {
    joinCommandKind :: JoinKind,
    joinCommandExpression :: TableName,
    joinCommandOn :: Expression}
  deriving (Eq, Ord, Read, Show)

_JoinCommand = (Core.Name "hydra/langs/kusto/kql.JoinCommand")

_JoinCommand_kind = (Core.Name "kind")

_JoinCommand_expression = (Core.Name "expression")

_JoinCommand_on = (Core.Name "on")

_JoinCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.JoinCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "kind"),
      Core.fieldTypeType = _JoinKind_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _TableName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "on"),
      Core.fieldTypeType = _Expression_type_}]}))

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

_JoinKind = (Core.Name "hydra/langs/kusto/kql.JoinKind")

_JoinKind_leftouter = (Core.Name "leftouter")

_JoinKind_leftsemi = (Core.Name "leftsemi")

_JoinKind_leftanti = (Core.Name "leftanti")

_JoinKind_fullouter = (Core.Name "fullouter")

_JoinKind_inner = (Core.Name "inner")

_JoinKind_innerunique = (Core.Name "innerunique")

_JoinKind_rightouter = (Core.Name "rightouter")

_JoinKind_rightsemi = (Core.Name "rightsemi")

_JoinKind_rightanti = (Core.Name "rightanti")

_JoinKind_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.JoinKind"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftouter"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftsemi"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftanti"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "fullouter"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "innerunique"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rightouter"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rightsemi"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rightanti"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data KeyValuePair = 
  KeyValuePair {
    keyValuePairKey :: String,
    keyValuePairValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_KeyValuePair = (Core.Name "hydra/langs/kusto/kql.KeyValuePair")

_KeyValuePair_key = (Core.Name "key")

_KeyValuePair_value = (Core.Name "value")

_KeyValuePair_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.KeyValuePair"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Expression_type_}]}))

data LetBinding = 
  LetBinding {
    letBindingName :: ColumnName,
    letBindingExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_LetBinding = (Core.Name "hydra/langs/kusto/kql.LetBinding")

_LetBinding_name = (Core.Name "name")

_LetBinding_expression = (Core.Name "expression")

_LetBinding_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.LetBinding"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data LetExpression = 
  LetExpression {
    letExpressionBindings :: [LetBinding],
    letExpressionExpression :: TabularExpression}
  deriving (Eq, Ord, Read, Show)

_LetExpression = (Core.Name "hydra/langs/kusto/kql.LetExpression")

_LetExpression_bindings = (Core.Name "bindings")

_LetExpression_expression = (Core.Name "expression")

_LetExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.LetExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bindings"),
      Core.fieldTypeType = (Core.TypeList _LetBinding_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _TabularExpression_type_}]}))

data Literal = 
  LiteralDuration Duration |
  LiteralDatetime Datetime |
  LiteralString String |
  LiteralInt Int |
  LiteralLong Int64 |
  LiteralDouble Double |
  LiteralBoolean Bool
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/kusto/kql.Literal")

_Literal_duration = (Core.Name "duration")

_Literal_datetime = (Core.Name "datetime")

_Literal_string = (Core.Name "string")

_Literal_int = (Core.Name "int")

_Literal_long = (Core.Name "long")

_Literal_double = (Core.Name "double")

_Literal_boolean = (Core.Name "boolean")

_Literal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Literal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "duration"),
      Core.fieldTypeType = _Duration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datetime"),
      Core.fieldTypeType = _Datetime_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "int"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "long"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt64))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

data Order = 
  OrderAscending  |
  OrderDescending 
  deriving (Eq, Ord, Read, Show)

_Order = (Core.Name "hydra/langs/kusto/kql.Order")

_Order_ascending = (Core.Name "ascending")

_Order_descending = (Core.Name "descending")

_Order_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Order"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ascending"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "descending"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data Parameter = 
  Parameter {
    parameterKey :: String,
    parameterValue :: Literal}
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra/langs/kusto/kql.Parameter")

_Parameter_key = (Core.Name "key")

_Parameter_value = (Core.Name "value")

_Parameter_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Parameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Literal_type_}]}))

data ParseCommand = 
  ParseCommand {
    parseCommandColumn :: ColumnName,
    parseCommandPairs :: [KeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_ParseCommand = (Core.Name "hydra/langs/kusto/kql.ParseCommand")

_ParseCommand_column = (Core.Name "column")

_ParseCommand_pairs = (Core.Name "pairs")

_ParseCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.ParseCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pairs"),
      Core.fieldTypeType = (Core.TypeList _KeyValuePair_type_)}]}))

newtype PipelineExpression = 
  PipelineExpression {
    unPipelineExpression :: [TabularExpression]}
  deriving (Eq, Ord, Read, Show)

_PipelineExpression = (Core.Name "hydra/langs/kusto/kql.PipelineExpression")

_PipelineExpression_type_ = (Core.TypeList _TabularExpression_type_)

data PrintCommand = 
  PrintCommand {
    printCommandColumn :: (Maybe ColumnName),
    printCommandExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_PrintCommand = (Core.Name "hydra/langs/kusto/kql.PrintCommand")

_PrintCommand_column = (Core.Name "column")

_PrintCommand_expression = (Core.Name "expression")

_PrintCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.PrintCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = (Core.TypeOptional _ColumnName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data Projection = 
  Projection {
    projectionExpression :: Expression,
    projectionAlias :: (Maybe ColumnName)}
  deriving (Eq, Ord, Read, Show)

_Projection = (Core.Name "hydra/langs/kusto/kql.Projection")

_Projection_expression = (Core.Name "expression")

_Projection_alias = (Core.Name "alias")

_Projection_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.Projection"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alias"),
      Core.fieldTypeType = (Core.TypeOptional _ColumnName_type_)}]}))

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionExpression :: Expression,
    propertyExpressionProperty :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyExpression = (Core.Name "hydra/langs/kusto/kql.PropertyExpression")

_PropertyExpression_expression = (Core.Name "expression")

_PropertyExpression_property = (Core.Name "property")

_PropertyExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.PropertyExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

newtype Query = 
  Query {
    unQuery :: TabularExpression}
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/kusto/kql.Query")

_Query_type_ = _TabularExpression_type_

-- | Search across all datasets and columns or, if provided, specific datasets and/or columns
data SearchCommand = 
  SearchCommand {
    searchCommandDatasets :: [TableName],
    searchCommandPattern :: Expression}
  deriving (Eq, Ord, Read, Show)

_SearchCommand = (Core.Name "hydra/langs/kusto/kql.SearchCommand")

_SearchCommand_datasets = (Core.Name "datasets")

_SearchCommand_pattern = (Core.Name "pattern")

_SearchCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.SearchCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datasets"),
      Core.fieldTypeType = (Core.TypeList _TableName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Expression_type_}]}))

data SummarizeCommand = 
  SummarizeCommand {
    summarizeCommandColumns :: [ColumnAssignment],
    summarizeCommandBy :: [ColumnName]}
  deriving (Eq, Ord, Read, Show)

_SummarizeCommand = (Core.Name "hydra/langs/kusto/kql.SummarizeCommand")

_SummarizeCommand_columns = (Core.Name "columns")

_SummarizeCommand_by = (Core.Name "by")

_SummarizeCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.SummarizeCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "columns"),
      Core.fieldTypeType = (Core.TypeList _ColumnAssignment_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "by"),
      Core.fieldTypeType = (Core.TypeList _ColumnName_type_)}]}))

newtype TableName = 
  TableName {
    unTableName :: String}
  deriving (Eq, Ord, Read, Show)

_TableName = (Core.Name "hydra/langs/kusto/kql.TableName")

_TableName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data TopCommand = 
  TopCommand {
    topCommandCount :: Int,
    topCommandSort :: [SortBy]}
  deriving (Eq, Ord, Read, Show)

_TopCommand = (Core.Name "hydra/langs/kusto/kql.TopCommand")

_TopCommand_count = (Core.Name "count")

_TopCommand_sort = (Core.Name "sort")

_TopCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.TopCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "count"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeInt32))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sort"),
      Core.fieldTypeType = (Core.TypeList _SortBy_type_)}]}))

data SortBy = 
  SortBy {
    sortByColumn :: ColumnName,
    sortByOrder :: (Maybe Order)}
  deriving (Eq, Ord, Read, Show)

_SortBy = (Core.Name "hydra/langs/kusto/kql.SortBy")

_SortBy_column = (Core.Name "column")

_SortBy_order = (Core.Name "order")

_SortBy_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.SortBy"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "column"),
      Core.fieldTypeType = _ColumnName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "order"),
      Core.fieldTypeType = (Core.TypeOptional _Order_type_)}]}))

data TabularExpression = 
  TabularExpressionCommand Command |
  TabularExpressionPipeline PipelineExpression |
  TabularExpressionLet LetExpression |
  TabularExpressionTable TableName
  deriving (Eq, Ord, Read, Show)

_TabularExpression = (Core.Name "hydra/langs/kusto/kql.TabularExpression")

_TabularExpression_command = (Core.Name "command")

_TabularExpression_pipeline = (Core.Name "pipeline")

_TabularExpression_let = (Core.Name "let")

_TabularExpression_table = (Core.Name "table")

_TabularExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.TabularExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "command"),
      Core.fieldTypeType = _Command_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pipeline"),
      Core.fieldTypeType = _PipelineExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "let"),
      Core.fieldTypeType = _LetExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "table"),
      Core.fieldTypeType = _TableName_type_}]}))

data UnaryExpression = 
  UnaryExpression {
    unaryExpressionOperator :: UnaryOperator,
    unaryExpressionExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_UnaryExpression = (Core.Name "hydra/langs/kusto/kql.UnaryExpression")

_UnaryExpression_operator = (Core.Name "operator")

_UnaryExpression_expression = (Core.Name "expression")

_UnaryExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.UnaryExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _UnaryOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data UnaryOperator = 
  UnaryOperatorNot 
  deriving (Eq, Ord, Read, Show)

_UnaryOperator = (Core.Name "hydra/langs/kusto/kql.UnaryOperator")

_UnaryOperator_not = (Core.Name "not")

_UnaryOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.UnaryOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data UnionCommand = 
  UnionCommand {
    unionCommandParameters :: [Parameter],
    unionCommandKind :: (Maybe UnionKind),
    unionCommandWithSource :: (Maybe ColumnName),
    unionCommandIsFuzzy :: (Maybe Bool),
    unionCommandTables :: [TableName]}
  deriving (Eq, Ord, Read, Show)

_UnionCommand = (Core.Name "hydra/langs/kusto/kql.UnionCommand")

_UnionCommand_parameters = (Core.Name "parameters")

_UnionCommand_kind = (Core.Name "kind")

_UnionCommand_withSource = (Core.Name "withSource")

_UnionCommand_isFuzzy = (Core.Name "isFuzzy")

_UnionCommand_tables = (Core.Name "tables")

_UnionCommand_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.UnionCommand"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameters"),
      Core.fieldTypeType = (Core.TypeList _Parameter_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "kind"),
      Core.fieldTypeType = (Core.TypeOptional _UnionKind_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "withSource"),
      Core.fieldTypeType = (Core.TypeOptional _ColumnName_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "isFuzzy"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeBoolean))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "tables"),
      Core.fieldTypeType = (Core.TypeList _TableName_type_)}]}))

data UnionKind = 
  UnionKindInner  |
  UnionKindOuter 
  deriving (Eq, Ord, Read, Show)

_UnionKind = (Core.Name "hydra/langs/kusto/kql.UnionKind")

_UnionKind_inner = (Core.Name "inner")

_UnionKind_outer = (Core.Name "outer")

_UnionKind_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/kusto/kql.UnionKind"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inner"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "outer"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))