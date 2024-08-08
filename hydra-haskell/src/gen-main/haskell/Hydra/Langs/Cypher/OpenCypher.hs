-- | A Cypher model based on the OpenCypher specification (version 23), copyright Neo Technology, available at:
-- |   https://opencypher.org/resources/

module Hydra.Langs.Cypher.OpenCypher where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data Query = 
  QueryRegular RegularQuery |
  QueryStandalone StandaloneCall
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra/langs/cypher/openCypher.Query")

_Query_regular = (Core.Name "regular")

_Query_standalone = (Core.Name "standalone")

_Query_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Query"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "regular"),
      Core.fieldTypeType = _RegularQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "standalone"),
      Core.fieldTypeType = _StandaloneCall_type_}]}))

data RegularQuery = 
  RegularQuery {
    regularQueryHead :: SingleQuery,
    regularQueryRest :: [Union]}
  deriving (Eq, Ord, Read, Show)

_RegularQuery = (Core.Name "hydra/langs/cypher/openCypher.RegularQuery")

_RegularQuery_head = (Core.Name "head")

_RegularQuery_rest = (Core.Name "rest")

_RegularQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RegularQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "head"),
      Core.fieldTypeType = _SingleQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rest"),
      Core.fieldTypeType = (Core.TypeList _Union_type_)}]}))

data Union = 
  Union {
    unionAll :: Bool,
    unionQuery :: SingleQuery}
  deriving (Eq, Ord, Read, Show)

_Union = (Core.Name "hydra/langs/cypher/openCypher.Union")

_Union_all = (Core.Name "all")

_Union_query = (Core.Name "query")

_Union_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Union"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "query"),
      Core.fieldTypeType = _SingleQuery_type_}]}))

data SingleQuery = 
  SingleQuerySinglePart SinglePartQuery |
  SingleQueryMultiPart MultiPartQuery
  deriving (Eq, Ord, Read, Show)

_SingleQuery = (Core.Name "hydra/langs/cypher/openCypher.SingleQuery")

_SingleQuery_singlePart = (Core.Name "singlePart")

_SingleQuery_multiPart = (Core.Name "multiPart")

_SingleQuery_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.SingleQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "singlePart"),
      Core.fieldTypeType = _SinglePartQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "multiPart"),
      Core.fieldTypeType = _MultiPartQuery_type_}]}))

data SinglePartQuery = 
  SinglePartQuery {
    singlePartQueryReading :: [ReadingClause],
    singlePartQueryUpdating :: [UpdatingClause],
    singlePartQueryReturn :: (Maybe Return)}
  deriving (Eq, Ord, Read, Show)

_SinglePartQuery = (Core.Name "hydra/langs/cypher/openCypher.SinglePartQuery")

_SinglePartQuery_reading = (Core.Name "reading")

_SinglePartQuery_updating = (Core.Name "updating")

_SinglePartQuery_return = (Core.Name "return")

_SinglePartQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.SinglePartQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reading"),
      Core.fieldTypeType = (Core.TypeList _ReadingClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "updating"),
      Core.fieldTypeType = (Core.TypeList _UpdatingClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "return"),
      Core.fieldTypeType = (Core.TypeOptional _Return_type_)}]}))

data WithClause = 
  WithClause {
    withClauseReading :: [ReadingClause],
    withClauseUpdating :: [UpdatingClause],
    withClauseWith :: With}
  deriving (Eq, Ord, Read, Show)

_WithClause = (Core.Name "hydra/langs/cypher/openCypher.WithClause")

_WithClause_reading = (Core.Name "reading")

_WithClause_updating = (Core.Name "updating")

_WithClause_with = (Core.Name "with")

_WithClause_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.WithClause"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reading"),
      Core.fieldTypeType = (Core.TypeList _ReadingClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "updating"),
      Core.fieldTypeType = (Core.TypeList _UpdatingClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "with"),
      Core.fieldTypeType = _With_type_}]}))

data MultiPartQuery = 
  MultiPartQuery {
    multiPartQueryWith :: [WithClause],
    multiPartQueryBody :: SinglePartQuery}
  deriving (Eq, Ord, Read, Show)

_MultiPartQuery = (Core.Name "hydra/langs/cypher/openCypher.MultiPartQuery")

_MultiPartQuery_with = (Core.Name "with")

_MultiPartQuery_body = (Core.Name "body")

_MultiPartQuery_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.MultiPartQuery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "with"),
      Core.fieldTypeType = (Core.TypeList _WithClause_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "body"),
      Core.fieldTypeType = _SinglePartQuery_type_}]}))

data UpdatingClause = 
  UpdatingClauseCreate Create |
  UpdatingClauseMerge Merge |
  UpdatingClauseDelete Delete |
  UpdatingClauseSet Set_ |
  UpdatingClauseRemove Remove
  deriving (Eq, Ord, Read, Show)

_UpdatingClause = (Core.Name "hydra/langs/cypher/openCypher.UpdatingClause")

_UpdatingClause_create = (Core.Name "create")

_UpdatingClause_merge = (Core.Name "merge")

_UpdatingClause_delete = (Core.Name "delete")

_UpdatingClause_set = (Core.Name "set")

_UpdatingClause_remove = (Core.Name "remove")

_UpdatingClause_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.UpdatingClause"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "create"),
      Core.fieldTypeType = _Create_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "merge"),
      Core.fieldTypeType = _Merge_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "delete"),
      Core.fieldTypeType = _Delete_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "set"),
      Core.fieldTypeType = _Set_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "remove"),
      Core.fieldTypeType = _Remove_type_}]}))

data ReadingClause = 
  ReadingClauseMatch Match |
  ReadingClauseUnwind Unwind |
  ReadingClauseInQueryCall InQueryCall
  deriving (Eq, Ord, Read, Show)

_ReadingClause = (Core.Name "hydra/langs/cypher/openCypher.ReadingClause")

_ReadingClause_match = (Core.Name "match")

_ReadingClause_unwind = (Core.Name "unwind")

_ReadingClause_inQueryCall = (Core.Name "inQueryCall")

_ReadingClause_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ReadingClause"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "match"),
      Core.fieldTypeType = _Match_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "unwind"),
      Core.fieldTypeType = _Unwind_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inQueryCall"),
      Core.fieldTypeType = _InQueryCall_type_}]}))

data Match = 
  Match {
    matchOptional :: Bool,
    matchPattern :: Pattern,
    matchWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_Match = (Core.Name "hydra/langs/cypher/openCypher.Match")

_Match_optional = (Core.Name "optional")

_Match_pattern = (Core.Name "pattern")

_Match_where = (Core.Name "where")

_Match_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Match"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "optional"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Where_type_)}]}))

data Unwind = 
  Unwind {
    unwindExpression :: Expression,
    unwindVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_Unwind = (Core.Name "hydra/langs/cypher/openCypher.Unwind")

_Unwind_expression = (Core.Name "expression")

_Unwind_variable = (Core.Name "variable")

_Unwind_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Unwind"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_}]}))

data Merge = 
  Merge {
    mergePatternPart :: PatternPart,
    mergeActions :: [MergeAction]}
  deriving (Eq, Ord, Read, Show)

_Merge = (Core.Name "hydra/langs/cypher/openCypher.Merge")

_Merge_patternPart = (Core.Name "patternPart")

_Merge_actions = (Core.Name "actions")

_Merge_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Merge"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "patternPart"),
      Core.fieldTypeType = _PatternPart_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "actions"),
      Core.fieldTypeType = (Core.TypeList _MergeAction_type_)}]}))

data MatchOrCreate = 
  MatchOrCreateMatch  |
  MatchOrCreateCreate 
  deriving (Eq, Ord, Read, Show)

_MatchOrCreate = (Core.Name "hydra/langs/cypher/openCypher.MatchOrCreate")

_MatchOrCreate_match = (Core.Name "match")

_MatchOrCreate_create = (Core.Name "create")

_MatchOrCreate_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.MatchOrCreate"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "match"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "create"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data MergeAction = 
  MergeAction {
    mergeActionAction :: MatchOrCreate,
    mergeActionSet :: Set_}
  deriving (Eq, Ord, Read, Show)

_MergeAction = (Core.Name "hydra/langs/cypher/openCypher.MergeAction")

_MergeAction_action = (Core.Name "action")

_MergeAction_set = (Core.Name "set")

_MergeAction_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.MergeAction"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "action"),
      Core.fieldTypeType = _MatchOrCreate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "set"),
      Core.fieldTypeType = _Set_type_}]}))

newtype Create = 
  Create {
    unCreate :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Create = (Core.Name "hydra/langs/cypher/openCypher.Create")

_Create_type_ = _Pattern_type_

newtype Set_ = 
  Set_ {
    unSet :: [SetItem]}
  deriving (Eq, Ord, Read, Show)

_Set = (Core.Name "hydra/langs/cypher/openCypher.Set")

_Set_type_ = (Core.TypeList _SetItem_type_)

data SetItem = 
  SetItemProperty PropertyEquals |
  SetItemVariableEqual VariableEquals |
  SetItemVariablePlusEqual VariablePlusEquals |
  SetItemVariableLabels VariableAndNodeLabels
  deriving (Eq, Ord, Read, Show)

_SetItem = (Core.Name "hydra/langs/cypher/openCypher.SetItem")

_SetItem_property = (Core.Name "property")

_SetItem_variableEqual = (Core.Name "variableEqual")

_SetItem_variablePlusEqual = (Core.Name "variablePlusEqual")

_SetItem_variableLabels = (Core.Name "variableLabels")

_SetItem_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.SetItem"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyEquals_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variableEqual"),
      Core.fieldTypeType = _VariableEquals_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variablePlusEqual"),
      Core.fieldTypeType = _VariablePlusEquals_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variableLabels"),
      Core.fieldTypeType = _VariableAndNodeLabels_type_}]}))

data PropertyEquals = 
  PropertyEquals {
    propertyEqualsLhs :: PropertyExpression,
    propertyEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_PropertyEquals = (Core.Name "hydra/langs/cypher/openCypher.PropertyEquals")

_PropertyEquals_lhs = (Core.Name "lhs")

_PropertyEquals_rhs = (Core.Name "rhs")

_PropertyEquals_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PropertyEquals"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _PropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Expression_type_}]}))

data VariableEquals = 
  VariableEquals {
    variableEqualsLhs :: Variable,
    variableEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariableEquals = (Core.Name "hydra/langs/cypher/openCypher.VariableEquals")

_VariableEquals_lhs = (Core.Name "lhs")

_VariableEquals_rhs = (Core.Name "rhs")

_VariableEquals_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.VariableEquals"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Expression_type_}]}))

data VariablePlusEquals = 
  VariablePlusEquals {
    variablePlusEqualsLhs :: Variable,
    variablePlusEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariablePlusEquals = (Core.Name "hydra/langs/cypher/openCypher.VariablePlusEquals")

_VariablePlusEquals_lhs = (Core.Name "lhs")

_VariablePlusEquals_rhs = (Core.Name "rhs")

_VariablePlusEquals_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.VariablePlusEquals"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lhs"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rhs"),
      Core.fieldTypeType = _Expression_type_}]}))

data VariableAndNodeLabels = 
  VariableAndNodeLabels {
    variableAndNodeLabelsVariable :: Variable,
    variableAndNodeLabelsLabels :: NodeLabels}
  deriving (Eq, Ord, Read, Show)

_VariableAndNodeLabels = (Core.Name "hydra/langs/cypher/openCypher.VariableAndNodeLabels")

_VariableAndNodeLabels_variable = (Core.Name "variable")

_VariableAndNodeLabels_labels = (Core.Name "labels")

_VariableAndNodeLabels_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.VariableAndNodeLabels"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labels"),
      Core.fieldTypeType = _NodeLabels_type_}]}))

data Delete = 
  Delete {
    deleteDetach :: Bool,
    deleteExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Delete = (Core.Name "hydra/langs/cypher/openCypher.Delete")

_Delete_detach = (Core.Name "detach")

_Delete_expressions = (Core.Name "expressions")

_Delete_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Delete"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "detach"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expressions"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)}]}))

newtype Remove = 
  Remove {
    unRemove :: [RemoveItem]}
  deriving (Eq, Ord, Read, Show)

_Remove = (Core.Name "hydra/langs/cypher/openCypher.Remove")

_Remove_type_ = (Core.TypeList _RemoveItem_type_)

data RemoveItem = 
  RemoveItemVariableLabels VariableAndNodeLabels |
  RemoveItemProperty PropertyExpression
  deriving (Eq, Ord, Read, Show)

_RemoveItem = (Core.Name "hydra/langs/cypher/openCypher.RemoveItem")

_RemoveItem_variableLabels = (Core.Name "variableLabels")

_RemoveItem_property = (Core.Name "property")

_RemoveItem_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RemoveItem"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variableLabels"),
      Core.fieldTypeType = _VariableAndNodeLabels_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyExpression_type_}]}))

data InQueryCall = 
  InQueryCall {
    inQueryCallCall :: ExplicitProcedureInvocation,
    inQueryCallYieldItems :: (Maybe YieldItems)}
  deriving (Eq, Ord, Read, Show)

_InQueryCall = (Core.Name "hydra/langs/cypher/openCypher.InQueryCall")

_InQueryCall_call = (Core.Name "call")

_InQueryCall_yieldItems = (Core.Name "yieldItems")

_InQueryCall_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.InQueryCall"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "call"),
      Core.fieldTypeType = _ExplicitProcedureInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "yieldItems"),
      Core.fieldTypeType = (Core.TypeOptional _YieldItems_type_)}]}))

data ProcedureInvocation = 
  ProcedureInvocationExplicit ExplicitProcedureInvocation |
  ProcedureInvocationImplicit ImplicitProcedureInvocation
  deriving (Eq, Ord, Read, Show)

_ProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ProcedureInvocation")

_ProcedureInvocation_explicit = (Core.Name "explicit")

_ProcedureInvocation_implicit = (Core.Name "implicit")

_ProcedureInvocation_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ProcedureInvocation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "explicit"),
      Core.fieldTypeType = _ExplicitProcedureInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "implicit"),
      Core.fieldTypeType = _ImplicitProcedureInvocation_type_}]}))

data StarOrYieldItems = 
  StarOrYieldItemsStar  |
  StarOrYieldItemsItems YieldItems
  deriving (Eq, Ord, Read, Show)

_StarOrYieldItems = (Core.Name "hydra/langs/cypher/openCypher.StarOrYieldItems")

_StarOrYieldItems_star = (Core.Name "star")

_StarOrYieldItems_items = (Core.Name "items")

_StarOrYieldItems_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.StarOrYieldItems"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "star"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "items"),
      Core.fieldTypeType = _YieldItems_type_}]}))

data StandaloneCall = 
  StandaloneCall {
    standaloneCallCall :: ProcedureInvocation,
    standaloneCallYieldItems :: (Maybe StarOrYieldItems)}
  deriving (Eq, Ord, Read, Show)

_StandaloneCall = (Core.Name "hydra/langs/cypher/openCypher.StandaloneCall")

_StandaloneCall_call = (Core.Name "call")

_StandaloneCall_yieldItems = (Core.Name "yieldItems")

_StandaloneCall_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.StandaloneCall"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "call"),
      Core.fieldTypeType = _ProcedureInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "yieldItems"),
      Core.fieldTypeType = (Core.TypeOptional _StarOrYieldItems_type_)}]}))

data YieldItems = 
  YieldItems {
    yieldItemsItems :: [YieldItem],
    yieldItemsWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_YieldItems = (Core.Name "hydra/langs/cypher/openCypher.YieldItems")

_YieldItems_items = (Core.Name "items")

_YieldItems_where = (Core.Name "where")

_YieldItems_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.YieldItems"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "items"),
      Core.fieldTypeType = (Core.TypeList _YieldItem_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Where_type_)}]}))

data YieldItem = 
  YieldItem {
    yieldItemField :: (Maybe ProcedureResultField),
    yieldItemVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_YieldItem = (Core.Name "hydra/langs/cypher/openCypher.YieldItem")

_YieldItem_field = (Core.Name "field")

_YieldItem_variable = (Core.Name "variable")

_YieldItem_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.YieldItem"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "field"),
      Core.fieldTypeType = (Core.TypeOptional _ProcedureResultField_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_}]}))

data With = 
  With {
    withProjection :: ProjectionBody,
    withWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_With = (Core.Name "hydra/langs/cypher/openCypher.With")

_With_projection = (Core.Name "projection")

_With_where = (Core.Name "where")

_With_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.With"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "projection"),
      Core.fieldTypeType = _ProjectionBody_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Where_type_)}]}))

newtype Return = 
  Return {
    unReturn :: ProjectionBody}
  deriving (Eq, Ord, Read, Show)

_Return = (Core.Name "hydra/langs/cypher/openCypher.Return")

_Return_type_ = _ProjectionBody_type_

data ProjectionBody = 
  ProjectionBody {
    projectionBodyDistinct :: Bool,
    projectionBodyProjectionItems :: ProjectionItems,
    projectionBodyOrder :: (Maybe Order),
    projectionBodySkip :: (Maybe Skip),
    projectionBodyLimit :: (Maybe Limit)}
  deriving (Eq, Ord, Read, Show)

_ProjectionBody = (Core.Name "hydra/langs/cypher/openCypher.ProjectionBody")

_ProjectionBody_distinct = (Core.Name "distinct")

_ProjectionBody_projectionItems = (Core.Name "projectionItems")

_ProjectionBody_order = (Core.Name "order")

_ProjectionBody_skip = (Core.Name "skip")

_ProjectionBody_limit = (Core.Name "limit")

_ProjectionBody_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ProjectionBody"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "distinct"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "projectionItems"),
      Core.fieldTypeType = _ProjectionItems_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "order"),
      Core.fieldTypeType = (Core.TypeOptional _Order_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "skip"),
      Core.fieldTypeType = (Core.TypeOptional _Skip_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "limit"),
      Core.fieldTypeType = (Core.TypeOptional _Limit_type_)}]}))

data ProjectionItems = 
  ProjectionItems {
    projectionItemsStar :: Bool,
    projectionItemsExplicit :: [ProjectionItem]}
  deriving (Eq, Ord, Read, Show)

_ProjectionItems = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItems")

_ProjectionItems_star = (Core.Name "star")

_ProjectionItems_explicit = (Core.Name "explicit")

_ProjectionItems_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItems"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "star"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "explicit"),
      Core.fieldTypeType = (Core.TypeList _ProjectionItem_type_)}]}))

data ProjectionItem = 
  ProjectionItem {
    projectionItemExpression :: Expression,
    projectionItemVariable :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_ProjectionItem = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItem")

_ProjectionItem_expression = (Core.Name "expression")

_ProjectionItem_variable = (Core.Name "variable")

_ProjectionItem_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItem"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)}]}))

newtype Order = 
  Order {
    unOrder :: [SortItem]}
  deriving (Eq, Ord, Read, Show)

_Order = (Core.Name "hydra/langs/cypher/openCypher.Order")

_Order_type_ = (Core.TypeList _SortItem_type_)

newtype Skip = 
  Skip {
    unSkip :: Expression}
  deriving (Eq, Ord, Read, Show)

_Skip = (Core.Name "hydra/langs/cypher/openCypher.Skip")

_Skip_type_ = _Expression_type_

newtype Limit = 
  Limit {
    unLimit :: Expression}
  deriving (Eq, Ord, Read, Show)

_Limit = (Core.Name "hydra/langs/cypher/openCypher.Limit")

_Limit_type_ = _Expression_type_

data SortOrder = 
  SortOrderAscending  |
  SortOrderDescending 
  deriving (Eq, Ord, Read, Show)

_SortOrder = (Core.Name "hydra/langs/cypher/openCypher.SortOrder")

_SortOrder_ascending = (Core.Name "ascending")

_SortOrder_descending = (Core.Name "descending")

_SortOrder_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.SortOrder"),
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

data SortItem = 
  SortItem {
    sortItemExpression :: Expression,
    sortItemOrder :: (Maybe SortOrder)}
  deriving (Eq, Ord, Read, Show)

_SortItem = (Core.Name "hydra/langs/cypher/openCypher.SortItem")

_SortItem_expression = (Core.Name "expression")

_SortItem_order = (Core.Name "order")

_SortItem_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.SortItem"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "order"),
      Core.fieldTypeType = (Core.TypeOptional _SortOrder_type_)}]}))

newtype Where = 
  Where {
    unWhere :: Expression}
  deriving (Eq, Ord, Read, Show)

_Where = (Core.Name "hydra/langs/cypher/openCypher.Where")

_Where_type_ = _Expression_type_

newtype Pattern = 
  Pattern {
    unPattern :: [PatternPart]}
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/langs/cypher/openCypher.Pattern")

_Pattern_type_ = (Core.TypeList _PatternPart_type_)

data PatternPart = 
  PatternPart {
    patternPartVariable :: (Maybe Variable),
    patternPartPattern :: AnonymousPatternPart}
  deriving (Eq, Ord, Read, Show)

_PatternPart = (Core.Name "hydra/langs/cypher/openCypher.PatternPart")

_PatternPart_variable = (Core.Name "variable")

_PatternPart_pattern = (Core.Name "pattern")

_PatternPart_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PatternPart"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _AnonymousPatternPart_type_}]}))

newtype AnonymousPatternPart = 
  AnonymousPatternPart {
    unAnonymousPatternPart :: PatternElement}
  deriving (Eq, Ord, Read, Show)

_AnonymousPatternPart = (Core.Name "hydra/langs/cypher/openCypher.AnonymousPatternPart")

_AnonymousPatternPart_type_ = _PatternElement_type_

data NodePatternChain = 
  NodePatternChain {
    nodePatternChainNodePattern :: NodePattern,
    nodePatternChainChain :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_NodePatternChain = (Core.Name "hydra/langs/cypher/openCypher.NodePatternChain")

_NodePatternChain_nodePattern = (Core.Name "nodePattern")

_NodePatternChain_chain = (Core.Name "chain")

_NodePatternChain_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.NodePatternChain"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nodePattern"),
      Core.fieldTypeType = _NodePattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "chain"),
      Core.fieldTypeType = (Core.TypeList _PatternElementChain_type_)}]}))

data PatternElement = 
  PatternElementChained NodePatternChain |
  PatternElementParenthesized PatternElement
  deriving (Eq, Ord, Read, Show)

_PatternElement = (Core.Name "hydra/langs/cypher/openCypher.PatternElement")

_PatternElement_chained = (Core.Name "chained")

_PatternElement_parenthesized = (Core.Name "parenthesized")

_PatternElement_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PatternElement"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "chained"),
      Core.fieldTypeType = _NodePatternChain_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parenthesized"),
      Core.fieldTypeType = _PatternElement_type_}]}))

data RelationshipsPattern = 
  RelationshipsPattern {
    relationshipsPatternNodePattern :: NodePattern,
    relationshipsPatternChain :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_RelationshipsPattern = (Core.Name "hydra/langs/cypher/openCypher.RelationshipsPattern")

_RelationshipsPattern_nodePattern = (Core.Name "nodePattern")

_RelationshipsPattern_chain = (Core.Name "chain")

_RelationshipsPattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RelationshipsPattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nodePattern"),
      Core.fieldTypeType = _NodePattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "chain"),
      Core.fieldTypeType = (Core.TypeList _PatternElementChain_type_)}]}))

data NodePattern = 
  NodePattern {
    nodePatternVariable :: (Maybe Variable),
    nodePatternLabels :: (Maybe NodeLabels),
    nodePatternProperties :: (Maybe Properties)}
  deriving (Eq, Ord, Read, Show)

_NodePattern = (Core.Name "hydra/langs/cypher/openCypher.NodePattern")

_NodePattern_variable = (Core.Name "variable")

_NodePattern_labels = (Core.Name "labels")

_NodePattern_properties = (Core.Name "properties")

_NodePattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.NodePattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labels"),
      Core.fieldTypeType = (Core.TypeOptional _NodeLabels_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeOptional _Properties_type_)}]}))

data PatternElementChain = 
  PatternElementChain {
    patternElementChainRelationship :: RelationshipPattern,
    patternElementChainNode :: NodePattern}
  deriving (Eq, Ord, Read, Show)

_PatternElementChain = (Core.Name "hydra/langs/cypher/openCypher.PatternElementChain")

_PatternElementChain_relationship = (Core.Name "relationship")

_PatternElementChain_node = (Core.Name "node")

_PatternElementChain_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PatternElementChain"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "relationship"),
      Core.fieldTypeType = _RelationshipPattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "node"),
      Core.fieldTypeType = _NodePattern_type_}]}))

data RelationshipPattern = 
  RelationshipPattern {
    relationshipPatternLeftArrow :: Bool,
    relationshipPatternDetail :: (Maybe RelationshipDetail),
    relationshipPatternRightArrow :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipPattern = (Core.Name "hydra/langs/cypher/openCypher.RelationshipPattern")

_RelationshipPattern_leftArrow = (Core.Name "leftArrow")

_RelationshipPattern_detail = (Core.Name "detail")

_RelationshipPattern_rightArrow = (Core.Name "rightArrow")

_RelationshipPattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RelationshipPattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "leftArrow"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "detail"),
      Core.fieldTypeType = (Core.TypeOptional _RelationshipDetail_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "rightArrow"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)}]}))

data RelationshipDetail = 
  RelationshipDetail {
    relationshipDetailVariable :: (Maybe Variable),
    relationshipDetailTypes :: (Maybe RelationshipTypes),
    relationshipDetailRange :: (Maybe RangeLiteral),
    relationshipDetailProperties :: (Maybe Properties)}
  deriving (Eq, Ord, Read, Show)

_RelationshipDetail = (Core.Name "hydra/langs/cypher/openCypher.RelationshipDetail")

_RelationshipDetail_variable = (Core.Name "variable")

_RelationshipDetail_types = (Core.Name "types")

_RelationshipDetail_range = (Core.Name "range")

_RelationshipDetail_properties = (Core.Name "properties")

_RelationshipDetail_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RelationshipDetail"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "types"),
      Core.fieldTypeType = (Core.TypeOptional _RelationshipTypes_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = (Core.TypeOptional _RangeLiteral_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeOptional _Properties_type_)}]}))

data Properties = 
  PropertiesMap MapLiteral |
  PropertiesParameter Parameter
  deriving (Eq, Ord, Read, Show)

_Properties = (Core.Name "hydra/langs/cypher/openCypher.Properties")

_Properties_map = (Core.Name "map")

_Properties_parameter = (Core.Name "parameter")

_Properties_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Properties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _MapLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameter"),
      Core.fieldTypeType = _Parameter_type_}]}))

newtype RelationshipTypes = 
  RelationshipTypes {
    unRelationshipTypes :: [RelTypeName]}
  deriving (Eq, Ord, Read, Show)

_RelationshipTypes = (Core.Name "hydra/langs/cypher/openCypher.RelationshipTypes")

_RelationshipTypes_type_ = (Core.TypeList _RelTypeName_type_)

newtype NodeLabels = 
  NodeLabels {
    unNodeLabels :: [NodeLabel]}
  deriving (Eq, Ord, Read, Show)

_NodeLabels = (Core.Name "hydra/langs/cypher/openCypher.NodeLabels")

_NodeLabels_type_ = (Core.TypeList _NodeLabel_type_)

newtype NodeLabel = 
  NodeLabel {
    unNodeLabel :: String}
  deriving (Eq, Ord, Read, Show)

_NodeLabel = (Core.Name "hydra/langs/cypher/openCypher.NodeLabel")

_NodeLabel_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data RangeLiteral = 
  RangeLiteral {
    rangeLiteralStart :: (Maybe Integer),
    rangeLiteralEnd :: (Maybe Integer)}
  deriving (Eq, Ord, Read, Show)

_RangeLiteral = (Core.Name "hydra/langs/cypher/openCypher.RangeLiteral")

_RangeLiteral_start = (Core.Name "start")

_RangeLiteral_end = (Core.Name "end")

_RangeLiteral_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RangeLiteral"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "start"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "end"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)))}]}))

newtype RelTypeName = 
  RelTypeName {
    unRelTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_RelTypeName = (Core.Name "hydra/langs/cypher/openCypher.RelTypeName")

_RelTypeName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionAtom :: Atom,
    propertyExpressionLookups :: [PropertyLookup]}
  deriving (Eq, Ord, Read, Show)

_PropertyExpression = (Core.Name "hydra/langs/cypher/openCypher.PropertyExpression")

_PropertyExpression_atom = (Core.Name "atom")

_PropertyExpression_lookups = (Core.Name "lookups")

_PropertyExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PropertyExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "atom"),
      Core.fieldTypeType = _Atom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lookups"),
      Core.fieldTypeType = (Core.TypeList _PropertyLookup_type_)}]}))

newtype Expression = 
  Expression {
    unExpression :: OrExpression}
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/cypher/openCypher.Expression")

_Expression_type_ = _OrExpression_type_

newtype OrExpression = 
  OrExpression {
    unOrExpression :: [XorExpression]}
  deriving (Eq, Ord, Read, Show)

_OrExpression = (Core.Name "hydra/langs/cypher/openCypher.OrExpression")

_OrExpression_type_ = (Core.TypeList _XorExpression_type_)

newtype XorExpression = 
  XorExpression {
    unXorExpression :: [AndExpression]}
  deriving (Eq, Ord, Read, Show)

_XorExpression = (Core.Name "hydra/langs/cypher/openCypher.XorExpression")

_XorExpression_type_ = (Core.TypeList _AndExpression_type_)

newtype AndExpression = 
  AndExpression {
    unAndExpression :: [NotExpression]}
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra/langs/cypher/openCypher.AndExpression")

_AndExpression_type_ = (Core.TypeList _NotExpression_type_)

data NotExpression = 
  NotExpression {
    notExpressionNot :: Bool,
    notExpressionExpression :: ComparisonExpression}
  deriving (Eq, Ord, Read, Show)

_NotExpression = (Core.Name "hydra/langs/cypher/openCypher.NotExpression")

_NotExpression_not = (Core.Name "not")

_NotExpression_expression = (Core.Name "expression")

_NotExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.NotExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _ComparisonExpression_type_}]}))

data ComparisonExpression = 
  ComparisonExpression {
    comparisonExpressionLeft :: StringListNullPredicateExpression,
    comparisonExpressionRight :: [PartialComparisonExpression]}
  deriving (Eq, Ord, Read, Show)

_ComparisonExpression = (Core.Name "hydra/langs/cypher/openCypher.ComparisonExpression")

_ComparisonExpression_left = (Core.Name "left")

_ComparisonExpression_right = (Core.Name "right")

_ComparisonExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ComparisonExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _StringListNullPredicateExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeList _PartialComparisonExpression_type_)}]}))

data ComparisonOperator = 
  ComparisonOperatorEq  |
  ComparisonOperatorNeq  |
  ComparisonOperatorLt  |
  ComparisonOperatorGt  |
  ComparisonOperatorLte  |
  ComparisonOperatorGte 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra/langs/cypher/openCypher.ComparisonOperator")

_ComparisonOperator_eq = (Core.Name "eq")

_ComparisonOperator_neq = (Core.Name "neq")

_ComparisonOperator_lt = (Core.Name "lt")

_ComparisonOperator_gt = (Core.Name "gt")

_ComparisonOperator_lte = (Core.Name "lte")

_ComparisonOperator_gte = (Core.Name "gte")

_ComparisonOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ComparisonOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "eq"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "neq"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lt"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gt"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "gte"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data PartialComparisonExpression = 
  PartialComparisonExpression {
    partialComparisonExpressionOperator :: ComparisonOperator,
    partialComparisonExpressionRight :: StringListNullPredicateExpression}
  deriving (Eq, Ord, Read, Show)

_PartialComparisonExpression = (Core.Name "hydra/langs/cypher/openCypher.PartialComparisonExpression")

_PartialComparisonExpression_operator = (Core.Name "operator")

_PartialComparisonExpression_right = (Core.Name "right")

_PartialComparisonExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PartialComparisonExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _ComparisonOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _StringListNullPredicateExpression_type_}]}))

data StringListNullPredicateExpression = 
  StringListNullPredicateExpression {
    stringListNullPredicateExpressionLeft :: AddOrSubtractExpression,
    stringListNullPredicateExpressionRight :: [StringListNullPredicateRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateExpression")

_StringListNullPredicateExpression_left = (Core.Name "left")

_StringListNullPredicateExpression_right = (Core.Name "right")

_StringListNullPredicateExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _AddOrSubtractExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeList _StringListNullPredicateRightHandSide_type_)}]}))

data StringListNullPredicateRightHandSide = 
  StringListNullPredicateRightHandSideString StringPredicateExpression |
  StringListNullPredicateRightHandSideList ListPredicateExpression |
  StringListNullPredicateRightHandSideNull NullPredicateExpression
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateRightHandSide = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateRightHandSide")

_StringListNullPredicateRightHandSide_string = (Core.Name "string")

_StringListNullPredicateRightHandSide_list = (Core.Name "list")

_StringListNullPredicateRightHandSide_null = (Core.Name "null")

_StringListNullPredicateRightHandSide_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateRightHandSide"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringPredicateExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _ListPredicateExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = _NullPredicateExpression_type_}]}))

data StringPredicateExpression = 
  StringPredicateExpression {
    stringPredicateExpressionOperator :: StringPredicateOperator,
    stringPredicateExpressionExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_StringPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateExpression")

_StringPredicateExpression_operator = (Core.Name "operator")

_StringPredicateExpression_expression = (Core.Name "expression")

_StringPredicateExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _StringPredicateOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _AddOrSubtractExpression_type_}]}))

data StringPredicateOperator = 
  StringPredicateOperatorStartsWith  |
  StringPredicateOperatorEndsWith  |
  StringPredicateOperatorContains 
  deriving (Eq, Ord, Read, Show)

_StringPredicateOperator = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateOperator")

_StringPredicateOperator_startsWith = (Core.Name "startsWith")

_StringPredicateOperator_endsWith = (Core.Name "endsWith")

_StringPredicateOperator_contains = (Core.Name "contains")

_StringPredicateOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "startsWith"),
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
      Core.fieldTypeName = (Core.Name "contains"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype ListPredicateExpression = 
  ListPredicateExpression {
    unListPredicateExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_ListPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.ListPredicateExpression")

_ListPredicateExpression_type_ = _AddOrSubtractExpression_type_

newtype NullPredicateExpression = 
  NullPredicateExpression {
    unNullPredicateExpression :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.NullPredicateExpression")

_NullPredicateExpression_type_ = (Core.TypeLiteral Core.LiteralTypeBoolean)

data AddOrSubtractExpression = 
  AddOrSubtractExpression {
    addOrSubtractExpressionLeft :: MultiplyDivideModuloExpression,
    addOrSubtractExpressionRight :: [AddOrSubtractRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractExpression = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractExpression")

_AddOrSubtractExpression_left = (Core.Name "left")

_AddOrSubtractExpression_right = (Core.Name "right")

_AddOrSubtractExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _MultiplyDivideModuloExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeList _AddOrSubtractRightHandSide_type_)}]}))

data AddOrSubtractRightHandSide = 
  AddOrSubtractRightHandSide {
    addOrSubtractRightHandSideOperator :: AddOrSubtractOperator,
    addOrSubtractRightHandSideExpression :: MultiplyDivideModuloExpression}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractRightHandSide = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractRightHandSide")

_AddOrSubtractRightHandSide_operator = (Core.Name "operator")

_AddOrSubtractRightHandSide_expression = (Core.Name "expression")

_AddOrSubtractRightHandSide_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractRightHandSide"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _AddOrSubtractOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _MultiplyDivideModuloExpression_type_}]}))

data AddOrSubtractOperator = 
  AddOrSubtractOperatorAdd  |
  AddOrSubtractOperatorSubtract 
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractOperator = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractOperator")

_AddOrSubtractOperator_add = (Core.Name "add")

_AddOrSubtractOperator_subtract = (Core.Name "subtract")

_AddOrSubtractOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "add"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subtract"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data MultiplyDivideModuloExpression = 
  MultiplyDivideModuloExpression {
    multiplyDivideModuloExpressionLeft :: PowerOfExpression,
    multiplyDivideModuloExpressionRight :: [MultiplyDivideModuloRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloExpression = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloExpression")

_MultiplyDivideModuloExpression_left = (Core.Name "left")

_MultiplyDivideModuloExpression_right = (Core.Name "right")

_MultiplyDivideModuloExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _PowerOfExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeList _MultiplyDivideModuloRightHandSide_type_)}]}))

data MultiplyDivideModuloRightHandSide = 
  MultiplyDivideModuloRightHandSide {
    multiplyDivideModuloRightHandSideOperator :: MultiplyDivideModuloOperator,
    multiplyDivideModuloRightHandSideExpression :: PowerOfExpression}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloRightHandSide = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloRightHandSide")

_MultiplyDivideModuloRightHandSide_operator = (Core.Name "operator")

_MultiplyDivideModuloRightHandSide_expression = (Core.Name "expression")

_MultiplyDivideModuloRightHandSide_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloRightHandSide"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _MultiplyDivideModuloOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _PowerOfExpression_type_}]}))

data MultiplyDivideModuloOperator = 
  MultiplyDivideModuloOperatorMultiply  |
  MultiplyDivideModuloOperatorDivide  |
  MultiplyDivideModuloOperatorModulo 
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloOperator = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloOperator")

_MultiplyDivideModuloOperator_multiply = (Core.Name "multiply")

_MultiplyDivideModuloOperator_divide = (Core.Name "divide")

_MultiplyDivideModuloOperator_modulo = (Core.Name "modulo")

_MultiplyDivideModuloOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "multiply"),
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
      Core.fieldTypeName = (Core.Name "modulo"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

newtype PowerOfExpression = 
  PowerOfExpression {
    unPowerOfExpression :: [UnaryAddOrSubtractExpression]}
  deriving (Eq, Ord, Read, Show)

_PowerOfExpression = (Core.Name "hydra/langs/cypher/openCypher.PowerOfExpression")

_PowerOfExpression_type_ = (Core.TypeList _UnaryAddOrSubtractExpression_type_)

data UnaryAddOrSubtractExpression = 
  UnaryAddOrSubtractExpression {
    unaryAddOrSubtractExpressionOperator :: (Maybe AddOrSubtractOperator),
    unaryAddOrSubtractExpressionExpression :: NonArithmeticOperatorExpression}
  deriving (Eq, Ord, Read, Show)

_UnaryAddOrSubtractExpression = (Core.Name "hydra/langs/cypher/openCypher.UnaryAddOrSubtractExpression")

_UnaryAddOrSubtractExpression_operator = (Core.Name "operator")

_UnaryAddOrSubtractExpression_expression = (Core.Name "expression")

_UnaryAddOrSubtractExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.UnaryAddOrSubtractExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = (Core.TypeOptional _AddOrSubtractOperator_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _NonArithmeticOperatorExpression_type_}]}))

data ListOperatorExpressionOrPropertyLookup = 
  ListOperatorExpressionOrPropertyLookupList ListOperatorExpression |
  ListOperatorExpressionOrPropertyLookupProperty PropertyLookup
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpressionOrPropertyLookup = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpressionOrPropertyLookup")

_ListOperatorExpressionOrPropertyLookup_list = (Core.Name "list")

_ListOperatorExpressionOrPropertyLookup_property = (Core.Name "property")

_ListOperatorExpressionOrPropertyLookup_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpressionOrPropertyLookup"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _ListOperatorExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyLookup_type_}]}))

data NonArithmeticOperatorExpression = 
  NonArithmeticOperatorExpression {
    nonArithmeticOperatorExpressionAtom :: Atom,
    nonArithmeticOperatorExpressionListsAndLookups :: [ListOperatorExpressionOrPropertyLookup],
    nonArithmeticOperatorExpressionLabels :: (Maybe NodeLabels)}
  deriving (Eq, Ord, Read, Show)

_NonArithmeticOperatorExpression = (Core.Name "hydra/langs/cypher/openCypher.NonArithmeticOperatorExpression")

_NonArithmeticOperatorExpression_atom = (Core.Name "atom")

_NonArithmeticOperatorExpression_listsAndLookups = (Core.Name "listsAndLookups")

_NonArithmeticOperatorExpression_labels = (Core.Name "labels")

_NonArithmeticOperatorExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.NonArithmeticOperatorExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "atom"),
      Core.fieldTypeType = _Atom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "listsAndLookups"),
      Core.fieldTypeType = (Core.TypeList _ListOperatorExpressionOrPropertyLookup_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "labels"),
      Core.fieldTypeType = (Core.TypeOptional _NodeLabels_type_)}]}))

data RangeExpression = 
  RangeExpression {
    rangeExpressionStart :: (Maybe Expression),
    rangeExpressionEnd :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_RangeExpression = (Core.Name "hydra/langs/cypher/openCypher.RangeExpression")

_RangeExpression_start = (Core.Name "start")

_RangeExpression_end = (Core.Name "end")

_RangeExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.RangeExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "start"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "end"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)}]}))

data ListOperatorExpression = 
  ListOperatorExpressionSingle Expression |
  ListOperatorExpressionRange RangeExpression
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpression = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpression")

_ListOperatorExpression_single = (Core.Name "single")

_ListOperatorExpression_range = (Core.Name "range")

_ListOperatorExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "single"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _RangeExpression_type_}]}))

newtype PropertyLookup = 
  PropertyLookup {
    unPropertyLookup :: PropertyKeyName}
  deriving (Eq, Ord, Read, Show)

_PropertyLookup = (Core.Name "hydra/langs/cypher/openCypher.PropertyLookup")

_PropertyLookup_type_ = _PropertyKeyName_type_

data Atom = 
  AtomLiteral Literal |
  AtomParameter Parameter |
  AtomCase CaseExpression |
  AtomCountStar  |
  AtomListComprehension ListComprehension |
  AtomPatternComprehension PatternComprehension |
  AtomQuantifier Quantifier |
  AtomPatternPredicate PatternPredicate |
  AtomParenthesized ParenthesizedExpression |
  AtomFunctionInvocation FunctionInvocation |
  AtomExistentialSubquery ExistentialSubquery |
  AtomVariable Variable
  deriving (Eq, Ord, Read, Show)

_Atom = (Core.Name "hydra/langs/cypher/openCypher.Atom")

_Atom_literal = (Core.Name "literal")

_Atom_parameter = (Core.Name "parameter")

_Atom_case = (Core.Name "case")

_Atom_countStar = (Core.Name "countStar")

_Atom_listComprehension = (Core.Name "listComprehension")

_Atom_patternComprehension = (Core.Name "patternComprehension")

_Atom_quantifier = (Core.Name "quantifier")

_Atom_patternPredicate = (Core.Name "patternPredicate")

_Atom_parenthesized = (Core.Name "parenthesized")

_Atom_functionInvocation = (Core.Name "functionInvocation")

_Atom_existentialSubquery = (Core.Name "existentialSubquery")

_Atom_variable = (Core.Name "variable")

_Atom_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Atom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parameter"),
      Core.fieldTypeType = _Parameter_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "case"),
      Core.fieldTypeType = _CaseExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "countStar"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "listComprehension"),
      Core.fieldTypeType = _ListComprehension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "patternComprehension"),
      Core.fieldTypeType = _PatternComprehension_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "quantifier"),
      Core.fieldTypeType = _Quantifier_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "patternPredicate"),
      Core.fieldTypeType = _PatternPredicate_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "parenthesized"),
      Core.fieldTypeType = _ParenthesizedExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "functionInvocation"),
      Core.fieldTypeType = _FunctionInvocation_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "existentialSubquery"),
      Core.fieldTypeType = _ExistentialSubquery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_}]}))

data CaseExpression = 
  CaseExpression {
    caseExpressionExpression :: (Maybe Expression),
    caseExpressionAlternatives :: [CaseAlternative],
    caseExpressionElse :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra/langs/cypher/openCypher.CaseExpression")

_CaseExpression_expression = (Core.Name "expression")

_CaseExpression_alternatives = (Core.Name "alternatives")

_CaseExpression_else = (Core.Name "else")

_CaseExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.CaseExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "alternatives"),
      Core.fieldTypeType = (Core.TypeList _CaseAlternative_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "else"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)}]}))

data CaseAlternative = 
  CaseAlternative {
    caseAlternativeCondition :: Expression,
    caseAlternativeResult :: Expression}
  deriving (Eq, Ord, Read, Show)

_CaseAlternative = (Core.Name "hydra/langs/cypher/openCypher.CaseAlternative")

_CaseAlternative_condition = (Core.Name "condition")

_CaseAlternative_result = (Core.Name "result")

_CaseAlternative_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.CaseAlternative"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "condition"),
      Core.fieldTypeType = _Expression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "result"),
      Core.fieldTypeType = _Expression_type_}]}))

data ListComprehension = 
  ListComprehension {
    listComprehensionLeft :: FilterExpression,
    listComprehensionRight :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ListComprehension = (Core.Name "hydra/langs/cypher/openCypher.ListComprehension")

_ListComprehension_left = (Core.Name "left")

_ListComprehension_right = (Core.Name "right")

_ListComprehension_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ListComprehension"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "left"),
      Core.fieldTypeType = _FilterExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = (Core.TypeOptional _Expression_type_)}]}))

data PatternComprehension = 
  PatternComprehension {
    patternComprehensionVariable :: (Maybe Variable),
    patternComprehensionPattern :: RelationshipsPattern,
    patternComprehensionWhere :: (Maybe Where),
    patternComprehensionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_PatternComprehension = (Core.Name "hydra/langs/cypher/openCypher.PatternComprehension")

_PatternComprehension_variable = (Core.Name "variable")

_PatternComprehension_pattern = (Core.Name "pattern")

_PatternComprehension_where = (Core.Name "where")

_PatternComprehension_right = (Core.Name "right")

_PatternComprehension_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PatternComprehension"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = (Core.TypeOptional _Variable_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _RelationshipsPattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Where_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "right"),
      Core.fieldTypeType = _Expression_type_}]}))

data Quantifier = 
  Quantifier {
    quantifierOperator :: QuantifierOperator,
    quantifierExpression :: FilterExpression}
  deriving (Eq, Ord, Read, Show)

_Quantifier = (Core.Name "hydra/langs/cypher/openCypher.Quantifier")

_Quantifier_operator = (Core.Name "operator")

_Quantifier_expression = (Core.Name "expression")

_Quantifier_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Quantifier"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "operator"),
      Core.fieldTypeType = _QuantifierOperator_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _FilterExpression_type_}]}))

data QuantifierOperator = 
  QuantifierOperatorAll  |
  QuantifierOperatorAny  |
  QuantifierOperatorNone  |
  QuantifierOperatorSingle 
  deriving (Eq, Ord, Read, Show)

_QuantifierOperator = (Core.Name "hydra/langs/cypher/openCypher.QuantifierOperator")

_QuantifierOperator_all = (Core.Name "all")

_QuantifierOperator_any = (Core.Name "any")

_QuantifierOperator_none = (Core.Name "none")

_QuantifierOperator_single = (Core.Name "single")

_QuantifierOperator_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.QuantifierOperator"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "all"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "any"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "none"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "single"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

data FilterExpression = 
  FilterExpression {
    filterExpressionIdInColl :: IdInColl,
    filterExpressionWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_FilterExpression = (Core.Name "hydra/langs/cypher/openCypher.FilterExpression")

_FilterExpression_idInColl = (Core.Name "idInColl")

_FilterExpression_where = (Core.Name "where")

_FilterExpression_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.FilterExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "idInColl"),
      Core.fieldTypeType = _IdInColl_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Where_type_)}]}))

newtype PatternPredicate = 
  PatternPredicate {
    unPatternPredicate :: RelationshipsPattern}
  deriving (Eq, Ord, Read, Show)

_PatternPredicate = (Core.Name "hydra/langs/cypher/openCypher.PatternPredicate")

_PatternPredicate_type_ = _RelationshipsPattern_type_

newtype ParenthesizedExpression = 
  ParenthesizedExpression {
    unParenthesizedExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedExpression = (Core.Name "hydra/langs/cypher/openCypher.ParenthesizedExpression")

_ParenthesizedExpression_type_ = _Expression_type_

data IdInColl = 
  IdInColl {
    idInCollVariable :: Variable,
    idInCollExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_IdInColl = (Core.Name "hydra/langs/cypher/openCypher.IdInColl")

_IdInColl_variable = (Core.Name "variable")

_IdInColl_expression = (Core.Name "expression")

_IdInColl_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.IdInColl"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "variable"),
      Core.fieldTypeType = _Variable_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "expression"),
      Core.fieldTypeType = _Expression_type_}]}))

data FunctionInvocation = 
  FunctionInvocation {
    functionInvocationName :: QualifiedName,
    functionInvocationDistinct :: Bool,
    functionInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionInvocation = (Core.Name "hydra/langs/cypher/openCypher.FunctionInvocation")

_FunctionInvocation_name = (Core.Name "name")

_FunctionInvocation_distinct = (Core.Name "distinct")

_FunctionInvocation_arguments = (Core.Name "arguments")

_FunctionInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.FunctionInvocation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "distinct"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)}]}))

data QualifiedName = 
  QualifiedName {
    qualifiedNameNamespace :: String,
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra/langs/cypher/openCypher.QualifiedName")

_QualifiedName_namespace = (Core.Name "namespace")

_QualifiedName_local = (Core.Name "local")

_QualifiedName_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.QualifiedName"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namespace"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "local"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)}]}))

data PatternWhere = 
  PatternWhere {
    patternWherePattern :: Pattern,
    patternWhereWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_PatternWhere = (Core.Name "hydra/langs/cypher/openCypher.PatternWhere")

_PatternWhere_pattern = (Core.Name "pattern")

_PatternWhere_where = (Core.Name "where")

_PatternWhere_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.PatternWhere"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "where"),
      Core.fieldTypeType = (Core.TypeOptional _Where_type_)}]}))

data ExistentialSubquery = 
  ExistentialSubqueryRegular RegularQuery |
  ExistentialSubqueryPattern PatternWhere
  deriving (Eq, Ord, Read, Show)

_ExistentialSubquery = (Core.Name "hydra/langs/cypher/openCypher.ExistentialSubquery")

_ExistentialSubquery_regular = (Core.Name "regular")

_ExistentialSubquery_pattern = (Core.Name "pattern")

_ExistentialSubquery_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ExistentialSubquery"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "regular"),
      Core.fieldTypeType = _RegularQuery_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _PatternWhere_type_}]}))

data ExplicitProcedureInvocation = 
  ExplicitProcedureInvocation {
    explicitProcedureInvocationName :: QualifiedName,
    explicitProcedureInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ExplicitProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ExplicitProcedureInvocation")

_ExplicitProcedureInvocation_name = (Core.Name "name")

_ExplicitProcedureInvocation_arguments = (Core.Name "arguments")

_ExplicitProcedureInvocation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.ExplicitProcedureInvocation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = _QualifiedName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "arguments"),
      Core.fieldTypeType = (Core.TypeList _Expression_type_)}]}))

newtype ImplicitProcedureInvocation = 
  ImplicitProcedureInvocation {
    unImplicitProcedureInvocation :: QualifiedName}
  deriving (Eq, Ord, Read, Show)

_ImplicitProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ImplicitProcedureInvocation")

_ImplicitProcedureInvocation_type_ = _QualifiedName_type_

newtype ProcedureResultField = 
  ProcedureResultField {
    unProcedureResultField :: String}
  deriving (Eq, Ord, Read, Show)

_ProcedureResultField = (Core.Name "hydra/langs/cypher/openCypher.ProcedureResultField")

_ProcedureResultField_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/cypher/openCypher.Variable")

_Variable_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Literal = 
  LiteralBoolean Bool |
  LiteralNull  |
  LiteralNumber NumberLiteral |
  LiteralString StringLiteral |
  LiteralList ListLiteral |
  LiteralMap MapLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/cypher/openCypher.Literal")

_Literal_boolean = (Core.Name "boolean")

_Literal_null = (Core.Name "null")

_Literal_number = (Core.Name "number")

_Literal_string = (Core.Name "string")

_Literal_list = (Core.Name "list")

_Literal_map = (Core.Name "map")

_Literal_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Literal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "boolean"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "null"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "number"),
      Core.fieldTypeType = _NumberLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "string"),
      Core.fieldTypeType = _StringLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "list"),
      Core.fieldTypeType = _ListLiteral_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "map"),
      Core.fieldTypeType = _MapLiteral_type_}]}))

data NumberLiteral = 
  NumberLiteralDouble Double |
  NumberLiteralInteger Integer
  deriving (Eq, Ord, Read, Show)

_NumberLiteral = (Core.Name "hydra/langs/cypher/openCypher.NumberLiteral")

_NumberLiteral_double = (Core.Name "double")

_NumberLiteral_integer = (Core.Name "integer")

_NumberLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.NumberLiteral"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "double"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeFloat Core.FloatTypeFloat64))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))}]}))

newtype StringLiteral = 
  StringLiteral {
    unStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra/langs/cypher/openCypher.StringLiteral")

_StringLiteral_type_ = (Core.TypeLiteral Core.LiteralTypeString)

newtype ListLiteral = 
  ListLiteral {
    unListLiteral :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ListLiteral = (Core.Name "hydra/langs/cypher/openCypher.ListLiteral")

_ListLiteral_type_ = (Core.TypeList _Expression_type_)

newtype MapLiteral = 
  MapLiteral {
    unMapLiteral :: [KeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_MapLiteral = (Core.Name "hydra/langs/cypher/openCypher.MapLiteral")

_MapLiteral_type_ = (Core.TypeList _KeyValuePair_type_)

data KeyValuePair = 
  KeyValuePair {
    keyValuePairKey :: PropertyKeyName,
    keyValuePairValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_KeyValuePair = (Core.Name "hydra/langs/cypher/openCypher.KeyValuePair")

_KeyValuePair_key = (Core.Name "key")

_KeyValuePair_value = (Core.Name "value")

_KeyValuePair_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.KeyValuePair"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "key"),
      Core.fieldTypeType = _PropertyKeyName_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _Expression_type_}]}))

newtype PropertyKeyName = 
  PropertyKeyName {
    unPropertyKeyName :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyName = (Core.Name "hydra/langs/cypher/openCypher.PropertyKeyName")

_PropertyKeyName_type_ = (Core.TypeLiteral Core.LiteralTypeString)

data Parameter = 
  ParameterSymbolic String |
  ParameterInteger Integer
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra/langs/cypher/openCypher.Parameter")

_Parameter_symbolic = (Core.Name "symbolic")

_Parameter_integer = (Core.Name "integer")

_Parameter_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/cypher/openCypher.Parameter"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "symbolic"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "integer"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))}]}))