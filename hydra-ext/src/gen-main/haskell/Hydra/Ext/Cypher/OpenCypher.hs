-- Note: this is an automatically generated file. Do not edit.

-- | A Cypher model based on the OpenCypher specification (version 23), copyright Neo Technology, available at:
-- |   https://opencypher.org/resources/

module Hydra.Ext.Cypher.OpenCypher where

import qualified Hydra.Core as Core
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

data Query = 
  QueryRegular RegularQuery |
  QueryStandalone StandaloneCall
  deriving (Eq, Ord, Read, Show)

_Query = (Core.Name "hydra.ext.cypher.openCypher.Query")

_Query_regular = (Core.Name "regular")

_Query_standalone = (Core.Name "standalone")

data RegularQuery = 
  RegularQuery {
    regularQueryHead :: SingleQuery,
    regularQueryRest :: [Union]}
  deriving (Eq, Ord, Read, Show)

_RegularQuery = (Core.Name "hydra.ext.cypher.openCypher.RegularQuery")

_RegularQuery_head = (Core.Name "head")

_RegularQuery_rest = (Core.Name "rest")

data Union = 
  Union {
    unionAll :: Bool,
    unionQuery :: SingleQuery}
  deriving (Eq, Ord, Read, Show)

_Union = (Core.Name "hydra.ext.cypher.openCypher.Union")

_Union_all = (Core.Name "all")

_Union_query = (Core.Name "query")

data SingleQuery = 
  SingleQuerySinglePart SinglePartQuery |
  SingleQueryMultiPart MultiPartQuery
  deriving (Eq, Ord, Read, Show)

_SingleQuery = (Core.Name "hydra.ext.cypher.openCypher.SingleQuery")

_SingleQuery_singlePart = (Core.Name "singlePart")

_SingleQuery_multiPart = (Core.Name "multiPart")

data SinglePartQuery = 
  SinglePartQuery {
    singlePartQueryReading :: [ReadingClause],
    singlePartQueryUpdating :: [UpdatingClause],
    singlePartQueryReturn :: (Maybe Return)}
  deriving (Eq, Ord, Read, Show)

_SinglePartQuery = (Core.Name "hydra.ext.cypher.openCypher.SinglePartQuery")

_SinglePartQuery_reading = (Core.Name "reading")

_SinglePartQuery_updating = (Core.Name "updating")

_SinglePartQuery_return = (Core.Name "return")

data WithClause = 
  WithClause {
    withClauseReading :: [ReadingClause],
    withClauseUpdating :: [UpdatingClause],
    withClauseWith :: With}
  deriving (Eq, Ord, Read, Show)

_WithClause = (Core.Name "hydra.ext.cypher.openCypher.WithClause")

_WithClause_reading = (Core.Name "reading")

_WithClause_updating = (Core.Name "updating")

_WithClause_with = (Core.Name "with")

data MultiPartQuery = 
  MultiPartQuery {
    multiPartQueryWith :: [WithClause],
    multiPartQueryBody :: SinglePartQuery}
  deriving (Eq, Ord, Read, Show)

_MultiPartQuery = (Core.Name "hydra.ext.cypher.openCypher.MultiPartQuery")

_MultiPartQuery_with = (Core.Name "with")

_MultiPartQuery_body = (Core.Name "body")

data UpdatingClause = 
  UpdatingClauseCreate Create |
  UpdatingClauseMerge Merge |
  UpdatingClauseDelete Delete |
  UpdatingClauseSet Set |
  UpdatingClauseRemove Remove
  deriving (Eq, Ord, Read, Show)

_UpdatingClause = (Core.Name "hydra.ext.cypher.openCypher.UpdatingClause")

_UpdatingClause_create = (Core.Name "create")

_UpdatingClause_merge = (Core.Name "merge")

_UpdatingClause_delete = (Core.Name "delete")

_UpdatingClause_set = (Core.Name "set")

_UpdatingClause_remove = (Core.Name "remove")

data ReadingClause = 
  ReadingClauseMatch Match |
  ReadingClauseUnwind Unwind |
  ReadingClauseInQueryCall InQueryCall
  deriving (Eq, Ord, Read, Show)

_ReadingClause = (Core.Name "hydra.ext.cypher.openCypher.ReadingClause")

_ReadingClause_match = (Core.Name "match")

_ReadingClause_unwind = (Core.Name "unwind")

_ReadingClause_inQueryCall = (Core.Name "inQueryCall")

data Match = 
  Match {
    matchOptional :: Bool,
    matchPattern :: Pattern,
    matchWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_Match = (Core.Name "hydra.ext.cypher.openCypher.Match")

_Match_optional = (Core.Name "optional")

_Match_pattern = (Core.Name "pattern")

_Match_where = (Core.Name "where")

data Unwind = 
  Unwind {
    unwindExpression :: Expression,
    unwindVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_Unwind = (Core.Name "hydra.ext.cypher.openCypher.Unwind")

_Unwind_expression = (Core.Name "expression")

_Unwind_variable = (Core.Name "variable")

data Merge = 
  Merge {
    mergePatternPart :: PatternPart,
    mergeActions :: [MergeAction]}
  deriving (Eq, Ord, Read, Show)

_Merge = (Core.Name "hydra.ext.cypher.openCypher.Merge")

_Merge_patternPart = (Core.Name "patternPart")

_Merge_actions = (Core.Name "actions")

data MatchOrCreate = 
  MatchOrCreateMatch  |
  MatchOrCreateCreate 
  deriving (Eq, Ord, Read, Show)

_MatchOrCreate = (Core.Name "hydra.ext.cypher.openCypher.MatchOrCreate")

_MatchOrCreate_match = (Core.Name "match")

_MatchOrCreate_create = (Core.Name "create")

data MergeAction = 
  MergeAction {
    mergeActionAction :: MatchOrCreate,
    mergeActionSet :: Set}
  deriving (Eq, Ord, Read, Show)

_MergeAction = (Core.Name "hydra.ext.cypher.openCypher.MergeAction")

_MergeAction_action = (Core.Name "action")

_MergeAction_set = (Core.Name "set")

newtype Create = 
  Create {
    unCreate :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Create = (Core.Name "hydra.ext.cypher.openCypher.Create")

newtype Set = 
  Set {
    unSet :: [SetItem]}
  deriving (Eq, Ord, Read, Show)

_Set = (Core.Name "hydra.ext.cypher.openCypher.Set")

data SetItem = 
  SetItemProperty PropertyEquals |
  SetItemVariableEqual VariableEquals |
  SetItemVariablePlusEqual VariablePlusEquals |
  SetItemVariableLabels VariableAndNodeLabels
  deriving (Eq, Ord, Read, Show)

_SetItem = (Core.Name "hydra.ext.cypher.openCypher.SetItem")

_SetItem_property = (Core.Name "property")

_SetItem_variableEqual = (Core.Name "variableEqual")

_SetItem_variablePlusEqual = (Core.Name "variablePlusEqual")

_SetItem_variableLabels = (Core.Name "variableLabels")

data PropertyEquals = 
  PropertyEquals {
    propertyEqualsLhs :: PropertyExpression,
    propertyEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_PropertyEquals = (Core.Name "hydra.ext.cypher.openCypher.PropertyEquals")

_PropertyEquals_lhs = (Core.Name "lhs")

_PropertyEquals_rhs = (Core.Name "rhs")

data VariableEquals = 
  VariableEquals {
    variableEqualsLhs :: Variable,
    variableEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariableEquals = (Core.Name "hydra.ext.cypher.openCypher.VariableEquals")

_VariableEquals_lhs = (Core.Name "lhs")

_VariableEquals_rhs = (Core.Name "rhs")

data VariablePlusEquals = 
  VariablePlusEquals {
    variablePlusEqualsLhs :: Variable,
    variablePlusEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariablePlusEquals = (Core.Name "hydra.ext.cypher.openCypher.VariablePlusEquals")

_VariablePlusEquals_lhs = (Core.Name "lhs")

_VariablePlusEquals_rhs = (Core.Name "rhs")

data VariableAndNodeLabels = 
  VariableAndNodeLabels {
    variableAndNodeLabelsVariable :: Variable,
    variableAndNodeLabelsLabels :: NodeLabels}
  deriving (Eq, Ord, Read, Show)

_VariableAndNodeLabels = (Core.Name "hydra.ext.cypher.openCypher.VariableAndNodeLabels")

_VariableAndNodeLabels_variable = (Core.Name "variable")

_VariableAndNodeLabels_labels = (Core.Name "labels")

data Delete = 
  Delete {
    deleteDetach :: Bool,
    deleteExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Delete = (Core.Name "hydra.ext.cypher.openCypher.Delete")

_Delete_detach = (Core.Name "detach")

_Delete_expressions = (Core.Name "expressions")

newtype Remove = 
  Remove {
    unRemove :: [RemoveItem]}
  deriving (Eq, Ord, Read, Show)

_Remove = (Core.Name "hydra.ext.cypher.openCypher.Remove")

data RemoveItem = 
  RemoveItemVariableLabels VariableAndNodeLabels |
  RemoveItemProperty PropertyExpression
  deriving (Eq, Ord, Read, Show)

_RemoveItem = (Core.Name "hydra.ext.cypher.openCypher.RemoveItem")

_RemoveItem_variableLabels = (Core.Name "variableLabels")

_RemoveItem_property = (Core.Name "property")

data InQueryCall = 
  InQueryCall {
    inQueryCallCall :: ExplicitProcedureInvocation,
    inQueryCallYieldItems :: (Maybe YieldItems)}
  deriving (Eq, Ord, Read, Show)

_InQueryCall = (Core.Name "hydra.ext.cypher.openCypher.InQueryCall")

_InQueryCall_call = (Core.Name "call")

_InQueryCall_yieldItems = (Core.Name "yieldItems")

data ProcedureInvocation = 
  ProcedureInvocationExplicit ExplicitProcedureInvocation |
  ProcedureInvocationImplicit ImplicitProcedureInvocation
  deriving (Eq, Ord, Read, Show)

_ProcedureInvocation = (Core.Name "hydra.ext.cypher.openCypher.ProcedureInvocation")

_ProcedureInvocation_explicit = (Core.Name "explicit")

_ProcedureInvocation_implicit = (Core.Name "implicit")

data StarOrYieldItems = 
  StarOrYieldItemsStar  |
  StarOrYieldItemsItems YieldItems
  deriving (Eq, Ord, Read, Show)

_StarOrYieldItems = (Core.Name "hydra.ext.cypher.openCypher.StarOrYieldItems")

_StarOrYieldItems_star = (Core.Name "star")

_StarOrYieldItems_items = (Core.Name "items")

data StandaloneCall = 
  StandaloneCall {
    standaloneCallCall :: ProcedureInvocation,
    standaloneCallYieldItems :: (Maybe StarOrYieldItems)}
  deriving (Eq, Ord, Read, Show)

_StandaloneCall = (Core.Name "hydra.ext.cypher.openCypher.StandaloneCall")

_StandaloneCall_call = (Core.Name "call")

_StandaloneCall_yieldItems = (Core.Name "yieldItems")

data YieldItems = 
  YieldItems {
    yieldItemsItems :: [YieldItem],
    yieldItemsWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_YieldItems = (Core.Name "hydra.ext.cypher.openCypher.YieldItems")

_YieldItems_items = (Core.Name "items")

_YieldItems_where = (Core.Name "where")

data YieldItem = 
  YieldItem {
    yieldItemField :: (Maybe ProcedureResultField),
    yieldItemVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_YieldItem = (Core.Name "hydra.ext.cypher.openCypher.YieldItem")

_YieldItem_field = (Core.Name "field")

_YieldItem_variable = (Core.Name "variable")

data With = 
  With {
    withProjection :: ProjectionBody,
    withWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_With = (Core.Name "hydra.ext.cypher.openCypher.With")

_With_projection = (Core.Name "projection")

_With_where = (Core.Name "where")

newtype Return = 
  Return {
    unReturn :: ProjectionBody}
  deriving (Eq, Ord, Read, Show)

_Return = (Core.Name "hydra.ext.cypher.openCypher.Return")

data ProjectionBody = 
  ProjectionBody {
    projectionBodyDistinct :: Bool,
    projectionBodyProjectionItems :: ProjectionItems,
    projectionBodyOrder :: (Maybe Order),
    projectionBodySkip :: (Maybe Skip),
    projectionBodyLimit :: (Maybe Limit)}
  deriving (Eq, Ord, Read, Show)

_ProjectionBody = (Core.Name "hydra.ext.cypher.openCypher.ProjectionBody")

_ProjectionBody_distinct = (Core.Name "distinct")

_ProjectionBody_projectionItems = (Core.Name "projectionItems")

_ProjectionBody_order = (Core.Name "order")

_ProjectionBody_skip = (Core.Name "skip")

_ProjectionBody_limit = (Core.Name "limit")

data ProjectionItems = 
  ProjectionItems {
    projectionItemsStar :: Bool,
    projectionItemsExplicit :: [ProjectionItem]}
  deriving (Eq, Ord, Read, Show)

_ProjectionItems = (Core.Name "hydra.ext.cypher.openCypher.ProjectionItems")

_ProjectionItems_star = (Core.Name "star")

_ProjectionItems_explicit = (Core.Name "explicit")

data ProjectionItem = 
  ProjectionItem {
    projectionItemExpression :: Expression,
    projectionItemVariable :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_ProjectionItem = (Core.Name "hydra.ext.cypher.openCypher.ProjectionItem")

_ProjectionItem_expression = (Core.Name "expression")

_ProjectionItem_variable = (Core.Name "variable")

newtype Order = 
  Order {
    unOrder :: [SortItem]}
  deriving (Eq, Ord, Read, Show)

_Order = (Core.Name "hydra.ext.cypher.openCypher.Order")

newtype Skip = 
  Skip {
    unSkip :: Expression}
  deriving (Eq, Ord, Read, Show)

_Skip = (Core.Name "hydra.ext.cypher.openCypher.Skip")

newtype Limit = 
  Limit {
    unLimit :: Expression}
  deriving (Eq, Ord, Read, Show)

_Limit = (Core.Name "hydra.ext.cypher.openCypher.Limit")

data SortOrder = 
  SortOrderAscending  |
  SortOrderDescending 
  deriving (Eq, Ord, Read, Show)

_SortOrder = (Core.Name "hydra.ext.cypher.openCypher.SortOrder")

_SortOrder_ascending = (Core.Name "ascending")

_SortOrder_descending = (Core.Name "descending")

data SortItem = 
  SortItem {
    sortItemExpression :: Expression,
    sortItemOrder :: (Maybe SortOrder)}
  deriving (Eq, Ord, Read, Show)

_SortItem = (Core.Name "hydra.ext.cypher.openCypher.SortItem")

_SortItem_expression = (Core.Name "expression")

_SortItem_order = (Core.Name "order")

newtype Where = 
  Where {
    unWhere :: Expression}
  deriving (Eq, Ord, Read, Show)

_Where = (Core.Name "hydra.ext.cypher.openCypher.Where")

newtype Pattern = 
  Pattern {
    unPattern :: [PatternPart]}
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra.ext.cypher.openCypher.Pattern")

data PatternPart = 
  PatternPart {
    patternPartVariable :: (Maybe Variable),
    patternPartPattern :: AnonymousPatternPart}
  deriving (Eq, Ord, Read, Show)

_PatternPart = (Core.Name "hydra.ext.cypher.openCypher.PatternPart")

_PatternPart_variable = (Core.Name "variable")

_PatternPart_pattern = (Core.Name "pattern")

newtype AnonymousPatternPart = 
  AnonymousPatternPart {
    unAnonymousPatternPart :: PatternElement}
  deriving (Eq, Ord, Read, Show)

_AnonymousPatternPart = (Core.Name "hydra.ext.cypher.openCypher.AnonymousPatternPart")

data NodePatternChain = 
  NodePatternChain {
    nodePatternChainNodePattern :: NodePattern,
    nodePatternChainChain :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_NodePatternChain = (Core.Name "hydra.ext.cypher.openCypher.NodePatternChain")

_NodePatternChain_nodePattern = (Core.Name "nodePattern")

_NodePatternChain_chain = (Core.Name "chain")

data PatternElement = 
  PatternElementChained NodePatternChain |
  PatternElementParenthesized PatternElement
  deriving (Eq, Ord, Read, Show)

_PatternElement = (Core.Name "hydra.ext.cypher.openCypher.PatternElement")

_PatternElement_chained = (Core.Name "chained")

_PatternElement_parenthesized = (Core.Name "parenthesized")

data RelationshipsPattern = 
  RelationshipsPattern {
    relationshipsPatternNodePattern :: NodePattern,
    relationshipsPatternChain :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_RelationshipsPattern = (Core.Name "hydra.ext.cypher.openCypher.RelationshipsPattern")

_RelationshipsPattern_nodePattern = (Core.Name "nodePattern")

_RelationshipsPattern_chain = (Core.Name "chain")

data NodePattern = 
  NodePattern {
    nodePatternVariable :: (Maybe Variable),
    nodePatternLabels :: (Maybe NodeLabels),
    nodePatternProperties :: (Maybe Properties)}
  deriving (Eq, Ord, Read, Show)

_NodePattern = (Core.Name "hydra.ext.cypher.openCypher.NodePattern")

_NodePattern_variable = (Core.Name "variable")

_NodePattern_labels = (Core.Name "labels")

_NodePattern_properties = (Core.Name "properties")

data PatternElementChain = 
  PatternElementChain {
    patternElementChainRelationship :: RelationshipPattern,
    patternElementChainNode :: NodePattern}
  deriving (Eq, Ord, Read, Show)

_PatternElementChain = (Core.Name "hydra.ext.cypher.openCypher.PatternElementChain")

_PatternElementChain_relationship = (Core.Name "relationship")

_PatternElementChain_node = (Core.Name "node")

data RelationshipPattern = 
  RelationshipPattern {
    relationshipPatternLeftArrow :: Bool,
    relationshipPatternDetail :: (Maybe RelationshipDetail),
    relationshipPatternRightArrow :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipPattern = (Core.Name "hydra.ext.cypher.openCypher.RelationshipPattern")

_RelationshipPattern_leftArrow = (Core.Name "leftArrow")

_RelationshipPattern_detail = (Core.Name "detail")

_RelationshipPattern_rightArrow = (Core.Name "rightArrow")

data RelationshipDetail = 
  RelationshipDetail {
    relationshipDetailVariable :: (Maybe Variable),
    relationshipDetailTypes :: (Maybe RelationshipTypes),
    relationshipDetailRange :: (Maybe RangeLiteral),
    relationshipDetailProperties :: (Maybe Properties)}
  deriving (Eq, Ord, Read, Show)

_RelationshipDetail = (Core.Name "hydra.ext.cypher.openCypher.RelationshipDetail")

_RelationshipDetail_variable = (Core.Name "variable")

_RelationshipDetail_types = (Core.Name "types")

_RelationshipDetail_range = (Core.Name "range")

_RelationshipDetail_properties = (Core.Name "properties")

data Properties = 
  PropertiesMap MapLiteral |
  PropertiesParameter Parameter
  deriving (Eq, Ord, Read, Show)

_Properties = (Core.Name "hydra.ext.cypher.openCypher.Properties")

_Properties_map = (Core.Name "map")

_Properties_parameter = (Core.Name "parameter")

newtype RelationshipTypes = 
  RelationshipTypes {
    unRelationshipTypes :: [RelTypeName]}
  deriving (Eq, Ord, Read, Show)

_RelationshipTypes = (Core.Name "hydra.ext.cypher.openCypher.RelationshipTypes")

newtype NodeLabels = 
  NodeLabels {
    unNodeLabels :: [NodeLabel]}
  deriving (Eq, Ord, Read, Show)

_NodeLabels = (Core.Name "hydra.ext.cypher.openCypher.NodeLabels")

newtype NodeLabel = 
  NodeLabel {
    unNodeLabel :: String}
  deriving (Eq, Ord, Read, Show)

_NodeLabel = (Core.Name "hydra.ext.cypher.openCypher.NodeLabel")

data RangeLiteral = 
  RangeLiteral {
    rangeLiteralStart :: (Maybe Integer),
    rangeLiteralEnd :: (Maybe Integer)}
  deriving (Eq, Ord, Read, Show)

_RangeLiteral = (Core.Name "hydra.ext.cypher.openCypher.RangeLiteral")

_RangeLiteral_start = (Core.Name "start")

_RangeLiteral_end = (Core.Name "end")

newtype RelTypeName = 
  RelTypeName {
    unRelTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_RelTypeName = (Core.Name "hydra.ext.cypher.openCypher.RelTypeName")

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionAtom :: Atom,
    propertyExpressionLookups :: [PropertyLookup]}
  deriving (Eq, Ord, Read, Show)

_PropertyExpression = (Core.Name "hydra.ext.cypher.openCypher.PropertyExpression")

_PropertyExpression_atom = (Core.Name "atom")

_PropertyExpression_lookups = (Core.Name "lookups")

newtype Expression = 
  Expression {
    unExpression :: OrExpression}
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra.ext.cypher.openCypher.Expression")

newtype OrExpression = 
  OrExpression {
    unOrExpression :: [XorExpression]}
  deriving (Eq, Ord, Read, Show)

_OrExpression = (Core.Name "hydra.ext.cypher.openCypher.OrExpression")

newtype XorExpression = 
  XorExpression {
    unXorExpression :: [AndExpression]}
  deriving (Eq, Ord, Read, Show)

_XorExpression = (Core.Name "hydra.ext.cypher.openCypher.XorExpression")

newtype AndExpression = 
  AndExpression {
    unAndExpression :: [NotExpression]}
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra.ext.cypher.openCypher.AndExpression")

data NotExpression = 
  NotExpression {
    notExpressionNot :: Bool,
    notExpressionExpression :: ComparisonExpression}
  deriving (Eq, Ord, Read, Show)

_NotExpression = (Core.Name "hydra.ext.cypher.openCypher.NotExpression")

_NotExpression_not = (Core.Name "not")

_NotExpression_expression = (Core.Name "expression")

data ComparisonExpression = 
  ComparisonExpression {
    comparisonExpressionLeft :: StringListNullPredicateExpression,
    comparisonExpressionRight :: [PartialComparisonExpression]}
  deriving (Eq, Ord, Read, Show)

_ComparisonExpression = (Core.Name "hydra.ext.cypher.openCypher.ComparisonExpression")

_ComparisonExpression_left = (Core.Name "left")

_ComparisonExpression_right = (Core.Name "right")

data ComparisonOperator = 
  ComparisonOperatorEq  |
  ComparisonOperatorNeq  |
  ComparisonOperatorLt  |
  ComparisonOperatorGt  |
  ComparisonOperatorLte  |
  ComparisonOperatorGte 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra.ext.cypher.openCypher.ComparisonOperator")

_ComparisonOperator_eq = (Core.Name "eq")

_ComparisonOperator_neq = (Core.Name "neq")

_ComparisonOperator_lt = (Core.Name "lt")

_ComparisonOperator_gt = (Core.Name "gt")

_ComparisonOperator_lte = (Core.Name "lte")

_ComparisonOperator_gte = (Core.Name "gte")

data PartialComparisonExpression = 
  PartialComparisonExpression {
    partialComparisonExpressionOperator :: ComparisonOperator,
    partialComparisonExpressionRight :: StringListNullPredicateExpression}
  deriving (Eq, Ord, Read, Show)

_PartialComparisonExpression = (Core.Name "hydra.ext.cypher.openCypher.PartialComparisonExpression")

_PartialComparisonExpression_operator = (Core.Name "operator")

_PartialComparisonExpression_right = (Core.Name "right")

data StringListNullPredicateExpression = 
  StringListNullPredicateExpression {
    stringListNullPredicateExpressionLeft :: AddOrSubtractExpression,
    stringListNullPredicateExpressionRight :: [StringListNullPredicateRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateExpression = (Core.Name "hydra.ext.cypher.openCypher.StringListNullPredicateExpression")

_StringListNullPredicateExpression_left = (Core.Name "left")

_StringListNullPredicateExpression_right = (Core.Name "right")

data StringListNullPredicateRightHandSide = 
  StringListNullPredicateRightHandSideString StringPredicateExpression |
  StringListNullPredicateRightHandSideList ListPredicateExpression |
  StringListNullPredicateRightHandSideNull NullPredicateExpression
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateRightHandSide = (Core.Name "hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide")

_StringListNullPredicateRightHandSide_string = (Core.Name "string")

_StringListNullPredicateRightHandSide_list = (Core.Name "list")

_StringListNullPredicateRightHandSide_null = (Core.Name "null")

data StringPredicateExpression = 
  StringPredicateExpression {
    stringPredicateExpressionOperator :: StringPredicateOperator,
    stringPredicateExpressionExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_StringPredicateExpression = (Core.Name "hydra.ext.cypher.openCypher.StringPredicateExpression")

_StringPredicateExpression_operator = (Core.Name "operator")

_StringPredicateExpression_expression = (Core.Name "expression")

data StringPredicateOperator = 
  StringPredicateOperatorStartsWith  |
  StringPredicateOperatorEndsWith  |
  StringPredicateOperatorContains 
  deriving (Eq, Ord, Read, Show)

_StringPredicateOperator = (Core.Name "hydra.ext.cypher.openCypher.StringPredicateOperator")

_StringPredicateOperator_startsWith = (Core.Name "startsWith")

_StringPredicateOperator_endsWith = (Core.Name "endsWith")

_StringPredicateOperator_contains = (Core.Name "contains")

newtype ListPredicateExpression = 
  ListPredicateExpression {
    unListPredicateExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_ListPredicateExpression = (Core.Name "hydra.ext.cypher.openCypher.ListPredicateExpression")

newtype NullPredicateExpression = 
  NullPredicateExpression {
    unNullPredicateExpression :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullPredicateExpression = (Core.Name "hydra.ext.cypher.openCypher.NullPredicateExpression")

data AddOrSubtractExpression = 
  AddOrSubtractExpression {
    addOrSubtractExpressionLeft :: MultiplyDivideModuloExpression,
    addOrSubtractExpressionRight :: [AddOrSubtractRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractExpression = (Core.Name "hydra.ext.cypher.openCypher.AddOrSubtractExpression")

_AddOrSubtractExpression_left = (Core.Name "left")

_AddOrSubtractExpression_right = (Core.Name "right")

data AddOrSubtractRightHandSide = 
  AddOrSubtractRightHandSide {
    addOrSubtractRightHandSideOperator :: AddOrSubtractOperator,
    addOrSubtractRightHandSideExpression :: MultiplyDivideModuloExpression}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractRightHandSide = (Core.Name "hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide")

_AddOrSubtractRightHandSide_operator = (Core.Name "operator")

_AddOrSubtractRightHandSide_expression = (Core.Name "expression")

data AddOrSubtractOperator = 
  AddOrSubtractOperatorAdd  |
  AddOrSubtractOperatorSubtract 
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractOperator = (Core.Name "hydra.ext.cypher.openCypher.AddOrSubtractOperator")

_AddOrSubtractOperator_add = (Core.Name "add")

_AddOrSubtractOperator_subtract = (Core.Name "subtract")

data MultiplyDivideModuloExpression = 
  MultiplyDivideModuloExpression {
    multiplyDivideModuloExpressionLeft :: PowerOfExpression,
    multiplyDivideModuloExpressionRight :: [MultiplyDivideModuloRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloExpression = (Core.Name "hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression")

_MultiplyDivideModuloExpression_left = (Core.Name "left")

_MultiplyDivideModuloExpression_right = (Core.Name "right")

data MultiplyDivideModuloRightHandSide = 
  MultiplyDivideModuloRightHandSide {
    multiplyDivideModuloRightHandSideOperator :: MultiplyDivideModuloOperator,
    multiplyDivideModuloRightHandSideExpression :: PowerOfExpression}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloRightHandSide = (Core.Name "hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide")

_MultiplyDivideModuloRightHandSide_operator = (Core.Name "operator")

_MultiplyDivideModuloRightHandSide_expression = (Core.Name "expression")

data MultiplyDivideModuloOperator = 
  MultiplyDivideModuloOperatorMultiply  |
  MultiplyDivideModuloOperatorDivide  |
  MultiplyDivideModuloOperatorModulo 
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloOperator = (Core.Name "hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator")

_MultiplyDivideModuloOperator_multiply = (Core.Name "multiply")

_MultiplyDivideModuloOperator_divide = (Core.Name "divide")

_MultiplyDivideModuloOperator_modulo = (Core.Name "modulo")

newtype PowerOfExpression = 
  PowerOfExpression {
    unPowerOfExpression :: [UnaryAddOrSubtractExpression]}
  deriving (Eq, Ord, Read, Show)

_PowerOfExpression = (Core.Name "hydra.ext.cypher.openCypher.PowerOfExpression")

data UnaryAddOrSubtractExpression = 
  UnaryAddOrSubtractExpression {
    unaryAddOrSubtractExpressionOperator :: (Maybe AddOrSubtractOperator),
    unaryAddOrSubtractExpressionExpression :: NonArithmeticOperatorExpression}
  deriving (Eq, Ord, Read, Show)

_UnaryAddOrSubtractExpression = (Core.Name "hydra.ext.cypher.openCypher.UnaryAddOrSubtractExpression")

_UnaryAddOrSubtractExpression_operator = (Core.Name "operator")

_UnaryAddOrSubtractExpression_expression = (Core.Name "expression")

data ListOperatorExpressionOrPropertyLookup = 
  ListOperatorExpressionOrPropertyLookupList ListOperatorExpression |
  ListOperatorExpressionOrPropertyLookupProperty PropertyLookup
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpressionOrPropertyLookup = (Core.Name "hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup")

_ListOperatorExpressionOrPropertyLookup_list = (Core.Name "list")

_ListOperatorExpressionOrPropertyLookup_property = (Core.Name "property")

data NonArithmeticOperatorExpression = 
  NonArithmeticOperatorExpression {
    nonArithmeticOperatorExpressionAtom :: Atom,
    nonArithmeticOperatorExpressionListsAndLookups :: [ListOperatorExpressionOrPropertyLookup],
    nonArithmeticOperatorExpressionLabels :: (Maybe NodeLabels)}
  deriving (Eq, Ord, Read, Show)

_NonArithmeticOperatorExpression = (Core.Name "hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression")

_NonArithmeticOperatorExpression_atom = (Core.Name "atom")

_NonArithmeticOperatorExpression_listsAndLookups = (Core.Name "listsAndLookups")

_NonArithmeticOperatorExpression_labels = (Core.Name "labels")

data RangeExpression = 
  RangeExpression {
    rangeExpressionStart :: (Maybe Expression),
    rangeExpressionEnd :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_RangeExpression = (Core.Name "hydra.ext.cypher.openCypher.RangeExpression")

_RangeExpression_start = (Core.Name "start")

_RangeExpression_end = (Core.Name "end")

data ListOperatorExpression = 
  ListOperatorExpressionSingle Expression |
  ListOperatorExpressionRange RangeExpression
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpression = (Core.Name "hydra.ext.cypher.openCypher.ListOperatorExpression")

_ListOperatorExpression_single = (Core.Name "single")

_ListOperatorExpression_range = (Core.Name "range")

newtype PropertyLookup = 
  PropertyLookup {
    unPropertyLookup :: PropertyKeyName}
  deriving (Eq, Ord, Read, Show)

_PropertyLookup = (Core.Name "hydra.ext.cypher.openCypher.PropertyLookup")

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

_Atom = (Core.Name "hydra.ext.cypher.openCypher.Atom")

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

data CaseExpression = 
  CaseExpression {
    caseExpressionExpression :: (Maybe Expression),
    caseExpressionAlternatives :: [CaseAlternative],
    caseExpressionElse :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra.ext.cypher.openCypher.CaseExpression")

_CaseExpression_expression = (Core.Name "expression")

_CaseExpression_alternatives = (Core.Name "alternatives")

_CaseExpression_else = (Core.Name "else")

data CaseAlternative = 
  CaseAlternative {
    caseAlternativeCondition :: Expression,
    caseAlternativeResult :: Expression}
  deriving (Eq, Ord, Read, Show)

_CaseAlternative = (Core.Name "hydra.ext.cypher.openCypher.CaseAlternative")

_CaseAlternative_condition = (Core.Name "condition")

_CaseAlternative_result = (Core.Name "result")

data ListComprehension = 
  ListComprehension {
    listComprehensionLeft :: FilterExpression,
    listComprehensionRight :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ListComprehension = (Core.Name "hydra.ext.cypher.openCypher.ListComprehension")

_ListComprehension_left = (Core.Name "left")

_ListComprehension_right = (Core.Name "right")

data PatternComprehension = 
  PatternComprehension {
    patternComprehensionVariable :: (Maybe Variable),
    patternComprehensionPattern :: RelationshipsPattern,
    patternComprehensionWhere :: (Maybe Where),
    patternComprehensionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_PatternComprehension = (Core.Name "hydra.ext.cypher.openCypher.PatternComprehension")

_PatternComprehension_variable = (Core.Name "variable")

_PatternComprehension_pattern = (Core.Name "pattern")

_PatternComprehension_where = (Core.Name "where")

_PatternComprehension_right = (Core.Name "right")

data Quantifier = 
  Quantifier {
    quantifierOperator :: QuantifierOperator,
    quantifierExpression :: FilterExpression}
  deriving (Eq, Ord, Read, Show)

_Quantifier = (Core.Name "hydra.ext.cypher.openCypher.Quantifier")

_Quantifier_operator = (Core.Name "operator")

_Quantifier_expression = (Core.Name "expression")

data QuantifierOperator = 
  QuantifierOperatorAll  |
  QuantifierOperatorAny  |
  QuantifierOperatorNone  |
  QuantifierOperatorSingle 
  deriving (Eq, Ord, Read, Show)

_QuantifierOperator = (Core.Name "hydra.ext.cypher.openCypher.QuantifierOperator")

_QuantifierOperator_all = (Core.Name "all")

_QuantifierOperator_any = (Core.Name "any")

_QuantifierOperator_none = (Core.Name "none")

_QuantifierOperator_single = (Core.Name "single")

data FilterExpression = 
  FilterExpression {
    filterExpressionIdInColl :: IdInColl,
    filterExpressionWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_FilterExpression = (Core.Name "hydra.ext.cypher.openCypher.FilterExpression")

_FilterExpression_idInColl = (Core.Name "idInColl")

_FilterExpression_where = (Core.Name "where")

newtype PatternPredicate = 
  PatternPredicate {
    unPatternPredicate :: RelationshipsPattern}
  deriving (Eq, Ord, Read, Show)

_PatternPredicate = (Core.Name "hydra.ext.cypher.openCypher.PatternPredicate")

newtype ParenthesizedExpression = 
  ParenthesizedExpression {
    unParenthesizedExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedExpression = (Core.Name "hydra.ext.cypher.openCypher.ParenthesizedExpression")

data IdInColl = 
  IdInColl {
    idInCollVariable :: Variable,
    idInCollExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_IdInColl = (Core.Name "hydra.ext.cypher.openCypher.IdInColl")

_IdInColl_variable = (Core.Name "variable")

_IdInColl_expression = (Core.Name "expression")

data FunctionInvocation = 
  FunctionInvocation {
    functionInvocationName :: QualifiedName,
    functionInvocationDistinct :: Bool,
    functionInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionInvocation = (Core.Name "hydra.ext.cypher.openCypher.FunctionInvocation")

_FunctionInvocation_name = (Core.Name "name")

_FunctionInvocation_distinct = (Core.Name "distinct")

_FunctionInvocation_arguments = (Core.Name "arguments")

data QualifiedName = 
  QualifiedName {
    qualifiedNameNamespace :: String,
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra.ext.cypher.openCypher.QualifiedName")

_QualifiedName_namespace = (Core.Name "namespace")

_QualifiedName_local = (Core.Name "local")

data PatternWhere = 
  PatternWhere {
    patternWherePattern :: Pattern,
    patternWhereWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_PatternWhere = (Core.Name "hydra.ext.cypher.openCypher.PatternWhere")

_PatternWhere_pattern = (Core.Name "pattern")

_PatternWhere_where = (Core.Name "where")

data ExistentialSubquery = 
  ExistentialSubqueryRegular RegularQuery |
  ExistentialSubqueryPattern PatternWhere
  deriving (Eq, Ord, Read, Show)

_ExistentialSubquery = (Core.Name "hydra.ext.cypher.openCypher.ExistentialSubquery")

_ExistentialSubquery_regular = (Core.Name "regular")

_ExistentialSubquery_pattern = (Core.Name "pattern")

data ExplicitProcedureInvocation = 
  ExplicitProcedureInvocation {
    explicitProcedureInvocationName :: QualifiedName,
    explicitProcedureInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ExplicitProcedureInvocation = (Core.Name "hydra.ext.cypher.openCypher.ExplicitProcedureInvocation")

_ExplicitProcedureInvocation_name = (Core.Name "name")

_ExplicitProcedureInvocation_arguments = (Core.Name "arguments")

newtype ImplicitProcedureInvocation = 
  ImplicitProcedureInvocation {
    unImplicitProcedureInvocation :: QualifiedName}
  deriving (Eq, Ord, Read, Show)

_ImplicitProcedureInvocation = (Core.Name "hydra.ext.cypher.openCypher.ImplicitProcedureInvocation")

newtype ProcedureResultField = 
  ProcedureResultField {
    unProcedureResultField :: String}
  deriving (Eq, Ord, Read, Show)

_ProcedureResultField = (Core.Name "hydra.ext.cypher.openCypher.ProcedureResultField")

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra.ext.cypher.openCypher.Variable")

data Literal = 
  LiteralBoolean Bool |
  LiteralNull  |
  LiteralNumber NumberLiteral |
  LiteralString StringLiteral |
  LiteralList ListLiteral |
  LiteralMap MapLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.cypher.openCypher.Literal")

_Literal_boolean = (Core.Name "boolean")

_Literal_null = (Core.Name "null")

_Literal_number = (Core.Name "number")

_Literal_string = (Core.Name "string")

_Literal_list = (Core.Name "list")

_Literal_map = (Core.Name "map")

data NumberLiteral = 
  NumberLiteralDouble Double |
  NumberLiteralInteger Integer
  deriving (Eq, Ord, Read, Show)

_NumberLiteral = (Core.Name "hydra.ext.cypher.openCypher.NumberLiteral")

_NumberLiteral_double = (Core.Name "double")

_NumberLiteral_integer = (Core.Name "integer")

newtype StringLiteral = 
  StringLiteral {
    unStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra.ext.cypher.openCypher.StringLiteral")

newtype ListLiteral = 
  ListLiteral {
    unListLiteral :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ListLiteral = (Core.Name "hydra.ext.cypher.openCypher.ListLiteral")

newtype MapLiteral = 
  MapLiteral {
    unMapLiteral :: [KeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_MapLiteral = (Core.Name "hydra.ext.cypher.openCypher.MapLiteral")

data KeyValuePair = 
  KeyValuePair {
    keyValuePairKey :: PropertyKeyName,
    keyValuePairValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_KeyValuePair = (Core.Name "hydra.ext.cypher.openCypher.KeyValuePair")

_KeyValuePair_key = (Core.Name "key")

_KeyValuePair_value = (Core.Name "value")

newtype PropertyKeyName = 
  PropertyKeyName {
    unPropertyKeyName :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyName = (Core.Name "hydra.ext.cypher.openCypher.PropertyKeyName")

data Parameter = 
  ParameterSymbolic String |
  ParameterInteger Integer
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra.ext.cypher.openCypher.Parameter")

_Parameter_symbolic = (Core.Name "symbolic")

_Parameter_integer = (Core.Name "integer")
