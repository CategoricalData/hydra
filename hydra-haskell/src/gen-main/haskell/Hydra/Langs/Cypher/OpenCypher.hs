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

_Query_regular = (Core.FieldName "regular")

_Query_standalone = (Core.FieldName "standalone")

data RegularQuery = 
  RegularQuery {
    regularQueryHead :: SingleQuery,
    regularQueryRest :: [Union]}
  deriving (Eq, Ord, Read, Show)

_RegularQuery = (Core.Name "hydra/langs/cypher/openCypher.RegularQuery")

_RegularQuery_head = (Core.FieldName "head")

_RegularQuery_rest = (Core.FieldName "rest")

data Union = 
  Union {
    unionAll :: Bool,
    unionQuery :: SingleQuery}
  deriving (Eq, Ord, Read, Show)

_Union = (Core.Name "hydra/langs/cypher/openCypher.Union")

_Union_all = (Core.FieldName "all")

_Union_query = (Core.FieldName "query")

data SingleQuery = 
  SingleQuerySinglePart SinglePartQuery |
  SingleQueryMultiPart MultiPartQuery
  deriving (Eq, Ord, Read, Show)

_SingleQuery = (Core.Name "hydra/langs/cypher/openCypher.SingleQuery")

_SingleQuery_singlePart = (Core.FieldName "singlePart")

_SingleQuery_multiPart = (Core.FieldName "multiPart")

data SinglePartQuery = 
  SinglePartQuery {
    singlePartQueryReading :: [ReadingClause],
    singlePartQueryUpdating :: [UpdatingClause],
    singlePartQueryReturn :: (Maybe Return)}
  deriving (Eq, Ord, Read, Show)

_SinglePartQuery = (Core.Name "hydra/langs/cypher/openCypher.SinglePartQuery")

_SinglePartQuery_reading = (Core.FieldName "reading")

_SinglePartQuery_updating = (Core.FieldName "updating")

_SinglePartQuery_return = (Core.FieldName "return")

data WithClause = 
  WithClause {
    withClauseReading :: [ReadingClause],
    withClauseUpdating :: [UpdatingClause],
    withClauseWith :: With}
  deriving (Eq, Ord, Read, Show)

_WithClause = (Core.Name "hydra/langs/cypher/openCypher.WithClause")

_WithClause_reading = (Core.FieldName "reading")

_WithClause_updating = (Core.FieldName "updating")

_WithClause_with = (Core.FieldName "with")

data MultiPartQuery = 
  MultiPartQuery {
    multiPartQueryWith :: [WithClause],
    multiPartQueryBody :: SinglePartQuery}
  deriving (Eq, Ord, Read, Show)

_MultiPartQuery = (Core.Name "hydra/langs/cypher/openCypher.MultiPartQuery")

_MultiPartQuery_with = (Core.FieldName "with")

_MultiPartQuery_body = (Core.FieldName "body")

data UpdatingClause = 
  UpdatingClauseCreate Create |
  UpdatingClauseMerge Merge |
  UpdatingClauseDelete Delete |
  UpdatingClauseSet Set_ |
  UpdatingClauseRemove Remove
  deriving (Eq, Ord, Read, Show)

_UpdatingClause = (Core.Name "hydra/langs/cypher/openCypher.UpdatingClause")

_UpdatingClause_create = (Core.FieldName "create")

_UpdatingClause_merge = (Core.FieldName "merge")

_UpdatingClause_delete = (Core.FieldName "delete")

_UpdatingClause_set = (Core.FieldName "set")

_UpdatingClause_remove = (Core.FieldName "remove")

data ReadingClause = 
  ReadingClauseMatch Match |
  ReadingClauseUnwind Unwind |
  ReadingClauseInQueryCall InQueryCall
  deriving (Eq, Ord, Read, Show)

_ReadingClause = (Core.Name "hydra/langs/cypher/openCypher.ReadingClause")

_ReadingClause_match = (Core.FieldName "match")

_ReadingClause_unwind = (Core.FieldName "unwind")

_ReadingClause_inQueryCall = (Core.FieldName "inQueryCall")

data Match = 
  Match {
    matchOptional :: Bool,
    matchPattern :: Pattern,
    matchWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_Match = (Core.Name "hydra/langs/cypher/openCypher.Match")

_Match_optional = (Core.FieldName "optional")

_Match_pattern = (Core.FieldName "pattern")

_Match_where = (Core.FieldName "where")

data Unwind = 
  Unwind {
    unwindExpression :: Expression,
    unwindVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_Unwind = (Core.Name "hydra/langs/cypher/openCypher.Unwind")

_Unwind_expression = (Core.FieldName "expression")

_Unwind_variable = (Core.FieldName "variable")

data Merge = 
  Merge {
    mergePatternPart :: PatternPart,
    mergeActions :: [MergeAction]}
  deriving (Eq, Ord, Read, Show)

_Merge = (Core.Name "hydra/langs/cypher/openCypher.Merge")

_Merge_patternPart = (Core.FieldName "patternPart")

_Merge_actions = (Core.FieldName "actions")

data MatchOrCreate = 
  MatchOrCreateMatch  |
  MatchOrCreateCreate 
  deriving (Eq, Ord, Read, Show)

_MatchOrCreate = (Core.Name "hydra/langs/cypher/openCypher.MatchOrCreate")

_MatchOrCreate_match = (Core.FieldName "match")

_MatchOrCreate_create = (Core.FieldName "create")

data MergeAction = 
  MergeAction {
    mergeActionAction :: MatchOrCreate,
    mergeActionSet :: Set_}
  deriving (Eq, Ord, Read, Show)

_MergeAction = (Core.Name "hydra/langs/cypher/openCypher.MergeAction")

_MergeAction_action = (Core.FieldName "action")

_MergeAction_set = (Core.FieldName "set")

newtype Create = 
  Create {
    unCreate :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Create = (Core.Name "hydra/langs/cypher/openCypher.Create")

newtype Set_ = 
  Set_ {
    unSet :: [SetItem]}
  deriving (Eq, Ord, Read, Show)

_Set = (Core.Name "hydra/langs/cypher/openCypher.Set")

data SetItem = 
  SetItemProperty PropertyEquals |
  SetItemVariableEqual VariableEquals |
  SetItemVariablePlusEqual VariablePlusEquals |
  SetItemVariableLabels VariableAndNodeLabels
  deriving (Eq, Ord, Read, Show)

_SetItem = (Core.Name "hydra/langs/cypher/openCypher.SetItem")

_SetItem_property = (Core.FieldName "property")

_SetItem_variableEqual = (Core.FieldName "variableEqual")

_SetItem_variablePlusEqual = (Core.FieldName "variablePlusEqual")

_SetItem_variableLabels = (Core.FieldName "variableLabels")

data PropertyEquals = 
  PropertyEquals {
    propertyEqualsLhs :: PropertyExpression,
    propertyEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_PropertyEquals = (Core.Name "hydra/langs/cypher/openCypher.PropertyEquals")

_PropertyEquals_lhs = (Core.FieldName "lhs")

_PropertyEquals_rhs = (Core.FieldName "rhs")

data VariableEquals = 
  VariableEquals {
    variableEqualsLhs :: Variable,
    variableEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariableEquals = (Core.Name "hydra/langs/cypher/openCypher.VariableEquals")

_VariableEquals_lhs = (Core.FieldName "lhs")

_VariableEquals_rhs = (Core.FieldName "rhs")

data VariablePlusEquals = 
  VariablePlusEquals {
    variablePlusEqualsLhs :: Variable,
    variablePlusEqualsRhs :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariablePlusEquals = (Core.Name "hydra/langs/cypher/openCypher.VariablePlusEquals")

_VariablePlusEquals_lhs = (Core.FieldName "lhs")

_VariablePlusEquals_rhs = (Core.FieldName "rhs")

data VariableAndNodeLabels = 
  VariableAndNodeLabels {
    variableAndNodeLabelsVariable :: Variable,
    variableAndNodeLabelsLabels :: NodeLabels}
  deriving (Eq, Ord, Read, Show)

_VariableAndNodeLabels = (Core.Name "hydra/langs/cypher/openCypher.VariableAndNodeLabels")

_VariableAndNodeLabels_variable = (Core.FieldName "variable")

_VariableAndNodeLabels_labels = (Core.FieldName "labels")

data Delete = 
  Delete {
    deleteDetach :: Bool,
    deleteExpressions :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_Delete = (Core.Name "hydra/langs/cypher/openCypher.Delete")

_Delete_detach = (Core.FieldName "detach")

_Delete_expressions = (Core.FieldName "expressions")

newtype Remove = 
  Remove {
    unRemove :: [RemoveItem]}
  deriving (Eq, Ord, Read, Show)

_Remove = (Core.Name "hydra/langs/cypher/openCypher.Remove")

data RemoveItem = 
  RemoveItemVariableLabels VariableAndNodeLabels |
  RemoveItemProperty PropertyExpression
  deriving (Eq, Ord, Read, Show)

_RemoveItem = (Core.Name "hydra/langs/cypher/openCypher.RemoveItem")

_RemoveItem_variableLabels = (Core.FieldName "variableLabels")

_RemoveItem_property = (Core.FieldName "property")

data InQueryCall = 
  InQueryCall {
    inQueryCallCall :: ExplicitProcedureInvocation,
    inQueryCallYieldItems :: (Maybe YieldItems)}
  deriving (Eq, Ord, Read, Show)

_InQueryCall = (Core.Name "hydra/langs/cypher/openCypher.InQueryCall")

_InQueryCall_call = (Core.FieldName "call")

_InQueryCall_yieldItems = (Core.FieldName "yieldItems")

data ProcedureInvocation = 
  ProcedureInvocationExplicit ExplicitProcedureInvocation |
  ProcedureInvocationImplicit ImplicitProcedureInvocation
  deriving (Eq, Ord, Read, Show)

_ProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ProcedureInvocation")

_ProcedureInvocation_explicit = (Core.FieldName "explicit")

_ProcedureInvocation_implicit = (Core.FieldName "implicit")

data StarOrYieldItems = 
  StarOrYieldItemsStar  |
  StarOrYieldItemsItems YieldItems
  deriving (Eq, Ord, Read, Show)

_StarOrYieldItems = (Core.Name "hydra/langs/cypher/openCypher.StarOrYieldItems")

_StarOrYieldItems_star = (Core.FieldName "star")

_StarOrYieldItems_items = (Core.FieldName "items")

data StandaloneCall = 
  StandaloneCall {
    standaloneCallCall :: ProcedureInvocation,
    standaloneCallYieldItems :: (Maybe StarOrYieldItems)}
  deriving (Eq, Ord, Read, Show)

_StandaloneCall = (Core.Name "hydra/langs/cypher/openCypher.StandaloneCall")

_StandaloneCall_call = (Core.FieldName "call")

_StandaloneCall_yieldItems = (Core.FieldName "yieldItems")

data YieldItems = 
  YieldItems {
    yieldItemsItems :: [YieldItem],
    yieldItemsWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_YieldItems = (Core.Name "hydra/langs/cypher/openCypher.YieldItems")

_YieldItems_items = (Core.FieldName "items")

_YieldItems_where = (Core.FieldName "where")

data YieldItem = 
  YieldItem {
    yieldItemField :: (Maybe ProcedureResultField),
    yieldItemVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_YieldItem = (Core.Name "hydra/langs/cypher/openCypher.YieldItem")

_YieldItem_field = (Core.FieldName "field")

_YieldItem_variable = (Core.FieldName "variable")

data With = 
  With {
    withProjection :: ProjectionBody,
    withWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_With = (Core.Name "hydra/langs/cypher/openCypher.With")

_With_projection = (Core.FieldName "projection")

_With_where = (Core.FieldName "where")

newtype Return = 
  Return {
    unReturn :: ProjectionBody}
  deriving (Eq, Ord, Read, Show)

_Return = (Core.Name "hydra/langs/cypher/openCypher.Return")

data ProjectionBody = 
  ProjectionBody {
    projectionBodyDistinct :: Bool,
    projectionBodyProjectionItems :: ProjectionItems,
    projectionBodyOrder :: (Maybe Order),
    projectionBodySkip :: (Maybe Skip),
    projectionBodyLimit :: (Maybe Limit)}
  deriving (Eq, Ord, Read, Show)

_ProjectionBody = (Core.Name "hydra/langs/cypher/openCypher.ProjectionBody")

_ProjectionBody_distinct = (Core.FieldName "distinct")

_ProjectionBody_projectionItems = (Core.FieldName "projectionItems")

_ProjectionBody_order = (Core.FieldName "order")

_ProjectionBody_skip = (Core.FieldName "skip")

_ProjectionBody_limit = (Core.FieldName "limit")

data ProjectionItems = 
  ProjectionItems {
    projectionItemsStar :: Bool,
    projectionItemsExplicit :: [ProjectionItem]}
  deriving (Eq, Ord, Read, Show)

_ProjectionItems = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItems")

_ProjectionItems_star = (Core.FieldName "star")

_ProjectionItems_explicit = (Core.FieldName "explicit")

data ProjectionItem = 
  ProjectionItem {
    projectionItemExpression :: Expression,
    projectionItemVariable :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_ProjectionItem = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItem")

_ProjectionItem_expression = (Core.FieldName "expression")

_ProjectionItem_variable = (Core.FieldName "variable")

newtype Order = 
  Order {
    unOrder :: [SortItem]}
  deriving (Eq, Ord, Read, Show)

_Order = (Core.Name "hydra/langs/cypher/openCypher.Order")

newtype Skip = 
  Skip {
    unSkip :: Expression}
  deriving (Eq, Ord, Read, Show)

_Skip = (Core.Name "hydra/langs/cypher/openCypher.Skip")

newtype Limit = 
  Limit {
    unLimit :: Expression}
  deriving (Eq, Ord, Read, Show)

_Limit = (Core.Name "hydra/langs/cypher/openCypher.Limit")

data SortOrder = 
  SortOrderAscending  |
  SortOrderDescending 
  deriving (Eq, Ord, Read, Show)

_SortOrder = (Core.Name "hydra/langs/cypher/openCypher.SortOrder")

_SortOrder_ascending = (Core.FieldName "ascending")

_SortOrder_descending = (Core.FieldName "descending")

data SortItem = 
  SortItem {
    sortItemExpression :: Expression,
    sortItemOrder :: (Maybe SortOrder)}
  deriving (Eq, Ord, Read, Show)

_SortItem = (Core.Name "hydra/langs/cypher/openCypher.SortItem")

_SortItem_expression = (Core.FieldName "expression")

_SortItem_order = (Core.FieldName "order")

newtype Where = 
  Where {
    unWhere :: Expression}
  deriving (Eq, Ord, Read, Show)

_Where = (Core.Name "hydra/langs/cypher/openCypher.Where")

newtype Pattern = 
  Pattern {
    unPattern :: [PatternPart]}
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/langs/cypher/openCypher.Pattern")

data PatternPart = 
  PatternPart {
    patternPartVariable :: (Maybe Variable),
    patternPartPattern :: AnonymousPatternPart}
  deriving (Eq, Ord, Read, Show)

_PatternPart = (Core.Name "hydra/langs/cypher/openCypher.PatternPart")

_PatternPart_variable = (Core.FieldName "variable")

_PatternPart_pattern = (Core.FieldName "pattern")

newtype AnonymousPatternPart = 
  AnonymousPatternPart {
    unAnonymousPatternPart :: PatternElement}
  deriving (Eq, Ord, Read, Show)

_AnonymousPatternPart = (Core.Name "hydra/langs/cypher/openCypher.AnonymousPatternPart")

data NodePatternChain = 
  NodePatternChain {
    nodePatternChainNodePattern :: NodePattern,
    nodePatternChainChain :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_NodePatternChain = (Core.Name "hydra/langs/cypher/openCypher.NodePatternChain")

_NodePatternChain_nodePattern = (Core.FieldName "nodePattern")

_NodePatternChain_chain = (Core.FieldName "chain")

data PatternElement = 
  PatternElementChained NodePatternChain |
  PatternElementParenthesized PatternElement
  deriving (Eq, Ord, Read, Show)

_PatternElement = (Core.Name "hydra/langs/cypher/openCypher.PatternElement")

_PatternElement_chained = (Core.FieldName "chained")

_PatternElement_parenthesized = (Core.FieldName "parenthesized")

data RelationshipsPattern = 
  RelationshipsPattern {
    relationshipsPatternNodePattern :: NodePattern,
    relationshipsPatternChain :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_RelationshipsPattern = (Core.Name "hydra/langs/cypher/openCypher.RelationshipsPattern")

_RelationshipsPattern_nodePattern = (Core.FieldName "nodePattern")

_RelationshipsPattern_chain = (Core.FieldName "chain")

data NodePattern = 
  NodePattern {
    nodePatternVariable :: (Maybe Variable),
    nodePatternLabels :: (Maybe NodeLabels),
    nodePatternProperties :: (Maybe Properties)}
  deriving (Eq, Ord, Read, Show)

_NodePattern = (Core.Name "hydra/langs/cypher/openCypher.NodePattern")

_NodePattern_variable = (Core.FieldName "variable")

_NodePattern_labels = (Core.FieldName "labels")

_NodePattern_properties = (Core.FieldName "properties")

data PatternElementChain = 
  PatternElementChain {
    patternElementChainRelationship :: RelationshipPattern,
    patternElementChainNode :: NodePattern}
  deriving (Eq, Ord, Read, Show)

_PatternElementChain = (Core.Name "hydra/langs/cypher/openCypher.PatternElementChain")

_PatternElementChain_relationship = (Core.FieldName "relationship")

_PatternElementChain_node = (Core.FieldName "node")

data RelationshipPattern = 
  RelationshipPattern {
    relationshipPatternLeftArrow :: Bool,
    relationshipPatternDetail :: (Maybe RelationshipDetail),
    relationshipPatternRightArrow :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipPattern = (Core.Name "hydra/langs/cypher/openCypher.RelationshipPattern")

_RelationshipPattern_leftArrow = (Core.FieldName "leftArrow")

_RelationshipPattern_detail = (Core.FieldName "detail")

_RelationshipPattern_rightArrow = (Core.FieldName "rightArrow")

data RelationshipDetail = 
  RelationshipDetail {
    relationshipDetailVariable :: (Maybe Variable),
    relationshipDetailTypes :: (Maybe RelationshipTypes),
    relationshipDetailRange :: (Maybe RangeLiteral),
    relationshipDetailProperties :: (Maybe Properties)}
  deriving (Eq, Ord, Read, Show)

_RelationshipDetail = (Core.Name "hydra/langs/cypher/openCypher.RelationshipDetail")

_RelationshipDetail_variable = (Core.FieldName "variable")

_RelationshipDetail_types = (Core.FieldName "types")

_RelationshipDetail_range = (Core.FieldName "range")

_RelationshipDetail_properties = (Core.FieldName "properties")

data Properties = 
  PropertiesMap MapLiteral |
  PropertiesParameter Parameter
  deriving (Eq, Ord, Read, Show)

_Properties = (Core.Name "hydra/langs/cypher/openCypher.Properties")

_Properties_map = (Core.FieldName "map")

_Properties_parameter = (Core.FieldName "parameter")

newtype RelationshipTypes = 
  RelationshipTypes {
    unRelationshipTypes :: [RelTypeName]}
  deriving (Eq, Ord, Read, Show)

_RelationshipTypes = (Core.Name "hydra/langs/cypher/openCypher.RelationshipTypes")

newtype NodeLabels = 
  NodeLabels {
    unNodeLabels :: [NodeLabel]}
  deriving (Eq, Ord, Read, Show)

_NodeLabels = (Core.Name "hydra/langs/cypher/openCypher.NodeLabels")

newtype NodeLabel = 
  NodeLabel {
    unNodeLabel :: String}
  deriving (Eq, Ord, Read, Show)

_NodeLabel = (Core.Name "hydra/langs/cypher/openCypher.NodeLabel")

data RangeLiteral = 
  RangeLiteral {
    rangeLiteralStart :: (Maybe Integer),
    rangeLiteralEnd :: (Maybe Integer)}
  deriving (Eq, Ord, Read, Show)

_RangeLiteral = (Core.Name "hydra/langs/cypher/openCypher.RangeLiteral")

_RangeLiteral_start = (Core.FieldName "start")

_RangeLiteral_end = (Core.FieldName "end")

newtype RelTypeName = 
  RelTypeName {
    unRelTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_RelTypeName = (Core.Name "hydra/langs/cypher/openCypher.RelTypeName")

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionAtom :: Atom,
    propertyExpressionLookups :: [PropertyLookup]}
  deriving (Eq, Ord, Read, Show)

_PropertyExpression = (Core.Name "hydra/langs/cypher/openCypher.PropertyExpression")

_PropertyExpression_atom = (Core.FieldName "atom")

_PropertyExpression_lookups = (Core.FieldName "lookups")

newtype Expression = 
  Expression {
    unExpression :: OrExpression}
  deriving (Eq, Ord, Read, Show)

_Expression = (Core.Name "hydra/langs/cypher/openCypher.Expression")

newtype OrExpression = 
  OrExpression {
    unOrExpression :: [XorExpression]}
  deriving (Eq, Ord, Read, Show)

_OrExpression = (Core.Name "hydra/langs/cypher/openCypher.OrExpression")

newtype XorExpression = 
  XorExpression {
    unXorExpression :: [AndExpression]}
  deriving (Eq, Ord, Read, Show)

_XorExpression = (Core.Name "hydra/langs/cypher/openCypher.XorExpression")

newtype AndExpression = 
  AndExpression {
    unAndExpression :: [NotExpression]}
  deriving (Eq, Ord, Read, Show)

_AndExpression = (Core.Name "hydra/langs/cypher/openCypher.AndExpression")

data NotExpression = 
  NotExpression {
    notExpressionNot :: Bool,
    notExpressionExpression :: ComparisonExpression}
  deriving (Eq, Ord, Read, Show)

_NotExpression = (Core.Name "hydra/langs/cypher/openCypher.NotExpression")

_NotExpression_not = (Core.FieldName "not")

_NotExpression_expression = (Core.FieldName "expression")

data ComparisonExpression = 
  ComparisonExpression {
    comparisonExpressionLeft :: StringListNullPredicateExpression,
    comparisonExpressionRight :: [PartialComparisonExpression]}
  deriving (Eq, Ord, Read, Show)

_ComparisonExpression = (Core.Name "hydra/langs/cypher/openCypher.ComparisonExpression")

_ComparisonExpression_left = (Core.FieldName "left")

_ComparisonExpression_right = (Core.FieldName "right")

data ComparisonOperator = 
  ComparisonOperatorEq  |
  ComparisonOperatorNeq  |
  ComparisonOperatorLt  |
  ComparisonOperatorGt  |
  ComparisonOperatorLte  |
  ComparisonOperatorGte 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra/langs/cypher/openCypher.ComparisonOperator")

_ComparisonOperator_eq = (Core.FieldName "eq")

_ComparisonOperator_neq = (Core.FieldName "neq")

_ComparisonOperator_lt = (Core.FieldName "lt")

_ComparisonOperator_gt = (Core.FieldName "gt")

_ComparisonOperator_lte = (Core.FieldName "lte")

_ComparisonOperator_gte = (Core.FieldName "gte")

data PartialComparisonExpression = 
  PartialComparisonExpression {
    partialComparisonExpressionOperator :: ComparisonOperator,
    partialComparisonExpressionRight :: StringListNullPredicateExpression}
  deriving (Eq, Ord, Read, Show)

_PartialComparisonExpression = (Core.Name "hydra/langs/cypher/openCypher.PartialComparisonExpression")

_PartialComparisonExpression_operator = (Core.FieldName "operator")

_PartialComparisonExpression_right = (Core.FieldName "right")

data StringListNullPredicateExpression = 
  StringListNullPredicateExpression {
    stringListNullPredicateExpressionLeft :: AddOrSubtractExpression,
    stringListNullPredicateExpressionRight :: [StringListNullPredicateRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateExpression")

_StringListNullPredicateExpression_left = (Core.FieldName "left")

_StringListNullPredicateExpression_right = (Core.FieldName "right")

data StringListNullPredicateRightHandSide = 
  StringListNullPredicateRightHandSideString StringPredicateExpression |
  StringListNullPredicateRightHandSideList ListPredicateExpression |
  StringListNullPredicateRightHandSideNull NullPredicateExpression
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateRightHandSide = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateRightHandSide")

_StringListNullPredicateRightHandSide_string = (Core.FieldName "string")

_StringListNullPredicateRightHandSide_list = (Core.FieldName "list")

_StringListNullPredicateRightHandSide_null = (Core.FieldName "null")

data StringPredicateExpression = 
  StringPredicateExpression {
    stringPredicateExpressionOperator :: StringPredicateOperator,
    stringPredicateExpressionExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_StringPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateExpression")

_StringPredicateExpression_operator = (Core.FieldName "operator")

_StringPredicateExpression_expression = (Core.FieldName "expression")

data StringPredicateOperator = 
  StringPredicateOperatorStartsWith  |
  StringPredicateOperatorEndsWith  |
  StringPredicateOperatorContains 
  deriving (Eq, Ord, Read, Show)

_StringPredicateOperator = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateOperator")

_StringPredicateOperator_startsWith = (Core.FieldName "startsWith")

_StringPredicateOperator_endsWith = (Core.FieldName "endsWith")

_StringPredicateOperator_contains = (Core.FieldName "contains")

newtype ListPredicateExpression = 
  ListPredicateExpression {
    unListPredicateExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_ListPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.ListPredicateExpression")

newtype NullPredicateExpression = 
  NullPredicateExpression {
    unNullPredicateExpression :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.NullPredicateExpression")

data AddOrSubtractExpression = 
  AddOrSubtractExpression {
    addOrSubtractExpressionLeft :: MultiplyDivideModuloExpression,
    addOrSubtractExpressionRight :: [AddOrSubtractRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractExpression = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractExpression")

_AddOrSubtractExpression_left = (Core.FieldName "left")

_AddOrSubtractExpression_right = (Core.FieldName "right")

data AddOrSubtractRightHandSide = 
  AddOrSubtractRightHandSide {
    addOrSubtractRightHandSideOperator :: AddOrSubtractOperator,
    addOrSubtractRightHandSideExpression :: MultiplyDivideModuloExpression}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractRightHandSide = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractRightHandSide")

_AddOrSubtractRightHandSide_operator = (Core.FieldName "operator")

_AddOrSubtractRightHandSide_expression = (Core.FieldName "expression")

data AddOrSubtractOperator = 
  AddOrSubtractOperatorAdd  |
  AddOrSubtractOperatorSubtract 
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractOperator = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractOperator")

_AddOrSubtractOperator_add = (Core.FieldName "add")

_AddOrSubtractOperator_subtract = (Core.FieldName "subtract")

data MultiplyDivideModuloExpression = 
  MultiplyDivideModuloExpression {
    multiplyDivideModuloExpressionLeft :: PowerOfExpression,
    multiplyDivideModuloExpressionRight :: [MultiplyDivideModuloRightHandSide]}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloExpression = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloExpression")

_MultiplyDivideModuloExpression_left = (Core.FieldName "left")

_MultiplyDivideModuloExpression_right = (Core.FieldName "right")

data MultiplyDivideModuloRightHandSide = 
  MultiplyDivideModuloRightHandSide {
    multiplyDivideModuloRightHandSideOperator :: MultiplyDivideModuloOperator,
    multiplyDivideModuloRightHandSideExpression :: PowerOfExpression}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloRightHandSide = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloRightHandSide")

_MultiplyDivideModuloRightHandSide_operator = (Core.FieldName "operator")

_MultiplyDivideModuloRightHandSide_expression = (Core.FieldName "expression")

data MultiplyDivideModuloOperator = 
  MultiplyDivideModuloOperatorMultiply  |
  MultiplyDivideModuloOperatorDivide  |
  MultiplyDivideModuloOperatorModulo 
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloOperator = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloOperator")

_MultiplyDivideModuloOperator_multiply = (Core.FieldName "multiply")

_MultiplyDivideModuloOperator_divide = (Core.FieldName "divide")

_MultiplyDivideModuloOperator_modulo = (Core.FieldName "modulo")

newtype PowerOfExpression = 
  PowerOfExpression {
    unPowerOfExpression :: [UnaryAddOrSubtractExpression]}
  deriving (Eq, Ord, Read, Show)

_PowerOfExpression = (Core.Name "hydra/langs/cypher/openCypher.PowerOfExpression")

data UnaryAddOrSubtractExpression = 
  UnaryAddOrSubtractExpression {
    unaryAddOrSubtractExpressionOperator :: (Maybe AddOrSubtractOperator),
    unaryAddOrSubtractExpressionExpression :: NonArithmeticOperatorExpression}
  deriving (Eq, Ord, Read, Show)

_UnaryAddOrSubtractExpression = (Core.Name "hydra/langs/cypher/openCypher.UnaryAddOrSubtractExpression")

_UnaryAddOrSubtractExpression_operator = (Core.FieldName "operator")

_UnaryAddOrSubtractExpression_expression = (Core.FieldName "expression")

data ListOperatorExpressionOrPropertyLookup = 
  ListOperatorExpressionOrPropertyLookupList ListOperatorExpression |
  ListOperatorExpressionOrPropertyLookupProperty PropertyLookup
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpressionOrPropertyLookup = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpressionOrPropertyLookup")

_ListOperatorExpressionOrPropertyLookup_list = (Core.FieldName "list")

_ListOperatorExpressionOrPropertyLookup_property = (Core.FieldName "property")

data NonArithmeticOperatorExpression = 
  NonArithmeticOperatorExpression {
    nonArithmeticOperatorExpressionAtom :: Atom,
    nonArithmeticOperatorExpressionListsAndLookups :: [ListOperatorExpressionOrPropertyLookup],
    nonArithmeticOperatorExpressionLabels :: (Maybe NodeLabels)}
  deriving (Eq, Ord, Read, Show)

_NonArithmeticOperatorExpression = (Core.Name "hydra/langs/cypher/openCypher.NonArithmeticOperatorExpression")

_NonArithmeticOperatorExpression_atom = (Core.FieldName "atom")

_NonArithmeticOperatorExpression_listsAndLookups = (Core.FieldName "listsAndLookups")

_NonArithmeticOperatorExpression_labels = (Core.FieldName "labels")

data RangeExpression = 
  RangeExpression {
    rangeExpressionStart :: (Maybe Expression),
    rangeExpressionEnd :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_RangeExpression = (Core.Name "hydra/langs/cypher/openCypher.RangeExpression")

_RangeExpression_start = (Core.FieldName "start")

_RangeExpression_end = (Core.FieldName "end")

data ListOperatorExpression = 
  ListOperatorExpressionSingle Expression |
  ListOperatorExpressionRange RangeExpression
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpression = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpression")

_ListOperatorExpression_single = (Core.FieldName "single")

_ListOperatorExpression_range = (Core.FieldName "range")

newtype PropertyLookup = 
  PropertyLookup {
    unPropertyLookup :: PropertyKeyName}
  deriving (Eq, Ord, Read, Show)

_PropertyLookup = (Core.Name "hydra/langs/cypher/openCypher.PropertyLookup")

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

_Atom_literal = (Core.FieldName "literal")

_Atom_parameter = (Core.FieldName "parameter")

_Atom_case = (Core.FieldName "case")

_Atom_countStar = (Core.FieldName "countStar")

_Atom_listComprehension = (Core.FieldName "listComprehension")

_Atom_patternComprehension = (Core.FieldName "patternComprehension")

_Atom_quantifier = (Core.FieldName "quantifier")

_Atom_patternPredicate = (Core.FieldName "patternPredicate")

_Atom_parenthesized = (Core.FieldName "parenthesized")

_Atom_functionInvocation = (Core.FieldName "functionInvocation")

_Atom_existentialSubquery = (Core.FieldName "existentialSubquery")

_Atom_variable = (Core.FieldName "variable")

data CaseExpression = 
  CaseExpression {
    caseExpressionExpression :: (Maybe Expression),
    caseExpressionAlternatives :: [CaseAlternative],
    caseExpressionElse :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra/langs/cypher/openCypher.CaseExpression")

_CaseExpression_expression = (Core.FieldName "expression")

_CaseExpression_alternatives = (Core.FieldName "alternatives")

_CaseExpression_else = (Core.FieldName "else")

data CaseAlternative = 
  CaseAlternative {
    caseAlternativeCondition :: Expression,
    caseAlternativeResult :: Expression}
  deriving (Eq, Ord, Read, Show)

_CaseAlternative = (Core.Name "hydra/langs/cypher/openCypher.CaseAlternative")

_CaseAlternative_condition = (Core.FieldName "condition")

_CaseAlternative_result = (Core.FieldName "result")

data ListComprehension = 
  ListComprehension {
    listComprehensionLeft :: FilterExpression,
    listComprehensionRight :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ListComprehension = (Core.Name "hydra/langs/cypher/openCypher.ListComprehension")

_ListComprehension_left = (Core.FieldName "left")

_ListComprehension_right = (Core.FieldName "right")

data PatternComprehension = 
  PatternComprehension {
    patternComprehensionVariable :: (Maybe Variable),
    patternComprehensionPattern :: RelationshipsPattern,
    patternComprehensionWhere :: (Maybe Where),
    patternComprehensionRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_PatternComprehension = (Core.Name "hydra/langs/cypher/openCypher.PatternComprehension")

_PatternComprehension_variable = (Core.FieldName "variable")

_PatternComprehension_pattern = (Core.FieldName "pattern")

_PatternComprehension_where = (Core.FieldName "where")

_PatternComprehension_right = (Core.FieldName "right")

data Quantifier = 
  Quantifier {
    quantifierOperator :: QuantifierOperator,
    quantifierExpression :: FilterExpression}
  deriving (Eq, Ord, Read, Show)

_Quantifier = (Core.Name "hydra/langs/cypher/openCypher.Quantifier")

_Quantifier_operator = (Core.FieldName "operator")

_Quantifier_expression = (Core.FieldName "expression")

data QuantifierOperator = 
  QuantifierOperatorAll  |
  QuantifierOperatorAny  |
  QuantifierOperatorNone  |
  QuantifierOperatorSingle 
  deriving (Eq, Ord, Read, Show)

_QuantifierOperator = (Core.Name "hydra/langs/cypher/openCypher.QuantifierOperator")

_QuantifierOperator_all = (Core.FieldName "all")

_QuantifierOperator_any = (Core.FieldName "any")

_QuantifierOperator_none = (Core.FieldName "none")

_QuantifierOperator_single = (Core.FieldName "single")

data FilterExpression = 
  FilterExpression {
    filterExpressionIdInColl :: IdInColl,
    filterExpressionWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_FilterExpression = (Core.Name "hydra/langs/cypher/openCypher.FilterExpression")

_FilterExpression_idInColl = (Core.FieldName "idInColl")

_FilterExpression_where = (Core.FieldName "where")

newtype PatternPredicate = 
  PatternPredicate {
    unPatternPredicate :: RelationshipsPattern}
  deriving (Eq, Ord, Read, Show)

_PatternPredicate = (Core.Name "hydra/langs/cypher/openCypher.PatternPredicate")

newtype ParenthesizedExpression = 
  ParenthesizedExpression {
    unParenthesizedExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_ParenthesizedExpression = (Core.Name "hydra/langs/cypher/openCypher.ParenthesizedExpression")

data IdInColl = 
  IdInColl {
    idInCollVariable :: Variable,
    idInCollExpression :: Expression}
  deriving (Eq, Ord, Read, Show)

_IdInColl = (Core.Name "hydra/langs/cypher/openCypher.IdInColl")

_IdInColl_variable = (Core.FieldName "variable")

_IdInColl_expression = (Core.FieldName "expression")

data FunctionInvocation = 
  FunctionInvocation {
    functionInvocationName :: QualifiedName,
    functionInvocationDistinct :: Bool,
    functionInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionInvocation = (Core.Name "hydra/langs/cypher/openCypher.FunctionInvocation")

_FunctionInvocation_name = (Core.FieldName "name")

_FunctionInvocation_distinct = (Core.FieldName "distinct")

_FunctionInvocation_arguments = (Core.FieldName "arguments")

data QualifiedName = 
  QualifiedName {
    qualifiedNameNamespace :: String,
    qualifiedNameLocal :: String}
  deriving (Eq, Ord, Read, Show)

_QualifiedName = (Core.Name "hydra/langs/cypher/openCypher.QualifiedName")

_QualifiedName_namespace = (Core.FieldName "namespace")

_QualifiedName_local = (Core.FieldName "local")

data PatternWhere = 
  PatternWhere {
    patternWherePattern :: Pattern,
    patternWhereWhere :: (Maybe Where)}
  deriving (Eq, Ord, Read, Show)

_PatternWhere = (Core.Name "hydra/langs/cypher/openCypher.PatternWhere")

_PatternWhere_pattern = (Core.FieldName "pattern")

_PatternWhere_where = (Core.FieldName "where")

data ExistentialSubquery = 
  ExistentialSubqueryRegular RegularQuery |
  ExistentialSubqueryPattern PatternWhere
  deriving (Eq, Ord, Read, Show)

_ExistentialSubquery = (Core.Name "hydra/langs/cypher/openCypher.ExistentialSubquery")

_ExistentialSubquery_regular = (Core.FieldName "regular")

_ExistentialSubquery_pattern = (Core.FieldName "pattern")

data ExplicitProcedureInvocation = 
  ExplicitProcedureInvocation {
    explicitProcedureInvocationName :: QualifiedName,
    explicitProcedureInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ExplicitProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ExplicitProcedureInvocation")

_ExplicitProcedureInvocation_name = (Core.FieldName "name")

_ExplicitProcedureInvocation_arguments = (Core.FieldName "arguments")

newtype ImplicitProcedureInvocation = 
  ImplicitProcedureInvocation {
    unImplicitProcedureInvocation :: QualifiedName}
  deriving (Eq, Ord, Read, Show)

_ImplicitProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ImplicitProcedureInvocation")

newtype ProcedureResultField = 
  ProcedureResultField {
    unProcedureResultField :: String}
  deriving (Eq, Ord, Read, Show)

_ProcedureResultField = (Core.Name "hydra/langs/cypher/openCypher.ProcedureResultField")

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/cypher/openCypher.Variable")

data Literal = 
  LiteralBoolean Bool |
  LiteralNull  |
  LiteralNumber NumberLiteral |
  LiteralString StringLiteral |
  LiteralList ListLiteral |
  LiteralMap MapLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/cypher/openCypher.Literal")

_Literal_boolean = (Core.FieldName "boolean")

_Literal_null = (Core.FieldName "null")

_Literal_number = (Core.FieldName "number")

_Literal_string = (Core.FieldName "string")

_Literal_list = (Core.FieldName "list")

_Literal_map = (Core.FieldName "map")

data NumberLiteral = 
  NumberLiteralDouble Double |
  NumberLiteralInteger Integer
  deriving (Eq, Ord, Read, Show)

_NumberLiteral = (Core.Name "hydra/langs/cypher/openCypher.NumberLiteral")

_NumberLiteral_double = (Core.FieldName "double")

_NumberLiteral_integer = (Core.FieldName "integer")

newtype StringLiteral = 
  StringLiteral {
    unStringLiteral :: String}
  deriving (Eq, Ord, Read, Show)

_StringLiteral = (Core.Name "hydra/langs/cypher/openCypher.StringLiteral")

newtype ListLiteral = 
  ListLiteral {
    unListLiteral :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_ListLiteral = (Core.Name "hydra/langs/cypher/openCypher.ListLiteral")

newtype MapLiteral = 
  MapLiteral {
    unMapLiteral :: [KeyValuePair]}
  deriving (Eq, Ord, Read, Show)

_MapLiteral = (Core.Name "hydra/langs/cypher/openCypher.MapLiteral")

data KeyValuePair = 
  KeyValuePair {
    keyValuePairKey :: PropertyKeyName,
    keyValuePairValue :: Expression}
  deriving (Eq, Ord, Read, Show)

_KeyValuePair = (Core.Name "hydra/langs/cypher/openCypher.KeyValuePair")

_KeyValuePair_key = (Core.FieldName "key")

_KeyValuePair_value = (Core.FieldName "value")

newtype PropertyKeyName = 
  PropertyKeyName {
    unPropertyKeyName :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyName = (Core.Name "hydra/langs/cypher/openCypher.PropertyKeyName")

data Parameter = 
  ParameterSymbolic String |
  ParameterInteger Integer
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra/langs/cypher/openCypher.Parameter")

_Parameter_symbolic = (Core.FieldName "symbolic")

_Parameter_integer = (Core.FieldName "integer")