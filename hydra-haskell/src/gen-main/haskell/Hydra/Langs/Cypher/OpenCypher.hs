-- | An OpenCypher query model based on the M23 EBNF grammar. See https://opencypher.org/resources.

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
    regularQueryQuery :: SingleQuery,
    regularQueryUnion :: [Union]}
  deriving (Eq, Ord, Read, Show)

_RegularQuery = (Core.Name "hydra/langs/cypher/openCypher.RegularQuery")

_RegularQuery_query = (Core.FieldName "query")

_RegularQuery_union = (Core.FieldName "union")

data Union = 
  UnionAll SingleQuery |
  UnionDistinct SingleQuery
  deriving (Eq, Ord, Read, Show)

_Union = (Core.Name "hydra/langs/cypher/openCypher.Union")

_Union_all = (Core.FieldName "all")

_Union_distinct = (Core.FieldName "distinct")

data SingleQuery = 
  SingleQuerySinglePart SinglePartQuery |
  SingleQueryMultiPart MultiPartQuery
  deriving (Eq, Ord, Read, Show)

_SingleQuery = (Core.Name "hydra/langs/cypher/openCypher.SingleQuery")

_SingleQuery_singlePart = (Core.FieldName "singlePart")

_SingleQuery_multiPart = (Core.FieldName "multiPart")

data SinglePartQuery = 
  SinglePartQueryReading ReadingQuery |
  SinglePartQueryReturn UpdatingQuery
  deriving (Eq, Ord, Read, Show)

_SinglePartQuery = (Core.Name "hydra/langs/cypher/openCypher.SinglePartQuery")

_SinglePartQuery_reading = (Core.FieldName "reading")

_SinglePartQuery_return = (Core.FieldName "return")

data ReadingQuery = 
  ReadingQuery {
    readingQueryReading :: [ReadingClause],
    readingQueryReturn :: ProjectionBody}
  deriving (Eq, Ord, Read, Show)

_ReadingQuery = (Core.Name "hydra/langs/cypher/openCypher.ReadingQuery")

_ReadingQuery_reading = (Core.FieldName "reading")

_ReadingQuery_return = (Core.FieldName "return")

data UpdatingQuery = 
  UpdatingQuery {
    updatingQueryReading :: [ReadingClause],
    updatingQueryUpdating :: [UpdatingClause],
    updatingQueryReturn :: (Maybe ProjectionBody)}
  deriving (Eq, Ord, Read, Show)

_UpdatingQuery = (Core.Name "hydra/langs/cypher/openCypher.UpdatingQuery")

_UpdatingQuery_reading = (Core.FieldName "reading")

_UpdatingQuery_updating = (Core.FieldName "updating")

_UpdatingQuery_return = (Core.FieldName "return")

data MultiPartQuery = 
  MultiPartQuery {
    multiPartQueryReading :: [ReadingClause],
    multiPartQueryUpdating :: [UpdatingClause],
    multiPartQueryWith :: With,
    multiPartQueryQuery :: SinglePartQuery}
  deriving (Eq, Ord, Read, Show)

_MultiPartQuery = (Core.Name "hydra/langs/cypher/openCypher.MultiPartQuery")

_MultiPartQuery_reading = (Core.FieldName "reading")

_MultiPartQuery_updating = (Core.FieldName "updating")

_MultiPartQuery_with = (Core.FieldName "with")

_MultiPartQuery_query = (Core.FieldName "query")

data UpdatingClause = 
  UpdatingClauseCreate Create |
  UpdatingClauseMerge Merge |
  UpdatingClauseDelete Delete |
  UpdatingClauseSet [SetItem] |
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
  ReadingClauseCall InQueryCall
  deriving (Eq, Ord, Read, Show)

_ReadingClause = (Core.Name "hydra/langs/cypher/openCypher.ReadingClause")

_ReadingClause_match = (Core.FieldName "match")

_ReadingClause_unwind = (Core.FieldName "unwind")

_ReadingClause_call = (Core.FieldName "call")

data Match = 
  Match {
    matchOptional :: Bool,
    matchPattern :: Pattern}
  deriving (Eq, Ord, Read, Show)

_Match = (Core.Name "hydra/langs/cypher/openCypher.Match")

_Match_optional = (Core.FieldName "optional")

_Match_pattern = (Core.FieldName "pattern")

data Pattern = 
  Pattern {
    patternParts :: [PatternPart],
    patternWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/langs/cypher/openCypher.Pattern")

_Pattern_parts = (Core.FieldName "parts")

_Pattern_where = (Core.FieldName "where")

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
    mergePattern :: PatternPart,
    mergeActions :: [MergeAction]}
  deriving (Eq, Ord, Read, Show)

_Merge = (Core.Name "hydra/langs/cypher/openCypher.Merge")

_Merge_pattern = (Core.FieldName "pattern")

_Merge_actions = (Core.FieldName "actions")

data MergeAction = 
  MergeAction {
    mergeActionCreate :: Bool,
    mergeActionSet :: [SetItem]}
  deriving (Eq, Ord, Read, Show)

_MergeAction = (Core.Name "hydra/langs/cypher/openCypher.MergeAction")

_MergeAction_create = (Core.FieldName "create")

_MergeAction_set = (Core.FieldName "set")

newtype Create = 
  Create {
    unCreate :: [PatternPart]}
  deriving (Eq, Ord, Read, Show)

_Create = (Core.Name "hydra/langs/cypher/openCypher.Create")

data SetItem = 
  SetItemPropertyEquals PropertyEquals |
  SetItemVariableEquals VariableEquals |
  SetItemVariablePlusEquals VariablePlusEquals |
  SetItemVariableNodeLabels VariableNodeLabels
  deriving (Eq, Ord, Read, Show)

_SetItem = (Core.Name "hydra/langs/cypher/openCypher.SetItem")

_SetItem_propertyEquals = (Core.FieldName "propertyEquals")

_SetItem_variableEquals = (Core.FieldName "variableEquals")

_SetItem_variablePlusEquals = (Core.FieldName "variablePlusEquals")

_SetItem_variableNodeLabels = (Core.FieldName "variableNodeLabels")

data PropertyEquals = 
  PropertyEquals {
    propertyEqualsLeft :: PropertyExpression,
    propertyEqualsRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_PropertyEquals = (Core.Name "hydra/langs/cypher/openCypher.PropertyEquals")

_PropertyEquals_left = (Core.FieldName "left")

_PropertyEquals_right = (Core.FieldName "right")

data VariableEquals = 
  VariableEquals {
    variableEqualsLeft :: Variable,
    variableEqualsRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariableEquals = (Core.Name "hydra/langs/cypher/openCypher.VariableEquals")

_VariableEquals_left = (Core.FieldName "left")

_VariableEquals_right = (Core.FieldName "right")

data VariablePlusEquals = 
  VariablePlusEquals {
    variablePlusEqualsLeft :: Variable,
    variablePlusEqualsRight :: Expression}
  deriving (Eq, Ord, Read, Show)

_VariablePlusEquals = (Core.Name "hydra/langs/cypher/openCypher.VariablePlusEquals")

_VariablePlusEquals_left = (Core.FieldName "left")

_VariablePlusEquals_right = (Core.FieldName "right")

data VariableNodeLabels = 
  VariableNodeLabels {
    variableNodeLabelsVariable :: Variable,
    variableNodeLabelsLabels :: [NodeLabel]}
  deriving (Eq, Ord, Read, Show)

_VariableNodeLabels = (Core.Name "hydra/langs/cypher/openCypher.VariableNodeLabels")

_VariableNodeLabels_variable = (Core.FieldName "variable")

_VariableNodeLabels_labels = (Core.FieldName "labels")

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
  RemoveItemVariableNodeLabels VariableNodeLabels |
  RemoveItemPropertyExpression PropertyExpression
  deriving (Eq, Ord, Read, Show)

_RemoveItem = (Core.Name "hydra/langs/cypher/openCypher.RemoveItem")

_RemoveItem_variableNodeLabels = (Core.FieldName "variableNodeLabels")

_RemoveItem_propertyExpression = (Core.FieldName "propertyExpression")

data InQueryCall = 
  InQueryCall {
    inQueryCallInvocation :: ProcedureInvocation,
    inQueryCallYield :: (Maybe YieldItems)}
  deriving (Eq, Ord, Read, Show)

_InQueryCall = (Core.Name "hydra/langs/cypher/openCypher.InQueryCall")

_InQueryCall_invocation = (Core.FieldName "invocation")

_InQueryCall_yield = (Core.FieldName "yield")

data StandaloneCall = 
  StandaloneCall {
    standaloneCallInvocation :: ProcedureInvocation,
    standaloneCallYield :: (Maybe YieldExpression)}
  deriving (Eq, Ord, Read, Show)

_StandaloneCall = (Core.Name "hydra/langs/cypher/openCypher.StandaloneCall")

_StandaloneCall_invocation = (Core.FieldName "invocation")

_StandaloneCall_yield = (Core.FieldName "yield")

data YieldExpression = 
  YieldExpressionAll  |
  YieldExpressionList YieldItems
  deriving (Eq, Ord, Read, Show)

_YieldExpression = (Core.Name "hydra/langs/cypher/openCypher.YieldExpression")

_YieldExpression_all = (Core.FieldName "all")

_YieldExpression_list = (Core.FieldName "list")

data YieldItems = 
  YieldItems {
    yieldItemsItems :: [YieldItem],
    yieldItemsWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_YieldItems = (Core.Name "hydra/langs/cypher/openCypher.YieldItems")

_YieldItems_items = (Core.FieldName "items")

_YieldItems_where = (Core.FieldName "where")

data YieldItem = 
  YieldItem {
    yieldItemResultField :: (Maybe ProcedureResultField),
    yieldItemVariable :: Variable}
  deriving (Eq, Ord, Read, Show)

_YieldItem = (Core.Name "hydra/langs/cypher/openCypher.YieldItem")

_YieldItem_resultField = (Core.FieldName "resultField")

_YieldItem_variable = (Core.FieldName "variable")

data With = 
  With {
    withProjection :: ProjectionBody,
    withWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_With = (Core.Name "hydra/langs/cypher/openCypher.With")

_With_projection = (Core.FieldName "projection")

_With_where = (Core.FieldName "where")

data ProjectionBody = 
  ProjectionBody {
    projectionBodyDistinct :: Bool,
    projectionBodyItems :: ProjectionItems,
    projectionBodyOrder :: [SortItem],
    projectionBodySkip :: (Maybe Expression),
    projectionBodyLimit :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ProjectionBody = (Core.Name "hydra/langs/cypher/openCypher.ProjectionBody")

_ProjectionBody_distinct = (Core.FieldName "distinct")

_ProjectionBody_items = (Core.FieldName "items")

_ProjectionBody_order = (Core.FieldName "order")

_ProjectionBody_skip = (Core.FieldName "skip")

_ProjectionBody_limit = (Core.FieldName "limit")

data ProjectionItems = 
  ProjectionItems {
    projectionItemsAll :: Bool,
    projectionItemsItems :: [ProjectionItem]}
  deriving (Eq, Ord, Read, Show)

_ProjectionItems = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItems")

_ProjectionItems_all = (Core.FieldName "all")

_ProjectionItems_items = (Core.FieldName "items")

data ProjectionItem = 
  ProjectionItem {
    projectionItemExpression :: Expression,
    projectionItemAlias :: (Maybe Variable)}
  deriving (Eq, Ord, Read, Show)

_ProjectionItem = (Core.Name "hydra/langs/cypher/openCypher.ProjectionItem")

_ProjectionItem_expression = (Core.FieldName "expression")

_ProjectionItem_alias = (Core.FieldName "alias")

data SortItem = 
  SortItem {
    sortItemSortBy :: Expression,
    sortItemDescending :: Bool}
  deriving (Eq, Ord, Read, Show)

_SortItem = (Core.Name "hydra/langs/cypher/openCypher.SortItem")

_SortItem_sortBy = (Core.FieldName "sortBy")

_SortItem_descending = (Core.FieldName "descending")

data PatternPart = 
  PatternPart {
    patternPartVariable :: (Maybe Variable),
    patternPartPattern :: PatternElement}
  deriving (Eq, Ord, Read, Show)

_PatternPart = (Core.Name "hydra/langs/cypher/openCypher.PatternPart")

_PatternPart_variable = (Core.FieldName "variable")

_PatternPart_pattern = (Core.FieldName "pattern")

data PatternElement = 
  PatternElementNodePattern NodePatternExpression |
  PatternElementParens PatternElement
  deriving (Eq, Ord, Read, Show)

_PatternElement = (Core.Name "hydra/langs/cypher/openCypher.PatternElement")

_PatternElement_nodePattern = (Core.FieldName "nodePattern")

_PatternElement_parens = (Core.FieldName "parens")

data NodePatternExpression = 
  NodePatternExpression {
    nodePatternExpressionPattern :: NodePattern,
    nodePatternExpressionChains :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_NodePatternExpression = (Core.Name "hydra/langs/cypher/openCypher.NodePatternExpression")

_NodePatternExpression_pattern = (Core.FieldName "pattern")

_NodePatternExpression_chains = (Core.FieldName "chains")

data RelationshipsPattern = 
  RelationshipsPattern {
    relationshipsPatternPattern :: NodePattern,
    relationshipsPatternChains :: [PatternElementChain]}
  deriving (Eq, Ord, Read, Show)

_RelationshipsPattern = (Core.Name "hydra/langs/cypher/openCypher.RelationshipsPattern")

_RelationshipsPattern_pattern = (Core.FieldName "pattern")

_RelationshipsPattern_chains = (Core.FieldName "chains")

data NodePattern = 
  NodePattern {
    nodePatternVariable :: (Maybe Variable),
    nodePatternLabels :: [NodeLabel],
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
    relationshipPatternDirection :: RelationshipDirection,
    relationshipPatternDetail :: RelationshipDetail}
  deriving (Eq, Ord, Read, Show)

_RelationshipPattern = (Core.Name "hydra/langs/cypher/openCypher.RelationshipPattern")

_RelationshipPattern_direction = (Core.FieldName "direction")

_RelationshipPattern_detail = (Core.FieldName "detail")

data RelationshipDirection = 
  RelationshipDirectionLeftToRight  |
  RelationshipDirectionRightToLeft  |
  RelationshipDirectionBidirectional  |
  RelationshipDirectionUndirected 
  deriving (Eq, Ord, Read, Show)

_RelationshipDirection = (Core.Name "hydra/langs/cypher/openCypher.RelationshipDirection")

_RelationshipDirection_leftToRight = (Core.FieldName "leftToRight")

_RelationshipDirection_rightToLeft = (Core.FieldName "rightToLeft")

_RelationshipDirection_bidirectional = (Core.FieldName "bidirectional")

_RelationshipDirection_undirected = (Core.FieldName "undirected")

data RelationshipDetail = 
  RelationshipDetail {
    relationshipDetailVariable :: (Maybe Variable),
    relationshipDetailTypes :: [RelTypeName],
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

newtype NodeLabel = 
  NodeLabel {
    unNodeLabel :: String}
  deriving (Eq, Ord, Read, Show)

_NodeLabel = (Core.Name "hydra/langs/cypher/openCypher.NodeLabel")

data RangeLiteral = 
  RangeLiteral {
    rangeLiteralFrom :: (Maybe Int),
    rangeLiteralTo :: (Maybe Int)}
  deriving (Eq, Ord, Read, Show)

_RangeLiteral = (Core.Name "hydra/langs/cypher/openCypher.RangeLiteral")

_RangeLiteral_from = (Core.FieldName "from")

_RangeLiteral_to = (Core.FieldName "to")

newtype RelTypeName = 
  RelTypeName {
    unRelTypeName :: String}
  deriving (Eq, Ord, Read, Show)

_RelTypeName = (Core.Name "hydra/langs/cypher/openCypher.RelTypeName")

data PropertyExpression = 
  PropertyExpression {
    propertyExpressionAtom :: Atom,
    propertyExpressionLookups :: [PropertyKeyName]}
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
    comparisonExpressionPrimary :: StringListNullPredicateExpression,
    comparisonExpressionComparisons :: [PartialComparisonExpression]}
  deriving (Eq, Ord, Read, Show)

_ComparisonExpression = (Core.Name "hydra/langs/cypher/openCypher.ComparisonExpression")

_ComparisonExpression_primary = (Core.FieldName "primary")

_ComparisonExpression_comparisons = (Core.FieldName "comparisons")

data PartialComparisonExpression = 
  PartialComparisonExpression {
    partialComparisonExpressionOperator :: ComparisonOperator,
    partialComparisonExpressionExpression :: StringListNullPredicateExpression}
  deriving (Eq, Ord, Read, Show)

_PartialComparisonExpression = (Core.Name "hydra/langs/cypher/openCypher.PartialComparisonExpression")

_PartialComparisonExpression_operator = (Core.FieldName "operator")

_PartialComparisonExpression_expression = (Core.FieldName "expression")

data ComparisonOperator = 
  ComparisonOperatorEqual  |
  ComparisonOperatorNotEqual  |
  ComparisonOperatorLessThan  |
  ComparisonOperatorGreaterThan  |
  ComparisonOperatorLessThanOrEqual  |
  ComparisonOperatorGreaterThanOrEqual 
  deriving (Eq, Ord, Read, Show)

_ComparisonOperator = (Core.Name "hydra/langs/cypher/openCypher.ComparisonOperator")

_ComparisonOperator_equal = (Core.FieldName "equal")

_ComparisonOperator_notEqual = (Core.FieldName "notEqual")

_ComparisonOperator_lessThan = (Core.FieldName "lessThan")

_ComparisonOperator_greaterThan = (Core.FieldName "greaterThan")

_ComparisonOperator_lessThanOrEqual = (Core.FieldName "lessThanOrEqual")

_ComparisonOperator_greaterThanOrEqual = (Core.FieldName "greaterThanOrEqual")

data StringListNullPredicateExpression = 
  StringListNullPredicateExpression {
    stringListNullPredicateExpressionPrefix :: AddOrSubtractExpression,
    stringListNullPredicateExpressionSuffixes :: [StringListNullPredicateSuffix]}
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateExpression")

_StringListNullPredicateExpression_prefix = (Core.FieldName "prefix")

_StringListNullPredicateExpression_suffixes = (Core.FieldName "suffixes")

data StringListNullPredicateSuffix = 
  StringListNullPredicateSuffixString StringPredicateExpression |
  StringListNullPredicateSuffixList AddOrSubtractExpression |
  -- | True if NULL, false if NOT NULL
  StringListNullPredicateSuffixNull Bool
  deriving (Eq, Ord, Read, Show)

_StringListNullPredicateSuffix = (Core.Name "hydra/langs/cypher/openCypher.StringListNullPredicateSuffix")

_StringListNullPredicateSuffix_string = (Core.FieldName "string")

_StringListNullPredicateSuffix_list = (Core.FieldName "list")

_StringListNullPredicateSuffix_null = (Core.FieldName "null")

data StringPredicateExpression = 
  StringPredicateExpression {
    stringPredicateExpressionPredicate :: StringPredicate,
    stringPredicateExpressionExpression :: AddOrSubtractExpression}
  deriving (Eq, Ord, Read, Show)

_StringPredicateExpression = (Core.Name "hydra/langs/cypher/openCypher.StringPredicateExpression")

_StringPredicateExpression_predicate = (Core.FieldName "predicate")

_StringPredicateExpression_expression = (Core.FieldName "expression")

data StringPredicate = 
  StringPredicateStartsWith  |
  StringPredicateEndsWith  |
  StringPredicateContains 
  deriving (Eq, Ord, Read, Show)

_StringPredicate = (Core.Name "hydra/langs/cypher/openCypher.StringPredicate")

_StringPredicate_startsWith = (Core.FieldName "startsWith")

_StringPredicate_endsWith = (Core.FieldName "endsWith")

_StringPredicate_contains = (Core.FieldName "contains")

data AddOrSubtractExpression = 
  AddOrSubtractExpression {
    addOrSubtractExpressionLhs :: MultiplyDivideModuloExpression,
    addOrSubtractExpressionRhs :: [AddOrSubtractRhs]}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractExpression = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractExpression")

_AddOrSubtractExpression_lhs = (Core.FieldName "lhs")

_AddOrSubtractExpression_rhs = (Core.FieldName "rhs")

data AddOrSubtractRhs = 
  AddOrSubtractRhs {
    addOrSubtractRhsOperator :: PlusOrMinus,
    addOrSubtractRhsExpression :: MultiplyDivideModuloExpression}
  deriving (Eq, Ord, Read, Show)

_AddOrSubtractRhs = (Core.Name "hydra/langs/cypher/openCypher.AddOrSubtractRhs")

_AddOrSubtractRhs_operator = (Core.FieldName "operator")

_AddOrSubtractRhs_expression = (Core.FieldName "expression")

data MultiplyDivideModuloExpression = 
  MultiplyDivideModuloExpression {
    multiplyDivideModuloExpressionLhs :: PowerOfExpression,
    multiplyDivideModuloExpressionRhs :: [MultiplyDivideModuloRhs]}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloExpression = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloExpression")

_MultiplyDivideModuloExpression_lhs = (Core.FieldName "lhs")

_MultiplyDivideModuloExpression_rhs = (Core.FieldName "rhs")

data MultiplyDivideModuloRhs = 
  MultiplyDivideModuloRhs {
    multiplyDivideModuloRhsOperator :: TimesOrDivideOrModulo,
    multiplyDivideModuloRhsExpression :: PowerOfExpression}
  deriving (Eq, Ord, Read, Show)

_MultiplyDivideModuloRhs = (Core.Name "hydra/langs/cypher/openCypher.MultiplyDivideModuloRhs")

_MultiplyDivideModuloRhs_operator = (Core.FieldName "operator")

_MultiplyDivideModuloRhs_expression = (Core.FieldName "expression")

data TimesOrDivideOrModulo = 
  TimesOrDivideOrModuloTimes  |
  TimesOrDivideOrModuloDivide  |
  TimesOrDivideOrModuloModulo 
  deriving (Eq, Ord, Read, Show)

_TimesOrDivideOrModulo = (Core.Name "hydra/langs/cypher/openCypher.TimesOrDivideOrModulo")

_TimesOrDivideOrModulo_times = (Core.FieldName "times")

_TimesOrDivideOrModulo_divide = (Core.FieldName "divide")

_TimesOrDivideOrModulo_modulo = (Core.FieldName "modulo")

newtype PowerOfExpression = 
  PowerOfExpression {
    unPowerOfExpression :: [UnaryAddOrSubtractExpression]}
  deriving (Eq, Ord, Read, Show)

_PowerOfExpression = (Core.Name "hydra/langs/cypher/openCypher.PowerOfExpression")

data UnaryAddOrSubtractExpression = 
  UnaryAddOrSubtractExpression {
    unaryAddOrSubtractExpressionOperator :: (Maybe PlusOrMinus),
    unaryAddOrSubtractExpressionExpression :: NonArithmeticOperatorExpression}
  deriving (Eq, Ord, Read, Show)

_UnaryAddOrSubtractExpression = (Core.Name "hydra/langs/cypher/openCypher.UnaryAddOrSubtractExpression")

_UnaryAddOrSubtractExpression_operator = (Core.FieldName "operator")

_UnaryAddOrSubtractExpression_expression = (Core.FieldName "expression")

data PlusOrMinus = 
  PlusOrMinusPlus  |
  PlusOrMinusMinus 
  deriving (Eq, Ord, Read, Show)

_PlusOrMinus = (Core.Name "hydra/langs/cypher/openCypher.PlusOrMinus")

_PlusOrMinus_plus = (Core.FieldName "plus")

_PlusOrMinus_minus = (Core.FieldName "minus")

data NonArithmeticOperatorExpression = 
  NonArithmeticOperatorExpression {
    nonArithmeticOperatorExpressionAtom :: Atom,
    nonArithmeticOperatorExpressionSuffixes :: [NonArithmeticOperatorInfix],
    nonArithmeticOperatorExpressionLabels :: [NodeLabel]}
  deriving (Eq, Ord, Read, Show)

_NonArithmeticOperatorExpression = (Core.Name "hydra/langs/cypher/openCypher.NonArithmeticOperatorExpression")

_NonArithmeticOperatorExpression_atom = (Core.FieldName "atom")

_NonArithmeticOperatorExpression_suffixes = (Core.FieldName "suffixes")

_NonArithmeticOperatorExpression_labels = (Core.FieldName "labels")

data NonArithmeticOperatorInfix = 
  NonArithmeticOperatorInfixList ListOperatorExpression |
  NonArithmeticOperatorInfixProperty PropertyKeyName
  deriving (Eq, Ord, Read, Show)

_NonArithmeticOperatorInfix = (Core.Name "hydra/langs/cypher/openCypher.NonArithmeticOperatorInfix")

_NonArithmeticOperatorInfix_list = (Core.FieldName "list")

_NonArithmeticOperatorInfix_property = (Core.FieldName "property")

data ListOperatorExpression = 
  ListOperatorExpressionSingle Expression |
  ListOperatorExpressionRange ListOperatorRange
  deriving (Eq, Ord, Read, Show)

_ListOperatorExpression = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorExpression")

_ListOperatorExpression_single = (Core.FieldName "single")

_ListOperatorExpression_range = (Core.FieldName "range")

data ListOperatorRange = 
  ListOperatorRange {
    listOperatorRangeFrom :: (Maybe Expression),
    listOperatorRangeTo :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ListOperatorRange = (Core.Name "hydra/langs/cypher/openCypher.ListOperatorRange")

_ListOperatorRange_from = (Core.FieldName "from")

_ListOperatorRange_to = (Core.FieldName "to")

data Atom = 
  AtomParameter Parameter |
  AtomCase CaseExpression |
  AtomCount  |
  AtomList ListComprehension |
  AtomPattern PatternComprehension |
  AtomQuantifier Quantifier |
  AtomPredicate RelationshipsPattern |
  AtomParens Expression |
  AtomFunction FunctionInvocation |
  AtomExistence ExistentialSubquery |
  AtomVariable Variable
  deriving (Eq, Ord, Read, Show)

_Atom = (Core.Name "hydra/langs/cypher/openCypher.Atom")

_Atom_parameter = (Core.FieldName "parameter")

_Atom_case = (Core.FieldName "case")

_Atom_count = (Core.FieldName "count")

_Atom_list = (Core.FieldName "list")

_Atom_pattern = (Core.FieldName "pattern")

_Atom_quantifier = (Core.FieldName "quantifier")

_Atom_predicate = (Core.FieldName "predicate")

_Atom_parens = (Core.FieldName "parens")

_Atom_function = (Core.FieldName "function")

_Atom_existence = (Core.FieldName "existence")

_Atom_variable = (Core.FieldName "variable")

data CaseExpression = 
  CaseExpressionCases [Case] |
  CaseExpressionElse (Maybe Expression)
  deriving (Eq, Ord, Read, Show)

_CaseExpression = (Core.Name "hydra/langs/cypher/openCypher.CaseExpression")

_CaseExpression_cases = (Core.FieldName "cases")

_CaseExpression_else = (Core.FieldName "else")

data Case = 
  Case {
    caseExpression :: (Maybe Expression),
    caseAlternatives :: [CaseAlternative]}
  deriving (Eq, Ord, Read, Show)

_Case = (Core.Name "hydra/langs/cypher/openCypher.Case")

_Case_expression = (Core.FieldName "expression")

_Case_alternatives = (Core.FieldName "alternatives")

data CaseAlternative = 
  CaseAlternative {
    caseAlternativeWhen :: Expression,
    caseAlternativeThen :: Expression}
  deriving (Eq, Ord, Read, Show)

_CaseAlternative = (Core.Name "hydra/langs/cypher/openCypher.CaseAlternative")

_CaseAlternative_when = (Core.FieldName "when")

_CaseAlternative_then = (Core.FieldName "then")

data ListComprehension = 
  ListComprehension {
    listComprehensionHead :: FilterExpression,
    listComprehensionBody :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_ListComprehension = (Core.Name "hydra/langs/cypher/openCypher.ListComprehension")

_ListComprehension_head = (Core.FieldName "head")

_ListComprehension_body = (Core.FieldName "body")

data PatternComprehension = 
  PatternComprehension {
    patternComprehensionVariable :: (Maybe Variable),
    patternComprehensionPattern :: RelationshipsPattern,
    patternComprehensionWhere :: (Maybe Expression),
    patternComprehensionBody :: Expression}
  deriving (Eq, Ord, Read, Show)

_PatternComprehension = (Core.Name "hydra/langs/cypher/openCypher.PatternComprehension")

_PatternComprehension_variable = (Core.FieldName "variable")

_PatternComprehension_pattern = (Core.FieldName "pattern")

_PatternComprehension_where = (Core.FieldName "where")

_PatternComprehension_body = (Core.FieldName "body")

data Quantifier = 
  Quantifier {
    quantifierType :: QuantifierType,
    quantifierExpression :: FilterExpression}
  deriving (Eq, Ord, Read, Show)

_Quantifier = (Core.Name "hydra/langs/cypher/openCypher.Quantifier")

_Quantifier_type = (Core.FieldName "type")

_Quantifier_expression = (Core.FieldName "expression")

data QuantifierType = 
  QuantifierTypeAll  |
  QuantifierTypeAny  |
  QuantifierTypeNone  |
  QuantifierTypeSingle 
  deriving (Eq, Ord, Read, Show)

_QuantifierType = (Core.Name "hydra/langs/cypher/openCypher.QuantifierType")

_QuantifierType_all = (Core.FieldName "all")

_QuantifierType_any = (Core.FieldName "any")

_QuantifierType_none = (Core.FieldName "none")

_QuantifierType_single = (Core.FieldName "single")

data FilterExpression = 
  FilterExpression {
    filterExpressionId :: IdInColl,
    filterExpressionWhere :: (Maybe Expression)}
  deriving (Eq, Ord, Read, Show)

_FilterExpression = (Core.Name "hydra/langs/cypher/openCypher.FilterExpression")

_FilterExpression_id = (Core.FieldName "id")

_FilterExpression_where = (Core.FieldName "where")

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
    functionInvocationFunction :: FunctionName,
    functionInvocationDistinct :: Bool,
    functionInvocationArguments :: [Expression]}
  deriving (Eq, Ord, Read, Show)

_FunctionInvocation = (Core.Name "hydra/langs/cypher/openCypher.FunctionInvocation")

_FunctionInvocation_function = (Core.FieldName "function")

_FunctionInvocation_distinct = (Core.FieldName "distinct")

_FunctionInvocation_arguments = (Core.FieldName "arguments")

data FunctionName = 
  FunctionName {
    functionNameNamespace :: Namespace,
    functionNameName :: String}
  deriving (Eq, Ord, Read, Show)

_FunctionName = (Core.Name "hydra/langs/cypher/openCypher.FunctionName")

_FunctionName_namespace = (Core.FieldName "namespace")

_FunctionName_name = (Core.FieldName "name")

data ExistentialSubquery = 
  ExistentialSubqueryQuery RegularQuery |
  ExistentialSubqueryPattern Pattern
  deriving (Eq, Ord, Read, Show)

_ExistentialSubquery = (Core.Name "hydra/langs/cypher/openCypher.ExistentialSubquery")

_ExistentialSubquery_query = (Core.FieldName "query")

_ExistentialSubquery_pattern = (Core.FieldName "pattern")

data ProcedureInvocation = 
  ProcedureInvocation {
    procedureInvocationName :: ProcedureName,
    procedureInvocationArguments :: (Maybe [Expression])}
  deriving (Eq, Ord, Read, Show)

_ProcedureInvocation = (Core.Name "hydra/langs/cypher/openCypher.ProcedureInvocation")

_ProcedureInvocation_name = (Core.FieldName "name")

_ProcedureInvocation_arguments = (Core.FieldName "arguments")

newtype ProcedureResultField = 
  ProcedureResultField {
    unProcedureResultField :: String}
  deriving (Eq, Ord, Read, Show)

_ProcedureResultField = (Core.Name "hydra/langs/cypher/openCypher.ProcedureResultField")

data ProcedureName = 
  ProcedureName {
    procedureNameNamespace :: Namespace,
    procedureNameName :: String}
  deriving (Eq, Ord, Read, Show)

_ProcedureName = (Core.Name "hydra/langs/cypher/openCypher.ProcedureName")

_ProcedureName_namespace = (Core.FieldName "namespace")

_ProcedureName_name = (Core.FieldName "name")

newtype Namespace = 
  Namespace {
    unNamespace :: [String]}
  deriving (Eq, Ord, Read, Show)

_Namespace = (Core.Name "hydra/langs/cypher/openCypher.Namespace")

newtype Variable = 
  Variable {
    unVariable :: String}
  deriving (Eq, Ord, Read, Show)

_Variable = (Core.Name "hydra/langs/cypher/openCypher.Variable")

data Literal = 
  LiteralBoolean Bool |
  LiteralNull  |
  LiteralDouble Double |
  LiteralInteger Int |
  LiteralString String |
  LiteralList [Expression] |
  LiteralMap MapLiteral
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/langs/cypher/openCypher.Literal")

_Literal_boolean = (Core.FieldName "boolean")

_Literal_null = (Core.FieldName "null")

_Literal_double = (Core.FieldName "double")

_Literal_integer = (Core.FieldName "integer")

_Literal_string = (Core.FieldName "string")

_Literal_list = (Core.FieldName "list")

_Literal_map = (Core.FieldName "map")

newtype MapLiteral = 
  MapLiteral {
    unMapLiteral :: (Map PropertyKeyName Expression)}
  deriving (Eq, Ord, Read, Show)

_MapLiteral = (Core.Name "hydra/langs/cypher/openCypher.MapLiteral")

newtype PropertyKeyName = 
  PropertyKeyName {
    unPropertyKeyName :: String}
  deriving (Eq, Ord, Read, Show)

_PropertyKeyName = (Core.Name "hydra/langs/cypher/openCypher.PropertyKeyName")

data Parameter = 
  Parameter {
    parameterName :: String,
    parameterIndex :: Int}
  deriving (Eq, Ord, Read, Show)

_Parameter = (Core.Name "hydra/langs/cypher/openCypher.Parameter")

_Parameter_name = (Core.FieldName "name")

_Parameter_index = (Core.FieldName "index")