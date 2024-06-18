-- | A model for characterizing OpenCypher queries and implementations in terms of included features.

module Hydra.Langs.Cypher.Features where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A set of features for aggregation functions.
data AggregateFeatures = 
  AggregateFeatures {
    -- | Whether to expect the AVG aggregate function.
    aggregateFeaturesAverageFeature :: Bool,
    -- | Whether to expect the COUNT aggregate function.
    aggregateFeaturesCountFeature :: Bool,
    -- | Whether to expect the MAX aggregate function.
    aggregateFeaturesMaxFeature :: Bool,
    -- | Whether to expect the MIN aggregate function.
    aggregateFeaturesMinFeature :: Bool,
    -- | Whether to expect the SUM aggregate function.
    aggregateFeaturesSumFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_AggregateFeatures = (Core.Name "hydra/langs/cypher/features.AggregateFeatures")

_AggregateFeatures_averageFeature = (Core.FieldName "averageFeature")

_AggregateFeatures_countFeature = (Core.FieldName "countFeature")

_AggregateFeatures_maxFeature = (Core.FieldName "maxFeature")

_AggregateFeatures_minFeature = (Core.FieldName "minFeature")

_AggregateFeatures_sumFeature = (Core.FieldName "sumFeature")

-- | A set of features for arithmetic operations.
data ArithmeticFeatures = 
  ArithmeticFeatures {
    -- | Whether to expect the + operator.
    arithmeticFeaturesPlusFeature :: Bool,
    -- | Whether to expect the - operator.
    arithmeticFeaturesMinusFeature :: Bool,
    -- | Whether to expect the * operator.
    arithmeticFeaturesMultiplyFeature :: Bool,
    -- | Whether to expect the / operator.
    arithmeticFeaturesDivideFeature :: Bool,
    -- | Whether to expect the % operator.
    arithmeticFeaturesModulusFeature :: Bool,
    -- | Whether to expect the ^ operator.
    arithmeticFeaturesPowerOfFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_ArithmeticFeatures = (Core.Name "hydra/langs/cypher/features.ArithmeticFeatures")

_ArithmeticFeatures_plusFeature = (Core.FieldName "plusFeature")

_ArithmeticFeatures_minusFeature = (Core.FieldName "minusFeature")

_ArithmeticFeatures_multiplyFeature = (Core.FieldName "multiplyFeature")

_ArithmeticFeatures_divideFeature = (Core.FieldName "divideFeature")

_ArithmeticFeatures_modulusFeature = (Core.FieldName "modulusFeature")

_ArithmeticFeatures_powerOfFeature = (Core.FieldName "powerOfFeature")

-- | A set of features for various kinds of atomic expressions.
data AtomFeatures = 
  AtomFeatures {
    -- | Whether to expect CASE expressions.
    atomFeaturesCaseExpressionFeature :: Bool,
    -- | Whether to expect the COUNT (*) expression.
    atomFeaturesCountFeature :: Bool,
    -- | Whether to expect existential subqueries.
    atomFeaturesExistentialSubqueryFeature :: Bool,
    -- | Whether to expect function invocation.
    atomFeaturesFunctionInvocationFeature :: Bool,
    -- | Whether to expect list comprehensions, and if so, which specific features
    atomFeaturesListComprehensionFeatures :: (Maybe ListComprehensionFeatures),
    -- | Whether to expect literal values, and if so, which specific features
    atomFeaturesLiteralFeatures :: (Maybe LiteralFeatures),
    -- | Whether to expect parameter expressions.
    atomFeaturesParameterFeature :: Bool,
    -- | Whether to expect pattern comprehensions.
    atomFeaturesPatternComprehensionFeature :: Bool,
    -- | Whether to expect relationship patterns as subexpressions.
    atomFeaturesPatternPredicateFeature :: Bool,
    -- | Whether to expect quantifier expressions, and if so, which specific features
    atomFeaturesQuantifierFeatures :: (Maybe QuantifierFeatures),
    -- | Whether to expect variable expressions (note: included by most if not all implementations).
    atomFeaturesVariableFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_AtomFeatures = (Core.Name "hydra/langs/cypher/features.AtomFeatures")

_AtomFeatures_caseExpressionFeature = (Core.FieldName "caseExpressionFeature")

_AtomFeatures_countFeature = (Core.FieldName "countFeature")

_AtomFeatures_existentialSubqueryFeature = (Core.FieldName "existentialSubqueryFeature")

_AtomFeatures_functionInvocationFeature = (Core.FieldName "functionInvocationFeature")

_AtomFeatures_listComprehensionFeatures = (Core.FieldName "listComprehensionFeatures")

_AtomFeatures_literalFeatures = (Core.FieldName "literalFeatures")

_AtomFeatures_parameterFeature = (Core.FieldName "parameterFeature")

_AtomFeatures_patternComprehensionFeature = (Core.FieldName "patternComprehensionFeature")

_AtomFeatures_patternPredicateFeature = (Core.FieldName "patternPredicateFeature")

_AtomFeatures_quantifierFeatures = (Core.FieldName "quantifierFeatures")

_AtomFeatures_variableFeature = (Core.FieldName "variableFeature")

-- | A set of features for comparison operations.
data ComparisonFeatures = 
  ComparisonFeatures {
    -- | Whether to expect the = comparison operator.
    comparisonFeaturesEqualFeature :: Bool,
    -- | Whether to expect the > comparison operator.
    comparisonFeaturesGreaterThanFeature :: Bool,
    -- | Whether to expect the >= comparison operator.
    comparisonFeaturesGreaterThanOrEqualFeature :: Bool,
    -- | Whether to expect the < comparison operator.
    comparisonFeaturesLessThanFeature :: Bool,
    -- | Whether to expect the <= comparison operator.
    comparisonFeaturesLessThanOrEqualFeature :: Bool,
    -- | Whether to expect the <> comparison operator.
    comparisonFeaturesNotEqualFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_ComparisonFeatures = (Core.Name "hydra/langs/cypher/features.ComparisonFeatures")

_ComparisonFeatures_equalFeature = (Core.FieldName "equalFeature")

_ComparisonFeatures_greaterThanFeature = (Core.FieldName "greaterThanFeature")

_ComparisonFeatures_greaterThanOrEqualFeature = (Core.FieldName "greaterThanOrEqualFeature")

_ComparisonFeatures_lessThanFeature = (Core.FieldName "lessThanFeature")

_ComparisonFeatures_lessThanOrEqualFeature = (Core.FieldName "lessThanOrEqualFeature")

_ComparisonFeatures_notEqualFeature = (Core.FieldName "notEqualFeature")

-- | A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired.
data CypherFeatures = 
  CypherFeatures {
    -- | Whether to expect aggregate functions, and if so, which specific features
    cypherFeaturesAggregateFeatures :: (Maybe AggregateFeatures),
    -- | Whether to expect arithmetic operations, and if so, which specific features
    cypherFeaturesArithmeticFeatures :: (Maybe ArithmeticFeatures),
    -- | Whether to expect atomic expressions, and if so, which specific features
    cypherFeaturesAtomFeatures :: (Maybe AtomFeatures),
    -- | Whether to expect comparison operations, and if so, which specific features
    cypherFeaturesComparisonFeatures :: (Maybe ComparisonFeatures),
    -- | Whether to expect delete operations, and if so, which specific features
    cypherFeaturesDeleteFeatures :: (Maybe DeleteFeatures),
    -- | Whether to expect logical operations, and if so, which specific features
    cypherFeaturesLogicalFeatures :: (Maybe LogicalFeatures),
    -- | Whether to expect match queries, and if so, which specific features
    cypherFeaturesMatchFeatures :: (Maybe MatchFeatures),
    -- | Whether to expect merge operations, and if so, which specific features
    cypherFeaturesMergeFeatures :: (Maybe MergeFeatures),
    -- | Whether to expect node patterns, and if so, which specific features
    cypherFeaturesNodePatternFeatures :: (Maybe NodePatternFeatures),
    -- | Whether to expect IS NULL / IS NOT NULL checks, and if so, which specific features
    cypherFeaturesNullFeatures :: (Maybe NullFeatures),
    -- | Whether to expect procedure calls, and if so, which specific features
    cypherFeaturesProcedureCallFeatures :: (Maybe ProcedureCallFeatures),
    -- | Whether to expect projection operations, and if so, which specific features
    cypherFeaturesProjectionFeatures :: (Maybe ProjectionFeatures),
    -- | Whether to expect range literals, and if so, which specific features
    cypherFeaturesRangeLiteralFeatures :: (Maybe RangeLiteralFeatures),
    -- | Whether to expect reading operations, and if so, which specific features
    cypherFeaturesReadingFeatures :: (Maybe ReadingFeatures),
    -- | Whether to expect relationship directions, and if so, which specific features
    cypherFeaturesRelationshipDirectionFeatures :: (Maybe RelationshipDirectionFeatures),
    -- | Whether to expect relationship patterns, and if so, which specific features
    cypherFeaturesRelationshipPatternFeatures :: (Maybe RelationshipPatternFeatures),
    -- | Whether to expect remove operations, and if so, which specific features
    cypherFeaturesRemoveFeatures :: (Maybe RemoveFeatures),
    -- | Whether to expect set operations, and if so, which specific features
    cypherFeaturesSetFeatures :: (Maybe SetFeatures),
    -- | Whether to expect string operations, and if so, which specific features
    cypherFeaturesStringFeatures :: (Maybe StringFeatures),
    -- | Whether to expect updating operations, and if so, which specific features
    cypherFeaturesUpdatingFeatures :: (Maybe UpdatingFeatures)}
  deriving (Eq, Ord, Read, Show)

_CypherFeatures = (Core.Name "hydra/langs/cypher/features.CypherFeatures")

_CypherFeatures_aggregateFeatures = (Core.FieldName "aggregateFeatures")

_CypherFeatures_arithmeticFeatures = (Core.FieldName "arithmeticFeatures")

_CypherFeatures_atomFeatures = (Core.FieldName "atomFeatures")

_CypherFeatures_comparisonFeatures = (Core.FieldName "comparisonFeatures")

_CypherFeatures_deleteFeatures = (Core.FieldName "deleteFeatures")

_CypherFeatures_logicalFeatures = (Core.FieldName "logicalFeatures")

_CypherFeatures_matchFeatures = (Core.FieldName "matchFeatures")

_CypherFeatures_mergeFeatures = (Core.FieldName "mergeFeatures")

_CypherFeatures_nodePatternFeatures = (Core.FieldName "nodePatternFeatures")

_CypherFeatures_nullFeatures = (Core.FieldName "nullFeatures")

_CypherFeatures_procedureCallFeatures = (Core.FieldName "procedureCallFeatures")

_CypherFeatures_projectionFeatures = (Core.FieldName "projectionFeatures")

_CypherFeatures_rangeLiteralFeatures = (Core.FieldName "rangeLiteralFeatures")

_CypherFeatures_readingFeatures = (Core.FieldName "readingFeatures")

_CypherFeatures_relationshipDirectionFeatures = (Core.FieldName "relationshipDirectionFeatures")

_CypherFeatures_relationshipPatternFeatures = (Core.FieldName "relationshipPatternFeatures")

_CypherFeatures_removeFeatures = (Core.FieldName "removeFeatures")

_CypherFeatures_setFeatures = (Core.FieldName "setFeatures")

_CypherFeatures_stringFeatures = (Core.FieldName "stringFeatures")

_CypherFeatures_updatingFeatures = (Core.FieldName "updatingFeatures")

-- | A set of features for delete operations.
data DeleteFeatures = 
  DeleteFeatures {
    -- | Whether to expect the basic DELETE clause.
    deleteFeaturesDeleteFeature :: Bool,
    -- | Whether to expect the DETACH DELETE clause.
    deleteFeaturesDetachDeleteFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_DeleteFeatures = (Core.Name "hydra/langs/cypher/features.DeleteFeatures")

_DeleteFeatures_deleteFeature = (Core.FieldName "deleteFeature")

_DeleteFeatures_detachDeleteFeature = (Core.FieldName "detachDeleteFeature")

-- | A set of features for list comprehensions.
data ListComprehensionFeatures = 
  ListComprehensionFeatures {
    -- | Whether to expect basic list comprehensions.
    listComprehensionFeaturesListComprehensionFeature :: Bool,
    -- | Whether to expect list range comprehensions (e.g. [1..10]).
    listComprehensionFeaturesListRangeFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListComprehensionFeatures = (Core.Name "hydra/langs/cypher/features.ListComprehensionFeatures")

_ListComprehensionFeatures_listComprehensionFeature = (Core.FieldName "listComprehensionFeature")

_ListComprehensionFeatures_listRangeFeature = (Core.FieldName "listRangeFeature")

-- | A set of features for various types of literal values.
data LiteralFeatures = 
  LiteralFeatures {
    -- | Whether to expect boolean literals (note: included by most if not all implementations).
    literalFeaturesBooleanLiteralFeature :: Bool,
    -- | Whether to expect double-precision floating-point literals.
    literalFeaturesDoubleLiteralFeature :: Bool,
    -- | Whether to expect integer literals.
    literalFeaturesIntegerLiteralFeature :: Bool,
    -- | Whether to expect list literals.
    literalFeaturesListLiteralFeature :: Bool,
    -- | Whether to expect map literals.
    literalFeaturesMapLiteralFeature :: Bool,
    -- | Whether to expect the NULL literal.
    literalFeaturesNullLiteralFeature :: Bool,
    -- | Whether to expect string literals (note: included by most if not all implementations).
    literalFeaturesStringLiteralFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_LiteralFeatures = (Core.Name "hydra/langs/cypher/features.LiteralFeatures")

_LiteralFeatures_booleanLiteralFeature = (Core.FieldName "booleanLiteralFeature")

_LiteralFeatures_doubleLiteralFeature = (Core.FieldName "doubleLiteralFeature")

_LiteralFeatures_integerLiteralFeature = (Core.FieldName "integerLiteralFeature")

_LiteralFeatures_listLiteralFeature = (Core.FieldName "listLiteralFeature")

_LiteralFeatures_mapLiteralFeature = (Core.FieldName "mapLiteralFeature")

_LiteralFeatures_nullLiteralFeature = (Core.FieldName "nullLiteralFeature")

_LiteralFeatures_stringLiteralFeature = (Core.FieldName "stringLiteralFeature")

-- | A set of features for logical operations.
data LogicalFeatures = 
  LogicalFeatures {
    -- | Whether to expect the AND operator.
    logicalFeaturesAndFeature :: Bool,
    -- | Whether to expect the NOT operator.
    logicalFeaturesNotFeature :: Bool,
    -- | Whether to expect the OR operator.
    logicalFeaturesOrFeature :: Bool,
    -- | Whether to expect the XOR operator.
    logicalFeaturesXorFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_LogicalFeatures = (Core.Name "hydra/langs/cypher/features.LogicalFeatures")

_LogicalFeatures_andFeature = (Core.FieldName "andFeature")

_LogicalFeatures_notFeature = (Core.FieldName "notFeature")

_LogicalFeatures_orFeature = (Core.FieldName "orFeature")

_LogicalFeatures_xorFeature = (Core.FieldName "xorFeature")

-- | A set of features for match queries.
data MatchFeatures = 
  MatchFeatures {
    -- | Whether to expect OPTIONAL MATCH.
    matchFeaturesOptionalMatchFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_MatchFeatures = (Core.Name "hydra/langs/cypher/features.MatchFeatures")

_MatchFeatures_optionalMatchFeature = (Core.FieldName "optionalMatchFeature")

-- | A set of features for merge operations.
data MergeFeatures = 
  MergeFeatures {
    -- | Whether to expect MERGE with the ON CREATE action.
    mergeFeaturesMergeOnCreateFeature :: Bool,
    -- | Whether to expect MERGE with the ON MATCH action.
    mergeFeaturesMergeOnMatchFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_MergeFeatures = (Core.Name "hydra/langs/cypher/features.MergeFeatures")

_MergeFeatures_mergeOnCreateFeature = (Core.FieldName "mergeOnCreateFeature")

_MergeFeatures_mergeOnMatchFeature = (Core.FieldName "mergeOnMatchFeature")

-- | A set of features for node patterns.
data NodePatternFeatures = 
  NodePatternFeatures {
    -- | Whether to expect specifying multiple labels in a node pattern.
    nodePatternFeaturesMultipleLabelsInNodePatternFeature :: Bool,
    -- | Whether to expect specifying a parameter as part of a node pattern.
    nodePatternFeaturesNodePatternParameterFeature :: Bool,
    -- | Whether to expect specifying a key/value map of properties in a node pattern.
    nodePatternFeaturesNodePatternPropertyMapFeature :: Bool,
    -- | Whether to expect binding a variable to a node in a node pattern (note: included by most if not all implementations).
    nodePatternFeaturesVariableNodeFeature :: Bool,
    -- | Whether to expect omitting labels from a node pattern.
    nodePatternFeaturesWildcardLabelNodePatternFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_NodePatternFeatures = (Core.Name "hydra/langs/cypher/features.NodePatternFeatures")

_NodePatternFeatures_multipleLabelsInNodePatternFeature = (Core.FieldName "multipleLabelsInNodePatternFeature")

_NodePatternFeatures_nodePatternParameterFeature = (Core.FieldName "nodePatternParameterFeature")

_NodePatternFeatures_nodePatternPropertyMapFeature = (Core.FieldName "nodePatternPropertyMapFeature")

_NodePatternFeatures_variableNodeFeature = (Core.FieldName "variableNodeFeature")

_NodePatternFeatures_wildcardLabelNodePatternFeature = (Core.FieldName "wildcardLabelNodePatternFeature")

-- | A set of features for IS NULL / IS NOT NULL checks.
data NullFeatures = 
  NullFeatures {
    -- | Whether to expect the IS NULL operator.
    nullFeaturesIsNullFeature :: Bool,
    -- | Whether to expect the IS NOT NULL operator.
    nullFeaturesIsNotNullFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullFeatures = (Core.Name "hydra/langs/cypher/features.NullFeatures")

_NullFeatures_isNullFeature = (Core.FieldName "isNullFeature")

_NullFeatures_isNotNullFeature = (Core.FieldName "isNotNullFeature")

-- | A set of features for procedure calls.
data ProcedureCallFeatures = 
  ProcedureCallFeatures {
    -- | Whether to expect CALL within a query.
    procedureCallFeaturesInQueryCallFeature :: Bool,
    -- | Whether to expect standalone / top-level CALL.
    procedureCallFeaturesStandaloneCallFeature :: Bool,
    -- | Whether to expect the YIELD clause in CALL.
    procedureCallFeaturesYieldFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_ProcedureCallFeatures = (Core.Name "hydra/langs/cypher/features.ProcedureCallFeatures")

_ProcedureCallFeatures_inQueryCallFeature = (Core.FieldName "inQueryCallFeature")

_ProcedureCallFeatures_standaloneCallFeature = (Core.FieldName "standaloneCallFeature")

_ProcedureCallFeatures_yieldFeature = (Core.FieldName "yieldFeature")

-- | A set of features for projections.
data ProjectionFeatures = 
  ProjectionFeatures {
    -- | Whether to expect the LIMIT clause.
    projectionFeaturesLimitFeature :: Bool,
    -- | Whether to expect the ORDER BY clause.
    projectionFeaturesOrderByFeature :: Bool,
    -- | Whether to expect the DISTINCT keyword.
    projectionFeaturesProjectDistinctFeature :: Bool,
    -- | Whether to expect the * projection.
    projectionFeaturesProjectAllFeature :: Bool,
    -- | Whether to expect the AS keyword.
    projectionFeaturesProjectAsFeature :: Bool,
    -- | Whether to expect the SKIP clause.
    projectionFeaturesSkipFeature :: Bool,
    -- | Whether to expect the ASC/ASCENDING and DESC/DESCENDING keywords.
    projectionFeaturesSortOrderFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_ProjectionFeatures = (Core.Name "hydra/langs/cypher/features.ProjectionFeatures")

_ProjectionFeatures_limitFeature = (Core.FieldName "limitFeature")

_ProjectionFeatures_orderByFeature = (Core.FieldName "orderByFeature")

_ProjectionFeatures_projectDistinctFeature = (Core.FieldName "projectDistinctFeature")

_ProjectionFeatures_projectAllFeature = (Core.FieldName "projectAllFeature")

_ProjectionFeatures_projectAsFeature = (Core.FieldName "projectAsFeature")

_ProjectionFeatures_skipFeature = (Core.FieldName "skipFeature")

_ProjectionFeatures_sortOrderFeature = (Core.FieldName "sortOrderFeature")

-- | A set of features for quantifier expressions.
data QuantifierFeatures = 
  QuantifierFeatures {
    -- | Whether to expect the ALL quantifier.
    quantifierFeaturesAllFeature :: Bool,
    -- | Whether to expect the ANY quantifier.
    quantifierFeaturesAnyFeature :: Bool,
    -- | Whether to expect the NONE quantifier.
    quantifierFeaturesNoneFeature :: Bool,
    -- | Whether to expect the SINGLE quantifier.
    quantifierFeaturesSingleFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_QuantifierFeatures = (Core.Name "hydra/langs/cypher/features.QuantifierFeatures")

_QuantifierFeatures_allFeature = (Core.FieldName "allFeature")

_QuantifierFeatures_anyFeature = (Core.FieldName "anyFeature")

_QuantifierFeatures_noneFeature = (Core.FieldName "noneFeature")

_QuantifierFeatures_singleFeature = (Core.FieldName "singleFeature")

-- | A set of features for range literals within relationship patterns.
data RangeLiteralFeatures = 
  RangeLiteralFeatures {
    -- | Whether to expect the * range literal.
    rangeLiteralFeaturesStarRangeLiteralFeature :: Bool,
    -- | Whether to expect range literals providing an exact number of repetitions.
    rangeLiteralFeaturesExactRangeLiteralFeature :: Bool,
    -- | Whether to expect range literals with a lower bound (only).
    rangeLiteralFeaturesRangeLiteralWithLowerBoundFeature :: Bool,
    -- | Whether to expect range literals with an upper bound (only).
    rangeLiteralFeaturesRangeLiteralWithUpperBoundFeature :: Bool,
    -- | Whether to expect range literals with both lower and upper bounds.
    rangeLiteralFeaturesRangeLiteralWithBoundsFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_RangeLiteralFeatures = (Core.Name "hydra/langs/cypher/features.RangeLiteralFeatures")

_RangeLiteralFeatures_starRangeLiteralFeature = (Core.FieldName "starRangeLiteralFeature")

_RangeLiteralFeatures_exactRangeLiteralFeature = (Core.FieldName "exactRangeLiteralFeature")

_RangeLiteralFeatures_rangeLiteralWithLowerBoundFeature = (Core.FieldName "rangeLiteralWithLowerBoundFeature")

_RangeLiteralFeatures_rangeLiteralWithUpperBoundFeature = (Core.FieldName "rangeLiteralWithUpperBoundFeature")

_RangeLiteralFeatures_rangeLiteralWithBoundsFeature = (Core.FieldName "rangeLiteralWithBoundsFeature")

-- | A set of features for specific syntax related to reading data from the graph..
data ReadingFeatures = 
  ReadingFeatures {
    -- | Whether to expect the UNION operator.
    readingFeaturesUnionFeature :: Bool,
    -- | Whether to expect the UNION ALL operator.
    readingFeaturesUnionAllFeature :: Bool,
    -- | Whether to expect the UNWIND clause.
    readingFeaturesUnwindFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_ReadingFeatures = (Core.Name "hydra/langs/cypher/features.ReadingFeatures")

_ReadingFeatures_unionFeature = (Core.FieldName "unionFeature")

_ReadingFeatures_unionAllFeature = (Core.FieldName "unionAllFeature")

_ReadingFeatures_unwindFeature = (Core.FieldName "unwindFeature")

-- | A set of features for relationship directions / arrow patterns.
data RelationshipDirectionFeatures = 
  RelationshipDirectionFeatures {
    -- | Whether to expect the two-headed arrow (<-[]->) relationship direction.
    relationshipDirectionFeaturesBothArrowFeature :: Bool,
    -- | Whether to expect the left arrow (<-[]-) relationship direction.
    relationshipDirectionFeaturesLeftArrowFeature :: Bool,
    -- | Whether to expect the headless arrow (-[]-) relationship direction.
    relationshipDirectionFeaturesNeitherArrowFeature :: Bool,
    -- | Whether to expect the right arrow (-[]->) relationship direction.
    relationshipDirectionFeaturesRightArrowFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipDirectionFeatures = (Core.Name "hydra/langs/cypher/features.RelationshipDirectionFeatures")

_RelationshipDirectionFeatures_bothArrowFeature = (Core.FieldName "bothArrowFeature")

_RelationshipDirectionFeatures_leftArrowFeature = (Core.FieldName "leftArrowFeature")

_RelationshipDirectionFeatures_neitherArrowFeature = (Core.FieldName "neitherArrowFeature")

_RelationshipDirectionFeatures_rightArrowFeature = (Core.FieldName "rightArrowFeature")

-- | A set of features for relationship patterns.
data RelationshipPatternFeatures = 
  RelationshipPatternFeatures {
    -- | Whether to expect specifying a disjunction of multiple types in a relationship pattern.
    relationshipPatternFeaturesMultipleTypesInRelationshipPatternFeature :: Bool,
    -- | Whether to expect binding a variable to a relationship in a relationship pattern (note: included by most if not all implementations).
    relationshipPatternFeaturesVariableRelationshipFeature :: Bool,
    -- | Whether to expect omitting types from a relationship pattern.
    relationshipPatternFeaturesWildcardTypeInRelationshipPatternFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipPatternFeatures = (Core.Name "hydra/langs/cypher/features.RelationshipPatternFeatures")

_RelationshipPatternFeatures_multipleTypesInRelationshipPatternFeature = (Core.FieldName "multipleTypesInRelationshipPatternFeature")

_RelationshipPatternFeatures_variableRelationshipFeature = (Core.FieldName "variableRelationshipFeature")

_RelationshipPatternFeatures_wildcardTypeInRelationshipPatternFeature = (Core.FieldName "wildcardTypeInRelationshipPatternFeature")

-- | A set of features for REMOVE operations.
data RemoveFeatures = 
  RemoveFeatures {
    -- | Whether to expect REMOVE Variable:NodeLabels.
    removeFeaturesRemoveByLabelFeature :: Bool,
    -- | Whether to expect REMOVE PropertyExpression.
    removeFeaturesRemoveByPropertyFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_RemoveFeatures = (Core.Name "hydra/langs/cypher/features.RemoveFeatures")

_RemoveFeatures_removeByLabelFeature = (Core.FieldName "removeByLabelFeature")

_RemoveFeatures_removeByPropertyFeature = (Core.FieldName "removeByPropertyFeature")

-- | A set of features for set definitions.
data SetFeatures = 
  SetFeatures {
    -- | Whether to expect defining a set using PropertyExpression = Expression.
    setFeaturesSetPropertyEqualsFeature :: Bool,
    -- | Whether to expect defining a set using Variable = Expression.
    setFeaturesSetVariableEqualsFeature :: Bool,
    -- | Whether to expect defining a set using Variable += Expression.
    setFeaturesSetVariablePlusEqualsFeature :: Bool,
    -- | Whether to expect defining a set using Variable:NodeLabels.
    setFeaturesSetVariableWithNodeLabelsFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_SetFeatures = (Core.Name "hydra/langs/cypher/features.SetFeatures")

_SetFeatures_setPropertyEqualsFeature = (Core.FieldName "setPropertyEqualsFeature")

_SetFeatures_setVariableEqualsFeature = (Core.FieldName "setVariableEqualsFeature")

_SetFeatures_setVariablePlusEqualsFeature = (Core.FieldName "setVariablePlusEqualsFeature")

_SetFeatures_setVariableWithNodeLabelsFeature = (Core.FieldName "setVariableWithNodeLabelsFeature")

-- | A set of features for string functions.
data StringFeatures = 
  StringFeatures {
    -- | Whether to expect the CONTAINS function.
    stringFeaturesContainsFeature :: Bool,
    -- | Whether to expect the ENDS WITH function.
    stringFeaturesEndsWithFeature :: Bool,
    -- | Whether to expect the IN function.
    stringFeaturesInFeature :: Bool,
    -- | Whether to expect the STARTS WITH function.
    stringFeaturesStartsWithFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringFeatures = (Core.Name "hydra/langs/cypher/features.StringFeatures")

_StringFeatures_containsFeature = (Core.FieldName "containsFeature")

_StringFeatures_endsWithFeature = (Core.FieldName "endsWithFeature")

_StringFeatures_inFeature = (Core.FieldName "inFeature")

_StringFeatures_startsWithFeature = (Core.FieldName "startsWithFeature")

-- | A set of features for specific syntax related to updating data in the graph.
data UpdatingFeatures = 
  UpdatingFeatures {
    -- | Whether to expect the CREATE clause.
    updatingFeaturesCreateFeature :: Bool,
    -- | Whether to expect the SET clause.
    updatingFeaturesSetFeature :: Bool,
    -- | Whether to expect multi-part queries using WITH.
    updatingFeaturesWithFeature :: Bool}
  deriving (Eq, Ord, Read, Show)

_UpdatingFeatures = (Core.Name "hydra/langs/cypher/features.UpdatingFeatures")

_UpdatingFeatures_createFeature = (Core.FieldName "createFeature")

_UpdatingFeatures_setFeature = (Core.FieldName "setFeature")

_UpdatingFeatures_withFeature = (Core.FieldName "withFeature")