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
    -- | Whether to expect the avg() / AVG aggregate function.
    aggregateFeaturesAvg :: Bool,
    -- | Whether to expect the collect() / COLLECT aggregate function.
    aggregateFeaturesCollect :: Bool,
    -- | Whether to expect the count() / COUNT aggregate function.
    aggregateFeaturesCount :: Bool,
    -- | Whether to expect the max() / MAX aggregate function.
    aggregateFeaturesMax :: Bool,
    -- | Whether to expect the min() / MIN aggregate function.
    aggregateFeaturesMin :: Bool,
    -- | Whether to expect the sum() / SUM aggregate function.
    aggregateFeaturesSum :: Bool}
  deriving (Eq, Ord, Read, Show)

_AggregateFeatures = (Core.Name "hydra/langs/cypher/features.AggregateFeatures")

_AggregateFeatures_avg = (Core.FieldName "avg")

_AggregateFeatures_collect = (Core.FieldName "collect")

_AggregateFeatures_count = (Core.FieldName "count")

_AggregateFeatures_max = (Core.FieldName "max")

_AggregateFeatures_min = (Core.FieldName "min")

_AggregateFeatures_sum = (Core.FieldName "sum")

-- | A set of features for arithmetic operations.
data ArithmeticFeatures = 
  ArithmeticFeatures {
    -- | Whether to expect the + operator.
    arithmeticFeaturesPlus :: Bool,
    -- | Whether to expect the - operator.
    arithmeticFeaturesMinus :: Bool,
    -- | Whether to expect the * operator.
    arithmeticFeaturesMultiply :: Bool,
    -- | Whether to expect the / operator.
    arithmeticFeaturesDivide :: Bool,
    -- | Whether to expect the % operator.
    arithmeticFeaturesModulus :: Bool,
    -- | Whether to expect the ^ operator.
    arithmeticFeaturesPowerOf :: Bool}
  deriving (Eq, Ord, Read, Show)

_ArithmeticFeatures = (Core.Name "hydra/langs/cypher/features.ArithmeticFeatures")

_ArithmeticFeatures_plus = (Core.FieldName "plus")

_ArithmeticFeatures_minus = (Core.FieldName "minus")

_ArithmeticFeatures_multiply = (Core.FieldName "multiply")

_ArithmeticFeatures_divide = (Core.FieldName "divide")

_ArithmeticFeatures_modulus = (Core.FieldName "modulus")

_ArithmeticFeatures_powerOf = (Core.FieldName "powerOf")

-- | A set of features for various kinds of atomic expressions.
data AtomFeatures = 
  AtomFeatures {
    -- | Whether to expect CASE expressions.
    atomFeaturesCaseExpression :: Bool,
    -- | Whether to expect the COUNT (*) expression.
    atomFeaturesCount :: Bool,
    -- | Whether to expect existential subqueries.
    atomFeaturesExistentialSubquery :: Bool,
    -- | Whether to expect function invocation.
    atomFeaturesFunctionInvocation :: Bool,
    -- | Whether to expect list comprehensions, and if so, which specific features
    atomFeaturesListComprehension :: (Maybe ListComprehensionFeatures),
    -- | Whether to expect literal values, and if so, which specific features
    atomFeaturesLiteral :: (Maybe LiteralFeatures),
    -- | Whether to expect parameter expressions.
    atomFeaturesParameter :: Bool,
    -- | Whether to expect pattern comprehensions.
    atomFeaturesPatternComprehension :: Bool,
    -- | Whether to expect relationship patterns as subexpressions.
    atomFeaturesPatternPredicate :: Bool,
    -- | Whether to expect quantifier expressions, and if so, which specific features
    atomFeaturesQuantifier :: (Maybe QuantifierFeatures),
    -- | Whether to expect variable expressions (note: included by most if not all implementations).
    atomFeaturesVariable :: Bool}
  deriving (Eq, Ord, Read, Show)

_AtomFeatures = (Core.Name "hydra/langs/cypher/features.AtomFeatures")

_AtomFeatures_caseExpression = (Core.FieldName "caseExpression")

_AtomFeatures_count = (Core.FieldName "count")

_AtomFeatures_existentialSubquery = (Core.FieldName "existentialSubquery")

_AtomFeatures_functionInvocation = (Core.FieldName "functionInvocation")

_AtomFeatures_listComprehension = (Core.FieldName "listComprehension")

_AtomFeatures_literal = (Core.FieldName "literal")

_AtomFeatures_parameter = (Core.FieldName "parameter")

_AtomFeatures_patternComprehension = (Core.FieldName "patternComprehension")

_AtomFeatures_patternPredicate = (Core.FieldName "patternPredicate")

_AtomFeatures_quantifier = (Core.FieldName "quantifier")

_AtomFeatures_variable = (Core.FieldName "variable")

-- | A set of features for comparison operations.
data ComparisonFeatures = 
  ComparisonFeatures {
    -- | Whether to expect the = comparison operator.
    comparisonFeaturesEqual :: Bool,
    -- | Whether to expect the > comparison operator.
    comparisonFeaturesGreaterThan :: Bool,
    -- | Whether to expect the >= comparison operator.
    comparisonFeaturesGreaterThanOrEqual :: Bool,
    -- | Whether to expect the < comparison operator.
    comparisonFeaturesLessThan :: Bool,
    -- | Whether to expect the <= comparison operator.
    comparisonFeaturesLessThanOrEqual :: Bool,
    -- | Whether to expect the <> comparison operator.
    comparisonFeaturesNotEqual :: Bool}
  deriving (Eq, Ord, Read, Show)

_ComparisonFeatures = (Core.Name "hydra/langs/cypher/features.ComparisonFeatures")

_ComparisonFeatures_equal = (Core.FieldName "equal")

_ComparisonFeatures_greaterThan = (Core.FieldName "greaterThan")

_ComparisonFeatures_greaterThanOrEqual = (Core.FieldName "greaterThanOrEqual")

_ComparisonFeatures_lessThan = (Core.FieldName "lessThan")

_ComparisonFeatures_lessThanOrEqual = (Core.FieldName "lessThanOrEqual")

_ComparisonFeatures_notEqual = (Core.FieldName "notEqual")

-- | A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired.
data CypherFeatures = 
  CypherFeatures {
    -- | Whether to expect aggregate functions, and if so, which specific features
    cypherFeaturesAggregate :: (Maybe AggregateFeatures),
    -- | Whether to expect arithmetic operations, and if so, which specific features
    cypherFeaturesArithmetic :: (Maybe ArithmeticFeatures),
    -- | Whether to expect atomic expressions, and if so, which specific features
    cypherFeaturesAtom :: (Maybe AtomFeatures),
    -- | Whether to expect comparison operations, and if so, which specific features
    cypherFeaturesComparison :: (Maybe ComparisonFeatures),
    -- | Whether to expect delete operations, and if so, which specific features
    cypherFeaturesDelete :: (Maybe DeleteFeatures),
    -- | Whether to expect logical operations, and if so, which specific features
    cypherFeaturesLogical :: (Maybe LogicalFeatures),
    -- | Whether to expect match queries, and if so, which specific features
    cypherFeaturesMatch :: (Maybe MatchFeatures),
    -- | Whether to expect merge operations, and if so, which specific features
    cypherFeaturesMerge :: (Maybe MergeFeatures),
    -- | Whether to expect node patterns, and if so, which specific features
    cypherFeaturesNodePattern :: (Maybe NodePatternFeatures),
    -- | Whether to expect IS NULL / IS NOT NULL checks, and if so, which specific features
    cypherFeaturesNull :: (Maybe NullFeatures),
    -- | Whether to expect path functions, and if so, which specific features
    cypherFeaturesPath :: (Maybe PathFeatures),
    -- | Whether to expect procedure calls, and if so, which specific features
    cypherFeaturesProcedureCall :: (Maybe ProcedureCallFeatures),
    -- | Whether to expect projection operations, and if so, which specific features
    cypherFeaturesProjection :: (Maybe ProjectionFeatures),
    -- | Whether to expect range literals, and if so, which specific features
    cypherFeaturesRangeLiteral :: (Maybe RangeLiteralFeatures),
    -- | Whether to expect reading operations, and if so, which specific features
    cypherFeaturesReading :: (Maybe ReadingFeatures),
    -- | Whether to expect relationship directions, and if so, which specific features
    cypherFeaturesRelationshipDirection :: (Maybe RelationshipDirectionFeatures),
    -- | Whether to expect relationship patterns, and if so, which specific features
    cypherFeaturesRelationshipPattern :: (Maybe RelationshipPatternFeatures),
    -- | Whether to expect remove operations, and if so, which specific features
    cypherFeaturesRemove :: (Maybe RemoveFeatures),
    -- | Whether to expect schema functions, and if so, which specific features
    cypherFeaturesSchema :: (Maybe SchemaFeatures),
    -- | Whether to expect set operations, and if so, which specific features
    cypherFeaturesSet :: (Maybe SetFeatures),
    -- | Whether to expect string operations, and if so, which specific features
    cypherFeaturesString :: (Maybe StringFeatures),
    -- | Whether to expect updating operations, and if so, which specific features
    cypherFeaturesUpdating :: (Maybe UpdatingFeatures)}
  deriving (Eq, Ord, Read, Show)

_CypherFeatures = (Core.Name "hydra/langs/cypher/features.CypherFeatures")

_CypherFeatures_aggregate = (Core.FieldName "aggregate")

_CypherFeatures_arithmetic = (Core.FieldName "arithmetic")

_CypherFeatures_atom = (Core.FieldName "atom")

_CypherFeatures_comparison = (Core.FieldName "comparison")

_CypherFeatures_delete = (Core.FieldName "delete")

_CypherFeatures_logical = (Core.FieldName "logical")

_CypherFeatures_match = (Core.FieldName "match")

_CypherFeatures_merge = (Core.FieldName "merge")

_CypherFeatures_nodePattern = (Core.FieldName "nodePattern")

_CypherFeatures_null = (Core.FieldName "null")

_CypherFeatures_path = (Core.FieldName "path")

_CypherFeatures_procedureCall = (Core.FieldName "procedureCall")

_CypherFeatures_projection = (Core.FieldName "projection")

_CypherFeatures_rangeLiteral = (Core.FieldName "rangeLiteral")

_CypherFeatures_reading = (Core.FieldName "reading")

_CypherFeatures_relationshipDirection = (Core.FieldName "relationshipDirection")

_CypherFeatures_relationshipPattern = (Core.FieldName "relationshipPattern")

_CypherFeatures_remove = (Core.FieldName "remove")

_CypherFeatures_schema = (Core.FieldName "schema")

_CypherFeatures_set = (Core.FieldName "set")

_CypherFeatures_string = (Core.FieldName "string")

_CypherFeatures_updating = (Core.FieldName "updating")

-- | A set of features for delete operations.
data DeleteFeatures = 
  DeleteFeatures {
    -- | Whether to expect the basic DELETE clause.
    deleteFeaturesDelete :: Bool,
    -- | Whether to expect the DETACH DELETE clause.
    deleteFeaturesDetachDelete :: Bool}
  deriving (Eq, Ord, Read, Show)

_DeleteFeatures = (Core.Name "hydra/langs/cypher/features.DeleteFeatures")

_DeleteFeatures_delete = (Core.FieldName "delete")

_DeleteFeatures_detachDelete = (Core.FieldName "detachDelete")

-- | A set of features for list comprehensions.
data ListComprehensionFeatures = 
  ListComprehensionFeatures {
    -- | Whether to expect basic list comprehensions.
    listComprehensionFeaturesListComprehension :: Bool,
    -- | Whether to expect list range comprehensions (e.g. [1..10]).
    listComprehensionFeaturesListRange :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListComprehensionFeatures = (Core.Name "hydra/langs/cypher/features.ListComprehensionFeatures")

_ListComprehensionFeatures_listComprehension = (Core.FieldName "listComprehension")

_ListComprehensionFeatures_listRange = (Core.FieldName "listRange")

-- | A set of features for various types of literal values.
data LiteralFeatures = 
  LiteralFeatures {
    -- | Whether to expect boolean literals (note: included by most if not all implementations).
    literalFeaturesBoolean :: Bool,
    -- | Whether to expect double-precision floating-point literals.
    literalFeaturesDouble :: Bool,
    -- | Whether to expect integer literals.
    literalFeaturesInteger :: Bool,
    -- | Whether to expect list literals.
    literalFeaturesList :: Bool,
    -- | Whether to expect map literals.
    literalFeaturesMap :: Bool,
    -- | Whether to expect the NULL literal.
    literalFeaturesNull :: Bool,
    -- | Whether to expect string literals (note: included by most if not all implementations).
    literalFeaturesString :: Bool}
  deriving (Eq, Ord, Read, Show)

_LiteralFeatures = (Core.Name "hydra/langs/cypher/features.LiteralFeatures")

_LiteralFeatures_boolean = (Core.FieldName "boolean")

_LiteralFeatures_double = (Core.FieldName "double")

_LiteralFeatures_integer = (Core.FieldName "integer")

_LiteralFeatures_list = (Core.FieldName "list")

_LiteralFeatures_map = (Core.FieldName "map")

_LiteralFeatures_null = (Core.FieldName "null")

_LiteralFeatures_string = (Core.FieldName "string")

-- | A set of features for logical operations.
data LogicalFeatures = 
  LogicalFeatures {
    -- | Whether to expect the AND operator.
    logicalFeaturesAnd :: Bool,
    -- | Whether to expect the NOT operator.
    logicalFeaturesNot :: Bool,
    -- | Whether to expect the OR operator.
    logicalFeaturesOr :: Bool,
    -- | Whether to expect the XOR operator.
    logicalFeaturesXor :: Bool}
  deriving (Eq, Ord, Read, Show)

_LogicalFeatures = (Core.Name "hydra/langs/cypher/features.LogicalFeatures")

_LogicalFeatures_and = (Core.FieldName "and")

_LogicalFeatures_not = (Core.FieldName "not")

_LogicalFeatures_or = (Core.FieldName "or")

_LogicalFeatures_xor = (Core.FieldName "xor")

-- | A set of features for match queries.
data MatchFeatures = 
  MatchFeatures {
    -- | Whether to expect OPTIONAL MATCH.
    matchFeaturesOptional :: Bool}
  deriving (Eq, Ord, Read, Show)

_MatchFeatures = (Core.Name "hydra/langs/cypher/features.MatchFeatures")

_MatchFeatures_optional = (Core.FieldName "optional")

-- | A set of features for merge operations.
data MergeFeatures = 
  MergeFeatures {
    -- | Whether to expect the basic MERGE clause.
    mergeFeaturesMerge :: Bool,
    -- | Whether to expect MERGE with the ON CREATE action.
    mergeFeaturesMergeOnCreate :: Bool,
    -- | Whether to expect MERGE with the ON MATCH action.
    mergeFeaturesMergeOnMatch :: Bool}
  deriving (Eq, Ord, Read, Show)

_MergeFeatures = (Core.Name "hydra/langs/cypher/features.MergeFeatures")

_MergeFeatures_merge = (Core.FieldName "merge")

_MergeFeatures_mergeOnCreate = (Core.FieldName "mergeOnCreate")

_MergeFeatures_mergeOnMatch = (Core.FieldName "mergeOnMatch")

-- | A set of features for node patterns.
data NodePatternFeatures = 
  NodePatternFeatures {
    -- | Whether to expect specifying multiple labels in a node pattern.
    nodePatternFeaturesMultipleLabels :: Bool,
    -- | Whether to expect specifying a parameter as part of a node pattern.
    nodePatternFeaturesParameter :: Bool,
    -- | Whether to expect specifying a key/value map of properties in a node pattern.
    nodePatternFeaturesPropertyMap :: Bool,
    -- | Whether to expect binding a variable to a node in a node pattern (note: included by most if not all implementations).
    nodePatternFeaturesVariableNode :: Bool,
    -- | Whether to expect omitting labels from a node pattern.
    nodePatternFeaturesWildcardLabel :: Bool}
  deriving (Eq, Ord, Read, Show)

_NodePatternFeatures = (Core.Name "hydra/langs/cypher/features.NodePatternFeatures")

_NodePatternFeatures_multipleLabels = (Core.FieldName "multipleLabels")

_NodePatternFeatures_parameter = (Core.FieldName "parameter")

_NodePatternFeatures_propertyMap = (Core.FieldName "propertyMap")

_NodePatternFeatures_variableNode = (Core.FieldName "variableNode")

_NodePatternFeatures_wildcardLabel = (Core.FieldName "wildcardLabel")

-- | A set of features for IS NULL / IS NOT NULL checks.
data NullFeatures = 
  NullFeatures {
    -- | Whether to expect the IS NULL operator.
    nullFeaturesIsNull :: Bool,
    -- | Whether to expect the IS NOT NULL operator.
    nullFeaturesIsNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullFeatures = (Core.Name "hydra/langs/cypher/features.NullFeatures")

_NullFeatures_isNull = (Core.FieldName "isNull")

_NullFeatures_isNotNull = (Core.FieldName "isNotNull")

-- | A set of features for path functions.
data PathFeatures = 
  PathFeatures {
    -- | Whether to expect the length() function.
    pathFeaturesLength :: Bool,
    -- | Whether to expect the shortestPath() function.
    pathFeaturesShortestPath :: Bool}
  deriving (Eq, Ord, Read, Show)

_PathFeatures = (Core.Name "hydra/langs/cypher/features.PathFeatures")

_PathFeatures_length = (Core.FieldName "length")

_PathFeatures_shortestPath = (Core.FieldName "shortestPath")

-- | A set of features for procedure calls.
data ProcedureCallFeatures = 
  ProcedureCallFeatures {
    -- | Whether to expect CALL within a query.
    procedureCallFeaturesInQueryCall :: Bool,
    -- | Whether to expect standalone / top-level CALL.
    procedureCallFeaturesStandaloneCall :: Bool,
    -- | Whether to expect the YIELD clause in CALL.
    procedureCallFeaturesYield :: Bool}
  deriving (Eq, Ord, Read, Show)

_ProcedureCallFeatures = (Core.Name "hydra/langs/cypher/features.ProcedureCallFeatures")

_ProcedureCallFeatures_inQueryCall = (Core.FieldName "inQueryCall")

_ProcedureCallFeatures_standaloneCall = (Core.FieldName "standaloneCall")

_ProcedureCallFeatures_yield = (Core.FieldName "yield")

-- | A set of features for projections.
data ProjectionFeatures = 
  ProjectionFeatures {
    -- | Whether to expect the LIMIT clause.
    projectionFeaturesLimit :: Bool,
    -- | Whether to expect the ORDER BY clause.
    projectionFeaturesOrderBy :: Bool,
    -- | Whether to expect the DISTINCT keyword.
    projectionFeaturesProjectDistinct :: Bool,
    -- | Whether to expect the * projection.
    projectionFeaturesProjectAll :: Bool,
    -- | Whether to expect the AS keyword.
    projectionFeaturesProjectAs :: Bool,
    -- | Whether to expect the SKIP clause.
    projectionFeaturesSkip :: Bool,
    -- | Whether to expect the ASC/ASCENDING and DESC/DESCENDING keywords.
    projectionFeaturesSortOrder :: Bool}
  deriving (Eq, Ord, Read, Show)

_ProjectionFeatures = (Core.Name "hydra/langs/cypher/features.ProjectionFeatures")

_ProjectionFeatures_limit = (Core.FieldName "limit")

_ProjectionFeatures_orderBy = (Core.FieldName "orderBy")

_ProjectionFeatures_projectDistinct = (Core.FieldName "projectDistinct")

_ProjectionFeatures_projectAll = (Core.FieldName "projectAll")

_ProjectionFeatures_projectAs = (Core.FieldName "projectAs")

_ProjectionFeatures_skip = (Core.FieldName "skip")

_ProjectionFeatures_sortOrder = (Core.FieldName "sortOrder")

-- | A set of features for quantifier expressions.
data QuantifierFeatures = 
  QuantifierFeatures {
    -- | Whether to expect the ALL quantifier.
    quantifierFeaturesAll :: Bool,
    -- | Whether to expect the ANY quantifier.
    quantifierFeaturesAny :: Bool,
    -- | Whether to expect the NONE quantifier.
    quantifierFeaturesNone :: Bool,
    -- | Whether to expect the SINGLE quantifier.
    quantifierFeaturesSingle :: Bool}
  deriving (Eq, Ord, Read, Show)

_QuantifierFeatures = (Core.Name "hydra/langs/cypher/features.QuantifierFeatures")

_QuantifierFeatures_all = (Core.FieldName "all")

_QuantifierFeatures_any = (Core.FieldName "any")

_QuantifierFeatures_none = (Core.FieldName "none")

_QuantifierFeatures_single = (Core.FieldName "single")

-- | A set of features for range literals within relationship patterns.
data RangeLiteralFeatures = 
  RangeLiteralFeatures {
    -- | Whether to expect range literals with both lower and upper bounds.
    rangeLiteralFeaturesBounds :: Bool,
    -- | Whether to expect range literals providing an exact number of repetitions.
    rangeLiteralFeaturesExactRange :: Bool,
    -- | Whether to expect range literals with a lower bound (only).
    rangeLiteralFeaturesLowerBound :: Bool,
    -- | Whether to expect the * range literal.
    rangeLiteralFeaturesStarRange :: Bool,
    -- | Whether to expect range literals with an upper bound (only).
    rangeLiteralFeaturesUpperBound :: Bool}
  deriving (Eq, Ord, Read, Show)

_RangeLiteralFeatures = (Core.Name "hydra/langs/cypher/features.RangeLiteralFeatures")

_RangeLiteralFeatures_bounds = (Core.FieldName "bounds")

_RangeLiteralFeatures_exactRange = (Core.FieldName "exactRange")

_RangeLiteralFeatures_lowerBound = (Core.FieldName "lowerBound")

_RangeLiteralFeatures_starRange = (Core.FieldName "starRange")

_RangeLiteralFeatures_upperBound = (Core.FieldName "upperBound")

-- | A set of features for specific syntax related to reading data from the graph..
data ReadingFeatures = 
  ReadingFeatures {
    -- | Whether to expect the UNION operator.
    readingFeaturesUnion :: Bool,
    -- | Whether to expect the UNION ALL operator.
    readingFeaturesUnionAll :: Bool,
    -- | Whether to expect the UNWIND clause.
    readingFeaturesUnwind :: Bool}
  deriving (Eq, Ord, Read, Show)

_ReadingFeatures = (Core.Name "hydra/langs/cypher/features.ReadingFeatures")

_ReadingFeatures_union = (Core.FieldName "union")

_ReadingFeatures_unionAll = (Core.FieldName "unionAll")

_ReadingFeatures_unwind = (Core.FieldName "unwind")

-- | A set of features for relationship directions / arrow patterns.
data RelationshipDirectionFeatures = 
  RelationshipDirectionFeatures {
    -- | Whether to expect the two-headed arrow (<-[]->) relationship direction.
    relationshipDirectionFeaturesBoth :: Bool,
    -- | Whether to expect the left arrow (<-[]-) relationship direction.
    relationshipDirectionFeaturesLeft :: Bool,
    -- | Whether to expect the headless arrow (-[]-) relationship direction.
    relationshipDirectionFeaturesNeither :: Bool,
    -- | Whether to expect the right arrow (-[]->) relationship direction.
    relationshipDirectionFeaturesRight :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipDirectionFeatures = (Core.Name "hydra/langs/cypher/features.RelationshipDirectionFeatures")

_RelationshipDirectionFeatures_both = (Core.FieldName "both")

_RelationshipDirectionFeatures_left = (Core.FieldName "left")

_RelationshipDirectionFeatures_neither = (Core.FieldName "neither")

_RelationshipDirectionFeatures_right = (Core.FieldName "right")

-- | A set of features for relationship patterns.
data RelationshipPatternFeatures = 
  RelationshipPatternFeatures {
    -- | Whether to expect specifying a disjunction of multiple types in a relationship pattern.
    relationshipPatternFeaturesMultipleTypes :: Bool,
    -- | Whether to expect binding a variable to a relationship in a relationship pattern (note: included by most if not all implementations).
    relationshipPatternFeaturesVariableRelationship :: Bool,
    -- | Whether to expect omitting types from a relationship pattern.
    relationshipPatternFeaturesWildcardType :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipPatternFeatures = (Core.Name "hydra/langs/cypher/features.RelationshipPatternFeatures")

_RelationshipPatternFeatures_multipleTypes = (Core.FieldName "multipleTypes")

_RelationshipPatternFeatures_variableRelationship = (Core.FieldName "variableRelationship")

_RelationshipPatternFeatures_wildcardType = (Core.FieldName "wildcardType")

-- | A set of features for REMOVE operations.
data RemoveFeatures = 
  RemoveFeatures {
    -- | Whether to expect REMOVE Variable:NodeLabels.
    removeFeaturesByLabel :: Bool,
    -- | Whether to expect REMOVE PropertyExpression.
    removeFeaturesByProperty :: Bool}
  deriving (Eq, Ord, Read, Show)

_RemoveFeatures = (Core.Name "hydra/langs/cypher/features.RemoveFeatures")

_RemoveFeatures_byLabel = (Core.FieldName "byLabel")

_RemoveFeatures_byProperty = (Core.FieldName "byProperty")

-- | A set of features for schema functions.
data SchemaFeatures = 
  SchemaFeatures {
    -- | Whether to expect the type() function.
    schemaFeaturesType :: Bool}
  deriving (Eq, Ord, Read, Show)

_SchemaFeatures = (Core.Name "hydra/langs/cypher/features.SchemaFeatures")

_SchemaFeatures_type = (Core.FieldName "type")

-- | A set of features for set definitions.
data SetFeatures = 
  SetFeatures {
    -- | Whether to expect defining a set using PropertyExpression = Expression.
    setFeaturesPropertyEquals :: Bool,
    -- | Whether to expect defining a set using Variable = Expression.
    setFeaturesVariableEquals :: Bool,
    -- | Whether to expect defining a set using Variable += Expression.
    setFeaturesVariablePlusEquals :: Bool,
    -- | Whether to expect defining a set using Variable:NodeLabels.
    setFeaturesVariableWithNodeLabels :: Bool}
  deriving (Eq, Ord, Read, Show)

_SetFeatures = (Core.Name "hydra/langs/cypher/features.SetFeatures")

_SetFeatures_propertyEquals = (Core.FieldName "propertyEquals")

_SetFeatures_variableEquals = (Core.FieldName "variableEquals")

_SetFeatures_variablePlusEquals = (Core.FieldName "variablePlusEquals")

_SetFeatures_variableWithNodeLabels = (Core.FieldName "variableWithNodeLabels")

-- | A set of features for string functions.
data StringFeatures = 
  StringFeatures {
    -- | Whether to expect the contains() / CONTAINS aggregate function.
    stringFeaturesContains :: Bool,
    -- | Whether to expect the endsWith() / ENDS WITH aggregate function.
    stringFeaturesEndsWith :: Bool,
    -- | Whether to expect the in() / IN aggregate function.
    stringFeaturesIn :: Bool,
    -- | Whether to expect the startsWith() / STARTS WITH aggregate function.
    stringFeaturesStartsWith :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringFeatures = (Core.Name "hydra/langs/cypher/features.StringFeatures")

_StringFeatures_contains = (Core.FieldName "contains")

_StringFeatures_endsWith = (Core.FieldName "endsWith")

_StringFeatures_in = (Core.FieldName "in")

_StringFeatures_startsWith = (Core.FieldName "startsWith")

-- | A set of features for specific syntax related to updating data in the graph.
data UpdatingFeatures = 
  UpdatingFeatures {
    -- | Whether to expect the CREATE clause.
    updatingFeaturesCreate :: Bool,
    -- | Whether to expect the SET clause.
    updatingFeaturesSet :: Bool,
    -- | Whether to expect multi-part queries using WITH.
    updatingFeaturesWith :: Bool}
  deriving (Eq, Ord, Read, Show)

_UpdatingFeatures = (Core.Name "hydra/langs/cypher/features.UpdatingFeatures")

_UpdatingFeatures_create = (Core.FieldName "create")

_UpdatingFeatures_set = (Core.FieldName "set")

_UpdatingFeatures_with = (Core.FieldName "with")