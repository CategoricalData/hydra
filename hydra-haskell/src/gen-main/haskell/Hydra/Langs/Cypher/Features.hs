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
    -- | Whether to expect the percentileCont() function.
    aggregateFeaturesPercentileCont :: Bool,
    -- | Whether to expect the percentileDisc() function.
    aggregateFeaturesPercentileDisc :: Bool,
    -- | Whether to expect the stdev() function.
    aggregateFeaturesStdev :: Bool,
    -- | Whether to expect the sum() / SUM aggregate function.
    aggregateFeaturesSum :: Bool}
  deriving (Eq, Ord, Read, Show)

_AggregateFeatures = (Core.Name "hydra/langs/cypher/features.AggregateFeatures")

_AggregateFeatures_avg = (Core.Name "avg")

_AggregateFeatures_collect = (Core.Name "collect")

_AggregateFeatures_count = (Core.Name "count")

_AggregateFeatures_max = (Core.Name "max")

_AggregateFeatures_min = (Core.Name "min")

_AggregateFeatures_percentileCont = (Core.Name "percentileCont")

_AggregateFeatures_percentileDisc = (Core.Name "percentileDisc")

_AggregateFeatures_stdev = (Core.Name "stdev")

_AggregateFeatures_sum = (Core.Name "sum")

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

_ArithmeticFeatures_plus = (Core.Name "plus")

_ArithmeticFeatures_minus = (Core.Name "minus")

_ArithmeticFeatures_multiply = (Core.Name "multiply")

_ArithmeticFeatures_divide = (Core.Name "divide")

_ArithmeticFeatures_modulus = (Core.Name "modulus")

_ArithmeticFeatures_powerOf = (Core.Name "powerOf")

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
    -- | Whether to expect lists, and if so, which specific features
    atomFeaturesList :: (Maybe ListFeatures),
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

_AtomFeatures_caseExpression = (Core.Name "caseExpression")

_AtomFeatures_count = (Core.Name "count")

_AtomFeatures_existentialSubquery = (Core.Name "existentialSubquery")

_AtomFeatures_functionInvocation = (Core.Name "functionInvocation")

_AtomFeatures_list = (Core.Name "list")

_AtomFeatures_literal = (Core.Name "literal")

_AtomFeatures_parameter = (Core.Name "parameter")

_AtomFeatures_patternComprehension = (Core.Name "patternComprehension")

_AtomFeatures_patternPredicate = (Core.Name "patternPredicate")

_AtomFeatures_quantifier = (Core.Name "quantifier")

_AtomFeatures_variable = (Core.Name "variable")

-- | A set of features for comparison operators and functions.
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
    comparisonFeaturesNotEqual :: Bool,
    -- | Whether to expect the nullIf() function.
    comparisonFeaturesNullIf :: Bool}
  deriving (Eq, Ord, Read, Show)

_ComparisonFeatures = (Core.Name "hydra/langs/cypher/features.ComparisonFeatures")

_ComparisonFeatures_equal = (Core.Name "equal")

_ComparisonFeatures_greaterThan = (Core.Name "greaterThan")

_ComparisonFeatures_greaterThanOrEqual = (Core.Name "greaterThanOrEqual")

_ComparisonFeatures_lessThan = (Core.Name "lessThan")

_ComparisonFeatures_lessThanOrEqual = (Core.Name "lessThanOrEqual")

_ComparisonFeatures_notEqual = (Core.Name "notEqual")

_ComparisonFeatures_nullIf = (Core.Name "nullIf")

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
    -- | Whether to expect element functions, and if so, which specific features
    cypherFeaturesElement :: (Maybe ElementFeatures),
    -- | Whether to expect logical operations, and if so, which specific features
    cypherFeaturesLogical :: (Maybe LogicalFeatures),
    -- | Whether to expect property map functions, and if so, which specific features
    cypherFeaturesMap :: (Maybe MapFeatures),
    -- | Whether to expect match queries, and if so, which specific features
    cypherFeaturesMatch :: (Maybe MatchFeatures),
    -- | Whether to expect merge operations, and if so, which specific features
    cypherFeaturesMerge :: (Maybe MergeFeatures),
    -- | Whether to expect node patterns, and if so, which specific features
    cypherFeaturesNodePattern :: (Maybe NodePatternFeatures),
    -- | Whether to expect IS NULL / IS NOT NULL checks, and if so, which specific features
    cypherFeaturesNull :: (Maybe NullFeatures),
    -- | Whether to expect numeric functions, and if so, which specific features
    cypherFeaturesNumeric :: (Maybe NumericFeatures),
    -- | Whether to expect path functions, and if so, which specific features
    cypherFeaturesPath :: (Maybe PathFeatures),
    -- | Whether to expect procedure calls, and if so, which specific features
    cypherFeaturesProcedureCall :: (Maybe ProcedureCallFeatures),
    -- | Whether to expect projection operations, and if so, which specific features
    cypherFeaturesProjection :: (Maybe ProjectionFeatures),
    -- | Whether to expect random value generation, and if so, which specific features
    cypherFeaturesRandomness :: (Maybe RandomnessFeatures),
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

_CypherFeatures_aggregate = (Core.Name "aggregate")

_CypherFeatures_arithmetic = (Core.Name "arithmetic")

_CypherFeatures_atom = (Core.Name "atom")

_CypherFeatures_comparison = (Core.Name "comparison")

_CypherFeatures_delete = (Core.Name "delete")

_CypherFeatures_element = (Core.Name "element")

_CypherFeatures_logical = (Core.Name "logical")

_CypherFeatures_map = (Core.Name "map")

_CypherFeatures_match = (Core.Name "match")

_CypherFeatures_merge = (Core.Name "merge")

_CypherFeatures_nodePattern = (Core.Name "nodePattern")

_CypherFeatures_null = (Core.Name "null")

_CypherFeatures_numeric = (Core.Name "numeric")

_CypherFeatures_path = (Core.Name "path")

_CypherFeatures_procedureCall = (Core.Name "procedureCall")

_CypherFeatures_projection = (Core.Name "projection")

_CypherFeatures_randomness = (Core.Name "randomness")

_CypherFeatures_rangeLiteral = (Core.Name "rangeLiteral")

_CypherFeatures_reading = (Core.Name "reading")

_CypherFeatures_relationshipDirection = (Core.Name "relationshipDirection")

_CypherFeatures_relationshipPattern = (Core.Name "relationshipPattern")

_CypherFeatures_remove = (Core.Name "remove")

_CypherFeatures_schema = (Core.Name "schema")

_CypherFeatures_set = (Core.Name "set")

_CypherFeatures_string = (Core.Name "string")

_CypherFeatures_updating = (Core.Name "updating")

-- | A set of features for delete operations.
data DeleteFeatures = 
  DeleteFeatures {
    -- | Whether to expect the basic DELETE clause.
    deleteFeaturesDelete :: Bool,
    -- | Whether to expect the DETACH DELETE clause.
    deleteFeaturesDetachDelete :: Bool}
  deriving (Eq, Ord, Read, Show)

_DeleteFeatures = (Core.Name "hydra/langs/cypher/features.DeleteFeatures")

_DeleteFeatures_delete = (Core.Name "delete")

_DeleteFeatures_detachDelete = (Core.Name "detachDelete")

-- | A set of features for element functions.
data ElementFeatures = 
  ElementFeatures {
    -- | Whether to expect the elementId() function.
    elementFeaturesElementId :: Bool,
    -- | Whether to expect the endNode() function.
    elementFeaturesEndNode :: Bool,
    -- | Whether to expect the labels() function.
    elementFeaturesLabels :: Bool,
    -- | Whether to expect the properties() function.
    elementFeaturesProperties :: Bool,
    -- | Whether to expect the startNode() function.
    elementFeaturesStartNode :: Bool}
  deriving (Eq, Ord, Read, Show)

_ElementFeatures = (Core.Name "hydra/langs/cypher/features.ElementFeatures")

_ElementFeatures_elementId = (Core.Name "elementId")

_ElementFeatures_endNode = (Core.Name "endNode")

_ElementFeatures_labels = (Core.Name "labels")

_ElementFeatures_properties = (Core.Name "properties")

_ElementFeatures_startNode = (Core.Name "startNode")

-- | A set of features for list functionality.
data ListFeatures = 
  ListFeatures {
    -- | Whether to expect the all() function.
    listFeaturesAll :: Bool,
    -- | Whether to expect the any() function.
    listFeaturesAny :: Bool,
    -- | Whether to expect the coalesce() function.
    listFeaturesCoalesce :: Bool,
    -- | Whether to expect the isEmpty() function.
    listFeaturesIsEmpty :: Bool,
    -- | Whether to expect the head() function.
    listFeaturesHead :: Bool,
    -- | Whether to expect the last() function.
    listFeaturesLast :: Bool,
    -- | Whether to expect basic list comprehensions.
    listFeaturesListComprehension :: Bool,
    -- | Whether to expect list range comprehensions (e.g. [1..10]).
    listFeaturesListRange :: Bool,
    -- | Whether to expect the none() function.
    listFeaturesNone :: Bool,
    -- | Whether to expect the reduce() function.
    listFeaturesReduce :: Bool,
    -- | Whether to expect the reverse() function.
    listFeaturesReverse :: Bool,
    -- | Whether to expect the single() function.
    listFeaturesSingle :: Bool,
    -- | Whether to expect the size() function.
    listFeaturesSize :: Bool,
    -- | Whether to expect the tail() function.
    listFeaturesTail :: Bool,
    -- | Whether to expect the toBooleanList() function.
    listFeaturesToBooleanList :: Bool,
    -- | Whether to expect the toFloatList() function.
    listFeaturesToFloatList :: Bool,
    -- | Whether to expect the toIntegerList() function.
    listFeaturesToIntegerList :: Bool,
    -- | Whether to expect the toStringList() function.
    listFeaturesToStringList :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListFeatures = (Core.Name "hydra/langs/cypher/features.ListFeatures")

_ListFeatures_all = (Core.Name "all")

_ListFeatures_any = (Core.Name "any")

_ListFeatures_coalesce = (Core.Name "coalesce")

_ListFeatures_isEmpty = (Core.Name "isEmpty")

_ListFeatures_head = (Core.Name "head")

_ListFeatures_last = (Core.Name "last")

_ListFeatures_listComprehension = (Core.Name "listComprehension")

_ListFeatures_listRange = (Core.Name "listRange")

_ListFeatures_none = (Core.Name "none")

_ListFeatures_reduce = (Core.Name "reduce")

_ListFeatures_reverse = (Core.Name "reverse")

_ListFeatures_single = (Core.Name "single")

_ListFeatures_size = (Core.Name "size")

_ListFeatures_tail = (Core.Name "tail")

_ListFeatures_toBooleanList = (Core.Name "toBooleanList")

_ListFeatures_toFloatList = (Core.Name "toFloatList")

_ListFeatures_toIntegerList = (Core.Name "toIntegerList")

_ListFeatures_toStringList = (Core.Name "toStringList")

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

_LiteralFeatures_boolean = (Core.Name "boolean")

_LiteralFeatures_double = (Core.Name "double")

_LiteralFeatures_integer = (Core.Name "integer")

_LiteralFeatures_list = (Core.Name "list")

_LiteralFeatures_map = (Core.Name "map")

_LiteralFeatures_null = (Core.Name "null")

_LiteralFeatures_string = (Core.Name "string")

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

_LogicalFeatures_and = (Core.Name "and")

_LogicalFeatures_not = (Core.Name "not")

_LogicalFeatures_or = (Core.Name "or")

_LogicalFeatures_xor = (Core.Name "xor")

-- | A set of features for property map functions.
data MapFeatures = 
  MapFeatures {
    -- | Whether to expect the keys() function.
    mapFeaturesKeys :: Bool}
  deriving (Eq, Ord, Read, Show)

_MapFeatures = (Core.Name "hydra/langs/cypher/features.MapFeatures")

_MapFeatures_keys = (Core.Name "keys")

-- | A set of features for match queries.
data MatchFeatures = 
  MatchFeatures {
    -- | Whether to expect the basic (non-optional) MATCH clause.
    matchFeaturesMatch :: Bool,
    -- | Whether to expect OPTIONAL MATCH.
    matchFeaturesOptionalMatch :: Bool}
  deriving (Eq, Ord, Read, Show)

_MatchFeatures = (Core.Name "hydra/langs/cypher/features.MatchFeatures")

_MatchFeatures_match = (Core.Name "match")

_MatchFeatures_optionalMatch = (Core.Name "optionalMatch")

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

_MergeFeatures_merge = (Core.Name "merge")

_MergeFeatures_mergeOnCreate = (Core.Name "mergeOnCreate")

_MergeFeatures_mergeOnMatch = (Core.Name "mergeOnMatch")

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

_NodePatternFeatures_multipleLabels = (Core.Name "multipleLabels")

_NodePatternFeatures_parameter = (Core.Name "parameter")

_NodePatternFeatures_propertyMap = (Core.Name "propertyMap")

_NodePatternFeatures_variableNode = (Core.Name "variableNode")

_NodePatternFeatures_wildcardLabel = (Core.Name "wildcardLabel")

-- | A set of features for IS NULL / IS NOT NULL checks.
data NullFeatures = 
  NullFeatures {
    -- | Whether to expect the IS NULL operator.
    nullFeaturesIsNull :: Bool,
    -- | Whether to expect the IS NOT NULL operator.
    nullFeaturesIsNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullFeatures = (Core.Name "hydra/langs/cypher/features.NullFeatures")

_NullFeatures_isNull = (Core.Name "isNull")

_NullFeatures_isNotNull = (Core.Name "isNotNull")

-- | A set of features for numeric functions.
data NumericFeatures = 
  NumericFeatures {
    -- | Whether to expect the abs() function.
    numericFeaturesAbs :: Bool,
    -- | Whether to expect the ceil() function.
    numericFeaturesCeil :: Bool,
    -- | Whether to expect the e() function.
    numericFeaturesE :: Bool,
    -- | Whether to expect the exp() function.
    numericFeaturesExp :: Bool,
    -- | Whether to expect the floor() function.
    numericFeaturesFloor :: Bool,
    -- | Whether to expect the isNaN() function.
    numericFeaturesIsNaN :: Bool,
    -- | Whether to expect the log() function.
    numericFeaturesLog :: Bool,
    -- | Whether to expect the log10() function.
    numericFeaturesLog10 :: Bool,
    -- | Whether to expect the range() function.
    numericFeaturesRange :: Bool,
    -- | Whether to expect the round() function.
    numericFeaturesRound :: Bool,
    -- | Whether to expect the sign() function.
    numericFeaturesSign :: Bool,
    -- | Whether to expect the sqrt() function.
    numericFeaturesSqrt :: Bool}
  deriving (Eq, Ord, Read, Show)

_NumericFeatures = (Core.Name "hydra/langs/cypher/features.NumericFeatures")

_NumericFeatures_abs = (Core.Name "abs")

_NumericFeatures_ceil = (Core.Name "ceil")

_NumericFeatures_e = (Core.Name "e")

_NumericFeatures_exp = (Core.Name "exp")

_NumericFeatures_floor = (Core.Name "floor")

_NumericFeatures_isNaN = (Core.Name "isNaN")

_NumericFeatures_log = (Core.Name "log")

_NumericFeatures_log10 = (Core.Name "log10")

_NumericFeatures_range = (Core.Name "range")

_NumericFeatures_round = (Core.Name "round")

_NumericFeatures_sign = (Core.Name "sign")

_NumericFeatures_sqrt = (Core.Name "sqrt")

-- | A set of features for path functions.
data PathFeatures = 
  PathFeatures {
    -- | Whether to expect the length() function.
    pathFeaturesLength :: Bool,
    -- | Whether to expect the nodes() function.
    pathFeaturesNodes :: Bool,
    -- | Whether to expect the relationships() function.
    pathFeaturesRelationships :: Bool,
    -- | Whether to expect the shortestPath() function.
    pathFeaturesShortestPath :: Bool}
  deriving (Eq, Ord, Read, Show)

_PathFeatures = (Core.Name "hydra/langs/cypher/features.PathFeatures")

_PathFeatures_length = (Core.Name "length")

_PathFeatures_nodes = (Core.Name "nodes")

_PathFeatures_relationships = (Core.Name "relationships")

_PathFeatures_shortestPath = (Core.Name "shortestPath")

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

_ProcedureCallFeatures_inQueryCall = (Core.Name "inQueryCall")

_ProcedureCallFeatures_standaloneCall = (Core.Name "standaloneCall")

_ProcedureCallFeatures_yield = (Core.Name "yield")

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

_ProjectionFeatures_limit = (Core.Name "limit")

_ProjectionFeatures_orderBy = (Core.Name "orderBy")

_ProjectionFeatures_projectDistinct = (Core.Name "projectDistinct")

_ProjectionFeatures_projectAll = (Core.Name "projectAll")

_ProjectionFeatures_projectAs = (Core.Name "projectAs")

_ProjectionFeatures_skip = (Core.Name "skip")

_ProjectionFeatures_sortOrder = (Core.Name "sortOrder")

-- | A set of features for quantifier expressions.
data QuantifierFeatures = 
  QuantifierFeatures {
    -- | Whether to expect the ALL quantifier.
    quantifierFeaturesAll :: Bool,
    -- | Whether to expect the ANY quantifier.
    quantifierFeaturesAny :: Bool,
    -- | Whether to expect the exists() function.
    quantifierFeaturesExists :: Bool,
    -- | Whether to expect the NONE quantifier.
    quantifierFeaturesNone :: Bool,
    -- | Whether to expect the SINGLE quantifier.
    quantifierFeaturesSingle :: Bool}
  deriving (Eq, Ord, Read, Show)

_QuantifierFeatures = (Core.Name "hydra/langs/cypher/features.QuantifierFeatures")

_QuantifierFeatures_all = (Core.Name "all")

_QuantifierFeatures_any = (Core.Name "any")

_QuantifierFeatures_exists = (Core.Name "exists")

_QuantifierFeatures_none = (Core.Name "none")

_QuantifierFeatures_single = (Core.Name "single")

-- | A set of features for random value generation.
data RandomnessFeatures = 
  RandomnessFeatures {
    -- | Whether to expect the rand() function.
    randomnessFeaturesRand :: Bool,
    -- | Whether to expect the randomUUID() function.
    randomnessFeaturesRandomUUID :: Bool}
  deriving (Eq, Ord, Read, Show)

_RandomnessFeatures = (Core.Name "hydra/langs/cypher/features.RandomnessFeatures")

_RandomnessFeatures_rand = (Core.Name "rand")

_RandomnessFeatures_randomUUID = (Core.Name "randomUUID")

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

_RangeLiteralFeatures_bounds = (Core.Name "bounds")

_RangeLiteralFeatures_exactRange = (Core.Name "exactRange")

_RangeLiteralFeatures_lowerBound = (Core.Name "lowerBound")

_RangeLiteralFeatures_starRange = (Core.Name "starRange")

_RangeLiteralFeatures_upperBound = (Core.Name "upperBound")

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

_ReadingFeatures_union = (Core.Name "union")

_ReadingFeatures_unionAll = (Core.Name "unionAll")

_ReadingFeatures_unwind = (Core.Name "unwind")

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

_RelationshipDirectionFeatures_both = (Core.Name "both")

_RelationshipDirectionFeatures_left = (Core.Name "left")

_RelationshipDirectionFeatures_neither = (Core.Name "neither")

_RelationshipDirectionFeatures_right = (Core.Name "right")

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

_RelationshipPatternFeatures_multipleTypes = (Core.Name "multipleTypes")

_RelationshipPatternFeatures_variableRelationship = (Core.Name "variableRelationship")

_RelationshipPatternFeatures_wildcardType = (Core.Name "wildcardType")

-- | A set of features for REMOVE operations.
data RemoveFeatures = 
  RemoveFeatures {
    -- | Whether to expect REMOVE Variable:NodeLabels.
    removeFeaturesByLabel :: Bool,
    -- | Whether to expect REMOVE PropertyExpression.
    removeFeaturesByProperty :: Bool}
  deriving (Eq, Ord, Read, Show)

_RemoveFeatures = (Core.Name "hydra/langs/cypher/features.RemoveFeatures")

_RemoveFeatures_byLabel = (Core.Name "byLabel")

_RemoveFeatures_byProperty = (Core.Name "byProperty")

-- | A set of features for schema functions.
data SchemaFeatures = 
  SchemaFeatures {
    -- | Whether to expect the type() function.
    schemaFeaturesType :: Bool,
    -- | Whether to expect the valueType() function.
    schemaFeaturesValueType :: Bool}
  deriving (Eq, Ord, Read, Show)

_SchemaFeatures = (Core.Name "hydra/langs/cypher/features.SchemaFeatures")

_SchemaFeatures_type = (Core.Name "type")

_SchemaFeatures_valueType = (Core.Name "valueType")

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

_SetFeatures_propertyEquals = (Core.Name "propertyEquals")

_SetFeatures_variableEquals = (Core.Name "variableEquals")

_SetFeatures_variablePlusEquals = (Core.Name "variablePlusEquals")

_SetFeatures_variableWithNodeLabels = (Core.Name "variableWithNodeLabels")

-- | A set of features for string functions.
data StringFeatures = 
  StringFeatures {
    -- | Whether to expect the char_length() function.
    stringFeaturesChar_length :: Bool,
    -- | Whether to expect the character_length() function.
    stringFeaturesCharacter_length :: Bool,
    -- | Whether to expect the contains() / CONTAINS aggregate function.
    stringFeaturesContains :: Bool,
    -- | Whether to expect the endsWith() / ENDS WITH aggregate function.
    stringFeaturesEndsWith :: Bool,
    -- | Whether to expect the in() / IN aggregate function.
    stringFeaturesIn :: Bool,
    -- | Whether to expect the startsWith() / STARTS WITH aggregate function.
    stringFeaturesStartsWith :: Bool,
    -- | Whether to expect the toBoolean() function.
    stringFeaturesToBoolean :: Bool,
    -- | Whether to expect the toBooleanOrNull() function.
    stringFeaturesToBooleanOrNull :: Bool,
    -- | Whether to expect the toFloat() function.
    stringFeaturesToFloat :: Bool,
    -- | Whether to expect the toFloatOrNull() function.
    stringFeaturesToFloatOrNull :: Bool,
    -- | Whether to expect the toInteger() function.
    stringFeaturesToInteger :: Bool,
    -- | Whether to expect the toIntegerOrNull() function.
    stringFeaturesToIntegerOrNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringFeatures = (Core.Name "hydra/langs/cypher/features.StringFeatures")

_StringFeatures_char_length = (Core.Name "char_length")

_StringFeatures_character_length = (Core.Name "character_length")

_StringFeatures_contains = (Core.Name "contains")

_StringFeatures_endsWith = (Core.Name "endsWith")

_StringFeatures_in = (Core.Name "in")

_StringFeatures_startsWith = (Core.Name "startsWith")

_StringFeatures_toBoolean = (Core.Name "toBoolean")

_StringFeatures_toBooleanOrNull = (Core.Name "toBooleanOrNull")

_StringFeatures_toFloat = (Core.Name "toFloat")

_StringFeatures_toFloatOrNull = (Core.Name "toFloatOrNull")

_StringFeatures_toInteger = (Core.Name "toInteger")

_StringFeatures_toIntegerOrNull = (Core.Name "toIntegerOrNull")

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

_UpdatingFeatures_create = (Core.Name "create")

_UpdatingFeatures_set = (Core.Name "set")

_UpdatingFeatures_with = (Core.Name "with")