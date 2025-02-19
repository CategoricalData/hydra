-- | A model for characterizing OpenCypher queries and implementations in terms of included features.Based on the OpenCypher grammar and the list of standard Cypher functions at https://neo4j.com/docs/cypher-manual/current/functions. Current as of August 2024.

module Hydra.Ext.Cypher.Features where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired.
data CypherFeatures = 
  CypherFeatures {
    -- | Arithmetic operations
    cypherFeaturesArithmetic :: ArithmeticFeatures,
    -- | Various kinds of atomic expressions
    cypherFeaturesAtom :: AtomFeatures,
    -- | Comparison operators and functions
    cypherFeaturesComparison :: ComparisonFeatures,
    -- | Delete operations
    cypherFeaturesDelete :: DeleteFeatures,
    -- | Standard Cypher functions
    cypherFeaturesFunction :: FunctionFeatures,
    -- | List functionality
    cypherFeaturesList :: ListFeatures,
    -- | Various types of literal values
    cypherFeaturesLiteral :: LiteralFeatures,
    -- | Logical operations
    cypherFeaturesLogical :: LogicalFeatures,
    -- | Match queries
    cypherFeaturesMatch :: MatchFeatures,
    -- | Merge operations
    cypherFeaturesMerge :: MergeFeatures,
    -- | Node patterns
    cypherFeaturesNodePattern :: NodePatternFeatures,
    -- | IS NULL / IS NOT NULL checks
    cypherFeaturesNull :: NullFeatures,
    -- | Path functions only found in OpenCypher
    cypherFeaturesPath :: PathFeatures,
    -- | Procedure calls
    cypherFeaturesProcedureCall :: ProcedureCallFeatures,
    -- | Projections
    cypherFeaturesProjection :: ProjectionFeatures,
    -- | Quantifier expressions
    cypherFeaturesQuantifier :: QuantifierFeatures,
    -- | Range literals within relationship patterns
    cypherFeaturesRangeLiteral :: RangeLiteralFeatures,
    -- | Specific syntax related to reading data from the graph.
    cypherFeaturesReading :: ReadingFeatures,
    -- | Relationship directions / arrow patterns
    cypherFeaturesRelationshipDirection :: RelationshipDirectionFeatures,
    -- | Relationship patterns
    cypherFeaturesRelationshipPattern :: RelationshipPatternFeatures,
    -- | REMOVE operations
    cypherFeaturesRemove :: RemoveFeatures,
    -- | Set definitions
    cypherFeaturesSet :: SetFeatures,
    -- | String functions/keywords only found in OpenCypher
    cypherFeaturesString :: StringFeatures,
    -- | Specific syntax related to updating data in the graph
    cypherFeaturesUpdating :: UpdatingFeatures}
  deriving (Eq, Ord, Read, Show)

_CypherFeatures = (Core.Name "hydra.ext/cypher/features.CypherFeatures")

_CypherFeatures_arithmetic = (Core.Name "arithmetic")

_CypherFeatures_atom = (Core.Name "atom")

_CypherFeatures_comparison = (Core.Name "comparison")

_CypherFeatures_delete = (Core.Name "delete")

_CypherFeatures_function = (Core.Name "function")

_CypherFeatures_list = (Core.Name "list")

_CypherFeatures_literal = (Core.Name "literal")

_CypherFeatures_logical = (Core.Name "logical")

_CypherFeatures_match = (Core.Name "match")

_CypherFeatures_merge = (Core.Name "merge")

_CypherFeatures_nodePattern = (Core.Name "nodePattern")

_CypherFeatures_null = (Core.Name "null")

_CypherFeatures_path = (Core.Name "path")

_CypherFeatures_procedureCall = (Core.Name "procedureCall")

_CypherFeatures_projection = (Core.Name "projection")

_CypherFeatures_quantifier = (Core.Name "quantifier")

_CypherFeatures_rangeLiteral = (Core.Name "rangeLiteral")

_CypherFeatures_reading = (Core.Name "reading")

_CypherFeatures_relationshipDirection = (Core.Name "relationshipDirection")

_CypherFeatures_relationshipPattern = (Core.Name "relationshipPattern")

_CypherFeatures_remove = (Core.Name "remove")

_CypherFeatures_set = (Core.Name "set")

_CypherFeatures_string = (Core.Name "string")

_CypherFeatures_updating = (Core.Name "updating")

-- | Arithmetic operations
data ArithmeticFeatures = 
  ArithmeticFeatures {
    -- | The + operator
    arithmeticFeaturesPlus :: Bool,
    -- | The - operator
    arithmeticFeaturesMinus :: Bool,
    -- | The * operator
    arithmeticFeaturesMultiply :: Bool,
    -- | The / operator
    arithmeticFeaturesDivide :: Bool,
    -- | The % operator
    arithmeticFeaturesModulus :: Bool,
    -- | The ^ operator
    arithmeticFeaturesPowerOf :: Bool}
  deriving (Eq, Ord, Read, Show)

_ArithmeticFeatures = (Core.Name "hydra.ext/cypher/features.ArithmeticFeatures")

_ArithmeticFeatures_plus = (Core.Name "plus")

_ArithmeticFeatures_minus = (Core.Name "minus")

_ArithmeticFeatures_multiply = (Core.Name "multiply")

_ArithmeticFeatures_divide = (Core.Name "divide")

_ArithmeticFeatures_modulus = (Core.Name "modulus")

_ArithmeticFeatures_powerOf = (Core.Name "powerOf")

-- | Various kinds of atomic expressions
data AtomFeatures = 
  AtomFeatures {
    -- | CASE expressions
    atomFeaturesCaseExpression :: Bool,
    -- | The COUNT (*) expression
    atomFeaturesCount :: Bool,
    -- | Existential subqueries
    atomFeaturesExistentialSubquery :: Bool,
    -- | Function invocation
    atomFeaturesFunctionInvocation :: Bool,
    -- | Parameter expressions
    atomFeaturesParameter :: Bool,
    -- | Pattern comprehensions
    atomFeaturesPatternComprehension :: Bool,
    -- | Relationship patterns as subexpressions
    atomFeaturesPatternPredicate :: Bool,
    -- | Variable expressions (note: included by most if not all implementations).
    atomFeaturesVariable :: Bool}
  deriving (Eq, Ord, Read, Show)

_AtomFeatures = (Core.Name "hydra.ext/cypher/features.AtomFeatures")

_AtomFeatures_caseExpression = (Core.Name "caseExpression")

_AtomFeatures_count = (Core.Name "count")

_AtomFeatures_existentialSubquery = (Core.Name "existentialSubquery")

_AtomFeatures_functionInvocation = (Core.Name "functionInvocation")

_AtomFeatures_parameter = (Core.Name "parameter")

_AtomFeatures_patternComprehension = (Core.Name "patternComprehension")

_AtomFeatures_patternPredicate = (Core.Name "patternPredicate")

_AtomFeatures_variable = (Core.Name "variable")

-- | Comparison operators and functions
data ComparisonFeatures = 
  ComparisonFeatures {
    -- | The = comparison operator
    comparisonFeaturesEqual :: Bool,
    -- | The > comparison operator
    comparisonFeaturesGreaterThan :: Bool,
    -- | The >= comparison operator
    comparisonFeaturesGreaterThanOrEqual :: Bool,
    -- | The < comparison operator
    comparisonFeaturesLessThan :: Bool,
    -- | The <= comparison operator
    comparisonFeaturesLessThanOrEqual :: Bool,
    -- | The <> comparison operator
    comparisonFeaturesNotEqual :: Bool}
  deriving (Eq, Ord, Read, Show)

_ComparisonFeatures = (Core.Name "hydra.ext/cypher/features.ComparisonFeatures")

_ComparisonFeatures_equal = (Core.Name "equal")

_ComparisonFeatures_greaterThan = (Core.Name "greaterThan")

_ComparisonFeatures_greaterThanOrEqual = (Core.Name "greaterThanOrEqual")

_ComparisonFeatures_lessThan = (Core.Name "lessThan")

_ComparisonFeatures_lessThanOrEqual = (Core.Name "lessThanOrEqual")

_ComparisonFeatures_notEqual = (Core.Name "notEqual")

-- | Delete operations
data DeleteFeatures = 
  DeleteFeatures {
    -- | The basic DELETE clause
    deleteFeaturesDelete :: Bool,
    -- | The DETACH DELETE clause
    deleteFeaturesDetachDelete :: Bool}
  deriving (Eq, Ord, Read, Show)

_DeleteFeatures = (Core.Name "hydra.ext/cypher/features.DeleteFeatures")

_DeleteFeatures_delete = (Core.Name "delete")

_DeleteFeatures_detachDelete = (Core.Name "detachDelete")

-- | Standard Cypher functions
data FunctionFeatures = 
  FunctionFeatures {
    -- | Aggregate functions
    functionFeaturesAggregateFunction :: AggregateFunctionFeatures,
    -- | Database functions
    functionFeaturesDatabaseFunction :: DatabaseFunctionFeatures,
    -- | GenAI functions
    functionFeaturesGenAIFunction :: GenAIFunctionFeatures,
    -- | Graph functions
    functionFeaturesGraphFunction :: GraphFunctionFeatures,
    -- | List functions
    functionFeaturesListFunction :: ListFunctionFeatures,
    -- | Load CSV functions
    functionFeaturesLoadCSVFunction :: LoadCSVFunctionFeatures,
    -- | Logarithmic functions
    functionFeaturesLogarithmicFunction :: LogarithmicFunctionFeatures,
    -- | Numeric functions
    functionFeaturesNumericFunction :: NumericFunctionFeatures,
    -- | Predicate functions
    functionFeaturesPredicateFunction :: PredicateFunctionFeatures,
    -- | Scalar functions
    functionFeaturesScalarFunction :: ScalarFunctionFeatures,
    -- | Spatial functions
    functionFeaturesSpatialFunction :: SpatialFunctionFeatures,
    -- | String functions
    functionFeaturesStringFunction :: StringFunctionFeatures,
    -- | Temporal duration functions
    functionFeaturesTemporalDurationFunction :: TemporalDurationFunctionFeatures,
    -- | Temporal instant functions
    functionFeaturesTemporalInstantFunction :: TemporalInstantFunctionFeatures,
    -- | Trigonometric functions
    functionFeaturesTrigonometricFunction :: TrigonometricFunctionFeatures,
    -- | Vector functions
    functionFeaturesVectorFunction :: VectorFunctionFeatures}
  deriving (Eq, Ord, Read, Show)

_FunctionFeatures = (Core.Name "hydra.ext/cypher/features.FunctionFeatures")

_FunctionFeatures_aggregateFunction = (Core.Name "aggregateFunction")

_FunctionFeatures_databaseFunction = (Core.Name "databaseFunction")

_FunctionFeatures_genAIFunction = (Core.Name "genAIFunction")

_FunctionFeatures_graphFunction = (Core.Name "graphFunction")

_FunctionFeatures_listFunction = (Core.Name "listFunction")

_FunctionFeatures_loadCSVFunction = (Core.Name "loadCSVFunction")

_FunctionFeatures_logarithmicFunction = (Core.Name "logarithmicFunction")

_FunctionFeatures_numericFunction = (Core.Name "numericFunction")

_FunctionFeatures_predicateFunction = (Core.Name "predicateFunction")

_FunctionFeatures_scalarFunction = (Core.Name "scalarFunction")

_FunctionFeatures_spatialFunction = (Core.Name "spatialFunction")

_FunctionFeatures_stringFunction = (Core.Name "stringFunction")

_FunctionFeatures_temporalDurationFunction = (Core.Name "temporalDurationFunction")

_FunctionFeatures_temporalInstantFunction = (Core.Name "temporalInstantFunction")

_FunctionFeatures_trigonometricFunction = (Core.Name "trigonometricFunction")

_FunctionFeatures_vectorFunction = (Core.Name "vectorFunction")

-- | Aggregate functions
data AggregateFunctionFeatures = 
  AggregateFunctionFeatures {
    -- | The avg() function / AVG. Returns the average of a set of DURATION values.; Returns the average of a set of FLOAT values.; Returns the average of a set of INTEGER values.
    aggregateFunctionFeaturesAvg :: Bool,
    -- | The collect() function / COLLECT. Returns a list containing the values returned by an expression.
    aggregateFunctionFeaturesCollect :: Bool,
    -- | The count() function / COUNT. Returns the number of values or rows.
    aggregateFunctionFeaturesCount :: Bool,
    -- | The max() function / MAX. Returns the maximum value in a set of values.
    aggregateFunctionFeaturesMax :: Bool,
    -- | The min() function / MIN. Returns the minimum value in a set of values.
    aggregateFunctionFeaturesMin :: Bool,
    -- | The percentileCont() function. Returns the percentile of a value over a group using linear interpolation.
    aggregateFunctionFeaturesPercentileCont :: Bool,
    -- | The percentileDisc() function. Returns the nearest FLOAT value to the given percentile over a group using a rounding method.; Returns the nearest INTEGER value to the given percentile over a group using a rounding method.
    aggregateFunctionFeaturesPercentileDisc :: Bool,
    -- | The stdev() function. Returns the standard deviation for the given value over a group for a sample of a population.
    aggregateFunctionFeaturesStdev :: Bool,
    -- | The stdevp() function. Returns the standard deviation for the given value over a group for an entire population.
    aggregateFunctionFeaturesStdevp :: Bool,
    -- | The sum() function / SUM. Returns the sum of a set of DURATION values.; Returns the sum of a set of FLOAT values.; Returns the sum of a set of INTEGER values.
    aggregateFunctionFeaturesSum :: Bool}
  deriving (Eq, Ord, Read, Show)

_AggregateFunctionFeatures = (Core.Name "hydra.ext/cypher/features.AggregateFunctionFeatures")

_AggregateFunctionFeatures_avg = (Core.Name "avg")

_AggregateFunctionFeatures_collect = (Core.Name "collect")

_AggregateFunctionFeatures_count = (Core.Name "count")

_AggregateFunctionFeatures_max = (Core.Name "max")

_AggregateFunctionFeatures_min = (Core.Name "min")

_AggregateFunctionFeatures_percentileCont = (Core.Name "percentileCont")

_AggregateFunctionFeatures_percentileDisc = (Core.Name "percentileDisc")

_AggregateFunctionFeatures_stdev = (Core.Name "stdev")

_AggregateFunctionFeatures_stdevp = (Core.Name "stdevp")

_AggregateFunctionFeatures_sum = (Core.Name "sum")

-- | Database functions
data DatabaseFunctionFeatures = 
  DatabaseFunctionFeatures {
    -- | The db.nameFromElementId() function. Resolves the database name from the given element id. Introduced in 5.12.
    databaseFunctionFeaturesDb_nameFromElementId :: Bool}
  deriving (Eq, Ord, Read, Show)

_DatabaseFunctionFeatures = (Core.Name "hydra.ext/cypher/features.DatabaseFunctionFeatures")

_DatabaseFunctionFeatures_db_nameFromElementId = (Core.Name "db.nameFromElementId")

-- | GenAI functions
data GenAIFunctionFeatures = 
  GenAIFunctionFeatures {
    -- | The genai.vector.encode() function. Encode a given resource as a vector using the named provider. Introduced in 5.17.
    genAIFunctionFeaturesGenai_vector_encode :: Bool}
  deriving (Eq, Ord, Read, Show)

_GenAIFunctionFeatures = (Core.Name "hydra.ext/cypher/features.GenAIFunctionFeatures")

_GenAIFunctionFeatures_genai_vector_encode = (Core.Name "genai.vector.encode")

-- | Graph functions
data GraphFunctionFeatures = 
  GraphFunctionFeatures {
    -- | The graph.byElementId() function. Resolves the constituent graph to which a given element id belongs. Introduced in 5.13.
    graphFunctionFeaturesGraph_byElementId :: Bool,
    -- | The graph.byName() function. Resolves a constituent graph by name.
    graphFunctionFeaturesGraph_byName :: Bool,
    -- | The graph.names() function. Returns a list containing the names of all graphs in the current composite database.
    graphFunctionFeaturesGraph_names :: Bool,
    -- | The graph.propertiesByName() function. Returns a map containing the properties associated with the given graph.
    graphFunctionFeaturesGraph_propertiesByName :: Bool}
  deriving (Eq, Ord, Read, Show)

_GraphFunctionFeatures = (Core.Name "hydra.ext/cypher/features.GraphFunctionFeatures")

_GraphFunctionFeatures_graph_byElementId = (Core.Name "graph.byElementId")

_GraphFunctionFeatures_graph_byName = (Core.Name "graph.byName")

_GraphFunctionFeatures_graph_names = (Core.Name "graph.names")

_GraphFunctionFeatures_graph_propertiesByName = (Core.Name "graph.propertiesByName")

-- | List functions
data ListFunctionFeatures = 
  ListFunctionFeatures {
    -- | The keys() function. Returns a LIST<STRING> containing the STRING representations for all the property names of a MAP.; Returns a LIST<STRING> containing the STRING representations for all the property names of a NODE.; Returns a LIST<STRING> containing the STRING representations for all the property names of a RELATIONSHIP.
    listFunctionFeaturesKeys :: Bool,
    -- | The labels() function. Returns a LIST<STRING> containing the STRING representations for all the labels of a NODE.
    listFunctionFeaturesLabels :: Bool,
    -- | The nodes() function. Returns a LIST<NODE> containing all the NODE values in a PATH.
    listFunctionFeaturesNodes :: Bool,
    -- | The range() function. Returns a LIST<INTEGER> comprising all INTEGER values within a specified range.; Returns a LIST<INTEGER> comprising all INTEGER values within a specified range created with step length.
    listFunctionFeaturesRange :: Bool,
    -- | The reduce() function. Runs an expression against individual elements of a LIST<ANY>, storing the result of the expression in an accumulator.
    listFunctionFeaturesReduce :: Bool,
    -- | The relationships() function. Returns a LIST<RELATIONSHIP> containing all the RELATIONSHIP values in a PATH.
    listFunctionFeaturesRelationships :: Bool,
    -- | The reverse() function. Returns a LIST<ANY> in which the order of all elements in the given LIST<ANY> have been reversed.
    listFunctionFeaturesReverse :: Bool,
    -- | The tail() function. Returns all but the first element in a LIST<ANY>.
    listFunctionFeaturesTail :: Bool,
    -- | The toBooleanList() function. Converts a LIST<ANY> of values to a LIST<BOOLEAN> values. If any values are not convertible to BOOLEAN they will be null in the LIST<BOOLEAN> returned.
    listFunctionFeaturesToBooleanList :: Bool,
    -- | The toFloatList() function. Converts a LIST<ANY> to a LIST<FLOAT> values. If any values are not convertible to FLOAT they will be null in the LIST<FLOAT> returned.
    listFunctionFeaturesToFloatList :: Bool,
    -- | The toIntegerList() function. Converts a LIST<ANY> to a LIST<INTEGER> values. If any values are not convertible to INTEGER they will be null in the LIST<INTEGER> returned.
    listFunctionFeaturesToIntegerList :: Bool,
    -- | The toStringList() function. Converts a LIST<ANY> to a LIST<STRING> values. If any values are not convertible to STRING they will be null in the LIST<STRING> returned.
    listFunctionFeaturesToStringList :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListFunctionFeatures = (Core.Name "hydra.ext/cypher/features.ListFunctionFeatures")

_ListFunctionFeatures_keys = (Core.Name "keys")

_ListFunctionFeatures_labels = (Core.Name "labels")

_ListFunctionFeatures_nodes = (Core.Name "nodes")

_ListFunctionFeatures_range = (Core.Name "range")

_ListFunctionFeatures_reduce = (Core.Name "reduce")

_ListFunctionFeatures_relationships = (Core.Name "relationships")

_ListFunctionFeatures_reverse = (Core.Name "reverse")

_ListFunctionFeatures_tail = (Core.Name "tail")

_ListFunctionFeatures_toBooleanList = (Core.Name "toBooleanList")

_ListFunctionFeatures_toFloatList = (Core.Name "toFloatList")

_ListFunctionFeatures_toIntegerList = (Core.Name "toIntegerList")

_ListFunctionFeatures_toStringList = (Core.Name "toStringList")

-- | Load CSV functions
data LoadCSVFunctionFeatures = 
  LoadCSVFunctionFeatures {
    -- | The file() function. Returns the absolute path of the file that LOAD CSV is using.
    loadCSVFunctionFeaturesFile :: Bool,
    -- | The linenumber() function. Returns the line number that LOAD CSV is currently using.
    loadCSVFunctionFeaturesLinenumber :: Bool}
  deriving (Eq, Ord, Read, Show)

_LoadCSVFunctionFeatures = (Core.Name "hydra.ext/cypher/features.LoadCSVFunctionFeatures")

_LoadCSVFunctionFeatures_file = (Core.Name "file")

_LoadCSVFunctionFeatures_linenumber = (Core.Name "linenumber")

-- | Logarithmic functions
data LogarithmicFunctionFeatures = 
  LogarithmicFunctionFeatures {
    -- | The e() function. Returns the base of the natural logarithm, e.
    logarithmicFunctionFeaturesE :: Bool,
    -- | The exp() function. Returns e^n, where e is the base of the natural logarithm, and n is the value of the argument expression.
    logarithmicFunctionFeaturesExp :: Bool,
    -- | The log() function. Returns the natural logarithm of a FLOAT.
    logarithmicFunctionFeaturesLog :: Bool,
    -- | The log10() function. Returns the common logarithm (base 10) of a FLOAT.
    logarithmicFunctionFeaturesLog10 :: Bool,
    -- | The sqrt() function. Returns the square root of a FLOAT.
    logarithmicFunctionFeaturesSqrt :: Bool}
  deriving (Eq, Ord, Read, Show)

_LogarithmicFunctionFeatures = (Core.Name "hydra.ext/cypher/features.LogarithmicFunctionFeatures")

_LogarithmicFunctionFeatures_e = (Core.Name "e")

_LogarithmicFunctionFeatures_exp = (Core.Name "exp")

_LogarithmicFunctionFeatures_log = (Core.Name "log")

_LogarithmicFunctionFeatures_log10 = (Core.Name "log10")

_LogarithmicFunctionFeatures_sqrt = (Core.Name "sqrt")

-- | Numeric functions
data NumericFunctionFeatures = 
  NumericFunctionFeatures {
    -- | The abs() function. Returns the absolute value of a FLOAT.; Returns the absolute value of an INTEGER.
    numericFunctionFeaturesAbs :: Bool,
    -- | The ceil() function. Returns the smallest FLOAT that is greater than or equal to a number and equal to an INTEGER.
    numericFunctionFeaturesCeil :: Bool,
    -- | The floor() function. Returns the largest FLOAT that is less than or equal to a number and equal to an INTEGER.
    numericFunctionFeaturesFloor :: Bool,
    -- | The isNaN() function. Returns true if the floating point number is NaN.; Returns true if the integer number is NaN.
    numericFunctionFeaturesIsNaN :: Bool,
    -- | The rand() function. Returns a random FLOAT in the range from 0 (inclusive) to 1 (exclusive).
    numericFunctionFeaturesRand :: Bool,
    -- | The round() function. Returns the value of a number rounded to the nearest INTEGER.; Returns the value of a number rounded to the specified precision using rounding mode HALF_UP.; Returns the value of a number rounded to the specified precision with the specified rounding mode.
    numericFunctionFeaturesRound :: Bool,
    -- | The sign() function. Returns the signum of a FLOAT: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.; Returns the signum of an INTEGER: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.
    numericFunctionFeaturesSign :: Bool}
  deriving (Eq, Ord, Read, Show)

_NumericFunctionFeatures = (Core.Name "hydra.ext/cypher/features.NumericFunctionFeatures")

_NumericFunctionFeatures_abs = (Core.Name "abs")

_NumericFunctionFeatures_ceil = (Core.Name "ceil")

_NumericFunctionFeatures_floor = (Core.Name "floor")

_NumericFunctionFeatures_isNaN = (Core.Name "isNaN")

_NumericFunctionFeatures_rand = (Core.Name "rand")

_NumericFunctionFeatures_round = (Core.Name "round")

_NumericFunctionFeatures_sign = (Core.Name "sign")

-- | Predicate functions
data PredicateFunctionFeatures = 
  PredicateFunctionFeatures {
    -- | The all() function. Returns true if the predicate holds for all elements in the given LIST<ANY>.
    predicateFunctionFeaturesAll :: Bool,
    -- | The any() function. Returns true if the predicate holds for at least one element in the given LIST<ANY>.
    predicateFunctionFeaturesAny :: Bool,
    -- | The exists() function. Returns true if a match for the pattern exists in the graph.
    predicateFunctionFeaturesExists :: Bool,
    -- | The isEmpty() function. Checks whether a LIST<ANY> is empty.; Checks whether a MAP is empty.; Checks whether a STRING is empty.
    predicateFunctionFeaturesIsEmpty :: Bool,
    -- | The none() function. Returns true if the predicate holds for no element in the given LIST<ANY>.
    predicateFunctionFeaturesNone :: Bool,
    -- | The single() function. Returns true if the predicate holds for exactly one of the elements in the given LIST<ANY>.
    predicateFunctionFeaturesSingle :: Bool}
  deriving (Eq, Ord, Read, Show)

_PredicateFunctionFeatures = (Core.Name "hydra.ext/cypher/features.PredicateFunctionFeatures")

_PredicateFunctionFeatures_all = (Core.Name "all")

_PredicateFunctionFeatures_any = (Core.Name "any")

_PredicateFunctionFeatures_exists = (Core.Name "exists")

_PredicateFunctionFeatures_isEmpty = (Core.Name "isEmpty")

_PredicateFunctionFeatures_none = (Core.Name "none")

_PredicateFunctionFeatures_single = (Core.Name "single")

-- | Scalar functions
data ScalarFunctionFeatures = 
  ScalarFunctionFeatures {
    -- | The char_length() function. Returns the number of Unicode characters in a STRING.
    scalarFunctionFeaturesChar_length :: Bool,
    -- | The character_length() function. Returns the number of Unicode characters in a STRING.
    scalarFunctionFeaturesCharacter_length :: Bool,
    -- | The coalesce() function. Returns the first non-null value in a list of expressions.
    scalarFunctionFeaturesCoalesce :: Bool,
    -- | The elementId() function. Returns a node identifier, unique within a specific transaction and DBMS.; Returns a relationship identifier, unique within a specific transaction and DBMS.
    scalarFunctionFeaturesElementId :: Bool,
    -- | The endNode() function. Returns a relationship identifier, unique within a specific transaction and DBMS.
    scalarFunctionFeaturesEndNode :: Bool,
    -- | The head() function. Returns the first element in a LIST<ANY>.
    scalarFunctionFeaturesHead :: Bool,
    -- | The id() function. [Deprecated] Returns the id of a NODE. Replaced by elementId().; [Deprecated] Returns the id of a RELATIONSHIP. Replaced by elementId().
    scalarFunctionFeaturesId :: Bool,
    -- | The last() function. Returns the last element in a LIST<ANY>.
    scalarFunctionFeaturesLast :: Bool,
    -- | The length() function. Returns the length of a PATH.
    scalarFunctionFeaturesLength :: Bool,
    -- | The nullIf() function. Returns null if the two given parameters are equivalent, otherwise returns the value of the first parameter.
    scalarFunctionFeaturesNullIf :: Bool,
    -- | The properties() function. Returns a MAP containing all the properties of a MAP.; Returns a MAP containing all the properties of a NODE.; Returns a MAP containing all the properties of a RELATIONSHIP.
    scalarFunctionFeaturesProperties :: Bool,
    -- | The randomUUID() function. Generates a random UUID.
    scalarFunctionFeaturesRandomUUID :: Bool,
    -- | The size() function. Returns the number of items in a LIST<ANY>.; Returns the number of Unicode characters in a STRING.
    scalarFunctionFeaturesSize :: Bool,
    -- | The startNode() function. Returns the start NODE of a RELATIONSHIP.
    scalarFunctionFeaturesStartNode :: Bool,
    -- | The toBoolean() function. Converts a STRING value to a BOOLEAN value.; Converts a BOOLEAN value to a BOOLEAN value.; Converts an INTEGER value to a BOOLEAN value.
    scalarFunctionFeaturesToBoolean :: Bool,
    -- | The toBooleanOrNull() function. Converts a value to a BOOLEAN value, or null if the value cannot be converted.
    scalarFunctionFeaturesToBooleanOrNull :: Bool,
    -- | The toFloat() function. Converts an INTEGER value to a FLOAT value.; Converts a STRING value to a FLOAT value.
    scalarFunctionFeaturesToFloat :: Bool,
    -- | The toFloatOrNull() function. Converts a value to a FLOAT value, or null if the value cannot be converted.
    scalarFunctionFeaturesToFloatOrNull :: Bool,
    -- | The toInteger() function. Converts a FLOAT value to an INTEGER value.; Converts a BOOLEAN value to an INTEGER value.; Converts a STRING value to an INTEGER value.
    scalarFunctionFeaturesToInteger :: Bool,
    -- | The toIntegerOrNull() function. Converts a value to an INTEGER value, or null if the value cannot be converted.
    scalarFunctionFeaturesToIntegerOrNull :: Bool,
    -- | The type() function. Returns a STRING representation of the RELATIONSHIP type.
    scalarFunctionFeaturesType :: Bool,
    -- | The valueType() function. Returns a STRING representation of the most precise value type that the given expression evaluates to.
    scalarFunctionFeaturesValueType :: Bool}
  deriving (Eq, Ord, Read, Show)

_ScalarFunctionFeatures = (Core.Name "hydra.ext/cypher/features.ScalarFunctionFeatures")

_ScalarFunctionFeatures_char_length = (Core.Name "char_length")

_ScalarFunctionFeatures_character_length = (Core.Name "character_length")

_ScalarFunctionFeatures_coalesce = (Core.Name "coalesce")

_ScalarFunctionFeatures_elementId = (Core.Name "elementId")

_ScalarFunctionFeatures_endNode = (Core.Name "endNode")

_ScalarFunctionFeatures_head = (Core.Name "head")

_ScalarFunctionFeatures_id = (Core.Name "id")

_ScalarFunctionFeatures_last = (Core.Name "last")

_ScalarFunctionFeatures_length = (Core.Name "length")

_ScalarFunctionFeatures_nullIf = (Core.Name "nullIf")

_ScalarFunctionFeatures_properties = (Core.Name "properties")

_ScalarFunctionFeatures_randomUUID = (Core.Name "randomUUID")

_ScalarFunctionFeatures_size = (Core.Name "size")

_ScalarFunctionFeatures_startNode = (Core.Name "startNode")

_ScalarFunctionFeatures_toBoolean = (Core.Name "toBoolean")

_ScalarFunctionFeatures_toBooleanOrNull = (Core.Name "toBooleanOrNull")

_ScalarFunctionFeatures_toFloat = (Core.Name "toFloat")

_ScalarFunctionFeatures_toFloatOrNull = (Core.Name "toFloatOrNull")

_ScalarFunctionFeatures_toInteger = (Core.Name "toInteger")

_ScalarFunctionFeatures_toIntegerOrNull = (Core.Name "toIntegerOrNull")

_ScalarFunctionFeatures_type = (Core.Name "type")

_ScalarFunctionFeatures_valueType = (Core.Name "valueType")

-- | Spatial functions
data SpatialFunctionFeatures = 
  SpatialFunctionFeatures {
    -- | The point.distance() function. Returns a FLOAT representing the geodesic distance between any two points in the same CRS.
    spatialFunctionFeaturesPoint_distance :: Bool,
    -- | The point() function. Returns a 2D point object, given two coordinate values in the Cartesian coordinate system.; Returns a 3D point object, given three coordinate values in the Cartesian coordinate system.; Returns a 2D point object, given two coordinate values in the WGS 84 geographic coordinate system.; Returns a 3D point object, given three coordinate values in the WGS 84 geographic coordinate system.
    spatialFunctionFeaturesPoint :: Bool,
    -- | The point.withinBBox() function. Returns true if the provided point is within the bounding box defined by the two provided points, lowerLeft and upperRight.
    spatialFunctionFeaturesPoint_withinBBox :: Bool}
  deriving (Eq, Ord, Read, Show)

_SpatialFunctionFeatures = (Core.Name "hydra.ext/cypher/features.SpatialFunctionFeatures")

_SpatialFunctionFeatures_point_distance = (Core.Name "point.distance")

_SpatialFunctionFeatures_point = (Core.Name "point")

_SpatialFunctionFeatures_point_withinBBox = (Core.Name "point.withinBBox")

-- | String functions
data StringFunctionFeatures = 
  StringFunctionFeatures {
    -- | The btrim() function. Returns the given STRING with leading and trailing whitespace removed.; Returns the given STRING with leading and trailing trimCharacterString characters removed. Introduced in 5.20.
    stringFunctionFeaturesBtrim :: Bool,
    -- | The left() function. Returns a STRING containing the specified number (INTEGER) of leftmost characters in the given STRING.
    stringFunctionFeaturesLeft :: Bool,
    -- | The lower() function. Returns the given STRING in lowercase. This function is an alias to the toLower() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21.
    stringFunctionFeaturesLower :: Bool,
    -- | The ltrim() function. Returns the given STRING with leading whitespace removed.; Returns the given STRING with leading trimCharacterString characters removed. Introduced in 5.20.
    stringFunctionFeaturesLtrim :: Bool,
    -- | The normalize() function. Returns the given STRING normalized according to the normalization CypherFunctionForm NFC. Introduced in 5.17.; Returns the given STRING normalized according to the specified normalization CypherFunctionForm. Introduced in 5.17.
    stringFunctionFeaturesNormalize :: Bool,
    -- | The replace() function. Returns a STRING in which all occurrences of a specified search STRING in the given STRING have been replaced by another (specified) replacement STRING.
    stringFunctionFeaturesReplace :: Bool,
    -- | The reverse() function. Returns a STRING in which the order of all characters in the given STRING have been reversed.
    stringFunctionFeaturesReverse :: Bool,
    -- | The right() function. Returns a STRING containing the specified number of rightmost characters in the given STRING.
    stringFunctionFeaturesRight :: Bool,
    -- | The rtrim() function. Returns the given STRING with trailing whitespace removed.; Returns the given STRING with trailing trimCharacterString characters removed. Introduced in 5.20.
    stringFunctionFeaturesRtrim :: Bool,
    -- | The split() function. Returns a LIST<STRING> resulting from the splitting of the given STRING around matches of the given delimiter.; Returns a LIST<STRING> resulting from the splitting of the given STRING around matches of any of the given delimiters.
    stringFunctionFeaturesSplit :: Bool,
    -- | The substring() function. Returns a substring of the given STRING, beginning with a 0-based index start.; Returns a substring of a given length from the given STRING, beginning with a 0-based index start.
    stringFunctionFeaturesSubstring :: Bool,
    -- | The toLower() function. Returns the given STRING in lowercase.
    stringFunctionFeaturesToLower :: Bool,
    -- | The toString() function. Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING.
    stringFunctionFeaturesToString :: Bool,
    -- | The toStringOrNull() function. Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING, or null if the value cannot be converted.
    stringFunctionFeaturesToStringOrNull :: Bool,
    -- | The toUpper() function. Returns the given STRING in uppercase.
    stringFunctionFeaturesToUpper :: Bool,
    -- | The trim() function. Returns the given STRING with leading and trailing whitespace removed.; Returns the given STRING with the leading and/or trailing trimCharacterString character removed. Introduced in 5.20.
    stringFunctionFeaturesTrim :: Bool,
    -- | The upper() function. Returns the given STRING in uppercase. This function is an alias to the toUpper() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21.
    stringFunctionFeaturesUpper :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringFunctionFeatures = (Core.Name "hydra.ext/cypher/features.StringFunctionFeatures")

_StringFunctionFeatures_btrim = (Core.Name "btrim")

_StringFunctionFeatures_left = (Core.Name "left")

_StringFunctionFeatures_lower = (Core.Name "lower")

_StringFunctionFeatures_ltrim = (Core.Name "ltrim")

_StringFunctionFeatures_normalize = (Core.Name "normalize")

_StringFunctionFeatures_replace = (Core.Name "replace")

_StringFunctionFeatures_reverse = (Core.Name "reverse")

_StringFunctionFeatures_right = (Core.Name "right")

_StringFunctionFeatures_rtrim = (Core.Name "rtrim")

_StringFunctionFeatures_split = (Core.Name "split")

_StringFunctionFeatures_substring = (Core.Name "substring")

_StringFunctionFeatures_toLower = (Core.Name "toLower")

_StringFunctionFeatures_toString = (Core.Name "toString")

_StringFunctionFeatures_toStringOrNull = (Core.Name "toStringOrNull")

_StringFunctionFeatures_toUpper = (Core.Name "toUpper")

_StringFunctionFeatures_trim = (Core.Name "trim")

_StringFunctionFeatures_upper = (Core.Name "upper")

-- | Temporal duration functions
data TemporalDurationFunctionFeatures = 
  TemporalDurationFunctionFeatures {
    -- | The duration() function. Constructs a DURATION value.
    temporalDurationFunctionFeaturesDuration :: Bool,
    -- | The duration.between() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in logical units.
    temporalDurationFunctionFeaturesDuration_between :: Bool,
    -- | The duration.inDays() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in days.
    temporalDurationFunctionFeaturesDuration_inDays :: Bool,
    -- | The duration.inMonths() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in months.
    temporalDurationFunctionFeaturesDuration_inMonths :: Bool,
    -- | The duration.inSeconds() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in seconds.
    temporalDurationFunctionFeaturesDuration_inSeconds :: Bool}
  deriving (Eq, Ord, Read, Show)

_TemporalDurationFunctionFeatures = (Core.Name "hydra.ext/cypher/features.TemporalDurationFunctionFeatures")

_TemporalDurationFunctionFeatures_duration = (Core.Name "duration")

_TemporalDurationFunctionFeatures_duration_between = (Core.Name "duration.between")

_TemporalDurationFunctionFeatures_duration_inDays = (Core.Name "duration.inDays")

_TemporalDurationFunctionFeatures_duration_inMonths = (Core.Name "duration.inMonths")

_TemporalDurationFunctionFeatures_duration_inSeconds = (Core.Name "duration.inSeconds")

-- | Temporal instant functions
data TemporalInstantFunctionFeatures = 
  TemporalInstantFunctionFeatures {
    -- | The date() function. Creates a DATE instant.
    temporalInstantFunctionFeaturesDate :: Bool,
    -- | The date.realtime() function. Returns the current DATE instant using the realtime clock.
    temporalInstantFunctionFeaturesDate_realtime :: Bool,
    -- | The date.statement() function. Returns the current DATE instant using the statement clock.
    temporalInstantFunctionFeaturesDate_statement :: Bool,
    -- | The date.transaction() function. Returns the current DATE instant using the transaction clock.
    temporalInstantFunctionFeaturesDate_transaction :: Bool,
    -- | The date.truncate() function. Truncates the given temporal value to a DATE instant using the specified unit.
    temporalInstantFunctionFeaturesDate_truncate :: Bool,
    -- | The datetime() function. Creates a ZONED DATETIME instant.
    temporalInstantFunctionFeaturesDatetime :: Bool,
    -- | The datetime.fromepoch() function. Creates a ZONED DATETIME given the seconds and nanoseconds since the start of the epoch.
    temporalInstantFunctionFeaturesDatetime_fromepoch :: Bool,
    -- | The datetime.fromepochmillis() function. Creates a ZONED DATETIME given the milliseconds since the start of the epoch.
    temporalInstantFunctionFeaturesDatetime_fromepochmillis :: Bool,
    -- | The datetime.realtime() function. Returns the current ZONED DATETIME instant using the realtime clock.
    temporalInstantFunctionFeaturesDatetime_realtime :: Bool,
    -- | The datetime.statement() function. Returns the current ZONED DATETIME instant using the statement clock.
    temporalInstantFunctionFeaturesDatetime_statement :: Bool,
    -- | The datetime.transaction() function. Returns the current ZONED DATETIME instant using the transaction clock.
    temporalInstantFunctionFeaturesDatetime_transaction :: Bool,
    -- | The datetime.truncate() function. Truncates the given temporal value to a ZONED DATETIME instant using the specified unit.
    temporalInstantFunctionFeaturesDatetime_truncate :: Bool,
    -- | The localdatetime() function. Creates a LOCAL DATETIME instant.
    temporalInstantFunctionFeaturesLocaldatetime :: Bool,
    -- | The localdatetime.realtime() function. Returns the current LOCAL DATETIME instant using the realtime clock.
    temporalInstantFunctionFeaturesLocaldatetime_realtime :: Bool,
    -- | The localdatetime.statement() function. Returns the current LOCAL DATETIME instant using the statement clock.
    temporalInstantFunctionFeaturesLocaldatetime_statement :: Bool,
    -- | The localdatetime.transaction() function. Returns the current LOCAL DATETIME instant using the transaction clock.
    temporalInstantFunctionFeaturesLocaldatetime_transaction :: Bool,
    -- | The localdatetime.truncate() function. Truncates the given temporal value to a LOCAL DATETIME instant using the specified unit.
    temporalInstantFunctionFeaturesLocaldatetime_truncate :: Bool,
    -- | The localtime() function. Creates a LOCAL TIME instant.
    temporalInstantFunctionFeaturesLocaltime :: Bool,
    -- | The localtime.realtime() function. Returns the current LOCAL TIME instant using the realtime clock.
    temporalInstantFunctionFeaturesLocaltime_realtime :: Bool,
    -- | The localtime.statement() function. Returns the current LOCAL TIME instant using the statement clock.
    temporalInstantFunctionFeaturesLocaltime_statement :: Bool,
    -- | The localtime.transaction() function. Returns the current LOCAL TIME instant using the transaction clock.
    temporalInstantFunctionFeaturesLocaltime_transaction :: Bool,
    -- | The localtime.truncate() function. Truncates the given temporal value to a LOCAL TIME instant using the specified unit.
    temporalInstantFunctionFeaturesLocaltime_truncate :: Bool,
    -- | The time() function. Creates a ZONED TIME instant.
    temporalInstantFunctionFeaturesTime :: Bool,
    -- | The time.realtime() function. Returns the current ZONED TIME instant using the realtime clock.
    temporalInstantFunctionFeaturesTime_realtime :: Bool,
    -- | The time.statement() function. Returns the current ZONED TIME instant using the statement clock.
    temporalInstantFunctionFeaturesTime_statement :: Bool,
    -- | The time.transaction() function. Returns the current ZONED TIME instant using the transaction clock.
    temporalInstantFunctionFeaturesTime_transaction :: Bool,
    -- | The time.truncate() function. Truncates the given temporal value to a ZONED TIME instant using the specified unit.
    temporalInstantFunctionFeaturesTime_truncate :: Bool}
  deriving (Eq, Ord, Read, Show)

_TemporalInstantFunctionFeatures = (Core.Name "hydra.ext/cypher/features.TemporalInstantFunctionFeatures")

_TemporalInstantFunctionFeatures_date = (Core.Name "date")

_TemporalInstantFunctionFeatures_date_realtime = (Core.Name "date.realtime")

_TemporalInstantFunctionFeatures_date_statement = (Core.Name "date.statement")

_TemporalInstantFunctionFeatures_date_transaction = (Core.Name "date.transaction")

_TemporalInstantFunctionFeatures_date_truncate = (Core.Name "date.truncate")

_TemporalInstantFunctionFeatures_datetime = (Core.Name "datetime")

_TemporalInstantFunctionFeatures_datetime_fromepoch = (Core.Name "datetime.fromepoch")

_TemporalInstantFunctionFeatures_datetime_fromepochmillis = (Core.Name "datetime.fromepochmillis")

_TemporalInstantFunctionFeatures_datetime_realtime = (Core.Name "datetime.realtime")

_TemporalInstantFunctionFeatures_datetime_statement = (Core.Name "datetime.statement")

_TemporalInstantFunctionFeatures_datetime_transaction = (Core.Name "datetime.transaction")

_TemporalInstantFunctionFeatures_datetime_truncate = (Core.Name "datetime.truncate")

_TemporalInstantFunctionFeatures_localdatetime = (Core.Name "localdatetime")

_TemporalInstantFunctionFeatures_localdatetime_realtime = (Core.Name "localdatetime.realtime")

_TemporalInstantFunctionFeatures_localdatetime_statement = (Core.Name "localdatetime.statement")

_TemporalInstantFunctionFeatures_localdatetime_transaction = (Core.Name "localdatetime.transaction")

_TemporalInstantFunctionFeatures_localdatetime_truncate = (Core.Name "localdatetime.truncate")

_TemporalInstantFunctionFeatures_localtime = (Core.Name "localtime")

_TemporalInstantFunctionFeatures_localtime_realtime = (Core.Name "localtime.realtime")

_TemporalInstantFunctionFeatures_localtime_statement = (Core.Name "localtime.statement")

_TemporalInstantFunctionFeatures_localtime_transaction = (Core.Name "localtime.transaction")

_TemporalInstantFunctionFeatures_localtime_truncate = (Core.Name "localtime.truncate")

_TemporalInstantFunctionFeatures_time = (Core.Name "time")

_TemporalInstantFunctionFeatures_time_realtime = (Core.Name "time.realtime")

_TemporalInstantFunctionFeatures_time_statement = (Core.Name "time.statement")

_TemporalInstantFunctionFeatures_time_transaction = (Core.Name "time.transaction")

_TemporalInstantFunctionFeatures_time_truncate = (Core.Name "time.truncate")

-- | Trigonometric functions
data TrigonometricFunctionFeatures = 
  TrigonometricFunctionFeatures {
    -- | The acos() function. Returns the arccosine of a FLOAT in radians.
    trigonometricFunctionFeaturesAcos :: Bool,
    -- | The asin() function. Returns the arcsine of a FLOAT in radians.
    trigonometricFunctionFeaturesAsin :: Bool,
    -- | The atan() function. Returns the arctangent of a FLOAT in radians.
    trigonometricFunctionFeaturesAtan :: Bool,
    -- | The atan2() function. Returns the arctangent2 of a set of coordinates in radians.
    trigonometricFunctionFeaturesAtan2 :: Bool,
    -- | The cos() function. Returns the cosine of a FLOAT.
    trigonometricFunctionFeaturesCos :: Bool,
    -- | The cot() function. Returns the cotangent of a FLOAT.
    trigonometricFunctionFeaturesCot :: Bool,
    -- | The degrees() function. Converts radians to degrees.
    trigonometricFunctionFeaturesDegrees :: Bool,
    -- | The haversin() function. Returns half the versine of a number.
    trigonometricFunctionFeaturesHaversin :: Bool,
    -- | The pi() function. Returns the mathematical constant pi.
    trigonometricFunctionFeaturesPi :: Bool,
    -- | The radians() function. Converts degrees to radians.
    trigonometricFunctionFeaturesRadians :: Bool,
    -- | The sin() function. Returns the sine of a FLOAT.
    trigonometricFunctionFeaturesSin :: Bool,
    -- | The tan() function. Returns the tangent of a FLOAT.
    trigonometricFunctionFeaturesTan :: Bool}
  deriving (Eq, Ord, Read, Show)

_TrigonometricFunctionFeatures = (Core.Name "hydra.ext/cypher/features.TrigonometricFunctionFeatures")

_TrigonometricFunctionFeatures_acos = (Core.Name "acos")

_TrigonometricFunctionFeatures_asin = (Core.Name "asin")

_TrigonometricFunctionFeatures_atan = (Core.Name "atan")

_TrigonometricFunctionFeatures_atan2 = (Core.Name "atan2")

_TrigonometricFunctionFeatures_cos = (Core.Name "cos")

_TrigonometricFunctionFeatures_cot = (Core.Name "cot")

_TrigonometricFunctionFeatures_degrees = (Core.Name "degrees")

_TrigonometricFunctionFeatures_haversin = (Core.Name "haversin")

_TrigonometricFunctionFeatures_pi = (Core.Name "pi")

_TrigonometricFunctionFeatures_radians = (Core.Name "radians")

_TrigonometricFunctionFeatures_sin = (Core.Name "sin")

_TrigonometricFunctionFeatures_tan = (Core.Name "tan")

-- | Vector functions
data VectorFunctionFeatures = 
  VectorFunctionFeatures {
    -- | The vector.similarity.cosine() function. Returns a FLOAT representing the similarity between the argument vectors based on their cosine.
    vectorFunctionFeaturesVector_similarity_cosine :: Bool,
    -- | The vector.similarity.euclidean() function. Returns a FLOAT representing the similarity between the argument vectors based on their Euclidean distance.
    vectorFunctionFeaturesVector_similarity_euclidean :: Bool}
  deriving (Eq, Ord, Read, Show)

_VectorFunctionFeatures = (Core.Name "hydra.ext/cypher/features.VectorFunctionFeatures")

_VectorFunctionFeatures_vector_similarity_cosine = (Core.Name "vector.similarity.cosine")

_VectorFunctionFeatures_vector_similarity_euclidean = (Core.Name "vector.similarity.euclidean")

-- | List functionality
data ListFeatures = 
  ListFeatures {
    -- | Basic list comprehensions
    listFeaturesListComprehension :: Bool,
    -- | List range comprehensions (e.g. [1..10])
    listFeaturesListRange :: Bool}
  deriving (Eq, Ord, Read, Show)

_ListFeatures = (Core.Name "hydra.ext/cypher/features.ListFeatures")

_ListFeatures_listComprehension = (Core.Name "listComprehension")

_ListFeatures_listRange = (Core.Name "listRange")

-- | Various types of literal values
data LiteralFeatures = 
  LiteralFeatures {
    -- | Boolean literals (note: included by most if not all implementations).
    literalFeaturesBoolean :: Bool,
    -- | Double-precision floating-point literals
    literalFeaturesDouble :: Bool,
    -- | Integer literals
    literalFeaturesInteger :: Bool,
    -- | List literals
    literalFeaturesList :: Bool,
    -- | Map literals
    literalFeaturesMap :: Bool,
    -- | The NULL literal
    literalFeaturesNull :: Bool,
    -- | String literals (note: included by most if not all implementations).
    literalFeaturesString :: Bool}
  deriving (Eq, Ord, Read, Show)

_LiteralFeatures = (Core.Name "hydra.ext/cypher/features.LiteralFeatures")

_LiteralFeatures_boolean = (Core.Name "boolean")

_LiteralFeatures_double = (Core.Name "double")

_LiteralFeatures_integer = (Core.Name "integer")

_LiteralFeatures_list = (Core.Name "list")

_LiteralFeatures_map = (Core.Name "map")

_LiteralFeatures_null = (Core.Name "null")

_LiteralFeatures_string = (Core.Name "string")

-- | Logical operations
data LogicalFeatures = 
  LogicalFeatures {
    -- | The AND operator
    logicalFeaturesAnd :: Bool,
    -- | The NOT operator
    logicalFeaturesNot :: Bool,
    -- | The OR operator
    logicalFeaturesOr :: Bool,
    -- | The XOR operator
    logicalFeaturesXor :: Bool}
  deriving (Eq, Ord, Read, Show)

_LogicalFeatures = (Core.Name "hydra.ext/cypher/features.LogicalFeatures")

_LogicalFeatures_and = (Core.Name "and")

_LogicalFeatures_not = (Core.Name "not")

_LogicalFeatures_or = (Core.Name "or")

_LogicalFeatures_xor = (Core.Name "xor")

-- | Match queries
data MatchFeatures = 
  MatchFeatures {
    -- | The basic (non-optional) MATCH clause
    matchFeaturesMatch :: Bool,
    -- | OPTIONAL MATCH
    matchFeaturesOptionalMatch :: Bool}
  deriving (Eq, Ord, Read, Show)

_MatchFeatures = (Core.Name "hydra.ext/cypher/features.MatchFeatures")

_MatchFeatures_match = (Core.Name "match")

_MatchFeatures_optionalMatch = (Core.Name "optionalMatch")

-- | Merge operations
data MergeFeatures = 
  MergeFeatures {
    -- | The basic MERGE clause
    mergeFeaturesMerge :: Bool,
    -- | MERGE with the ON CREATE action
    mergeFeaturesMergeOnCreate :: Bool,
    -- | MERGE with the ON MATCH action
    mergeFeaturesMergeOnMatch :: Bool}
  deriving (Eq, Ord, Read, Show)

_MergeFeatures = (Core.Name "hydra.ext/cypher/features.MergeFeatures")

_MergeFeatures_merge = (Core.Name "merge")

_MergeFeatures_mergeOnCreate = (Core.Name "mergeOnCreate")

_MergeFeatures_mergeOnMatch = (Core.Name "mergeOnMatch")

-- | Node patterns
data NodePatternFeatures = 
  NodePatternFeatures {
    -- | Specifying multiple labels in a node pattern
    nodePatternFeaturesMultipleLabels :: Bool,
    -- | Specifying a parameter as part of a node pattern
    nodePatternFeaturesParameter :: Bool,
    -- | Specifying a key/value map of properties in a node pattern
    nodePatternFeaturesPropertyMap :: Bool,
    -- | Binding a variable to a node in a node pattern (note: included by most if not all implementations).
    nodePatternFeaturesVariableNode :: Bool,
    -- | Omitting labels from a node pattern
    nodePatternFeaturesWildcardLabel :: Bool}
  deriving (Eq, Ord, Read, Show)

_NodePatternFeatures = (Core.Name "hydra.ext/cypher/features.NodePatternFeatures")

_NodePatternFeatures_multipleLabels = (Core.Name "multipleLabels")

_NodePatternFeatures_parameter = (Core.Name "parameter")

_NodePatternFeatures_propertyMap = (Core.Name "propertyMap")

_NodePatternFeatures_variableNode = (Core.Name "variableNode")

_NodePatternFeatures_wildcardLabel = (Core.Name "wildcardLabel")

-- | IS NULL / IS NOT NULL checks
data NullFeatures = 
  NullFeatures {
    -- | The IS NULL operator
    nullFeaturesIsNull :: Bool,
    -- | The IS NOT NULL operator
    nullFeaturesIsNotNull :: Bool}
  deriving (Eq, Ord, Read, Show)

_NullFeatures = (Core.Name "hydra.ext/cypher/features.NullFeatures")

_NullFeatures_isNull = (Core.Name "isNull")

_NullFeatures_isNotNull = (Core.Name "isNotNull")

-- | Path functions only found in OpenCypher
data PathFeatures = 
  PathFeatures {
    -- | The shortestPath() function
    pathFeaturesShortestPath :: Bool}
  deriving (Eq, Ord, Read, Show)

_PathFeatures = (Core.Name "hydra.ext/cypher/features.PathFeatures")

_PathFeatures_shortestPath = (Core.Name "shortestPath")

-- | Procedure calls
data ProcedureCallFeatures = 
  ProcedureCallFeatures {
    -- | CALL within a query
    procedureCallFeaturesInQueryCall :: Bool,
    -- | Standalone / top-level CALL
    procedureCallFeaturesStandaloneCall :: Bool,
    -- | The YIELD clause in CALL
    procedureCallFeaturesYield :: Bool}
  deriving (Eq, Ord, Read, Show)

_ProcedureCallFeatures = (Core.Name "hydra.ext/cypher/features.ProcedureCallFeatures")

_ProcedureCallFeatures_inQueryCall = (Core.Name "inQueryCall")

_ProcedureCallFeatures_standaloneCall = (Core.Name "standaloneCall")

_ProcedureCallFeatures_yield = (Core.Name "yield")

-- | Projections
data ProjectionFeatures = 
  ProjectionFeatures {
    -- | The LIMIT clause
    projectionFeaturesLimit :: Bool,
    -- | The ORDER BY clause
    projectionFeaturesOrderBy :: Bool,
    -- | The DISTINCT keyword
    projectionFeaturesProjectDistinct :: Bool,
    -- | The * projection
    projectionFeaturesProjectAll :: Bool,
    -- | The AS keyword
    projectionFeaturesProjectAs :: Bool,
    -- | The SKIP clause
    projectionFeaturesSkip :: Bool,
    -- | The ASC/ASCENDING and DESC/DESCENDING keywords
    projectionFeaturesSortOrder :: Bool}
  deriving (Eq, Ord, Read, Show)

_ProjectionFeatures = (Core.Name "hydra.ext/cypher/features.ProjectionFeatures")

_ProjectionFeatures_limit = (Core.Name "limit")

_ProjectionFeatures_orderBy = (Core.Name "orderBy")

_ProjectionFeatures_projectDistinct = (Core.Name "projectDistinct")

_ProjectionFeatures_projectAll = (Core.Name "projectAll")

_ProjectionFeatures_projectAs = (Core.Name "projectAs")

_ProjectionFeatures_skip = (Core.Name "skip")

_ProjectionFeatures_sortOrder = (Core.Name "sortOrder")

-- | Quantifier expressions
data QuantifierFeatures = 
  QuantifierFeatures {
    -- | The ALL quantifier
    quantifierFeaturesAll :: Bool,
    -- | The ANY quantifier
    quantifierFeaturesAny :: Bool,
    -- | The NONE quantifier
    quantifierFeaturesNone :: Bool,
    -- | The SINGLE quantifier
    quantifierFeaturesSingle :: Bool}
  deriving (Eq, Ord, Read, Show)

_QuantifierFeatures = (Core.Name "hydra.ext/cypher/features.QuantifierFeatures")

_QuantifierFeatures_all = (Core.Name "all")

_QuantifierFeatures_any = (Core.Name "any")

_QuantifierFeatures_none = (Core.Name "none")

_QuantifierFeatures_single = (Core.Name "single")

-- | Range literals within relationship patterns
data RangeLiteralFeatures = 
  RangeLiteralFeatures {
    -- | Range literals with both lower and upper bounds
    rangeLiteralFeaturesBounds :: Bool,
    -- | Range literals providing an exact number of repetitions
    rangeLiteralFeaturesExactRange :: Bool,
    -- | Range literals with a lower bound (only)
    rangeLiteralFeaturesLowerBound :: Bool,
    -- | The * range literal
    rangeLiteralFeaturesStarRange :: Bool,
    -- | Range literals with an upper bound (only)
    rangeLiteralFeaturesUpperBound :: Bool}
  deriving (Eq, Ord, Read, Show)

_RangeLiteralFeatures = (Core.Name "hydra.ext/cypher/features.RangeLiteralFeatures")

_RangeLiteralFeatures_bounds = (Core.Name "bounds")

_RangeLiteralFeatures_exactRange = (Core.Name "exactRange")

_RangeLiteralFeatures_lowerBound = (Core.Name "lowerBound")

_RangeLiteralFeatures_starRange = (Core.Name "starRange")

_RangeLiteralFeatures_upperBound = (Core.Name "upperBound")

-- | Specific syntax related to reading data from the graph.
data ReadingFeatures = 
  ReadingFeatures {
    -- | The UNION operator
    readingFeaturesUnion :: Bool,
    -- | The UNION ALL operator
    readingFeaturesUnionAll :: Bool,
    -- | The UNWIND clause
    readingFeaturesUnwind :: Bool}
  deriving (Eq, Ord, Read, Show)

_ReadingFeatures = (Core.Name "hydra.ext/cypher/features.ReadingFeatures")

_ReadingFeatures_union = (Core.Name "union")

_ReadingFeatures_unionAll = (Core.Name "unionAll")

_ReadingFeatures_unwind = (Core.Name "unwind")

-- | Relationship directions / arrow patterns
data RelationshipDirectionFeatures = 
  RelationshipDirectionFeatures {
    -- | The two-headed arrow (<-[]->) relationship direction
    relationshipDirectionFeaturesBoth :: Bool,
    -- | The left arrow (<-[]-) relationship direction
    relationshipDirectionFeaturesLeft :: Bool,
    -- | The headless arrow (-[]-) relationship direction
    relationshipDirectionFeaturesNeither :: Bool,
    -- | The right arrow (-[]->) relationship direction
    relationshipDirectionFeaturesRight :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipDirectionFeatures = (Core.Name "hydra.ext/cypher/features.RelationshipDirectionFeatures")

_RelationshipDirectionFeatures_both = (Core.Name "both")

_RelationshipDirectionFeatures_left = (Core.Name "left")

_RelationshipDirectionFeatures_neither = (Core.Name "neither")

_RelationshipDirectionFeatures_right = (Core.Name "right")

-- | Relationship patterns
data RelationshipPatternFeatures = 
  RelationshipPatternFeatures {
    -- | Specifying a disjunction of multiple types in a relationship pattern
    relationshipPatternFeaturesMultipleTypes :: Bool,
    -- | Binding a variable to a relationship in a relationship pattern (note: included by most if not all implementations).
    relationshipPatternFeaturesVariableRelationship :: Bool,
    -- | Omitting types from a relationship pattern
    relationshipPatternFeaturesWildcardType :: Bool}
  deriving (Eq, Ord, Read, Show)

_RelationshipPatternFeatures = (Core.Name "hydra.ext/cypher/features.RelationshipPatternFeatures")

_RelationshipPatternFeatures_multipleTypes = (Core.Name "multipleTypes")

_RelationshipPatternFeatures_variableRelationship = (Core.Name "variableRelationship")

_RelationshipPatternFeatures_wildcardType = (Core.Name "wildcardType")

-- | REMOVE operations
data RemoveFeatures = 
  RemoveFeatures {
    -- | REMOVE Variable:NodeLabels
    removeFeaturesByLabel :: Bool,
    -- | REMOVE PropertyExpression
    removeFeaturesByProperty :: Bool}
  deriving (Eq, Ord, Read, Show)

_RemoveFeatures = (Core.Name "hydra.ext/cypher/features.RemoveFeatures")

_RemoveFeatures_byLabel = (Core.Name "byLabel")

_RemoveFeatures_byProperty = (Core.Name "byProperty")

-- | Set definitions
data SetFeatures = 
  SetFeatures {
    -- | Defining a set using PropertyExpression = Expression
    setFeaturesPropertyEquals :: Bool,
    -- | Defining a set using Variable = Expression
    setFeaturesVariableEquals :: Bool,
    -- | Defining a set using Variable += Expression
    setFeaturesVariablePlusEquals :: Bool,
    -- | Defining a set using Variable:NodeLabels
    setFeaturesVariableWithNodeLabels :: Bool}
  deriving (Eq, Ord, Read, Show)

_SetFeatures = (Core.Name "hydra.ext/cypher/features.SetFeatures")

_SetFeatures_propertyEquals = (Core.Name "propertyEquals")

_SetFeatures_variableEquals = (Core.Name "variableEquals")

_SetFeatures_variablePlusEquals = (Core.Name "variablePlusEquals")

_SetFeatures_variableWithNodeLabels = (Core.Name "variableWithNodeLabels")

-- | String functions/keywords only found in OpenCypher
data StringFeatures = 
  StringFeatures {
    -- | The contains() function / CONTAINS
    stringFeaturesContains :: Bool,
    -- | The endsWith() function / ENDS WITH
    stringFeaturesEndsWith :: Bool,
    -- | The in() function / IN
    stringFeaturesIn :: Bool,
    -- | The startsWith() function / STARTS WITH
    stringFeaturesStartsWith :: Bool}
  deriving (Eq, Ord, Read, Show)

_StringFeatures = (Core.Name "hydra.ext/cypher/features.StringFeatures")

_StringFeatures_contains = (Core.Name "contains")

_StringFeatures_endsWith = (Core.Name "endsWith")

_StringFeatures_in = (Core.Name "in")

_StringFeatures_startsWith = (Core.Name "startsWith")

-- | Specific syntax related to updating data in the graph
data UpdatingFeatures = 
  UpdatingFeatures {
    -- | The CREATE clause
    updatingFeaturesCreate :: Bool,
    -- | The SET clause
    updatingFeaturesSet :: Bool,
    -- | Multi-part queries using WITH
    updatingFeaturesWith :: Bool}
  deriving (Eq, Ord, Read, Show)

_UpdatingFeatures = (Core.Name "hydra.ext/cypher/features.UpdatingFeatures")

_UpdatingFeatures_create = (Core.Name "create")

_UpdatingFeatures_set = (Core.Name "set")

_UpdatingFeatures_with = (Core.Name "with")
