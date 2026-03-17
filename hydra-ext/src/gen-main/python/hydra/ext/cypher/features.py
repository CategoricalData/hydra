# Note: this is an automatically generated file. Do not edit.

r"""A model for characterizing OpenCypher queries and implementations in terms of included features.Based on the OpenCypher grammar and the list of standard Cypher functions at https://neo4j.com/docs/cypher-manual/current/functions. Current as of August 2024."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from typing import Annotated, TypeAlias, cast
import hydra.core

@dataclass(frozen=True)
class CypherFeatures:
    r"""A set of features which characterize an OpenCypher query or implementation. Any features which are omitted from the set are assumed to be unsupported or nonrequired."""
    
    arithmetic: Annotated[ArithmeticFeatures, "Arithmetic operations"]
    atom: Annotated[AtomFeatures, "Various kinds of atomic expressions"]
    comparison: Annotated[ComparisonFeatures, "Comparison operators and functions"]
    delete: Annotated[DeleteFeatures, "Delete operations"]
    function: Annotated[FunctionFeatures, "Standard Cypher functions"]
    list: Annotated[ListFeatures, "List functionality"]
    literal: Annotated[LiteralFeatures, "Various types of literal values"]
    logical: Annotated[LogicalFeatures, "Logical operations"]
    match: Annotated[MatchFeatures, "Match queries"]
    merge: Annotated[MergeFeatures, "Merge operations"]
    node_pattern: Annotated[NodePatternFeatures, "Node patterns"]
    null: Annotated[NullFeatures, "IS NULL / IS NOT NULL checks"]
    path: Annotated[PathFeatures, "Path functions only found in OpenCypher"]
    procedure_call: Annotated[ProcedureCallFeatures, "Procedure calls"]
    projection: Annotated[ProjectionFeatures, "Projections"]
    quantifier: Annotated[QuantifierFeatures, "Quantifier expressions"]
    range_literal: Annotated[RangeLiteralFeatures, "Range literals within relationship patterns"]
    reading: Annotated[ReadingFeatures, "Specific syntax related to reading data from the graph."]
    relationship_direction: Annotated[RelationshipDirectionFeatures, "Relationship directions / arrow patterns"]
    relationship_pattern: Annotated[RelationshipPatternFeatures, "Relationship patterns"]
    remove: Annotated[RemoveFeatures, "REMOVE operations"]
    set: Annotated[SetFeatures, "Set definitions"]
    string: Annotated[StringFeatures, "String functions/keywords only found in OpenCypher"]
    updating: Annotated[UpdatingFeatures, "Specific syntax related to updating data in the graph"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.CypherFeatures")
    ARITHMETIC = hydra.core.Name("arithmetic")
    ATOM = hydra.core.Name("atom")
    COMPARISON = hydra.core.Name("comparison")
    DELETE = hydra.core.Name("delete")
    FUNCTION = hydra.core.Name("function")
    LIST = hydra.core.Name("list")
    LITERAL = hydra.core.Name("literal")
    LOGICAL = hydra.core.Name("logical")
    MATCH = hydra.core.Name("match")
    MERGE = hydra.core.Name("merge")
    NODE_PATTERN = hydra.core.Name("nodePattern")
    NULL = hydra.core.Name("null")
    PATH = hydra.core.Name("path")
    PROCEDURE_CALL = hydra.core.Name("procedureCall")
    PROJECTION = hydra.core.Name("projection")
    QUANTIFIER = hydra.core.Name("quantifier")
    RANGE_LITERAL = hydra.core.Name("rangeLiteral")
    READING = hydra.core.Name("reading")
    RELATIONSHIP_DIRECTION = hydra.core.Name("relationshipDirection")
    RELATIONSHIP_PATTERN = hydra.core.Name("relationshipPattern")
    REMOVE = hydra.core.Name("remove")
    SET = hydra.core.Name("set")
    STRING = hydra.core.Name("string")
    UPDATING = hydra.core.Name("updating")

@dataclass(frozen=True)
class ArithmeticFeatures:
    r"""Arithmetic operations."""
    
    plus: Annotated[bool, "The + operator"]
    minus: Annotated[bool, "The - operator"]
    multiply: Annotated[bool, "The * operator"]
    divide: Annotated[bool, "The / operator"]
    modulus: Annotated[bool, "The % operator"]
    power_of: Annotated[bool, "The ^ operator"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ArithmeticFeatures")
    PLUS = hydra.core.Name("plus")
    MINUS = hydra.core.Name("minus")
    MULTIPLY = hydra.core.Name("multiply")
    DIVIDE = hydra.core.Name("divide")
    MODULUS = hydra.core.Name("modulus")
    POWER_OF = hydra.core.Name("powerOf")

@dataclass(frozen=True)
class AtomFeatures:
    r"""Various kinds of atomic expressions."""
    
    case_expression: Annotated[bool, "CASE expressions"]
    count: Annotated[bool, "The COUNT (*) expression"]
    existential_subquery: Annotated[bool, "Existential subqueries"]
    function_invocation: Annotated[bool, "Function invocation"]
    parameter: Annotated[bool, "Parameter expressions"]
    pattern_comprehension: Annotated[bool, "Pattern comprehensions"]
    pattern_predicate: Annotated[bool, "Relationship patterns as subexpressions"]
    variable: Annotated[bool, "Variable expressions (note: included by most if not all implementations)."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.AtomFeatures")
    CASE_EXPRESSION = hydra.core.Name("caseExpression")
    COUNT = hydra.core.Name("count")
    EXISTENTIAL_SUBQUERY = hydra.core.Name("existentialSubquery")
    FUNCTION_INVOCATION = hydra.core.Name("functionInvocation")
    PARAMETER = hydra.core.Name("parameter")
    PATTERN_COMPREHENSION = hydra.core.Name("patternComprehension")
    PATTERN_PREDICATE = hydra.core.Name("patternPredicate")
    VARIABLE = hydra.core.Name("variable")

@dataclass(frozen=True)
class ComparisonFeatures:
    r"""Comparison operators and functions."""
    
    equal: Annotated[bool, "The = comparison operator"]
    greater_than: Annotated[bool, "The > comparison operator"]
    greater_than_or_equal: Annotated[bool, "The >= comparison operator"]
    less_than: Annotated[bool, "The < comparison operator"]
    less_than_or_equal: Annotated[bool, "The <= comparison operator"]
    not_equal: Annotated[bool, "The <> comparison operator"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ComparisonFeatures")
    EQUAL = hydra.core.Name("equal")
    GREATER_THAN = hydra.core.Name("greaterThan")
    GREATER_THAN_OR_EQUAL = hydra.core.Name("greaterThanOrEqual")
    LESS_THAN = hydra.core.Name("lessThan")
    LESS_THAN_OR_EQUAL = hydra.core.Name("lessThanOrEqual")
    NOT_EQUAL = hydra.core.Name("notEqual")

@dataclass(frozen=True)
class DeleteFeatures:
    r"""Delete operations."""
    
    delete: Annotated[bool, "The basic DELETE clause"]
    detach_delete: Annotated[bool, "The DETACH DELETE clause"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.DeleteFeatures")
    DELETE = hydra.core.Name("delete")
    DETACH_DELETE = hydra.core.Name("detachDelete")

@dataclass(frozen=True)
class FunctionFeatures:
    r"""Standard Cypher functions."""
    
    aggregate_function: Annotated[AggregateFunctionFeatures, "Aggregate functions"]
    database_function: Annotated[DatabaseFunctionFeatures, "Database functions"]
    gen_a_i_function: Annotated[GenAIFunctionFeatures, "GenAI functions"]
    graph_function: Annotated[GraphFunctionFeatures, "Graph functions"]
    list_function: Annotated[ListFunctionFeatures, "List functions"]
    load_c_s_v_function: Annotated[LoadCSVFunctionFeatures, "Load CSV functions"]
    logarithmic_function: Annotated[LogarithmicFunctionFeatures, "Logarithmic functions"]
    numeric_function: Annotated[NumericFunctionFeatures, "Numeric functions"]
    predicate_function: Annotated[PredicateFunctionFeatures, "Predicate functions"]
    scalar_function: Annotated[ScalarFunctionFeatures, "Scalar functions"]
    spatial_function: Annotated[SpatialFunctionFeatures, "Spatial functions"]
    string_function: Annotated[StringFunctionFeatures, "String functions"]
    temporal_duration_function: Annotated[TemporalDurationFunctionFeatures, "Temporal duration functions"]
    temporal_instant_function: Annotated[TemporalInstantFunctionFeatures, "Temporal instant functions"]
    trigonometric_function: Annotated[TrigonometricFunctionFeatures, "Trigonometric functions"]
    vector_function: Annotated[VectorFunctionFeatures, "Vector functions"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.FunctionFeatures")
    AGGREGATE_FUNCTION = hydra.core.Name("aggregateFunction")
    DATABASE_FUNCTION = hydra.core.Name("databaseFunction")
    GEN_A_I_FUNCTION = hydra.core.Name("genAIFunction")
    GRAPH_FUNCTION = hydra.core.Name("graphFunction")
    LIST_FUNCTION = hydra.core.Name("listFunction")
    LOAD_C_S_V_FUNCTION = hydra.core.Name("loadCSVFunction")
    LOGARITHMIC_FUNCTION = hydra.core.Name("logarithmicFunction")
    NUMERIC_FUNCTION = hydra.core.Name("numericFunction")
    PREDICATE_FUNCTION = hydra.core.Name("predicateFunction")
    SCALAR_FUNCTION = hydra.core.Name("scalarFunction")
    SPATIAL_FUNCTION = hydra.core.Name("spatialFunction")
    STRING_FUNCTION = hydra.core.Name("stringFunction")
    TEMPORAL_DURATION_FUNCTION = hydra.core.Name("temporalDurationFunction")
    TEMPORAL_INSTANT_FUNCTION = hydra.core.Name("temporalInstantFunction")
    TRIGONOMETRIC_FUNCTION = hydra.core.Name("trigonometricFunction")
    VECTOR_FUNCTION = hydra.core.Name("vectorFunction")

@dataclass(frozen=True)
class AggregateFunctionFeatures:
    r"""Aggregate functions."""
    
    avg: Annotated[bool, "The avg() function / AVG. Returns the average of a set of DURATION values.; Returns the average of a set of FLOAT values.; Returns the average of a set of INTEGER values."]
    collect: Annotated[bool, "The collect() function / COLLECT. Returns a list containing the values returned by an expression."]
    count: Annotated[bool, "The count() function / COUNT. Returns the number of values or rows."]
    max: Annotated[bool, "The max() function / MAX. Returns the maximum value in a set of values."]
    min: Annotated[bool, "The min() function / MIN. Returns the minimum value in a set of values."]
    percentile_cont: Annotated[bool, "The percentileCont() function. Returns the percentile of a value over a group using linear interpolation."]
    percentile_disc: Annotated[bool, "The percentileDisc() function. Returns the nearest FLOAT value to the given percentile over a group using a rounding method.; Returns the nearest INTEGER value to the given percentile over a group using a rounding method."]
    stdev: Annotated[bool, "The stdev() function. Returns the standard deviation for the given value over a group for a sample of a population."]
    stdevp: Annotated[bool, "The stdevp() function. Returns the standard deviation for the given value over a group for an entire population."]
    sum: Annotated[bool, "The sum() function / SUM. Returns the sum of a set of DURATION values.; Returns the sum of a set of FLOAT values.; Returns the sum of a set of INTEGER values."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.AggregateFunctionFeatures")
    AVG = hydra.core.Name("avg")
    COLLECT = hydra.core.Name("collect")
    COUNT = hydra.core.Name("count")
    MAX = hydra.core.Name("max")
    MIN = hydra.core.Name("min")
    PERCENTILE_CONT = hydra.core.Name("percentileCont")
    PERCENTILE_DISC = hydra.core.Name("percentileDisc")
    STDEV = hydra.core.Name("stdev")
    STDEVP = hydra.core.Name("stdevp")
    SUM = hydra.core.Name("sum")

@dataclass(frozen=True)
class DatabaseFunctionFeatures:
    r"""Database functions."""
    
    name_from_element_id: Annotated[bool, "The db.nameFromElementId() function. Resolves the database name from the given element id. Introduced in 5.12."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.DatabaseFunctionFeatures")
    DB.NAME_FROM_ELEMENT_ID = hydra.core.Name("db.nameFromElementId")

@dataclass(frozen=True)
class GenAIFunctionFeatures:
    r"""GenAI functions."""
    
    encode: Annotated[bool, "The genai.vector.encode() function. Encode a given resource as a vector using the named provider. Introduced in 5.17."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.GenAIFunctionFeatures")
    GENAI.VECTOR.ENCODE = hydra.core.Name("genai.vector.encode")

@dataclass(frozen=True)
class GraphFunctionFeatures:
    r"""Graph functions."""
    
    by_element_id: Annotated[bool, "The graph.byElementId() function. Resolves the constituent graph to which a given element id belongs. Introduced in 5.13."]
    by_name: Annotated[bool, "The graph.byName() function. Resolves a constituent graph by name."]
    names: Annotated[bool, "The graph.names() function. Returns a list containing the names of all graphs in the current composite database."]
    properties_by_name: Annotated[bool, "The graph.propertiesByName() function. Returns a map containing the properties associated with the given graph."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.GraphFunctionFeatures")
    GRAPH.BY_ELEMENT_ID = hydra.core.Name("graph.byElementId")
    GRAPH.BY_NAME = hydra.core.Name("graph.byName")
    GRAPH.NAMES = hydra.core.Name("graph.names")
    GRAPH.PROPERTIES_BY_NAME = hydra.core.Name("graph.propertiesByName")

@dataclass(frozen=True)
class ListFunctionFeatures:
    r"""List functions."""
    
    keys: Annotated[bool, "The keys() function. Returns a LIST<STRING> containing the STRING representations for all the property names of a MAP.; Returns a LIST<STRING> containing the STRING representations for all the property names of a NODE.; Returns a LIST<STRING> containing the STRING representations for all the property names of a RELATIONSHIP."]
    labels: Annotated[bool, "The labels() function. Returns a LIST<STRING> containing the STRING representations for all the labels of a NODE."]
    nodes: Annotated[bool, "The nodes() function. Returns a LIST<NODE> containing all the NODE values in a PATH."]
    range_: Annotated[bool, "The range() function. Returns a LIST<INTEGER> comprising all INTEGER values within a specified range.; Returns a LIST<INTEGER> comprising all INTEGER values within a specified range created with step length."]
    reduce: Annotated[bool, "The reduce() function. Runs an expression against individual elements of a LIST<ANY>, storing the result of the expression in an accumulator."]
    relationships: Annotated[bool, "The relationships() function. Returns a LIST<RELATIONSHIP> containing all the RELATIONSHIP values in a PATH."]
    reverse: Annotated[bool, "The reverse() function. Returns a LIST<ANY> in which the order of all elements in the given LIST<ANY> have been reversed."]
    tail: Annotated[bool, "The tail() function. Returns all but the first element in a LIST<ANY>."]
    to_boolean_list: Annotated[bool, "The toBooleanList() function. Converts a LIST<ANY> of values to a LIST<BOOLEAN> values. If any values are not convertible to BOOLEAN they will be null in the LIST<BOOLEAN> returned."]
    to_float_list: Annotated[bool, "The toFloatList() function. Converts a LIST<ANY> to a LIST<FLOAT> values. If any values are not convertible to FLOAT they will be null in the LIST<FLOAT> returned."]
    to_integer_list: Annotated[bool, "The toIntegerList() function. Converts a LIST<ANY> to a LIST<INTEGER> values. If any values are not convertible to INTEGER they will be null in the LIST<INTEGER> returned."]
    to_string_list: Annotated[bool, "The toStringList() function. Converts a LIST<ANY> to a LIST<STRING> values. If any values are not convertible to STRING they will be null in the LIST<STRING> returned."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ListFunctionFeatures")
    KEYS = hydra.core.Name("keys")
    LABELS = hydra.core.Name("labels")
    NODES = hydra.core.Name("nodes")
    RANGE = hydra.core.Name("range")
    REDUCE = hydra.core.Name("reduce")
    RELATIONSHIPS = hydra.core.Name("relationships")
    REVERSE = hydra.core.Name("reverse")
    TAIL = hydra.core.Name("tail")
    TO_BOOLEAN_LIST = hydra.core.Name("toBooleanList")
    TO_FLOAT_LIST = hydra.core.Name("toFloatList")
    TO_INTEGER_LIST = hydra.core.Name("toIntegerList")
    TO_STRING_LIST = hydra.core.Name("toStringList")

@dataclass(frozen=True)
class LoadCSVFunctionFeatures:
    r"""Load CSV functions."""
    
    file: Annotated[bool, "The file() function. Returns the absolute path of the file that LOAD CSV is using."]
    linenumber: Annotated[bool, "The linenumber() function. Returns the line number that LOAD CSV is currently using."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.LoadCSVFunctionFeatures")
    FILE = hydra.core.Name("file")
    LINENUMBER = hydra.core.Name("linenumber")

@dataclass(frozen=True)
class LogarithmicFunctionFeatures:
    r"""Logarithmic functions."""
    
    e: Annotated[bool, "The e() function. Returns the base of the natural logarithm, e."]
    exp: Annotated[bool, "The exp() function. Returns e^n, where e is the base of the natural logarithm, and n is the value of the argument expression."]
    log: Annotated[bool, "The log() function. Returns the natural logarithm of a FLOAT."]
    log10: Annotated[bool, "The log10() function. Returns the common logarithm (base 10) of a FLOAT."]
    sqrt: Annotated[bool, "The sqrt() function. Returns the square root of a FLOAT."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.LogarithmicFunctionFeatures")
    E = hydra.core.Name("e")
    EXP = hydra.core.Name("exp")
    LOG = hydra.core.Name("log")
    LOG10 = hydra.core.Name("log10")
    SQRT = hydra.core.Name("sqrt")

@dataclass(frozen=True)
class NumericFunctionFeatures:
    r"""Numeric functions."""
    
    abs: Annotated[bool, "The abs() function. Returns the absolute value of a FLOAT.; Returns the absolute value of an INTEGER."]
    ceil: Annotated[bool, "The ceil() function. Returns the smallest FLOAT that is greater than or equal to a number and equal to an INTEGER."]
    floor: Annotated[bool, "The floor() function. Returns the largest FLOAT that is less than or equal to a number and equal to an INTEGER."]
    is_na_n: Annotated[bool, "The isNaN() function. Returns true if the floating point number is NaN.; Returns true if the integer number is NaN."]
    rand: Annotated[bool, "The rand() function. Returns a random FLOAT in the range from 0 (inclusive) to 1 (exclusive)."]
    round: Annotated[bool, "The round() function. Returns the value of a number rounded to the nearest INTEGER.; Returns the value of a number rounded to the specified precision using rounding mode HALF_UP.; Returns the value of a number rounded to the specified precision with the specified rounding mode."]
    sign: Annotated[bool, "The sign() function. Returns the signum of a FLOAT: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.; Returns the signum of an INTEGER: 0 if the number is 0, -1 for any negative number, and 1 for any positive number."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.NumericFunctionFeatures")
    ABS = hydra.core.Name("abs")
    CEIL = hydra.core.Name("ceil")
    FLOOR = hydra.core.Name("floor")
    IS_NA_N = hydra.core.Name("isNaN")
    RAND = hydra.core.Name("rand")
    ROUND = hydra.core.Name("round")
    SIGN = hydra.core.Name("sign")

@dataclass(frozen=True)
class PredicateFunctionFeatures:
    r"""Predicate functions."""
    
    all: Annotated[bool, "The all() function. Returns true if the predicate holds for all elements in the given LIST<ANY>."]
    any: Annotated[bool, "The any() function. Returns true if the predicate holds for at least one element in the given LIST<ANY>."]
    exists: Annotated[bool, "The exists() function. Returns true if a match for the pattern exists in the graph."]
    is_empty: Annotated[bool, "The isEmpty() function. Checks whether a LIST<ANY> is empty.; Checks whether a MAP is empty.; Checks whether a STRING is empty."]
    none: Annotated[bool, "The none() function. Returns true if the predicate holds for no element in the given LIST<ANY>."]
    single: Annotated[bool, "The single() function. Returns true if the predicate holds for exactly one of the elements in the given LIST<ANY>."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.PredicateFunctionFeatures")
    ALL = hydra.core.Name("all")
    ANY = hydra.core.Name("any")
    EXISTS = hydra.core.Name("exists")
    IS_EMPTY = hydra.core.Name("isEmpty")
    NONE = hydra.core.Name("none")
    SINGLE = hydra.core.Name("single")

@dataclass(frozen=True)
class ScalarFunctionFeatures:
    r"""Scalar functions."""
    
    char_length: Annotated[bool, "The char_length() function. Returns the number of Unicode characters in a STRING."]
    character_length: Annotated[bool, "The character_length() function. Returns the number of Unicode characters in a STRING."]
    coalesce: Annotated[bool, "The coalesce() function. Returns the first non-null value in a list of expressions."]
    element_id: Annotated[bool, "The elementId() function. Returns a node identifier, unique within a specific transaction and DBMS.; Returns a relationship identifier, unique within a specific transaction and DBMS."]
    end_node: Annotated[bool, "The endNode() function. Returns a relationship identifier, unique within a specific transaction and DBMS."]
    head: Annotated[bool, "The head() function. Returns the first element in a LIST<ANY>."]
    id: Annotated[bool, "The id() function. [Deprecated] Returns the id of a NODE. Replaced by elementId().; [Deprecated] Returns the id of a RELATIONSHIP. Replaced by elementId()."]
    last: Annotated[bool, "The last() function. Returns the last element in a LIST<ANY>."]
    length: Annotated[bool, "The length() function. Returns the length of a PATH."]
    null_if: Annotated[bool, "The nullIf() function. Returns null if the two given parameters are equivalent, otherwise returns the value of the first parameter."]
    properties: Annotated[bool, "The properties() function. Returns a MAP containing all the properties of a MAP.; Returns a MAP containing all the properties of a NODE.; Returns a MAP containing all the properties of a RELATIONSHIP."]
    random_u_u_i_d: Annotated[bool, "The randomUUID() function. Generates a random UUID."]
    size: Annotated[bool, "The size() function. Returns the number of items in a LIST<ANY>.; Returns the number of Unicode characters in a STRING."]
    start_node: Annotated[bool, "The startNode() function. Returns the start NODE of a RELATIONSHIP."]
    to_boolean: Annotated[bool, "The toBoolean() function. Converts a STRING value to a BOOLEAN value.; Converts a BOOLEAN value to a BOOLEAN value.; Converts an INTEGER value to a BOOLEAN value."]
    to_boolean_or_null: Annotated[bool, "The toBooleanOrNull() function. Converts a value to a BOOLEAN value, or null if the value cannot be converted."]
    to_float: Annotated[bool, "The toFloat() function. Converts an INTEGER value to a FLOAT value.; Converts a STRING value to a FLOAT value."]
    to_float_or_null: Annotated[bool, "The toFloatOrNull() function. Converts a value to a FLOAT value, or null if the value cannot be converted."]
    to_integer: Annotated[bool, "The toInteger() function. Converts a FLOAT value to an INTEGER value.; Converts a BOOLEAN value to an INTEGER value.; Converts a STRING value to an INTEGER value."]
    to_integer_or_null: Annotated[bool, "The toIntegerOrNull() function. Converts a value to an INTEGER value, or null if the value cannot be converted."]
    type: Annotated[bool, "The type() function. Returns a STRING representation of the RELATIONSHIP type."]
    value_type: Annotated[bool, "The valueType() function. Returns a STRING representation of the most precise value type that the given expression evaluates to."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ScalarFunctionFeatures")
    CHAR_LENGTH = hydra.core.Name("char_length")
    CHARACTER_LENGTH = hydra.core.Name("character_length")
    COALESCE = hydra.core.Name("coalesce")
    ELEMENT_ID = hydra.core.Name("elementId")
    END_NODE = hydra.core.Name("endNode")
    HEAD = hydra.core.Name("head")
    ID = hydra.core.Name("id")
    LAST = hydra.core.Name("last")
    LENGTH = hydra.core.Name("length")
    NULL_IF = hydra.core.Name("nullIf")
    PROPERTIES = hydra.core.Name("properties")
    RANDOM_U_U_I_D = hydra.core.Name("randomUUID")
    SIZE = hydra.core.Name("size")
    START_NODE = hydra.core.Name("startNode")
    TO_BOOLEAN = hydra.core.Name("toBoolean")
    TO_BOOLEAN_OR_NULL = hydra.core.Name("toBooleanOrNull")
    TO_FLOAT = hydra.core.Name("toFloat")
    TO_FLOAT_OR_NULL = hydra.core.Name("toFloatOrNull")
    TO_INTEGER = hydra.core.Name("toInteger")
    TO_INTEGER_OR_NULL = hydra.core.Name("toIntegerOrNull")
    TYPE = hydra.core.Name("type")
    VALUE_TYPE = hydra.core.Name("valueType")

@dataclass(frozen=True)
class SpatialFunctionFeatures:
    r"""Spatial functions."""
    
    distance: Annotated[bool, "The point.distance() function. Returns a FLOAT representing the geodesic distance between any two points in the same CRS."]
    point: Annotated[bool, "The point() function. Returns a 2D point object, given two coordinate values in the Cartesian coordinate system.; Returns a 3D point object, given three coordinate values in the Cartesian coordinate system.; Returns a 2D point object, given two coordinate values in the WGS 84 geographic coordinate system.; Returns a 3D point object, given three coordinate values in the WGS 84 geographic coordinate system."]
    within_b_box: Annotated[bool, "The point.withinBBox() function. Returns true if the provided point is within the bounding box defined by the two provided points, lowerLeft and upperRight."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.SpatialFunctionFeatures")
    POINT.DISTANCE = hydra.core.Name("point.distance")
    POINT = hydra.core.Name("point")
    POINT.WITHIN_B_BOX = hydra.core.Name("point.withinBBox")

@dataclass(frozen=True)
class StringFunctionFeatures:
    r"""String functions."""
    
    btrim: Annotated[bool, "The btrim() function. Returns the given STRING with leading and trailing whitespace removed.; Returns the given STRING with leading and trailing trimCharacterString characters removed. Introduced in 5.20."]
    left: Annotated[bool, "The left() function. Returns a STRING containing the specified number (INTEGER) of leftmost characters in the given STRING."]
    lower: Annotated[bool, "The lower() function. Returns the given STRING in lowercase. This function is an alias to the toLower() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21."]
    ltrim: Annotated[bool, "The ltrim() function. Returns the given STRING with leading whitespace removed.; Returns the given STRING with leading trimCharacterString characters removed. Introduced in 5.20."]
    normalize: Annotated[bool, "The normalize() function. Returns the given STRING normalized according to the normalization CypherFunctionForm NFC. Introduced in 5.17.; Returns the given STRING normalized according to the specified normalization CypherFunctionForm. Introduced in 5.17."]
    replace: Annotated[bool, "The replace() function. Returns a STRING in which all occurrences of a specified search STRING in the given STRING have been replaced by another (specified) replacement STRING."]
    reverse: Annotated[bool, "The reverse() function. Returns a STRING in which the order of all characters in the given STRING have been reversed."]
    right: Annotated[bool, "The right() function. Returns a STRING containing the specified number of rightmost characters in the given STRING."]
    rtrim: Annotated[bool, "The rtrim() function. Returns the given STRING with trailing whitespace removed.; Returns the given STRING with trailing trimCharacterString characters removed. Introduced in 5.20."]
    split: Annotated[bool, "The split() function. Returns a LIST<STRING> resulting from the splitting of the given STRING around matches of the given delimiter.; Returns a LIST<STRING> resulting from the splitting of the given STRING around matches of any of the given delimiters."]
    substring: Annotated[bool, "The substring() function. Returns a substring of the given STRING, beginning with a 0-based index start.; Returns a substring of a given length from the given STRING, beginning with a 0-based index start."]
    to_lower: Annotated[bool, "The toLower() function. Returns the given STRING in lowercase."]
    to_string: Annotated[bool, "The toString() function. Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING."]
    to_string_or_null: Annotated[bool, "The toStringOrNull() function. Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING, or null if the value cannot be converted."]
    to_upper: Annotated[bool, "The toUpper() function. Returns the given STRING in uppercase."]
    trim: Annotated[bool, "The trim() function. Returns the given STRING with leading and trailing whitespace removed.; Returns the given STRING with the leading and/or trailing trimCharacterString character removed. Introduced in 5.20."]
    upper: Annotated[bool, "The upper() function. Returns the given STRING in uppercase. This function is an alias to the toUpper() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.StringFunctionFeatures")
    BTRIM = hydra.core.Name("btrim")
    LEFT = hydra.core.Name("left")
    LOWER = hydra.core.Name("lower")
    LTRIM = hydra.core.Name("ltrim")
    NORMALIZE = hydra.core.Name("normalize")
    REPLACE = hydra.core.Name("replace")
    REVERSE = hydra.core.Name("reverse")
    RIGHT = hydra.core.Name("right")
    RTRIM = hydra.core.Name("rtrim")
    SPLIT = hydra.core.Name("split")
    SUBSTRING = hydra.core.Name("substring")
    TO_LOWER = hydra.core.Name("toLower")
    TO_STRING = hydra.core.Name("toString")
    TO_STRING_OR_NULL = hydra.core.Name("toStringOrNull")
    TO_UPPER = hydra.core.Name("toUpper")
    TRIM = hydra.core.Name("trim")
    UPPER = hydra.core.Name("upper")

@dataclass(frozen=True)
class TemporalDurationFunctionFeatures:
    r"""Temporal duration functions."""
    
    duration: Annotated[bool, "The duration() function. Constructs a DURATION value."]
    between: Annotated[bool, "The duration.between() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in logical units."]
    in_days: Annotated[bool, "The duration.inDays() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in days."]
    in_months: Annotated[bool, "The duration.inMonths() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in months."]
    in_seconds: Annotated[bool, "The duration.inSeconds() function. Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in seconds."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.TemporalDurationFunctionFeatures")
    DURATION = hydra.core.Name("duration")
    DURATION.BETWEEN = hydra.core.Name("duration.between")
    DURATION.IN_DAYS = hydra.core.Name("duration.inDays")
    DURATION.IN_MONTHS = hydra.core.Name("duration.inMonths")
    DURATION.IN_SECONDS = hydra.core.Name("duration.inSeconds")

@dataclass(frozen=True)
class TemporalInstantFunctionFeatures:
    r"""Temporal instant functions."""
    
    date: Annotated[bool, "The date() function. Creates a DATE instant."]
    realtime: Annotated[bool, "The date.realtime() function. Returns the current DATE instant using the realtime clock."]
    statement: Annotated[bool, "The date.statement() function. Returns the current DATE instant using the statement clock."]
    transaction: Annotated[bool, "The date.transaction() function. Returns the current DATE instant using the transaction clock."]
    truncate: Annotated[bool, "The date.truncate() function. Truncates the given temporal value to a DATE instant using the specified unit."]
    datetime: Annotated[bool, "The datetime() function. Creates a ZONED DATETIME instant."]
    fromepoch: Annotated[bool, "The datetime.fromepoch() function. Creates a ZONED DATETIME given the seconds and nanoseconds since the start of the epoch."]
    fromepochmillis: Annotated[bool, "The datetime.fromepochmillis() function. Creates a ZONED DATETIME given the milliseconds since the start of the epoch."]
    realtime: Annotated[bool, "The datetime.realtime() function. Returns the current ZONED DATETIME instant using the realtime clock."]
    statement: Annotated[bool, "The datetime.statement() function. Returns the current ZONED DATETIME instant using the statement clock."]
    transaction: Annotated[bool, "The datetime.transaction() function. Returns the current ZONED DATETIME instant using the transaction clock."]
    truncate: Annotated[bool, "The datetime.truncate() function. Truncates the given temporal value to a ZONED DATETIME instant using the specified unit."]
    localdatetime: Annotated[bool, "The localdatetime() function. Creates a LOCAL DATETIME instant."]
    realtime: Annotated[bool, "The localdatetime.realtime() function. Returns the current LOCAL DATETIME instant using the realtime clock."]
    statement: Annotated[bool, "The localdatetime.statement() function. Returns the current LOCAL DATETIME instant using the statement clock."]
    transaction: Annotated[bool, "The localdatetime.transaction() function. Returns the current LOCAL DATETIME instant using the transaction clock."]
    truncate: Annotated[bool, "The localdatetime.truncate() function. Truncates the given temporal value to a LOCAL DATETIME instant using the specified unit."]
    localtime: Annotated[bool, "The localtime() function. Creates a LOCAL TIME instant."]
    realtime: Annotated[bool, "The localtime.realtime() function. Returns the current LOCAL TIME instant using the realtime clock."]
    statement: Annotated[bool, "The localtime.statement() function. Returns the current LOCAL TIME instant using the statement clock."]
    transaction: Annotated[bool, "The localtime.transaction() function. Returns the current LOCAL TIME instant using the transaction clock."]
    truncate: Annotated[bool, "The localtime.truncate() function. Truncates the given temporal value to a LOCAL TIME instant using the specified unit."]
    time: Annotated[bool, "The time() function. Creates a ZONED TIME instant."]
    realtime: Annotated[bool, "The time.realtime() function. Returns the current ZONED TIME instant using the realtime clock."]
    statement: Annotated[bool, "The time.statement() function. Returns the current ZONED TIME instant using the statement clock."]
    transaction: Annotated[bool, "The time.transaction() function. Returns the current ZONED TIME instant using the transaction clock."]
    truncate: Annotated[bool, "The time.truncate() function. Truncates the given temporal value to a ZONED TIME instant using the specified unit."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.TemporalInstantFunctionFeatures")
    DATE = hydra.core.Name("date")
    DATE.REALTIME = hydra.core.Name("date.realtime")
    DATE.STATEMENT = hydra.core.Name("date.statement")
    DATE.TRANSACTION = hydra.core.Name("date.transaction")
    DATE.TRUNCATE = hydra.core.Name("date.truncate")
    DATETIME = hydra.core.Name("datetime")
    DATETIME.FROMEPOCH = hydra.core.Name("datetime.fromepoch")
    DATETIME.FROMEPOCHMILLIS = hydra.core.Name("datetime.fromepochmillis")
    DATETIME.REALTIME = hydra.core.Name("datetime.realtime")
    DATETIME.STATEMENT = hydra.core.Name("datetime.statement")
    DATETIME.TRANSACTION = hydra.core.Name("datetime.transaction")
    DATETIME.TRUNCATE = hydra.core.Name("datetime.truncate")
    LOCALDATETIME = hydra.core.Name("localdatetime")
    LOCALDATETIME.REALTIME = hydra.core.Name("localdatetime.realtime")
    LOCALDATETIME.STATEMENT = hydra.core.Name("localdatetime.statement")
    LOCALDATETIME.TRANSACTION = hydra.core.Name("localdatetime.transaction")
    LOCALDATETIME.TRUNCATE = hydra.core.Name("localdatetime.truncate")
    LOCALTIME = hydra.core.Name("localtime")
    LOCALTIME.REALTIME = hydra.core.Name("localtime.realtime")
    LOCALTIME.STATEMENT = hydra.core.Name("localtime.statement")
    LOCALTIME.TRANSACTION = hydra.core.Name("localtime.transaction")
    LOCALTIME.TRUNCATE = hydra.core.Name("localtime.truncate")
    TIME = hydra.core.Name("time")
    TIME.REALTIME = hydra.core.Name("time.realtime")
    TIME.STATEMENT = hydra.core.Name("time.statement")
    TIME.TRANSACTION = hydra.core.Name("time.transaction")
    TIME.TRUNCATE = hydra.core.Name("time.truncate")

@dataclass(frozen=True)
class TrigonometricFunctionFeatures:
    r"""Trigonometric functions."""
    
    acos: Annotated[bool, "The acos() function. Returns the arccosine of a FLOAT in radians."]
    asin: Annotated[bool, "The asin() function. Returns the arcsine of a FLOAT in radians."]
    atan: Annotated[bool, "The atan() function. Returns the arctangent of a FLOAT in radians."]
    atan2: Annotated[bool, "The atan2() function. Returns the arctangent2 of a set of coordinates in radians."]
    cos: Annotated[bool, "The cos() function. Returns the cosine of a FLOAT."]
    cot: Annotated[bool, "The cot() function. Returns the cotangent of a FLOAT."]
    degrees: Annotated[bool, "The degrees() function. Converts radians to degrees."]
    haversin: Annotated[bool, "The haversin() function. Returns half the versine of a number."]
    pi: Annotated[bool, "The pi() function. Returns the mathematical constant pi."]
    radians: Annotated[bool, "The radians() function. Converts degrees to radians."]
    sin: Annotated[bool, "The sin() function. Returns the sine of a FLOAT."]
    tan: Annotated[bool, "The tan() function. Returns the tangent of a FLOAT."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.TrigonometricFunctionFeatures")
    ACOS = hydra.core.Name("acos")
    ASIN = hydra.core.Name("asin")
    ATAN = hydra.core.Name("atan")
    ATAN2 = hydra.core.Name("atan2")
    COS = hydra.core.Name("cos")
    COT = hydra.core.Name("cot")
    DEGREES = hydra.core.Name("degrees")
    HAVERSIN = hydra.core.Name("haversin")
    PI = hydra.core.Name("pi")
    RADIANS = hydra.core.Name("radians")
    SIN = hydra.core.Name("sin")
    TAN = hydra.core.Name("tan")

@dataclass(frozen=True)
class VectorFunctionFeatures:
    r"""Vector functions."""
    
    cosine: Annotated[bool, "The vector.similarity.cosine() function. Returns a FLOAT representing the similarity between the argument vectors based on their cosine."]
    euclidean: Annotated[bool, "The vector.similarity.euclidean() function. Returns a FLOAT representing the similarity between the argument vectors based on their Euclidean distance."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.VectorFunctionFeatures")
    VECTOR.SIMILARITY.COSINE = hydra.core.Name("vector.similarity.cosine")
    VECTOR.SIMILARITY.EUCLIDEAN = hydra.core.Name("vector.similarity.euclidean")

@dataclass(frozen=True)
class ListFeatures:
    r"""List functionality."""
    
    list_comprehension: Annotated[bool, "Basic list comprehensions"]
    list_range: Annotated[bool, "List range comprehensions (e.g. [1..10])"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ListFeatures")
    LIST_COMPREHENSION = hydra.core.Name("listComprehension")
    LIST_RANGE = hydra.core.Name("listRange")

@dataclass(frozen=True)
class LiteralFeatures:
    r"""Various types of literal values."""
    
    boolean: Annotated[bool, "Boolean literals (note: included by most if not all implementations)."]
    double: Annotated[bool, "Double-precision floating-point literals"]
    integer: Annotated[bool, "Integer literals"]
    list: Annotated[bool, "List literals"]
    map: Annotated[bool, "Map literals"]
    null: Annotated[bool, "The NULL literal"]
    string: Annotated[bool, "String literals (note: included by most if not all implementations)."]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.LiteralFeatures")
    BOOLEAN = hydra.core.Name("boolean")
    DOUBLE = hydra.core.Name("double")
    INTEGER = hydra.core.Name("integer")
    LIST = hydra.core.Name("list")
    MAP = hydra.core.Name("map")
    NULL = hydra.core.Name("null")
    STRING = hydra.core.Name("string")

@dataclass(frozen=True)
class LogicalFeatures:
    r"""Logical operations."""
    
    and_: Annotated[bool, "The AND operator"]
    not_: Annotated[bool, "The NOT operator"]
    or_: Annotated[bool, "The OR operator"]
    xor: Annotated[bool, "The XOR operator"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.LogicalFeatures")
    AND = hydra.core.Name("and")
    NOT = hydra.core.Name("not")
    OR = hydra.core.Name("or")
    XOR = hydra.core.Name("xor")

@dataclass(frozen=True)
class MatchFeatures:
    r"""Match queries."""
    
    match: Annotated[bool, "The basic (non-optional) MATCH clause"]
    optional_match: Annotated[bool, "OPTIONAL MATCH"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.MatchFeatures")
    MATCH = hydra.core.Name("match")
    OPTIONAL_MATCH = hydra.core.Name("optionalMatch")

@dataclass(frozen=True)
class MergeFeatures:
    r"""Merge operations."""
    
    merge: Annotated[bool, "The basic MERGE clause"]
    merge_on_create: Annotated[bool, "MERGE with the ON CREATE action"]
    merge_on_match: Annotated[bool, "MERGE with the ON MATCH action"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.MergeFeatures")
    MERGE = hydra.core.Name("merge")
    MERGE_ON_CREATE = hydra.core.Name("mergeOnCreate")
    MERGE_ON_MATCH = hydra.core.Name("mergeOnMatch")

@dataclass(frozen=True)
class NodePatternFeatures:
    r"""Node patterns."""
    
    multiple_labels: Annotated[bool, "Specifying multiple labels in a node pattern"]
    parameter: Annotated[bool, "Specifying a parameter as part of a node pattern"]
    property_map: Annotated[bool, "Specifying a key/value map of properties in a node pattern"]
    variable_node: Annotated[bool, "Binding a variable to a node in a node pattern (note: included by most if not all implementations)."]
    wildcard_label: Annotated[bool, "Omitting labels from a node pattern"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.NodePatternFeatures")
    MULTIPLE_LABELS = hydra.core.Name("multipleLabels")
    PARAMETER = hydra.core.Name("parameter")
    PROPERTY_MAP = hydra.core.Name("propertyMap")
    VARIABLE_NODE = hydra.core.Name("variableNode")
    WILDCARD_LABEL = hydra.core.Name("wildcardLabel")

@dataclass(frozen=True)
class NullFeatures:
    r"""IS NULL / IS NOT NULL checks."""
    
    is_null: Annotated[bool, "The IS NULL operator"]
    is_not_null: Annotated[bool, "The IS NOT NULL operator"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.NullFeatures")
    IS_NULL = hydra.core.Name("isNull")
    IS_NOT_NULL = hydra.core.Name("isNotNull")

@dataclass(frozen=True)
class PathFeatures:
    r"""Path functions only found in OpenCypher."""
    
    shortest_path: Annotated[bool, "The shortestPath() function"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.PathFeatures")
    SHORTEST_PATH = hydra.core.Name("shortestPath")

@dataclass(frozen=True)
class ProcedureCallFeatures:
    r"""Procedure calls."""
    
    in_query_call: Annotated[bool, "CALL within a query"]
    standalone_call: Annotated[bool, "Standalone / top-level CALL"]
    yield_: Annotated[bool, "The YIELD clause in CALL"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ProcedureCallFeatures")
    IN_QUERY_CALL = hydra.core.Name("inQueryCall")
    STANDALONE_CALL = hydra.core.Name("standaloneCall")
    YIELD = hydra.core.Name("yield")

@dataclass(frozen=True)
class ProjectionFeatures:
    r"""Projections."""
    
    limit: Annotated[bool, "The LIMIT clause"]
    order_by: Annotated[bool, "The ORDER BY clause"]
    project_distinct: Annotated[bool, "The DISTINCT keyword"]
    project_all: Annotated[bool, "The * projection"]
    project_as: Annotated[bool, "The AS keyword"]
    skip: Annotated[bool, "The SKIP clause"]
    sort_order: Annotated[bool, "The ASC/ASCENDING and DESC/DESCENDING keywords"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ProjectionFeatures")
    LIMIT = hydra.core.Name("limit")
    ORDER_BY = hydra.core.Name("orderBy")
    PROJECT_DISTINCT = hydra.core.Name("projectDistinct")
    PROJECT_ALL = hydra.core.Name("projectAll")
    PROJECT_AS = hydra.core.Name("projectAs")
    SKIP = hydra.core.Name("skip")
    SORT_ORDER = hydra.core.Name("sortOrder")

@dataclass(frozen=True)
class QuantifierFeatures:
    r"""Quantifier expressions."""
    
    all: Annotated[bool, "The ALL quantifier"]
    any: Annotated[bool, "The ANY quantifier"]
    none: Annotated[bool, "The NONE quantifier"]
    single: Annotated[bool, "The SINGLE quantifier"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.QuantifierFeatures")
    ALL = hydra.core.Name("all")
    ANY = hydra.core.Name("any")
    NONE = hydra.core.Name("none")
    SINGLE = hydra.core.Name("single")

@dataclass(frozen=True)
class RangeLiteralFeatures:
    r"""Range literals within relationship patterns."""
    
    bounds: Annotated[bool, "Range literals with both lower and upper bounds"]
    exact_range: Annotated[bool, "Range literals providing an exact number of repetitions"]
    lower_bound: Annotated[bool, "Range literals with a lower bound (only)"]
    star_range: Annotated[bool, "The * range literal"]
    upper_bound: Annotated[bool, "Range literals with an upper bound (only)"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.RangeLiteralFeatures")
    BOUNDS = hydra.core.Name("bounds")
    EXACT_RANGE = hydra.core.Name("exactRange")
    LOWER_BOUND = hydra.core.Name("lowerBound")
    STAR_RANGE = hydra.core.Name("starRange")
    UPPER_BOUND = hydra.core.Name("upperBound")

@dataclass(frozen=True)
class ReadingFeatures:
    r"""Specific syntax related to reading data from the graph."""
    
    union: Annotated[bool, "The UNION operator"]
    union_all: Annotated[bool, "The UNION ALL operator"]
    unwind: Annotated[bool, "The UNWIND clause"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.ReadingFeatures")
    UNION = hydra.core.Name("union")
    UNION_ALL = hydra.core.Name("unionAll")
    UNWIND = hydra.core.Name("unwind")

@dataclass(frozen=True)
class RelationshipDirectionFeatures:
    r"""Relationship directions / arrow patterns."""
    
    both: Annotated[bool, "The two-headed arrow (<-[]->) relationship direction"]
    left: Annotated[bool, "The left arrow (<-[]-) relationship direction"]
    neither: Annotated[bool, "The headless arrow (-[]-) relationship direction"]
    right: Annotated[bool, "The right arrow (-[]->) relationship direction"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.RelationshipDirectionFeatures")
    BOTH = hydra.core.Name("both")
    LEFT = hydra.core.Name("left")
    NEITHER = hydra.core.Name("neither")
    RIGHT = hydra.core.Name("right")

@dataclass(frozen=True)
class RelationshipPatternFeatures:
    r"""Relationship patterns."""
    
    multiple_types: Annotated[bool, "Specifying a disjunction of multiple types in a relationship pattern"]
    variable_relationship: Annotated[bool, "Binding a variable to a relationship in a relationship pattern (note: included by most if not all implementations)."]
    wildcard_type: Annotated[bool, "Omitting types from a relationship pattern"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.RelationshipPatternFeatures")
    MULTIPLE_TYPES = hydra.core.Name("multipleTypes")
    VARIABLE_RELATIONSHIP = hydra.core.Name("variableRelationship")
    WILDCARD_TYPE = hydra.core.Name("wildcardType")

@dataclass(frozen=True)
class RemoveFeatures:
    r"""REMOVE operations."""
    
    by_label: Annotated[bool, "REMOVE Variable:NodeLabels"]
    by_property: Annotated[bool, "REMOVE PropertyExpression"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.RemoveFeatures")
    BY_LABEL = hydra.core.Name("byLabel")
    BY_PROPERTY = hydra.core.Name("byProperty")

@dataclass(frozen=True)
class SetFeatures:
    r"""Set definitions."""
    
    property_equals: Annotated[bool, "Defining a set using PropertyExpression = Expression"]
    variable_equals: Annotated[bool, "Defining a set using Variable = Expression"]
    variable_plus_equals: Annotated[bool, "Defining a set using Variable += Expression"]
    variable_with_node_labels: Annotated[bool, "Defining a set using Variable:NodeLabels"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.SetFeatures")
    PROPERTY_EQUALS = hydra.core.Name("propertyEquals")
    VARIABLE_EQUALS = hydra.core.Name("variableEquals")
    VARIABLE_PLUS_EQUALS = hydra.core.Name("variablePlusEquals")
    VARIABLE_WITH_NODE_LABELS = hydra.core.Name("variableWithNodeLabels")

@dataclass(frozen=True)
class StringFeatures:
    r"""String functions/keywords only found in OpenCypher."""
    
    contains: Annotated[bool, "The contains() function / CONTAINS"]
    ends_with: Annotated[bool, "The endsWith() function / ENDS WITH"]
    in_: Annotated[bool, "The in() function / IN"]
    starts_with: Annotated[bool, "The startsWith() function / STARTS WITH"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.StringFeatures")
    CONTAINS = hydra.core.Name("contains")
    ENDS_WITH = hydra.core.Name("endsWith")
    IN = hydra.core.Name("in")
    STARTS_WITH = hydra.core.Name("startsWith")

@dataclass(frozen=True)
class UpdatingFeatures:
    r"""Specific syntax related to updating data in the graph."""
    
    create: Annotated[bool, "The CREATE clause"]
    set: Annotated[bool, "The SET clause"]
    with_: Annotated[bool, "Multi-part queries using WITH"]
    
    TYPE_ = hydra.core.Name("hydra.ext.cypher.features.UpdatingFeatures")
    CREATE = hydra.core.Name("create")
    SET = hydra.core.Name("set")
    WITH = hydra.core.Name("with")
