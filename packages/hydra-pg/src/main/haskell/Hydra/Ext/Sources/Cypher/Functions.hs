module Hydra.Ext.Sources.Cypher.Functions where

data CypherFunction = CypherFunction {
  cypherFunctionName :: String,
  cypherFunctionKeyword :: Maybe String,
  cypherFunctionForms :: [CypherFunctionForm]} deriving (Eq, Ord, Show)

data CypherFunctionForm = CypherFunctionForm {
  cypherFunctionFormSignature :: String,
  cypherFunctionFormDescription :: String} deriving (Eq, Ord, Show)

data CypherLibrary = CypherLibrary {
  cypherLibraryName :: String,
  cypherLibraryDescription :: String,
  cypherLibraryFunctions :: [CypherFunction]} deriving (Eq, Ord, Show)

cypherLibraries :: [CypherLibrary]
cypherLibraries = [
  CypherLibrary "Aggregate" "aggregate functions" [
    CypherFunction "avg" (Just "AVG") [
      CypherFunctionForm
        "avg(input :: DURATION) :: DURATION"
        "Returns the average of a set of DURATION values.",
      CypherFunctionForm
        "avg(input :: FLOAT) :: FLOAT"
        "Returns the average of a set of FLOAT values.",
      CypherFunctionForm
        "avg(input :: INTEGER) :: INTEGER"
        "Returns the average of a set of INTEGER values."],
    CypherFunction "collect" (Just "COLLECT") [CypherFunctionForm
      "collect(input :: ANY) :: LIST<ANY>"
      "Returns a list containing the values returned by an expression."],
    CypherFunction "count" (Just "COUNT") [CypherFunctionForm
      "count(input :: ANY) :: INTEGER"
      "Returns the number of values or rows."],
    CypherFunction "max" (Just "MAX") [CypherFunctionForm
      "max(input :: ANY) :: ANY"
      "Returns the maximum value in a set of values."],
    CypherFunction "min" (Just "MIN") [CypherFunctionForm
      "min(input :: ANY) :: ANY"
      "Returns the minimum value in a set of values."],
    CypherFunction "percentileCont" Nothing [CypherFunctionForm
      "percentileCont(input :: FLOAT, percentile :: FLOAT) :: FLOAT"
      "Returns the percentile of a value over a group using linear interpolation."],
    CypherFunction "percentileDisc" Nothing [
      CypherFunctionForm
        "percentileDisc(input :: FLOAT, percentile :: FLOAT) :: FLOAT"
        "Returns the nearest FLOAT value to the given percentile over a group using a rounding method.",
      CypherFunctionForm
        "percentileDisc(input :: INTEGER, percentile :: FLOAT) :: INTEGER"
        "Returns the nearest INTEGER value to the given percentile over a group using a rounding method."],
    CypherFunction "stdev" Nothing [CypherFunctionForm
      "stdev(input :: FLOAT) :: FLOAT"
      "Returns the standard deviation for the given value over a group for a sample of a population."],
    CypherFunction "stdevp" Nothing [CypherFunctionForm
      "stdevp(input :: FLOAT) :: FLOAT"
      "Returns the standard deviation for the given value over a group for an entire population."],
    CypherFunction "sum" (Just "SUM") [
      CypherFunctionForm
        "sum(input :: DURATION) :: DURATION"
        "Returns the sum of a set of DURATION values.",
      CypherFunctionForm
        "sum(input :: FLOAT) :: FLOAT"
        "Returns the sum of a set of FLOAT values.",
      CypherFunctionForm
        "sum(input :: INTEGER) :: INTEGER"
        "Returns the sum of a set of INTEGER values."]],

  CypherLibrary "Database" "database functions" [
    CypherFunction "db.nameFromElementId" Nothing [CypherFunctionForm
      "db.nameFromElementId(name :: STRING) :: STRING"
      "Resolves the database name from the given element id. Introduced in 5.12."]],

  CypherLibrary "GenAI" "genAI functions" [
    CypherFunction "genai.vector.encode" Nothing [CypherFunctionForm
      "genai.vector.encode(resource :: STRING, provider :: STRING, configuration :: MAP = {}) :: LIST<FLOAT>"
      "Encode a given resource as a vector using the named provider. Introduced in 5.17."]],

  CypherLibrary "Graph" "graph functions" [
    CypherFunction "graph.byElementId" Nothing [CypherFunctionForm
      "USE graph.byElementId(elementId :: STRING)"
      "Resolves the constituent graph to which a given element id belongs. Introduced in 5.13."],
    CypherFunction "graph.byName" Nothing [CypherFunctionForm
      "USE graph.byName(name :: STRING)"
      "Resolves a constituent graph by name."],
    CypherFunction "graph.names" Nothing [CypherFunctionForm
      "graph.names() :: LIST<STRING>"
      "Returns a list containing the names of all graphs in the current composite database."],
    CypherFunction "graph.propertiesByName" Nothing [CypherFunctionForm
      "graph.propertiesByName(name :: STRING) :: MAP"
      "Returns a map containing the properties associated with the given graph."]],

  CypherLibrary "List" "list functions" [
    CypherFunction "keys" Nothing [
      CypherFunctionForm
        "keys(input :: MAP) :: LIST<STRING>"
        "Returns a LIST<STRING> containing the STRING representations for all the property names of a MAP.",
      CypherFunctionForm
        "keys(input :: NODE) :: LIST<STRING>"
        "Returns a LIST<STRING> containing the STRING representations for all the property names of a NODE.",
      CypherFunctionForm
        "keys(input :: RELATIONSHIP) :: LIST<STRING>"
        "Returns a LIST<STRING> containing the STRING representations for all the property names of a RELATIONSHIP."],
    CypherFunction "labels" Nothing [CypherFunctionForm
      "labels(input :: NODE) :: LIST<STRING>"
      "Returns a LIST<STRING> containing the STRING representations for all the labels of a NODE."],
    CypherFunction "nodes" Nothing [CypherFunctionForm
      "nodes(input :: PATH) :: LIST<NODE>"
      "Returns a LIST<NODE> containing all the NODE values in a PATH."],
    CypherFunction "range" Nothing [
      CypherFunctionForm
        "range(start :: INTEGER, end :: INTEGER) :: LIST<INTEGER>"
        "Returns a LIST<INTEGER> comprising all INTEGER values within a specified range.",
      CypherFunctionForm
        "range(start :: INTEGER, end :: INTEGER, step :: INTEGER) :: LIST<INTEGER>"
        "Returns a LIST<INTEGER> comprising all INTEGER values within a specified range created with step length."],
    CypherFunction "reduce" Nothing [CypherFunctionForm
      "reduce(accumulator :: VARIABLE = initial :: ANY, variable :: VARIABLE IN list :: LIST<ANY> | expression :: ANY) :: ANY"
      "Runs an expression against individual elements of a LIST<ANY>, storing the result of the expression in an accumulator."],
    CypherFunction "relationships" Nothing [CypherFunctionForm
      "relationships(input :: PATH) :: LIST<RELATIONSHIP>"
      "Returns a LIST<RELATIONSHIP> containing all the RELATIONSHIP values in a PATH."],
    CypherFunction "reverse" Nothing [CypherFunctionForm
      "reverse(input :: LIST<ANY>) :: LIST<ANY>"
      "Returns a LIST<ANY> in which the order of all elements in the given LIST<ANY> have been reversed."],
    CypherFunction "tail" Nothing [CypherFunctionForm
      "tail(input :: LIST<ANY>) :: LIST<ANY>"
      "Returns all but the first element in a LIST<ANY>."],
    CypherFunction "toBooleanList" Nothing [CypherFunctionForm
      "toBooleanList(input :: LIST<ANY>) :: LIST<BOOLEAN>"
      "Converts a LIST<ANY> of values to a LIST<BOOLEAN> values. If any values are not convertible to BOOLEAN they will be null in the LIST<BOOLEAN> returned."],
    CypherFunction "toFloatList" Nothing [CypherFunctionForm
      "toFloatList(input :: LIST<ANY>) :: LIST<FLOAT>"
      "Converts a LIST<ANY> to a LIST<FLOAT> values. If any values are not convertible to FLOAT they will be null in the LIST<FLOAT> returned."],
    CypherFunction "toIntegerList" Nothing [CypherFunctionForm
      "toIntegerList(input :: LIST<ANY>) :: LIST<INTEGER>"
      "Converts a LIST<ANY> to a LIST<INTEGER> values. If any values are not convertible to INTEGER they will be null in the LIST<INTEGER> returned."],
    CypherFunction "toStringList" Nothing [CypherFunctionForm
      "toStringList(input :: LIST<ANY>) :: LIST<STRING>"
      "Converts a LIST<ANY> to a LIST<STRING> values. If any values are not convertible to STRING they will be null in the LIST<STRING> returned."]],

  CypherLibrary "LoadCSV" "load CSV functions" [
    CypherFunction "file" Nothing [CypherFunctionForm
      "file() :: STRING"
      "Returns the absolute path of the file that LOAD CSV is using."],
    CypherFunction "linenumber" Nothing [CypherFunctionForm
      "linenumber() :: INTEGER"
      "Returns the line number that LOAD CSV is currently using."]],

  CypherLibrary "Logarithmic" "logarithmic functions" [
    CypherFunction "e" Nothing [CypherFunctionForm
      "e() :: FLOAT"
      "Returns the base of the natural logarithm, e."],
    CypherFunction "exp" Nothing [CypherFunctionForm
      "exp(input :: FLOAT) :: FLOAT"
      "Returns e^n, where e is the base of the natural logarithm, and n is the value of the argument expression."],
    CypherFunction "log" Nothing [CypherFunctionForm
      "log(input :: FLOAT) :: FLOAT"
      "Returns the natural logarithm of a FLOAT."],
    CypherFunction "log10" Nothing [CypherFunctionForm
      "log10(input :: FLOAT) :: FLOAT"
      "Returns the common logarithm (base 10) of a FLOAT."],
    CypherFunction "sqrt" Nothing [CypherFunctionForm
      "sqrt(input :: FLOAT) :: FLOAT"
      "Returns the square root of a FLOAT."]],

  CypherLibrary "Numeric" "numeric functions" [
    CypherFunction "abs" Nothing [
      CypherFunctionForm
        "abs(input :: FLOAT) :: FLOAT"
        "Returns the absolute value of a FLOAT.",
      CypherFunctionForm
        "abs(input :: INTEGER) :: INTEGER"
        "Returns the absolute value of an INTEGER."],
    CypherFunction "ceil" Nothing [CypherFunctionForm
      "ceil(input :: FLOAT) :: FLOAT"
      "Returns the smallest FLOAT that is greater than or equal to a number and equal to an INTEGER."],
    CypherFunction "floor" Nothing [CypherFunctionForm
      "floor(input :: FLOAT) :: FLOAT"
      "Returns the largest FLOAT that is less than or equal to a number and equal to an INTEGER."],
    CypherFunction "isNaN" Nothing [
      CypherFunctionForm
        "isNaN(input :: FLOAT) :: BOOLEAN"
        "Returns true if the floating point number is NaN.",
      CypherFunctionForm
        "isNaN(input :: INTEGER) :: BOOLEAN"
        "Returns true if the integer number is NaN."],
    CypherFunction "rand" Nothing [CypherFunctionForm
      "rand() :: FLOAT"
      "Returns a random FLOAT in the range from 0 (inclusive) to 1 (exclusive)."],
    CypherFunction "round" Nothing [
      CypherFunctionForm
        "round(input :: FLOAT) :: FLOAT"
        "Returns the value of a number rounded to the nearest INTEGER.",
      CypherFunctionForm
        "round(value :: FLOAT, precision :: INTEGER | FLOAT) :: FLOAT"
        "Returns the value of a number rounded to the specified precision using rounding mode HALF_UP.",
      CypherFunctionForm
        "round(value :: FLOAT, precision :: INTEGER | FLOAT, mode :: STRING) :: FLOAT"
        "Returns the value of a number rounded to the specified precision with the specified rounding mode."],
    CypherFunction "sign" Nothing [
      CypherFunctionForm
        "sign(input :: FLOAT) :: INTEGER"
        "Returns the signum of a FLOAT: 0 if the number is 0, -1 for any negative number, and 1 for any positive number.",
      CypherFunctionForm
        "sign(input :: INTEGER) :: INTEGER"
        "Returns the signum of an INTEGER: 0 if the number is 0, -1 for any negative number, and 1 for any positive number."]],

  CypherLibrary "Predicate" "predicate functions" [
    CypherFunction "all" Nothing [CypherFunctionForm
      "all(variable :: VARIABLE IN list :: LIST<ANY> WHERE predicate :: ANY) :: BOOLEAN"
      "Returns true if the predicate holds for all elements in the given LIST<ANY>."],
    CypherFunction "any" Nothing [CypherFunctionForm
      "any(variable :: VARIABLE IN list :: LIST<ANY> WHERE predicate :: ANY) :: BOOLEAN"
      "Returns true if the predicate holds for at least one element in the given LIST<ANY>."],
    CypherFunction "exists" Nothing [CypherFunctionForm
      "exists(input :: ANY) :: BOOLEAN"
      "Returns true if a match for the pattern exists in the graph."],
    CypherFunction "isEmpty" Nothing [
      CypherFunctionForm
        "isEmpty(input :: LIST<ANY>) :: BOOLEAN"
        "Checks whether a LIST<ANY> is empty.",
      CypherFunctionForm
        "isEmpty(input :: MAP) :: BOOLEAN"
        "Checks whether a MAP is empty.",
      CypherFunctionForm
        "isEmpty(input :: STRING) :: BOOLEAN"
        "Checks whether a STRING is empty."],
    CypherFunction "none" Nothing [CypherFunctionForm
      "none(variable :: VARIABLE IN list :: LIST<ANY> WHERE predicate :: ANY) :: BOOLEAN"
      "Returns true if the predicate holds for no element in the given LIST<ANY>."],
    CypherFunction "single" Nothing [CypherFunctionForm
      "single(variable :: VARIABLE IN list :: LIST<ANY> WHERE predicate :: ANY) :: BOOLEAN"
      "Returns true if the predicate holds for exactly one of the elements in the given LIST<ANY>."]],

  CypherLibrary "Scalar" "scalar functions" [
    CypherFunction "char_length" Nothing [CypherFunctionForm
      "char_length(input :: STRING) :: INTEGER"
      "Returns the number of Unicode characters in a STRING."],
    CypherFunction "character_length" Nothing [CypherFunctionForm
      "character_length(input :: STRING) :: INTEGER"
      "Returns the number of Unicode characters in a STRING."],
    CypherFunction "coalesce" Nothing [CypherFunctionForm
      "coalesce(input :: ANY) :: ANY"
      "Returns the first non-null value in a list of expressions."],
    CypherFunction "elementId" Nothing [
      CypherFunctionForm
        "elementId(input :: NODE) :: STRING"
        "Returns a node identifier, unique within a specific transaction and DBMS.",
      CypherFunctionForm
        "elementId(input :: RELATIONSHIP) :: STRING"
        "Returns a relationship identifier, unique within a specific transaction and DBMS."],
    CypherFunction "endNode" Nothing [CypherFunctionForm
      "elementId(input :: RELATIONSHIP) :: STRING"
      "Returns a relationship identifier, unique within a specific transaction and DBMS."],
    CypherFunction "head" Nothing [CypherFunctionForm
      "head(list :: LIST<ANY>) :: ANY"
      "Returns the first element in a LIST<ANY>."],
    CypherFunction "id" Nothing [
      CypherFunctionForm
        "id(input :: NODE) :: INTEGER"
        "[Deprecated] Returns the id of a NODE. Replaced by elementId().",
      CypherFunctionForm
        "id(input :: RELATIONSHIP) :: INTEGER"
        "[Deprecated] Returns the id of a RELATIONSHIP. Replaced by elementId()."],
    CypherFunction "last" Nothing [CypherFunctionForm
      "last(list :: LIST<ANY>) :: ANY"
      "Returns the last element in a LIST<ANY>."],
    CypherFunction "length" Nothing [CypherFunctionForm
      "length(input :: PATH) :: INTEGER"
      "Returns the length of a PATH."],
    CypherFunction "nullIf" Nothing [CypherFunctionForm
      "nullIf(v1 :: ANY, v2 :: ANY) :: ANY"
      "Returns null if the two given parameters are equivalent, otherwise returns the value of the first parameter."],
    CypherFunction "properties" Nothing [
      CypherFunctionForm
        "properties(input :: MAP) :: MAP"
        "Returns a MAP containing all the properties of a MAP.",
      CypherFunctionForm
        "properties(input :: NODE) :: MAP"
        "Returns a MAP containing all the properties of a NODE.",
      CypherFunctionForm
        "properties(input :: RELATIONSHIP) :: MAP"
        "Returns a MAP containing all the properties of a RELATIONSHIP."],
    CypherFunction "randomUUID" Nothing [CypherFunctionForm
      "randomUUID() :: STRING"
      "Generates a random UUID."],
    CypherFunction "size" Nothing [
      CypherFunctionForm
        "size(input :: LIST<ANY>) :: INTEGER"
        "Returns the number of items in a LIST<ANY>.",
      CypherFunctionForm
        "size(input :: STRING) :: INTEGER"
        "Returns the number of Unicode characters in a STRING."],
    CypherFunction "startNode" Nothing [CypherFunctionForm
      "startNode(input :: RELATIONSHIP) :: NODE"
      "Returns the start NODE of a RELATIONSHIP."],
    CypherFunction "toBoolean" Nothing [
      CypherFunctionForm
        "toBoolean(input :: STRING) :: BOOLEAN"
        "Converts a STRING value to a BOOLEAN value.",
      CypherFunctionForm
        "toBoolean(input :: BOOLEAN) :: BOOLEAN"
        "Converts a BOOLEAN value to a BOOLEAN value.",
      CypherFunctionForm
        "toBoolean(input :: INTEGER) :: BOOLEAN"
        "Converts an INTEGER value to a BOOLEAN value."],
    CypherFunction "toBooleanOrNull" Nothing [CypherFunctionForm
      "toBooleanOrNull(input :: ANY) :: BOOLEAN"
      "Converts a value to a BOOLEAN value, or null if the value cannot be converted."],
    CypherFunction "toFloat" Nothing [
      CypherFunctionForm
        "toFloat(input :: INTEGER | FLOAT) :: FLOAT"
        "Converts an INTEGER value to a FLOAT value.",
      CypherFunctionForm
        "toFloat(input :: STRING) :: FLOAT"
        "Converts a STRING value to a FLOAT value."],
    CypherFunction "toFloatOrNull" Nothing [CypherFunctionForm
      "toFloatOrNull(input :: ANY) :: FLOAT"
      "Converts a value to a FLOAT value, or null if the value cannot be converted."],
    CypherFunction "toInteger" Nothing [
      CypherFunctionForm
        "toInteger(input :: INTEGER | FLOAT) :: INTEGER"
        "Converts a FLOAT value to an INTEGER value.",
      CypherFunctionForm
        "toInteger(input :: BOOLEAN) :: INTEGER"
        "Converts a BOOLEAN value to an INTEGER value.",
      CypherFunctionForm
        "toInteger(input :: STRING) :: INTEGER"
        "Converts a STRING value to an INTEGER value."],
    CypherFunction "toIntegerOrNull" Nothing [CypherFunctionForm
      "toIntegerOrNull(input :: ANY) :: INTEGER"
      "Converts a value to an INTEGER value, or null if the value cannot be converted."],
    CypherFunction "type" Nothing [CypherFunctionForm
      "type(input :: RELATIONSHIP) :: STRING"
      "Returns a STRING representation of the RELATIONSHIP type."],
    CypherFunction "valueType" Nothing [CypherFunctionForm
      "valueType(input :: ANY) :: STRING"
      "Returns a STRING representation of the most precise value type that the given expression evaluates to."]],

  CypherLibrary "Spatial" "spatial functions" [
    CypherFunction "point.distance" Nothing [CypherFunctionForm
      "point.distance(from :: POINT, to :: POINT) :: FLOAT"
      "Returns a FLOAT representing the geodesic distance between any two points in the same CRS."],
    CypherFunction "point" Nothing [
      CypherFunctionForm
        "point(input :: MAP) :: POINT"
        "Returns a 2D point object, given two coordinate values in the Cartesian coordinate system.",
      CypherFunctionForm
        "point(input :: MAP) :: POINT"
        "Returns a 3D point object, given three coordinate values in the Cartesian coordinate system.",
      CypherFunctionForm
        "point(input :: MAP) :: POINT"
        "Returns a 2D point object, given two coordinate values in the WGS 84 geographic coordinate system.",
      CypherFunctionForm
        "point(input :: MAP) :: POINT"
        "Returns a 3D point object, given three coordinate values in the WGS 84 geographic coordinate system."],
    CypherFunction "point.withinBBox" Nothing [CypherFunctionForm
      "point.withinBBox(point :: POINT, lowerLeft :: POINT, upperRight :: POINT) :: BOOLEAN"
      "Returns true if the provided point is within the bounding box defined by the two provided points, lowerLeft and upperRight."]],

  CypherLibrary "String" "string functions" [
    CypherFunction "btrim" Nothing [
      CypherFunctionForm
        "btrim(original :: STRING) :: STRING"
        "Returns the given STRING with leading and trailing whitespace removed.",
      CypherFunctionForm
        "btrim(input :: STRING, trimCharacterString :: STRING) :: STRING"
        "Returns the given STRING with leading and trailing trimCharacterString characters removed. Introduced in 5.20."],
    CypherFunction "left" Nothing [CypherFunctionForm
      "left(original :: STRING, length :: INTEGER) :: STRING"
      "Returns a STRING containing the specified number (INTEGER) of leftmost characters in the given STRING."],
    CypherFunction "lower" Nothing [CypherFunctionForm
      "lower(input :: STRING) :: STRING"
      "Returns the given STRING in lowercase. This function is an alias to the toLower() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21."],
    CypherFunction "ltrim" Nothing [
      CypherFunctionForm
        "ltrim(input :: STRING) :: STRING"
        "Returns the given STRING with leading whitespace removed.",
      CypherFunctionForm
        "ltrim(input :: STRING, trimCharacterString :: STRING) :: STRING"
        "Returns the given STRING with leading trimCharacterString characters removed. Introduced in 5.20."],
    CypherFunction "normalize" Nothing [
      CypherFunctionForm
        "normalize(input :: STRING) :: STRING"
        "Returns the given STRING normalized according to the normalization CypherFunctionForm NFC. Introduced in 5.17.",
      CypherFunctionForm
        "normalize(input :: STRING, normalForm = NFC :: [NFC, NFD, NFKC, NFKD]) :: STRING"
        "Returns the given STRING normalized according to the specified normalization CypherFunctionForm. Introduced in 5.17."],
    CypherFunction "replace" Nothing [CypherFunctionForm
      "replace(original :: STRING, search :: STRING, replace :: STRING) :: STRING"
      "Returns a STRING in which all occurrences of a specified search STRING in the given STRING have been replaced by another (specified) replacement STRING."],
    CypherFunction "reverse" Nothing [CypherFunctionForm
      "reverse(input :: STRING) :: STRING"
      "Returns a STRING in which the order of all characters in the given STRING have been reversed."],
    CypherFunction "right" Nothing [CypherFunctionForm
      "right(original :: STRING, length :: INTEGER) :: STRING"
      "Returns a STRING containing the specified number of rightmost characters in the given STRING."],
    CypherFunction "rtrim" Nothing [
      CypherFunctionForm
        "rtrim(input :: STRING) :: STRING"
        "Returns the given STRING with trailing whitespace removed.",
      CypherFunctionForm
        "rtrim(input :: STRING, trimCharacterString :: STRING) :: STRING"
        "Returns the given STRING with trailing trimCharacterString characters removed. Introduced in 5.20."],
    CypherFunction "split" Nothing [
      CypherFunctionForm
        "split(original :: STRING, splitDelimiter :: STRING) :: LIST<STRING>"
        "Returns a LIST<STRING> resulting from the splitting of the given STRING around matches of the given delimiter.",
      CypherFunctionForm
        "split(original :: STRING, splitDelimiters :: LIST<STRING>) :: LIST<STRING>"
        "Returns a LIST<STRING> resulting from the splitting of the given STRING around matches of any of the given delimiters."],
    CypherFunction "substring" Nothing [
      CypherFunctionForm
        "substring(original :: STRING, start :: INTEGER) :: STRING"
        "Returns a substring of the given STRING, beginning with a 0-based index start.",
      CypherFunctionForm
        "substring(original :: STRING, start :: INTEGER, length :: INTEGER) :: STRING"
        "Returns a substring of a given length from the given STRING, beginning with a 0-based index start."],
    CypherFunction "toLower" Nothing [CypherFunctionForm
      "toLower(input :: STRING) :: STRING"
      "Returns the given STRING in lowercase."],
    CypherFunction "toString" Nothing [CypherFunctionForm
      "toString(input :: ANY) :: STRING"
      "Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING."],
    CypherFunction "toStringOrNull" Nothing [CypherFunctionForm
      "toStringOrNull(input :: ANY) :: STRING"
      "Converts an INTEGER, FLOAT, BOOLEAN, POINT or temporal type (i.e. DATE, ZONED TIME, LOCAL TIME, ZONED DATETIME, LOCAL DATETIME or DURATION) value to a STRING, or null if the value cannot be converted."],
    CypherFunction "toUpper" Nothing [CypherFunctionForm
      "toUpper(input :: STRING) :: STRING"
      "Returns the given STRING in uppercase."],
    CypherFunction "trim" Nothing [
      CypherFunctionForm
        "trim(input :: STRING) :: STRING"
        "Returns the given STRING with leading and trailing whitespace removed.",
      CypherFunctionForm
        "trim([LEADING | TRAILING | BOTH] [trimCharacterString :: STRING] FROM input :: STRING) :: STRING"
        "Returns the given STRING with the leading and/or trailing trimCharacterString character removed. Introduced in 5.20."],
    CypherFunction "upper" Nothing [CypherFunctionForm
      "upper(input :: STRING) :: STRING"
      "Returns the given STRING in uppercase. This function is an alias to the toUpper() function, and it was introduced as part of Cypher's GQL conformance. Introduced in 5.21."]],

  CypherLibrary "TemporalDuration" "temporal duration functions" [
    CypherFunction "duration" Nothing [CypherFunctionForm
      "duration(input :: ANY) :: DURATION"
      "Constructs a DURATION value."],
    CypherFunction "duration.between" Nothing [CypherFunctionForm
      "duration.between(from :: ANY, to :: ANY) :: DURATION"
      "Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in logical units."],
    CypherFunction "duration.inDays" Nothing [CypherFunctionForm
      "duration.inDays(from :: ANY, to :: ANY) :: DURATION"
      "Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in days."],
    CypherFunction "duration.inMonths" Nothing [CypherFunctionForm
      "duration.inMonths(from :: ANY, to :: ANY) :: DURATION"
      "Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in months."],
    CypherFunction "duration.inSeconds" Nothing [CypherFunctionForm
      "duration.inSeconds(from :: ANY, to :: ANY) :: DURATION"
      "Computes the DURATION between the from instant (inclusive) and the to instant (exclusive) in seconds."]],

  CypherLibrary "TemporalInstant" "temporal instant functions" [
    CypherFunction "date" Nothing [CypherFunctionForm
      "date(input = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: DATE"
      "Creates a DATE instant."],
    CypherFunction "date.realtime" Nothing [CypherFunctionForm
      "date.realtime(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: DATE"
      "Returns the current DATE instant using the realtime clock."],
    CypherFunction "date.statement" Nothing [CypherFunctionForm
      "date.statement(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: DATE"
      "Returns the current DATE instant using the statement clock."],
    CypherFunction "date.transaction" Nothing [CypherFunctionForm
      "date.transaction(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: DATE"
      "Returns the current DATE instant using the transaction clock."],
    CypherFunction "date.truncate" Nothing [CypherFunctionForm
      "date.truncate(unit :: STRING, input = DEFAULT_TEMPORAL_ARGUMENT :: ANY, fields = null :: MAP) :: DATE"
      "Truncates the given temporal value to a DATE instant using the specified unit."],
    CypherFunction "datetime" Nothing [CypherFunctionForm
      "datetime(input = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED DATETIME"
      "Creates a ZONED DATETIME instant."],
    CypherFunction "datetime.fromepoch" Nothing [CypherFunctionForm
      "datetime.fromepoch(seconds :: INTEGER | FLOAT, nanoseconds :: INTEGER | FLOAT) :: ZONED DATETIME"
      "Creates a ZONED DATETIME given the seconds and nanoseconds since the start of the epoch."],
    CypherFunction "datetime.fromepochmillis" Nothing [CypherFunctionForm
      "datetime.fromepochmillis(milliseconds :: INTEGER | FLOAT) :: ZONED DATETIME"
      "Creates a ZONED DATETIME given the milliseconds since the start of the epoch."],
    CypherFunction "datetime.realtime" Nothing [CypherFunctionForm
      "datetime.realtime(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED DATETIME"
      "Returns the current ZONED DATETIME instant using the realtime clock."],
    CypherFunction "datetime.statement" Nothing [CypherFunctionForm
      "datetime.statement(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED DATETIME"
      "Returns the current ZONED DATETIME instant using the statement clock."],
    CypherFunction "datetime.transaction" Nothing [CypherFunctionForm
      "datetime.transaction(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED DATETIME"
      "Returns the current ZONED DATETIME instant using the transaction clock."],
    CypherFunction "datetime.truncate" Nothing [CypherFunctionForm
      "datetime.truncate(unit :: STRING, input = DEFAULT_TEMPORAL_ARGUMENT :: ANY, fields = null :: MAP) :: ZONED DATETIME"
      "Truncates the given temporal value to a ZONED DATETIME instant using the specified unit."],
    CypherFunction "localdatetime" Nothing [CypherFunctionForm
      "localdatetime(input = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL DATETIME"
      "Creates a LOCAL DATETIME instant."],
    CypherFunction "localdatetime.realtime" Nothing [CypherFunctionForm
      "localdatetime.realtime(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL DATETIME"
      "Returns the current LOCAL DATETIME instant using the realtime clock."],
    CypherFunction "localdatetime.statement" Nothing [CypherFunctionForm
      "localdatetime.statement(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL DATETIME"
      "Returns the current LOCAL DATETIME instant using the statement clock."],
    CypherFunction "localdatetime.transaction" Nothing [CypherFunctionForm
      "localdatetime.transaction(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL DATETIME"
      "Returns the current LOCAL DATETIME instant using the transaction clock."],
    CypherFunction "localdatetime.truncate" Nothing [CypherFunctionForm
      "localdatetime.truncate(unit :: STRING, input = DEFAULT_TEMPORAL_ARGUMENT :: ANY, fields = null :: MAP) :: LOCAL DATETIME"
      "Truncates the given temporal value to a LOCAL DATETIME instant using the specified unit."],
    CypherFunction "localtime" Nothing [CypherFunctionForm
      "localtime(input = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL TIME"
      "Creates a LOCAL TIME instant."],
    CypherFunction "localtime.realtime" Nothing [CypherFunctionForm
      "localtime.realtime(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL TIME"
      "Returns the current LOCAL TIME instant using the realtime clock."],
    CypherFunction "localtime.statement" Nothing [CypherFunctionForm
      "localtime.statement(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL TIME"
      "Returns the current LOCAL TIME instant using the statement clock."],
    CypherFunction "localtime.transaction" Nothing [CypherFunctionForm
      "localtime.transaction(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: LOCAL TIME"
      "Returns the current LOCAL TIME instant using the transaction clock."],
    CypherFunction "localtime.truncate" Nothing [CypherFunctionForm
      "localtime.truncate(unit :: STRING, input = DEFAULT_TEMPORAL_ARGUMENT :: ANY, fields = null :: MAP) :: LOCAL TIME"
      "Truncates the given temporal value to a LOCAL TIME instant using the specified unit."],
    CypherFunction "time" Nothing [CypherFunctionForm
      "time(input = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED TIME"
      "Creates a ZONED TIME instant."],
    CypherFunction "time.realtime" Nothing [CypherFunctionForm
      "time.realtime(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED TIME"
      "Returns the current ZONED TIME instant using the realtime clock."],
    CypherFunction "time.statement" Nothing [CypherFunctionForm
      "time.statement(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED TIME"
      "Returns the current ZONED TIME instant using the statement clock."],
    CypherFunction "time.transaction" Nothing [CypherFunctionForm
      "time.transaction(timezone = DEFAULT_TEMPORAL_ARGUMENT :: ANY) :: ZONED TIME"
      "Returns the current ZONED TIME instant using the transaction clock."],
    CypherFunction "time.truncate" Nothing [CypherFunctionForm
      "time.truncate(unit :: STRING, input = DEFAULT_TEMPORAL_ARGUMENT :: ANY, fields = null :: MAP) :: ZONED TIME"
      "Truncates the given temporal value to a ZONED TIME instant using the specified unit."]],

  CypherLibrary "Trigonometric" "trigonometric functions" [
    CypherFunction "acos" Nothing [CypherFunctionForm
      "acos(input :: FLOAT) :: FLOAT"
      "Returns the arccosine of a FLOAT in radians."],
    CypherFunction "asin" Nothing [CypherFunctionForm
      "asin(input :: FLOAT) :: FLOAT"
      "Returns the arcsine of a FLOAT in radians."],
    CypherFunction "atan" Nothing [CypherFunctionForm
      "atan(input :: FLOAT) :: FLOAT"
      "Returns the arctangent of a FLOAT in radians."],
    CypherFunction "atan2" Nothing [CypherFunctionForm
      "atan2(y :: FLOAT, x :: FLOAT) :: FLOAT"
      "Returns the arctangent2 of a set of coordinates in radians."],
    CypherFunction "cos" Nothing [CypherFunctionForm
      "cos(input :: FLOAT) :: FLOAT"
      "Returns the cosine of a FLOAT."],
    CypherFunction "cot" Nothing [CypherFunctionForm
      "cot(input :: FLOAT) :: FLOAT"
      "Returns the cotangent of a FLOAT."],
    CypherFunction "degrees" Nothing [CypherFunctionForm
      "degrees(input :: FLOAT) :: FLOAT"
      "Converts radians to degrees."],
    CypherFunction "haversin" Nothing [CypherFunctionForm
      "haversin(input :: FLOAT) :: FLOAT"
      "Returns half the versine of a number."],
    CypherFunction "pi" Nothing [CypherFunctionForm
      "pi() :: FLOAT"
      "Returns the mathematical constant pi."],
    CypherFunction "radians" Nothing [CypherFunctionForm
      "radians(input :: FLOAT) :: FLOAT"
      "Converts degrees to radians."],
    CypherFunction "sin" Nothing [CypherFunctionForm
      "sin(input :: FLOAT) :: FLOAT"
      "Returns the sine of a FLOAT."],
    CypherFunction "tan" Nothing [CypherFunctionForm
      "tan(input :: FLOAT) :: FLOAT"
      "Returns the tangent of a FLOAT."]],

  CypherLibrary "Vector" "vector functions" [
    CypherFunction "vector.similarity.cosine" Nothing [CypherFunctionForm
      "vector.similarity.cosine(a :: LIST<INTEGER | FLOAT>, b :: LIST<INTEGER | FLOAT>) :: FLOAT"
      "Returns a FLOAT representing the similarity between the argument vectors based on their cosine."],
    CypherFunction "vector.similarity.euclidean" Nothing [CypherFunctionForm
      "vector.similarity.euclidean(a :: LIST<INTEGER | FLOAT>, b :: LIST<INTEGER | FLOAT>) :: FLOAT"
      "Returns a FLOAT representing the similarity between the argument vectors based on their Euclidean distance."]]]
