{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Cypher.Features where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import qualified Data.Maybe as Y


openCypherFeaturesModule :: Module
openCypherFeaturesModule = Module ns elements [hydraCoreModule] tier0Modules $
    Just ("A model for characterizing OpenCypher queries and implementations in terms of included features.")
  where
    ns = Namespace "hydra/langs/cypher/features"
    cypherFeatures = typeref ns
    def = datatype ns
    defFeatureSet name desc fields = def (capitalize name ++ "Features") $
        doc ("A set of features for " ++ desc ++ ".") $ record fields
    feature n s = featureField n $ "Whether to expect " ++ s ++ "."
    featureField name comment = name>: doc comment boolean
    featureSet name desc = name>:
      doc ("Whether to expect " ++ desc ++ ", and if so, which specific features") $
      optional $ cypherFeatures $ capitalize name ++ "Features"
    fixedFeature n s = featureField n $ "Whether to expect " ++ s ++ " (note: included by most if not all implementations)."
    function name = feature name $ "the " ++ name ++ "() function"
    functionWithKeyword name keyword = feature name $ "the " ++ name ++ "() / " ++ keyword ++ " aggregate function"

    elements = [

      defFeatureSet "Aggregate" "aggregation functions" [
        functionWithKeyword "avg" "AVG",
        functionWithKeyword "collect" "COLLECT",
        functionWithKeyword "count" "COUNT",
        functionWithKeyword "max" "MAX",
        functionWithKeyword "min" "MIN",
        function "percentileCont",
        function "percentileDisc",
        function "stdev",
        functionWithKeyword "sum" "SUM"],

      defFeatureSet "Arithmetic" "arithmetic operations" [
        feature "plus" "the + operator",
        feature "minus" "the - operator",
        feature "multiply" "the * operator",
        feature "divide" "the / operator",
        feature "modulus" "the % operator",
        feature "powerOf" "the ^ operator"],

      defFeatureSet "Atom" "various kinds of atomic expressions" [
        feature "caseExpression" "CASE expressions",
        feature "count" "the COUNT (*) expression",
        feature "existentialSubquery" "existential subqueries",
        feature "functionInvocation" "function invocation",
        featureSet "list" "lists",
        featureSet "literal" "literal values",
        feature "parameter" "parameter expressions",
        feature "patternComprehension" "pattern comprehensions",
        feature "patternPredicate" "relationship patterns as subexpressions",
        featureSet "quantifier" "quantifier expressions",
        fixedFeature "variable" "variable expressions"],

      defFeatureSet "Comparison" "comparison operators and functions" [
        feature "equal" "the = comparison operator",
        feature "greaterThan" "the > comparison operator",
        feature "greaterThanOrEqual" "the >= comparison operator",
        feature "lessThan" "the < comparison operator",
        feature "lessThanOrEqual" "the <= comparison operator",
        feature "notEqual" "the <> comparison operator",
        function "nullIf"],

      def "CypherFeatures" $
        doc ("A set of features which characterize an OpenCypher query or implementation. "
          ++ "Any features which are omitted from the set are assumed to be unsupported or nonrequired.") $
        record [
          featureSet "aggregate" "aggregate functions",
          featureSet "arithmetic" "arithmetic operations",
          featureSet "atom" "atomic expressions",
          featureSet "comparison" "comparison operations",
          featureSet "delete" "delete operations",
          featureSet "element" "element functions",
          featureSet "logical" "logical operations",
          featureSet "map" "property map functions",
          featureSet "match" "match queries",
          featureSet "merge" "merge operations",
          featureSet "nodePattern" "node patterns",
          featureSet "null" "IS NULL / IS NOT NULL checks",
          featureSet "numeric" "numeric functions",
          featureSet "path" "path functions",
          featureSet "procedureCall" "procedure calls",
          featureSet "projection" "projection operations",
          featureSet "randomness" "random value generation",
          featureSet "rangeLiteral" "range literals",
          featureSet "reading" "reading operations",
          featureSet "relationshipDirection" "relationship directions",
          featureSet "relationshipPattern" "relationship patterns",
          featureSet "remove" "remove operations",
          featureSet "schema" "schema functions",
          featureSet "set" "set operations",
          featureSet "string" "string operations",
          featureSet "updating" "updating operations"],

      defFeatureSet "Delete" "delete operations" [
        feature "delete" "the basic DELETE clause",
        feature "detachDelete" "the DETACH DELETE clause"],

      defFeatureSet "Element" "element functions" [
        function "elementId",
        function "endNode",
        function "labels",
        function "properties",
        function "startNode"],

      defFeatureSet "List" "list functionality" [
        function "all",
        function "any",
        function "coalesce",
        function "isEmpty",
        function "head",
        function "last",
        feature "listComprehension" "basic list comprehensions",
        feature "listRange" "list range comprehensions (e.g. [1..10])",
        function "none",
        function "reduce",
        function "reverse",
        function "single",
        function "size",
        function "tail",
        function "toBooleanList",
        function "toFloatList",
        function "toIntegerList",
        function "toStringList"],

      defFeatureSet "Literal" "various types of literal values" [
        fixedFeature "boolean" "boolean literals",
        feature "double" "double-precision floating-point literals",
        feature "integer" "integer literals",
        feature "list" "list literals",
        feature "map" "map literals",
        feature "null" "the NULL literal",
        fixedFeature "string" "string literals"],

      defFeatureSet "Logical" "logical operations" [
        feature "and" "the AND operator",
        feature "not" "the NOT operator",
        feature "or" "the OR operator",
        feature "xor" "the XOR operator"],

      defFeatureSet "Map" "property map functions" [
        function "keys"],

      defFeatureSet "Match" "match queries" [
        feature "match" "the basic (non-optional) MATCH clause",
        feature "optionalMatch" "OPTIONAL MATCH"],

      defFeatureSet "Merge" "merge operations" [
        feature "merge" "the basic MERGE clause",
        feature "mergeOnCreate" "MERGE with the ON CREATE action",
        feature "mergeOnMatch" "MERGE with the ON MATCH action"],

      defFeatureSet "NodePattern" "node patterns" [
        feature "multipleLabels" "specifying multiple labels in a node pattern",
        feature "parameter" "specifying a parameter as part of a node pattern",
        feature "propertyMap" "specifying a key/value map of properties in a node pattern",
        fixedFeature "variableNode" "binding a variable to a node in a node pattern",
        feature "wildcardLabel" "omitting labels from a node pattern"],

      defFeatureSet "Null" "IS NULL / IS NOT NULL checks" [
        feature "isNull" "the IS NULL operator",
        feature "isNotNull" "the IS NOT NULL operator"],

      defFeatureSet "Numeric" "numeric functions" [
        function "abs",
        function "ceil",
        function "e",
        function "exp",
        function "floor",
        function "isNaN",
        function "log",
        function "log10",
        function "range",
        function "round",
        function "sign",
        function "sqrt"],

      defFeatureSet "Path" "path functions" [
        function "length",
        function "nodes",
        function "relationships",
        function "shortestPath"],

      defFeatureSet "ProcedureCall" "procedure calls" [
        feature "inQueryCall" "CALL within a query",
        feature "standaloneCall" "standalone / top-level CALL",
        -- Note: additional features are possible around YIELD
        feature "yield" "the YIELD clause in CALL"],

      defFeatureSet "Projection" "projections" [
        feature "limit" "the LIMIT clause",
        feature "orderBy" "the ORDER BY clause",
        feature "projectDistinct" "the DISTINCT keyword",
        feature "projectAll" "the * projection",
        feature "projectAs" "the AS keyword",
        feature "skip" "the SKIP clause",
        feature "sortOrder" "the ASC/ASCENDING and DESC/DESCENDING keywords"],

      defFeatureSet "Quantifier" "quantifier expressions" [
        feature "all" "the ALL quantifier",
        feature "any" "the ANY quantifier",
        function "exists",
        feature "none" "the NONE quantifier",
        feature "single" "the SINGLE quantifier"],

      defFeatureSet "Randomness" "random value generation" [
        function "rand",
        function "randomUUID"],

      defFeatureSet "RangeLiteral" "range literals within relationship patterns" [
        feature "bounds" "range literals with both lower and upper bounds",
        feature "exactRange" "range literals providing an exact number of repetitions",
        feature "lowerBound" "range literals with a lower bound (only)",
        feature "starRange" "the * range literal",
        feature "upperBound" "range literals with an upper bound (only)"],

      defFeatureSet "Reading" "specific syntax related to reading data from the graph." [
        feature "union" "the UNION operator",
        feature "unionAll" "the UNION ALL operator",
        feature "unwind" "the UNWIND clause"],

      defFeatureSet "RelationshipDirection" "relationship directions / arrow patterns" [
        feature "both" "the two-headed arrow (<-[]->) relationship direction",
        feature "left" "the left arrow (<-[]-) relationship direction",
        feature "neither" "the headless arrow (-[]-) relationship direction",
        feature "right" "the right arrow (-[]->) relationship direction"],

      defFeatureSet "RelationshipPattern" "relationship patterns" [
        feature "multipleTypes" "specifying a disjunction of multiple types in a relationship pattern",
        fixedFeature "variableRelationship" "binding a variable to a relationship in a relationship pattern",
        feature "wildcardType" "omitting types from a relationship pattern"],

      defFeatureSet "Remove" "REMOVE operations" [
        feature "byLabel" "REMOVE Variable:NodeLabels",
        feature "byProperty" "REMOVE PropertyExpression"],

      defFeatureSet "Schema" "schema functions" [
          function "type",
          function "valueType"],

      defFeatureSet "Set" "set definitions" [
        feature "propertyEquals" "defining a set using PropertyExpression = Expression",
        feature "variableEquals" "defining a set using Variable = Expression",
        feature "variablePlusEquals" "defining a set using Variable += Expression",
        feature "variableWithNodeLabels" "defining a set using Variable:NodeLabels"],

      defFeatureSet "String" "string functions" [
        function "char_length",
        function "character_length",
        functionWithKeyword "contains" "CONTAINS",
        functionWithKeyword "endsWith" "ENDS WITH",
        functionWithKeyword "in" "IN",
        functionWithKeyword "startsWith" "STARTS WITH",
        function "toBoolean",
        function "toBooleanOrNull",
        function "toFloat",
        function "toFloatOrNull",
        function "toInteger",
        function "toIntegerOrNull"],

      defFeatureSet "Updating" "specific syntax related to updating data in the graph" [
        feature "create" "the CREATE clause",
        feature "set" "the SET clause",
        feature "with" "multi-part queries using WITH"]]
