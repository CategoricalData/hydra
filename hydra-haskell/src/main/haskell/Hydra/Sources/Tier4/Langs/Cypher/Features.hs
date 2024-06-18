{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Cypher.Features where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import qualified Data.Maybe as Y


openCypherFeaturesModule :: Module Kv
openCypherFeaturesModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A model for characterizing OpenCypher queries and implementations in terms of included features.")
  where
    ns = Namespace "hydra/langs/cypher/features"
    core = typeref $ moduleNamespace hydraCoreModule
    cypherFeatures = typeref ns
    def = datatype ns
    featureField name comment = (name ++ "Feature")>: doc comment boolean
    defFeatureSet name desc fields = def (capitalize name ++ "Features") $
        doc ("A set of features for " ++ desc ++ ".") $ record fields
    featureSet name desc = (name ++ "Features")>:
      doc ("Whether to expect " ++ desc ++ ", and if so, which specific features") $
      optional $ cypherFeatures $ capitalize name ++ "Features"
    feature n s = featureField n $ "Whether to expect " ++ s ++ "."
    fixedFeature n s = featureField n $ "Whether to expect " ++ s ++ " (note: included by most if not all implementations)."

    elements = [

      defFeatureSet "Aggregate" "aggregation functions" [
        feature "average" "the AVG aggregate function",
        feature "count" "the COUNT aggregate function",
        feature "max" "the MAX aggregate function",
        feature "min" "the MIN aggregate function",
        feature "sum" "the SUM aggregate function"],

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
        featureSet "listComprehension" "list comprehensions",
        featureSet "literal" "literal values",
        feature "parameter" "parameter expressions",
        feature "patternComprehension" "pattern comprehensions",
        feature "patternPredicate" "relationship patterns as subexpressions",
        featureSet "quantifier" "quantifier expressions",
        fixedFeature "variable" "variable expressions"],

      defFeatureSet "Comparison" "comparison operations" [
        feature "equal" "the = comparison operator",
        feature "greaterThan" "the > comparison operator",
        feature "greaterThanOrEqual" "the >= comparison operator",
        feature "lessThan" "the < comparison operator",
        feature "lessThanOrEqual" "the <= comparison operator",
        feature "notEqual" "the <> comparison operator"],

      def "CypherFeatures" $
        doc ("A set of features which characterize an OpenCypher query or implementation. "
          ++ "Any features which are omitted from the set are assumed to be unsupported or nonrequired.") $
        record [
          featureSet "aggregate" "aggregate functions",
          featureSet "arithmetic" "arithmetic operations",
          featureSet "atom" "atomic expressions",
          featureSet "comparison" "comparison operations",
          featureSet "delete" "delete operations",
          featureSet "logical" "logical operations",
          featureSet "match" "match queries",
          featureSet "merge" "merge operations",
          featureSet "nodePattern" "node patterns",
          featureSet "null" "IS NULL / IS NOT NULL checks",
          featureSet "procedureCall" "procedure calls",
          featureSet "projection" "projection operations",
          featureSet "rangeLiteral" "range literals",
          featureSet "reading" "reading operations",
          featureSet "relationshipDirection" "relationship directions",
          featureSet "relationshipPattern" "relationship patterns",
          featureSet "remove" "remove operations",
          featureSet "set" "set operations",
          featureSet "string" "string operations",
          featureSet "updating" "updating operations"],

      defFeatureSet "Delete" "delete operations" [
        feature "delete" "the basic DELETE clause",
        feature "detachDelete" "the DETACH DELETE clause"],

      defFeatureSet "ListComprehension" "list comprehensions" [
        feature "listComprehension" "basic list comprehensions",
        feature "listRange" "list range comprehensions (e.g. [1..10])"],

      defFeatureSet "Literal" "various types of literal values" [
        fixedFeature "booleanLiteral" "boolean literals",
        feature "doubleLiteral" "double-precision floating-point literals",
        feature "integerLiteral" "integer literals",
        feature "listLiteral" "list literals",
        feature "mapLiteral" "map literals",
        feature "nullLiteral" "the NULL literal",
        fixedFeature "stringLiteral" "string literals"],

      defFeatureSet "Logical" "logical operations" [
        feature "and" "the AND operator",
        feature "not" "the NOT operator",
        feature "or" "the OR operator",
        feature "xor" "the XOR operator"],

      defFeatureSet "Match" "match queries" [
        feature "optionalMatch" "OPTIONAL MATCH"],

      defFeatureSet "Merge" "merge operations" [
        feature "mergeOnCreate" "MERGE with the ON CREATE action",
        feature "mergeOnMatch" "MERGE with the ON MATCH action"],

      defFeatureSet "NodePattern" "node patterns" [
        feature "multipleLabelsInNodePattern" "specifying multiple labels in a node pattern",
        feature "nodePatternParameter" "specifying a parameter as part of a node pattern",
        feature "nodePatternPropertyMap" "specifying a key/value map of properties in a node pattern",
        fixedFeature "variableNode" "binding a variable to a node in a node pattern",
        feature "wildcardLabelNodePattern" "omitting labels from a node pattern"],

      defFeatureSet "Null" "IS NULL / IS NOT NULL checks" [
        feature "isNull" "the IS NULL operator",
        feature "isNotNull" "the IS NOT NULL operator"],

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
        feature "none" "the NONE quantifier",
        feature "single" "the SINGLE quantifier"],

      defFeatureSet "RangeLiteral" "range literals within relationship patterns" [
        feature "starRangeLiteral" "the * range literal",
        feature "exactRangeLiteral" "range literals providing an exact number of repetitions",
        feature "rangeLiteralWithLowerBound" "range literals with a lower bound (only)",
        feature "rangeLiteralWithUpperBound" "range literals with an upper bound (only)",
        feature "rangeLiteralWithBounds" "range literals with both lower and upper bounds"],

      defFeatureSet "Reading" "specific syntax related to reading data from the graph." [
        feature "union" "the UNION operator",
        feature "unionAll" "the UNION ALL operator",
        feature "unwind" "the UNWIND clause"],

      defFeatureSet "RelationshipDirection" "relationship directions / arrow patterns" [
        feature "bothArrow" "the two-headed arrow (<-[]->) relationship direction",
        feature "leftArrow" "the left arrow (<-[]-) relationship direction",
        feature "neitherArrow" "the headless arrow (-[]-) relationship direction",
        feature "rightArrow" "the right arrow (-[]->) relationship direction"],

      defFeatureSet "RelationshipPattern" "relationship patterns" [
        feature "multipleTypesInRelationshipPattern" "specifying a disjunction of multiple types in a relationship pattern",
        fixedFeature "variableRelationship" "binding a variable to a relationship in a relationship pattern",
        feature "wildcardTypeInRelationshipPattern" "omitting types from a relationship pattern"],

      defFeatureSet "Remove" "REMOVE operations" [
        feature "removeByLabel" "REMOVE Variable:NodeLabels",
        feature "removeByProperty" "REMOVE PropertyExpression"],

      defFeatureSet "Set" "set definitions" [
        feature "setPropertyEquals" "defining a set using PropertyExpression = Expression",
        feature "setVariableEquals" "defining a set using Variable = Expression",
        feature "setVariablePlusEquals" "defining a set using Variable += Expression",
        feature "setVariableWithNodeLabels" "defining a set using Variable:NodeLabels"],

      defFeatureSet "String" "string functions" [
        feature "contains" "the CONTAINS function",
        feature "endsWith" "the ENDS WITH function",
        feature "in" "the IN function",
        feature "startsWith" "the STARTS WITH function"],

      defFeatureSet "Updating" "specific syntax related to updating data in the graph" [
        feature "create" "the CREATE clause",
        feature "set" "the SET clause",
        feature "with" "multi-part queries using WITH"]]
