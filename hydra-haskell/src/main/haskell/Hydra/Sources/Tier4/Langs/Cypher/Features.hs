{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Cypher.Features where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


openCypherFeaturesModule :: Module Kv
openCypherFeaturesModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A model for characterizing OpenCypher implementations in terms of supported features.")
  where
    ns = Namespace "hydra/langs/cypher/features"
    core = typeref $ moduleNamespace hydraCoreModule
    features = typeref ns
    def = datatype ns
    supports name comment = ("supports" ++ capitalize name)>: doc comment boolean
    sometimes n s = supports n $ "Supports " ++ s ++ "."
    usually n s = supports n $ "Supports" ++ s ++ " (note: should be true in most if not all implementations)."

    elements = [

      def "AggregateSupport" $
        doc ("A feature set indicating that a Cypher implementation supports aggregates, "
          ++ "along with specifically supported aggregation features.") $
        record [
          sometimes "average" "the AVG aggregate function",
          sometimes "count" "the COUNT aggregate function",
          sometimes "max" "the MAX aggregate function",
          sometimes "min" "the MIN aggregate function",
          sometimes "sum" "the SUM aggregate function"],

      def "ArithmeticSupport" $
        record [
          sometimes "plus" "the + operator",
          sometimes "minus" "the - operator",
          sometimes "multiply" "the * operator",
          sometimes "divide" "the / operator",
          sometimes "modulus" "the % operator",
          sometimes "powerOf" "the ^ operator"],
      
      def "ComparisonSupport" $
        doc ("A feature set indicating that a Cypher implementation supports comparison operations, "
          ++ "along with specifically supported comparison operators.") $
        record [
          sometimes "equal" "the = comparison operator",
          sometimes "greaterThan" "the > comparison operator",
          sometimes "greaterThanOrEqual" "the >= comparison operator",
          sometimes "lessThan" "the < comparison operator",
          sometimes "lessThanOrEqual" "the <= comparison operator",
          sometimes "notEqual" "the <> comparison operator"],

      def "AtomSupport" $
        doc ("A feature set indicating that a Cypher implementation supports various kinds of atomic expressions.") $
        record [
          sometimes "caseExpression" "CASE expressions",
          sometimes "count" "the COUNT (*) expression",
          sometimes "existentialSubquery" "existential subqueries",
          sometimes "functionInvocation" "function invocation",
          "listComprehension">: optional $ features "ListComprehensionSupport",
          "literal">: optional $ features "LiteralSupport",
          sometimes "parameter" "parameter expressions",
          sometimes "patternComprehension" "pattern comprehensions",
          sometimes "patternPredicate" "relationship patterns as subexpressions",
          "quantifier">: optional $ features "QuantifierSupport",
          usually "variable" "variable expressions"],

      -- TODO: check completeness
      def "CypherSupport" $
        doc ("A feature set indicating that a Cypher implementation supports specific features. "
          ++ "Any features which are omitted from the set are assumed to be unsupported.") $
        record [
          "aggregate">: optional $ features "AggregateSupport",
          "arithmetic">: optional $ features "ArithmeticSupport",
          "atom">: optional $ features "AtomSupport",
          "comparison">: optional $ features "ComparisonSupport",
          "delete">: optional $ features "DeleteSupport",
          "logical">: optional $ features "LogicalSupport",
          "match">: optional $ features "MatchSupport",
          "merge">: optional $ features "MergeSupport",
          "nodePattern">: optional $ features "NodePatternSupport",
          "null">: optional $ features "NullSupport",
          "procedureCall">: optional $ features "ProcedureCallSupport",
          "projection">: optional $ features "ProjectionSupport",
          "rangeLiteral">: optional $ features "RangeLiteralSupport",
          "reading">: optional $ features "ReadingSupport",
          "relationshipDirection">: optional $ features "RelationshipDirectionSupport",
          "relationshipPattern">: optional $ features "RelationshipPatternSupport",
          "remove">: optional $ features "RemoveSupport",
          "set">: optional $ features "SetSupport",
          "string">: optional $ features "StringSupport",
          "updating">: optional $ features "UpdatingSupport"],

      def "DeleteSupport" $
        record [
          sometimes "delete" "the basic DELETE clause",
          sometimes "detachDelete" "the DETACH DELETE clause"],

      def "ListComprehensionSupport" $
        record [
          sometimes "listComprehension" "list comprehensions",
          sometimes "listRange" "list range (e.g. [1..10])"],

      def "LiteralSupport" $
        record [
          usually "booleanLiteral" "boolean literals",
          sometimes "doubleLiteral" "double-precision floating-point literals",
          sometimes "integerLiteral" "integer literals",
          sometimes "listLiteral" "list literals",
          sometimes "mapLiteral" "map literals",
          sometimes "nullLiteral" "the NULL literal",
          usually "stringLiteral" "string literals"],

      def "LogicalSupport" $
        doc ("A feature set indicating that a Cypher implementation supports logical operations, "
          ++ "along with specifically supported logical operators.") $
        record [
          sometimes "and" "the AND operator",
          sometimes "not" "the NOT operator",
          sometimes "or" "the OR operator",
          sometimes "xor" "the XOR operator"],
          
      def "MatchSupport" $
        doc ("Indicates that a Cypher implementation supports match queries (as most do), "
          ++ "along with specifically supported match features.") $
        record [
          sometimes "optionalMatch" "OPTIONAL MATCH"],

      def "MergeSupport" $
         doc "..." $
          record [
            sometimes "mergeOnCreate" "MERGE with the ON CREATE action",
            sometimes "mergeOnMatch" "MERGE with the ON MATCH action"],

      def "NodePatternSupport" $
        record [
          sometimes "multipleLabelsInNodePattern" "specifying multiple labels in a node pattern",
          sometimes "nodePatternParameter" "specifying a parameter as part of a node pattern",
          sometimes "nodePatternPropertyMap" "specifying a key/value map of properties in a node pattern",
          usually "variableNode" "binding a variable to a node in a node pattern",
          sometimes "wildcardLabelNodePattern" "omitting labels from a node pattern"],

      def "NullSupport" $
        record [
          sometimes "isNull" "the IS NULL operator",
          sometimes "isNotNull" "the IS NOT NULL operator"],

      def "ProcedureCallSupport" $
        doc ("Indicates that a Cypher implementation supports procedure calls, "
          ++ "along with specifically supported procedure call features.") $
        record [
          sometimes "inQueryCall" "CALL within a query",
          sometimes "standaloneCall" "standalone / top-level CALL",
          -- Note: additional features are possible around YIELD
          sometimes "yield" "the YIELD clause in CALL"],

      def "ProjectionSupport" $
        record [
          sometimes "limit" "the LIMIT clause",
          sometimes "orderBy" "the ORDER BY clause",
          sometimes "projectDistinct" "the DISTINCT keyword",
          sometimes "projectAll" "the * projection",
          sometimes "projectAs" "the AS keyword",
          sometimes "skip" "the SKIP clause",
          sometimes "sortOrder" "the ASC/ASCENDING and DESC/DESCENDING keywords"],

      def "QuantifierSupport" $
        doc ("A feature set indicating that a Cypher implementation supports quantifier expressions, "
          ++ "along with specifically supported quantifiers.") $
        record [
          sometimes "all" "the ALL quantifier",
          sometimes "any" "the ANY quantifier",
          sometimes "none" "the NONE quantifier",
          sometimes "single" "the SINGLE quantifier"],

      def "RangeLiteralSupport" $
        doc ("A feature set indicating that a Cypher implementation supports range literals within relationship patterns, "
          ++ "along with specifically supported range syntax.") $
        record [
          sometimes "starRangeLiteral" "the * range literal",
          sometimes "exactRangeLiteral" "range literals providing an exact number of repetitions",
          sometimes "rangeLiteralWithLowerBound" "range literals with a lower bound (only)",
          sometimes "rangeLiteralWithUpperBound" "range literals with an upper bound (only)",
          sometimes "rangeLiteralWithBounds" "range literals with both lower and upper bounds"],

      def "ReadingSupport" $
        doc ("A feature set capturing specific features related to reading data from the graph.") $
        record [
          sometimes "union" "the UNION operator",
          sometimes "unionAll" "the UNION ALL operator",
          sometimes "unwind" "the UNWIND clause"],

      def "RelationshipDirectionSupport" $
        doc ("A feature set indicating supported relationship directions / arrow patterns.") $
        record [
          sometimes "bothArrow" "the two-headed arrow (<-[]->) relationship direction",
          sometimes "leftArrow" "the left arrow (<-[]-) relationship direction",
          sometimes "neitherArrow" "the headless arrow (-[]-) relationship direction",
          sometimes "rightArrow" "the right arrow (-[]->) relationship direction"],

      def "RelationshipPatternSupport" $
        doc ("A feature set indicating that a Cypher implementation supports relationship patterns, "
          ++ "along with specifically supported relationship pattern features.") $
        record [
          sometimes "multipleTypesInRelationshipPattern" "specifying a disjunction of multiple types in a relationship pattern",
          usually "variableRelationship" "binding a variable to a relationship in a relationship pattern",
          sometimes "wildcardTypeInRelationshipPattern" "omitting types from a relationship pattern"],

      def "RemoveSupport" $
        doc ("Indicates that a Cypher implementation supports REMOVE operations, "
          ++ "along with specifically supported REMOVE features.") $
        record [
          sometimes "removeByLabel" "REMOVE Variable:NodeLabels",
          sometimes "romeveByProperty" "REMOVE PropertyExpression"],

      def "SetSupport" $
        doc ("A feature set indicating that a Cypher implementation supports set definitions, "
          ++ "along with specifically supported set features.") $
        record [
          sometimes "setPropertyEquals" "defining a set using PropertyExpression = Expression",
          sometimes "setVariableEquals" "defining a set using Variable = Expression",
          sometimes "setVariablePlusEquals" "defining a set using Variable += Expression",
          sometimes "setVariableWithNodeLabels" "defining a set using Variable:NodeLabels"],

      def "StringSupport" $
        doc ("A feature set indicating that a Cypher implementation supports string functions, "
          ++ "along with specifically supported string functions.") $
        record [
          sometimes "contains" "the CONTAINS function",
          sometimes "endsWith" "the ENDS WITH function",
          sometimes "in" "the IN function",
          sometimes "startsWith" "the STARTS WITH function"],
          
      def "UpdatingSupport" $
        doc ("A feature set capturing specific features related to updating data in the graph.") $
        record [
          sometimes "create" "the CREATE clause",
          sometimes "set" "the SET clause",
          sometimes "with" "multi-part queries using WITH"]]
