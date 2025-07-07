{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Ext.Cypher.Features where

import Hydra.Kernel
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Terms.All as Tier2
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import Hydra.Sources.Ext.Cypher.Functions

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


data FeatureSet = FeatureSet {
  featureSetName :: String,
  featureSetDescription :: String,
  featureSetChildren :: [FeatureSet]}

openCypherFeaturesModule :: Module
openCypherFeaturesModule = Module ns elements [KernelTypes.hydraCoreModule] [KernelTypes.hydraCoreModule] $
    Just ("A model for characterizing OpenCypher queries and implementations in terms of included features."
      ++ "Based on the OpenCypher grammar and the list of standard Cypher functions at "
      ++ "https://neo4j.com/docs/cypher-manual/current/functions."
      ++ " Current as of August 2024.")
  where
    ns = Namespace "hydra.ext.cypher.features"
    cypherFeatures = typeref ns

    elements = featureSetToType <$> flatten openCypherFeatures
      where
        flatten fs = if L.null children then [] else (fs:(L.concat (flatten <$> children)))
          where
            children = featureSetChildren fs
    featureSetToType (FeatureSet name desc children) = datatype ns (featureSetName name) $
        doc (featureSetDesc desc) $ record (toField <$> children)
      where
        toField (FeatureSet name1 desc1 children1) = (decapitalize name1)>: if L.null children1
          then doc (featureSetDesc desc1) boolean
          else doc (featureSetDesc desc1) $ cypherFeatures $ featureSetName name1
          where
            fieldDesc = "Whether to expect " ++ desc1
    featureSetName name = capitalize name ++ "Features"
    featureSetDesc desc = capitalize desc

openCypherFeatures :: FeatureSet
openCypherFeatures =  FeatureSet "Cypher"
  ("A set of features which characterize an OpenCypher query or implementation. "
     ++ "Any features which are omitted from the set are assumed to be unsupported or nonrequired.") [

    FeatureSet "Arithmetic" "arithmetic operations" [
      feature "plus" "the + operator",
      feature "minus" "the - operator",
      feature "multiply" "the * operator",
      feature "divide" "the / operator",
      feature "modulus" "the % operator",
      feature "powerOf" "the ^ operator"],

    FeatureSet "Atom" "various kinds of atomic expressions" [
      feature "caseExpression" "CASE expressions",
      feature "count" "the COUNT (*) expression",
      feature "existentialSubquery" "existential subqueries",
      feature "functionInvocation" "function invocation",
      feature "parameter" "parameter expressions",
      feature "patternComprehension" "pattern comprehensions",
      feature "patternPredicate" "relationship patterns as subexpressions",
      fixed $ feature "variable" "variable expressions"],

    FeatureSet "Comparison" "comparison operators and functions" [
      feature "equal" "the = comparison operator",
      feature "greaterThan" "the > comparison operator",
      feature "greaterThanOrEqual" "the >= comparison operator",
      feature "lessThan" "the < comparison operator",
      feature "lessThanOrEqual" "the <= comparison operator",
      feature "notEqual" "the <> comparison operator"],

    FeatureSet "Delete" "delete operations" [
      feature "delete" "the basic DELETE clause",
      feature "detachDelete" "the DETACH DELETE clause"],

    FeatureSet "Function" "standard Cypher functions" (libraryToFeatureSet <$> cypherLibraries),

    FeatureSet "List" "list functionality" [
      feature "listComprehension" "basic list comprehensions",
      feature "listRange" "list range comprehensions (e.g. [1..10])"],

    FeatureSet "Literal" "various types of literal values" [
      fixed $ feature "boolean" "boolean literals",
      feature "double" "double-precision floating-point literals",
      feature "integer" "integer literals",
      feature "list" "list literals",
      feature "map" "map literals",
      feature "null" "the NULL literal",
      fixed $ feature "string" "string literals"],

    FeatureSet "Logical" "logical operations" [
      feature "and" "the AND operator",
      feature "not" "the NOT operator",
      feature "or" "the OR operator",
      feature "xor" "the XOR operator"],

    FeatureSet "Match" "match queries" [
      feature "match" "the basic (non-optional) MATCH clause",
      feature "optionalMatch" "OPTIONAL MATCH"],

    FeatureSet "Merge" "merge operations" [
      feature "merge" "the basic MERGE clause",
      feature "mergeOnCreate" "MERGE with the ON CREATE action",
      feature "mergeOnMatch" "MERGE with the ON MATCH action"],

    FeatureSet "NodePattern" "node patterns" [
      feature "multipleLabels" "specifying multiple labels in a node pattern",
      feature "parameter" "specifying a parameter as part of a node pattern",
      feature "propertyMap" "specifying a key/value map of properties in a node pattern",
      fixed $ feature "variableNode" "binding a variable to a node in a node pattern",
      feature "wildcardLabel" "omitting labels from a node pattern"],

    FeatureSet "Null" "IS NULL / IS NOT NULL checks" [
      feature "isNull" "the IS NULL operator",
      feature "isNotNull" "the IS NOT NULL operator"],

    FeatureSet "Path" "path functions only found in OpenCypher" [
      function "shortestPath"],

    FeatureSet "ProcedureCall" "procedure calls" [
      feature "inQueryCall" "CALL within a query",
      feature "standaloneCall" "standalone / top-level CALL",
      -- Note: additional features are possible around YIELD
      feature "yield" "the YIELD clause in CALL"],

    FeatureSet "Projection" "projections" [
      feature "limit" "the LIMIT clause",
      feature "orderBy" "the ORDER BY clause",
      feature "projectDistinct" "the DISTINCT keyword",
      feature "projectAll" "the * projection",
      feature "projectAs" "the AS keyword",
      feature "skip" "the SKIP clause",
      feature "sortOrder" "the ASC/ASCENDING and DESC/DESCENDING keywords"],

    FeatureSet "Quantifier" "quantifier expressions" [
      feature "all" "the ALL quantifier",
      feature "any" "the ANY quantifier",
      feature "none" "the NONE quantifier",
      feature "single" "the SINGLE quantifier"],

    FeatureSet "RangeLiteral" "range literals within relationship patterns" [
      feature "bounds" "range literals with both lower and upper bounds",
      feature "exactRange" "range literals providing an exact number of repetitions",
      feature "lowerBound" "range literals with a lower bound (only)",
      feature "starRange" "the * range literal",
      feature "upperBound" "range literals with an upper bound (only)"],

    FeatureSet "Reading" "specific syntax related to reading data from the graph." [
      feature "union" "the UNION operator",
      feature "unionAll" "the UNION ALL operator",
      feature "unwind" "the UNWIND clause"],

    FeatureSet "RelationshipDirection" "relationship directions / arrow patterns" [
      feature "both" "the two-headed arrow (<-[]->) relationship direction",
      feature "left" "the left arrow (<-[]-) relationship direction",
      feature "neither" "the headless arrow (-[]-) relationship direction",
      feature "right" "the right arrow (-[]->) relationship direction"],

    FeatureSet "RelationshipPattern" "relationship patterns" [
      feature "multipleTypes" "specifying a disjunction of multiple types in a relationship pattern",
      fixed $ feature "variableRelationship" "binding a variable to a relationship in a relationship pattern",
      feature "wildcardType" "omitting types from a relationship pattern"],

    FeatureSet "Remove" "REMOVE operations" [
      feature "byLabel" "REMOVE Variable:NodeLabels",
      feature "byProperty" "REMOVE PropertyExpression"],

    FeatureSet "Set" "set definitions" [
      feature "propertyEquals" "defining a set using PropertyExpression = Expression",
      feature "variableEquals" "defining a set using Variable = Expression",
      feature "variablePlusEquals" "defining a set using Variable += Expression",
      feature "variableWithNodeLabels" "defining a set using Variable:NodeLabels"],

    FeatureSet "String" "string functions/keywords only found in OpenCypher" [
      functionWithKeyword "contains" "CONTAINS",
      functionWithKeyword "endsWith" "ENDS WITH",
      functionWithKeyword "in" "IN",
      functionWithKeyword "startsWith" "STARTS WITH"],

    FeatureSet "Updating" "specific syntax related to updating data in the graph" [
      feature "create" "the CREATE clause",
      feature "set" "the SET clause",
      feature "with" "multi-part queries using WITH"]]
  where
    feature name desc = FeatureSet name desc []
    fixed (FeatureSet name desc children)
      = FeatureSet name (desc ++ " (note: included by most if not all implementations).") children
    function name = FeatureSet name (funDesc name Nothing Nothing) []
    funDesc name mkeyword mdesc = "the " ++ name ++ "() function" ++ keyword ++ desc
      where
        keyword = Y.maybe "" (\k -> " / " ++ k) mkeyword
        desc = Y.maybe "" (\d -> ". " ++ d) mdesc
    functionWithKeyword name keyword = FeatureSet name (funDesc name (Just keyword) Nothing) []
    libraryToFeatureSet (CypherLibrary name desc funs) = FeatureSet (capitalize name ++ "Function") desc (toFeature <$> funs)
      where
        toFeature (CypherFunction name keyword forms) = FeatureSet name (funDesc name keyword $ Just desc) []
          where
            -- Note: signatures are currently not used
            desc = L.intercalate "; " (cypherFunctionFormDescription <$> forms)

-- | An alternative model of (Open)Cypher features, flattened into an enumeration.
-- Usage:
--   writeProtobuf "/tmp/proto" [openCypherFeaturesEnumModule]
openCypherFeaturesEnumModule :: Module
openCypherFeaturesEnumModule = Module ns elements [KernelTypes.hydraCoreModule] [KernelTypes.hydraCoreModule] $
    Just ("A model with an enumeration of (Open)Cypher features.")
  where
    ns = Namespace "hydra.org/opencypher/features"
    def = datatype ns
    elements = [
      def "CypherFeature" $
        doc "An enumeration of (Open)Cypher features."
        openCypherFeaturesEnum]

openCypherFeaturesEnum :: Type
openCypherFeaturesEnum = union $ gatherFields True "" openCypherFeatures
  where
    gatherFields root prefix (FeatureSet name desc children) = if L.null children
        then [FieldType (Name selfName) $ doc (capitalize desc) unit]
        else L.concat (gatherFields False selfName <$> children)
      where
        --selfName = capitalize name
        selfName = if root
          then ""
          else prefix ++ stripFunctionSuffix (capitalize name)
    stripFunctionSuffix name = if "Function" `L.isSuffixOf` name && L.length name > flen
        then removeLastN flen name
        else name
      where
        removeLastN n xs = L.take (L.length xs - n) xs
        flen = L.length ("Function" :: String)
