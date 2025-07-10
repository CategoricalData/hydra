{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Gql.PathAlgebra.Expressions where

import Hydra.Kernel
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

pathAlgExprNs = Namespace "com.gdblab.pathAlgebra.expressions"
expr = typeref pathAlgExprNs

pathAlgebraExpressionsModule :: Module
pathAlgebraExpressionsModule = Module pathAlgExprNs elements [] [] $
    Just "Algebraic expression trees for the path algebra by Angles et al., extended for GQL support"
  where
    def = datatype pathAlgExprNs
    doc s = setTypeDescription (Just s)

    elements = [
      -- Top-level query expression (beyond pure path algebra)
      def "QueryExpression" $
        doc "Complete query with path algebra and result projection" $
        record [
          "pathExpression">: expr "PathExpression",
          "resultProjection">: optional $ expr "ResultProjection"],

      -- Core expression tree type - recursive like Term
      def "PathExpression" $
        doc "A path algebra expression that evaluates to a set of paths" $
        union [
          "base">:
            doc "Base case: extract paths from graph" $
            expr "BaseExpression",
          "selection">:
            doc "Selection operator (σ): filter paths by condition" $
            expr "SelectionExpression",
          "join">:
            doc "Join operator (⊲⊳): concatenate compatible paths" $
            expr "JoinExpression",
          "union">:
            doc "Union operator (∪): combine path sets" $
            expr "UnionExpression",
          "recursive">:
            doc "Recursive operator (φ): compute transitive closure with semantics" $
            expr "RecursiveExpression",
          "groupBy">:
            doc "Group-by operator (γ): organize paths into solution space" $
            expr "GroupByExpression",
          "orderBy">:
            doc "Order-by operator (τ): sort solution space" $
            expr "OrderByExpression",
          "projection">:
            doc "Projection operator (π): extract paths from solution space" $
            expr "ProjectionExpression"],

      -- Base expressions - the atoms of the algebra
      def "BaseExpression" $
        doc "Base path expressions that extract paths from graph" $
        union [
          "paths0">:
            doc "Paths0(G): all paths of length 0 (nodes)" $
            expr "GraphReference",
          "paths1">:
            doc "Paths1(G): all paths of length 1 (edges)" $
            expr "GraphReference",
          "pathsStar">:
            doc "Paths*(G): all paths in graph (infinite without restrictions)" $
            expr "GraphReference"],

      def "GraphReference" $
        doc "Reference to a property graph" $
        wrap string, -- Could be more sophisticated

      -- Selection expressions
      def "SelectionExpression" $
        doc "Selection operator: σ_condition(expression)" $
        record [
          "condition">: expr "SelectionCondition",
          "expression">: expr "PathExpression"],

      def "SelectionCondition" $
        doc "Conditions for filtering paths" $
        union [
          "simple">: expr "SimpleCondition",
          "and">: expr "AndCondition",
          "or">: expr "OrCondition",
          "not">: expr "NotCondition"],

      def "SimpleCondition" $
        doc "Atomic selection conditions" $
        union [
          "labelEquals">: expr "LabelCondition",
          "propertyEquals">: expr "PropertyCondition",
          "propertyComparison">: expr "PropertyComparisonCondition",
          "lengthEquals">: expr "LengthCondition"],

      def "LabelCondition" $
        doc "Conditions on node/edge labels: label(node(i)) = v" $
        record [
          "target">: expr "PathElement",
          "value">: string],

      def "PropertyCondition" $
        doc "Property equality conditions: node(i).prop = v" $
        record [
          "target">: expr "PathElement",
          "property">: string,
          "value">: expr "LiteralValue"],

      def "PropertyComparisonCondition" $
        doc "Property comparison conditions: node(i).prop > v, etc." $
        record [
          "target">: expr "PathElement",
          "property">: string,
          "operator">: expr "ComparisonOperator",
          "value">: expr "LiteralValue"],

      def "ComparisonOperator" $
        doc "Comparison operators for property conditions" $
        enum [
          "equal",           -- =
          "notEqual",        -- !=
          "lessThan",        --
          "lessThanOrEqual", -- <=
          "greaterThan",     -- >
          "greaterThanOrEqual"], -- >=

      def "LiteralValue" $
        doc "Literal values for comparisons" $
        union [
          "string">: string,
          "integer">: int32,
          "float">: float64,
          "boolean">: boolean],

      def "LengthCondition" $
        doc "Condition on path length: len() = i" $
        record [
          "length">: int32],

      def "PathElement" $
        doc "References to elements within a path" $
        union [
          "node">: int32,      -- node(i)
          "edge">: int32,      -- edge(i)
          "first">: unit,      -- first node
          "last">: unit],      -- last node

      def "AndCondition" $
        record [
          "left">: expr "SelectionCondition",
          "right">: expr "SelectionCondition"],

      def "OrCondition" $
        record [
          "left">: expr "SelectionCondition",
          "right">: expr "SelectionCondition"],

      def "NotCondition" $
        record [
          "condition">: expr "SelectionCondition"],

      -- Join expressions
      def "JoinExpression" $
        doc "Join operator: expr1 ⊲⊳ expr2" $
        record [
          "left">: expr "PathExpression",
          "right">: expr "PathExpression"],

      -- Union expressions
      def "UnionExpression" $
        doc "Union operator: expr1 ∪ expr2" $
        record [
          "left">: expr "PathExpression",
          "right">: expr "PathExpression"],

      -- Recursive expressions
      def "RecursiveExpression" $
        doc "Recursive operator with path semantics" $
        record [
          "semantics">: expr "PathSemantics",
          "expression">: expr "PathExpression"],

      def "PathSemantics" $
        doc "Path semantics for recursive operations" $
        enum [
          "walk",     -- φ_Walk: no restrictions
          "trail",    -- φ_Trail: no repeated edges
          "acyclic",  -- φ_Acyclic: no repeated nodes
          "simple",   -- φ_Simple: no repeated nodes except first=last
          "shortest"], -- φ_Shortest: shortest paths only

      -- Extended algebra - Solution spaces
      def "SolutionSpaceExpression" $
        doc "Expressions that work with solution spaces" $
        union [
          "groupBy">: expr "GroupByExpression",
          "orderBy">: expr "OrderByExpression"],

      def "GroupByExpression" $
        doc "Group-by operator: γ_criterion(expression)" $
        record [
          "criterion">: expr "GroupByCriterion",
          "expression">: expr "PathExpression"],

      def "GroupByCriterion" $
        doc "Grouping criteria corresponding to paper's γ variants" $
        enum [
          "none",              -- γ: single partition, single group
          "source",            -- γ_S: partition by source node
          "target",            -- γ_T: partition by target node
          "length",            -- γ_L: group by length
          "sourceTarget",      -- γ_ST: partition by source-target pairs
          "sourceLength",      -- γ_SL: partition by source, group by length
          "targetLength",      -- γ_TL: partition by target, group by length
          "sourceTargetLength"], -- γ_STL: partition by source-target, group by length

      def "OrderByExpression" $
        doc "Order-by operator: τ_criterion(solutionSpace)" $
        record [
          "criterion">: expr "OrderByCriterion",
          "expression">: expr "SolutionSpaceExpression"],

      def "OrderByCriterion" $
        doc "Ordering criteria corresponding to paper's τ variants" $
        enum [
          "partition",           -- P: sort partitions by shortest path length
          "group",               -- G: sort groups by shortest path length
          "path",                -- A: sort paths by length
          "partitionGroup",      -- PG: sort partitions and groups
          "partitionPath",       -- PA: sort partitions and paths
          "groupPath",           -- GA: sort groups and paths
          "partitionGroupPath"], -- PGA: sort all levels

      def "ProjectionExpression" $
        doc "Projection operator: π_(#P,#G,#A)(solutionSpace)" $
        record [
          "partitions">: expr "ProjectionSpec",
          "groups">: expr "ProjectionSpec",
          "paths">: expr "ProjectionSpec",
          "expression">: expr "SolutionSpaceExpression"],

      def "ProjectionSpec" $
        doc "Projection specification: * or specific number" $
        union [
          "all">: unit,
          "limited">: int32],

      -- Result projection (beyond pure path algebra)
      def "ResultProjection" $
        doc "Extract specific values from paths for RETURN clause" $
        record [
          "projections">: list $ expr "PropertyExtraction"],

      def "PropertyExtraction" $
        doc "Extract properties from path elements" $
        record [
          "alias">: optional string,  -- Optional alias for the result column
          "source">: expr "PropertySource"],

      def "PropertySource" $
        doc "Source of a property value" $
        union [
          "nodeProperty">: expr "NodePropertyRef",
          "edgeProperty">: expr "EdgePropertyRef",
          "pathProperty">: expr "PathPropertyRef"],

      def "NodePropertyRef" $
        doc "Reference to a node property: node.property" $
        record [
          "element">: expr "PathElement",
          "property">: string],

      def "EdgePropertyRef" $
        doc "Reference to an edge property: edge.property" $
        record [
          "element">: expr "PathElement",
          "property">: string],

      def "PathPropertyRef" $
        doc "Reference to path-level properties: length, etc." $
        enum [
          "length",
          "startNode",
          "endNode"]]
