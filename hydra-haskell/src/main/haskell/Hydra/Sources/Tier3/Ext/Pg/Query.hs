{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier3.Ext.Pg.Query where

import Hydra.Sources.Tier2.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types
import Hydra.Sources.Tier0.Core
import Hydra.Sources.Tier3.Ext.Pg.Model


pgQueryModule :: Module
pgQueryModule = Module ns elements [pgModelModule] [hydraCoreModule] $
    Just ("A common model for pattern-matching queries over property graphs")
  where
    ns = Namespace "hydra.pg.query"
    pg = typeref $ moduleNamespace pgModelModule
    q = typeref ns
    def = datatype ns

    elements = [

      -- table of bindings to list of integers
      def "AggregationQuery" $
        union [
          "count">: unit],

      def "ApplicationQuery" $
        nonemptyList $ q "Query",

      def "AssociativeExpression" $
        record [
          "operator">: q "BinaryOperator",
          "operands">: nonemptyList $ q "Expression"],

      def "BinaryExpression" $
        record [
          "left">: q "Expression",
          "operator">: q "BinaryOperator",
          "right">: q "Expression"],

      def "BinaryBooleanOperator" $
        enum ["and", "or", "xor"],

      def "BinaryOperator" $
        union [
          "boolean">: q "BinaryBooleanOperator",
          "comparison">: q "ComparisonOperator",
          "power">: unit],

      def "Binding" $
        record [
          "key">: q "Variable",
          "value">: q "Query"],

      def "ComparisonOperator" $
        enum ["eq", "neq", "lt", "lte", "gt", "gte"],

      def "EdgeProjectionPattern" $
        record [
          "direction">: pg "Direction",
          "label">: optional $ pg "EdgeLabel",
          "properties">: list $ q "PropertyPattern",
          "vertex">: optional $ q "VertexPattern"],

      def "Expression" $
        union [
          "associative">: q "AssociativeExpression",
          "binary">: q "BinaryExpression",
          "property">: q "PropertyProjection",
          "unary">: q "UnaryExpression",
          "variable">: q "Variable",
          "vertex">: q "VertexPattern"],

      def "LetQuery" $
        record [
          "bindings">: list $ q "Binding",
          "environment">: q "Query"],

      def "MatchQuery" $
        record [
          "optional">: boolean,
          "pattern">: list $ q "Projection",
          "where">: optional $ q "Expression"],

      def "Projection" $
        record [
          "value">: q "Expression",
          "as">: optional $ q "Variable"],

      def "Projections" $
        record [
          "all">: boolean,
          "explicit">: list $ q "Projection"],

      def "PropertyPattern" $
        record [
          "key">: pg "PropertyKey",
          "value">: q "PropertyValuePattern"],

      def "PropertyProjection" $
        record [
          "base">: q "Expression",
          "key">: pg "PropertyKey"],

      -- TODO: temporary
      def "PropertyValue" $ string,

      def "PropertyValuePattern" $
        union [
          "variable">: pg "PropertyKey",
          "value">: string], -- TODO: re-use pg property value parameter

      def "Query" $
        union [
          "application">: q "ApplicationQuery",
          "aggregate">: q "AggregationQuery",
          "LetQuery">: q "LetQuery",
          "match">: q "MatchQuery",
          "select">: q "SelectQuery",
          "value">: string],

      def "SelectQuery" $
        record [
          "distinct">: boolean,
          "projection">: q "Projections"],

      def "UnaryExpression" $
        record [
          "operator">: q "UnaryOperator",
          "operand">: q "Expression"],

      def "UnaryOperator" $
        enum ["negate"],

      def "Variable" string,

      def "VertexPattern" $
        record [
          "variable">: optional $ q "Variable",
          "label">: optional $ pg "VertexLabel",
          "properties">: list $ q "PropertyPattern",
          "edges">: list $ q "EdgeProjectionPattern"]]
