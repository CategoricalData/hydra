{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier4.Langs.Kusto.Kql where

import Hydra.Sources.Tier3.All
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types


kqlModule :: Module Kv
kqlModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative.")
  where
    ns = Namespace "hydra/langs/kusto/kql"
    def = datatype ns
    kql = typeref ns

    elements = [

      def "AndExpression" $
        nonemptyList $ kql "BasicExpression",

      def "BasicExpression" $
        union [
          "any">: unit,
          "between">: kql "BetweenExpression",
          "binary">: kql "BinaryExpression",
          "column">: kql "ColumnName",
          "index">: kql "IndexExpression",
          "list">: list $ kql "Expression",
          "literal">: kql "Literal",
          "notBetween">: kql "BetweenExpression",
          "parentheses">: kql "Expression",
          "pipeline">: kql "PipelineExpression",
          "property">: kql "PropertyExpression",
          "unary">: kql "UnaryExpression",
          "union">: kql "UnionExpression"],

      def "BetweenExpression" $
        record [
          "expression">: kql "Expression",
          "lowerBound">: kql "Expression",
          "upperBound">: kql "Expression"],
          
      def "BinaryExpression" $
        record [
          "left">: kql "Expression",
          "operator">: kql "BinaryOperator",
          "right">: kql "Expression"],

      def "BinaryOperator" $
        enum [
          "caseInsensitiveEqual",
          "contains",
          "divide",
          "endsWith",
          "equal",
          "greater",
          "greaterOrEqual",
          "has",
          "hasPrefix",
          "hasSuffix",
          "less",
          "lessOrEqual",
          "matchesRegex",
          "minus",
          "notEqual",
          "plus",
          "startsWith",
          "times"],

      def "BuiltInFunction" $
        enum [
          "ago",
          "bin",
          "count",
          "dcount",
          "endofday",
          "extract",
          "format_datetime",
          "materialize",
          "now",
          "range",
          "startofday",
          "strcat",
          "todynamic"],

      def "ColumnAlias" $
        record [
          "column">: kql "ColumnName",
          "alias">: kql "ColumnName"],

      def "ColumnAssignment" $
        record [
          "column">: kql "ColumnName",
          "expression">: kql "Expression"],

      def "ColumnName" string,

      def "Columns" $
        union [
          "all">: unit,
          "single">: kql "ColumnName"],

      def "Command" $
        union [
          "count">: unit,
          "distinct">: nonemptyList $ kql "ColumnName",
          "extend">: nonemptyList $ kql "ColumnAssignment",
          "join">: kql "JoinCommand",
          "let">: nonemptyList $ kql "LetBinding",
          "limit">: int32,
          "mvexpand">: kql "ColumnName",
          "order by">: nonemptyList $ kql "SortBy",
          "parse">: kql "ParseCommand",
          "print">: kql "PrintCommand",
          "project">: nonemptyList $ kql "Projection",
          "projectAway">: nonemptyList $ kql "ColumnName",
          "projectRename">: nonemptyList $ kql "ColumnAlias",
          "render">: unit, -- TODO
          "search">: kql "SearchCommand",
          "sort">: nonemptyList $ kql "SortBy",
          "summarize">: kql "SummarizeCommand",
          "take">:
            doc "Limit a search to a specified number of results"
            int32,
          "top">: kql "TopCommand",
          "union">: kql "UnionExpression",
          "where">: kql "Expression"],

      def "DatasetName" string,

      def "Duration" $
        record [
          "value">: int32,
          "unit">: kql "DurationUnit"],

      def "DurationUnit" $
        enum ["second", "minute", "hour"],

      -- TODO
      def "Expression" $
        nonemptyList $ kql "OrExpression",

      def "Function" $
        union [
          "builtIn">: kql "BuiltInFunction",
          "custom">: kql "FunctionName"],
      
      def "FunctionExpression" $
        record [
          "function">: kql "Function",
          "arguments">: list $ kql "Expression"],

      def "FunctionName" string,
      
      def "IndexExpression" $
        record [
          "expression">: kql "Expression",
          "index">: string],

      def "JoinCommand" $
        record [
          "kind">: kql "JoinKind",
          "expression">: kql "DatasetName",
          "on">: kql "Expression"],

      def "JoinKind" $
        enum ["inner", "leftanti", "leftantisemi", "leftsemi", "outer", "rightanti", "rightantisemi", "rightsemi"],

      def "KeyValuePair" $
        record [
          "key">: string,
          "value">: kql "Expression"],

      def "LetBinding" $
        record [
          "name">: kql "ColumnName",
          "expression">: kql "Expression"],

      def "Literal" $
        union [
          "duration">: kql "Duration",
          "datetime">: string,
          "string">: string,
          -- TODO: unverified
          "int">: int32,
          "long">: int64,
          "double">: float64,
          "boolean">: boolean],

      def "OrExpression" $
        nonemptyList $ kql "AndExpression",

      def "Order" $
        enum ["ascending", "descending"],

      def "ParseCommand" $
        record [
          "column">: kql "ColumnName",
          "pairs">: nonemptyList $ kql "KeyValuePair"],

      -- TODO: what are these expressions actually called in KQL?
      def "PipelineExpression" $
        record [
          "dataset">: kql "DatasetName",
          "commands">: list $ kql "Command"],

      def "PrintCommand" $
        record [
          "column">: optional $ kql "ColumnName",
          "expression">: kql "Expression"],

      def "Projection" $
        record [
          "expression">: kql "Expression",
          "alias">: optional $ kql "ColumnName"],

      def "PropertyExpression" $
        record [
          "expression">: kql "Expression",
          "property">: string],

      def "Query" $ kql "PipelineExpression",

      def "SearchCommand" $
        doc "Search across all datasets and columns or, if provided, specific datasets and/or columns" $
        record [
          "datasets">: list $ kql "DatasetName",
          "pattern">: kql "Expression"],

      def "SummarizeCommand" $
        record [
           "columns">: nonemptyList $ kql "ColumnAssignment",
           "by">: list $ kql "ColumnName"],

      def "TopCommand" $
        record [
          "count">: int32,
          "sort">: list $ kql "SortBy"],

      def "SortBy" $
        record [
          "column">: kql "ColumnName",
          "order">: optional $ kql "Order"],

      def "UnaryExpression" $
        record [
          "operator">: kql "UnaryOperator",
          "expression">: kql "Expression"],

      def "UnaryOperator" $
        enum ["not"],

      def "UnionExpression" $
        nonemptyList $ kql "Expression"] -- TODO
