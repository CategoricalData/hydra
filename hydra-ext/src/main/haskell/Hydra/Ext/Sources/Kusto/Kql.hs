{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Kusto.Kql where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import Hydra.Dsl.Types as Types

import qualified Hydra.Sources.Kernel.Types.Accessors   as Accessors
import qualified Hydra.Sources.Kernel.Types.Ast         as Ast
import qualified Hydra.Sources.Kernel.Types.Coders      as Coders
import qualified Hydra.Sources.Kernel.Types.Compute     as Compute
import qualified Hydra.Sources.Kernel.Types.Constraints as Constraints
import qualified Hydra.Sources.Kernel.Types.Core        as Core
import qualified Hydra.Sources.Kernel.Types.Grammar     as Grammar
import qualified Hydra.Sources.Kernel.Types.Graph       as Graph
import qualified Hydra.Sources.Kernel.Types.Json        as Json
import qualified Hydra.Sources.Kernel.Types.Mantle      as Mantle
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow


kqlModule :: Module
kqlModule = Module ns elements [Core.module_] [Core.module_] $
    Just ("A partial KQL (Kusto Query Language) model, based on examples from the documentation. Not normative.")
  where
    ns = Namespace "hydra.ext.com.microsoft.kusto.kql"
    def = datatype ns
    kql = typeref ns

    elements = [

      def "BetweenExpression" $
        record [
          "not">: boolean,
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

      def "ColumnName" $ wrap string,

      def "Columns" $
        union [
          "all">: unit,
          "single">: kql "ColumnName"],

      def "Command" $
        union [
          "count">: unit,
          "distinct">:
            doc "See https://learn.microsoft.com/en-us/azure/data-explorer/kusto/query/distinct-operator" $
            nonemptyList $ kql "ColumnName",
          "extend">: nonemptyList $ kql "ColumnAssignment",
          "join">: kql "JoinCommand",
          "limit">: int32,
          "mvexpand">: kql "ColumnName",
          "orderBy">: nonemptyList $ kql "SortBy",
          "parse">: kql "ParseCommand",
          "print">: kql "PrintCommand",
          "project">: nonemptyList $ kql "Projection",
          "projectAway">: nonemptyList $ kql "ColumnName",
          "projectRename">: nonemptyList $ kql "ColumnAlias",
          "render">: string,
          "search">: kql "SearchCommand",
          "sortBy">: nonemptyList $ kql "SortBy",
          "summarize">: kql "SummarizeCommand",
          "take">:
            doc "Limit a search to a specified number of results"
            int32,
          "top">: kql "TopCommand",
          "union">: kql "UnionCommand",
          "where">: kql "Expression"],

      def "Datetime" $ wrap string,

      def "Duration" $
        record [
          "value">: int32,
          "unit">: kql "DurationUnit"],

      def "DurationUnit" $
        enum ["second", "minute", "hour"],

      def "Expression" $
        union [
          "and">: nonemptyList $ kql "Expression",
          "any">: unit,
          "between">: kql "BetweenExpression",
          "binary">: kql "BinaryExpression",
          "braces">: kql "Expression", -- TODO: what do braces represent? E.g. "let timeRange = {TimeRange}"
          "column">: kql "ColumnName",
          "dataset">: kql "TableName",
          "index">: kql "IndexExpression",
          "list">: list $ kql "Expression",
          "literal">: kql "Literal",
          "or">: nonemptyList $ kql "Expression",
          "parentheses">: kql "Expression",
          "property">: kql "PropertyExpression",
          "unary">: kql "UnaryExpression"],

      def "Function" $
        union [
          "builtIn">: kql "BuiltInFunction",
          "custom">: kql "FunctionName"],

      def "FunctionExpression" $
        record [
          "function">: kql "Function",
          "arguments">: list $ kql "Expression"],

      def "FunctionName" $ wrap string,

      def "IndexExpression" $
        record [
          "expression">: kql "Expression",
          "index">: string],

      def "JoinCommand" $
        record [
          "kind">: kql "JoinKind",
          "expression">: kql "TableName",
          "on">: kql "Expression"],

      def "JoinKind" $
        enum ["leftouter", "leftsemi", "leftanti", "fullouter", "inner", "innerunique", "rightouter", "rightsemi", "rightanti"],

      def "KeyValuePair" $
        record [
          "key">: string,
          "value">: kql "Expression"],

      def "LetBinding" $
        record [
          "name">: kql "ColumnName",
          "expression">: kql "Expression"],

      def "LetExpression" $
        record [
          "bindings">: nonemptyList $ kql "LetBinding",
          "expression">: kql "TabularExpression"],

      def "Literal" $
        union [
          "duration">: kql "Duration",
          "datetime">: kql "Datetime",
          "string">: string,
          -- TODO: unverified
          "int">: int32,
          "long">: int64,
          "double">: float64,
          "boolean">: boolean],

      def "Order" $
        enum ["ascending", "descending"],

      def "Parameter" $
        record [
          "key">: string,
          "value">: kql "Literal"],

      def "ParseCommand" $
        record [
          "column">: kql "ColumnName",
          "pairs">: nonemptyList $ kql "KeyValuePair"],

      -- TODO: what are these expressions actually called in KQL?
      def "PipelineExpression" $
        wrap $ nonemptyList $ kql "TabularExpression",

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

      def "Query" $ wrap $ kql "TabularExpression",

      def "SearchCommand" $
        doc "Search across all datasets and columns or, if provided, specific datasets and/or columns" $
        record [
          "datasets">: list $ kql "TableName",
          "pattern">: kql "Expression"],

      def "SummarizeCommand" $
        record [
           "columns">: nonemptyList $ kql "ColumnAssignment",
           "by">: list $ kql "ColumnName"],

      def "TableName" $ wrap string,

      def "TopCommand" $
        record [
          "count">: int32,
          "sort">: list $ kql "SortBy"],

      def "SortBy" $
        record [
          "column">: kql "ColumnName",
          "order">: optional $ kql "Order"],

      def "TabularExpression" $
        union [
          "command">: kql "Command",
          "pipeline">: kql "PipelineExpression",
          "let">: kql "LetExpression",
          "table">: kql "TableName"],

      def "UnaryExpression" $
        record [
          "operator">: kql "UnaryOperator",
          "expression">: kql "Expression"],

      def "UnaryOperator" $
        enum ["not"],

      def "UnionCommand" $
        record [
          "parameters">: list $ kql "Parameter",
          "kind">: optional $ kql "UnionKind",
          "withSource">: optional $ kql "ColumnName",
          "isFuzzy">: optional boolean,
          "tables">: nonemptyList $ kql "TableName"],

      def "UnionKind" $
        enum ["inner", "outer"]]
