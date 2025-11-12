{-# LANGUAGE OverloadedStrings #-}

module Hydra.Ext.Sources.Pg.Graphson.Syntax where

-- Standard imports for type-level sources outside of the kernel
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
import qualified Hydra.Sources.Kernel.Types.Meta        as Meta
import qualified Hydra.Sources.Kernel.Types.Module      as Module
import qualified Hydra.Sources.Kernel.Types.Phantoms    as Phantoms
import qualified Hydra.Sources.Kernel.Types.Query       as Query
import qualified Hydra.Sources.Kernel.Types.Relational  as Relational
import qualified Hydra.Sources.Kernel.Types.Tabular     as Tabular
import qualified Hydra.Sources.Kernel.Types.Testing     as Testing
import qualified Hydra.Sources.Kernel.Types.Topology    as Topology
import qualified Hydra.Sources.Kernel.Types.Typing      as Typing
import qualified Hydra.Sources.Kernel.Types.Util        as Util
import qualified Hydra.Sources.Kernel.Types.Workflow    as Workflow
import qualified Data.Int                               as I
import qualified Data.List                              as L
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Maybe                             as Y


graphsonSyntaxModule :: Module
graphsonSyntaxModule = Module ns elements [] [Core.module_] $
    Just ("A syntax model for TinkerPop's GraphSON format."
      ++ " This model is designed to be as inclusive as possible, supporting GraphSON 4.0 as well as earlier versions."
      ++ " See https://github.com/apache/tinkerpop/blob/master/docs/src/dev/io/graphson.asciidoc.")
  where
    ns = Namespace "hydra.pg.graphson.syntax"
    gson = typeref ns
    def = datatype ns

    elements = [

      def "BigDecimalValue" $
        wrap string,

      def "CompositeTypedValue" $
        record [
          "type">: gson "TypeName",
          "fields">: gson "Map"],

      def "DateTime" $
        wrap string,

      def "DoubleValue" $
        union [
          "finite">: float64,
          "infinity">: unit,
          "negativeInfinity">: unit,
          "notANumber">: unit],

      def "Duration" $
        wrap string,

      def "EdgeLabel" $
        wrap string,

      def "FloatValue" $
        union [
          "finite">: float32,
          "infinity">: unit,
          "negativeInfinity">: unit,
          "notANumber">: unit],

      def "Map" $
        wrap $ list $ gson "ValuePair",

      def "AdjacentEdge" $
        record [
          "id">: gson "Value",
          "vertexId">: gson "Value",
          "properties">: Types.map (gson "PropertyKey") (gson "Value")],

      def "PrimitiveTypedValue" $
        record [
          "type">: gson "TypeName",
          "value">: string],

      def "PropertyKey" $
        wrap string,

      def "TypeName" $
        wrap string,

      def "Uuid" $
        wrap string,

      -- Note: the following are currently unsupported as values:
      --   * BulkSet
      --   * Direction
      --   * Edge
      --   * Error Result
      --   * Graph
      --   * Path
      --   * Property
      --   * Standard Request
      --   * Standard Result
      --   * T (enum value)
      --   * Tree
      --   * Vertex
      --   * VertexProperty
      def "Value" $
        union [
          "bigDecimal">: gson "BigDecimalValue",
          "bigInteger">: bigint,
          "binary">: binary,
          "boolean">: boolean,
          "byte">: uint8,
          "char">: uint32,
          "composite">: gson "CompositeTypedValue",
          "dateTime">: gson "DateTime",
          "double">: gson "DoubleValue",
          "duration">: gson "Duration",
          "float">: gson "FloatValue",
          "integer">: int32,
          "list">: list $ gson "Value",
          "long">: int64,
          "map">: gson "Map",
          "null">: unit,
          "primitive">: gson "PrimitiveTypedValue",
          "set">: list $ gson "Value",
          "short">: int16,
          "string">: string,
          "uuid">: gson "Uuid"],

      def "ValuePair" $
        record [
          "first">: gson "Value",
          "second">: gson "Value"],

      def "Vertex" $
        record [
          "id">: gson "Value",
          "label">: optional $ gson "VertexLabel",
          "inEdges">: Types.map (gson "EdgeLabel") (nonemptyList $ gson "AdjacentEdge"),
          "outEdges">: Types.map (gson "EdgeLabel") (nonemptyList $ gson "AdjacentEdge"),
          "properties">: Types.map (gson "PropertyKey") (nonemptyList $ gson "VertexPropertyValue")],

      def "VertexLabel" $
        wrap string,

      def "VertexPropertyValue" $
        record [
          "id">: optional $ gson "Value",
          "value">: gson "Value"]]
