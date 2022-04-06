module Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.V3 where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard

tinkerpopV3Name = "hydra/ext/tinkerpop/v3"

tinkerpopV3 :: Graph Meta
tinkerpopV3 = Graph tinkerpopV3Name elements (const True) hydraCoreName
  where
    def = datatype tinkerpopV3Name
    v3 = nominal . qualify tinkerpopV3Name
    core = nominal . qualify hydraCoreName

    elements = [

      def "CollectionType"
        "The type of a collection, such as a list of strings or an optional integer value" $
        union [
          field "list" $ v3 "Type",
          field "map" $ v3 "Type",
          field "optional" $ v3 "Type",
          field "set" $ v3 "Type"],

      def "CollectionValue"
        "A collection of values, such as a list of strings or an optional integer value" $
        union [
          field "list" $ list $ v3 "Value",
          field "map" $ Types.map (v3 "Key") (v3 "Value"),
          field "optional" $ optional $ v3 "Value",
          field "set" $ set $ v3 "Value"],

      def "Edge"
        "An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties" $
        record [
          field "id" $ v3 "EdgeId",
          field "label" $ v3 "Label",
          field "out" $ v3 "VertexId",
          field "in" $ v3 "VertexId",
          field "properties" $ Types.map (v3 "Key") (v3 "Value")],

      def "EdgeId"
        "A literal value representing an edge id" $
        core "Literal",

      def "EdgeIdType"
        "The type of a reference to an edge by id" $
        v3 "EdgeType",

      def "EdgeType"
        "The type of an edge, with characteristic id, out-vertex, in-vertex, and property types" $
        record [
          field "id" $ core "LiteralType",
          field "out" $ v3 "VertexIdType",
          field "in" $ v3 "VertexIdType",
          field "properties" $ Types.map (v3 "Key") (v3 "Type")],

      def "Id"
        "A vertex or edge id" $
        union [
          field "vertex" $ v3 "VertexId",
          field "edge" $ v3 "EdgeId"],

      def "IdType"
        "The type of a reference to a strongly-typed element (vertex or edge) by id" $
        union [
          field "vertex" $ v3 "VertexType",
          field "edge" $ v3 "EdgeType"],

      def "Key"
        "A property key or map key"
        string,

      def "Label"
        "A vertex or edge label"
        string,

      def "Type"
        "The type of a value, such as a property value" $
        union [
          field "atomic" $ core "LiteralType",
          field "collection" $ v3 "CollectionType",
          field "element" $ v3 "IdType"],

      def "Value"
        "A concrete value such as a number or string, a collection of other values, or an element reference" $
        union [
          field "atomic" $ core "Literal",
          field "collection" $ v3 "CollectionValue",
          field "element" $ v3 "Id"],

      def "Vertex"
        "A vertex, comprised of an id and zero or more properties" $
        record [
          field "id" $ v3 "VertexId",
          field "label" $ v3 "Label",
          field "properties" $ Types.map (v3 "Key") (v3 "Value")],

      def "VertexId"
        "A literal value representing a vertex id" $
        core "Literal",

      def "VertexIdType"
        "The type of a reference to a vertex by id" $
        v3 "VertexType",

      def "VertexType"
        "The type of a vertex, with characteristic id and property types" $
        record [
          field "id" $ core "LiteralType",
          field "properties" $ Types.map (v3 "Key") (v3 "Value")]]
