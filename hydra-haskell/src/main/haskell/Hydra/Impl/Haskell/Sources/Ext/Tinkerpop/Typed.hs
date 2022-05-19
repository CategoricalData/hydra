module Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Typed where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


tinkerpopTypedModule = Module tinkerpopTyped [hydraCoreModule]

tinkerpopTypedName = "hydra/ext/tinkerpop/typed"

tinkerpopTyped :: Graph Meta
tinkerpopTyped = Graph tinkerpopTypedName elements (const True) hydraCoreName
  where
    def = datatype tinkerpopTypedName
    typed = nominal . qualify tinkerpopTypedName
    core = nominal . qualify hydraCoreName

    elements = [

      def "CollectionType" $
        doc "The type of a collection, such as a list of strings or an optional integer value" $
        union [
          field "list" $ typed "Type",
          field "map" $ typed "Type",
          field "optional" $ typed "Type",
          field "set" $ typed "Type"],

      def "CollectionValue" $
        doc "A collection of values, such as a list of strings or an optional integer value" $
        union [
          field "list" $ list $ typed "Value",
          field "map" $ Types.map (typed "Key") (typed "Value"),
          field "optional" $ optional $ typed "Value",
          field "set" $ set $ typed "Value"],

      def "Edge" $
        doc "An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties" $
        record [
          field "id" $ typed "EdgeId",
          field "label" $ typed "Label",
          field "out" $ typed "VertexId",
          field "in" $ typed "VertexId",
          field "properties" $ Types.map (typed "Key") (typed "Value")],

      def "EdgeId" $
        doc "A literal value representing an edge id" $
        core "Literal",

      def "EdgeIdType" $
        doc "The type of a reference to an edge by id" $
        typed "EdgeType",

      def "EdgeType" $
        doc "The type of an edge, with characteristic id, out-vertex, in-vertex, and property types" $
        record [
          field "id" $ core "LiteralType",
          field "out" $ typed "VertexIdType",
          field "in" $ typed "VertexIdType",
          field "properties" $ Types.map (typed "Key") (typed "Type")],

      def "Id" $
        doc "A vertex or edge id" $
        union [
          field "vertex" $ typed "VertexId",
          field "edge" $ typed "EdgeId"],

      def "IdType" $
        doc "The type of a reference to a strongly-typed element (vertex or edge) by id" $
        union [
          field "vertex" $ typed "VertexType",
          field "edge" $ typed "EdgeType"],

      def "Key" $
        doc "A property key or map key"
        string,

      def "Label" $
        doc "A vertex or edge label"
        string,

      def "Type" $
        doc "The type of a value, such as a property value" $
        union [
          field "atomic" $ core "LiteralType",
          field "collection" $ typed "CollectionType",
          field "element" $ typed "IdType"],

      def "Value" $
        doc "A concrete value such as a number or string, a collection of other values, or an element reference" $
        union [
          field "atomic" $ core "Literal",
          field "collection" $ typed "CollectionValue",
          field "element" $ typed "Id"],

      def "Vertex" $
        doc "A vertex, comprised of an id and zero or more properties" $
        record [
          field "id" $ typed "VertexId",
          field "label" $ typed "Label",
          field "properties" $ Types.map (typed "Key") (typed "Value")],

      def "VertexId" $
        doc "A literal value representing a vertex id" $
        core "Literal",

      def "VertexIdType" $
        doc "The type of a reference to a vertex by id" $
        typed "VertexType",

      def "VertexType" $
        doc "The type of a vertex, with characteristic id and property types" $
        record [
          field "id" $ core "LiteralType",
          field "properties" $ Types.map (typed "Key") (typed "Value")]]
