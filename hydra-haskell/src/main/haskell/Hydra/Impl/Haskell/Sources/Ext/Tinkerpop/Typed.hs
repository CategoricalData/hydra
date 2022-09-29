module Hydra.Impl.Haskell.Sources.Ext.Tinkerpop.Typed where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Compute
import Hydra.Module
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


tinkerpopTypedModule :: Module Meta
tinkerpopTypedModule = Module ns elements [hydraCoreModule] Nothing
  where
    ns = Namespace "hydra/ext/tinkerpop/typed"
    def = datatype ns
    typed = nsref ns
    core = nsref $ moduleNamespace hydraCoreModule

    elements = [

      def "CollectionType" $
        doc "The type of a collection, such as a list of strings or an optional integer value" $
        union [
          "list">: typed "Type",
          "map">: typed "Type",
          "optional">: typed "Type",
          "set">: typed "Type"],

      def "CollectionValue" $
        doc "A collection of values, such as a list of strings or an optional integer value" $
        union [
          "list">: list $ typed "Value",
          "map">: Types.map (typed "Key") (typed "Value"),
          "optional">: optional $ typed "Value",
          "set">: set $ typed "Value"],

      def "Edge" $
        doc "An edge, comprised of an id, an out-vertex and in-vertex id, and zero or more properties" $
        record [
          "id">: typed "EdgeId",
          "label">: typed "Label",
          "out">: typed "VertexId",
          "in">: typed "VertexId",
          "properties">: Types.map (typed "Key") (typed "Value")],

      def "EdgeId" $
        doc "A literal value representing an edge id" $
        core "Literal",

      def "EdgeIdType" $
        doc "The type of a reference to an edge by id" $
        typed "EdgeType",

      def "EdgeType" $
        doc "The type of an edge, with characteristic id, out-vertex, in-vertex, and property types" $
        record [
          "id">: core "LiteralType",
          "out">: typed "VertexIdType",
          "in">: typed "VertexIdType",
          "properties">: Types.map (typed "Key") (typed "Type")],

      def "Id" $
        doc "A vertex or edge id" $
        union [
          "vertex">: typed "VertexId",
          "edge">: typed "EdgeId"],

      def "IdType" $
        doc "The type of a reference to a strongly-typed element (vertex or edge) by id" $
        union [
          "vertex">: typed "VertexType",
          "edge">: typed "EdgeType"],

      def "Key" $
        doc "A property key or map key"
        string,

      def "Label" $
        doc "A vertex or edge label"
        string,

      def "Type" $
        doc "The type of a value, such as a property value" $
        union [
          "literal">: core "LiteralType",
          "collection">: typed "CollectionType",
          "element">: typed "IdType"],

      def "Value" $
        doc "A concrete value such as a number or string, a collection of other values, or an element reference" $
        union [
          "literal">: core "Literal",
          "collection">: typed "CollectionValue",
          "element">: typed "Id"],

      def "Vertex" $
        doc "A vertex, comprised of an id and zero or more properties" $
        record [
          "id">: typed "VertexId",
          "label">: typed "Label",
          "properties">: Types.map (typed "Key") (typed "Value")],

      def "VertexId" $
        doc "A literal value representing a vertex id" $
        core "Literal",

      def "VertexIdType" $
        doc "The type of a reference to a vertex by id" $
        typed "VertexType",

      def "VertexType" $
        doc "The type of a vertex, with characteristic id and property types" $
        record [
          "id">: core "LiteralType",
          "properties">: Types.map (typed "Key") (typed "Value")]]
