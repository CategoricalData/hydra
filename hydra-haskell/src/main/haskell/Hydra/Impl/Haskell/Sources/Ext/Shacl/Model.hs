module Hydra.Impl.Haskell.Sources.Ext.Shacl.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Ext.Rdf.Syntax



shaclModelModule :: Module Meta
shaclModelModule = Module shaclModel [rdfSyntaxModule]

shaclModelName :: GraphName
shaclModelName = GraphName "hydra/ext/shacl/model"

shaclModel :: Graph Meta
shaclModel = Graph shaclModelName elements (const True) hydraCoreName
  where
    def = datatype shaclModelName
    shacl = nominal . qualify shaclModelName . Name
    rdf = nominal . qualify rdfSyntaxName . Name

    elements = [
      def "Shape" $
        doc "A NodeShape or PropertyShape. For the official definition, see https://www.w3.org/TR/shacl/#dfn-shape" $
        union [
          field "node" $ shacl "NodeShape",
          field "property" $ shacl "PropertyShape"],






      def "NodeShape" $
        doc "A shape which matches nodes in the data graph. For the official definition, see https://www.w3.org/TR/shacl/#node-shapes" $
        record [
          field "property"  $ list $ shacl "PropertyShape",
          field "xone" $ list $ shacl "Shape",
          field "and" $ list $ shacl "Shape",
          field "node" $ list $ shacl "NodeShape"],

      def "PropertyShape" $
        doc "A shape which matches a property or property path from a node. For the official definition, see https://www.w3.org/TR/shacl/#property-shapes" $
        record [
          field "name" $ set string,
          field "description" $ set string,
          field "path"  $ rdf "Iri",
          field "datatype" $ optional $ rdf "Iri",
          field "minCount" $ optional int32,
          field "maxCount" $ optional int32,
          field "node" $ list $ shacl "NodeShape",
          field "property" $ list $ shacl "PropertyShape",
          field "order" $ optional int32],

      def "ShapesGraph" $
        doc "An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes" $
        record [
          field "shapes" $ list $ shacl "Shape"]]
