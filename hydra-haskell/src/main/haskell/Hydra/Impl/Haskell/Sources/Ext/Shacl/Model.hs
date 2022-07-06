module Hydra.Impl.Haskell.Sources.Ext.Shacl.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Ext.Rdf.Model



shaclModelModule :: Module Meta
shaclModelModule = Module shaclModel [rdfModelModule]

shaclModelName :: GraphName
shaclModelName = GraphName "hydra/ext/shacl/model"

shaclModel :: Graph Meta
shaclModel = Graph shaclModelName elements (const True) hydraCoreName
  where
    def = datatype shaclModelName
    shacl = nominal . qualify shaclModelName . Name
    rdf = nominal . qualify rdfModelName . Name


    elements = [
      def "Shape" $
        union [
          field "nodeShape" $ shacl "NodeShape",
          field "propertyShape" $ shacl "PropertyShape"],

      def "NodeShape" $
        record [
          field "name" $ set string,
          field "description" $ set string,
          field "property"  $ list $ shacl "PropertyShape",
          field "xone" $ list $ shacl "Shape",
          field "and" $ list $ shacl "Shape",
          field "node" $ list $ shacl "NodeShape"],

      def "PropertyShape" $
        record [
          field "name" $ set string,
          field "description" $ set string,
          field "path"  $ rdf "IRI",
          field "datatype" $ optional $ rdf "IRI",
          field "minCount" $ optional int32,
          field "maxCount" $ optional int32,
          field "node" $ list $ shacl "NodeShape",
          field "property" $ list $ shacl "PropertyShape"]]
