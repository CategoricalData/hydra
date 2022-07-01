module Hydra.Impl.Haskell.Sources.Ext.Rdf.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


rdfModelModule :: Module Meta
rdfModelModule = Module rdfModel []

rdfModelName :: GraphName
rdfModelName = GraphName "hydra/ext/rdf/model"

rdfModel :: Graph Meta
rdfModel = Graph rdfModelName elements (const True) hydraCoreName
  where
    def = datatype rdfModelName
    rdf = nominal . qualify rdfModelName . Name

    elements = [

      def "BlankNode" string,
      def "IRI" string,
      def "Literal" $
        record [
          field "lexicalForm" string,
          field "datatypeIri" $ rdf "IRI",
          field "language" $ optional string],

      def "Node" $
        union [
          field "iri" $ rdf "IRI",
          field "bnode" $ rdf "BlankNode",
          field "literal" $ rdf "Literal"],

      def "Quad" $
        record [
          field "subject" $ rdf "Node",
          field "predicate" $ rdf "IRI",
          field "object" $ rdf "Node",
          field "graph" $ optional $ rdf "IRI"],

      def "Dataset" $ list $ rdf "Quad"]
