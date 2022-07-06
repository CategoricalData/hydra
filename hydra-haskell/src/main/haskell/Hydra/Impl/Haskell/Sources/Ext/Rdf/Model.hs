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
      
      def "Dataset" $ set $ rdf "Quad",
      
      def "Iri" string,
      
      def "IriOrLiteral" $
        doc "An IRI or a literal; this type is a convenience for other models like SHACL which may exclude blank nodes" $
        union [
          field "iri" $ rdf "Iri",
          field "literal" $ rdf "Literal"],
                  
      def "Literal" $
        record [
          field "lexicalForm" string,
          field "datatypeIri" $ rdf "Iri",
          field "language" $ optional string],

      def "Node" $
        union [
          field "iri" $ rdf "Iri",
          field "bnode" $ rdf "BlankNode",
          field "literal" $ rdf "Literal"],

      def "Quad" $
        doc "An RDF triple with an optional context/graph component" $
        record [
          field "subject" $ rdf "Resource",
          field "predicate" $ rdf "Iri",
          field "object" $ rdf "Node",
          field "graph" $ optional $ rdf "Iri"],

      def "Resource" $
        union [
          field "iri" $ rdf "Iri",
          field "bnode" $ rdf "BlankNode"]]
