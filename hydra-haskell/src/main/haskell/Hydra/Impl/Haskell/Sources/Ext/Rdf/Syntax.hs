module Hydra.Impl.Haskell.Sources.Ext.Rdf.Syntax where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


rdfSyntaxModule :: Module Meta
rdfSyntaxModule = Module rdfSyntax []

rdfSyntaxName :: GraphName
rdfSyntaxName = GraphName "hydra/ext/rdf/syntax"

rdfSyntax :: Graph Meta
rdfSyntax = Graph rdfSyntaxName elements (const True) hydraCoreName
  where
    def = datatype rdfSyntaxName
    rdf = nominal . qualify rdfSyntaxName . Name

    elements = [

      def "BlankNode" string,

      def "Dataset" $ set $ rdf "Quad",

      def "Iri" string,

      def "IriOrLiteral" $
        doc ("An IRI or a literal; " ++
             "this type is a convenience for downstream models like SHACL which may exclude blank nodes") $
        union [
          field "iri" $ rdf "Iri",
          field "literal" $ rdf "Literal"],

      def "LanguageTag" $
        doc "A BCP47 language tag"
        string,

      def "Literal" $
        doc "A value such as a string, number, or date" $
        record [
          field "lexicalForm" $
            doc "a Unicode string, which should be in Normal Form C"
            string,
          field "datatypeIri" $
            doc "an IRI identifying a datatype that determines how the lexical form maps to a literal value" $
            rdf "Iri",
          field "languageTag" $
            doc "An optional language tag, present if and only if the datatype IRI is http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" $
            optional $ rdf "LanguageTag"],

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
          field "bnode" $ rdf "BlankNode"],

      def "StringOrLangString" $
        doc ("A string (Literal with datatype IRI xsd:string) or a language-tagged string (rdf:langString). " ++
             " This type is a convenience for downstream models like SHACL") $
        record [
          field "lexicalForm" string,
          field "languageTag" $ optional $ rdf "LanguageTag"]]
