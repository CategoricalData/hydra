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
    rdf = nsref rdfSyntaxName

    elements = [

      def "BlankNode" string,

      def "RdfsClass"
        $ doc "Stand-in for rdfs:Class" unit,
        
      def "Dataset" $ set $ rdf "Quad",

      def "Graph" $ set $ rdf "Triple",
      
      def "Iri" string,

      def "IriOrLiteral" $
        doc ("An IRI or a literal; " ++
             "this type is a convenience for downstream models like SHACL which may exclude blank nodes") $
        union [
          field "iri" $ rdf "Iri",
          field "literal" $ rdf "Literal"],

      def "LangStrings" $
        doc "A convenience type which provides at most one string value per language, and optionally a value without a language" $
        Types.map (optional $ rdf "LanguageTag") string,

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

      def "Property" $
        doc "A type representing an RDF property, and encapsulating its domain, range, and subclass relationships" $
        record [
          field "domain" $
            doc "State that any resource that has a given property is an instance of one or more classes" $
            set $ rdf "RdfsClass",
          field "range" $
            doc "States that the values of a property are instances of one or more classes" $
            set $ rdf "RdfsClass",
          field "subPropertyOf" $
            set $ rdf "Property"],
            
      def "Quad" $
        doc "An RDF triple with an optional named graph component" $
        record [
          field "subject" $ rdf "Resource",
          field "predicate" $ rdf "Iri",
          field "object" $ rdf "Node",
          field "graph" $ optional $ rdf "Iri"],

      def "Resource" $
        union [
          field "iri" $ rdf "Iri",
          field "bnode" $ rdf "BlankNode"],

      def "Triple" $
        doc "An RDF triple defined by a subject, predicate, and object" $
        record [
          field "subject" $ rdf "Resource",
          field "predicate" $ rdf "Iri",
          field "object" $ rdf "Node"]]
