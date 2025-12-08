module Hydra.Ext.Sources.Rdf.Syntax where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core


ns :: Namespace
ns = Namespace "hydra.ext.org.w3.rdf.syntax"

define :: String -> Type -> Binding
define = defineType ns

rdf :: String -> Type
rdf = typeref ns

module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just "An RDF 1.1 syntax model"
  where
    elements = [
      blankNode,
      rdfsClass,
      dataset,
      description,
      graph_,
      iri,
      iriOrLiteral,
      langStrings,
      languageTag,
      literal_,
      node_,
      property_,
      quad,
      resource_,
      triple]

blankNode :: Binding
blankNode = define "BlankNode" $ T.wrap T.string

rdfsClass :: Binding
rdfsClass = define "RdfsClass" $
  doc "Stand-in for rdfs:Class" $ T.wrap T.unit

dataset :: Binding
dataset = define "Dataset" $ T.wrap $ T.set $ rdf "Quad"

description :: Binding
description = define "Description" $
  doc "A graph of RDF statements together with a distinguished subject and/or object node" $
  T.record [
    "subject">: rdf "Node",
    "graph">: rdf "Graph"]

graph_ :: Binding
graph_ = define "Graph" $ T.wrap $ T.set $ rdf "Triple"

iri :: Binding
iri = define "Iri" $
  doc "An Internationalized Resource Identifier" $
  T.wrap T.string

iriOrLiteral :: Binding
iriOrLiteral = define "IriOrLiteral" $
  doc ("An IRI or a literal; " ++
       "this type is a convenience for downstream models like SHACL which may exclude blank nodes") $
  T.union [
    "iri">: rdf "Iri",
    "literal">: rdf "Literal"]

langStrings :: Binding
langStrings = define "LangStrings" $
  doc "A convenience type which provides at most one string value per language, and optionally a value without a language" $
  T.wrap $ T.map (T.maybe $ rdf "LanguageTag") T.string

languageTag :: Binding
languageTag = define "LanguageTag" $
  doc "A BCP47 language tag" $
  T.wrap T.string

literal_ :: Binding
literal_ = define "Literal" $
  doc "A value such as a string, number, or date" $
  T.record [
    "lexicalForm">:
      doc "a Unicode string, which should be in Normal Form C"
      T.string,
    "datatypeIri">:
      doc "an IRI identifying a datatype that determines how the lexical form maps to a literal value" $
      rdf "Iri",
    "languageTag">:
      doc "An optional language tag, present if and only if the datatype IRI is http://www.w3.org/1999/02/22-rdf-syntax-ns#langString" $
      T.maybe $ rdf "LanguageTag"]

node_ :: Binding
node_ = define "Node" $
  T.union [
    "iri">: rdf "Iri",
    "bnode">: rdf "BlankNode",
    "literal">: rdf "Literal"]

property_ :: Binding
property_ = define "Property" $
  doc "A type representing an RDF property, and encapsulating its domain, range, and subclass relationships" $
  T.record [
    "domain">:
      doc "State that any resource that has a given property is an instance of one or more classes" $
      T.set $ rdf "RdfsClass",
    "range">:
      doc "States that the values of a property are instances of one or more classes" $
      T.set $ rdf "RdfsClass",
    "subPropertyOf">:
      T.set $ rdf "Property"]

quad :: Binding
quad = define "Quad" $
  doc "An RDF triple with an optional named graph component" $
  T.record [
    "subject">: rdf "Resource",
    "predicate">: rdf "Iri",
    "object">: rdf "Node",
    "graph">: T.maybe $ rdf "Iri"]

resource_ :: Binding
resource_ = define "Resource" $
  T.union [
    "iri">: rdf "Iri",
    "bnode">: rdf "BlankNode"]

triple :: Binding
triple = define "Triple" $
  doc "An RDF triple defined by a subject, predicate, and object" $
  T.record [
    "subject">: rdf "Resource",
    "predicate">: rdf "Iri",
    "object">: rdf "Node"]
