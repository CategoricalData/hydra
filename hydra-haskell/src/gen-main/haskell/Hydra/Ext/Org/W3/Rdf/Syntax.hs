-- | An RDF 1.1 syntax model

module Hydra.Ext.Org.W3.Rdf.Syntax where

import qualified Hydra.Core as Core
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

newtype BlankNode = 
  BlankNode {
    unBlankNode :: String}
  deriving (Eq, Ord, Read, Show)

_BlankNode = (Core.Name "hydra.ext.org.w3.rdf.syntax.BlankNode")

-- | Stand-in for rdfs:Class
data RdfsClass = 
  RdfsClass {}
  deriving (Eq, Ord, Read, Show)

_RdfsClass = (Core.Name "hydra.ext.org.w3.rdf.syntax.RdfsClass")

newtype Dataset = 
  Dataset {
    unDataset :: (S.Set Quad)}
  deriving (Eq, Ord, Read, Show)

_Dataset = (Core.Name "hydra.ext.org.w3.rdf.syntax.Dataset")

-- | A graph of RDF statements together with a distinguished subject and/or object node
data Description = 
  Description {
    descriptionSubject :: Node,
    descriptionGraph :: Graph}
  deriving (Eq, Ord, Read, Show)

_Description = (Core.Name "hydra.ext.org.w3.rdf.syntax.Description")

_Description_subject = (Core.Name "subject")

_Description_graph = (Core.Name "graph")

newtype Graph = 
  Graph {
    unGraph :: (S.Set Triple)}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra.ext.org.w3.rdf.syntax.Graph")

-- | An Internationalized Resource Identifier
newtype Iri = 
  Iri {
    unIri :: String}
  deriving (Eq, Ord, Read, Show)

_Iri = (Core.Name "hydra.ext.org.w3.rdf.syntax.Iri")

-- | An IRI or a literal; this type is a convenience for downstream models like SHACL which may exclude blank nodes
data IriOrLiteral = 
  IriOrLiteralIri Iri |
  IriOrLiteralLiteral Literal
  deriving (Eq, Ord, Read, Show)

_IriOrLiteral = (Core.Name "hydra.ext.org.w3.rdf.syntax.IriOrLiteral")

_IriOrLiteral_iri = (Core.Name "iri")

_IriOrLiteral_literal = (Core.Name "literal")

-- | A convenience type which provides at most one string value per language, and optionally a value without a language
newtype LangStrings = 
  LangStrings {
    unLangStrings :: (M.Map (Maybe LanguageTag) String)}
  deriving (Eq, Ord, Read, Show)

_LangStrings = (Core.Name "hydra.ext.org.w3.rdf.syntax.LangStrings")

-- | A BCP47 language tag
newtype LanguageTag = 
  LanguageTag {
    unLanguageTag :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageTag = (Core.Name "hydra.ext.org.w3.rdf.syntax.LanguageTag")

-- | A value such as a string, number, or date
data Literal = 
  Literal {
    -- | a Unicode string, which should be in Normal Form C
    literalLexicalForm :: String,
    -- | an IRI identifying a datatype that determines how the lexical form maps to a literal value
    literalDatatypeIri :: Iri,
    -- | An optional language tag, present if and only if the datatype IRI is http://www.w3.org/1999/02/22-rdf-syntax-ns#langString
    literalLanguageTag :: (Maybe LanguageTag)}
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra.ext.org.w3.rdf.syntax.Literal")

_Literal_lexicalForm = (Core.Name "lexicalForm")

_Literal_datatypeIri = (Core.Name "datatypeIri")

_Literal_languageTag = (Core.Name "languageTag")

data Node = 
  NodeIri Iri |
  NodeBnode BlankNode |
  NodeLiteral Literal
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra.ext.org.w3.rdf.syntax.Node")

_Node_iri = (Core.Name "iri")

_Node_bnode = (Core.Name "bnode")

_Node_literal = (Core.Name "literal")

-- | A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
data Property = 
  Property {
    -- | State that any resource that has a given property is an instance of one or more classes
    propertyDomain :: (S.Set RdfsClass),
    -- | States that the values of a property are instances of one or more classes
    propertyRange :: (S.Set RdfsClass),
    propertySubPropertyOf :: (S.Set Property)}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra.ext.org.w3.rdf.syntax.Property")

_Property_domain = (Core.Name "domain")

_Property_range = (Core.Name "range")

_Property_subPropertyOf = (Core.Name "subPropertyOf")

-- | An RDF triple with an optional named graph component
data Quad = 
  Quad {
    quadSubject :: Resource,
    quadPredicate :: Iri,
    quadObject :: Node,
    quadGraph :: (Maybe Iri)}
  deriving (Eq, Ord, Read, Show)

_Quad = (Core.Name "hydra.ext.org.w3.rdf.syntax.Quad")

_Quad_subject = (Core.Name "subject")

_Quad_predicate = (Core.Name "predicate")

_Quad_object = (Core.Name "object")

_Quad_graph = (Core.Name "graph")

data Resource = 
  ResourceIri Iri |
  ResourceBnode BlankNode
  deriving (Eq, Ord, Read, Show)

_Resource = (Core.Name "hydra.ext.org.w3.rdf.syntax.Resource")

_Resource_iri = (Core.Name "iri")

_Resource_bnode = (Core.Name "bnode")

-- | An RDF triple defined by a subject, predicate, and object
data Triple = 
  Triple {
    tripleSubject :: Resource,
    triplePredicate :: Iri,
    tripleObject :: Node}
  deriving (Eq, Ord, Read, Show)

_Triple = (Core.Name "hydra.ext.org.w3.rdf.syntax.Triple")

_Triple_subject = (Core.Name "subject")

_Triple_predicate = (Core.Name "predicate")

_Triple_object = (Core.Name "object")