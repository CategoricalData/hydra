module Hydra.Ext.Rdf.Syntax where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

newtype BlankNode 
  = BlankNode {
    unBlankNode :: String}
  deriving (Eq, Ord, Read, Show)

_BlankNode = (Core.Name "hydra/ext/rdf/syntax.BlankNode")

-- Stand-in for rdfs:Class
data RdfsClass 
  = RdfsClass {}
  deriving (Eq, Ord, Read, Show)

_RdfsClass = (Core.Name "hydra/ext/rdf/syntax.RdfsClass")

newtype Dataset 
  = Dataset {
    unDataset :: (Set Quad)}
  deriving (Eq, Ord, Read, Show)

_Dataset = (Core.Name "hydra/ext/rdf/syntax.Dataset")

newtype Iri 
  = Iri {
    unIri :: String}
  deriving (Eq, Ord, Read, Show)

_Iri = (Core.Name "hydra/ext/rdf/syntax.Iri")

-- An IRI or a literal; this type is a convenience for downstream models like SHACL which may exclude blank nodes
data IriOrLiteral 
  = IriOrLiteralIri Iri
  | IriOrLiteralLiteral Literal
  deriving (Eq, Ord, Read, Show)

_IriOrLiteral = (Core.Name "hydra/ext/rdf/syntax.IriOrLiteral")

_IriOrLiteral_iri = (Core.FieldName "iri")

_IriOrLiteral_literal = (Core.FieldName "literal")

-- A convenience type which provides at most one string value per language, and optionally a value without a language
newtype LangStrings 
  = LangStrings {
    unLangStrings :: (Map (Maybe LanguageTag) String)}
  deriving (Eq, Ord, Read, Show)

_LangStrings = (Core.Name "hydra/ext/rdf/syntax.LangStrings")

-- A BCP47 language tag
newtype LanguageTag 
  = LanguageTag {
    unLanguageTag :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageTag = (Core.Name "hydra/ext/rdf/syntax.LanguageTag")

-- A value such as a string, number, or date
data Literal 
  = Literal {
    literalLexicalForm :: String,
    literalDatatypeIri :: Iri,
    literalLanguageTag :: (Maybe LanguageTag)}
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/ext/rdf/syntax.Literal")

_Literal_lexicalForm = (Core.FieldName "lexicalForm")

_Literal_datatypeIri = (Core.FieldName "datatypeIri")

_Literal_languageTag = (Core.FieldName "languageTag")

data Node 
  = NodeIri Iri
  | NodeBnode BlankNode
  | NodeLiteral Literal
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra/ext/rdf/syntax.Node")

_Node_iri = (Core.FieldName "iri")

_Node_bnode = (Core.FieldName "bnode")

_Node_literal = (Core.FieldName "literal")

-- A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
data Property 
  = Property {
    propertyDomain :: (Set RdfsClass),
    propertyRange :: (Set RdfsClass),
    propertySubPropertyOf :: (Set Property)}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/ext/rdf/syntax.Property")

_Property_domain = (Core.FieldName "domain")

_Property_range = (Core.FieldName "range")

_Property_subPropertyOf = (Core.FieldName "subPropertyOf")

-- An RDF triple with an optional context/graph component
data Quad 
  = Quad {
    quadSubject :: Resource,
    quadPredicate :: Iri,
    quadObject :: Node,
    quadGraph :: (Maybe Iri)}
  deriving (Eq, Ord, Read, Show)

_Quad = (Core.Name "hydra/ext/rdf/syntax.Quad")

_Quad_subject = (Core.FieldName "subject")

_Quad_predicate = (Core.FieldName "predicate")

_Quad_object = (Core.FieldName "object")

_Quad_graph = (Core.FieldName "graph")

data Resource 
  = ResourceIri Iri
  | ResourceBnode BlankNode
  deriving (Eq, Ord, Read, Show)

_Resource = (Core.Name "hydra/ext/rdf/syntax.Resource")

_Resource_iri = (Core.FieldName "iri")

_Resource_bnode = (Core.FieldName "bnode")