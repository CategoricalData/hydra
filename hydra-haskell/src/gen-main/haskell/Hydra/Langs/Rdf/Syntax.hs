-- | An RDF 1.1 syntax model

module Hydra.Langs.Rdf.Syntax where

import qualified Hydra.Core as Core
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

newtype BlankNode = 
  BlankNode {
    unBlankNode :: String}
  deriving (Eq, Ord, Read, Show)

_BlankNode = (Core.Name "hydra/langs/rdf/syntax.BlankNode")

_BlankNode_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | Stand-in for rdfs:Class
data RdfsClass = 
  RdfsClass {}
  deriving (Eq, Ord, Read, Show)

_RdfsClass = (Core.Name "hydra/langs/rdf/syntax.RdfsClass")

_RdfsClass_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

newtype Dataset = 
  Dataset {
    unDataset :: (Set Quad)}
  deriving (Eq, Ord, Read, Show)

_Dataset = (Core.Name "hydra/langs/rdf/syntax.Dataset")

_Dataset_type_ = (Core.TypeSet _Quad_type_)

-- | A graph of RDF statements together with a distinguished subject and/or object node
data Description = 
  Description {
    descriptionSubject :: Node,
    descriptionGraph :: Graph}
  deriving (Eq, Ord, Read, Show)

_Description = (Core.Name "hydra/langs/rdf/syntax.Description")

_Description_subject = (Core.Name "subject")

_Description_graph = (Core.Name "graph")

_Description_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Description"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subject"),
      Core.fieldTypeType = _Node_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "graph"),
      Core.fieldTypeType = _Graph_type_}]}))

newtype Graph = 
  Graph {
    unGraph :: (Set Triple)}
  deriving (Eq, Ord, Read, Show)

_Graph = (Core.Name "hydra/langs/rdf/syntax.Graph")

_Graph_type_ = (Core.TypeSet _Triple_type_)

-- | An Internationalized Resource Identifier
newtype Iri = 
  Iri {
    unIri :: String}
  deriving (Eq, Ord, Read, Show)

_Iri = (Core.Name "hydra/langs/rdf/syntax.Iri")

_Iri_type_ = (Core.TypeLiteral Core.LiteralTypeString)

-- | An IRI or a literal; this type is a convenience for downstream models like SHACL which may exclude blank nodes
data IriOrLiteral = 
  IriOrLiteralIri Iri |
  IriOrLiteralLiteral Literal
  deriving (Eq, Ord, Read, Show)

_IriOrLiteral = (Core.Name "hydra/langs/rdf/syntax.IriOrLiteral")

_IriOrLiteral_iri = (Core.Name "iri")

_IriOrLiteral_literal = (Core.Name "literal")

_IriOrLiteral_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.IriOrLiteral"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = _Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_}]}))

-- | A convenience type which provides at most one string value per language, and optionally a value without a language
newtype LangStrings = 
  LangStrings {
    unLangStrings :: (Map (Maybe LanguageTag) String)}
  deriving (Eq, Ord, Read, Show)

_LangStrings = (Core.Name "hydra/langs/rdf/syntax.LangStrings")

_LangStrings_type_ = (Core.TypeMap (Core.MapType {
  Core.mapTypeKeys = (Core.TypeOptional _LanguageTag_type_),
  Core.mapTypeValues = (Core.TypeLiteral Core.LiteralTypeString)}))

-- | A BCP47 language tag
newtype LanguageTag = 
  LanguageTag {
    unLanguageTag :: String}
  deriving (Eq, Ord, Read, Show)

_LanguageTag = (Core.Name "hydra/langs/rdf/syntax.LanguageTag")

_LanguageTag_type_ = (Core.TypeLiteral Core.LiteralTypeString)

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

_Literal = (Core.Name "hydra/langs/rdf/syntax.Literal")

_Literal_lexicalForm = (Core.Name "lexicalForm")

_Literal_datatypeIri = (Core.Name "datatypeIri")

_Literal_languageTag = (Core.Name "languageTag")

_Literal_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Literal"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lexicalForm"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatypeIri"),
      Core.fieldTypeType = _Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "languageTag"),
      Core.fieldTypeType = (Core.TypeOptional _LanguageTag_type_)}]}))

data Node = 
  NodeIri Iri |
  NodeBnode BlankNode |
  NodeLiteral Literal
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra/langs/rdf/syntax.Node")

_Node_iri = (Core.Name "iri")

_Node_bnode = (Core.Name "bnode")

_Node_literal = (Core.Name "literal")

_Node_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Node"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = _Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bnode"),
      Core.fieldTypeType = _BlankNode_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = _Literal_type_}]}))

-- | A type representing an RDF property, and encapsulating its domain, range, and subclass relationships
data Property = 
  Property {
    -- | State that any resource that has a given property is an instance of one or more classes
    propertyDomain :: (Set RdfsClass),
    -- | States that the values of a property are instances of one or more classes
    propertyRange :: (Set RdfsClass),
    propertySubPropertyOf :: (Set Property)}
  deriving (Eq, Ord, Read, Show)

_Property = (Core.Name "hydra/langs/rdf/syntax.Property")

_Property_domain = (Core.Name "domain")

_Property_range = (Core.Name "range")

_Property_subPropertyOf = (Core.Name "subPropertyOf")

_Property_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Property"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "domain"),
      Core.fieldTypeType = (Core.TypeSet _RdfsClass_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = (Core.TypeSet _RdfsClass_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subPropertyOf"),
      Core.fieldTypeType = (Core.TypeSet _Property_type_)}]}))

-- | An RDF triple with an optional named graph component
data Quad = 
  Quad {
    quadSubject :: Resource,
    quadPredicate :: Iri,
    quadObject :: Node,
    quadGraph :: (Maybe Iri)}
  deriving (Eq, Ord, Read, Show)

_Quad = (Core.Name "hydra/langs/rdf/syntax.Quad")

_Quad_subject = (Core.Name "subject")

_Quad_predicate = (Core.Name "predicate")

_Quad_object = (Core.Name "object")

_Quad_graph = (Core.Name "graph")

_Quad_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Quad"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subject"),
      Core.fieldTypeType = _Resource_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _Node_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "graph"),
      Core.fieldTypeType = (Core.TypeOptional _Iri_type_)}]}))

data Resource = 
  ResourceIri Iri |
  ResourceBnode BlankNode
  deriving (Eq, Ord, Read, Show)

_Resource = (Core.Name "hydra/langs/rdf/syntax.Resource")

_Resource_iri = (Core.Name "iri")

_Resource_bnode = (Core.Name "bnode")

_Resource_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Resource"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = _Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bnode"),
      Core.fieldTypeType = _BlankNode_type_}]}))

-- | An RDF triple defined by a subject, predicate, and object
data Triple = 
  Triple {
    tripleSubject :: Resource,
    triplePredicate :: Iri,
    tripleObject :: Node}
  deriving (Eq, Ord, Read, Show)

_Triple = (Core.Name "hydra/langs/rdf/syntax.Triple")

_Triple_subject = (Core.Name "subject")

_Triple_predicate = (Core.Name "predicate")

_Triple_object = (Core.Name "object")

_Triple_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/rdf/syntax.Triple"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subject"),
      Core.fieldTypeType = _Resource_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "predicate"),
      Core.fieldTypeType = _Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _Node_type_}]}))