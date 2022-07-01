module Hydra.Ext.Rdf.Model where

import qualified Hydra.Core as Core
import Data.Map
import Data.Set

newtype BlankNode 
  = BlankNode {
    unBlankNode :: String}
  deriving (Eq, Ord, Read, Show)

_BlankNode = (Core.Name "hydra/ext/rdf/model.BlankNode")

newtype IRI 
  = IRI {
    unIRI :: String}
  deriving (Eq, Ord, Read, Show)

_IRI = (Core.Name "hydra/ext/rdf/model.IRI")

data Literal 
  = Literal {
    literalLexicalForm :: String,
    literalDatatypeIri :: IRI,
    literalLanguage :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Literal = (Core.Name "hydra/ext/rdf/model.Literal")

_Literal_lexicalForm = (Core.FieldName "lexicalForm")

_Literal_datatypeIri = (Core.FieldName "datatypeIri")

_Literal_language = (Core.FieldName "language")

data Node 
  = NodeIri IRI
  | NodeBnode BlankNode
  | NodeLiteral Literal
  deriving (Eq, Ord, Read, Show)

_Node = (Core.Name "hydra/ext/rdf/model.Node")

_Node_iri = (Core.FieldName "iri")

_Node_bnode = (Core.FieldName "bnode")

_Node_literal = (Core.FieldName "literal")

data Quad 
  = Quad {
    quadSubject :: Node,
    quadPredicate :: IRI,
    quadObject :: Node,
    quadGraph :: (Maybe IRI)}
  deriving (Eq, Ord, Read, Show)

_Quad = (Core.Name "hydra/ext/rdf/model.Quad")

_Quad_subject = (Core.FieldName "subject")

_Quad_predicate = (Core.FieldName "predicate")

_Quad_object = (Core.FieldName "object")

_Quad_graph = (Core.FieldName "graph")

newtype Dataset 
  = Dataset {
    unDataset :: [Quad]}
  deriving (Eq, Ord, Read, Show)

_Dataset = (Core.Name "hydra/ext/rdf/model.Dataset")