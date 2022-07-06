module Hydra.Ext.Shacl.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Rdf.Model as Model
import Data.Map
import Data.Set

data Shape 
  = ShapeNodeShape NodeShape
  | ShapePropertyShape PropertyShape
  deriving (Eq, Ord, Read, Show)

_Shape = (Core.Name "hydra/ext/shacl/model.Shape")

_Shape_nodeShape = (Core.FieldName "nodeShape")

_Shape_propertyShape = (Core.FieldName "propertyShape")

data NodeShape 
  = NodeShape {
    nodeShapeIri :: Model.Resource,
    nodeShapeName :: (Set String),
    nodeShapeDescription :: (Set String),
    nodeShapeProperty :: [PropertyShape],
    nodeShapeXone :: [Shape],
    nodeShapeAnd :: [Shape],
    nodeShapeNode :: (Maybe NodeShape)}
  deriving (Eq, Ord, Read, Show)

_NodeShape = (Core.Name "hydra/ext/shacl/model.NodeShape")

_NodeShape_iri = (Core.FieldName "iri")

_NodeShape_name = (Core.FieldName "name")

_NodeShape_description = (Core.FieldName "description")

_NodeShape_property = (Core.FieldName "property")

_NodeShape_xone = (Core.FieldName "xone")

_NodeShape_and = (Core.FieldName "and")

_NodeShape_node = (Core.FieldName "node")

data PropertyShape 
  = PropertyShape {
    propertyShapeName :: (Set String),
    propertyShapeDescription :: (Set String),
    propertyShapePath :: Model.IRI,
    propertyShapeDatatype :: (Maybe Model.IRI),
    propertyShapeMinCount :: (Maybe Int),
    propertyShapeMaxCount :: (Maybe Int),
    propertyShapeNode :: (Maybe NodeShape),
    propertyShapeProperty :: (Maybe PropertyShape)}
  deriving (Eq, Ord, Read, Show)

_PropertyShape = (Core.Name "hydra/ext/shacl/model.PropertyShape")

_PropertyShape_name = (Core.FieldName "name")

_PropertyShape_description = (Core.FieldName "description")

_PropertyShape_path = (Core.FieldName "path")

_PropertyShape_datatype = (Core.FieldName "datatype")

_PropertyShape_minCount = (Core.FieldName "minCount")

_PropertyShape_maxCount = (Core.FieldName "maxCount")

_PropertyShape_node = (Core.FieldName "node")

_PropertyShape_property = (Core.FieldName "property")