module Hydra.Ext.Shacl.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Rdf.Syntax as Syntax
import Data.Map
import Data.Set

-- A NodeShape or PropertyShape. For the official definition, see https://www.w3.org/TR/shacl/#dfn-shape
data Shape 
  = ShapeNode NodeShape
  | ShapeProperty PropertyShape
  deriving (Eq, Ord, Read, Show)

_Shape = (Core.Name "hydra/ext/shacl/model.Shape")

_Shape_node = (Core.FieldName "node")

_Shape_property = (Core.FieldName "property")

-- A shape which matches nodes in the data graph. For the official definition, see https://www.w3.org/TR/shacl/#node-shapes
data NodeShape 
  = NodeShape {
    nodeShapeProperty :: [PropertyShape],
    nodeShapeXone :: [Shape],
    nodeShapeAnd :: [Shape],
    nodeShapeNode :: [NodeShape]}
  deriving (Eq, Ord, Read, Show)

_NodeShape = (Core.Name "hydra/ext/shacl/model.NodeShape")

_NodeShape_property = (Core.FieldName "property")

_NodeShape_xone = (Core.FieldName "xone")

_NodeShape_and = (Core.FieldName "and")

_NodeShape_node = (Core.FieldName "node")

-- A shape which matches a property or property path from a node. For the official definition, see https://www.w3.org/TR/shacl/#property-shapes
data PropertyShape 
  = PropertyShape {
    propertyShapeName :: (Set String),
    propertyShapeDescription :: (Set String),
    propertyShapePath :: Syntax.Iri,
    propertyShapeDatatype :: (Maybe Syntax.Iri),
    propertyShapeMinCount :: (Maybe Int),
    propertyShapeMaxCount :: (Maybe Int),
    propertyShapeNode :: [NodeShape],
    propertyShapeProperty :: [PropertyShape],
    propertyShapeOrder :: (Maybe Int)}
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

_PropertyShape_order = (Core.FieldName "order")

-- An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes
data ShapesGraph 
  = ShapesGraph {
    shapesGraphShapes :: [Shape]}
  deriving (Eq, Ord, Read, Show)

_ShapesGraph = (Core.Name "hydra/ext/shacl/model.ShapesGraph")

_ShapesGraph_shapes = (Core.FieldName "shapes")