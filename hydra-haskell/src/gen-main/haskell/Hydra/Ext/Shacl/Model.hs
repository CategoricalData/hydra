module Hydra.Ext.Shacl.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Rdf.Syntax as Syntax
import Data.Map
import Data.Set

-- Any of a number of constraint parameters which can be applied either to node or property shapes
data CommonConstraints 
  = CommonConstraintsAnd [Shape]
  | CommonConstraintsClosed Bool
  | CommonConstraintsDatatype (Set Syntax.Iri)
  | CommonConstraintsHasValue (Set Syntax.Node)
  | CommonConstraintsIgnoredProperties [Syntax.Property]
  | CommonConstraintsIn [Syntax.Node]
  | CommonConstraintsNode (Set NodeShape)
  | CommonConstraintsNot (Set Shape)
  | CommonConstraintsProperty (Set PropertyShape)
  | CommonConstraintsOr [Shape]
  | CommonConstraintsXone [Shape]
  deriving (Eq, Ord, Read, Show)

_CommonConstraints = (Core.Name "hydra/ext/shacl/model.CommonConstraints")

_CommonConstraints_and = (Core.FieldName "and")

_CommonConstraints_closed = (Core.FieldName "closed")

_CommonConstraints_datatype = (Core.FieldName "datatype")

_CommonConstraints_hasValue = (Core.FieldName "hasValue")

_CommonConstraints_ignoredProperties = (Core.FieldName "ignoredProperties")

_CommonConstraints_in = (Core.FieldName "in")

_CommonConstraints_node = (Core.FieldName "node")

_CommonConstraints_not = (Core.FieldName "not")

_CommonConstraints_property = (Core.FieldName "property")

_CommonConstraints_or = (Core.FieldName "or")

_CommonConstraints_xone = (Core.FieldName "xone")

-- Common constraint parameters and other properties for SHACL shapes
data CommonProperties 
  = CommonProperties {
    commonPropertiesConstraints :: CommonConstraints,
    commonPropertiesDeactivated :: (Maybe Bool),
    commonPropertiesMessage :: Syntax.LangStrings,
    commonPropertiesSeverity :: (Maybe Severity),
    commonPropertiesTargetClass :: (Set Syntax.RdfsClass),
    commonPropertiesTargetNode :: (Maybe Syntax.IriOrLiteral),
    commonPropertiesTargetObjectsOf :: (Set Syntax.Property),
    commonPropertiesTargetSubjectsOf :: (Set Syntax.Property)}
  deriving (Eq, Ord, Read, Show)

_CommonProperties = (Core.Name "hydra/ext/shacl/model.CommonProperties")

_CommonProperties_constraints = (Core.FieldName "constraints")

_CommonProperties_deactivated = (Core.FieldName "deactivated")

_CommonProperties_message = (Core.FieldName "message")

_CommonProperties_severity = (Core.FieldName "severity")

_CommonProperties_targetClass = (Core.FieldName "targetClass")

_CommonProperties_targetNode = (Core.FieldName "targetNode")

_CommonProperties_targetObjectsOf = (Core.FieldName "targetObjectsOf")

_CommonProperties_targetSubjectsOf = (Core.FieldName "targetSubjectsOf")

-- A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes
data NodeShape 
  = NodeShape {
    nodeShapeCommon :: CommonProperties}
  deriving (Eq, Ord, Read, Show)

_NodeShape = (Core.Name "hydra/ext/shacl/model.NodeShape")

_NodeShape_common = (Core.FieldName "common")

-- A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes
data PropertyShape 
  = PropertyShape {
    propertyShapeCommon :: CommonProperties,
    propertyShapeConstraints :: PropertyShapeConstraints,
    propertyShapeDefaultValue :: (Maybe Syntax.Node),
    propertyShapeDescription :: Syntax.LangStrings,
    propertyShapeName :: Syntax.LangStrings,
    propertyShapeOrder :: (Maybe Int),
    propertyShapePath :: Syntax.Resource}
  deriving (Eq, Ord, Read, Show)

_PropertyShape = (Core.Name "hydra/ext/shacl/model.PropertyShape")

_PropertyShape_common = (Core.FieldName "common")

_PropertyShape_constraints = (Core.FieldName "constraints")

_PropertyShape_defaultValue = (Core.FieldName "defaultValue")

_PropertyShape_description = (Core.FieldName "description")

_PropertyShape_name = (Core.FieldName "name")

_PropertyShape_order = (Core.FieldName "order")

_PropertyShape_path = (Core.FieldName "path")

-- Any of a number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes
data PropertyShapeConstraints 
  = PropertyShapeConstraintsMaxCount Int
  | PropertyShapeConstraintsMinCount Int
  deriving (Eq, Ord, Read, Show)

_PropertyShapeConstraints = (Core.Name "hydra/ext/shacl/model.PropertyShapeConstraints")

_PropertyShapeConstraints_maxCount = (Core.FieldName "maxCount")

_PropertyShapeConstraints_minCount = (Core.FieldName "minCount")

data Severity 
  = SeverityInfo 
  | SeverityWarning 
  | SeverityViolation 
  deriving (Eq, Ord, Read, Show)

_Severity = (Core.Name "hydra/ext/shacl/model.Severity")

_Severity_info = (Core.FieldName "info")

_Severity_warning = (Core.FieldName "warning")

_Severity_violation = (Core.FieldName "violation")

-- A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes
data Shape 
  = ShapeNode NodeShape
  | ShapeProperty PropertyShape
  deriving (Eq, Ord, Read, Show)

_Shape = (Core.Name "hydra/ext/shacl/model.Shape")

_Shape_node = (Core.FieldName "node")

_Shape_property = (Core.FieldName "property")

-- An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes
newtype ShapesGraph 
  = ShapesGraph {
    unShapesGraph :: (Set Shape)}
  deriving (Eq, Ord, Read, Show)

_ShapesGraph = (Core.Name "hydra/ext/shacl/model.ShapesGraph")