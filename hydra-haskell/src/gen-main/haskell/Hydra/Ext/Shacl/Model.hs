module Hydra.Ext.Shacl.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Ext.Rdf.Syntax as Syntax
import Data.Map
import Data.Set

-- See https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent
data Closed 
  = Closed {
    closedIsClosed :: Bool,
    closedIgnoredProperties :: (Maybe [Syntax.Property])}
  deriving (Eq, Ord, Read, Show)

_Closed = (Core.Name "hydra/ext/shacl/model.Closed")

_Closed_isClosed = (Core.FieldName "isClosed")

_Closed_ignoredProperties = (Core.FieldName "ignoredProperties")

-- Any of a number of constraint parameters which can be applied either to node or property shapes
data CommonConstraint 
  = CommonConstraintAnd [Shape]
  | CommonConstraintClosed Closed
  | CommonConstraintClass (Set Syntax.RdfsClass)
  | CommonConstraintDatatype Syntax.Iri
  | CommonConstraintDisjoint (Set Syntax.Property)
  | CommonConstraintEquals (Set Syntax.Property)
  | CommonConstraintHasValue (Set Syntax.Node)
  | CommonConstraintIn [Syntax.Node]
  | CommonConstraintLanguageIn [Syntax.LanguageTag]
  | CommonConstraintNodeKind NodeKind
  | CommonConstraintNode (Set NodeShape)
  | CommonConstraintNot (Set Shape)
  | CommonConstraintMaxExclusive Syntax.Literal
  | CommonConstraintMaxInclusive Syntax.Literal
  | CommonConstraintMaxLength Integer
  | CommonConstraintMinExclusive Syntax.Literal
  | CommonConstraintMinInclusive Syntax.Literal
  | CommonConstraintMinLength Integer
  | CommonConstraintPattern Pattern
  | CommonConstraintProperty (Set PropertyShape)
  | CommonConstraintOr [Shape]
  | CommonConstraintXone [Shape]
  deriving (Eq, Ord, Read, Show)

_CommonConstraint = (Core.Name "hydra/ext/shacl/model.CommonConstraint")

_CommonConstraint_and = (Core.FieldName "and")

_CommonConstraint_closed = (Core.FieldName "closed")

_CommonConstraint_class = (Core.FieldName "class")

_CommonConstraint_datatype = (Core.FieldName "datatype")

_CommonConstraint_disjoint = (Core.FieldName "disjoint")

_CommonConstraint_equals = (Core.FieldName "equals")

_CommonConstraint_hasValue = (Core.FieldName "hasValue")

_CommonConstraint_in = (Core.FieldName "in")

_CommonConstraint_languageIn = (Core.FieldName "languageIn")

_CommonConstraint_nodeKind = (Core.FieldName "nodeKind")

_CommonConstraint_node = (Core.FieldName "node")

_CommonConstraint_not = (Core.FieldName "not")

_CommonConstraint_maxExclusive = (Core.FieldName "maxExclusive")

_CommonConstraint_maxInclusive = (Core.FieldName "maxInclusive")

_CommonConstraint_maxLength = (Core.FieldName "maxLength")

_CommonConstraint_minExclusive = (Core.FieldName "minExclusive")

_CommonConstraint_minInclusive = (Core.FieldName "minInclusive")

_CommonConstraint_minLength = (Core.FieldName "minLength")

_CommonConstraint_pattern = (Core.FieldName "pattern")

_CommonConstraint_property = (Core.FieldName "property")

_CommonConstraint_or = (Core.FieldName "or")

_CommonConstraint_xone = (Core.FieldName "xone")

-- Common constraint parameters and other properties for SHACL shapes
data CommonProperties 
  = CommonProperties {
    -- Common constraint parameters attached to this shape
    commonPropertiesConstraints :: [CommonConstraint],
    -- See https://www.w3.org/TR/shacl/#deactivated
    commonPropertiesDeactivated :: (Maybe Bool),
    -- See https://www.w3.org/TR/shacl/#message
    commonPropertiesMessage :: Syntax.LangStrings,
    -- See https://www.w3.org/TR/shacl/#severity
    commonPropertiesSeverity :: Severity,
    -- See https://www.w3.org/TR/shacl/#targetClass
    commonPropertiesTargetClass :: (Set Syntax.RdfsClass),
    -- See https://www.w3.org/TR/shacl/#targetNode
    commonPropertiesTargetNode :: (Set Syntax.IriOrLiteral),
    -- See https://www.w3.org/TR/shacl/#targetObjectsOf
    commonPropertiesTargetObjectsOf :: (Set Syntax.Property),
    -- See https://www.w3.org/TR/shacl/#targetSubjectsOf
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

data NodeKind 
  = NodeKindBlankNode 
  | NodeKindIri 
  | NodeKindLiteral 
  | NodeKindBlankNodeOrIri 
  | NodeKindBlankNodeOrLiteral 
  | NodeKindIriOrLiteral 
  deriving (Eq, Ord, Read, Show)

_NodeKind = (Core.Name "hydra/ext/shacl/model.NodeKind")

_NodeKind_blankNode = (Core.FieldName "blankNode")

_NodeKind_iri = (Core.FieldName "iri")

_NodeKind_literal = (Core.FieldName "literal")

_NodeKind_blankNodeOrIri = (Core.FieldName "blankNodeOrIri")

_NodeKind_blankNodeOrLiteral = (Core.FieldName "blankNodeOrLiteral")

_NodeKind_iriOrLiteral = (Core.FieldName "iriOrLiteral")

-- A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes
data NodeShape 
  = NodeShape {
    nodeShapeCommon :: CommonProperties}
  deriving (Eq, Ord, Read, Show)

_NodeShape = (Core.Name "hydra/ext/shacl/model.NodeShape")

_NodeShape_common = (Core.FieldName "common")

-- A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent
data Pattern 
  = Pattern {
    patternRegex :: String,
    patternFlags :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/ext/shacl/model.Pattern")

_Pattern_regex = (Core.FieldName "regex")

_Pattern_flags = (Core.FieldName "flags")

-- A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes
data PropertyShape 
  = PropertyShape {
    propertyShapeCommon :: CommonProperties,
    -- Any property shape -specific constraint parameters
    propertyShapeConstraints :: [PropertyShapeConstraint],
    -- See https://www.w3.org/TR/shacl/#defaultValue
    propertyShapeDefaultValue :: (Maybe Syntax.Node),
    -- See https://www.w3.org/TR/shacl/#name
    propertyShapeDescription :: Syntax.LangStrings,
    -- See https://www.w3.org/TR/shacl/#name
    propertyShapeName :: Syntax.LangStrings,
    -- See https://www.w3.org/TR/shacl/#order
    propertyShapeOrder :: (Maybe Integer),
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

-- A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes
data PropertyShapeConstraint 
  = PropertyShapeConstraintLessThan (Set Syntax.Property)
  | PropertyShapeConstraintLessThanOrEquals (Set Syntax.Property)
  | PropertyShapeConstraintMaxCount Integer
  | PropertyShapeConstraintMinCount Integer
  | PropertyShapeConstraintUniqueLang Bool
  | PropertyShapeConstraintQualifiedValueShape QualifiedValueShape
  deriving (Eq, Ord, Read, Show)

_PropertyShapeConstraint = (Core.Name "hydra/ext/shacl/model.PropertyShapeConstraint")

_PropertyShapeConstraint_lessThan = (Core.FieldName "lessThan")

_PropertyShapeConstraint_lessThanOrEquals = (Core.FieldName "lessThanOrEquals")

_PropertyShapeConstraint_maxCount = (Core.FieldName "maxCount")

_PropertyShapeConstraint_minCount = (Core.FieldName "minCount")

_PropertyShapeConstraint_uniqueLang = (Core.FieldName "uniqueLang")

_PropertyShapeConstraint_qualifiedValueShape = (Core.FieldName "qualifiedValueShape")

-- See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
data QualifiedValueShape 
  = QualifiedValueShape {
    qualifiedValueShapeShape :: Shape,
    qualifiedValueShapeQualifiedManCount :: Integer,
    qualifiedValueShapeQualifiedMinCount :: Integer,
    qualifiedValueShapeQualifiedValueShapesDisjoint :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_QualifiedValueShape = (Core.Name "hydra/ext/shacl/model.QualifiedValueShape")

_QualifiedValueShape_shape = (Core.FieldName "shape")

_QualifiedValueShape_qualifiedManCount = (Core.FieldName "qualifiedManCount")

_QualifiedValueShape_qualifiedMinCount = (Core.FieldName "qualifiedMinCount")

_QualifiedValueShape_qualifiedValueShapesDisjoint = (Core.FieldName "qualifiedValueShapesDisjoint")

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
    -- An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes
    unShapesGraph :: (Set Shape)}
  deriving (Eq, Ord, Read, Show)

_ShapesGraph = (Core.Name "hydra/ext/shacl/model.ShapesGraph")