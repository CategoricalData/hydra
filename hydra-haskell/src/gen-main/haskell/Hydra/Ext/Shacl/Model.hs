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
data CommonConstraints 
  = CommonConstraints {
    commonConstraintsAnd :: (Maybe [Shape]),
    commonConstraintsClosed :: (Maybe Closed),
    commonConstraintsClass :: (Set Syntax.RdfsClass),
    commonConstraintsDatatype :: (Maybe Syntax.Iri),
    commonConstraintsDisjoint :: (Set Syntax.Property),
    commonConstraintsEquals :: (Set Syntax.Property),
    commonConstraintsHasValue :: (Set Syntax.Node),
    commonConstraintsIn :: (Maybe [Syntax.Node]),
    commonConstraintsLanguageIn :: (Maybe [Syntax.LanguageTag]),
    commonConstraintsNodeKind :: (Maybe NodeKind),
    commonConstraintsNode :: (Set NodeShape),
    commonConstraintsNot :: (Set Shape),
    commonConstraintsMaxExclusive :: (Maybe Syntax.Literal),
    commonConstraintsMaxInclusive :: (Maybe Syntax.Literal),
    commonConstraintsMaxLength :: (Maybe Integer),
    commonConstraintsMinExclusive :: (Maybe Syntax.Literal),
    commonConstraintsMinInclusive :: (Maybe Syntax.Literal),
    commonConstraintsMinLength :: (Maybe Integer),
    commonConstraintsPattern :: (Maybe Pattern),
    commonConstraintsProperty :: (Set PropertyShape),
    commonConstraintsOr :: (Maybe [Shape]),
    commonConstraintsXone :: (Maybe [Shape])}
  deriving (Eq, Ord, Read, Show)

_CommonConstraints = (Core.Name "hydra/ext/shacl/model.CommonConstraints")

_CommonConstraints_and = (Core.FieldName "and")

_CommonConstraints_closed = (Core.FieldName "closed")

_CommonConstraints_class = (Core.FieldName "class")

_CommonConstraints_datatype = (Core.FieldName "datatype")

_CommonConstraints_disjoint = (Core.FieldName "disjoint")

_CommonConstraints_equals = (Core.FieldName "equals")

_CommonConstraints_hasValue = (Core.FieldName "hasValue")

_CommonConstraints_in = (Core.FieldName "in")

_CommonConstraints_languageIn = (Core.FieldName "languageIn")

_CommonConstraints_nodeKind = (Core.FieldName "nodeKind")

_CommonConstraints_node = (Core.FieldName "node")

_CommonConstraints_not = (Core.FieldName "not")

_CommonConstraints_maxExclusive = (Core.FieldName "maxExclusive")

_CommonConstraints_maxInclusive = (Core.FieldName "maxInclusive")

_CommonConstraints_maxLength = (Core.FieldName "maxLength")

_CommonConstraints_minExclusive = (Core.FieldName "minExclusive")

_CommonConstraints_minInclusive = (Core.FieldName "minInclusive")

_CommonConstraints_minLength = (Core.FieldName "minLength")

_CommonConstraints_pattern = (Core.FieldName "pattern")

_CommonConstraints_property = (Core.FieldName "property")

_CommonConstraints_or = (Core.FieldName "or")

_CommonConstraints_xone = (Core.FieldName "xone")

-- Common constraint parameters and other properties for SHACL shapes
data CommonProperties 
  = CommonProperties {
    commonPropertiesConstraints :: CommonConstraints,
    commonPropertiesDeactivated :: (Maybe Bool),
    commonPropertiesMessage :: Syntax.LangStrings,
    commonPropertiesSeverity :: Severity,
    commonPropertiesTargetClass :: (Set Syntax.RdfsClass),
    commonPropertiesTargetNode :: (Set Syntax.IriOrLiteral),
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
    propertyShapeConstraints :: PropertyShapeConstraints,
    propertyShapeDefaultValue :: (Maybe Syntax.Node),
    propertyShapeDescription :: Syntax.LangStrings,
    propertyShapeName :: Syntax.LangStrings,
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
data PropertyShapeConstraints 
  = PropertyShapeConstraints {
    propertyShapeConstraintsLessThan :: (Set Syntax.Property),
    propertyShapeConstraintsLessThanOrEquals :: (Set Syntax.Property),
    propertyShapeConstraintsMaxCount :: (Maybe Integer),
    propertyShapeConstraintsMinCount :: (Maybe Integer),
    propertyShapeConstraintsUniqueLang :: (Maybe Bool),
    propertyShapeConstraintsQualifiedValueShape :: (Maybe QualifiedValueShape)}
  deriving (Eq, Ord, Read, Show)

_PropertyShapeConstraints = (Core.Name "hydra/ext/shacl/model.PropertyShapeConstraints")

_PropertyShapeConstraints_lessThan = (Core.FieldName "lessThan")

_PropertyShapeConstraints_lessThanOrEquals = (Core.FieldName "lessThanOrEquals")

_PropertyShapeConstraints_maxCount = (Core.FieldName "maxCount")

_PropertyShapeConstraints_minCount = (Core.FieldName "minCount")

_PropertyShapeConstraints_uniqueLang = (Core.FieldName "uniqueLang")

_PropertyShapeConstraints_qualifiedValueShape = (Core.FieldName "qualifiedValueShape")

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
    unShapesGraph :: (Set Shape)}
  deriving (Eq, Ord, Read, Show)

_ShapesGraph = (Core.Name "hydra/ext/shacl/model.ShapesGraph")