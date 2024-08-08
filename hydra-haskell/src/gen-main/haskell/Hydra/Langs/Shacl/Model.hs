-- | A SHACL syntax model. See https://www.w3.org/TR/shacl

module Hydra.Langs.Shacl.Model where

import qualified Hydra.Core as Core
import qualified Hydra.Langs.Rdf.Syntax as Syntax
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

-- | See https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent
data Closed = 
  Closed {
    closedIsClosed :: Bool,
    closedIgnoredProperties :: (Maybe (Set Syntax.Property))}
  deriving (Eq, Ord, Read, Show)

_Closed = (Core.Name "hydra/langs/shacl/model.Closed")

_Closed_isClosed = (Core.Name "isClosed")

_Closed_ignoredProperties = (Core.Name "ignoredProperties")

_Closed_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.Closed"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "isClosed"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "ignoredProperties"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeSet Syntax._Property_type_))}]}))

-- | Any of a number of constraint parameters which can be applied either to node or property shapes
data CommonConstraint = 
  -- | See https://www.w3.org/TR/shacl/#AndConstraintComponent
  CommonConstraintAnd (Set (Reference Shape)) |
  -- | See https://www.w3.org/TR/shacl/#ClosedConstraintComponent
  CommonConstraintClosed Closed |
  -- | See https://www.w3.org/TR/shacl/#ClassConstraintComponent
  CommonConstraintClass (Set Syntax.RdfsClass) |
  -- | See https://www.w3.org/TR/shacl/#DatatypeConstraintComponent
  CommonConstraintDatatype Syntax.Iri |
  -- | See https://www.w3.org/TR/shacl/#DisjointConstraintComponent
  CommonConstraintDisjoint (Set Syntax.Property) |
  -- | See https://www.w3.org/TR/shacl/#EqualsConstraintComponent
  CommonConstraintEquals (Set Syntax.Property) |
  -- | Specifies the condition that at least one value node is equal to the given RDF term. See https://www.w3.org/TR/shacl/#HasValueConstraintComponent
  CommonConstraintHasValue (Set Syntax.Node) |
  -- | Specifies the condition that each value node is a member of a provided SHACL list. See https://www.w3.org/TR/shacl/#InConstraintComponent
  CommonConstraintIn [Syntax.Node] |
  -- | See https://www.w3.org/TR/shacl/#LanguageInConstraintComponent
  CommonConstraintLanguageIn (Set Syntax.LanguageTag) |
  -- | See https://www.w3.org/TR/shacl/#NodeKindConstraintComponent
  CommonConstraintNodeKind NodeKind |
  -- | See https://www.w3.org/TR/shacl/#NodeConstraintComponent
  CommonConstraintNode (Set (Reference NodeShape)) |
  -- | See https://www.w3.org/TR/shacl/#NotConstraintComponent
  CommonConstraintNot (Set (Reference Shape)) |
  -- | See https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent
  CommonConstraintMaxExclusive Syntax.Literal |
  -- | See https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent
  CommonConstraintMaxInclusive Syntax.Literal |
  -- | See https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent
  CommonConstraintMaxLength Integer |
  -- | See https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent
  CommonConstraintMinExclusive Syntax.Literal |
  -- | See https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent
  CommonConstraintMinInclusive Syntax.Literal |
  -- | See https://www.w3.org/TR/shacl/#MinLengthConstraintComponent
  CommonConstraintMinLength Integer |
  -- | See https://www.w3.org/TR/shacl/#PatternConstraintComponent
  CommonConstraintPattern Pattern |
  -- | See https://www.w3.org/TR/shacl/#PropertyConstraintComponent
  CommonConstraintProperty (Set (Reference PropertyShape)) |
  -- | See https://www.w3.org/TR/shacl/#OrConstraintComponent
  CommonConstraintOr (Set (Reference Shape)) |
  -- | See https://www.w3.org/TR/shacl/#XoneConstraintComponent
  CommonConstraintXone (Set (Reference Shape))
  deriving (Eq, Ord, Read, Show)

_CommonConstraint = (Core.Name "hydra/langs/shacl/model.CommonConstraint")

_CommonConstraint_and = (Core.Name "and")

_CommonConstraint_closed = (Core.Name "closed")

_CommonConstraint_class = (Core.Name "class")

_CommonConstraint_datatype = (Core.Name "datatype")

_CommonConstraint_disjoint = (Core.Name "disjoint")

_CommonConstraint_equals = (Core.Name "equals")

_CommonConstraint_hasValue = (Core.Name "hasValue")

_CommonConstraint_in = (Core.Name "in")

_CommonConstraint_languageIn = (Core.Name "languageIn")

_CommonConstraint_nodeKind = (Core.Name "nodeKind")

_CommonConstraint_node = (Core.Name "node")

_CommonConstraint_not = (Core.Name "not")

_CommonConstraint_maxExclusive = (Core.Name "maxExclusive")

_CommonConstraint_maxInclusive = (Core.Name "maxInclusive")

_CommonConstraint_maxLength = (Core.Name "maxLength")

_CommonConstraint_minExclusive = (Core.Name "minExclusive")

_CommonConstraint_minInclusive = (Core.Name "minInclusive")

_CommonConstraint_minLength = (Core.Name "minLength")

_CommonConstraint_pattern = (Core.Name "pattern")

_CommonConstraint_property = (Core.Name "property")

_CommonConstraint_or = (Core.Name "or")

_CommonConstraint_xone = (Core.Name "xone")

_CommonConstraint_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.CommonConstraint"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "and"),
      Core.fieldTypeType = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _Shape_type_})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "closed"),
      Core.fieldTypeType = _Closed_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = (Core.TypeSet Syntax._RdfsClass_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatype"),
      Core.fieldTypeType = Syntax._Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "disjoint"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Property_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equals"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Property_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasValue"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Node_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "in"),
      Core.fieldTypeType = (Core.TypeList Syntax._Node_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "languageIn"),
      Core.fieldTypeType = (Core.TypeSet Syntax._LanguageTag_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "nodeKind"),
      Core.fieldTypeType = _NodeKind_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "node"),
      Core.fieldTypeType = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _NodeShape_type_})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "not"),
      Core.fieldTypeType = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _Shape_type_})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "maxExclusive"),
      Core.fieldTypeType = Syntax._Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "maxInclusive"),
      Core.fieldTypeType = Syntax._Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "maxLength"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minExclusive"),
      Core.fieldTypeType = Syntax._Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minInclusive"),
      Core.fieldTypeType = Syntax._Literal_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minLength"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "pattern"),
      Core.fieldTypeType = _Pattern_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _PropertyShape_type_})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "or"),
      Core.fieldTypeType = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _Shape_type_})))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "xone"),
      Core.fieldTypeType = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _Shape_type_})))}]}))

-- | Common constraint parameters and other properties for SHACL shapes
data CommonProperties = 
  CommonProperties {
    -- | Common constraint parameters attached to this shape
    commonPropertiesConstraints :: (Set CommonConstraint),
    -- | See https://www.w3.org/TR/shacl/#deactivated
    commonPropertiesDeactivated :: (Maybe Bool),
    -- | See https://www.w3.org/TR/shacl/#message
    commonPropertiesMessage :: Syntax.LangStrings,
    -- | See https://www.w3.org/TR/shacl/#severity
    commonPropertiesSeverity :: Severity,
    -- | See https://www.w3.org/TR/shacl/#targetClass
    commonPropertiesTargetClass :: (Set Syntax.RdfsClass),
    -- | See https://www.w3.org/TR/shacl/#targetNode
    commonPropertiesTargetNode :: (Set Syntax.IriOrLiteral),
    -- | See https://www.w3.org/TR/shacl/#targetObjectsOf
    commonPropertiesTargetObjectsOf :: (Set Syntax.Property),
    -- | See https://www.w3.org/TR/shacl/#targetSubjectsOf
    commonPropertiesTargetSubjectsOf :: (Set Syntax.Property)}
  deriving (Eq, Ord, Read, Show)

_CommonProperties = (Core.Name "hydra/langs/shacl/model.CommonProperties")

_CommonProperties_constraints = (Core.Name "constraints")

_CommonProperties_deactivated = (Core.Name "deactivated")

_CommonProperties_message = (Core.Name "message")

_CommonProperties_severity = (Core.Name "severity")

_CommonProperties_targetClass = (Core.Name "targetClass")

_CommonProperties_targetNode = (Core.Name "targetNode")

_CommonProperties_targetObjectsOf = (Core.Name "targetObjectsOf")

_CommonProperties_targetSubjectsOf = (Core.Name "targetSubjectsOf")

_CommonProperties_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.CommonProperties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constraints"),
      Core.fieldTypeType = (Core.TypeSet _CommonConstraint_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "deactivated"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeBoolean))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "message"),
      Core.fieldTypeType = Syntax._LangStrings_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "severity"),
      Core.fieldTypeType = _Severity_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "targetClass"),
      Core.fieldTypeType = (Core.TypeSet Syntax._RdfsClass_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "targetNode"),
      Core.fieldTypeType = (Core.TypeSet Syntax._IriOrLiteral_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "targetObjectsOf"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Property_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "targetSubjectsOf"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Property_type_)}]}))

-- | An instance of a type like sh:Shape or sh:NodeShape, together with a unique IRI for that instance
data Definition a = 
  Definition {
    definitionIri :: Syntax.Iri,
    definitionTarget :: a}
  deriving (Eq, Ord, Read, Show)

_Definition = (Core.Name "hydra/langs/shacl/model.Definition")

_Definition_iri = (Core.Name "iri")

_Definition_target = (Core.Name "target")

_Definition_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "a"),
  Core.lambdaTypeBody = (Core.TypeRecord (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.Definition"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "iri"),
        Core.fieldTypeType = Syntax._Iri_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "target"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))}]}))}))

data NodeKind = 
  -- | A blank node
  NodeKindBlankNode  |
  -- | An IRI
  NodeKindIri  |
  -- | A literal
  NodeKindLiteral  |
  -- | A blank node or an IRI
  NodeKindBlankNodeOrIri  |
  -- | A blank node or a literal
  NodeKindBlankNodeOrLiteral  |
  -- | An IRI or a literal
  NodeKindIriOrLiteral 
  deriving (Eq, Ord, Read, Show)

_NodeKind = (Core.Name "hydra/langs/shacl/model.NodeKind")

_NodeKind_blankNode = (Core.Name "blankNode")

_NodeKind_iri = (Core.Name "iri")

_NodeKind_literal = (Core.Name "literal")

_NodeKind_blankNodeOrIri = (Core.Name "blankNodeOrIri")

_NodeKind_blankNodeOrLiteral = (Core.Name "blankNodeOrLiteral")

_NodeKind_iriOrLiteral = (Core.Name "iriOrLiteral")

_NodeKind_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.NodeKind"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "blankNode"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "blankNodeOrIri"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "blankNodeOrLiteral"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iriOrLiteral"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes
data NodeShape = 
  NodeShape {
    nodeShapeCommon :: CommonProperties}
  deriving (Eq, Ord, Read, Show)

_NodeShape = (Core.Name "hydra/langs/shacl/model.NodeShape")

_NodeShape_common = (Core.Name "common")

_NodeShape_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.NodeShape"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "common"),
      Core.fieldTypeType = _CommonProperties_type_}]}))

-- | A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent
data Pattern = 
  Pattern {
    patternRegex :: String,
    patternFlags :: (Maybe String)}
  deriving (Eq, Ord, Read, Show)

_Pattern = (Core.Name "hydra/langs/shacl/model.Pattern")

_Pattern_regex = (Core.Name "regex")

_Pattern_flags = (Core.Name "flags")

_Pattern_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.Pattern"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "regex"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeString)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "flags"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeString))}]}))

-- | A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes
data PropertyShape = 
  PropertyShape {
    propertyShapeCommon :: CommonProperties,
    -- | Any property shape -specific constraint parameters
    propertyShapeConstraints :: (Set PropertyShapeConstraint),
    -- | See https://www.w3.org/TR/shacl/#defaultValue
    propertyShapeDefaultValue :: (Maybe Syntax.Node),
    -- | See https://www.w3.org/TR/shacl/#name
    propertyShapeDescription :: Syntax.LangStrings,
    -- | See https://www.w3.org/TR/shacl/#name
    propertyShapeName :: Syntax.LangStrings,
    -- | See https://www.w3.org/TR/shacl/#order
    propertyShapeOrder :: (Maybe Integer),
    propertyShapePath :: Syntax.Iri}
  deriving (Eq, Ord, Read, Show)

_PropertyShape = (Core.Name "hydra/langs/shacl/model.PropertyShape")

_PropertyShape_common = (Core.Name "common")

_PropertyShape_constraints = (Core.Name "constraints")

_PropertyShape_defaultValue = (Core.Name "defaultValue")

_PropertyShape_description = (Core.Name "description")

_PropertyShape_name = (Core.Name "name")

_PropertyShape_order = (Core.Name "order")

_PropertyShape_path = (Core.Name "path")

_PropertyShape_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.PropertyShape"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "common"),
      Core.fieldTypeType = _CommonProperties_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constraints"),
      Core.fieldTypeType = (Core.TypeSet _PropertyShapeConstraint_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "defaultValue"),
      Core.fieldTypeType = (Core.TypeOptional Syntax._Node_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "description"),
      Core.fieldTypeType = Syntax._LangStrings_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "name"),
      Core.fieldTypeType = Syntax._LangStrings_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "order"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint)))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "path"),
      Core.fieldTypeType = Syntax._Iri_type_}]}))

-- | A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes
data PropertyShapeConstraint = 
  -- | See https://www.w3.org/TR/shacl/#LessThanConstraintComponent
  PropertyShapeConstraintLessThan (Set Syntax.Property) |
  -- | See https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent
  PropertyShapeConstraintLessThanOrEquals (Set Syntax.Property) |
  -- | The maximum cardinality. Node shapes cannot have any value for sh:maxCount. See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent
  PropertyShapeConstraintMaxCount Integer |
  -- | The minimum cardinality. Node shapes cannot have any value for sh:minCount. See https://www.w3.org/TR/shacl/#MinCountConstraintComponent
  PropertyShapeConstraintMinCount Integer |
  -- | See https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent
  PropertyShapeConstraintUniqueLang Bool |
  -- | See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
  PropertyShapeConstraintQualifiedValueShape QualifiedValueShape
  deriving (Eq, Ord, Read, Show)

_PropertyShapeConstraint = (Core.Name "hydra/langs/shacl/model.PropertyShapeConstraint")

_PropertyShapeConstraint_lessThan = (Core.Name "lessThan")

_PropertyShapeConstraint_lessThanOrEquals = (Core.Name "lessThanOrEquals")

_PropertyShapeConstraint_maxCount = (Core.Name "maxCount")

_PropertyShapeConstraint_minCount = (Core.Name "minCount")

_PropertyShapeConstraint_uniqueLang = (Core.Name "uniqueLang")

_PropertyShapeConstraint_qualifiedValueShape = (Core.Name "qualifiedValueShape")

_PropertyShapeConstraint_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.PropertyShapeConstraint"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessThan"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Property_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "lessThanOrEquals"),
      Core.fieldTypeType = (Core.TypeSet Syntax._Property_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "maxCount"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "minCount"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "uniqueLang"),
      Core.fieldTypeType = (Core.TypeLiteral Core.LiteralTypeBoolean)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiedValueShape"),
      Core.fieldTypeType = _QualifiedValueShape_type_}]}))

-- | See https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent
data QualifiedValueShape = 
  QualifiedValueShape {
    qualifiedValueShapeQualifiedValueShape :: (Reference Shape),
    qualifiedValueShapeQualifiedMaxCount :: Integer,
    qualifiedValueShapeQualifiedMinCount :: Integer,
    qualifiedValueShapeQualifiedValueShapesDisjoint :: (Maybe Bool)}
  deriving (Eq, Ord, Read, Show)

_QualifiedValueShape = (Core.Name "hydra/langs/shacl/model.QualifiedValueShape")

_QualifiedValueShape_qualifiedValueShape = (Core.Name "qualifiedValueShape")

_QualifiedValueShape_qualifiedMaxCount = (Core.Name "qualifiedMaxCount")

_QualifiedValueShape_qualifiedMinCount = (Core.Name "qualifiedMinCount")

_QualifiedValueShape_qualifiedValueShapesDisjoint = (Core.Name "qualifiedValueShapesDisjoint")

_QualifiedValueShape_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.QualifiedValueShape"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiedValueShape"),
      Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
        Core.applicationTypeFunction = _Reference_type_,
        Core.applicationTypeArgument = _Shape_type_}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiedMaxCount"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiedMinCount"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "qualifiedValueShapesDisjoint"),
      Core.fieldTypeType = (Core.TypeOptional (Core.TypeLiteral Core.LiteralTypeBoolean))}]}))

-- | Either an instance of a type like sh:Shape or sh:NodeShape, or an IRI which refers to an instance of that type
data Reference a = 
  ReferenceNamed Syntax.Iri |
  -- | An anonymous instance
  ReferenceAnonymous a |
  -- | An inline definition
  ReferenceDefinition (Definition a)
  deriving (Eq, Ord, Read, Show)

_Reference = (Core.Name "hydra/langs/shacl/model.Reference")

_Reference_named = (Core.Name "named")

_Reference_anonymous = (Core.Name "anonymous")

_Reference_definition = (Core.Name "definition")

_Reference_type_ = (Core.TypeLambda (Core.LambdaType {
  Core.lambdaTypeParameter = (Core.Name "a"),
  Core.lambdaTypeBody = (Core.TypeUnion (Core.RowType {
    Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.Reference"),
    Core.rowTypeExtends = Nothing,
    Core.rowTypeFields = [
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "named"),
        Core.fieldTypeType = Syntax._Iri_type_},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "anonymous"),
        Core.fieldTypeType = (Core.TypeVariable (Core.Name "a"))},
      Core.FieldType {
        Core.fieldTypeName = (Core.Name "definition"),
        Core.fieldTypeType = (Core.TypeApplication (Core.ApplicationType {
          Core.applicationTypeFunction = _Definition_type_,
          Core.applicationTypeArgument = (Core.TypeVariable (Core.Name "a"))}))}]}))}))

data Severity = 
  -- | A non-critical constraint violation indicating an informative message
  SeverityInfo  |
  -- | A non-critical constraint violation indicating a warning
  SeverityWarning  |
  -- | A constraint violation
  SeverityViolation 
  deriving (Eq, Ord, Read, Show)

_Severity = (Core.Name "hydra/langs/shacl/model.Severity")

_Severity_info = (Core.Name "info")

_Severity_warning = (Core.Name "warning")

_Severity_violation = (Core.Name "violation")

_Severity_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.Severity"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "info"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "warning"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "violation"),
      Core.fieldTypeType = (Core.TypeRecord (Core.RowType {
        Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
        Core.rowTypeExtends = Nothing,
        Core.rowTypeFields = []}))}]}))

-- | A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes
data Shape = 
  ShapeNode NodeShape |
  ShapeProperty PropertyShape
  deriving (Eq, Ord, Read, Show)

_Shape = (Core.Name "hydra/langs/shacl/model.Shape")

_Shape_node = (Core.Name "node")

_Shape_property = (Core.Name "property")

_Shape_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/shacl/model.Shape"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "node"),
      Core.fieldTypeType = _NodeShape_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _PropertyShape_type_}]}))

-- | An RDF graph containing zero or more shapes that is passed into a SHACL validation process so that a data graph can be validated against the shapes
newtype ShapesGraph = 
  ShapesGraph {
    unShapesGraph :: (Set (Definition Shape))}
  deriving (Eq, Ord, Read, Show)

_ShapesGraph = (Core.Name "hydra/langs/shacl/model.ShapesGraph")

_ShapesGraph_type_ = (Core.TypeSet (Core.TypeApplication (Core.ApplicationType {
  Core.applicationTypeFunction = _Definition_type_,
  Core.applicationTypeArgument = _Shape_type_})))