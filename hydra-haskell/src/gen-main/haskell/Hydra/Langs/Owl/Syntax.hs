-- | An OWL 2 syntax model. See https://www.w3.org/TR/owl2-syntax

module Hydra.Langs.Owl.Syntax where

import qualified Hydra.Core as Core
import qualified Hydra.Langs.Rdf.Syntax as Syntax
import qualified Hydra.Langs.Xml.Schema as Schema
import Data.Int
import Data.List as L
import Data.Map as M
import Data.Set as S

data Ontology = 
  Ontology {
    ontologyDirectImports :: [Ontology],
    ontologyAnnotations :: [Annotation],
    ontologyAxioms :: [Axiom]}
  deriving (Eq, Ord, Read, Show)

_Ontology = (Core.Name "hydra/langs/owl/syntax.Ontology")

_Ontology_directImports = (Core.Name "directImports")

_Ontology_annotations = (Core.Name "annotations")

_Ontology_axioms = (Core.Name "axioms")

_Ontology_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Ontology"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "directImports"),
      Core.fieldTypeType = (Core.TypeList _Ontology_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "axioms"),
      Core.fieldTypeType = (Core.TypeList _Axiom_type_)}]}))

data Declaration = 
  Declaration {
    declarationAnnotations :: [Annotation],
    declarationEntity :: Entity}
  deriving (Eq, Ord, Read, Show)

_Declaration = (Core.Name "hydra/langs/owl/syntax.Declaration")

_Declaration_annotations = (Core.Name "annotations")

_Declaration_entity = (Core.Name "entity")

_Declaration_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Declaration"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "entity"),
      Core.fieldTypeType = _Entity_type_}]}))

data Entity = 
  EntityAnnotationProperty AnnotationProperty |
  EntityClass Class |
  EntityDataProperty DataProperty |
  EntityDatatype Datatype |
  EntityNamedIndividual NamedIndividual |
  EntityObjectProperty ObjectProperty
  deriving (Eq, Ord, Read, Show)

_Entity = (Core.Name "hydra/langs/owl/syntax.Entity")

_Entity_annotationProperty = (Core.Name "annotationProperty")

_Entity_class = (Core.Name "class")

_Entity_dataProperty = (Core.Name "dataProperty")

_Entity_datatype = (Core.Name "datatype")

_Entity_namedIndividual = (Core.Name "namedIndividual")

_Entity_objectProperty = (Core.Name "objectProperty")

_Entity_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Entity"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationProperty"),
      Core.fieldTypeType = _AnnotationProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _Class_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataProperty"),
      Core.fieldTypeType = _DataProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatype"),
      Core.fieldTypeType = _Datatype_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "namedIndividual"),
      Core.fieldTypeType = _NamedIndividual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectProperty"),
      Core.fieldTypeType = _ObjectProperty_type_}]}))

data AnnotationSubject = 
  AnnotationSubjectIri Syntax.Iri |
  AnnotationSubjectAnonymousIndividual AnonymousIndividual
  deriving (Eq, Ord, Read, Show)

_AnnotationSubject = (Core.Name "hydra/langs/owl/syntax.AnnotationSubject")

_AnnotationSubject_iri = (Core.Name "iri")

_AnnotationSubject_anonymousIndividual = (Core.Name "anonymousIndividual")

_AnnotationSubject_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AnnotationSubject"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = Syntax._Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anonymousIndividual"),
      Core.fieldTypeType = _AnonymousIndividual_type_}]}))

data AnnotationValue = 
  AnnotationValueAnonymousIndividual AnonymousIndividual |
  AnnotationValueIri Syntax.Iri |
  AnnotationValueLiteral Syntax.Literal
  deriving (Eq, Ord, Read, Show)

_AnnotationValue = (Core.Name "hydra/langs/owl/syntax.AnnotationValue")

_AnnotationValue_anonymousIndividual = (Core.Name "anonymousIndividual")

_AnnotationValue_iri = (Core.Name "iri")

_AnnotationValue_literal = (Core.Name "literal")

_AnnotationValue_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AnnotationValue"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anonymousIndividual"),
      Core.fieldTypeType = _AnonymousIndividual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = Syntax._Iri_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "literal"),
      Core.fieldTypeType = Syntax._Literal_type_}]}))

data Annotation = 
  Annotation {
    annotationAnnotations :: [Annotation],
    annotationProperty :: AnnotationProperty,
    annotationValue :: AnnotationValue}
  deriving (Eq, Ord, Read, Show)

_Annotation = (Core.Name "hydra/langs/owl/syntax.Annotation")

_Annotation_annotations = (Core.Name "annotations")

_Annotation_property = (Core.Name "property")

_Annotation_value = (Core.Name "value")

_Annotation_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Annotation"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _AnnotationProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _AnnotationValue_type_}]}))

data AnnotationAxiom = 
  AnnotationAxiomAnnotationAssertion AnnotationAssertion |
  AnnotationAxiomAnnotationPropertyDomain AnnotationPropertyDomain |
  AnnotationAxiomAnnotationPropertyRange AnnotationPropertyRange |
  AnnotationAxiomSubAnnotationPropertyOf SubAnnotationPropertyOf
  deriving (Eq, Ord, Read, Show)

_AnnotationAxiom = (Core.Name "hydra/langs/owl/syntax.AnnotationAxiom")

_AnnotationAxiom_annotationAssertion = (Core.Name "annotationAssertion")

_AnnotationAxiom_annotationPropertyDomain = (Core.Name "annotationPropertyDomain")

_AnnotationAxiom_annotationPropertyRange = (Core.Name "annotationPropertyRange")

_AnnotationAxiom_subAnnotationPropertyOf = (Core.Name "subAnnotationPropertyOf")

_AnnotationAxiom_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AnnotationAxiom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationAssertion"),
      Core.fieldTypeType = _AnnotationAssertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationPropertyDomain"),
      Core.fieldTypeType = _AnnotationPropertyDomain_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationPropertyRange"),
      Core.fieldTypeType = _AnnotationPropertyRange_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subAnnotationPropertyOf"),
      Core.fieldTypeType = _SubAnnotationPropertyOf_type_}]}))

data AnnotationAssertion = 
  AnnotationAssertion {
    annotationAssertionAnnotations :: [Annotation],
    annotationAssertionProperty :: AnnotationProperty,
    annotationAssertionSubject :: AnnotationSubject,
    annotationAssertionValue :: AnnotationValue}
  deriving (Eq, Ord, Read, Show)

_AnnotationAssertion = (Core.Name "hydra/langs/owl/syntax.AnnotationAssertion")

_AnnotationAssertion_annotations = (Core.Name "annotations")

_AnnotationAssertion_property = (Core.Name "property")

_AnnotationAssertion_subject = (Core.Name "subject")

_AnnotationAssertion_value = (Core.Name "value")

_AnnotationAssertion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AnnotationAssertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _AnnotationProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subject"),
      Core.fieldTypeType = _AnnotationSubject_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = _AnnotationValue_type_}]}))

data SubAnnotationPropertyOf = 
  SubAnnotationPropertyOf {
    subAnnotationPropertyOfAnnotations :: [Annotation],
    subAnnotationPropertyOfSubProperty :: AnnotationProperty,
    subAnnotationPropertyOfSuperProperty :: AnnotationProperty}
  deriving (Eq, Ord, Read, Show)

_SubAnnotationPropertyOf = (Core.Name "hydra/langs/owl/syntax.SubAnnotationPropertyOf")

_SubAnnotationPropertyOf_annotations = (Core.Name "annotations")

_SubAnnotationPropertyOf_subProperty = (Core.Name "subProperty")

_SubAnnotationPropertyOf_superProperty = (Core.Name "superProperty")

_SubAnnotationPropertyOf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.SubAnnotationPropertyOf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subProperty"),
      Core.fieldTypeType = _AnnotationProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "superProperty"),
      Core.fieldTypeType = _AnnotationProperty_type_}]}))

data AnnotationPropertyDomain = 
  AnnotationPropertyDomain {
    annotationPropertyDomainAnnotations :: [Annotation],
    annotationPropertyDomainProperty :: AnnotationProperty,
    annotationPropertyDomainIri :: Syntax.Iri}
  deriving (Eq, Ord, Read, Show)

_AnnotationPropertyDomain = (Core.Name "hydra/langs/owl/syntax.AnnotationPropertyDomain")

_AnnotationPropertyDomain_annotations = (Core.Name "annotations")

_AnnotationPropertyDomain_property = (Core.Name "property")

_AnnotationPropertyDomain_iri = (Core.Name "iri")

_AnnotationPropertyDomain_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AnnotationPropertyDomain"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _AnnotationProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = Syntax._Iri_type_}]}))

data AnnotationPropertyRange = 
  AnnotationPropertyRange {
    annotationPropertyRangeAnnotations :: [Annotation],
    annotationPropertyRangeProperty :: AnnotationProperty,
    annotationPropertyRangeIri :: Syntax.Iri}
  deriving (Eq, Ord, Read, Show)

_AnnotationPropertyRange = (Core.Name "hydra/langs/owl/syntax.AnnotationPropertyRange")

_AnnotationPropertyRange_annotations = (Core.Name "annotations")

_AnnotationPropertyRange_property = (Core.Name "property")

_AnnotationPropertyRange_iri = (Core.Name "iri")

_AnnotationPropertyRange_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AnnotationPropertyRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _AnnotationProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "iri"),
      Core.fieldTypeType = Syntax._Iri_type_}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Classes
data Class = 
  Class {}
  deriving (Eq, Ord, Read, Show)

_Class = (Core.Name "hydra/langs/owl/syntax.Class")

_Class_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

-- | See https://www.w3.org/TR/owl2-syntax/#Datatypes
data Datatype = 
  -- | Note: XML Schema datatypes are treated as a special case in this model (not in the OWL 2 specification itself) because they are particularly common
  DatatypeXmlSchema Schema.Datatype |
  DatatypeOther Syntax.Iri
  deriving (Eq, Ord, Read, Show)

_Datatype = (Core.Name "hydra/langs/owl/syntax.Datatype")

_Datatype_xmlSchema = (Core.Name "xmlSchema")

_Datatype_other = (Core.Name "other")

_Datatype_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Datatype"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "xmlSchema"),
      Core.fieldTypeType = Schema._Datatype_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "other"),
      Core.fieldTypeType = Syntax._Iri_type_}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Object_Properties
data ObjectProperty = 
  ObjectProperty {}
  deriving (Eq, Ord, Read, Show)

_ObjectProperty = (Core.Name "hydra/langs/owl/syntax.ObjectProperty")

_ObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data DataProperty = 
  DataProperty {}
  deriving (Eq, Ord, Read, Show)

_DataProperty = (Core.Name "hydra/langs/owl/syntax.DataProperty")

_DataProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data AnnotationProperty = 
  AnnotationProperty {}
  deriving (Eq, Ord, Read, Show)

_AnnotationProperty = (Core.Name "hydra/langs/owl/syntax.AnnotationProperty")

_AnnotationProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data Individual = 
  IndividualNamed NamedIndividual |
  IndividualAnonymous AnonymousIndividual
  deriving (Eq, Ord, Read, Show)

_Individual = (Core.Name "hydra/langs/owl/syntax.Individual")

_Individual_named = (Core.Name "named")

_Individual_anonymous = (Core.Name "anonymous")

_Individual_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Individual"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "named"),
      Core.fieldTypeType = _NamedIndividual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "anonymous"),
      Core.fieldTypeType = _AnonymousIndividual_type_}]}))

data NamedIndividual = 
  NamedIndividual {}
  deriving (Eq, Ord, Read, Show)

_NamedIndividual = (Core.Name "hydra/langs/owl/syntax.NamedIndividual")

_NamedIndividual_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data AnonymousIndividual = 
  AnonymousIndividual {}
  deriving (Eq, Ord, Read, Show)

_AnonymousIndividual = (Core.Name "hydra/langs/owl/syntax.AnonymousIndividual")

_AnonymousIndividual_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/core.Unit"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = []}))

data ObjectPropertyExpression = 
  ObjectPropertyExpressionObject ObjectProperty |
  ObjectPropertyExpressionInverseObject InverseObjectProperty
  deriving (Eq, Ord, Read, Show)

_ObjectPropertyExpression = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyExpression")

_ObjectPropertyExpression_object = (Core.Name "object")

_ObjectPropertyExpression_inverseObject = (Core.Name "inverseObject")

_ObjectPropertyExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "object"),
      Core.fieldTypeType = _ObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inverseObject"),
      Core.fieldTypeType = _InverseObjectProperty_type_}]}))

newtype InverseObjectProperty = 
  InverseObjectProperty {
    unInverseObjectProperty :: ObjectProperty}
  deriving (Eq, Ord, Read, Show)

_InverseObjectProperty = (Core.Name "hydra/langs/owl/syntax.InverseObjectProperty")

_InverseObjectProperty_type_ = _ObjectProperty_type_

newtype DataPropertyExpression = 
  DataPropertyExpression {
    unDataPropertyExpression :: DataProperty}
  deriving (Eq, Ord, Read, Show)

_DataPropertyExpression = (Core.Name "hydra/langs/owl/syntax.DataPropertyExpression")

_DataPropertyExpression_type_ = _DataProperty_type_

-- | See https://www.w3.org/TR/owl2-syntax/#Data_Ranges
data DataRange = 
  DataRangeDataComplementOf DataComplementOf |
  DataRangeDataIntersectionOf DataIntersectionOf |
  DataRangeDataOneOf DataOneOf |
  DataRangeDataUnionOf DataUnionOf |
  DataRangeDatatype Datatype |
  DataRangeDatatypeRestriction DatatypeRestriction
  deriving (Eq, Ord, Read, Show)

_DataRange = (Core.Name "hydra/langs/owl/syntax.DataRange")

_DataRange_dataComplementOf = (Core.Name "dataComplementOf")

_DataRange_dataIntersectionOf = (Core.Name "dataIntersectionOf")

_DataRange_dataOneOf = (Core.Name "dataOneOf")

_DataRange_dataUnionOf = (Core.Name "dataUnionOf")

_DataRange_datatype = (Core.Name "datatype")

_DataRange_datatypeRestriction = (Core.Name "datatypeRestriction")

_DataRange_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataComplementOf"),
      Core.fieldTypeType = _DataComplementOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataIntersectionOf"),
      Core.fieldTypeType = _DataIntersectionOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataOneOf"),
      Core.fieldTypeType = _DataOneOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataUnionOf"),
      Core.fieldTypeType = _DataUnionOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatype"),
      Core.fieldTypeType = _Datatype_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatypeRestriction"),
      Core.fieldTypeType = _DatatypeRestriction_type_}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges
newtype DataIntersectionOf = 
  DataIntersectionOf {
    unDataIntersectionOf :: [DataRange]}
  deriving (Eq, Ord, Read, Show)

_DataIntersectionOf = (Core.Name "hydra/langs/owl/syntax.DataIntersectionOf")

_DataIntersectionOf_type_ = (Core.TypeList _DataRange_type_)

-- | See https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges
newtype DataUnionOf = 
  DataUnionOf {
    unDataUnionOf :: [DataRange]}
  deriving (Eq, Ord, Read, Show)

_DataUnionOf = (Core.Name "hydra/langs/owl/syntax.DataUnionOf")

_DataUnionOf_type_ = (Core.TypeList _DataRange_type_)

-- | See https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges
newtype DataComplementOf = 
  DataComplementOf {
    unDataComplementOf :: DataRange}
  deriving (Eq, Ord, Read, Show)

_DataComplementOf = (Core.Name "hydra/langs/owl/syntax.DataComplementOf")

_DataComplementOf_type_ = _DataRange_type_

-- | See https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals
newtype DataOneOf = 
  DataOneOf {
    unDataOneOf :: [Syntax.Literal]}
  deriving (Eq, Ord, Read, Show)

_DataOneOf = (Core.Name "hydra/langs/owl/syntax.DataOneOf")

_DataOneOf_type_ = (Core.TypeList Syntax._Literal_type_)

-- | See https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions
data DatatypeRestriction = 
  DatatypeRestriction {
    datatypeRestrictionDatatype :: Datatype,
    datatypeRestrictionConstraints :: [DatatypeRestriction_Constraint]}
  deriving (Eq, Ord, Read, Show)

_DatatypeRestriction = (Core.Name "hydra/langs/owl/syntax.DatatypeRestriction")

_DatatypeRestriction_datatype = (Core.Name "datatype")

_DatatypeRestriction_constraints = (Core.Name "constraints")

_DatatypeRestriction_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DatatypeRestriction"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatype"),
      Core.fieldTypeType = _Datatype_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constraints"),
      Core.fieldTypeType = (Core.TypeList _DatatypeRestriction_Constraint_type_)}]}))

data DatatypeRestriction_Constraint = 
  DatatypeRestriction_Constraint {
    datatypeRestriction_ConstraintConstrainingFacet :: DatatypeRestriction_ConstrainingFacet,
    datatypeRestriction_ConstraintRestrictionValue :: Syntax.Literal}
  deriving (Eq, Ord, Read, Show)

_DatatypeRestriction_Constraint = (Core.Name "hydra/langs/owl/syntax.DatatypeRestriction.Constraint")

_DatatypeRestriction_Constraint_constrainingFacet = (Core.Name "constrainingFacet")

_DatatypeRestriction_Constraint_restrictionValue = (Core.Name "restrictionValue")

_DatatypeRestriction_Constraint_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DatatypeRestriction.Constraint"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "constrainingFacet"),
      Core.fieldTypeType = _DatatypeRestriction_ConstrainingFacet_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "restrictionValue"),
      Core.fieldTypeType = Syntax._Literal_type_}]}))

data DatatypeRestriction_ConstrainingFacet = 
  -- | Note: XML Schema constraining facets are treated as a special case in this model (not in the OWL 2 specification itself) because they are particularly common
  DatatypeRestriction_ConstrainingFacetXmlSchema Schema.ConstrainingFacet |
  DatatypeRestriction_ConstrainingFacetOther Syntax.Iri
  deriving (Eq, Ord, Read, Show)

_DatatypeRestriction_ConstrainingFacet = (Core.Name "hydra/langs/owl/syntax.DatatypeRestriction.ConstrainingFacet")

_DatatypeRestriction_ConstrainingFacet_xmlSchema = (Core.Name "xmlSchema")

_DatatypeRestriction_ConstrainingFacet_other = (Core.Name "other")

_DatatypeRestriction_ConstrainingFacet_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DatatypeRestriction.ConstrainingFacet"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "xmlSchema"),
      Core.fieldTypeType = Schema._ConstrainingFacet_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "other"),
      Core.fieldTypeType = Syntax._Iri_type_}]}))

data ClassExpression = 
  ClassExpressionClass Class |
  ClassExpressionDataSomeValuesFrom DataSomeValuesFrom |
  ClassExpressionDataAllValuesFrom DataAllValuesFrom |
  ClassExpressionDataHasValue DataHasValue |
  ClassExpressionDataMinCardinality DataMinCardinality |
  ClassExpressionDataMaxCardinality DataMaxCardinality |
  ClassExpressionDataExactCardinality DataExactCardinality |
  ClassExpressionObjectAllValuesFrom ObjectAllValuesFrom |
  ClassExpressionObjectExactCardinality ObjectExactCardinality |
  ClassExpressionObjectHasSelf ObjectHasSelf |
  ClassExpressionObjectHasValue ObjectHasValue |
  ClassExpressionObjectIntersectionOf ObjectIntersectionOf |
  ClassExpressionObjectMaxCardinality ObjectMaxCardinality |
  ClassExpressionObjectMinCardinality ObjectMinCardinality |
  ClassExpressionObjectOneOf ObjectOneOf |
  ClassExpressionObjectSomeValuesFrom ObjectSomeValuesFrom |
  ClassExpressionObjectUnionOf ObjectUnionOf
  deriving (Eq, Ord, Read, Show)

_ClassExpression = (Core.Name "hydra/langs/owl/syntax.ClassExpression")

_ClassExpression_class = (Core.Name "class")

_ClassExpression_dataSomeValuesFrom = (Core.Name "dataSomeValuesFrom")

_ClassExpression_dataAllValuesFrom = (Core.Name "dataAllValuesFrom")

_ClassExpression_dataHasValue = (Core.Name "dataHasValue")

_ClassExpression_dataMinCardinality = (Core.Name "dataMinCardinality")

_ClassExpression_dataMaxCardinality = (Core.Name "dataMaxCardinality")

_ClassExpression_dataExactCardinality = (Core.Name "dataExactCardinality")

_ClassExpression_objectAllValuesFrom = (Core.Name "objectAllValuesFrom")

_ClassExpression_objectExactCardinality = (Core.Name "objectExactCardinality")

_ClassExpression_objectHasSelf = (Core.Name "objectHasSelf")

_ClassExpression_objectHasValue = (Core.Name "objectHasValue")

_ClassExpression_objectIntersectionOf = (Core.Name "objectIntersectionOf")

_ClassExpression_objectMaxCardinality = (Core.Name "objectMaxCardinality")

_ClassExpression_objectMinCardinality = (Core.Name "objectMinCardinality")

_ClassExpression_objectOneOf = (Core.Name "objectOneOf")

_ClassExpression_objectSomeValuesFrom = (Core.Name "objectSomeValuesFrom")

_ClassExpression_objectUnionOf = (Core.Name "objectUnionOf")

_ClassExpression_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ClassExpression"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _Class_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataSomeValuesFrom"),
      Core.fieldTypeType = _DataSomeValuesFrom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataAllValuesFrom"),
      Core.fieldTypeType = _DataAllValuesFrom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataHasValue"),
      Core.fieldTypeType = _DataHasValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataMinCardinality"),
      Core.fieldTypeType = _DataMinCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataMaxCardinality"),
      Core.fieldTypeType = _DataMaxCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataExactCardinality"),
      Core.fieldTypeType = _DataExactCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectAllValuesFrom"),
      Core.fieldTypeType = _ObjectAllValuesFrom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectExactCardinality"),
      Core.fieldTypeType = _ObjectExactCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectHasSelf"),
      Core.fieldTypeType = _ObjectHasSelf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectHasValue"),
      Core.fieldTypeType = _ObjectHasValue_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectIntersectionOf"),
      Core.fieldTypeType = _ObjectIntersectionOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectMaxCardinality"),
      Core.fieldTypeType = _ObjectMaxCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectMinCardinality"),
      Core.fieldTypeType = _ObjectMinCardinality_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectOneOf"),
      Core.fieldTypeType = _ObjectOneOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectSomeValuesFrom"),
      Core.fieldTypeType = _ObjectSomeValuesFrom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectUnionOf"),
      Core.fieldTypeType = _ObjectUnionOf_type_}]}))

newtype ObjectIntersectionOf = 
  ObjectIntersectionOf {
    unObjectIntersectionOf :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_ObjectIntersectionOf = (Core.Name "hydra/langs/owl/syntax.ObjectIntersectionOf")

_ObjectIntersectionOf_type_ = (Core.TypeList _ClassExpression_type_)

newtype ObjectUnionOf = 
  ObjectUnionOf {
    unObjectUnionOf :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_ObjectUnionOf = (Core.Name "hydra/langs/owl/syntax.ObjectUnionOf")

_ObjectUnionOf_type_ = (Core.TypeList _ClassExpression_type_)

newtype ObjectComplementOf = 
  ObjectComplementOf {
    unObjectComplementOf :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_ObjectComplementOf = (Core.Name "hydra/langs/owl/syntax.ObjectComplementOf")

_ObjectComplementOf_type_ = _ClassExpression_type_

newtype ObjectOneOf = 
  ObjectOneOf {
    unObjectOneOf :: [Individual]}
  deriving (Eq, Ord, Read, Show)

_ObjectOneOf = (Core.Name "hydra/langs/owl/syntax.ObjectOneOf")

_ObjectOneOf_type_ = (Core.TypeList _Individual_type_)

data ObjectSomeValuesFrom = 
  ObjectSomeValuesFrom {
    objectSomeValuesFromProperty :: ObjectPropertyExpression,
    objectSomeValuesFromClass :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_ObjectSomeValuesFrom = (Core.Name "hydra/langs/owl/syntax.ObjectSomeValuesFrom")

_ObjectSomeValuesFrom_property = (Core.Name "property")

_ObjectSomeValuesFrom_class = (Core.Name "class")

_ObjectSomeValuesFrom_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectSomeValuesFrom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

data ObjectAllValuesFrom = 
  ObjectAllValuesFrom {
    objectAllValuesFromProperty :: ObjectPropertyExpression,
    objectAllValuesFromClass :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_ObjectAllValuesFrom = (Core.Name "hydra/langs/owl/syntax.ObjectAllValuesFrom")

_ObjectAllValuesFrom_property = (Core.Name "property")

_ObjectAllValuesFrom_class = (Core.Name "class")

_ObjectAllValuesFrom_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectAllValuesFrom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

data ObjectHasValue = 
  ObjectHasValue {
    objectHasValueProperty :: ObjectPropertyExpression,
    objectHasValueIndividual :: Individual}
  deriving (Eq, Ord, Read, Show)

_ObjectHasValue = (Core.Name "hydra/langs/owl/syntax.ObjectHasValue")

_ObjectHasValue_property = (Core.Name "property")

_ObjectHasValue_individual = (Core.Name "individual")

_ObjectHasValue_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectHasValue"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "individual"),
      Core.fieldTypeType = _Individual_type_}]}))

newtype ObjectHasSelf = 
  ObjectHasSelf {
    unObjectHasSelf :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_ObjectHasSelf = (Core.Name "hydra/langs/owl/syntax.ObjectHasSelf")

_ObjectHasSelf_type_ = _ObjectPropertyExpression_type_

-- | See https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality
data ObjectMinCardinality = 
  ObjectMinCardinality {
    objectMinCardinalityBound :: Integer,
    objectMinCardinalityProperty :: ObjectPropertyExpression,
    objectMinCardinalityClass :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_ObjectMinCardinality = (Core.Name "hydra/langs/owl/syntax.ObjectMinCardinality")

_ObjectMinCardinality_bound = (Core.Name "bound")

_ObjectMinCardinality_property = (Core.Name "property")

_ObjectMinCardinality_class = (Core.Name "class")

_ObjectMinCardinality_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectMinCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = (Core.TypeList _ClassExpression_type_)}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality
data ObjectMaxCardinality = 
  ObjectMaxCardinality {
    objectMaxCardinalityBound :: Integer,
    objectMaxCardinalityProperty :: ObjectPropertyExpression,
    objectMaxCardinalityClass :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_ObjectMaxCardinality = (Core.Name "hydra/langs/owl/syntax.ObjectMaxCardinality")

_ObjectMaxCardinality_bound = (Core.Name "bound")

_ObjectMaxCardinality_property = (Core.Name "property")

_ObjectMaxCardinality_class = (Core.Name "class")

_ObjectMaxCardinality_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectMaxCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = (Core.TypeList _ClassExpression_type_)}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality
data ObjectExactCardinality = 
  ObjectExactCardinality {
    objectExactCardinalityBound :: Integer,
    objectExactCardinalityProperty :: ObjectPropertyExpression,
    objectExactCardinalityClass :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_ObjectExactCardinality = (Core.Name "hydra/langs/owl/syntax.ObjectExactCardinality")

_ObjectExactCardinality_bound = (Core.Name "bound")

_ObjectExactCardinality_property = (Core.Name "property")

_ObjectExactCardinality_class = (Core.Name "class")

_ObjectExactCardinality_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectExactCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = (Core.TypeList _ClassExpression_type_)}]}))

data DataSomeValuesFrom = 
  DataSomeValuesFrom {
    dataSomeValuesFromProperty :: [DataPropertyExpression],
    dataSomeValuesFromRange :: DataRange}
  deriving (Eq, Ord, Read, Show)

_DataSomeValuesFrom = (Core.Name "hydra/langs/owl/syntax.DataSomeValuesFrom")

_DataSomeValuesFrom_property = (Core.Name "property")

_DataSomeValuesFrom_range = (Core.Name "range")

_DataSomeValuesFrom_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataSomeValuesFrom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = (Core.TypeList _DataPropertyExpression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _DataRange_type_}]}))

data DataAllValuesFrom = 
  DataAllValuesFrom {
    dataAllValuesFromProperty :: [DataPropertyExpression],
    dataAllValuesFromRange :: DataRange}
  deriving (Eq, Ord, Read, Show)

_DataAllValuesFrom = (Core.Name "hydra/langs/owl/syntax.DataAllValuesFrom")

_DataAllValuesFrom_property = (Core.Name "property")

_DataAllValuesFrom_range = (Core.Name "range")

_DataAllValuesFrom_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataAllValuesFrom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = (Core.TypeList _DataPropertyExpression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _DataRange_type_}]}))

data DataHasValue = 
  DataHasValue {
    dataHasValueProperty :: DataPropertyExpression,
    dataHasValueValue :: Syntax.Literal}
  deriving (Eq, Ord, Read, Show)

_DataHasValue = (Core.Name "hydra/langs/owl/syntax.DataHasValue")

_DataHasValue_property = (Core.Name "property")

_DataHasValue_value = (Core.Name "value")

_DataHasValue_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataHasValue"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "value"),
      Core.fieldTypeType = Syntax._Literal_type_}]}))

data DataMinCardinality = 
  DataMinCardinality {
    dataMinCardinalityBound :: Integer,
    dataMinCardinalityProperty :: DataPropertyExpression,
    dataMinCardinalityRange :: [DataRange]}
  deriving (Eq, Ord, Read, Show)

_DataMinCardinality = (Core.Name "hydra/langs/owl/syntax.DataMinCardinality")

_DataMinCardinality_bound = (Core.Name "bound")

_DataMinCardinality_property = (Core.Name "property")

_DataMinCardinality_range = (Core.Name "range")

_DataMinCardinality_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataMinCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = (Core.TypeList _DataRange_type_)}]}))

data DataMaxCardinality = 
  DataMaxCardinality {
    dataMaxCardinalityBound :: Integer,
    dataMaxCardinalityProperty :: DataPropertyExpression,
    dataMaxCardinalityRange :: [DataRange]}
  deriving (Eq, Ord, Read, Show)

_DataMaxCardinality = (Core.Name "hydra/langs/owl/syntax.DataMaxCardinality")

_DataMaxCardinality_bound = (Core.Name "bound")

_DataMaxCardinality_property = (Core.Name "property")

_DataMaxCardinality_range = (Core.Name "range")

_DataMaxCardinality_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataMaxCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = (Core.TypeList _DataRange_type_)}]}))

data DataExactCardinality = 
  DataExactCardinality {
    dataExactCardinalityBound :: Integer,
    dataExactCardinalityProperty :: DataPropertyExpression,
    dataExactCardinalityRange :: [DataRange]}
  deriving (Eq, Ord, Read, Show)

_DataExactCardinality = (Core.Name "hydra/langs/owl/syntax.DataExactCardinality")

_DataExactCardinality_bound = (Core.Name "bound")

_DataExactCardinality_property = (Core.Name "property")

_DataExactCardinality_range = (Core.Name "range")

_DataExactCardinality_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataExactCardinality"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "bound"),
      Core.fieldTypeType = (Core.TypeLiteral (Core.LiteralTypeInteger Core.IntegerTypeBigint))},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = (Core.TypeList _DataRange_type_)}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Axioms
data Axiom = 
  AxiomAnnotationAxiom AnnotationAxiom |
  AxiomAssertion Assertion |
  AxiomClassAxiom ClassAxiom |
  AxiomDataPropertyAxiom DataPropertyAxiom |
  AxiomDatatypeDefinition DatatypeDefinition |
  AxiomDeclaration Declaration |
  AxiomHasKey HasKey |
  AxiomObjectPropertyAxiom ObjectPropertyAxiom
  deriving (Eq, Ord, Read, Show)

_Axiom = (Core.Name "hydra/langs/owl/syntax.Axiom")

_Axiom_annotationAxiom = (Core.Name "annotationAxiom")

_Axiom_assertion = (Core.Name "assertion")

_Axiom_classAxiom = (Core.Name "classAxiom")

_Axiom_dataPropertyAxiom = (Core.Name "dataPropertyAxiom")

_Axiom_datatypeDefinition = (Core.Name "datatypeDefinition")

_Axiom_declaration = (Core.Name "declaration")

_Axiom_hasKey = (Core.Name "hasKey")

_Axiom_objectPropertyAxiom = (Core.Name "objectPropertyAxiom")

_Axiom_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Axiom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotationAxiom"),
      Core.fieldTypeType = _AnnotationAxiom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "assertion"),
      Core.fieldTypeType = _Assertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classAxiom"),
      Core.fieldTypeType = _ClassAxiom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataPropertyAxiom"),
      Core.fieldTypeType = _DataPropertyAxiom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatypeDefinition"),
      Core.fieldTypeType = _DatatypeDefinition_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "declaration"),
      Core.fieldTypeType = _Declaration_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "hasKey"),
      Core.fieldTypeType = _HasKey_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectPropertyAxiom"),
      Core.fieldTypeType = _ObjectPropertyAxiom_type_}]}))

data ClassAxiom = 
  ClassAxiomDisjointClasses DisjointClasses |
  ClassAxiomDisjointUnion DisjointUnion |
  ClassAxiomEquivalentClasses EquivalentClasses |
  ClassAxiomSubClassOf SubClassOf
  deriving (Eq, Ord, Read, Show)

_ClassAxiom = (Core.Name "hydra/langs/owl/syntax.ClassAxiom")

_ClassAxiom_disjointClasses = (Core.Name "disjointClasses")

_ClassAxiom_disjointUnion = (Core.Name "disjointUnion")

_ClassAxiom_equivalentClasses = (Core.Name "equivalentClasses")

_ClassAxiom_subClassOf = (Core.Name "subClassOf")

_ClassAxiom_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ClassAxiom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "disjointClasses"),
      Core.fieldTypeType = _DisjointClasses_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "disjointUnion"),
      Core.fieldTypeType = _DisjointUnion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equivalentClasses"),
      Core.fieldTypeType = _EquivalentClasses_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subClassOf"),
      Core.fieldTypeType = _SubClassOf_type_}]}))

data SubClassOf = 
  SubClassOf {
    subClassOfAnnotations :: [Annotation],
    subClassOfSubClass :: ClassExpression,
    subClassOfSuperClass :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_SubClassOf = (Core.Name "hydra/langs/owl/syntax.SubClassOf")

_SubClassOf_annotations = (Core.Name "annotations")

_SubClassOf_subClass = (Core.Name "subClass")

_SubClassOf_superClass = (Core.Name "superClass")

_SubClassOf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.SubClassOf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subClass"),
      Core.fieldTypeType = _ClassExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "superClass"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

data EquivalentClasses = 
  EquivalentClasses {
    equivalentClassesAnnotations :: [Annotation],
    equivalentClassesClasses :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_EquivalentClasses = (Core.Name "hydra/langs/owl/syntax.EquivalentClasses")

_EquivalentClasses_annotations = (Core.Name "annotations")

_EquivalentClasses_classes = (Core.Name "classes")

_EquivalentClasses_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.EquivalentClasses"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classes"),
      Core.fieldTypeType = (Core.TypeList _ClassExpression_type_)}]}))

data DisjointClasses = 
  DisjointClasses {
    disjointClassesAnnotations :: [Annotation],
    disjointClassesClasses :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_DisjointClasses = (Core.Name "hydra/langs/owl/syntax.DisjointClasses")

_DisjointClasses_annotations = (Core.Name "annotations")

_DisjointClasses_classes = (Core.Name "classes")

_DisjointClasses_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DisjointClasses"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classes"),
      Core.fieldTypeType = (Core.TypeList _ClassExpression_type_)}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions
data DisjointUnion = 
  DisjointUnion {
    disjointUnionAnnotations :: [Annotation],
    disjointUnionClass :: Class,
    disjointUnionClasses :: [ClassExpression]}
  deriving (Eq, Ord, Read, Show)

_DisjointUnion = (Core.Name "hydra/langs/owl/syntax.DisjointUnion")

_DisjointUnion_annotations = (Core.Name "annotations")

_DisjointUnion_class = (Core.Name "class")

_DisjointUnion_classes = (Core.Name "classes")

_DisjointUnion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DisjointUnion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _Class_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classes"),
      Core.fieldTypeType = (Core.TypeList _ClassExpression_type_)}]}))

data ObjectPropertyAxiom = 
  ObjectPropertyAxiomAsymmetricObjectProperty AsymmetricObjectProperty |
  ObjectPropertyAxiomDisjointObjectProperties DisjointObjectProperties |
  ObjectPropertyAxiomEquivalentObjectProperties EquivalentObjectProperties |
  ObjectPropertyAxiomFunctionalObjectProperty FunctionalObjectProperty |
  ObjectPropertyAxiomInverseFunctionalObjectProperty InverseFunctionalObjectProperty |
  ObjectPropertyAxiomInverseObjectProperties InverseObjectProperties |
  ObjectPropertyAxiomIrreflexiveObjectProperty IrreflexiveObjectProperty |
  ObjectPropertyAxiomObjectPropertyDomain ObjectPropertyDomain |
  ObjectPropertyAxiomObjectPropertyRange ObjectPropertyRange |
  ObjectPropertyAxiomReflexiveObjectProperty ReflexiveObjectProperty |
  ObjectPropertyAxiomSubObjectPropertyOf SubObjectPropertyOf |
  ObjectPropertyAxiomSymmetricObjectProperty SymmetricObjectProperty |
  ObjectPropertyAxiomTransitiveObjectProperty TransitiveObjectProperty
  deriving (Eq, Ord, Read, Show)

_ObjectPropertyAxiom = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyAxiom")

_ObjectPropertyAxiom_asymmetricObjectProperty = (Core.Name "asymmetricObjectProperty")

_ObjectPropertyAxiom_disjointObjectProperties = (Core.Name "disjointObjectProperties")

_ObjectPropertyAxiom_equivalentObjectProperties = (Core.Name "equivalentObjectProperties")

_ObjectPropertyAxiom_functionalObjectProperty = (Core.Name "functionalObjectProperty")

_ObjectPropertyAxiom_inverseFunctionalObjectProperty = (Core.Name "inverseFunctionalObjectProperty")

_ObjectPropertyAxiom_inverseObjectProperties = (Core.Name "inverseObjectProperties")

_ObjectPropertyAxiom_irreflexiveObjectProperty = (Core.Name "irreflexiveObjectProperty")

_ObjectPropertyAxiom_objectPropertyDomain = (Core.Name "objectPropertyDomain")

_ObjectPropertyAxiom_objectPropertyRange = (Core.Name "objectPropertyRange")

_ObjectPropertyAxiom_reflexiveObjectProperty = (Core.Name "reflexiveObjectProperty")

_ObjectPropertyAxiom_subObjectPropertyOf = (Core.Name "subObjectPropertyOf")

_ObjectPropertyAxiom_symmetricObjectProperty = (Core.Name "symmetricObjectProperty")

_ObjectPropertyAxiom_transitiveObjectProperty = (Core.Name "transitiveObjectProperty")

_ObjectPropertyAxiom_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyAxiom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "asymmetricObjectProperty"),
      Core.fieldTypeType = _AsymmetricObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "disjointObjectProperties"),
      Core.fieldTypeType = _DisjointObjectProperties_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equivalentObjectProperties"),
      Core.fieldTypeType = _EquivalentObjectProperties_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "functionalObjectProperty"),
      Core.fieldTypeType = _FunctionalObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inverseFunctionalObjectProperty"),
      Core.fieldTypeType = _InverseFunctionalObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "inverseObjectProperties"),
      Core.fieldTypeType = _InverseObjectProperties_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "irreflexiveObjectProperty"),
      Core.fieldTypeType = _IrreflexiveObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectPropertyDomain"),
      Core.fieldTypeType = _ObjectPropertyDomain_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectPropertyRange"),
      Core.fieldTypeType = _ObjectPropertyRange_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "reflexiveObjectProperty"),
      Core.fieldTypeType = _ReflexiveObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subObjectPropertyOf"),
      Core.fieldTypeType = _SubObjectPropertyOf_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "symmetricObjectProperty"),
      Core.fieldTypeType = _SymmetricObjectProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "transitiveObjectProperty"),
      Core.fieldTypeType = _TransitiveObjectProperty_type_}]}))

data SubObjectPropertyOf = 
  SubObjectPropertyOf {
    subObjectPropertyOfAnnotations :: [Annotation],
    subObjectPropertyOfSubProperty :: [ObjectPropertyExpression],
    subObjectPropertyOfSuperProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_SubObjectPropertyOf = (Core.Name "hydra/langs/owl/syntax.SubObjectPropertyOf")

_SubObjectPropertyOf_annotations = (Core.Name "annotations")

_SubObjectPropertyOf_subProperty = (Core.Name "subProperty")

_SubObjectPropertyOf_superProperty = (Core.Name "superProperty")

_SubObjectPropertyOf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.SubObjectPropertyOf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subProperty"),
      Core.fieldTypeType = (Core.TypeList _ObjectPropertyExpression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "superProperty"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data EquivalentObjectProperties = 
  EquivalentObjectProperties {
    equivalentObjectPropertiesAnnotations :: [Annotation],
    equivalentObjectPropertiesProperties :: [ObjectPropertyExpression]}
  deriving (Eq, Ord, Read, Show)

_EquivalentObjectProperties = (Core.Name "hydra/langs/owl/syntax.EquivalentObjectProperties")

_EquivalentObjectProperties_annotations = (Core.Name "annotations")

_EquivalentObjectProperties_properties = (Core.Name "properties")

_EquivalentObjectProperties_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.EquivalentObjectProperties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _ObjectPropertyExpression_type_)}]}))

data DisjointObjectProperties = 
  DisjointObjectProperties {
    disjointObjectPropertiesAnnotations :: [Annotation],
    disjointObjectPropertiesProperties :: [ObjectPropertyExpression]}
  deriving (Eq, Ord, Read, Show)

_DisjointObjectProperties = (Core.Name "hydra/langs/owl/syntax.DisjointObjectProperties")

_DisjointObjectProperties_annotations = (Core.Name "annotations")

_DisjointObjectProperties_properties = (Core.Name "properties")

_DisjointObjectProperties_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DisjointObjectProperties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _ObjectPropertyExpression_type_)}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain
data ObjectPropertyDomain = 
  ObjectPropertyDomain {
    objectPropertyDomainAnnotations :: [Annotation],
    objectPropertyDomainProperty :: ObjectPropertyExpression,
    objectPropertyDomainDomain :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_ObjectPropertyDomain = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyDomain")

_ObjectPropertyDomain_annotations = (Core.Name "annotations")

_ObjectPropertyDomain_property = (Core.Name "property")

_ObjectPropertyDomain_domain = (Core.Name "domain")

_ObjectPropertyDomain_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyDomain"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "domain"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Object_Property_Range
data ObjectPropertyRange = 
  ObjectPropertyRange {
    objectPropertyRangeAnnotations :: [Annotation],
    objectPropertyRangeProperty :: ObjectPropertyExpression,
    objectPropertyRangeRange :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_ObjectPropertyRange = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyRange")

_ObjectPropertyRange_annotations = (Core.Name "annotations")

_ObjectPropertyRange_property = (Core.Name "property")

_ObjectPropertyRange_range = (Core.Name "range")

_ObjectPropertyRange_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

data InverseObjectProperties = 
  InverseObjectProperties {
    inverseObjectPropertiesAnnotations :: [Annotation],
    inverseObjectPropertiesProperty1 :: ObjectPropertyExpression,
    inverseObjectPropertiesProperty2 :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_InverseObjectProperties = (Core.Name "hydra/langs/owl/syntax.InverseObjectProperties")

_InverseObjectProperties_annotations = (Core.Name "annotations")

_InverseObjectProperties_property1 = (Core.Name "property1")

_InverseObjectProperties_property2 = (Core.Name "property2")

_InverseObjectProperties_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.InverseObjectProperties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property1"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property2"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data FunctionalObjectProperty = 
  FunctionalObjectProperty {
    functionalObjectPropertyAnnotations :: [Annotation],
    functionalObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_FunctionalObjectProperty = (Core.Name "hydra/langs/owl/syntax.FunctionalObjectProperty")

_FunctionalObjectProperty_annotations = (Core.Name "annotations")

_FunctionalObjectProperty_property = (Core.Name "property")

_FunctionalObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.FunctionalObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data InverseFunctionalObjectProperty = 
  InverseFunctionalObjectProperty {
    inverseFunctionalObjectPropertyAnnotations :: [Annotation],
    inverseFunctionalObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_InverseFunctionalObjectProperty = (Core.Name "hydra/langs/owl/syntax.InverseFunctionalObjectProperty")

_InverseFunctionalObjectProperty_annotations = (Core.Name "annotations")

_InverseFunctionalObjectProperty_property = (Core.Name "property")

_InverseFunctionalObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.InverseFunctionalObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data ReflexiveObjectProperty = 
  ReflexiveObjectProperty {
    reflexiveObjectPropertyAnnotations :: [Annotation],
    reflexiveObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_ReflexiveObjectProperty = (Core.Name "hydra/langs/owl/syntax.ReflexiveObjectProperty")

_ReflexiveObjectProperty_annotations = (Core.Name "annotations")

_ReflexiveObjectProperty_property = (Core.Name "property")

_ReflexiveObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ReflexiveObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data IrreflexiveObjectProperty = 
  IrreflexiveObjectProperty {
    irreflexiveObjectPropertyAnnotations :: [Annotation],
    irreflexiveObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_IrreflexiveObjectProperty = (Core.Name "hydra/langs/owl/syntax.IrreflexiveObjectProperty")

_IrreflexiveObjectProperty_annotations = (Core.Name "annotations")

_IrreflexiveObjectProperty_property = (Core.Name "property")

_IrreflexiveObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.IrreflexiveObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data SymmetricObjectProperty = 
  SymmetricObjectProperty {
    symmetricObjectPropertyAnnotations :: [Annotation],
    symmetricObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_SymmetricObjectProperty = (Core.Name "hydra/langs/owl/syntax.SymmetricObjectProperty")

_SymmetricObjectProperty_annotations = (Core.Name "annotations")

_SymmetricObjectProperty_property = (Core.Name "property")

_SymmetricObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.SymmetricObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data AsymmetricObjectProperty = 
  AsymmetricObjectProperty {
    asymmetricObjectPropertyAnnotations :: [Annotation],
    asymmetricObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_AsymmetricObjectProperty = (Core.Name "hydra/langs/owl/syntax.AsymmetricObjectProperty")

_AsymmetricObjectProperty_annotations = (Core.Name "annotations")

_AsymmetricObjectProperty_property = (Core.Name "property")

_AsymmetricObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.AsymmetricObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data TransitiveObjectProperty = 
  TransitiveObjectProperty {
    transitiveObjectPropertyAnnotations :: [Annotation],
    transitiveObjectPropertyProperty :: ObjectPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_TransitiveObjectProperty = (Core.Name "hydra/langs/owl/syntax.TransitiveObjectProperty")

_TransitiveObjectProperty_annotations = (Core.Name "annotations")

_TransitiveObjectProperty_property = (Core.Name "property")

_TransitiveObjectProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.TransitiveObjectProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_}]}))

data DataPropertyAxiom = 
  DataPropertyAxiomDataPropertyAxiom DataPropertyAxiom |
  DataPropertyAxiomDataPropertyRange DataPropertyRange |
  DataPropertyAxiomDisjointDataProperties DisjointDataProperties |
  DataPropertyAxiomEquivalentDataProperties EquivalentDataProperties |
  DataPropertyAxiomFunctionalDataProperty FunctionalDataProperty |
  DataPropertyAxiomSubDataPropertyOf SubDataPropertyOf
  deriving (Eq, Ord, Read, Show)

_DataPropertyAxiom = (Core.Name "hydra/langs/owl/syntax.DataPropertyAxiom")

_DataPropertyAxiom_dataPropertyAxiom = (Core.Name "dataPropertyAxiom")

_DataPropertyAxiom_dataPropertyRange = (Core.Name "dataPropertyRange")

_DataPropertyAxiom_disjointDataProperties = (Core.Name "disjointDataProperties")

_DataPropertyAxiom_equivalentDataProperties = (Core.Name "equivalentDataProperties")

_DataPropertyAxiom_functionalDataProperty = (Core.Name "functionalDataProperty")

_DataPropertyAxiom_subDataPropertyOf = (Core.Name "subDataPropertyOf")

_DataPropertyAxiom_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataPropertyAxiom"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataPropertyAxiom"),
      Core.fieldTypeType = _DataPropertyAxiom_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataPropertyRange"),
      Core.fieldTypeType = _DataPropertyRange_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "disjointDataProperties"),
      Core.fieldTypeType = _DisjointDataProperties_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "equivalentDataProperties"),
      Core.fieldTypeType = _EquivalentDataProperties_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "functionalDataProperty"),
      Core.fieldTypeType = _FunctionalDataProperty_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subDataPropertyOf"),
      Core.fieldTypeType = _SubDataPropertyOf_type_}]}))

data SubDataPropertyOf = 
  SubDataPropertyOf {
    subDataPropertyOfAnnotations :: [Annotation],
    subDataPropertyOfSubProperty :: DataPropertyExpression,
    subDataPropertyOfSuperProperty :: DataPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_SubDataPropertyOf = (Core.Name "hydra/langs/owl/syntax.SubDataPropertyOf")

_SubDataPropertyOf_annotations = (Core.Name "annotations")

_SubDataPropertyOf_subProperty = (Core.Name "subProperty")

_SubDataPropertyOf_superProperty = (Core.Name "superProperty")

_SubDataPropertyOf_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.SubDataPropertyOf"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "subProperty"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "superProperty"),
      Core.fieldTypeType = _DataPropertyExpression_type_}]}))

data EquivalentDataProperties = 
  EquivalentDataProperties {
    equivalentDataPropertiesAnnotations :: [Annotation],
    equivalentDataPropertiesProperties :: [DataPropertyExpression]}
  deriving (Eq, Ord, Read, Show)

_EquivalentDataProperties = (Core.Name "hydra/langs/owl/syntax.EquivalentDataProperties")

_EquivalentDataProperties_annotations = (Core.Name "annotations")

_EquivalentDataProperties_properties = (Core.Name "properties")

_EquivalentDataProperties_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.EquivalentDataProperties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _DataPropertyExpression_type_)}]}))

data DisjointDataProperties = 
  DisjointDataProperties {
    disjointDataPropertiesAnnotations :: [Annotation],
    disjointDataPropertiesProperties :: [DataPropertyExpression]}
  deriving (Eq, Ord, Read, Show)

_DisjointDataProperties = (Core.Name "hydra/langs/owl/syntax.DisjointDataProperties")

_DisjointDataProperties_annotations = (Core.Name "annotations")

_DisjointDataProperties_properties = (Core.Name "properties")

_DisjointDataProperties_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DisjointDataProperties"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "properties"),
      Core.fieldTypeType = (Core.TypeList _DataPropertyExpression_type_)}]}))

data DataPropertyDomain = 
  DataPropertyDomain {
    dataPropertyDomainAnnotations :: [Annotation],
    dataPropertyDomainProperty :: DataPropertyExpression,
    dataPropertyDomainDomain :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_DataPropertyDomain = (Core.Name "hydra/langs/owl/syntax.DataPropertyDomain")

_DataPropertyDomain_annotations = (Core.Name "annotations")

_DataPropertyDomain_property = (Core.Name "property")

_DataPropertyDomain_domain = (Core.Name "domain")

_DataPropertyDomain_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataPropertyDomain"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "domain"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

data DataPropertyRange = 
  DataPropertyRange {
    dataPropertyRangeAnnotations :: [Annotation],
    dataPropertyRangeProperty :: DataPropertyExpression,
    dataPropertyRangeRange :: ClassExpression}
  deriving (Eq, Ord, Read, Show)

_DataPropertyRange = (Core.Name "hydra/langs/owl/syntax.DataPropertyRange")

_DataPropertyRange_annotations = (Core.Name "annotations")

_DataPropertyRange_property = (Core.Name "property")

_DataPropertyRange_range = (Core.Name "range")

_DataPropertyRange_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataPropertyRange"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _ClassExpression_type_}]}))

data FunctionalDataProperty = 
  FunctionalDataProperty {
    functionalDataPropertyAnnotations :: [Annotation],
    functionalDataPropertyProperty :: DataPropertyExpression}
  deriving (Eq, Ord, Read, Show)

_FunctionalDataProperty = (Core.Name "hydra/langs/owl/syntax.FunctionalDataProperty")

_FunctionalDataProperty_annotations = (Core.Name "annotations")

_FunctionalDataProperty_property = (Core.Name "property")

_FunctionalDataProperty_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.FunctionalDataProperty"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_}]}))

data DatatypeDefinition = 
  DatatypeDefinition {
    datatypeDefinitionAnnotations :: [Annotation],
    datatypeDefinitionDatatype :: Datatype,
    datatypeDefinitionRange :: DataRange}
  deriving (Eq, Ord, Read, Show)

_DatatypeDefinition = (Core.Name "hydra/langs/owl/syntax.DatatypeDefinition")

_DatatypeDefinition_annotations = (Core.Name "annotations")

_DatatypeDefinition_datatype = (Core.Name "datatype")

_DatatypeDefinition_range = (Core.Name "range")

_DatatypeDefinition_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DatatypeDefinition"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "datatype"),
      Core.fieldTypeType = _Datatype_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "range"),
      Core.fieldTypeType = _DataRange_type_}]}))

-- | See https://www.w3.org/TR/owl2-syntax/#Keys
data HasKey = 
  HasKey {
    hasKeyAnnotations :: [Annotation],
    hasKeyClass :: ClassExpression,
    hasKeyObjectProperties :: [ObjectPropertyExpression],
    hasKeyDataProperties :: [DataPropertyExpression]}
  deriving (Eq, Ord, Read, Show)

_HasKey = (Core.Name "hydra/langs/owl/syntax.HasKey")

_HasKey_annotations = (Core.Name "annotations")

_HasKey_class = (Core.Name "class")

_HasKey_objectProperties = (Core.Name "objectProperties")

_HasKey_dataProperties = (Core.Name "dataProperties")

_HasKey_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.HasKey"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectProperties"),
      Core.fieldTypeType = (Core.TypeList _ObjectPropertyExpression_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataProperties"),
      Core.fieldTypeType = (Core.TypeList _DataPropertyExpression_type_)}]}))

data Assertion = 
  AssertionClassAssertion ClassAssertion |
  AssertionDataPropertyAssertion DataPropertyAssertion |
  AssertionDifferentIndividuals DifferentIndividuals |
  AssertionObjectPropertyAssertion ObjectPropertyAssertion |
  AssertionNegativeDataPropertyAssertion NegativeDataPropertyAssertion |
  AssertionNegativeObjectPropertyAssertion NegativeObjectPropertyAssertion |
  AssertionSameIndividual SameIndividual
  deriving (Eq, Ord, Read, Show)

_Assertion = (Core.Name "hydra/langs/owl/syntax.Assertion")

_Assertion_classAssertion = (Core.Name "classAssertion")

_Assertion_dataPropertyAssertion = (Core.Name "dataPropertyAssertion")

_Assertion_differentIndividuals = (Core.Name "differentIndividuals")

_Assertion_objectPropertyAssertion = (Core.Name "objectPropertyAssertion")

_Assertion_negativeDataPropertyAssertion = (Core.Name "negativeDataPropertyAssertion")

_Assertion_negativeObjectPropertyAssertion = (Core.Name "negativeObjectPropertyAssertion")

_Assertion_sameIndividual = (Core.Name "sameIndividual")

_Assertion_type_ = (Core.TypeUnion (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.Assertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "classAssertion"),
      Core.fieldTypeType = _ClassAssertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "dataPropertyAssertion"),
      Core.fieldTypeType = _DataPropertyAssertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "differentIndividuals"),
      Core.fieldTypeType = _DifferentIndividuals_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "objectPropertyAssertion"),
      Core.fieldTypeType = _ObjectPropertyAssertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "negativeDataPropertyAssertion"),
      Core.fieldTypeType = _NegativeDataPropertyAssertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "negativeObjectPropertyAssertion"),
      Core.fieldTypeType = _NegativeObjectPropertyAssertion_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "sameIndividual"),
      Core.fieldTypeType = _SameIndividual_type_}]}))

data SameIndividual = 
  SameIndividual {
    sameIndividualAnnotations :: [Annotation],
    sameIndividualIndividuals :: [Individual]}
  deriving (Eq, Ord, Read, Show)

_SameIndividual = (Core.Name "hydra/langs/owl/syntax.SameIndividual")

_SameIndividual_annotations = (Core.Name "annotations")

_SameIndividual_individuals = (Core.Name "individuals")

_SameIndividual_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.SameIndividual"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "individuals"),
      Core.fieldTypeType = (Core.TypeList _Individual_type_)}]}))

data DifferentIndividuals = 
  DifferentIndividuals {
    differentIndividualsAnnotations :: [Annotation],
    differentIndividualsIndividuals :: [Individual]}
  deriving (Eq, Ord, Read, Show)

_DifferentIndividuals = (Core.Name "hydra/langs/owl/syntax.DifferentIndividuals")

_DifferentIndividuals_annotations = (Core.Name "annotations")

_DifferentIndividuals_individuals = (Core.Name "individuals")

_DifferentIndividuals_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DifferentIndividuals"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "individuals"),
      Core.fieldTypeType = (Core.TypeList _Individual_type_)}]}))

data ClassAssertion = 
  ClassAssertion {
    classAssertionAnnotations :: [Annotation],
    classAssertionClass :: ClassExpression,
    classAssertionIndividual :: Individual}
  deriving (Eq, Ord, Read, Show)

_ClassAssertion = (Core.Name "hydra/langs/owl/syntax.ClassAssertion")

_ClassAssertion_annotations = (Core.Name "annotations")

_ClassAssertion_class = (Core.Name "class")

_ClassAssertion_individual = (Core.Name "individual")

_ClassAssertion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ClassAssertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "class"),
      Core.fieldTypeType = _ClassExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "individual"),
      Core.fieldTypeType = _Individual_type_}]}))

data ObjectPropertyAssertion = 
  ObjectPropertyAssertion {
    objectPropertyAssertionAnnotations :: [Annotation],
    objectPropertyAssertionProperty :: ObjectPropertyExpression,
    objectPropertyAssertionSource :: Individual,
    objectPropertyAssertionTarget :: Individual}
  deriving (Eq, Ord, Read, Show)

_ObjectPropertyAssertion = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyAssertion")

_ObjectPropertyAssertion_annotations = (Core.Name "annotations")

_ObjectPropertyAssertion_property = (Core.Name "property")

_ObjectPropertyAssertion_source = (Core.Name "source")

_ObjectPropertyAssertion_target = (Core.Name "target")

_ObjectPropertyAssertion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.ObjectPropertyAssertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _Individual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "target"),
      Core.fieldTypeType = _Individual_type_}]}))

data NegativeObjectPropertyAssertion = 
  NegativeObjectPropertyAssertion {
    negativeObjectPropertyAssertionAnnotations :: [Annotation],
    negativeObjectPropertyAssertionProperty :: ObjectPropertyExpression,
    negativeObjectPropertyAssertionSource :: Individual,
    negativeObjectPropertyAssertionTarget :: Individual}
  deriving (Eq, Ord, Read, Show)

_NegativeObjectPropertyAssertion = (Core.Name "hydra/langs/owl/syntax.NegativeObjectPropertyAssertion")

_NegativeObjectPropertyAssertion_annotations = (Core.Name "annotations")

_NegativeObjectPropertyAssertion_property = (Core.Name "property")

_NegativeObjectPropertyAssertion_source = (Core.Name "source")

_NegativeObjectPropertyAssertion_target = (Core.Name "target")

_NegativeObjectPropertyAssertion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.NegativeObjectPropertyAssertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _ObjectPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _Individual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "target"),
      Core.fieldTypeType = _Individual_type_}]}))

data DataPropertyAssertion = 
  DataPropertyAssertion {
    dataPropertyAssertionAnnotations :: [Annotation],
    dataPropertyAssertionProperty :: DataPropertyExpression,
    dataPropertyAssertionSource :: Individual,
    dataPropertyAssertionTarget :: Individual}
  deriving (Eq, Ord, Read, Show)

_DataPropertyAssertion = (Core.Name "hydra/langs/owl/syntax.DataPropertyAssertion")

_DataPropertyAssertion_annotations = (Core.Name "annotations")

_DataPropertyAssertion_property = (Core.Name "property")

_DataPropertyAssertion_source = (Core.Name "source")

_DataPropertyAssertion_target = (Core.Name "target")

_DataPropertyAssertion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.DataPropertyAssertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _Individual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "target"),
      Core.fieldTypeType = _Individual_type_}]}))

data NegativeDataPropertyAssertion = 
  NegativeDataPropertyAssertion {
    negativeDataPropertyAssertionAnnotations :: [Annotation],
    negativeDataPropertyAssertionProperty :: DataPropertyExpression,
    negativeDataPropertyAssertionSource :: Individual,
    negativeDataPropertyAssertionTarget :: Individual}
  deriving (Eq, Ord, Read, Show)

_NegativeDataPropertyAssertion = (Core.Name "hydra/langs/owl/syntax.NegativeDataPropertyAssertion")

_NegativeDataPropertyAssertion_annotations = (Core.Name "annotations")

_NegativeDataPropertyAssertion_property = (Core.Name "property")

_NegativeDataPropertyAssertion_source = (Core.Name "source")

_NegativeDataPropertyAssertion_target = (Core.Name "target")

_NegativeDataPropertyAssertion_type_ = (Core.TypeRecord (Core.RowType {
  Core.rowTypeTypeName = (Core.Name "hydra/langs/owl/syntax.NegativeDataPropertyAssertion"),
  Core.rowTypeExtends = Nothing,
  Core.rowTypeFields = [
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "annotations"),
      Core.fieldTypeType = (Core.TypeList _Annotation_type_)},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "property"),
      Core.fieldTypeType = _DataPropertyExpression_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "source"),
      Core.fieldTypeType = _Individual_type_},
    Core.FieldType {
      Core.fieldTypeName = (Core.Name "target"),
      Core.fieldTypeType = _Individual_type_}]}))