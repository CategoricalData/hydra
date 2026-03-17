# Note: this is an automatically generated file. Do not edit.

r"""An OWL 2 syntax model. See https://www.w3.org/TR/owl2-syntax."""

from __future__ import annotations
from dataclasses import dataclass
from functools import lru_cache
from hydra.dsl.python import Node, frozenlist
from typing import TypeAlias, cast
import hydra.core
import hydra.ext.org.w3.rdf.syntax
import hydra.ext.org.w3.xml.schema

@dataclass(frozen=True)
class Ontology:
    direct_imports: frozenlist[Ontology]
    annotations: frozenlist[Annotation]
    axioms: frozenlist[Axiom]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Ontology")
    DIRECT_IMPORTS = hydra.core.Name("directImports")
    ANNOTATIONS = hydra.core.Name("annotations")
    AXIOMS = hydra.core.Name("axioms")

@dataclass(frozen=True)
class Declaration:
    annotations: frozenlist[Annotation]
    entity: Entity
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Declaration")
    ANNOTATIONS = hydra.core.Name("annotations")
    ENTITY = hydra.core.Name("entity")

class EntityAnnotationProperty(Node["AnnotationProperty"]):
    ...

class EntityClass(Node["Class"]):
    ...

class EntityDataProperty(Node["DataProperty"]):
    ...

class EntityDatatype(Node["Datatype"]):
    ...

class EntityNamedIndividual(Node["NamedIndividual"]):
    ...

class EntityObjectProperty(Node["ObjectProperty"]):
    ...

class _EntityMeta(type):
    def __getitem__(cls, item):
        return object

class Entity(metaclass=_EntityMeta):
    r"""EntityAnnotationProperty | EntityClass | EntityDataProperty | EntityDatatype | EntityNamedIndividual | EntityObjectProperty"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Entity")
    ANNOTATION_PROPERTY = hydra.core.Name("annotationProperty")
    CLASS = hydra.core.Name("class")
    DATA_PROPERTY = hydra.core.Name("dataProperty")
    DATATYPE = hydra.core.Name("datatype")
    NAMED_INDIVIDUAL = hydra.core.Name("namedIndividual")
    OBJECT_PROPERTY = hydra.core.Name("objectProperty")

class AnnotationSubjectIri(Node["hydra.ext.org.w3.rdf.syntax.Iri"]):
    ...

class AnnotationSubjectAnonymousIndividual(Node["AnonymousIndividual"]):
    ...

class _AnnotationSubjectMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotationSubject(metaclass=_AnnotationSubjectMeta):
    r"""AnnotationSubjectIri | AnnotationSubjectAnonymousIndividual"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationSubject")
    IRI = hydra.core.Name("iri")
    ANONYMOUS_INDIVIDUAL = hydra.core.Name("anonymousIndividual")

class AnnotationValueAnonymousIndividual(Node["AnonymousIndividual"]):
    ...

class AnnotationValueIri(Node["hydra.ext.org.w3.rdf.syntax.Iri"]):
    ...

class AnnotationValueLiteral(Node["hydra.ext.org.w3.rdf.syntax.Literal"]):
    ...

class _AnnotationValueMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotationValue(metaclass=_AnnotationValueMeta):
    r"""AnnotationValueAnonymousIndividual | AnnotationValueIri | AnnotationValueLiteral"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationValue")
    ANONYMOUS_INDIVIDUAL = hydra.core.Name("anonymousIndividual")
    IRI = hydra.core.Name("iri")
    LITERAL = hydra.core.Name("literal")

@dataclass(frozen=True)
class Annotation:
    annotations: frozenlist[Annotation]
    property: AnnotationProperty
    value: AnnotationValue
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Annotation")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    VALUE = hydra.core.Name("value")

class AnnotationAxiomAnnotationAssertion(Node["AnnotationAssertion"]):
    ...

class AnnotationAxiomAnnotationPropertyDomain(Node["AnnotationPropertyDomain"]):
    ...

class AnnotationAxiomAnnotationPropertyRange(Node["AnnotationPropertyRange"]):
    ...

class AnnotationAxiomSubAnnotationPropertyOf(Node["SubAnnotationPropertyOf"]):
    ...

class _AnnotationAxiomMeta(type):
    def __getitem__(cls, item):
        return object

class AnnotationAxiom(metaclass=_AnnotationAxiomMeta):
    r"""AnnotationAxiomAnnotationAssertion | AnnotationAxiomAnnotationPropertyDomain | AnnotationAxiomAnnotationPropertyRange | AnnotationAxiomSubAnnotationPropertyOf"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationAxiom")
    ANNOTATION_ASSERTION = hydra.core.Name("annotationAssertion")
    ANNOTATION_PROPERTY_DOMAIN = hydra.core.Name("annotationPropertyDomain")
    ANNOTATION_PROPERTY_RANGE = hydra.core.Name("annotationPropertyRange")
    SUB_ANNOTATION_PROPERTY_OF = hydra.core.Name("subAnnotationPropertyOf")

@dataclass(frozen=True)
class AnnotationAssertion:
    annotations: frozenlist[Annotation]
    property: AnnotationProperty
    subject: AnnotationSubject
    value: AnnotationValue
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationAssertion")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    SUBJECT = hydra.core.Name("subject")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class SubAnnotationPropertyOf:
    annotations: frozenlist[Annotation]
    sub_property: AnnotationProperty
    super_property: AnnotationProperty
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubAnnotationPropertyOf")
    ANNOTATIONS = hydra.core.Name("annotations")
    SUB_PROPERTY = hydra.core.Name("subProperty")
    SUPER_PROPERTY = hydra.core.Name("superProperty")

@dataclass(frozen=True)
class AnnotationPropertyDomain:
    annotations: frozenlist[Annotation]
    property: AnnotationProperty
    iri: hydra.ext.org.w3.rdf.syntax.Iri
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationPropertyDomain")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    IRI = hydra.core.Name("iri")

@dataclass(frozen=True)
class AnnotationPropertyRange:
    annotations: frozenlist[Annotation]
    property: AnnotationProperty
    iri: hydra.ext.org.w3.rdf.syntax.Iri
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationPropertyRange")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    IRI = hydra.core.Name("iri")

class Class(Node[None]):
    r"""See https://www.w3.org/TR/owl2-syntax/#Classes."""

Class.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Class")

class DatatypeXmlSchema(Node["hydra.ext.org.w3.xml.schema.Datatype"]):
    r"""Note: XML Schema datatypes are treated as a special case in this model (not in the OWL 2 specification itself) because they are particularly common"""

class DatatypeOther(Node["hydra.ext.org.w3.rdf.syntax.Iri"]):
    ...

class _DatatypeMeta(type):
    def __getitem__(cls, item):
        return object

# See https://www.w3.org/TR/owl2-syntax/#Datatypes.
class Datatype(metaclass=_DatatypeMeta):
    r"""DatatypeXmlSchema | DatatypeOther"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Datatype")
    XML_SCHEMA = hydra.core.Name("xmlSchema")
    OTHER = hydra.core.Name("other")

class ObjectProperty(Node[None]):
    r"""See https://www.w3.org/TR/owl2-syntax/#Object_Properties."""

ObjectProperty.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectProperty")

class DataProperty(Node[None]):
    ...

DataProperty.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataProperty")

class AnnotationProperty(Node[None]):
    ...

AnnotationProperty.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnnotationProperty")

class IndividualNamed(Node["NamedIndividual"]):
    ...

class IndividualAnonymous(Node["AnonymousIndividual"]):
    ...

class _IndividualMeta(type):
    def __getitem__(cls, item):
        return object

class Individual(metaclass=_IndividualMeta):
    r"""IndividualNamed | IndividualAnonymous"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Individual")
    NAMED = hydra.core.Name("named")
    ANONYMOUS = hydra.core.Name("anonymous")

class NamedIndividual(Node[None]):
    ...

NamedIndividual.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.NamedIndividual")

class AnonymousIndividual(Node[None]):
    ...

AnonymousIndividual.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AnonymousIndividual")

class ObjectPropertyExpressionObject(Node["ObjectProperty"]):
    ...

class ObjectPropertyExpressionInverseObject(Node["InverseObjectProperty"]):
    ...

class _ObjectPropertyExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectPropertyExpression(metaclass=_ObjectPropertyExpressionMeta):
    r"""ObjectPropertyExpressionObject | ObjectPropertyExpressionInverseObject"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectPropertyExpression")
    OBJECT = hydra.core.Name("object")
    INVERSE_OBJECT = hydra.core.Name("inverseObject")

class InverseObjectProperty(Node["ObjectProperty"]):
    ...

InverseObjectProperty.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.InverseObjectProperty")

class DataPropertyExpression(Node["DataProperty"]):
    ...

DataPropertyExpression.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyExpression")

class DataRangeDataComplementOf(Node["DataComplementOf"]):
    ...

class DataRangeDataIntersectionOf(Node["DataIntersectionOf"]):
    ...

class DataRangeDataOneOf(Node["DataOneOf"]):
    ...

class DataRangeDataUnionOf(Node["DataUnionOf"]):
    ...

class DataRangeDatatype(Node["Datatype"]):
    ...

class DataRangeDatatypeRestriction(Node["DatatypeRestriction"]):
    ...

class _DataRangeMeta(type):
    def __getitem__(cls, item):
        return object

# See https://www.w3.org/TR/owl2-syntax/#Data_Ranges.
class DataRange(metaclass=_DataRangeMeta):
    r"""DataRangeDataComplementOf | DataRangeDataIntersectionOf | DataRangeDataOneOf | DataRangeDataUnionOf | DataRangeDatatype | DataRangeDatatypeRestriction"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataRange")
    DATA_COMPLEMENT_OF = hydra.core.Name("dataComplementOf")
    DATA_INTERSECTION_OF = hydra.core.Name("dataIntersectionOf")
    DATA_ONE_OF = hydra.core.Name("dataOneOf")
    DATA_UNION_OF = hydra.core.Name("dataUnionOf")
    DATATYPE = hydra.core.Name("datatype")
    DATATYPE_RESTRICTION = hydra.core.Name("datatypeRestriction")

class DataIntersectionOf(Node["frozenlist[DataRange]"]):
    r"""See https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges."""

DataIntersectionOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataIntersectionOf")

class DataUnionOf(Node["frozenlist[DataRange]"]):
    r"""See https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges."""

DataUnionOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataUnionOf")

class DataComplementOf(Node["DataRange"]):
    r"""See https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges."""

DataComplementOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataComplementOf")

class DataOneOf(Node["frozenlist[hydra.ext.org.w3.rdf.syntax.Literal]"]):
    r"""See https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals."""

DataOneOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataOneOf")

@dataclass(frozen=True)
class DatatypeRestriction:
    r"""See https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions."""
    
    datatype: Datatype
    constraints: frozenlist[DatatypeRestriction_Constraint]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeRestriction")
    DATATYPE = hydra.core.Name("datatype")
    CONSTRAINTS = hydra.core.Name("constraints")

@dataclass(frozen=True)
class DatatypeRestriction_Constraint:
    constraining_facet: DatatypeRestriction_ConstrainingFacet
    restriction_value: hydra.ext.org.w3.rdf.syntax.Literal
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeRestriction_Constraint")
    CONSTRAINING_FACET = hydra.core.Name("constrainingFacet")
    RESTRICTION_VALUE = hydra.core.Name("restrictionValue")

class DatatypeRestriction_ConstrainingFacetXmlSchema(Node["hydra.ext.org.w3.xml.schema.ConstrainingFacet"]):
    r"""Note: XML Schema constraining facets are treated as a special case in this model (not in the OWL 2 specification itself) because they are particularly common"""

class DatatypeRestriction_ConstrainingFacetOther(Node["hydra.ext.org.w3.rdf.syntax.Iri"]):
    ...

class _DatatypeRestriction_ConstrainingFacetMeta(type):
    def __getitem__(cls, item):
        return object

class DatatypeRestriction_ConstrainingFacet(metaclass=_DatatypeRestriction_ConstrainingFacetMeta):
    r"""DatatypeRestriction_ConstrainingFacetXmlSchema | DatatypeRestriction_ConstrainingFacetOther"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeRestriction_ConstrainingFacet")
    XML_SCHEMA = hydra.core.Name("xmlSchema")
    OTHER = hydra.core.Name("other")

class ClassExpressionClass(Node["Class"]):
    ...

class ClassExpressionDataSomeValuesFrom(Node["DataSomeValuesFrom"]):
    ...

class ClassExpressionDataAllValuesFrom(Node["DataAllValuesFrom"]):
    ...

class ClassExpressionDataHasValue(Node["DataHasValue"]):
    ...

class ClassExpressionDataMinCardinality(Node["DataMinCardinality"]):
    ...

class ClassExpressionDataMaxCardinality(Node["DataMaxCardinality"]):
    ...

class ClassExpressionDataExactCardinality(Node["DataExactCardinality"]):
    ...

class ClassExpressionObjectAllValuesFrom(Node["ObjectAllValuesFrom"]):
    ...

class ClassExpressionObjectExactCardinality(Node["ObjectExactCardinality"]):
    ...

class ClassExpressionObjectHasSelf(Node["ObjectHasSelf"]):
    ...

class ClassExpressionObjectHasValue(Node["ObjectHasValue"]):
    ...

class ClassExpressionObjectIntersectionOf(Node["ObjectIntersectionOf"]):
    ...

class ClassExpressionObjectMaxCardinality(Node["ObjectMaxCardinality"]):
    ...

class ClassExpressionObjectMinCardinality(Node["ObjectMinCardinality"]):
    ...

class ClassExpressionObjectOneOf(Node["ObjectOneOf"]):
    ...

class ClassExpressionObjectSomeValuesFrom(Node["ObjectSomeValuesFrom"]):
    ...

class ClassExpressionObjectUnionOf(Node["ObjectUnionOf"]):
    ...

class _ClassExpressionMeta(type):
    def __getitem__(cls, item):
        return object

class ClassExpression(metaclass=_ClassExpressionMeta):
    r"""ClassExpressionClass | ClassExpressionDataSomeValuesFrom | ClassExpressionDataAllValuesFrom | ClassExpressionDataHasValue | ClassExpressionDataMinCardinality | ClassExpressionDataMaxCardinality | ClassExpressionDataExactCardinality | ClassExpressionObjectAllValuesFrom | ClassExpressionObjectExactCardinality | ClassExpressionObjectHasSelf | ClassExpressionObjectHasValue | ClassExpressionObjectIntersectionOf | ClassExpressionObjectMaxCardinality | ClassExpressionObjectMinCardinality | ClassExpressionObjectOneOf | ClassExpressionObjectSomeValuesFrom | ClassExpressionObjectUnionOf"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ClassExpression")
    CLASS = hydra.core.Name("class")
    DATA_SOME_VALUES_FROM = hydra.core.Name("dataSomeValuesFrom")
    DATA_ALL_VALUES_FROM = hydra.core.Name("dataAllValuesFrom")
    DATA_HAS_VALUE = hydra.core.Name("dataHasValue")
    DATA_MIN_CARDINALITY = hydra.core.Name("dataMinCardinality")
    DATA_MAX_CARDINALITY = hydra.core.Name("dataMaxCardinality")
    DATA_EXACT_CARDINALITY = hydra.core.Name("dataExactCardinality")
    OBJECT_ALL_VALUES_FROM = hydra.core.Name("objectAllValuesFrom")
    OBJECT_EXACT_CARDINALITY = hydra.core.Name("objectExactCardinality")
    OBJECT_HAS_SELF = hydra.core.Name("objectHasSelf")
    OBJECT_HAS_VALUE = hydra.core.Name("objectHasValue")
    OBJECT_INTERSECTION_OF = hydra.core.Name("objectIntersectionOf")
    OBJECT_MAX_CARDINALITY = hydra.core.Name("objectMaxCardinality")
    OBJECT_MIN_CARDINALITY = hydra.core.Name("objectMinCardinality")
    OBJECT_ONE_OF = hydra.core.Name("objectOneOf")
    OBJECT_SOME_VALUES_FROM = hydra.core.Name("objectSomeValuesFrom")
    OBJECT_UNION_OF = hydra.core.Name("objectUnionOf")

class ObjectIntersectionOf(Node["frozenlist[ClassExpression]"]):
    ...

ObjectIntersectionOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectIntersectionOf")

class ObjectUnionOf(Node["frozenlist[ClassExpression]"]):
    ...

ObjectUnionOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectUnionOf")

class ObjectComplementOf(Node["ClassExpression"]):
    ...

ObjectComplementOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectComplementOf")

class ObjectOneOf(Node["frozenlist[Individual]"]):
    ...

ObjectOneOf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectOneOf")

@dataclass(frozen=True)
class ObjectSomeValuesFrom:
    property: ObjectPropertyExpression
    class_: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectSomeValuesFrom")
    PROPERTY = hydra.core.Name("property")
    CLASS = hydra.core.Name("class")

@dataclass(frozen=True)
class ObjectAllValuesFrom:
    property: ObjectPropertyExpression
    class_: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectAllValuesFrom")
    PROPERTY = hydra.core.Name("property")
    CLASS = hydra.core.Name("class")

@dataclass(frozen=True)
class ObjectHasValue:
    property: ObjectPropertyExpression
    individual: Individual
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectHasValue")
    PROPERTY = hydra.core.Name("property")
    INDIVIDUAL = hydra.core.Name("individual")

class ObjectHasSelf(Node["ObjectPropertyExpression"]):
    ...

ObjectHasSelf.TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectHasSelf")

@dataclass(frozen=True)
class ObjectMinCardinality:
    r"""See https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality."""
    
    bound: int
    property: ObjectPropertyExpression
    class_: frozenlist[ClassExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectMinCardinality")
    BOUND = hydra.core.Name("bound")
    PROPERTY = hydra.core.Name("property")
    CLASS = hydra.core.Name("class")

@dataclass(frozen=True)
class ObjectMaxCardinality:
    r"""See https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality."""
    
    bound: int
    property: ObjectPropertyExpression
    class_: frozenlist[ClassExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectMaxCardinality")
    BOUND = hydra.core.Name("bound")
    PROPERTY = hydra.core.Name("property")
    CLASS = hydra.core.Name("class")

@dataclass(frozen=True)
class ObjectExactCardinality:
    r"""See https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality."""
    
    bound: int
    property: ObjectPropertyExpression
    class_: frozenlist[ClassExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectExactCardinality")
    BOUND = hydra.core.Name("bound")
    PROPERTY = hydra.core.Name("property")
    CLASS = hydra.core.Name("class")

@dataclass(frozen=True)
class DataSomeValuesFrom:
    property: frozenlist[DataPropertyExpression]
    range_: DataRange
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataSomeValuesFrom")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class DataAllValuesFrom:
    property: frozenlist[DataPropertyExpression]
    range_: DataRange
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataAllValuesFrom")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class DataHasValue:
    property: DataPropertyExpression
    value: hydra.ext.org.w3.rdf.syntax.Literal
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataHasValue")
    PROPERTY = hydra.core.Name("property")
    VALUE = hydra.core.Name("value")

@dataclass(frozen=True)
class DataMinCardinality:
    bound: int
    property: DataPropertyExpression
    range_: frozenlist[DataRange]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataMinCardinality")
    BOUND = hydra.core.Name("bound")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class DataMaxCardinality:
    bound: int
    property: DataPropertyExpression
    range_: frozenlist[DataRange]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataMaxCardinality")
    BOUND = hydra.core.Name("bound")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class DataExactCardinality:
    bound: int
    property: DataPropertyExpression
    range_: frozenlist[DataRange]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataExactCardinality")
    BOUND = hydra.core.Name("bound")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

class AxiomAnnotationAxiom(Node["AnnotationAxiom"]):
    ...

class AxiomAssertion(Node["Assertion"]):
    ...

class AxiomClassAxiom(Node["ClassAxiom"]):
    ...

class AxiomDataPropertyAxiom(Node["DataPropertyAxiom"]):
    ...

class AxiomDatatypeDefinition(Node["DatatypeDefinition"]):
    ...

class AxiomDeclaration(Node["Declaration"]):
    ...

class AxiomHasKey(Node["HasKey"]):
    ...

class AxiomObjectPropertyAxiom(Node["ObjectPropertyAxiom"]):
    ...

class _AxiomMeta(type):
    def __getitem__(cls, item):
        return object

# See https://www.w3.org/TR/owl2-syntax/#Axioms.
class Axiom(metaclass=_AxiomMeta):
    r"""AxiomAnnotationAxiom | AxiomAssertion | AxiomClassAxiom | AxiomDataPropertyAxiom | AxiomDatatypeDefinition | AxiomDeclaration | AxiomHasKey | AxiomObjectPropertyAxiom"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Axiom")
    ANNOTATION_AXIOM = hydra.core.Name("annotationAxiom")
    ASSERTION = hydra.core.Name("assertion")
    CLASS_AXIOM = hydra.core.Name("classAxiom")
    DATA_PROPERTY_AXIOM = hydra.core.Name("dataPropertyAxiom")
    DATATYPE_DEFINITION = hydra.core.Name("datatypeDefinition")
    DECLARATION = hydra.core.Name("declaration")
    HAS_KEY = hydra.core.Name("hasKey")
    OBJECT_PROPERTY_AXIOM = hydra.core.Name("objectPropertyAxiom")

class ClassAxiomDisjointClasses(Node["DisjointClasses"]):
    ...

class ClassAxiomDisjointUnion(Node["DisjointUnion"]):
    ...

class ClassAxiomEquivalentClasses(Node["EquivalentClasses"]):
    ...

class ClassAxiomSubClassOf(Node["SubClassOf"]):
    ...

class _ClassAxiomMeta(type):
    def __getitem__(cls, item):
        return object

class ClassAxiom(metaclass=_ClassAxiomMeta):
    r"""ClassAxiomDisjointClasses | ClassAxiomDisjointUnion | ClassAxiomEquivalentClasses | ClassAxiomSubClassOf"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ClassAxiom")
    DISJOINT_CLASSES = hydra.core.Name("disjointClasses")
    DISJOINT_UNION = hydra.core.Name("disjointUnion")
    EQUIVALENT_CLASSES = hydra.core.Name("equivalentClasses")
    SUB_CLASS_OF = hydra.core.Name("subClassOf")

@dataclass(frozen=True)
class SubClassOf:
    annotations: frozenlist[Annotation]
    sub_class: ClassExpression
    super_class: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubClassOf")
    ANNOTATIONS = hydra.core.Name("annotations")
    SUB_CLASS = hydra.core.Name("subClass")
    SUPER_CLASS = hydra.core.Name("superClass")

@dataclass(frozen=True)
class EquivalentClasses:
    annotations: frozenlist[Annotation]
    classes: frozenlist[ClassExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentClasses")
    ANNOTATIONS = hydra.core.Name("annotations")
    CLASSES = hydra.core.Name("classes")

@dataclass(frozen=True)
class DisjointClasses:
    annotations: frozenlist[Annotation]
    classes: frozenlist[ClassExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointClasses")
    ANNOTATIONS = hydra.core.Name("annotations")
    CLASSES = hydra.core.Name("classes")

@dataclass(frozen=True)
class DisjointUnion:
    r"""See https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions."""
    
    annotations: frozenlist[Annotation]
    class_: Class
    classes: frozenlist[ClassExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointUnion")
    ANNOTATIONS = hydra.core.Name("annotations")
    CLASS = hydra.core.Name("class")
    CLASSES = hydra.core.Name("classes")

class ObjectPropertyAxiomAsymmetricObjectProperty(Node["AsymmetricObjectProperty"]):
    ...

class ObjectPropertyAxiomDisjointObjectProperties(Node["DisjointObjectProperties"]):
    ...

class ObjectPropertyAxiomEquivalentObjectProperties(Node["EquivalentObjectProperties"]):
    ...

class ObjectPropertyAxiomFunctionalObjectProperty(Node["FunctionalObjectProperty"]):
    ...

class ObjectPropertyAxiomInverseFunctionalObjectProperty(Node["InverseFunctionalObjectProperty"]):
    ...

class ObjectPropertyAxiomInverseObjectProperties(Node["InverseObjectProperties"]):
    ...

class ObjectPropertyAxiomIrreflexiveObjectProperty(Node["IrreflexiveObjectProperty"]):
    ...

class ObjectPropertyAxiomObjectPropertyDomain(Node["ObjectPropertyDomain"]):
    ...

class ObjectPropertyAxiomObjectPropertyRange(Node["ObjectPropertyRange"]):
    ...

class ObjectPropertyAxiomReflexiveObjectProperty(Node["ReflexiveObjectProperty"]):
    ...

class ObjectPropertyAxiomSubObjectPropertyOf(Node["SubObjectPropertyOf"]):
    ...

class ObjectPropertyAxiomSymmetricObjectProperty(Node["SymmetricObjectProperty"]):
    ...

class ObjectPropertyAxiomTransitiveObjectProperty(Node["TransitiveObjectProperty"]):
    ...

class _ObjectPropertyAxiomMeta(type):
    def __getitem__(cls, item):
        return object

class ObjectPropertyAxiom(metaclass=_ObjectPropertyAxiomMeta):
    r"""ObjectPropertyAxiomAsymmetricObjectProperty | ObjectPropertyAxiomDisjointObjectProperties | ObjectPropertyAxiomEquivalentObjectProperties | ObjectPropertyAxiomFunctionalObjectProperty | ObjectPropertyAxiomInverseFunctionalObjectProperty | ObjectPropertyAxiomInverseObjectProperties | ObjectPropertyAxiomIrreflexiveObjectProperty | ObjectPropertyAxiomObjectPropertyDomain | ObjectPropertyAxiomObjectPropertyRange | ObjectPropertyAxiomReflexiveObjectProperty | ObjectPropertyAxiomSubObjectPropertyOf | ObjectPropertyAxiomSymmetricObjectProperty | ObjectPropertyAxiomTransitiveObjectProperty"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectPropertyAxiom")
    ASYMMETRIC_OBJECT_PROPERTY = hydra.core.Name("asymmetricObjectProperty")
    DISJOINT_OBJECT_PROPERTIES = hydra.core.Name("disjointObjectProperties")
    EQUIVALENT_OBJECT_PROPERTIES = hydra.core.Name("equivalentObjectProperties")
    FUNCTIONAL_OBJECT_PROPERTY = hydra.core.Name("functionalObjectProperty")
    INVERSE_FUNCTIONAL_OBJECT_PROPERTY = hydra.core.Name("inverseFunctionalObjectProperty")
    INVERSE_OBJECT_PROPERTIES = hydra.core.Name("inverseObjectProperties")
    IRREFLEXIVE_OBJECT_PROPERTY = hydra.core.Name("irreflexiveObjectProperty")
    OBJECT_PROPERTY_DOMAIN = hydra.core.Name("objectPropertyDomain")
    OBJECT_PROPERTY_RANGE = hydra.core.Name("objectPropertyRange")
    REFLEXIVE_OBJECT_PROPERTY = hydra.core.Name("reflexiveObjectProperty")
    SUB_OBJECT_PROPERTY_OF = hydra.core.Name("subObjectPropertyOf")
    SYMMETRIC_OBJECT_PROPERTY = hydra.core.Name("symmetricObjectProperty")
    TRANSITIVE_OBJECT_PROPERTY = hydra.core.Name("transitiveObjectProperty")

@dataclass(frozen=True)
class SubObjectPropertyOf:
    annotations: frozenlist[Annotation]
    sub_property: frozenlist[ObjectPropertyExpression]
    super_property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubObjectPropertyOf")
    ANNOTATIONS = hydra.core.Name("annotations")
    SUB_PROPERTY = hydra.core.Name("subProperty")
    SUPER_PROPERTY = hydra.core.Name("superProperty")

@dataclass(frozen=True)
class EquivalentObjectProperties:
    annotations: frozenlist[Annotation]
    properties: frozenlist[ObjectPropertyExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentObjectProperties")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class DisjointObjectProperties:
    annotations: frozenlist[Annotation]
    properties: frozenlist[ObjectPropertyExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointObjectProperties")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class ObjectPropertyDomain:
    r"""See https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain."""
    
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    domain: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectPropertyDomain")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    DOMAIN = hydra.core.Name("domain")

@dataclass(frozen=True)
class ObjectPropertyRange:
    r"""See https://www.w3.org/TR/owl2-syntax/#Object_Property_Range."""
    
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    range_: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectPropertyRange")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class InverseObjectProperties:
    annotations: frozenlist[Annotation]
    property1: ObjectPropertyExpression
    property2: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.InverseObjectProperties")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY1 = hydra.core.Name("property1")
    PROPERTY2 = hydra.core.Name("property2")

@dataclass(frozen=True)
class FunctionalObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.FunctionalObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class InverseFunctionalObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.InverseFunctionalObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class ReflexiveObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ReflexiveObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class IrreflexiveObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.IrreflexiveObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class SymmetricObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.SymmetricObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class AsymmetricObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.AsymmetricObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class TransitiveObjectProperty:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.TransitiveObjectProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

class DataPropertyAxiomDataPropertyAxiom(Node["DataPropertyAxiom"]):
    ...

class DataPropertyAxiomDataPropertyRange(Node["DataPropertyRange"]):
    ...

class DataPropertyAxiomDisjointDataProperties(Node["DisjointDataProperties"]):
    ...

class DataPropertyAxiomEquivalentDataProperties(Node["EquivalentDataProperties"]):
    ...

class DataPropertyAxiomFunctionalDataProperty(Node["FunctionalDataProperty"]):
    ...

class DataPropertyAxiomSubDataPropertyOf(Node["SubDataPropertyOf"]):
    ...

class _DataPropertyAxiomMeta(type):
    def __getitem__(cls, item):
        return object

class DataPropertyAxiom(metaclass=_DataPropertyAxiomMeta):
    r"""DataPropertyAxiomDataPropertyAxiom | DataPropertyAxiomDataPropertyRange | DataPropertyAxiomDisjointDataProperties | DataPropertyAxiomEquivalentDataProperties | DataPropertyAxiomFunctionalDataProperty | DataPropertyAxiomSubDataPropertyOf"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyAxiom")
    DATA_PROPERTY_AXIOM = hydra.core.Name("dataPropertyAxiom")
    DATA_PROPERTY_RANGE = hydra.core.Name("dataPropertyRange")
    DISJOINT_DATA_PROPERTIES = hydra.core.Name("disjointDataProperties")
    EQUIVALENT_DATA_PROPERTIES = hydra.core.Name("equivalentDataProperties")
    FUNCTIONAL_DATA_PROPERTY = hydra.core.Name("functionalDataProperty")
    SUB_DATA_PROPERTY_OF = hydra.core.Name("subDataPropertyOf")

@dataclass(frozen=True)
class SubDataPropertyOf:
    annotations: frozenlist[Annotation]
    sub_property: DataPropertyExpression
    super_property: DataPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.SubDataPropertyOf")
    ANNOTATIONS = hydra.core.Name("annotations")
    SUB_PROPERTY = hydra.core.Name("subProperty")
    SUPER_PROPERTY = hydra.core.Name("superProperty")

@dataclass(frozen=True)
class EquivalentDataProperties:
    annotations: frozenlist[Annotation]
    properties: frozenlist[DataPropertyExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.EquivalentDataProperties")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class DisjointDataProperties:
    annotations: frozenlist[Annotation]
    properties: frozenlist[DataPropertyExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DisjointDataProperties")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTIES = hydra.core.Name("properties")

@dataclass(frozen=True)
class DataPropertyDomain:
    annotations: frozenlist[Annotation]
    property: DataPropertyExpression
    domain: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyDomain")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    DOMAIN = hydra.core.Name("domain")

@dataclass(frozen=True)
class DataPropertyRange:
    annotations: frozenlist[Annotation]
    property: DataPropertyExpression
    range_: ClassExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyRange")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class FunctionalDataProperty:
    annotations: frozenlist[Annotation]
    property: DataPropertyExpression
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.FunctionalDataProperty")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")

@dataclass(frozen=True)
class DatatypeDefinition:
    annotations: frozenlist[Annotation]
    datatype: Datatype
    range_: DataRange
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DatatypeDefinition")
    ANNOTATIONS = hydra.core.Name("annotations")
    DATATYPE = hydra.core.Name("datatype")
    RANGE = hydra.core.Name("range")

@dataclass(frozen=True)
class HasKey:
    r"""See https://www.w3.org/TR/owl2-syntax/#Keys."""
    
    annotations: frozenlist[Annotation]
    class_: ClassExpression
    object_properties: frozenlist[ObjectPropertyExpression]
    data_properties: frozenlist[DataPropertyExpression]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.HasKey")
    ANNOTATIONS = hydra.core.Name("annotations")
    CLASS = hydra.core.Name("class")
    OBJECT_PROPERTIES = hydra.core.Name("objectProperties")
    DATA_PROPERTIES = hydra.core.Name("dataProperties")

class AssertionClassAssertion(Node["ClassAssertion"]):
    ...

class AssertionDataPropertyAssertion(Node["DataPropertyAssertion"]):
    ...

class AssertionDifferentIndividuals(Node["DifferentIndividuals"]):
    ...

class AssertionObjectPropertyAssertion(Node["ObjectPropertyAssertion"]):
    ...

class AssertionNegativeDataPropertyAssertion(Node["NegativeDataPropertyAssertion"]):
    ...

class AssertionNegativeObjectPropertyAssertion(Node["NegativeObjectPropertyAssertion"]):
    ...

class AssertionSameIndividual(Node["SameIndividual"]):
    ...

class _AssertionMeta(type):
    def __getitem__(cls, item):
        return object

class Assertion(metaclass=_AssertionMeta):
    r"""AssertionClassAssertion | AssertionDataPropertyAssertion | AssertionDifferentIndividuals | AssertionObjectPropertyAssertion | AssertionNegativeDataPropertyAssertion | AssertionNegativeObjectPropertyAssertion | AssertionSameIndividual"""
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.Assertion")
    CLASS_ASSERTION = hydra.core.Name("classAssertion")
    DATA_PROPERTY_ASSERTION = hydra.core.Name("dataPropertyAssertion")
    DIFFERENT_INDIVIDUALS = hydra.core.Name("differentIndividuals")
    OBJECT_PROPERTY_ASSERTION = hydra.core.Name("objectPropertyAssertion")
    NEGATIVE_DATA_PROPERTY_ASSERTION = hydra.core.Name("negativeDataPropertyAssertion")
    NEGATIVE_OBJECT_PROPERTY_ASSERTION = hydra.core.Name("negativeObjectPropertyAssertion")
    SAME_INDIVIDUAL = hydra.core.Name("sameIndividual")

@dataclass(frozen=True)
class SameIndividual:
    annotations: frozenlist[Annotation]
    individuals: frozenlist[Individual]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.SameIndividual")
    ANNOTATIONS = hydra.core.Name("annotations")
    INDIVIDUALS = hydra.core.Name("individuals")

@dataclass(frozen=True)
class DifferentIndividuals:
    annotations: frozenlist[Annotation]
    individuals: frozenlist[Individual]
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DifferentIndividuals")
    ANNOTATIONS = hydra.core.Name("annotations")
    INDIVIDUALS = hydra.core.Name("individuals")

@dataclass(frozen=True)
class ClassAssertion:
    annotations: frozenlist[Annotation]
    class_: ClassExpression
    individual: Individual
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ClassAssertion")
    ANNOTATIONS = hydra.core.Name("annotations")
    CLASS = hydra.core.Name("class")
    INDIVIDUAL = hydra.core.Name("individual")

@dataclass(frozen=True)
class ObjectPropertyAssertion:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    source: Individual
    target: Individual
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.ObjectPropertyAssertion")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class NegativeObjectPropertyAssertion:
    annotations: frozenlist[Annotation]
    property: ObjectPropertyExpression
    source: Individual
    target: Individual
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.NegativeObjectPropertyAssertion")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class DataPropertyAssertion:
    annotations: frozenlist[Annotation]
    property: DataPropertyExpression
    source: Individual
    target: Individual
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.DataPropertyAssertion")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")

@dataclass(frozen=True)
class NegativeDataPropertyAssertion:
    annotations: frozenlist[Annotation]
    property: DataPropertyExpression
    source: Individual
    target: Individual
    
    TYPE_ = hydra.core.Name("hydra.ext.org.w3.owl.syntax.NegativeDataPropertyAssertion")
    ANNOTATIONS = hydra.core.Name("annotations")
    PROPERTY = hydra.core.Name("property")
    SOURCE = hydra.core.Name("source")
    TARGET = hydra.core.Name("target")
