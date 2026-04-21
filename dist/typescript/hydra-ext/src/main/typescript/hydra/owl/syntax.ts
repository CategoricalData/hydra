// Note: this is an automatically generated file. Do not edit.

/**
 * An OWL 2 syntax model. See https://www.w3.org/TR/owl2-syntax
 */



import * as Core from "../core.js";
import * as RdfSyntax from "../rdf/syntax.js";
import * as XmlSchema from "../xml/schema.js";

export interface Ontology {
  readonly directImports: ReadonlyArray<Ontology>;
  readonly annotations: ReadonlyArray<Annotation>;
  readonly axioms: ReadonlyArray<Axiom>;
}

export interface Declaration {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly entity: Entity;
}

export type Entity =
  | { readonly tag: "annotationProperty"; readonly value: AnnotationProperty }
  | { readonly tag: "class"; readonly value: Class }
  | { readonly tag: "dataProperty"; readonly value: DataProperty }
  | { readonly tag: "datatype"; readonly value: Datatype }
  | { readonly tag: "namedIndividual"; readonly value: NamedIndividual }
  | { readonly tag: "objectProperty"; readonly value: ObjectProperty };

export type AnnotationSubject =
  | { readonly tag: "iri"; readonly value: RdfSyntax.Iri }
  | { readonly tag: "anonymousIndividual"; readonly value: AnonymousIndividual };

export type AnnotationValue =
  | { readonly tag: "anonymousIndividual"; readonly value: AnonymousIndividual }
  | { readonly tag: "iri"; readonly value: RdfSyntax.Iri }
  | { readonly tag: "literal"; readonly value: RdfSyntax.Literal };

export interface Annotation {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: AnnotationProperty;
  readonly value: AnnotationValue;
}

export type AnnotationAxiom =
  | { readonly tag: "annotationAssertion"; readonly value: AnnotationAssertion }
  | { readonly tag: "annotationPropertyDomain"; readonly value: AnnotationPropertyDomain }
  | { readonly tag: "annotationPropertyRange"; readonly value: AnnotationPropertyRange }
  | { readonly tag: "subAnnotationPropertyOf"; readonly value: SubAnnotationPropertyOf };

export interface AnnotationAssertion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: AnnotationProperty;
  readonly subject: AnnotationSubject;
  readonly value: AnnotationValue;
}

export interface SubAnnotationPropertyOf {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly subProperty: AnnotationProperty;
  readonly superProperty: AnnotationProperty;
}

export interface AnnotationPropertyDomain {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: AnnotationProperty;
  readonly iri: RdfSyntax.Iri;
}

export interface AnnotationPropertyRange {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: AnnotationProperty;
  readonly iri: RdfSyntax.Iri;
}

export type Class = void & { readonly __brand: "Class" };

export type Datatype =
  | { readonly tag: "xmlSchema"; readonly value: XmlSchema.Datatype }
  | { readonly tag: "other"; readonly value: RdfSyntax.Iri };

export type ObjectProperty = void & { readonly __brand: "ObjectProperty" };

export type DataProperty = void & { readonly __brand: "DataProperty" };

export type AnnotationProperty = void & { readonly __brand: "AnnotationProperty" };

export type Individual =
  | { readonly tag: "named"; readonly value: NamedIndividual }
  | { readonly tag: "anonymous"; readonly value: AnonymousIndividual };

export type NamedIndividual = void & { readonly __brand: "NamedIndividual" };

export type AnonymousIndividual = void & { readonly __brand: "AnonymousIndividual" };

export type ObjectPropertyExpression =
  | { readonly tag: "object"; readonly value: ObjectProperty }
  | { readonly tag: "inverseObject"; readonly value: InverseObjectProperty };

export type InverseObjectProperty = ObjectProperty & { readonly __brand: "InverseObjectProperty" };

export type DataPropertyExpression = DataProperty & { readonly __brand: "DataPropertyExpression" };

export type DataRange =
  | { readonly tag: "dataComplementOf"; readonly value: DataComplementOf }
  | { readonly tag: "dataIntersectionOf"; readonly value: DataIntersectionOf }
  | { readonly tag: "dataOneOf"; readonly value: DataOneOf }
  | { readonly tag: "dataUnionOf"; readonly value: DataUnionOf }
  | { readonly tag: "datatype"; readonly value: Datatype }
  | { readonly tag: "datatypeRestriction"; readonly value: DatatypeRestriction };

export type DataIntersectionOf = ReadonlyArray<DataRange> & { readonly __brand: "DataIntersectionOf" };

export type DataUnionOf = ReadonlyArray<DataRange> & { readonly __brand: "DataUnionOf" };

export type DataComplementOf = DataRange & { readonly __brand: "DataComplementOf" };

export type DataOneOf = ReadonlyArray<RdfSyntax.Literal> & { readonly __brand: "DataOneOf" };

export interface DatatypeRestriction {
  readonly datatype: Datatype;
  readonly constraints: ReadonlyArray<DatatypeRestriction_Constraint>;
}

export interface DatatypeRestriction_Constraint {
  readonly constrainingFacet: DatatypeRestriction_ConstrainingFacet;
  readonly restrictionValue: RdfSyntax.Literal;
}

export type DatatypeRestriction_ConstrainingFacet =
  | { readonly tag: "xmlSchema"; readonly value: XmlSchema.ConstrainingFacet }
  | { readonly tag: "other"; readonly value: RdfSyntax.Iri };

export type ClassExpression =
  | { readonly tag: "class"; readonly value: Class }
  | { readonly tag: "dataSomeValuesFrom"; readonly value: DataSomeValuesFrom }
  | { readonly tag: "dataAllValuesFrom"; readonly value: DataAllValuesFrom }
  | { readonly tag: "dataHasValue"; readonly value: DataHasValue }
  | { readonly tag: "dataMinCardinality"; readonly value: DataMinCardinality }
  | { readonly tag: "dataMaxCardinality"; readonly value: DataMaxCardinality }
  | { readonly tag: "dataExactCardinality"; readonly value: DataExactCardinality }
  | { readonly tag: "objectAllValuesFrom"; readonly value: ObjectAllValuesFrom }
  | { readonly tag: "objectExactCardinality"; readonly value: ObjectExactCardinality }
  | { readonly tag: "objectHasSelf"; readonly value: ObjectHasSelf }
  | { readonly tag: "objectHasValue"; readonly value: ObjectHasValue }
  | { readonly tag: "objectIntersectionOf"; readonly value: ObjectIntersectionOf }
  | { readonly tag: "objectMaxCardinality"; readonly value: ObjectMaxCardinality }
  | { readonly tag: "objectMinCardinality"; readonly value: ObjectMinCardinality }
  | { readonly tag: "objectOneOf"; readonly value: ObjectOneOf }
  | { readonly tag: "objectSomeValuesFrom"; readonly value: ObjectSomeValuesFrom }
  | { readonly tag: "objectUnionOf"; readonly value: ObjectUnionOf };

export type ObjectIntersectionOf = ReadonlyArray<ClassExpression> & { readonly __brand: "ObjectIntersectionOf" };

export type ObjectUnionOf = ReadonlyArray<ClassExpression> & { readonly __brand: "ObjectUnionOf" };

export type ObjectComplementOf = ClassExpression & { readonly __brand: "ObjectComplementOf" };

export type ObjectOneOf = ReadonlyArray<Individual> & { readonly __brand: "ObjectOneOf" };

export interface ObjectSomeValuesFrom {
  readonly property: ObjectPropertyExpression;
  readonly class: ClassExpression;
}

export interface ObjectAllValuesFrom {
  readonly property: ObjectPropertyExpression;
  readonly class: ClassExpression;
}

export interface ObjectHasValue {
  readonly property: ObjectPropertyExpression;
  readonly individual: Individual;
}

export type ObjectHasSelf = ObjectPropertyExpression & { readonly __brand: "ObjectHasSelf" };

export interface ObjectMinCardinality {
  readonly bound: bigint;
  readonly property: ObjectPropertyExpression;
  readonly class: ReadonlyArray<ClassExpression>;
}

export interface ObjectMaxCardinality {
  readonly bound: bigint;
  readonly property: ObjectPropertyExpression;
  readonly class: ReadonlyArray<ClassExpression>;
}

export interface ObjectExactCardinality {
  readonly bound: bigint;
  readonly property: ObjectPropertyExpression;
  readonly class: ReadonlyArray<ClassExpression>;
}

export interface DataSomeValuesFrom {
  readonly property: ReadonlyArray<DataPropertyExpression>;
  readonly range: DataRange;
}

export interface DataAllValuesFrom {
  readonly property: ReadonlyArray<DataPropertyExpression>;
  readonly range: DataRange;
}

export interface DataHasValue {
  readonly property: DataPropertyExpression;
  readonly value: RdfSyntax.Literal;
}

export interface DataMinCardinality {
  readonly bound: bigint;
  readonly property: DataPropertyExpression;
  readonly range: ReadonlyArray<DataRange>;
}

export interface DataMaxCardinality {
  readonly bound: bigint;
  readonly property: DataPropertyExpression;
  readonly range: ReadonlyArray<DataRange>;
}

export interface DataExactCardinality {
  readonly bound: bigint;
  readonly property: DataPropertyExpression;
  readonly range: ReadonlyArray<DataRange>;
}

export type Axiom =
  | { readonly tag: "annotationAxiom"; readonly value: AnnotationAxiom }
  | { readonly tag: "assertion"; readonly value: Assertion }
  | { readonly tag: "classAxiom"; readonly value: ClassAxiom }
  | { readonly tag: "dataPropertyAxiom"; readonly value: DataPropertyAxiom }
  | { readonly tag: "datatypeDefinition"; readonly value: DatatypeDefinition }
  | { readonly tag: "declaration"; readonly value: Declaration }
  | { readonly tag: "hasKey"; readonly value: HasKey }
  | { readonly tag: "objectPropertyAxiom"; readonly value: ObjectPropertyAxiom };

export type ClassAxiom =
  | { readonly tag: "disjointClasses"; readonly value: DisjointClasses }
  | { readonly tag: "disjointUnion"; readonly value: DisjointUnion }
  | { readonly tag: "equivalentClasses"; readonly value: EquivalentClasses }
  | { readonly tag: "subClassOf"; readonly value: SubClassOf };

export interface SubClassOf {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly subClass: ClassExpression;
  readonly superClass: ClassExpression;
}

export interface EquivalentClasses {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly classes: ReadonlyArray<ClassExpression>;
}

export interface DisjointClasses {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly classes: ReadonlyArray<ClassExpression>;
}

export interface DisjointUnion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly class: Class;
  readonly classes: ReadonlyArray<ClassExpression>;
}

export type ObjectPropertyAxiom =
  | { readonly tag: "asymmetricObjectProperty"; readonly value: AsymmetricObjectProperty }
  | { readonly tag: "disjointObjectProperties"; readonly value: DisjointObjectProperties }
  | { readonly tag: "equivalentObjectProperties"; readonly value: EquivalentObjectProperties }
  | { readonly tag: "functionalObjectProperty"; readonly value: FunctionalObjectProperty }
  | { readonly tag: "inverseFunctionalObjectProperty"; readonly value: InverseFunctionalObjectProperty }
  | { readonly tag: "inverseObjectProperties"; readonly value: InverseObjectProperties }
  | { readonly tag: "irreflexiveObjectProperty"; readonly value: IrreflexiveObjectProperty }
  | { readonly tag: "objectPropertyDomain"; readonly value: ObjectPropertyDomain }
  | { readonly tag: "objectPropertyRange"; readonly value: ObjectPropertyRange }
  | { readonly tag: "reflexiveObjectProperty"; readonly value: ReflexiveObjectProperty }
  | { readonly tag: "subObjectPropertyOf"; readonly value: SubObjectPropertyOf }
  | { readonly tag: "symmetricObjectProperty"; readonly value: SymmetricObjectProperty }
  | { readonly tag: "transitiveObjectProperty"; readonly value: TransitiveObjectProperty };

export interface SubObjectPropertyOf {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly subProperty: ReadonlyArray<ObjectPropertyExpression>;
  readonly superProperty: ObjectPropertyExpression;
}

export interface EquivalentObjectProperties {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly properties: ReadonlyArray<ObjectPropertyExpression>;
}

export interface DisjointObjectProperties {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly properties: ReadonlyArray<ObjectPropertyExpression>;
}

export interface ObjectPropertyDomain {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
  readonly domain: ClassExpression;
}

export interface ObjectPropertyRange {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
  readonly range: ClassExpression;
}

export interface InverseObjectProperties {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property1: ObjectPropertyExpression;
  readonly property2: ObjectPropertyExpression;
}

export interface FunctionalObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export interface InverseFunctionalObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export interface ReflexiveObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export interface IrreflexiveObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export interface SymmetricObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export interface AsymmetricObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export interface TransitiveObjectProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
}

export type DataPropertyAxiom =
  | { readonly tag: "dataPropertyAxiom"; readonly value: DataPropertyAxiom }
  | { readonly tag: "dataPropertyRange"; readonly value: DataPropertyRange }
  | { readonly tag: "disjointDataProperties"; readonly value: DisjointDataProperties }
  | { readonly tag: "equivalentDataProperties"; readonly value: EquivalentDataProperties }
  | { readonly tag: "functionalDataProperty"; readonly value: FunctionalDataProperty }
  | { readonly tag: "subDataPropertyOf"; readonly value: SubDataPropertyOf };

export interface SubDataPropertyOf {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly subProperty: DataPropertyExpression;
  readonly superProperty: DataPropertyExpression;
}

export interface EquivalentDataProperties {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly properties: ReadonlyArray<DataPropertyExpression>;
}

export interface DisjointDataProperties {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly properties: ReadonlyArray<DataPropertyExpression>;
}

export interface DataPropertyDomain {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: DataPropertyExpression;
  readonly domain: ClassExpression;
}

export interface DataPropertyRange {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: DataPropertyExpression;
  readonly range: ClassExpression;
}

export interface FunctionalDataProperty {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: DataPropertyExpression;
}

export interface DatatypeDefinition {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly datatype: Datatype;
  readonly range: DataRange;
}

export interface HasKey {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly class: ClassExpression;
  readonly objectProperties: ReadonlyArray<ObjectPropertyExpression>;
  readonly dataProperties: ReadonlyArray<DataPropertyExpression>;
}

export type Assertion =
  | { readonly tag: "classAssertion"; readonly value: ClassAssertion }
  | { readonly tag: "dataPropertyAssertion"; readonly value: DataPropertyAssertion }
  | { readonly tag: "differentIndividuals"; readonly value: DifferentIndividuals }
  | { readonly tag: "objectPropertyAssertion"; readonly value: ObjectPropertyAssertion }
  | { readonly tag: "negativeDataPropertyAssertion"; readonly value: NegativeDataPropertyAssertion }
  | { readonly tag: "negativeObjectPropertyAssertion"; readonly value: NegativeObjectPropertyAssertion }
  | { readonly tag: "sameIndividual"; readonly value: SameIndividual };

export interface SameIndividual {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly individuals: ReadonlyArray<Individual>;
}

export interface DifferentIndividuals {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly individuals: ReadonlyArray<Individual>;
}

export interface ClassAssertion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly class: ClassExpression;
  readonly individual: Individual;
}

export interface ObjectPropertyAssertion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
  readonly source: Individual;
  readonly target: Individual;
}

export interface NegativeObjectPropertyAssertion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: ObjectPropertyExpression;
  readonly source: Individual;
  readonly target: Individual;
}

export interface DataPropertyAssertion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: DataPropertyExpression;
  readonly source: Individual;
  readonly target: Individual;
}

export interface NegativeDataPropertyAssertion {
  readonly annotations: ReadonlyArray<Annotation>;
  readonly property: DataPropertyExpression;
  readonly source: Individual;
  readonly target: Individual;
}
