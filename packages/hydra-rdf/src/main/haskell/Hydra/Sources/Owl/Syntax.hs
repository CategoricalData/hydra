module Hydra.Sources.Owl.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Overlay.Haskell.Dsl.Annotations
import           Hydra.Overlay.Haskell.Bootstrap
import           Hydra.Overlay.Haskell.Dsl.Types                 ((>:))
import qualified Hydra.Overlay.Haskell.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Overlay.Haskell.Dsl.Terms              as Terms
import qualified Hydra.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Sources.Xml.Schema as XmlSchema


ns :: ModuleName
ns = ModuleName "hydra.owl.syntax"

define :: String -> Type -> TypeDefinition
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (DefinitionType <$> definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, RdfSyntax.ns, XmlSchema.ns],
            moduleMetadata = descriptionMetadata (Just "An OWL 2 syntax model. See https://www.w3.org/TR/owl2-syntax")}
  where
    definitions = generalDefinitions ++ owl2Definitions

    generalDefinitions = [
      ontology,
      declaration,
      entity,
      annotationSubject,
      annotationValue,
      annotation,
      annotationAxiom,
      annotationAssertion,
      subAnnotationPropertyOf,
      annotationPropertyDomain,
      annotationPropertyRange]

    owl2Definitions = [
      class_,
      datatype_,
      objectProperty,
      dataProperty,
      annotationProperty,
      individual,
      namedIndividual,
      anonymousIndividual,
      objectPropertyExpression,
      inverseObjectProperty,
      dataPropertyExpression,
      dataRange,
      dataIntersectionOf,
      dataUnionOf,
      dataComplementOf,
      dataOneOf,
      datatypeRestriction,
      datatypeRestrictionConstraint,
      datatypeRestrictionConstrainingFacet,
      classExpression,
      objectIntersectionOf,
      objectUnionOf,
      objectComplementOf,
      objectOneOf,
      objectSomeValuesFrom,
      objectAllValuesFrom,
      objectHasValue,
      objectHasSelf,
      objectMinCardinality,
      objectMaxCardinality,
      objectExactCardinality,
      dataSomeValuesFrom,
      dataAllValuesFrom,
      dataHasValue,
      dataMinCardinality,
      dataMaxCardinality,
      dataExactCardinality,
      axiom,
      classAxiom,
      subClassOf,
      equivalentClasses,
      disjointClasses,
      disjointUnion,
      objectPropertyAxiom,
      subObjectPropertyOf,
      equivalentObjectProperties,
      disjointObjectProperties,
      objectPropertyDomain,
      objectPropertyRange,
      inverseObjectProperties,
      functionalObjectProperty,
      inverseFunctionalObjectProperty,
      reflexiveObjectProperty,
      irreflexiveObjectProperty,
      symmetricObjectProperty,
      asymmetricObjectProperty,
      transitiveObjectProperty,
      dataPropertyAxiom,
      subDataPropertyOf,
      equivalentDataProperties,
      disjointDataProperties,
      dataPropertyDomain,
      dataPropertyRange,
      functionalDataProperty,
      datatypeDefinition,
      hasKey,
      assertion,
      sameIndividual,
      differentIndividuals,
      classAssertion,
      objectPropertyAssertion,
      negativeObjectPropertyAssertion,
      dataPropertyAssertion,
      negativeDataPropertyAssertion]

-- nonNegativeInteger := a nonempty finite sequence of digits between 0 and 9
-- quotedString := a finite sequence of characters in which " (U+22) and \ (U+5C) occur only in pairs of the form \" (U+5C, U+22) and \\ (U+5C, U+5C), enclosed in a pair of " (U+22) characters
-- languageTag := @ (U+40) followed a nonempty sequence of characters matching the langtag production from [BCP 47]
-- nodeID := a finite sequence of characters matching the BLANK_NODE_LABEL production of [SPARQL]
-- fullIRI := an IRI as defined in [RFC3987], enclosed in a pair of < (U+3C) and > (U+3E) characters
-- prefixName := a finite sequence of characters matching the as PNAME_NS production of [SPARQL]
-- abbreviatedIRI := a finite sequence of characters matching the PNAME_LN production of [SPARQL]
-- IRI := fullIRI | abbreviatedIRI
-- ontologyDocument := { prefixDeclaration } Ontology
-- prefixDeclaration := 'Prefix' '(' prefixName '=' fullIRI ')'

-- AnnotationSubject := IRI | AnonymousIndividual
annotationSubject :: TypeDefinition
annotationSubject = define "AnnotationSubject" $
  doc "An OWL annotation subject: an IRI or an anonymous individual" $
  T.union [
  "iri">: rdf "Iri",
  "anonymousIndividual">: owl "AnonymousIndividual"]

-- AnnotationValue := AnonymousIndividual | IRI | Literal
annotationValue :: TypeDefinition
annotationValue = define "AnnotationValue" $
  doc "An OWL annotation value: an anonymous individual, IRI, or literal" $
  T.union [
  "anonymousIndividual">: owl "AnonymousIndividual",
  "iri">: rdf "Iri",
  "literal">: rdf "Literal"]

-- Declaration := 'Declaration' '(' axiomAnnotations Entity ')'
declaration :: TypeDefinition
declaration = define "Declaration" $
  doc "An OWL entity declaration" $
  withAnns [
  "entity">: owl "Entity"]

-- Entity :=
--     'Class' '(' Class ')' |
--     'Datatype' '(' Datatype ')' |
--     'ObjectProperty' '(' ObjectProperty ')' |
--     'DataProperty' '(' DataProperty ')' |
--     'AnnotationProperty' '(' AnnotationProperty ')' |
--     'NamedIndividual' '(' NamedIndividual ')'
entity :: TypeDefinition
entity = define "Entity" $
  doc "An OWL entity: a class, datatype, object/data/annotation property, or named individual" $
  simpleUnion [
  "AnnotationProperty",
  "Class",
  "DataProperty",
  "Datatype",
  "NamedIndividual",
  "ObjectProperty"]

key_iri :: Name
key_iri = Name "iri"

nonNegativeInteger :: Type
nonNegativeInteger = T.bigint

objectPropertyConstraint :: String -> TypeDefinition
objectPropertyConstraint lname = define lname $
  doc "An OWL axiom asserting a characteristic (e.g. symmetric, transitive) of an object property" $
  T.record [
  "annotations">: T.list $ owl "Annotation",
  "property">: owl "ObjectPropertyExpression"]

-- Ontology :=
--     'Ontology' '(' [ ontologyIRI [ versionIRI ] ]
--        directlyImportsDocuments
--        ontologyAnnotations
--        axioms
--     ')'
ontology :: TypeDefinition
ontology = define "Ontology" $
  doc "An OWL ontology: a set of imports, annotations, and axioms" $
  T.record [ -- note: omitting IRI and version
  "directImports">: T.list $ owl "Ontology",
  "annotations">: T.list $ owl "Annotation",
  "axioms">: T.list $ owl "Axiom"]

-- ontologyIRI := IRI
-- versionIRI := IRI
-- directlyImportsDocuments := { 'Import' '(' IRI ')' }
-- ontologyAnnotations := { Annotation }
-- axioms := { Axiom }

owl :: String -> Type
owl = typeref ns

owlIri :: [Char] -> Type -> Type
owlIri local = withIri $ "http://www.w3.org/2002/07/owl#" ++ local

rdf :: String -> Type
rdf = typeref $ RdfSyntax.ns

simpleUnion :: [String] -> Type
simpleUnion names = T.union $ (\n -> FieldType (Name $ decapitalize n) $ owl n) <$> names

withAnns :: [FieldType] -> Type
withAnns fields = T.record $
  ("annotations">: T.list (owl "Annotation")):fields

withIri :: String -> Type -> Type
withIri iriStr = annotateType key_iri (Just $ Terms.string iriStr)

xsd :: String -> Type
xsd = typeref $ XmlSchema.ns

-- axiomAnnotations := { Annotation }

-- Annotation := 'Annotation' '(' annotationAnnotations AnnotationProperty AnnotationValue ')'
annotation :: TypeDefinition
annotation = define "Annotation" $
  doc "An OWL annotation on an axiom, consisting of a property and value" $
  withAnns [
  "property">: owl "AnnotationProperty",
  "value">: owl "AnnotationValue"]

-- annotationAnnotations  := { Annotation }

-- AnnotationAssertion := 'AnnotationAssertion' '(' axiomAnnotations AnnotationProperty AnnotationSubject AnnotationValue ')'
annotationAssertion :: TypeDefinition
annotationAssertion = define "AnnotationAssertion" $
  doc "An OWL annotation assertion axiom" $
  withAnns [
  "property">: owl "AnnotationProperty",
  "subject">: owl "AnnotationSubject",
  "value">: owl "AnnotationValue"]

-- AnnotationAxiom := AnnotationAssertion | SubAnnotationPropertyOf | AnnotationPropertyDomain | AnnotationPropertyRange
annotationAxiom :: TypeDefinition
annotationAxiom = define "AnnotationAxiom" $
  doc "An OWL annotation axiom" $
  simpleUnion [
  "AnnotationAssertion",
  "AnnotationPropertyDomain",
  "AnnotationPropertyRange",
  "SubAnnotationPropertyOf"]

-- AnnotationProperty := IRI
annotationProperty :: TypeDefinition
annotationProperty = define "AnnotationProperty" $
  doc "An OWL annotation property, identified by an IRI" $
  T.wrap T.unit

-- AnnotationPropertyDomain := 'AnnotationPropertyDomain' '(' axiomAnnotations AnnotationProperty IRI ')'
annotationPropertyDomain :: TypeDefinition
annotationPropertyDomain = define "AnnotationPropertyDomain" $
  doc "An OWL axiom asserting the domain of an annotation property" $
  withAnns [
  "property">: owl "AnnotationProperty",
  "iri">: rdf "Iri"]

-- AnnotationPropertyRange := 'AnnotationPropertyRange' '(' axiomAnnotations AnnotationProperty IRI ')'
annotationPropertyRange :: TypeDefinition
annotationPropertyRange = define "AnnotationPropertyRange" $
  doc "An OWL axiom asserting the range of an annotation property" $
  withAnns [
  "property">: owl "AnnotationProperty",
  "iri">: rdf "Iri"]

-- AnonymousIndividual := nodeID
anonymousIndividual :: TypeDefinition
anonymousIndividual = define "AnonymousIndividual" $
  doc "An OWL anonymous individual, identified by a blank node id" $
  T.wrap T.unit

-- Literal := typedLiteral | stringLiteralNoLanguage | stringLiteralWithLanguage
-- typedLiteral := lexicalForm '^^' Datatype
-- lexicalForm := quotedString
-- stringLiteralNoLanguage := quotedString
-- stringLiteralWithLanguage := quotedString languageTag

-- Assertion :=
--     SameIndividual | DifferentIndividuals | ClassAssertion |
--     ObjectPropertyAssertion | NegativeObjectPropertyAssertion |
--     DataPropertyAssertion | NegativeDataPropertyAssertion
assertion :: TypeDefinition
assertion = define "Assertion" $
  doc "An OWL assertion axiom about individuals" $
  simpleUnion [
  "ClassAssertion",
  "DataPropertyAssertion",
  "DifferentIndividuals",
  "ObjectPropertyAssertion",
  "NegativeDataPropertyAssertion",
  "NegativeObjectPropertyAssertion",
  "SameIndividual"]

-- AsymmetricObjectProperty := 'AsymmetricObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
asymmetricObjectProperty :: TypeDefinition
asymmetricObjectProperty = objectPropertyConstraint "AsymmetricObjectProperty"

-- Axiom := Declaration | ClassAxiom | ObjectPropertyAxiom | DataPropertyAxiom | DatatypeDefinition | HasKey | Assertion | AnnotationAxiom
axiom :: TypeDefinition
axiom = define "Axiom" $
  doc "An OWL axiom" $
  see "https://www.w3.org/TR/owl2-syntax/#Axioms" $
  simpleUnion [
    "AnnotationAxiom",
    "Assertion",
    "ClassAxiom",
    "DataPropertyAxiom",
    "DatatypeDefinition",
    "Declaration",
    "HasKey",
    "ObjectPropertyAxiom"]

-- ClassAssertion := 'ClassAssertion' '(' axiomAnnotations ClassExpression Individual ')'
classAssertion :: TypeDefinition
classAssertion = define "ClassAssertion" $
  doc "An OWL axiom asserting that an individual belongs to a class" $
  withAnns [
  "class">: owl "ClassExpression",
  "individual">: owl "Individual"]

-- ClassAxiom := SubClassOf | EquivalentClasses | DisjointClasses | DisjointUnion
classAxiom :: TypeDefinition
classAxiom = define "ClassAxiom" $
  doc "An OWL axiom about classes" $
  simpleUnion [
  "DisjointClasses",
  "DisjointUnion",
  "EquivalentClasses",
  "SubClassOf"]

-- ClassExpression :=
--     Class |
--     ObjectIntersectionOf | ObjectUnionOf | ObjectComplementOf | ObjectOneOf |
--     ObjectSomeValuesFrom | ObjectAllValuesFrom | ObjectHasValue | ObjectHasSelf |
--     ObjectMinCardinality | ObjectMaxCardinality | ObjectExactCardinality |
--     DataSomeValuesFrom | DataAllValuesFrom | DataHasValue |
--     DataMinCardinality | DataMaxCardinality | DataExactCardinality
classExpression :: TypeDefinition
classExpression = define "ClassExpression" $
  doc "An OWL class expression" $
  simpleUnion [
  "Class",
  "DataSomeValuesFrom",
  "DataAllValuesFrom",
  "DataHasValue",
  "DataMinCardinality",
  "DataMaxCardinality",
  "DataExactCardinality",
  "ObjectAllValuesFrom",
  "ObjectExactCardinality",
  "ObjectHasSelf",
  "ObjectHasValue",
  "ObjectIntersectionOf",
  "ObjectMaxCardinality",
  "ObjectMinCardinality",
  "ObjectOneOf",
  "ObjectSomeValuesFrom",
  "ObjectUnionOf"]

-- Class := IRI
class_ :: TypeDefinition
class_ = define "Class" $
  doc "An OWL class, identified by an IRI" $
  see "https://www.w3.org/TR/owl2-syntax/#Classes" $ T.wrap T.unit

-- DataAllValuesFrom := 'DataAllValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
dataAllValuesFrom :: TypeDefinition
dataAllValuesFrom = define "DataAllValuesFrom" $
  doc "An OWL universal data property restriction" $
  T.record [
  "property">: nonemptyList $ owl "DataPropertyExpression",
  "range">: owl "DataRange"]

-- DataComplementOf := 'DataComplementOf' '(' DataRange ')'
dataComplementOf :: TypeDefinition
dataComplementOf = define "DataComplementOf" $
  doc "An OWL data range which is the complement of another data range" $
  see "https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges" $
  T.wrap $ owl "DataRange"

-- DataExactCardinality := 'DataExactCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataExactCardinality :: TypeDefinition
dataExactCardinality = define "DataExactCardinality" $
  doc "An OWL exact-cardinality restriction on a data property" $
  T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
dataHasValue :: TypeDefinition
dataHasValue = define "DataHasValue" $
  doc "An OWL data property restriction requiring a specific literal value" $
  T.record [
  "property">: owl "DataPropertyExpression",
  "value">: rdf "Literal"]

-- DataIntersectionOf := 'DataIntersectionOf' '(' DataRange DataRange { DataRange } ')'
dataIntersectionOf :: TypeDefinition
dataIntersectionOf = define "DataIntersectionOf" $
  doc "An OWL data range which is the intersection of two or more data ranges" $
  see "https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges" $
  T.wrap $ twoOrMoreList $ owl "DataRange"

-- DataMaxCardinality := 'DataMaxCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataMaxCardinality :: TypeDefinition
dataMaxCardinality = define "DataMaxCardinality" $
  doc "An OWL maximum-cardinality restriction on a data property" $
  T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataMinCardinality := 'DataMinCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataMinCardinality :: TypeDefinition
dataMinCardinality = define "DataMinCardinality" $
  doc "An OWL minimum-cardinality restriction on a data property" $
  T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataOneOf := 'DataOneOf' '(' Literal { Literal } ')'
dataOneOf :: TypeDefinition
dataOneOf = define "DataOneOf" $
  doc "An OWL data range enumerating one or more literals" $
  see "https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals" $
  T.wrap $ nonemptyList $ rdf "Literal"

-- DataProperty := IRI
dataProperty :: TypeDefinition
dataProperty = define "DataProperty" $
  doc "An OWL data property, identified by an IRI" $
  T.wrap T.unit

-- DataPropertyAssertion := 'DataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
dataPropertyAssertion :: TypeDefinition
dataPropertyAssertion = define "DataPropertyAssertion" $
  doc "An OWL axiom asserting a data property relationship between an individual and a literal" $
  withAnns [
  "property">: owl "DataPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- DataPropertyAxiom :=
--     SubDataPropertyOf | EquivalentDataProperties | DisjointDataProperties |
--     DataPropertyDomain | DataPropertyRange | FunctionalDataProperty
dataPropertyAxiom :: TypeDefinition
dataPropertyAxiom = define "DataPropertyAxiom" $
  doc "An OWL axiom about data properties" $
  simpleUnion [
  "DataPropertyAxiom",
  "DataPropertyRange",
  "DisjointDataProperties",
  "EquivalentDataProperties",
  "FunctionalDataProperty",
  "SubDataPropertyOf"]

-- DataPropertyDomain := 'DataPropertyDomain' '(' axiomAnnotations DataPropertyExpression ClassExpression ')'
dataPropertyDomain :: TypeDefinition
dataPropertyDomain = define "DataPropertyDomain" $
  doc "An OWL axiom asserting the domain of a data property" $
  withAnns [
  "property">: owl "DataPropertyExpression",
  "domain">: owl "ClassExpression"]

-- DataPropertyExpression := DataProperty
dataPropertyExpression :: TypeDefinition
dataPropertyExpression = define "DataPropertyExpression" $
  doc "An OWL data property expression" $
  T.wrap $ owl "DataProperty"

-- DataPropertyRange := 'DataPropertyRange' '(' axiomAnnotations DataPropertyExpression DataRange ')'
dataPropertyRange :: TypeDefinition
dataPropertyRange = define "DataPropertyRange" $
  doc "An OWL axiom asserting the range of a data property" $
  withAnns [
  "property">: owl "DataPropertyExpression",
  "range">: owl "ClassExpression"]

-- DataRange :=
--     Datatype |
--     DataIntersectionOf |
--     DataUnionOf |
--     DataComplementOf |
--     DataOneOf |
--     DatatypeRestriction
dataRange :: TypeDefinition
dataRange = define "DataRange" $
  doc "An OWL data range" $
  see "https://www.w3.org/TR/owl2-syntax/#Data_Ranges" $
  simpleUnion [
    "DataComplementOf",
    "DataIntersectionOf",
    "DataOneOf",
    "DataUnionOf",
    "Datatype",
    "DatatypeRestriction"]

-- DataSomeValuesFrom := 'DataSomeValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
dataSomeValuesFrom :: TypeDefinition
dataSomeValuesFrom = define "DataSomeValuesFrom" $
  doc "An OWL existential data property restriction" $
  T.record [
  "property">: nonemptyList $ owl "DataPropertyExpression",
  "range">: owl "DataRange"]

-- DataUnionOf := 'DataUnionOf' '(' DataRange DataRange { DataRange } ')'
dataUnionOf :: TypeDefinition
dataUnionOf = define "DataUnionOf" $
  doc "An OWL data range which is the union of two or more data ranges" $
  see "https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges" $
  T.wrap $ twoOrMoreList $ owl "DataRange"

-- DatatypeDefinition := 'DatatypeDefinition' '(' axiomAnnotations Datatype DataRange ')'
datatypeDefinition :: TypeDefinition
datatypeDefinition = define "DatatypeDefinition" $
  doc "An OWL axiom defining a datatype in terms of a data range" $
  withAnns [
  "datatype">: owl "Datatype",
  "range">: owl "DataRange"]

-- DatatypeRestriction := 'DatatypeRestriction' '(' Datatype constrainingFacet restrictionValue { constrainingFacet restrictionValue } ')'
-- constrainingFacet := IRI
-- restrictionValue := Literal
datatypeRestriction :: TypeDefinition
datatypeRestriction = define "DatatypeRestriction" $
  doc "An OWL datatype restricted by one or more constraining facets" $
  see "https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions" $
  T.record [
    "datatype">: owl "Datatype",
    "constraints">: nonemptyList $ owl "DatatypeRestrictionConstraint"]

datatypeRestrictionConstrainingFacet :: TypeDefinition
datatypeRestrictionConstrainingFacet = define "DatatypeRestrictionConstrainingFacet" $
  doc "A constraining facet used in an OWL datatype restriction" $
  T.union [
    "xmlSchema">:
      note ("XML Schema constraining facets are treated as a special case in this model " ++
            "(not in the OWL 2 specification itself) because they are particularly common") $
      xsd "ConstrainingFacet",
    "other">: rdf "Iri"]

datatypeRestrictionConstraint :: TypeDefinition
datatypeRestrictionConstraint = define "DatatypeRestrictionConstraint" $
  doc "A single constraining facet and its restriction value in an OWL datatype restriction" $
  T.record [
  "constrainingFacet">: owl "DatatypeRestrictionConstrainingFacet",
  "restrictionValue">: rdf "Literal"]

-- Datatype := IRI
datatype_ :: TypeDefinition
datatype_ = define "Datatype" $
  doc "An OWL datatype, identified by an IRI" $
  see "https://www.w3.org/TR/owl2-syntax/#Datatypes" $
  T.union [
    "xmlSchema">:
      note ("XML Schema datatypes are treated as a special case in this model " ++
            "(not in the OWL 2 specification itself) because they are particularly common") $
      xsd "Datatype",
    "other">: rdf "Iri"]

-- DifferentIndividuals := 'DifferentIndividuals' '(' axiomAnnotations Individual Individual { Individual } ')'
differentIndividuals :: TypeDefinition
differentIndividuals = define "DifferentIndividuals" $
  doc "An OWL axiom asserting that two or more individuals are pairwise distinct" $
  withAnns [
  "individuals">: twoOrMoreList $ owl "Individual"]

-- DisjointClasses := 'DisjointClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
disjointClasses :: TypeDefinition
disjointClasses = define "DisjointClasses" $
  doc "An OWL axiom asserting that two or more classes are pairwise disjoint" $
  withAnns [
  "classes">: twoOrMoreList $ owl "ClassExpression"]

-- DisjointDataProperties := 'DisjointDataProperties' '(' axiomAnnotations DataPropertyExpression DataPropertyExpression { DataPropertyExpression } ')'
disjointDataProperties :: TypeDefinition
disjointDataProperties = define "DisjointDataProperties" $
  doc "An OWL axiom asserting that two or more data properties are pairwise disjoint" $
  withAnns [
  "properties">: twoOrMoreList $ owl "DataPropertyExpression"]

-- DisjointObjectProperties := 'DisjointObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
disjointObjectProperties :: TypeDefinition
disjointObjectProperties = define "DisjointObjectProperties" $
  doc "An OWL axiom asserting that two or more object properties are pairwise disjoint" $
  withAnns [
  "properties">: twoOrMoreList $ owl "ObjectPropertyExpression"]

-- DisjointUnion := 'DisjointUnion' '(' axiomAnnotations Class disjointClassExpressions ')'
-- disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }
disjointUnion :: TypeDefinition
disjointUnion = define "DisjointUnion" $
  doc "An OWL axiom asserting that a class is the disjoint union of other class expressions" $
  see "https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions" $
  withAnns [
    "class">: owl "Class",
    "classes">: twoOrMoreList $ owl "ClassExpression"]

-- EquivalentClasses := 'EquivalentClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
equivalentClasses :: TypeDefinition
equivalentClasses = define "EquivalentClasses" $
  doc "An OWL axiom asserting that two or more classes are equivalent" $
  withAnns [
  "classes">: twoOrMoreList $ owl "ClassExpression"]

-- EquivalentDataProperties := 'EquivalentDataProperties' '(' axiomAnnotations DataPropertyExpression DataPropertyExpression { DataPropertyExpression } ')'
equivalentDataProperties :: TypeDefinition
equivalentDataProperties = define "EquivalentDataProperties" $
  doc "An OWL axiom asserting that two or more data properties are equivalent" $
  withAnns [
  "properties">: twoOrMoreList $ owl "DataPropertyExpression"]

-- EquivalentObjectProperties := 'EquivalentObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
equivalentObjectProperties :: TypeDefinition
equivalentObjectProperties = define "EquivalentObjectProperties" $
  doc "An OWL axiom asserting that two or more object properties are equivalent" $
  withAnns [
  "properties">: twoOrMoreList $ owl "ObjectPropertyExpression"]

-- FunctionalDataProperty := 'FunctionalDataProperty' '(' axiomAnnotations DataPropertyExpression ')'
functionalDataProperty :: TypeDefinition
functionalDataProperty = define "FunctionalDataProperty" $
  doc "An OWL axiom asserting that a data property is functional" $
  withAnns [
  "property">: owl "DataPropertyExpression"]

-- FunctionalObjectProperty := 'FunctionalObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
functionalObjectProperty :: TypeDefinition
functionalObjectProperty = objectPropertyConstraint "FunctionalObjectProperty"

-- HasKey := 'HasKey' '(' axiomAnnotations ClassExpression '(' { ObjectPropertyExpression } ')' '(' { DataPropertyExpression } ')' ')'
hasKey :: TypeDefinition
hasKey = define "HasKey" $
  doc "An OWL axiom asserting that a set of properties functions as a key for a class" $
  see "https://www.w3.org/TR/owl2-syntax/#Keys" $
  withAnns [
    "class">: owl "ClassExpression",
    "objectProperties">: T.list $ owl "ObjectPropertyExpression",
    "dataProperties">: T.list $ owl "DataPropertyExpression"]

-- Individual := NamedIndividual | AnonymousIndividual
individual :: TypeDefinition
individual = define "Individual" $
  doc "An OWL individual: a named individual or an anonymous individual" $
  T.union [
  "named">: owl "NamedIndividual",
  "anonymous">: owl "AnonymousIndividual"]

-- InverseFunctionalObjectProperty := 'InverseFunctionalObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
inverseFunctionalObjectProperty :: TypeDefinition
inverseFunctionalObjectProperty = objectPropertyConstraint "InverseFunctionalObjectProperty"

-- InverseObjectProperties := 'InverseObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression ')'
inverseObjectProperties :: TypeDefinition
inverseObjectProperties = define "InverseObjectProperties" $
  doc "An OWL axiom asserting that two object properties are inverses of each other" $
  withAnns [
  "property1">: owl "ObjectPropertyExpression",
  "property2">: owl "ObjectPropertyExpression"]

-- InverseObjectProperty := 'ObjectInverseOf' '(' ObjectProperty ')'
inverseObjectProperty :: TypeDefinition
inverseObjectProperty = define "InverseObjectProperty" $
  doc "The inverse of an OWL object property" $
  T.wrap $ owl "ObjectProperty"

-- IrreflexiveObjectProperty := 'IrreflexiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
irreflexiveObjectProperty :: TypeDefinition
irreflexiveObjectProperty = objectPropertyConstraint "IrreflexiveObjectProperty"

-- NamedIndividual := IRI
namedIndividual :: TypeDefinition
namedIndividual = define "NamedIndividual" $
  doc "An OWL named individual, identified by an IRI" $
  T.wrap T.unit

-- NegativeDataPropertyAssertion := 'NegativeDataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
negativeDataPropertyAssertion :: TypeDefinition
negativeDataPropertyAssertion = define "NegativeDataPropertyAssertion" $
  doc "An OWL axiom asserting that a data property relationship does NOT hold" $
  withAnns [
  "property">: owl "DataPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- NegativeObjectPropertyAssertion := 'NegativeObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression sourceIndividual targetIndividual ')'
negativeObjectPropertyAssertion :: TypeDefinition
negativeObjectPropertyAssertion = define "NegativeObjectPropertyAssertion" $
  doc "An OWL axiom asserting that an object property relationship does NOT hold" $
  withAnns [
  "property">: owl "ObjectPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- ObjectAllValuesFrom := 'ObjectAllValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
objectAllValuesFrom :: TypeDefinition
objectAllValuesFrom = define "ObjectAllValuesFrom" $
  doc "An OWL universal object property restriction" $
  T.record [
  "property">: owl "ObjectPropertyExpression",
  "class">: owl "ClassExpression"]

-- ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
objectComplementOf :: TypeDefinition
objectComplementOf = define "ObjectComplementOf" $
  doc "An OWL class expression which is the complement of another class expression" $
  T.wrap $ owl "ClassExpression"

-- ObjectExactCardinality := 'ObjectExactCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectExactCardinality :: TypeDefinition
objectExactCardinality = define "ObjectExactCardinality" $
  doc "An OWL exact-cardinality restriction on an object property" $
  see "https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
objectHasSelf :: TypeDefinition
objectHasSelf = define "ObjectHasSelf" $
  doc "An OWL class expression restricting an object property to self-relationships" $
  T.wrap $ owl "ObjectPropertyExpression"

-- ObjectHasValue := 'ObjectHasValue' '(' ObjectPropertyExpression Individual ')'
objectHasValue :: TypeDefinition
objectHasValue = define "ObjectHasValue" $
  doc "An OWL object property restriction requiring a specific individual value" $
  T.record [
  "property">: owl "ObjectPropertyExpression",
  "individual">: owl "Individual"]

-- ObjectIntersectionOf := 'ObjectIntersectionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
objectIntersectionOf :: TypeDefinition
objectIntersectionOf = define "ObjectIntersectionOf" $
  doc "An OWL class expression which is the intersection of two or more class expressions" $
  T.wrap $ twoOrMoreList $ owl "ClassExpression"

-- ObjectMaxCardinality := 'ObjectMaxCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectMaxCardinality :: TypeDefinition
objectMaxCardinality = define "ObjectMaxCardinality" $
  doc "An OWL maximum-cardinality restriction on an object property" $
  see "https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectMinCardinality := 'ObjectMinCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectMinCardinality :: TypeDefinition
objectMinCardinality = define "ObjectMinCardinality" $
  doc "An OWL minimum-cardinality restriction on an object property" $
  see "https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual }')'
objectOneOf :: TypeDefinition
objectOneOf = define "ObjectOneOf" $
  doc "An OWL class expression enumerating one or more individuals" $
  T.wrap $ nonemptyList $ owl "Individual"

-- ObjectProperty := IRI
objectProperty :: TypeDefinition
objectProperty = define "ObjectProperty" $
  doc "An OWL object property, identified by an IRI" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Properties" $ T.wrap T.unit

-- ObjectPropertyAssertion := 'ObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression sourceIndividual targetIndividual ')'
objectPropertyAssertion :: TypeDefinition
objectPropertyAssertion = define "ObjectPropertyAssertion" $
  doc "An OWL axiom asserting an object property relationship between two individuals" $
  withAnns [
  "property">: owl "ObjectPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- ObjectPropertyAxiom :=
--     SubObjectPropertyOf | EquivalentObjectProperties |
--     DisjointObjectProperties | InverseObjectProperties |
--     ObjectPropertyDomain | ObjectPropertyRange |
--     FunctionalObjectProperty | InverseFunctionalObjectProperty |
--     ReflexiveObjectProperty | IrreflexiveObjectProperty |
--     SymmetricObjectProperty | AsymmetricObjectProperty |
--     TransitiveObjectProperty
objectPropertyAxiom :: TypeDefinition
objectPropertyAxiom = define "ObjectPropertyAxiom" $
  doc "An OWL axiom about object properties" $
  simpleUnion [
  "AsymmetricObjectProperty",
  "DisjointObjectProperties",
  "EquivalentObjectProperties",
  "FunctionalObjectProperty",
  "InverseFunctionalObjectProperty",
  "InverseObjectProperties",
  "IrreflexiveObjectProperty",
  "ObjectPropertyDomain",
  "ObjectPropertyRange",
  "ReflexiveObjectProperty",
  "SubObjectPropertyOf",
  "SymmetricObjectProperty",
  "TransitiveObjectProperty"]

-- ObjectPropertyDomain := 'ObjectPropertyDomain' '(' axiomAnnotations ObjectPropertyExpression ClassExpression ')'
objectPropertyDomain :: TypeDefinition
objectPropertyDomain = define "ObjectPropertyDomain" $
  doc "An OWL axiom asserting the domain of an object property" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain" $
  withAnns [
    "property">: owl "ObjectPropertyExpression",
    "domain">: owl "ClassExpression"]

-- ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
objectPropertyExpression :: TypeDefinition
objectPropertyExpression = define "ObjectPropertyExpression" $
  doc "An OWL object property expression: an object property or its inverse" $
  T.union [
  "object">: owl "ObjectProperty",
  "inverseObject">: owl "InverseObjectProperty"]

-- ObjectPropertyRange := 'ObjectPropertyRange' '(' axiomAnnotations ObjectPropertyExpression ClassExpression ')'
objectPropertyRange :: TypeDefinition
objectPropertyRange = define "ObjectPropertyRange" $
  doc "An OWL axiom asserting the range of an object property" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Property_Range" $
  withAnns [
    "property">: owl "ObjectPropertyExpression",
    "range">: owl "ClassExpression"]

-- ObjectSomeValuesFrom := 'ObjectSomeValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
objectSomeValuesFrom :: TypeDefinition
objectSomeValuesFrom = define "ObjectSomeValuesFrom" $
  doc "An OWL existential object property restriction" $
  T.record [
  "property">: owl "ObjectPropertyExpression",
  "class">: owl "ClassExpression"]

-- ObjectUnionOf := 'ObjectUnionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
objectUnionOf :: TypeDefinition
objectUnionOf = define "ObjectUnionOf" $
  doc "An OWL class expression which is the union of two or more class expressions" $
  T.wrap $ twoOrMoreList $ owl "ClassExpression"

-- ReflexiveObjectProperty := 'ReflexiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
reflexiveObjectProperty :: TypeDefinition
reflexiveObjectProperty = objectPropertyConstraint "ReflexiveObjectProperty"

-- sourceIndividual := Individual
-- targetIndividual := Individual
-- targetValue := Literal
-- SameIndividual := 'SameIndividual' '(' axiomAnnotations Individual Individual { Individual } ')'
sameIndividual :: TypeDefinition
sameIndividual = define "SameIndividual" $
  doc "An OWL axiom asserting that two or more individuals are the same" $
  withAnns [
  "individuals">: twoOrMoreList $ owl "Individual"]

-- SubAnnotationPropertyOf := 'SubAnnotationPropertyOf' '(' axiomAnnotations subAnnotationProperty superAnnotationProperty ')'
subAnnotationPropertyOf :: TypeDefinition
subAnnotationPropertyOf = define "SubAnnotationPropertyOf" $
  doc "An OWL axiom asserting an annotation-property subsumption relationship" $
  withAnns [
  "subProperty">: owl "AnnotationProperty",
  "superProperty">: owl "AnnotationProperty"]

-- subAnnotationProperty := AnnotationProperty
-- superAnnotationProperty := AnnotationProperty

-- SubClassOf := 'SubClassOf' '(' axiomAnnotations subClassExpression superClassExpression ')'
-- subClassExpression := ClassExpression
-- superClassExpression := ClassExpression
subClassOf :: TypeDefinition
subClassOf = define "SubClassOf" $
  doc "An OWL axiom asserting a subclass relationship" $
  withAnns [
  "subClass">: owl "ClassExpression",
  "superClass">: owl "ClassExpression"]

-- SubDataPropertyOf := 'SubDataPropertyOf' '(' axiomAnnotations subDataPropertyExpression superDataPropertyExpression ')'
subDataPropertyOf :: TypeDefinition
subDataPropertyOf = define "SubDataPropertyOf" $
  doc "An OWL axiom asserting a data-property subsumption relationship" $
  withAnns [
  "subProperty">: owl "DataPropertyExpression",
  "superProperty">: owl "DataPropertyExpression"]
-- subDataPropertyExpression := DataPropertyExpression
-- superDataPropertyExpression := DataPropertyExpression

-- SubObjectPropertyOf := 'SubObjectPropertyOf' '(' axiomAnnotations subObjectPropertyExpression superObjectPropertyExpression ')'
subObjectPropertyOf :: TypeDefinition
subObjectPropertyOf = define "SubObjectPropertyOf" $
  doc "An OWL axiom asserting an object-property subsumption relationship" $
  withAnns [
  "subProperty">: nonemptyList $ owl "ObjectPropertyExpression",
  "superProperty">: owl "ObjectPropertyExpression"]
-- subObjectPropertyExpression := ObjectPropertyExpression | propertyExpressionChain
-- propertyExpressionChain := 'ObjectPropertyChain' '(' ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
-- superObjectPropertyExpression := ObjectPropertyExpression

-- SymmetricObjectProperty := 'SymmetricObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
symmetricObjectProperty :: TypeDefinition
symmetricObjectProperty = objectPropertyConstraint "SymmetricObjectProperty"

-- TransitiveObjectProperty := 'TransitiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
transitiveObjectProperty :: TypeDefinition
transitiveObjectProperty = objectPropertyConstraint "TransitiveObjectProperty"
