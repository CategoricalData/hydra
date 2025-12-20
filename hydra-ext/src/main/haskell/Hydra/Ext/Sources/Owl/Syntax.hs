module Hydra.Ext.Sources.Owl.Syntax where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Additional imports
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Ext.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Ext.Sources.Xml.Schema as XmlSchema


ns :: Namespace
ns = Namespace "hydra.ext.org.w3.owl.syntax"

define :: String -> Type -> Binding
define = defineType ns

owl :: String -> Type
owl = typeref ns

rdf :: String -> Type
rdf = typeref $ RdfSyntax.ns

xsd :: String -> Type
xsd = typeref $ XmlSchema.ns

key_iri :: Name
key_iri = Name "iri"

withIri :: String -> Type -> Type
withIri iriStr = annotateType key_iri (Just $ Terms.string iriStr)

nonNegativeInteger :: Type
nonNegativeInteger = T.bigint

owlIri :: [Char] -> Type -> Type
owlIri local = withIri $ "http://www.w3.org/2002/07/owl#" ++ local

objectPropertyConstraint :: String -> Binding
objectPropertyConstraint lname = define lname $ T.record [
  "annotations">: T.list $ owl "Annotation",
  "property">: owl "ObjectPropertyExpression"]

simpleUnion :: [String] -> Type
simpleUnion names = T.union $ (\n -> FieldType (Name $ decapitalize n) $ owl n) <$> names

withAnns :: [FieldType] -> Type
withAnns fields = T.record $
  ("annotations">: T.list (owl "Annotation")):fields

module_ :: Module
module_ = Module ns elements [Core.ns, RdfSyntax.ns, XmlSchema.ns] [Core.ns] $
    Just "An OWL 2 syntax model. See https://www.w3.org/TR/owl2-syntax"
  where
    elements = generalDefinitions ++ owl2Definitions

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
      datatypeRestriction_Constraint,
      datatypeRestriction_ConstrainingFacet,
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

-- Ontology :=
--     'Ontology' '(' [ ontologyIRI [ versionIRI ] ]
--        directlyImportsDocuments
--        ontologyAnnotations
--        axioms
--     ')'
ontology :: Binding
ontology = define "Ontology" $ T.record [ -- note: omitting IRI and version
  "directImports">: T.list $ owl "Ontology",
  "annotations">: T.list $ owl "Annotation",
  "axioms">: T.list $ owl "Axiom"]

-- ontologyIRI := IRI
-- versionIRI := IRI
-- directlyImportsDocuments := { 'Import' '(' IRI ')' }
-- ontologyAnnotations := { Annotation }
-- axioms := { Axiom }

-- Declaration := 'Declaration' '(' axiomAnnotations Entity ')'
declaration :: Binding
declaration = define "Declaration" $ withAnns [
  "entity">: owl "Entity"]

-- Entity :=
--     'Class' '(' Class ')' |
--     'Datatype' '(' Datatype ')' |
--     'ObjectProperty' '(' ObjectProperty ')' |
--     'DataProperty' '(' DataProperty ')' |
--     'AnnotationProperty' '(' AnnotationProperty ')' |
--     'NamedIndividual' '(' NamedIndividual ')'
entity :: Binding
entity = define "Entity" $ simpleUnion [
  "AnnotationProperty",
  "Class",
  "DataProperty",
  "Datatype",
  "NamedIndividual",
  "ObjectProperty"]

-- AnnotationSubject := IRI | AnonymousIndividual
annotationSubject :: Binding
annotationSubject = define "AnnotationSubject" $ T.union [
  "iri">: rdf "Iri",
  "anonymousIndividual">: owl "AnonymousIndividual"]

-- AnnotationValue := AnonymousIndividual | IRI | Literal
annotationValue :: Binding
annotationValue = define "AnnotationValue" $ T.union [
  "anonymousIndividual">: owl "AnonymousIndividual",
  "iri">: rdf "Iri",
  "literal">: rdf "Literal"]

-- axiomAnnotations := { Annotation }

-- Annotation := 'Annotation' '(' annotationAnnotations AnnotationProperty AnnotationValue ')'
annotation :: Binding
annotation = define "Annotation" $ withAnns [
  "property">: owl "AnnotationProperty",
  "value">: owl "AnnotationValue"]

-- annotationAnnotations  := { Annotation }

-- AnnotationAxiom := AnnotationAssertion | SubAnnotationPropertyOf | AnnotationPropertyDomain | AnnotationPropertyRange
annotationAxiom :: Binding
annotationAxiom = define "AnnotationAxiom" $ simpleUnion [
  "AnnotationAssertion",
  "AnnotationPropertyDomain",
  "AnnotationPropertyRange",
  "SubAnnotationPropertyOf"]

-- AnnotationAssertion := 'AnnotationAssertion' '(' axiomAnnotations AnnotationProperty AnnotationSubject AnnotationValue ')'
annotationAssertion :: Binding
annotationAssertion = define "AnnotationAssertion" $ withAnns [
  "property">: owl "AnnotationProperty",
  "subject">: owl "AnnotationSubject",
  "value">: owl "AnnotationValue"]

-- SubAnnotationPropertyOf := 'SubAnnotationPropertyOf' '(' axiomAnnotations subAnnotationProperty superAnnotationProperty ')'
subAnnotationPropertyOf :: Binding
subAnnotationPropertyOf = define "SubAnnotationPropertyOf" $ withAnns [
  "subProperty">: owl "AnnotationProperty",
  "superProperty">: owl "AnnotationProperty"]

-- subAnnotationProperty := AnnotationProperty
-- superAnnotationProperty := AnnotationProperty

-- AnnotationPropertyDomain := 'AnnotationPropertyDomain' '(' axiomAnnotations AnnotationProperty IRI ')'
annotationPropertyDomain :: Binding
annotationPropertyDomain = define "AnnotationPropertyDomain" $ withAnns [
  "property">: owl "AnnotationProperty",
  "iri">: rdf "Iri"]

-- AnnotationPropertyRange := 'AnnotationPropertyRange' '(' axiomAnnotations AnnotationProperty IRI ')'
annotationPropertyRange :: Binding
annotationPropertyRange = define "AnnotationPropertyRange" $ withAnns [
  "property">: owl "AnnotationProperty",
  "iri">: rdf "Iri"]

-- Class := IRI
class_ :: Binding
class_ = define "Class" $
  see "https://www.w3.org/TR/owl2-syntax/#Classes" $ T.wrap T.unit

-- Datatype := IRI
datatype_ :: Binding
datatype_ = define "Datatype" $
  see "https://www.w3.org/TR/owl2-syntax/#Datatypes" $
  T.union [
    "xmlSchema">:
      note ("XML Schema datatypes are treated as a special case in this model " ++
            "(not in the OWL 2 specification itself) because they are particularly common") $
      xsd "Datatype",
    "other">: rdf "Iri"]

-- ObjectProperty := IRI
objectProperty :: Binding
objectProperty = define "ObjectProperty" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Properties" $ T.wrap T.unit

-- DataProperty := IRI
dataProperty :: Binding
dataProperty = define "DataProperty" $ T.wrap T.unit

-- AnnotationProperty := IRI
annotationProperty :: Binding
annotationProperty = define "AnnotationProperty" $ T.wrap T.unit

-- Individual := NamedIndividual | AnonymousIndividual
individual :: Binding
individual = define "Individual" $ T.union [
  "named">: owl "NamedIndividual",
  "anonymous">: owl "AnonymousIndividual"]

-- NamedIndividual := IRI
namedIndividual :: Binding
namedIndividual = define "NamedIndividual" $ T.wrap T.unit

-- AnonymousIndividual := nodeID
anonymousIndividual :: Binding
anonymousIndividual = define "AnonymousIndividual" $ T.wrap T.unit

-- Literal := typedLiteral | stringLiteralNoLanguage | stringLiteralWithLanguage
-- typedLiteral := lexicalForm '^^' Datatype
-- lexicalForm := quotedString
-- stringLiteralNoLanguage := quotedString
-- stringLiteralWithLanguage := quotedString languageTag

-- ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
objectPropertyExpression :: Binding
objectPropertyExpression = define "ObjectPropertyExpression" $ T.union [
  "object">: owl "ObjectProperty",
  "inverseObject">: owl "InverseObjectProperty"]

-- InverseObjectProperty := 'ObjectInverseOf' '(' ObjectProperty ')'
inverseObjectProperty :: Binding
inverseObjectProperty = define "InverseObjectProperty" $ T.wrap $ owl "ObjectProperty"

-- DataPropertyExpression := DataProperty
dataPropertyExpression :: Binding
dataPropertyExpression = define "DataPropertyExpression" $ T.wrap $ owl "DataProperty"

-- DataRange :=
--     Datatype |
--     DataIntersectionOf |
--     DataUnionOf |
--     DataComplementOf |
--     DataOneOf |
--     DatatypeRestriction
dataRange :: Binding
dataRange = define "DataRange" $
  see "https://www.w3.org/TR/owl2-syntax/#Data_Ranges" $
  simpleUnion [
    "DataComplementOf",
    "DataIntersectionOf",
    "DataOneOf",
    "DataUnionOf",
    "Datatype",
    "DatatypeRestriction"]

-- DataIntersectionOf := 'DataIntersectionOf' '(' DataRange DataRange { DataRange } ')'
dataIntersectionOf :: Binding
dataIntersectionOf = define "DataIntersectionOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges" $
  T.wrap $ twoOrMoreList $ owl "DataRange"

-- DataUnionOf := 'DataUnionOf' '(' DataRange DataRange { DataRange } ')'
dataUnionOf :: Binding
dataUnionOf = define "DataUnionOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges" $
  T.wrap $ twoOrMoreList $ owl "DataRange"

-- DataComplementOf := 'DataComplementOf' '(' DataRange ')'
dataComplementOf :: Binding
dataComplementOf = define "DataComplementOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges" $
  T.wrap $ owl "DataRange"

-- DataOneOf := 'DataOneOf' '(' Literal { Literal } ')'
dataOneOf :: Binding
dataOneOf = define "DataOneOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals" $
  T.wrap $ nonemptyList $ rdf "Literal"

-- DatatypeRestriction := 'DatatypeRestriction' '(' Datatype constrainingFacet restrictionValue { constrainingFacet restrictionValue } ')'
-- constrainingFacet := IRI
-- restrictionValue := Literal
datatypeRestriction :: Binding
datatypeRestriction = define "DatatypeRestriction" $
  see "https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions" $
  T.record [
    "datatype">: owl "Datatype",
    "constraints">: nonemptyList $ owl "DatatypeRestriction_Constraint"]

datatypeRestriction_Constraint :: Binding
datatypeRestriction_Constraint = define "DatatypeRestriction_Constraint" $ T.record [
  "constrainingFacet">: owl "DatatypeRestriction_ConstrainingFacet",
  "restrictionValue">: rdf "Literal"]

datatypeRestriction_ConstrainingFacet :: Binding
datatypeRestriction_ConstrainingFacet = define "DatatypeRestriction_ConstrainingFacet" $
  T.union [
    "xmlSchema">:
      note ("XML Schema constraining facets are treated as a special case in this model " ++
            "(not in the OWL 2 specification itself) because they are particularly common") $
      xsd "ConstrainingFacet",
    "other">: rdf "Iri"]

-- ClassExpression :=
--     Class |
--     ObjectIntersectionOf | ObjectUnionOf | ObjectComplementOf | ObjectOneOf |
--     ObjectSomeValuesFrom | ObjectAllValuesFrom | ObjectHasValue | ObjectHasSelf |
--     ObjectMinCardinality | ObjectMaxCardinality | ObjectExactCardinality |
--     DataSomeValuesFrom | DataAllValuesFrom | DataHasValue |
--     DataMinCardinality | DataMaxCardinality | DataExactCardinality
classExpression :: Binding
classExpression = define "ClassExpression" $ simpleUnion [
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

-- ObjectIntersectionOf := 'ObjectIntersectionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
objectIntersectionOf :: Binding
objectIntersectionOf = define "ObjectIntersectionOf" $ T.wrap $ twoOrMoreList $ owl "ClassExpression"

-- ObjectUnionOf := 'ObjectUnionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
objectUnionOf :: Binding
objectUnionOf = define "ObjectUnionOf" $ T.wrap $ twoOrMoreList $ owl "ClassExpression"

-- ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
objectComplementOf :: Binding
objectComplementOf = define "ObjectComplementOf" $ T.wrap $ owl "ClassExpression"

-- ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual }')'
objectOneOf :: Binding
objectOneOf = define "ObjectOneOf" $ T.wrap $ nonemptyList $ owl "Individual"

-- ObjectSomeValuesFrom := 'ObjectSomeValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
objectSomeValuesFrom :: Binding
objectSomeValuesFrom = define "ObjectSomeValuesFrom" $ T.record [
  "property">: owl "ObjectPropertyExpression",
  "class">: owl "ClassExpression"]

-- ObjectAllValuesFrom := 'ObjectAllValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
objectAllValuesFrom :: Binding
objectAllValuesFrom = define "ObjectAllValuesFrom" $ T.record [
  "property">: owl "ObjectPropertyExpression",
  "class">: owl "ClassExpression"]

-- ObjectHasValue := 'ObjectHasValue' '(' ObjectPropertyExpression Individual ')'
objectHasValue :: Binding
objectHasValue = define "ObjectHasValue" $ T.record [
  "property">: owl "ObjectPropertyExpression",
  "individual">: owl "Individual"]

-- ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
objectHasSelf :: Binding
objectHasSelf = define "ObjectHasSelf" $ T.wrap $ owl "ObjectPropertyExpression"

-- ObjectMinCardinality := 'ObjectMinCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectMinCardinality :: Binding
objectMinCardinality = define "ObjectMinCardinality" $
  see "https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectMaxCardinality := 'ObjectMaxCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectMaxCardinality :: Binding
objectMaxCardinality = define "ObjectMaxCardinality" $
  see "https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectExactCardinality := 'ObjectExactCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectExactCardinality :: Binding
objectExactCardinality = define "ObjectExactCardinality" $
  see "https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- DataSomeValuesFrom := 'DataSomeValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
dataSomeValuesFrom :: Binding
dataSomeValuesFrom = define "DataSomeValuesFrom" $ T.record [
  "property">: nonemptyList $ owl "DataPropertyExpression",
  "range">: owl "DataRange"]

-- DataAllValuesFrom := 'DataAllValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
dataAllValuesFrom :: Binding
dataAllValuesFrom = define "DataAllValuesFrom" $ T.record [
  "property">: nonemptyList $ owl "DataPropertyExpression",
  "range">: owl "DataRange"]

-- DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
dataHasValue :: Binding
dataHasValue = define "DataHasValue" $ T.record [
  "property">: owl "DataPropertyExpression",
  "value">: rdf "Literal"]

-- DataMinCardinality := 'DataMinCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataMinCardinality :: Binding
dataMinCardinality = define "DataMinCardinality" $ T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataMaxCardinality := 'DataMaxCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataMaxCardinality :: Binding
dataMaxCardinality = define "DataMaxCardinality" $ T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataExactCardinality := 'DataExactCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataExactCardinality :: Binding
dataExactCardinality = define "DataExactCardinality" $ T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- Axiom := Declaration | ClassAxiom | ObjectPropertyAxiom | DataPropertyAxiom | DatatypeDefinition | HasKey | Assertion | AnnotationAxiom
axiom :: Binding
axiom = define "Axiom" $
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

-- ClassAxiom := SubClassOf | EquivalentClasses | DisjointClasses | DisjointUnion
classAxiom :: Binding
classAxiom = define "ClassAxiom" $ simpleUnion [
  "DisjointClasses",
  "DisjointUnion",
  "EquivalentClasses",
  "SubClassOf"]

-- SubClassOf := 'SubClassOf' '(' axiomAnnotations subClassExpression superClassExpression ')'
-- subClassExpression := ClassExpression
-- superClassExpression := ClassExpression
subClassOf :: Binding
subClassOf = define "SubClassOf" $ withAnns [
  "subClass">: owl "ClassExpression",
  "superClass">: owl "ClassExpression"]

-- EquivalentClasses := 'EquivalentClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
equivalentClasses :: Binding
equivalentClasses = define "EquivalentClasses" $ withAnns [
  "classes">: twoOrMoreList $ owl "ClassExpression"]

-- DisjointClasses := 'DisjointClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
disjointClasses :: Binding
disjointClasses = define "DisjointClasses" $ withAnns [
  "classes">: twoOrMoreList $ owl "ClassExpression"]

-- DisjointUnion := 'DisjointUnion' '(' axiomAnnotations Class disjointClassExpressions ')'
-- disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }
disjointUnion :: Binding
disjointUnion = define "DisjointUnion" $
  see "https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions" $
  withAnns [
    "class">: owl "Class",
    "classes">: twoOrMoreList $ owl "ClassExpression"]

-- ObjectPropertyAxiom :=
--     SubObjectPropertyOf | EquivalentObjectProperties |
--     DisjointObjectProperties | InverseObjectProperties |
--     ObjectPropertyDomain | ObjectPropertyRange |
--     FunctionalObjectProperty | InverseFunctionalObjectProperty |
--     ReflexiveObjectProperty | IrreflexiveObjectProperty |
--     SymmetricObjectProperty | AsymmetricObjectProperty |
--     TransitiveObjectProperty
objectPropertyAxiom :: Binding
objectPropertyAxiom = define "ObjectPropertyAxiom" $ simpleUnion [
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

-- SubObjectPropertyOf := 'SubObjectPropertyOf' '(' axiomAnnotations subObjectPropertyExpression superObjectPropertyExpression ')'
subObjectPropertyOf :: Binding
subObjectPropertyOf = define "SubObjectPropertyOf" $ withAnns [
  "subProperty">: nonemptyList $ owl "ObjectPropertyExpression",
  "superProperty">: owl "ObjectPropertyExpression"]
-- subObjectPropertyExpression := ObjectPropertyExpression | propertyExpressionChain
-- propertyExpressionChain := 'ObjectPropertyChain' '(' ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
-- superObjectPropertyExpression := ObjectPropertyExpression

-- EquivalentObjectProperties := 'EquivalentObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
equivalentObjectProperties :: Binding
equivalentObjectProperties = define "EquivalentObjectProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "ObjectPropertyExpression"]

-- DisjointObjectProperties := 'DisjointObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
disjointObjectProperties :: Binding
disjointObjectProperties = define "DisjointObjectProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "ObjectPropertyExpression"]

-- ObjectPropertyDomain := 'ObjectPropertyDomain' '(' axiomAnnotations ObjectPropertyExpression ClassExpression ')'
objectPropertyDomain :: Binding
objectPropertyDomain = define "ObjectPropertyDomain" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain" $
  withAnns [
    "property">: owl "ObjectPropertyExpression",
    "domain">: owl "ClassExpression"]

-- ObjectPropertyRange := 'ObjectPropertyRange' '(' axiomAnnotations ObjectPropertyExpression ClassExpression ')'
objectPropertyRange :: Binding
objectPropertyRange = define "ObjectPropertyRange" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Property_Range" $
  withAnns [
    "property">: owl "ObjectPropertyExpression",
    "range">: owl "ClassExpression"]

-- InverseObjectProperties := 'InverseObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression ')'
inverseObjectProperties :: Binding
inverseObjectProperties = define "InverseObjectProperties" $ withAnns [
  "property1">: owl "ObjectPropertyExpression",
  "property2">: owl "ObjectPropertyExpression"]

-- FunctionalObjectProperty := 'FunctionalObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
functionalObjectProperty :: Binding
functionalObjectProperty = objectPropertyConstraint "FunctionalObjectProperty"

-- InverseFunctionalObjectProperty := 'InverseFunctionalObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
inverseFunctionalObjectProperty :: Binding
inverseFunctionalObjectProperty = objectPropertyConstraint "InverseFunctionalObjectProperty"

-- ReflexiveObjectProperty := 'ReflexiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
reflexiveObjectProperty :: Binding
reflexiveObjectProperty = objectPropertyConstraint "ReflexiveObjectProperty"

-- IrreflexiveObjectProperty := 'IrreflexiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
irreflexiveObjectProperty :: Binding
irreflexiveObjectProperty = objectPropertyConstraint "IrreflexiveObjectProperty"

-- SymmetricObjectProperty := 'SymmetricObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
symmetricObjectProperty :: Binding
symmetricObjectProperty = objectPropertyConstraint "SymmetricObjectProperty"

-- AsymmetricObjectProperty := 'AsymmetricObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
asymmetricObjectProperty :: Binding
asymmetricObjectProperty = objectPropertyConstraint "AsymmetricObjectProperty"

-- TransitiveObjectProperty := 'TransitiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
transitiveObjectProperty :: Binding
transitiveObjectProperty = objectPropertyConstraint "TransitiveObjectProperty"

-- DataPropertyAxiom :=
--     SubDataPropertyOf | EquivalentDataProperties | DisjointDataProperties |
--     DataPropertyDomain | DataPropertyRange | FunctionalDataProperty
dataPropertyAxiom :: Binding
dataPropertyAxiom = define "DataPropertyAxiom" $ simpleUnion [
  "DataPropertyAxiom",
  "DataPropertyRange",
  "DisjointDataProperties",
  "EquivalentDataProperties",
  "FunctionalDataProperty",
  "SubDataPropertyOf"]

-- SubDataPropertyOf := 'SubDataPropertyOf' '(' axiomAnnotations subDataPropertyExpression superDataPropertyExpression ')'
subDataPropertyOf :: Binding
subDataPropertyOf = define "SubDataPropertyOf" $ withAnns [
  "subProperty">: owl "DataPropertyExpression",
  "superProperty">: owl "DataPropertyExpression"]
-- subDataPropertyExpression := DataPropertyExpression
-- superDataPropertyExpression := DataPropertyExpression

-- EquivalentDataProperties := 'EquivalentDataProperties' '(' axiomAnnotations DataPropertyExpression DataPropertyExpression { DataPropertyExpression } ')'
equivalentDataProperties :: Binding
equivalentDataProperties = define "EquivalentDataProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "DataPropertyExpression"]

-- DisjointDataProperties := 'DisjointDataProperties' '(' axiomAnnotations DataPropertyExpression DataPropertyExpression { DataPropertyExpression } ')'
disjointDataProperties :: Binding
disjointDataProperties = define "DisjointDataProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "DataPropertyExpression"]

-- DataPropertyDomain := 'DataPropertyDomain' '(' axiomAnnotations DataPropertyExpression ClassExpression ')'
dataPropertyDomain :: Binding
dataPropertyDomain = define "DataPropertyDomain" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "domain">: owl "ClassExpression"]

-- DataPropertyRange := 'DataPropertyRange' '(' axiomAnnotations DataPropertyExpression DataRange ')'
dataPropertyRange :: Binding
dataPropertyRange = define "DataPropertyRange" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "range">: owl "ClassExpression"]

-- FunctionalDataProperty := 'FunctionalDataProperty' '(' axiomAnnotations DataPropertyExpression ')'
functionalDataProperty :: Binding
functionalDataProperty = define "FunctionalDataProperty" $ withAnns [
  "property">: owl "DataPropertyExpression"]

-- DatatypeDefinition := 'DatatypeDefinition' '(' axiomAnnotations Datatype DataRange ')'
datatypeDefinition :: Binding
datatypeDefinition = define "DatatypeDefinition" $ withAnns [
  "datatype">: owl "Datatype",
  "range">: owl "DataRange"]

-- HasKey := 'HasKey' '(' axiomAnnotations ClassExpression '(' { ObjectPropertyExpression } ')' '(' { DataPropertyExpression } ')' ')'
hasKey :: Binding
hasKey = define "HasKey" $
  see "https://www.w3.org/TR/owl2-syntax/#Keys" $
  withAnns [
    "class">: owl "ClassExpression",
    "objectProperties">: T.list $ owl "ObjectPropertyExpression",
    "dataProperties">: T.list $ owl "DataPropertyExpression"]

-- Assertion :=
--     SameIndividual | DifferentIndividuals | ClassAssertion |
--     ObjectPropertyAssertion | NegativeObjectPropertyAssertion |
--     DataPropertyAssertion | NegativeDataPropertyAssertion
assertion :: Binding
assertion = define "Assertion" $ simpleUnion [
  "ClassAssertion",
  "DataPropertyAssertion",
  "DifferentIndividuals",
  "ObjectPropertyAssertion",
  "NegativeDataPropertyAssertion",
  "NegativeObjectPropertyAssertion",
  "SameIndividual"]

-- sourceIndividual := Individual
-- targetIndividual := Individual
-- targetValue := Literal
-- SameIndividual := 'SameIndividual' '(' axiomAnnotations Individual Individual { Individual } ')'
sameIndividual :: Binding
sameIndividual = define "SameIndividual" $ withAnns [
  "individuals">: twoOrMoreList $ owl "Individual"]

-- DifferentIndividuals := 'DifferentIndividuals' '(' axiomAnnotations Individual Individual { Individual } ')'
differentIndividuals :: Binding
differentIndividuals = define "DifferentIndividuals" $ withAnns [
  "individuals">: twoOrMoreList $ owl "Individual"]

-- ClassAssertion := 'ClassAssertion' '(' axiomAnnotations ClassExpression Individual ')'
classAssertion :: Binding
classAssertion = define "ClassAssertion" $ withAnns [
  "class">: owl "ClassExpression",
  "individual">: owl "Individual"]

-- ObjectPropertyAssertion := 'ObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression sourceIndividual targetIndividual ')'
objectPropertyAssertion :: Binding
objectPropertyAssertion = define "ObjectPropertyAssertion" $ withAnns [
  "property">: owl "ObjectPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- NegativeObjectPropertyAssertion := 'NegativeObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression sourceIndividual targetIndividual ')'
negativeObjectPropertyAssertion :: Binding
negativeObjectPropertyAssertion = define "NegativeObjectPropertyAssertion" $ withAnns [
  "property">: owl "ObjectPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- DataPropertyAssertion := 'DataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
dataPropertyAssertion :: Binding
dataPropertyAssertion = define "DataPropertyAssertion" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- NegativeDataPropertyAssertion := 'NegativeDataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
negativeDataPropertyAssertion :: Binding
negativeDataPropertyAssertion = define "NegativeDataPropertyAssertion" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]
