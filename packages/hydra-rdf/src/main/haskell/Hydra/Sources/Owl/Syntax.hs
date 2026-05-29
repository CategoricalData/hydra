module Hydra.Sources.Owl.Syntax where

-- Standard imports for type-level sources outside of the kernel
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types                 ((>:))
import qualified Hydra.Dsl.Types                 as T
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

-- Additional imports
import qualified Hydra.Dsl.Terms              as Terms
import qualified Hydra.Sources.Rdf.Syntax as RdfSyntax
import qualified Hydra.Sources.Xml.Schema as XmlSchema


ns :: ModuleName
ns = ModuleName "hydra.owl.syntax"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = (map toTypeDef definitions),
            moduleDependencies = unqualifiedDep <$> [Core.ns, RdfSyntax.ns, XmlSchema.ns],
            moduleDescription = Just "An OWL 2 syntax model. See https://www.w3.org/TR/owl2-syntax"}
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

key_iri :: Name
key_iri = Name "iri"

nonNegativeInteger :: Type
nonNegativeInteger = T.bigint

objectPropertyConstraint :: String -> Binding
objectPropertyConstraint lname = define lname $ T.record [
  "annotations">: T.list $ owl "Annotation",
  "property">: owl "ObjectPropertyExpression"]

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
annotation :: Binding
annotation = define "Annotation" $ withAnns [
  "property">: owl "AnnotationProperty",
  "value">: owl "AnnotationValue"]

-- annotationAnnotations  := { Annotation }

-- AnnotationAssertion := 'AnnotationAssertion' '(' axiomAnnotations AnnotationProperty AnnotationSubject AnnotationValue ')'
annotationAssertion :: Binding
annotationAssertion = define "AnnotationAssertion" $ withAnns [
  "property">: owl "AnnotationProperty",
  "subject">: owl "AnnotationSubject",
  "value">: owl "AnnotationValue"]

-- AnnotationAxiom := AnnotationAssertion | SubAnnotationPropertyOf | AnnotationPropertyDomain | AnnotationPropertyRange
annotationAxiom :: Binding
annotationAxiom = define "AnnotationAxiom" $ simpleUnion [
  "AnnotationAssertion",
  "AnnotationPropertyDomain",
  "AnnotationPropertyRange",
  "SubAnnotationPropertyOf"]

-- AnnotationProperty := IRI
annotationProperty :: Binding
annotationProperty = define "AnnotationProperty" $ T.wrap T.unit

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

-- AnonymousIndividual := nodeID
anonymousIndividual :: Binding
anonymousIndividual = define "AnonymousIndividual" $ T.wrap T.unit

-- Literal := typedLiteral | stringLiteralNoLanguage | stringLiteralWithLanguage
-- typedLiteral := lexicalForm '^^' Datatype
-- lexicalForm := quotedString
-- stringLiteralNoLanguage := quotedString
-- stringLiteralWithLanguage := quotedString languageTag

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

-- AsymmetricObjectProperty := 'AsymmetricObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
asymmetricObjectProperty :: Binding
asymmetricObjectProperty = objectPropertyConstraint "AsymmetricObjectProperty"

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

-- ClassAssertion := 'ClassAssertion' '(' axiomAnnotations ClassExpression Individual ')'
classAssertion :: Binding
classAssertion = define "ClassAssertion" $ withAnns [
  "class">: owl "ClassExpression",
  "individual">: owl "Individual"]

-- ClassAxiom := SubClassOf | EquivalentClasses | DisjointClasses | DisjointUnion
classAxiom :: Binding
classAxiom = define "ClassAxiom" $ simpleUnion [
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

-- Class := IRI
class_ :: Binding
class_ = define "Class" $
  see "https://www.w3.org/TR/owl2-syntax/#Classes" $ T.wrap T.unit

-- DataAllValuesFrom := 'DataAllValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
dataAllValuesFrom :: Binding
dataAllValuesFrom = define "DataAllValuesFrom" $ T.record [
  "property">: nonemptyList $ owl "DataPropertyExpression",
  "range">: owl "DataRange"]

-- DataComplementOf := 'DataComplementOf' '(' DataRange ')'
dataComplementOf :: Binding
dataComplementOf = define "DataComplementOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Complement_of_Data_Ranges" $
  T.wrap $ owl "DataRange"

-- DataExactCardinality := 'DataExactCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataExactCardinality :: Binding
dataExactCardinality = define "DataExactCardinality" $ T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataHasValue := 'DataHasValue' '(' DataPropertyExpression Literal ')'
dataHasValue :: Binding
dataHasValue = define "DataHasValue" $ T.record [
  "property">: owl "DataPropertyExpression",
  "value">: rdf "Literal"]

-- DataIntersectionOf := 'DataIntersectionOf' '(' DataRange DataRange { DataRange } ')'
dataIntersectionOf :: Binding
dataIntersectionOf = define "DataIntersectionOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Intersection_of_Data_Ranges" $
  T.wrap $ twoOrMoreList $ owl "DataRange"

-- DataMaxCardinality := 'DataMaxCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataMaxCardinality :: Binding
dataMaxCardinality = define "DataMaxCardinality" $ T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataMinCardinality := 'DataMinCardinality' '(' nonNegativeInteger DataPropertyExpression [ DataRange ] ')'
dataMinCardinality :: Binding
dataMinCardinality = define "DataMinCardinality" $ T.record [
  "bound">: nonNegativeInteger,
  "property">: owl "DataPropertyExpression",
  "range">: T.list $ owl "DataRange"]

-- DataOneOf := 'DataOneOf' '(' Literal { Literal } ')'
dataOneOf :: Binding
dataOneOf = define "DataOneOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Enumeration_of_Literals" $
  T.wrap $ nonemptyList $ rdf "Literal"

-- DataProperty := IRI
dataProperty :: Binding
dataProperty = define "DataProperty" $ T.wrap T.unit

-- DataPropertyAssertion := 'DataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
dataPropertyAssertion :: Binding
dataPropertyAssertion = define "DataPropertyAssertion" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

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

-- DataPropertyDomain := 'DataPropertyDomain' '(' axiomAnnotations DataPropertyExpression ClassExpression ')'
dataPropertyDomain :: Binding
dataPropertyDomain = define "DataPropertyDomain" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "domain">: owl "ClassExpression"]

-- DataPropertyExpression := DataProperty
dataPropertyExpression :: Binding
dataPropertyExpression = define "DataPropertyExpression" $ T.wrap $ owl "DataProperty"

-- DataPropertyRange := 'DataPropertyRange' '(' axiomAnnotations DataPropertyExpression DataRange ')'
dataPropertyRange :: Binding
dataPropertyRange = define "DataPropertyRange" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "range">: owl "ClassExpression"]

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

-- DataSomeValuesFrom := 'DataSomeValuesFrom' '(' DataPropertyExpression { DataPropertyExpression } DataRange ')'
dataSomeValuesFrom :: Binding
dataSomeValuesFrom = define "DataSomeValuesFrom" $ T.record [
  "property">: nonemptyList $ owl "DataPropertyExpression",
  "range">: owl "DataRange"]

-- DataUnionOf := 'DataUnionOf' '(' DataRange DataRange { DataRange } ')'
dataUnionOf :: Binding
dataUnionOf = define "DataUnionOf" $
  see "https://www.w3.org/TR/owl2-syntax/#Union_of_Data_Ranges" $
  T.wrap $ twoOrMoreList $ owl "DataRange"

-- DatatypeDefinition := 'DatatypeDefinition' '(' axiomAnnotations Datatype DataRange ')'
datatypeDefinition :: Binding
datatypeDefinition = define "DatatypeDefinition" $ withAnns [
  "datatype">: owl "Datatype",
  "range">: owl "DataRange"]

-- DatatypeRestriction := 'DatatypeRestriction' '(' Datatype constrainingFacet restrictionValue { constrainingFacet restrictionValue } ')'
-- constrainingFacet := IRI
-- restrictionValue := Literal
datatypeRestriction :: Binding
datatypeRestriction = define "DatatypeRestriction" $
  see "https://www.w3.org/TR/owl2-syntax/#Datatype_Restrictions" $
  T.record [
    "datatype">: owl "Datatype",
    "constraints">: nonemptyList $ owl "DatatypeRestriction_Constraint"]

datatypeRestriction_ConstrainingFacet :: Binding
datatypeRestriction_ConstrainingFacet = define "DatatypeRestriction_ConstrainingFacet" $
  T.union [
    "xmlSchema">:
      note ("XML Schema constraining facets are treated as a special case in this model " ++
            "(not in the OWL 2 specification itself) because they are particularly common") $
      xsd "ConstrainingFacet",
    "other">: rdf "Iri"]

datatypeRestriction_Constraint :: Binding
datatypeRestriction_Constraint = define "DatatypeRestriction_Constraint" $ T.record [
  "constrainingFacet">: owl "DatatypeRestriction_ConstrainingFacet",
  "restrictionValue">: rdf "Literal"]

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

-- DifferentIndividuals := 'DifferentIndividuals' '(' axiomAnnotations Individual Individual { Individual } ')'
differentIndividuals :: Binding
differentIndividuals = define "DifferentIndividuals" $ withAnns [
  "individuals">: twoOrMoreList $ owl "Individual"]

-- DisjointClasses := 'DisjointClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
disjointClasses :: Binding
disjointClasses = define "DisjointClasses" $ withAnns [
  "classes">: twoOrMoreList $ owl "ClassExpression"]

-- DisjointDataProperties := 'DisjointDataProperties' '(' axiomAnnotations DataPropertyExpression DataPropertyExpression { DataPropertyExpression } ')'
disjointDataProperties :: Binding
disjointDataProperties = define "DisjointDataProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "DataPropertyExpression"]

-- DisjointObjectProperties := 'DisjointObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
disjointObjectProperties :: Binding
disjointObjectProperties = define "DisjointObjectProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "ObjectPropertyExpression"]

-- DisjointUnion := 'DisjointUnion' '(' axiomAnnotations Class disjointClassExpressions ')'
-- disjointClassExpressions := ClassExpression ClassExpression { ClassExpression }
disjointUnion :: Binding
disjointUnion = define "DisjointUnion" $
  see "https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions" $
  withAnns [
    "class">: owl "Class",
    "classes">: twoOrMoreList $ owl "ClassExpression"]

-- EquivalentClasses := 'EquivalentClasses' '(' axiomAnnotations ClassExpression ClassExpression { ClassExpression } ')'
equivalentClasses :: Binding
equivalentClasses = define "EquivalentClasses" $ withAnns [
  "classes">: twoOrMoreList $ owl "ClassExpression"]

-- EquivalentDataProperties := 'EquivalentDataProperties' '(' axiomAnnotations DataPropertyExpression DataPropertyExpression { DataPropertyExpression } ')'
equivalentDataProperties :: Binding
equivalentDataProperties = define "EquivalentDataProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "DataPropertyExpression"]

-- EquivalentObjectProperties := 'EquivalentObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
equivalentObjectProperties :: Binding
equivalentObjectProperties = define "EquivalentObjectProperties" $ withAnns [
  "properties">: twoOrMoreList $ owl "ObjectPropertyExpression"]

-- FunctionalDataProperty := 'FunctionalDataProperty' '(' axiomAnnotations DataPropertyExpression ')'
functionalDataProperty :: Binding
functionalDataProperty = define "FunctionalDataProperty" $ withAnns [
  "property">: owl "DataPropertyExpression"]

-- FunctionalObjectProperty := 'FunctionalObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
functionalObjectProperty :: Binding
functionalObjectProperty = objectPropertyConstraint "FunctionalObjectProperty"

-- HasKey := 'HasKey' '(' axiomAnnotations ClassExpression '(' { ObjectPropertyExpression } ')' '(' { DataPropertyExpression } ')' ')'
hasKey :: Binding
hasKey = define "HasKey" $
  see "https://www.w3.org/TR/owl2-syntax/#Keys" $
  withAnns [
    "class">: owl "ClassExpression",
    "objectProperties">: T.list $ owl "ObjectPropertyExpression",
    "dataProperties">: T.list $ owl "DataPropertyExpression"]

-- Individual := NamedIndividual | AnonymousIndividual
individual :: Binding
individual = define "Individual" $ T.union [
  "named">: owl "NamedIndividual",
  "anonymous">: owl "AnonymousIndividual"]

-- InverseFunctionalObjectProperty := 'InverseFunctionalObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
inverseFunctionalObjectProperty :: Binding
inverseFunctionalObjectProperty = objectPropertyConstraint "InverseFunctionalObjectProperty"

-- InverseObjectProperties := 'InverseObjectProperties' '(' axiomAnnotations ObjectPropertyExpression ObjectPropertyExpression ')'
inverseObjectProperties :: Binding
inverseObjectProperties = define "InverseObjectProperties" $ withAnns [
  "property1">: owl "ObjectPropertyExpression",
  "property2">: owl "ObjectPropertyExpression"]

-- InverseObjectProperty := 'ObjectInverseOf' '(' ObjectProperty ')'
inverseObjectProperty :: Binding
inverseObjectProperty = define "InverseObjectProperty" $ T.wrap $ owl "ObjectProperty"

-- IrreflexiveObjectProperty := 'IrreflexiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
irreflexiveObjectProperty :: Binding
irreflexiveObjectProperty = objectPropertyConstraint "IrreflexiveObjectProperty"

-- NamedIndividual := IRI
namedIndividual :: Binding
namedIndividual = define "NamedIndividual" $ T.wrap T.unit

-- NegativeDataPropertyAssertion := 'NegativeDataPropertyAssertion' '(' axiomAnnotations DataPropertyExpression sourceIndividual targetValue ')'
negativeDataPropertyAssertion :: Binding
negativeDataPropertyAssertion = define "NegativeDataPropertyAssertion" $ withAnns [
  "property">: owl "DataPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- NegativeObjectPropertyAssertion := 'NegativeObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression sourceIndividual targetIndividual ')'
negativeObjectPropertyAssertion :: Binding
negativeObjectPropertyAssertion = define "NegativeObjectPropertyAssertion" $ withAnns [
  "property">: owl "ObjectPropertyExpression",
  "source">: owl "Individual",
  "target">: owl "Individual"]

-- ObjectAllValuesFrom := 'ObjectAllValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
objectAllValuesFrom :: Binding
objectAllValuesFrom = define "ObjectAllValuesFrom" $ T.record [
  "property">: owl "ObjectPropertyExpression",
  "class">: owl "ClassExpression"]

-- ObjectComplementOf := 'ObjectComplementOf' '(' ClassExpression ')'
objectComplementOf :: Binding
objectComplementOf = define "ObjectComplementOf" $ T.wrap $ owl "ClassExpression"

-- ObjectExactCardinality := 'ObjectExactCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectExactCardinality :: Binding
objectExactCardinality = define "ObjectExactCardinality" $
  see "https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectHasSelf := 'ObjectHasSelf' '(' ObjectPropertyExpression ')'
objectHasSelf :: Binding
objectHasSelf = define "ObjectHasSelf" $ T.wrap $ owl "ObjectPropertyExpression"

-- ObjectHasValue := 'ObjectHasValue' '(' ObjectPropertyExpression Individual ')'
objectHasValue :: Binding
objectHasValue = define "ObjectHasValue" $ T.record [
  "property">: owl "ObjectPropertyExpression",
  "individual">: owl "Individual"]

-- ObjectIntersectionOf := 'ObjectIntersectionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
objectIntersectionOf :: Binding
objectIntersectionOf = define "ObjectIntersectionOf" $ T.wrap $ twoOrMoreList $ owl "ClassExpression"

-- ObjectMaxCardinality := 'ObjectMaxCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectMaxCardinality :: Binding
objectMaxCardinality = define "ObjectMaxCardinality" $
  see "https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectMinCardinality := 'ObjectMinCardinality' '(' nonNegativeInteger ObjectPropertyExpression [ ClassExpression ] ')'
objectMinCardinality :: Binding
objectMinCardinality = define "ObjectMinCardinality" $
  see "https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality" $
  T.record [
    "bound">: nonNegativeInteger,
    "property">: owl "ObjectPropertyExpression",
    "class">: T.list $ owl "ClassExpression"]

-- ObjectOneOf := 'ObjectOneOf' '(' Individual { Individual }')'
objectOneOf :: Binding
objectOneOf = define "ObjectOneOf" $ T.wrap $ nonemptyList $ owl "Individual"

-- ObjectProperty := IRI
objectProperty :: Binding
objectProperty = define "ObjectProperty" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Properties" $ T.wrap T.unit

-- ObjectPropertyAssertion := 'ObjectPropertyAssertion' '(' axiomAnnotations ObjectPropertyExpression sourceIndividual targetIndividual ')'
objectPropertyAssertion :: Binding
objectPropertyAssertion = define "ObjectPropertyAssertion" $ withAnns [
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

-- ObjectPropertyDomain := 'ObjectPropertyDomain' '(' axiomAnnotations ObjectPropertyExpression ClassExpression ')'
objectPropertyDomain :: Binding
objectPropertyDomain = define "ObjectPropertyDomain" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain" $
  withAnns [
    "property">: owl "ObjectPropertyExpression",
    "domain">: owl "ClassExpression"]

-- ObjectPropertyExpression := ObjectProperty | InverseObjectProperty
objectPropertyExpression :: Binding
objectPropertyExpression = define "ObjectPropertyExpression" $ T.union [
  "object">: owl "ObjectProperty",
  "inverseObject">: owl "InverseObjectProperty"]

-- ObjectPropertyRange := 'ObjectPropertyRange' '(' axiomAnnotations ObjectPropertyExpression ClassExpression ')'
objectPropertyRange :: Binding
objectPropertyRange = define "ObjectPropertyRange" $
  see "https://www.w3.org/TR/owl2-syntax/#Object_Property_Range" $
  withAnns [
    "property">: owl "ObjectPropertyExpression",
    "range">: owl "ClassExpression"]

-- ObjectSomeValuesFrom := 'ObjectSomeValuesFrom' '(' ObjectPropertyExpression ClassExpression ')'
objectSomeValuesFrom :: Binding
objectSomeValuesFrom = define "ObjectSomeValuesFrom" $ T.record [
  "property">: owl "ObjectPropertyExpression",
  "class">: owl "ClassExpression"]

-- ObjectUnionOf := 'ObjectUnionOf' '(' ClassExpression ClassExpression { ClassExpression } ')'
objectUnionOf :: Binding
objectUnionOf = define "ObjectUnionOf" $ T.wrap $ twoOrMoreList $ owl "ClassExpression"

-- ReflexiveObjectProperty := 'ReflexiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
reflexiveObjectProperty :: Binding
reflexiveObjectProperty = objectPropertyConstraint "ReflexiveObjectProperty"

-- sourceIndividual := Individual
-- targetIndividual := Individual
-- targetValue := Literal
-- SameIndividual := 'SameIndividual' '(' axiomAnnotations Individual Individual { Individual } ')'
sameIndividual :: Binding
sameIndividual = define "SameIndividual" $ withAnns [
  "individuals">: twoOrMoreList $ owl "Individual"]

-- SubAnnotationPropertyOf := 'SubAnnotationPropertyOf' '(' axiomAnnotations subAnnotationProperty superAnnotationProperty ')'
subAnnotationPropertyOf :: Binding
subAnnotationPropertyOf = define "SubAnnotationPropertyOf" $ withAnns [
  "subProperty">: owl "AnnotationProperty",
  "superProperty">: owl "AnnotationProperty"]

-- subAnnotationProperty := AnnotationProperty
-- superAnnotationProperty := AnnotationProperty

-- SubClassOf := 'SubClassOf' '(' axiomAnnotations subClassExpression superClassExpression ')'
-- subClassExpression := ClassExpression
-- superClassExpression := ClassExpression
subClassOf :: Binding
subClassOf = define "SubClassOf" $ withAnns [
  "subClass">: owl "ClassExpression",
  "superClass">: owl "ClassExpression"]

-- SubDataPropertyOf := 'SubDataPropertyOf' '(' axiomAnnotations subDataPropertyExpression superDataPropertyExpression ')'
subDataPropertyOf :: Binding
subDataPropertyOf = define "SubDataPropertyOf" $ withAnns [
  "subProperty">: owl "DataPropertyExpression",
  "superProperty">: owl "DataPropertyExpression"]
-- subDataPropertyExpression := DataPropertyExpression
-- superDataPropertyExpression := DataPropertyExpression

-- SubObjectPropertyOf := 'SubObjectPropertyOf' '(' axiomAnnotations subObjectPropertyExpression superObjectPropertyExpression ')'
subObjectPropertyOf :: Binding
subObjectPropertyOf = define "SubObjectPropertyOf" $ withAnns [
  "subProperty">: nonemptyList $ owl "ObjectPropertyExpression",
  "superProperty">: owl "ObjectPropertyExpression"]
-- subObjectPropertyExpression := ObjectPropertyExpression | propertyExpressionChain
-- propertyExpressionChain := 'ObjectPropertyChain' '(' ObjectPropertyExpression ObjectPropertyExpression { ObjectPropertyExpression } ')'
-- superObjectPropertyExpression := ObjectPropertyExpression

-- SymmetricObjectProperty := 'SymmetricObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
symmetricObjectProperty :: Binding
symmetricObjectProperty = objectPropertyConstraint "SymmetricObjectProperty"

-- TransitiveObjectProperty := 'TransitiveObjectProperty' '(' axiomAnnotations ObjectPropertyExpression ')'
transitiveObjectProperty :: Binding
transitiveObjectProperty = objectPropertyConstraint "TransitiveObjectProperty"
