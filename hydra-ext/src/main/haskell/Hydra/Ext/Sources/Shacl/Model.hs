module Hydra.Ext.Sources.Shacl.Model where

-- Standard imports for type-level sources outside of the kernel
import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core

-- Additional imports
import qualified Hydra.Ext.Sources.Rdf.Syntax as RdfSyntax


ns :: Namespace
ns = Namespace "hydra.ext.org.w3.shacl.model"

define :: String -> Type -> Binding
define = defineType ns

shacl :: String -> Type
shacl = typeref ns

rdf :: String -> Type
rdf = typeref $ moduleNamespace RdfSyntax.module_

module_ :: Module
module_ = Module ns elements [RdfSyntax.module_] [Core.module_] $
    Just "A SHACL syntax model. See https://www.w3.org/TR/shacl"
  where
    elements = [
      closed,
      commonConstraint,
      commonProperties,
      definition,
      nodeKind,
      nodeShape,
      pattern_,
      propertyShape,
      propertyShapeConstraint,
      qualifiedValueShape,
      reference,
      severity,
      shape,
      shapesGraph]

closed :: Binding
closed = define "Closed" $
  see "https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent" $
  T.record [
    "isClosed">: T.boolean,
    "ignoredProperties">: T.optional $ T.set $ rdf "Property"]

commonConstraint :: Binding
commonConstraint = define "CommonConstraint" $
  doc "Any of a number of constraint parameters which can be applied either to node or property shapes" $
  T.union [
    "and">:
      see "https://www.w3.org/TR/shacl/#AndConstraintComponent" $
      T.set $ shacl "Reference" @@ shacl "Shape",

    "closed">:
      see "https://www.w3.org/TR/shacl/#ClosedConstraintComponent" $
      shacl "Closed",

    "class">:
      see "https://www.w3.org/TR/shacl/#ClassConstraintComponent" $
      T.set $ rdf "RdfsClass",

    "datatype">:
      see "https://www.w3.org/TR/shacl/#DatatypeConstraintComponent" $
      rdf "Iri",

    "disjoint">:
      see "https://www.w3.org/TR/shacl/#DisjointConstraintComponent" $
      T.set $ rdf "Property",

    "equals">:
      see "https://www.w3.org/TR/shacl/#EqualsConstraintComponent" $
      T.set $ rdf "Property",

    "hasValue">:
      doc ("Specifies the condition that at least one value node is equal to the given RDF term. " ++
           "See https://www.w3.org/TR/shacl/#HasValueConstraintComponent") $
      T.set $ rdf "Node",

    "in">:
      doc ("Specifies the condition that each value node is a member of a provided SHACL list. " ++
           "See https://www.w3.org/TR/shacl/#InConstraintComponent") $
      T.list $ rdf "Node",

    "languageIn">:
      see "https://www.w3.org/TR/shacl/#LanguageInConstraintComponent" $
      T.set $ rdf "LanguageTag",

    "nodeKind">:
      see "https://www.w3.org/TR/shacl/#NodeKindConstraintComponent" $
      shacl "NodeKind",

    "node">:
      see "https://www.w3.org/TR/shacl/#NodeConstraintComponent" $
      T.set $ shacl "Reference" @@ shacl "NodeShape",

    "not">:
      see "https://www.w3.org/TR/shacl/#NotConstraintComponent" $
      T.set $ shacl "Reference" @@ shacl "Shape",

    "maxExclusive">:
      see "https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent" $
      rdf "Literal",

    "maxInclusive">:
      see "https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent" $
      rdf "Literal",

    "maxLength">:
      see "https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent" $
      T.bigint,

    "minExclusive">:
      see "https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent" $
      rdf "Literal",

    "minInclusive">:
      see "https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent" $
      rdf "Literal",

    "minLength">:
      see "https://www.w3.org/TR/shacl/#MinLengthConstraintComponent" $
      T.bigint,

    "pattern">:
      see "https://www.w3.org/TR/shacl/#PatternConstraintComponent" $
      shacl "Pattern",

    "property">:
      see "https://www.w3.org/TR/shacl/#PropertyConstraintComponent" $
      T.set $ shacl "Reference" @@ shacl "PropertyShape",

    "or">:
      see "https://www.w3.org/TR/shacl/#OrConstraintComponent" $
      T.set $ shacl "Reference" @@ shacl "Shape",

    "xone">:
      see "https://www.w3.org/TR/shacl/#XoneConstraintComponent" $
      T.set $ shacl "Reference" @@ shacl "Shape"]

commonProperties :: Binding
commonProperties = define "CommonProperties" $
  doc "Common constraint parameters and other properties for SHACL shapes" $
  T.record [
    "constraints">:
      doc "Common constraint parameters attached to this shape"
      $ T.set $ shacl "CommonConstraint",

    "deactivated">:
      see "https://www.w3.org/TR/shacl/#deactivated" $
      T.optional T.boolean,

    "message">:
      see "https://www.w3.org/TR/shacl/#message" $
      rdf "LangStrings",

    "severity">:
      see "https://www.w3.org/TR/shacl/#severity" $
      shacl "Severity",

    "targetClass">:
      see "https://www.w3.org/TR/shacl/#targetClass" $
      T.set $ rdf "RdfsClass",

    "targetNode">:
      see "https://www.w3.org/TR/shacl/#targetNode" $
      T.set $ rdf "IriOrLiteral",

    "targetObjectsOf">:
      see "https://www.w3.org/TR/shacl/#targetObjectsOf" $
      T.set $ rdf "Property",

    "targetSubjectsOf">:
      see "https://www.w3.org/TR/shacl/#targetSubjectsOf" $
      T.set $ rdf "Property"]

definition :: Binding
definition = define "Definition" $
  doc "An instance of a type like sh:Shape or sh:NodeShape, together with a unique IRI for that instance" $
  T.forAll "a" $ T.record [
    "iri">: rdf "Iri",
    "target">: "a"]

nodeKind :: Binding
nodeKind = define "NodeKind" $ T.union [
  "blankNode">: doc "A blank node" T.unit,
  "iri">: doc "An IRI" T.unit,
  "literal">: doc "A literal" T.unit,
  "blankNodeOrIri">: doc "A blank node or an IRI" T.unit,
  "blankNodeOrLiteral">: doc "A blank node or a literal" T.unit,
  "iriOrLiteral">: doc "An IRI or a literal" T.unit]

nodeShape :: Binding
nodeShape = define "NodeShape" $
  doc "A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes" $
  T.record [
    "common">: shacl "CommonProperties"]

pattern_ :: Binding
pattern_ = define "Pattern" $
  doc "A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent" $
  T.record [
    "regex">: T.string,
    "flags">: T.optional T.string]

propertyShape :: Binding
propertyShape = define "PropertyShape" $
  doc "A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes" $
  T.record [
    "common">: shacl "CommonProperties",

    "constraints">:
      doc "Any property shape -specific constraint parameters" $
      T.set $ shacl "PropertyShapeConstraint",

    "defaultValue">:
      see "https://www.w3.org/TR/shacl/#defaultValue" $
      T.optional $ rdf "Node",

    "description">:
      see "https://www.w3.org/TR/shacl/#name" $
      rdf "LangStrings",

    "name">:
      see "https://www.w3.org/TR/shacl/#name" $
      rdf "LangStrings",

    "order">:
      see "https://www.w3.org/TR/shacl/#order" $
      T.optional T.bigint,

    "path">: rdf "Iri"]
    -- Note: sh:group is omitted for now, for lack of a clear definition of PropertyGroup

propertyShapeConstraint :: Binding
propertyShapeConstraint = define "PropertyShapeConstraint" $
  doc "A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes" $
  T.union [

    "lessThan">:
      see "https://www.w3.org/TR/shacl/#LessThanConstraintComponent" $
      T.set $ rdf "Property",

    "lessThanOrEquals">:
      see "https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent" $
      T.set $ rdf "Property",

    "maxCount">:
      doc ("The maximum cardinality. Node shapes cannot have any value for sh:maxCount. " ++
           "See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent") $
      T.bigint,

    "minCount">:
      doc ("The minimum cardinality. Node shapes cannot have any value for sh:minCount. " ++
           "See https://www.w3.org/TR/shacl/#MinCountConstraintComponent") $
      T.bigint,

    "uniqueLang">:
      see "https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent" $
      T.boolean,

    "qualifiedValueShape">:
      see "https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent" $
      shacl "QualifiedValueShape"]

qualifiedValueShape :: Binding
qualifiedValueShape = define "QualifiedValueShape" $
  see "https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent" $
  T.record [
    "qualifiedValueShape">: shacl "Reference" @@ shacl "Shape",
    "qualifiedMaxCount">: T.bigint,
    "qualifiedMinCount">: T.bigint,
    "qualifiedValueShapesDisjoint">: T.optional T.boolean]

reference :: Binding
reference = define "Reference" $
  doc "Either an instance of a type like sh:Shape or sh:NodeShape, or an IRI which refers to an instance of that type" $
  T.forAll "a" $ T.union [
    "named">: rdf "Iri",
    "anonymous">:
      doc "An anonymous instance"
      "a",
    "definition">:
      doc "An inline definition" $
      shacl "Definition" @@ "a"]

severity :: Binding
severity = define "Severity" $ T.union [
  "info">: doc "A non-critical constraint violation indicating an informative message" T.unit,
  "warning">: doc "A non-critical constraint violation indicating a warning" T.unit,
  "violation">: doc "A constraint violation" T.unit]

shape :: Binding
shape = define "Shape" $
  doc "A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes" $
  T.union [
    "node">: shacl "NodeShape",
    "property">: shacl "PropertyShape"]

shapesGraph :: Binding
shapesGraph = define "ShapesGraph" $
  doc ("An RDF graph containing zero or more shapes that is passed into a SHACL validation process " ++
       "so that a data graph can be validated against the shapes") $
  T.wrap $ T.set $ shacl "Definition" @@ shacl "Shape"
