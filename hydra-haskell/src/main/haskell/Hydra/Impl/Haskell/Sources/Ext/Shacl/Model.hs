module Hydra.Impl.Haskell.Sources.Ext.Shacl.Model where

import Hydra.Impl.Haskell.Sources.Core

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard
import Hydra.Impl.Haskell.Sources.Ext.Rdf.Syntax


shaclModelModule :: Module Meta
shaclModelModule = Module shaclModel [rdfSyntaxModule]

shaclModelName :: GraphName
shaclModelName = GraphName "hydra/ext/shacl/model"

shaclModel :: Graph Meta
shaclModel = Graph shaclModelName elements (const True) hydraCoreName
  where
    def = datatype shaclModelName
    shacl = nsref shaclModelName
    rdf = nsref rdfSyntaxName

    elements = [

      def "Closed" $
        see "https://www.w3.org/TR/shacl/#ClosedPatterConstraintComponent" $
        record [
          "isClosed">: boolean,
          "ignoredProperties">: optional $ list $ element $ rdf "Property"],

      def "CommonConstraints" $
        doc "Any of a number of constraint parameters which can be applied either to node or property shapes" $
        record [
          "and">:
            see "https://www.w3.org/TR/shacl/#AndConstraintComponent" $
            optional $ list $ shacl "Shape",

          "closed">:
            see "https://www.w3.org/TR/shacl/#ClosedConstraintComponent" $
            optional $ shacl "Closed",

          "class">:
            see "https://www.w3.org/TR/shacl/#ClassConstraintComponent" $
            set $ element $ rdf "RdfsClass",

          "datatype">:
            see "https://www.w3.org/TR/shacl/#DatatypeConstraintComponent" $
            optional $ rdf "Iri",

          "disjoint">:
            see "https://www.w3.org/TR/shacl/#DisjointConstraintComponent" $
            set $ element $ rdf "Property",

          "equals">:
            see "https://www.w3.org/TR/shacl/#EqualsConstraintComponent" $
            set $ element $ rdf "Property",

          "hasValue">:
            doc ("Specifies the condition that at least one value node is equal to the given RDF term. " ++
                 "See https://www.w3.org/TR/shacl/#HasValueConstraintComponent") $
            set $ rdf "Node",

          "in">:
            doc ("Specifies the condition that each value node is a member of a provided SHACL list. " ++
                 "See https://www.w3.org/TR/shacl/#InConstraintComponent") $
            optional $ list $ rdf "Node",

          "languageIn">:
            see "https://www.w3.org/TR/shacl/#LanguageInConstraintComponent" $
            optional $ list $ rdf "LanguageTag",

          "nodeKind">:
            see "https://www.w3.org/TR/shacl/#NodeKindConstraintComponent" $
            optional $ shacl "NodeKind",

          "node">:
            see "https://www.w3.org/TR/shacl/#NodeConstraintComponent" $
            set $ shacl "NodeShape",

          "not">:
            see "https://www.w3.org/TR/shacl/#NotConstraintComponent" $
            set $ shacl "Shape",

          "maxExclusive">:
            see "https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent" $
            optional $ rdf "Literal",

          "maxInclusive">:
            see "https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent" $
            optional $ rdf "Literal",

          "maxLength">:
            see "https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent" $
            optional bigint,

          "minExclusive">:
            see "https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent" $
            optional $ rdf "Literal",

          "minInclusive">:
            see "https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent" $
            optional $ rdf "Literal",

          "minLength">:
            see "https://www.w3.org/TR/shacl/#MinLengthConstraintComponent" $
            optional bigint,

          "pattern">:
            see "https://www.w3.org/TR/shacl/#PatternConstraintComponent" $
            optional $ shacl "Pattern",

          "property">:
            see "https://www.w3.org/TR/shacl/#PropertyConstraintComponent" $
            set $ shacl "PropertyShape",

          "or">:
            see "https://www.w3.org/TR/shacl/#OrConstraintComponent" $
            optional $ list $ shacl "Shape",

          "xone">:
            see "https://www.w3.org/TR/shacl/#XoneConstraintComponent" $
            optional $ list $ shacl "Shape"],

      def "CommonProperties" $
        doc "Common constraint parameters and other properties for SHACL shapes" $
        record [
          "constraints">:
            doc "Common constraint parameters attached to this shape"
            $ shacl "CommonConstraints",

          "deactivated">:
            see "https://www.w3.org/TR/shacl/#deactivated" $
            optional boolean,

          "message">:
            see "https://www.w3.org/TR/shacl/#message" $
            rdf "LangStrings",

          "severity">:
            see "https://www.w3.org/TR/shacl/#severity" $
            shacl "Severity",

          "targetClass">:
            see "https://www.w3.org/TR/shacl/#targetClass" $
            set $ element $ rdf "RdfsClass",

          "targetNode">:
            see "https://www.w3.org/TR/shacl/#targetNode" $
            set $ rdf "IriOrLiteral",

          "targetObjectsOf">:
            see "https://www.w3.org/TR/shacl/#targetObjectsOf" $
            set $ element $ rdf "Property",

          "targetSubjectsOf">:
            see "https://www.w3.org/TR/shacl/#targetSubjectsOf" $
            set $ element $ rdf "Property"],

      def "NodeKind" $ union [
        "blankNode">: doc "A blank node" unit,
        "iri">: doc "An IRI" unit,
        "literal">: doc "A literal" unit,
        "blankNodeOrIri">: doc "A blank node or an IRI" unit,
        "blankNodeOrLiteral">: doc "A blank node or a literal" unit,
        "iriOrLiteral">: doc "An IRI or a literal" unit],

      def "NodeShape" $
        doc "A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes" $
        record [
          "common">: shacl "CommonProperties"],

      def "Pattern" $
        doc "A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent" $
        record [
          "regex">: string,
          "flags">: optional string],

      def "PropertyShape" $
        doc "A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes" $
        record [
          "common">: shacl "CommonProperties",

          "constraints">:
            doc "Any property shape -specific constraint parameters" $
            shacl "PropertyShapeConstraints",

          "defaultValue">:
            see "https://www.w3.org/TR/shacl/#defaultValue" $
            optional $ rdf "Node",

          "description">:
            see "https://www.w3.org/TR/shacl/#name" $
            rdf "LangStrings",

          "name">:
            see "https://www.w3.org/TR/shacl/#name" $
            rdf "LangStrings",

          "order">:
            see "https://www.w3.org/TR/shacl/#order" $
            optional bigint,

          "path">: rdf "Resource"], -- TODO
          -- Note: sh:group is omitted for now, for lack of a clear definition of PropertyGroup

      def "PropertyShapeConstraints" $
        doc "A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes" $
        record [

          "lessThan">:
            see "https://www.w3.org/TR/shacl/#LessThanConstraintComponent" $
            set $ element $ rdf "Property",

          "lessThanOrEquals">:
            see "https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent" $
            set $ element $ rdf "Property",

          "maxCount">:
            doc ("The maximum cardinality. Node shapes cannot have any value for sh:maxCount. " ++
                 "See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent") $
            optional bigint,

          "minCount">:
            doc ("The minimum cardinality. Node shapes cannot have any value for sh:minCount. " ++
                 "See https://www.w3.org/TR/shacl/#MinCountConstraintComponent") $
            optional bigint,

          "uniqueLang">:
            see "https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent" $
            optional boolean,

          "qualifiedValueShape">:
            see "https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent" $
            optional $ shacl "QualifiedValueShape"],

      def "QualifiedValueShape" $
        see "https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent" $
        record [
          "shape">: shacl "Shape",
          "qualifiedManCount">: bigint,
          "qualifiedMinCount">: bigint,
          "qualifiedValueShapesDisjoint">: optional boolean],

      def "Severity" $ union [
        "info">: doc "A non-critical constraint violation indicating an informative message" unit,
        "warning">: doc "A non-critical constraint violation indicating a warning" unit,
        "violation">: doc "A constraint violation" unit],

      def "Shape" $
        doc "A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes" $
        union [
          "node">: shacl "NodeShape",
          "property">: shacl "PropertyShape"],

      def "ShapesGraph" $
        doc ("An RDF graph containing zero or more shapes that is passed into a SHACL validation process " ++
             "so that a data graph can be validated against the shapes") $
        set $ shacl "Shape"]
