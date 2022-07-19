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
          field "isClosed" boolean,
          field "ignoredProperties" $ optional $ list $ element $ rdf "Property"],

      def "CommonConstraints" $
        doc "Any of a number of constraint parameters which can be applied either to node or property shapes" $
        record [
          field "and" $
            see "https://www.w3.org/TR/shacl/#AndConstraintComponent" $
            optional $ list $ shacl "Shape",

          field "closed" $
            see "https://www.w3.org/TR/shacl/#ClosedConstraintComponent" $
            optional $ shacl "Closed",

          field "class" $
            see "https://www.w3.org/TR/shacl/#ClassConstraintComponent" $
            set $ element $ rdf "RdfsClass",

          field "datatype" $
            see "https://www.w3.org/TR/shacl/#DatatypeConstraintComponent" $
            optional $ rdf "Iri",

          field "disjoint" $
            see "https://www.w3.org/TR/shacl/#DisjointConstraintComponent" $
            set $ element $ rdf "Property",

          field "equals" $
            see "https://www.w3.org/TR/shacl/#EqualsConstraintComponent" $
            set $ element $ rdf "Property",

          field "hasValue" $
            doc ("Specifies the condition that at least one value node is equal to the given RDF term. " ++
                 "See https://www.w3.org/TR/shacl/#HasValueConstraintComponent") $
            set $ rdf "Node",

          field "in" $
            doc ("Specifies the condition that each value node is a member of a provided SHACL list. " ++
                 "See https://www.w3.org/TR/shacl/#InConstraintComponent") $
            optional $ list $ rdf "Node",

          field "languageIn" $
            see "https://www.w3.org/TR/shacl/#LanguageInConstraintComponent" $
            optional $ list $ rdf "LanguageTag",

          field "nodeKind" $
            see "https://www.w3.org/TR/shacl/#NodeKindConstraintComponent" $
            optional $ shacl "NodeKind",

          field "node" $
            see "https://www.w3.org/TR/shacl/#NodeConstraintComponent" $
            set $ shacl "NodeShape",

          field "not" $
            see "https://www.w3.org/TR/shacl/#NotConstraintComponent" $
            set $ shacl "Shape",

          field "maxExclusive" $
            see "https://www.w3.org/TR/shacl/#MaxExclusiveConstraintComponent" $
            optional $ rdf "Literal",

          field "maxInclusive" $
            see "https://www.w3.org/TR/shacl/#MaxInclusiveConstraintComponent" $
            optional $ rdf "Literal",

          field "maxLength" $
            see "https://www.w3.org/TR/shacl/#MaxLengthConstraintComponent" $
            optional bigint,

          field "minExclusive" $
            see "https://www.w3.org/TR/shacl/#MinExclusiveConstraintComponent" $
            optional $ rdf "Literal",

          field "minInclusive" $
            see "https://www.w3.org/TR/shacl/#MinInclusiveConstraintComponent" $
            optional $ rdf "Literal",

          field "minLength" $
            see "https://www.w3.org/TR/shacl/#MinLengthConstraintComponent" $
            optional bigint,

          field "pattern" $
            see "https://www.w3.org/TR/shacl/#PatternConstraintComponent" $
            optional $ shacl "Pattern",

          field "property" $
            see "https://www.w3.org/TR/shacl/#PropertyConstraintComponent" $
            set $ shacl "PropertyShape",

          field "or" $
            see "https://www.w3.org/TR/shacl/#OrConstraintComponent" $
            optional $ list $ shacl "Shape",

          field "xone" $
            see "https://www.w3.org/TR/shacl/#XoneConstraintComponent" $
            optional $ list $ shacl "Shape"],

      def "CommonProperties" $
        doc "Common constraint parameters and other properties for SHACL shapes" $
        record [
          field "constraints" $
            doc "Common constraint parameters attached to this shape"
            $ shacl "CommonConstraints",

          field "deactivated" $
            see "https://www.w3.org/TR/shacl/#deactivated" $
            optional boolean,

          field "message" $
            see "https://www.w3.org/TR/shacl/#message" $
            rdf "LangStrings",

          field "severity" $
            see "https://www.w3.org/TR/shacl/#severity" $
            shacl "Severity",

          field "targetClass" $
            see "https://www.w3.org/TR/shacl/#targetClass" $
            set $ element $ rdf "RdfsClass",

          field "targetNode" $
            see "https://www.w3.org/TR/shacl/#targetNode" $
            set $ rdf "IriOrLiteral",

          field "targetObjectsOf" $
            see "https://www.w3.org/TR/shacl/#targetObjectsOf" $
            set $ element $ rdf "Property",

          field "targetSubjectsOf" $
            see "https://www.w3.org/TR/shacl/#targetSubjectsOf" $
            set $ element $ rdf "Property"],

      def "NodeKind" $ union [
        field "blankNode" $ doc "A blank node" unit,
        field "iri" $ doc "An IRI" unit,
        field "literal" $ doc "A literal" unit,
        field "blankNodeOrIri" $ doc "A blank node or an IRI" unit,
        field "blankNodeOrLiteral" $ doc "A blank node or a literal" unit,
        field "iriOrLiteral" $ doc "An IRI or a literal" unit],

      def "NodeShape" $
        doc "A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes" $
        record [
          field "common" $ shacl "CommonProperties"],

      def "Pattern" $
        doc "A SHACL pattern. See https://www.w3.org/TR/shacl/#PatternConstraintComponent" $
        record [
          field "regex" string,
          field "flags" $ optional string],

      def "PropertyShape" $
        doc "A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes" $
        record [
          field "common" $ shacl "CommonProperties",

          field "constraints" $
            doc "Any property shape -specific constraint parameters" $
            shacl "PropertyShapeConstraints",

          field "defaultValue" $
            see "https://www.w3.org/TR/shacl/#defaultValue" $
            optional $ rdf "Node",

          field "description" $
            see "https://www.w3.org/TR/shacl/#name" $
            rdf "LangStrings",

          field "name" $
            see "https://www.w3.org/TR/shacl/#name" $
            rdf "LangStrings",

          field "order" $
            see "https://www.w3.org/TR/shacl/#order" $
            optional bigint,

          field "path"  $ rdf "Resource"], -- TODO
          -- Note: sh:group is omitted for now, for lack of a clear definition of PropertyGroup

      def "PropertyShapeConstraints" $
        doc "A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes" $
        record [

          field "lessThan" $
            see "https://www.w3.org/TR/shacl/#LessThanConstraintComponent" $
            set $ element $ rdf "Property",

          field "lessThanOrEquals" $
            see "https://www.w3.org/TR/shacl/#LessThanOrEqualsConstraintComponent" $
            set $ element $ rdf "Property",

          field "maxCount" $
            doc ("The maximum cardinality. Node shapes cannot have any value for sh:maxCount. " ++
                 "See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent") $
            optional bigint,

          field "minCount" $
            doc ("The minimum cardinality. Node shapes cannot have any value for sh:minCount. " ++
                 "See https://www.w3.org/TR/shacl/#MinCountConstraintComponent") $
            optional bigint,

          field "uniqueLang" $
            see "https://www.w3.org/TR/shacl/#UniqueLangConstraintComponent" $
            optional boolean,

          field "qualifiedValueShape" $
            see "https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent" $
            optional $ shacl "QualifiedValueShape"],

      def "QualifiedValueShape" $
        see "https://www.w3.org/TR/shacl/#QualifiedValueShapeConstraintComponent" $
        record [
          field "shape" $ shacl "Shape",
          field "qualifiedManCount" bigint,
          field "qualifiedMinCount" bigint,
          field "qualifiedValueShapesDisjoint" $ optional boolean],

      def "Severity" $ union [
        field "info" $ doc "A non-critical constraint violation indicating an informative message" unit,
        field "warning" $ doc "A non-critical constraint violation indicating a warning" unit,
        field "violation" $ doc "A constraint violation" unit],

      def "Shape" $
        doc "A SHACL node or property shape. See https://www.w3.org/TR/shacl/#shapes" $
        union [
          field "node" $ shacl "NodeShape",
          field "property" $ shacl "PropertyShape"],

      def "ShapesGraph" $
        doc ("An RDF graph containing zero or more shapes that is passed into a SHACL validation process " ++
             "so that a data graph can be validated against the shapes") $
        set $ shacl "Shape"]
