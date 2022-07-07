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
    shacl = nominal . qualify shaclModelName . Name
    rdf = nominal . qualify rdfSyntaxName . Name

    elements = [

      def "CommonConstraints" $
        doc "Any of a number of constraint parameters which can be applied either to node or property shapes" $
        record [
          field "and" $
            doc "See https://www.w3.org/TR/shacl/#AndConstraintComponent" $
            list $ shacl "Shape",

          field "closed" $
            doc "See https://www.w3.org/TR/shacl/#ClosedConstraintComponent"
            boolean,

          field "datatype" $
            doc "See https://www.w3.org/TR/shacl/#DatatypeConstraintComponent" $
            set $ rdf "Iri",

          field "hasValue" $
            doc ("Specifies the condition that at least one value node is equal to the given RDF term. " ++
                 "See https://www.w3.org/TR/shacl/#HasValueConstraintComponent") $
            set $ rdf "Node",

          field "ignoredProperties" $
            doc "See https://www.w3.org/TR/shacl/#ClosedConstraintComponent" $
            list $ element $ rdf "Property",

          field "in" $
            doc ("Specifies the condition that each value node is a member of a provided SHACL list. " ++
                 "See https://www.w3.org/TR/shacl/#InConstraintComponent") $
            list $ rdf "Node",

          field "node" $
            doc "See https://www.w3.org/TR/shacl/#NodeConstraintComponent" $
            set $ shacl "NodeShape",

          field "not" $
            doc "See https://www.w3.org/TR/shacl/#NotConstraintComponent" $
            set $ shacl "Shape",

          field "property" $
            doc "See https://www.w3.org/TR/shacl/#PropertyConstraintComponent" $
            set $ shacl "PropertyShape",

          field "or" $
            doc "See https://www.w3.org/TR/shacl/#OrConstraintComponent" $
            list $ shacl "Shape",

          field "xone" $
            doc "See https://www.w3.org/TR/shacl/#XoneConstraintComponent" $
            list $ shacl "Shape"],

      def "CommonProperties" $
        doc "Common constraint parameters and other properties for SHACL shapes" $
        record [
          field "constraints" $
            doc "Common constraint parameters attached to this shape"
            $ shacl "CommonConstraints",

          field "deactivated" $
            doc "See https://www.w3.org/TR/shacl/#deactivated" $
            boolean,

          field "message" $
            doc "See https://www.w3.org/TR/shacl/#message" $
            rdf "LangStrings",

          field "severity" $
            doc "See https://www.w3.org/TR/shacl/#severity" $
            shacl "Severity",

          field "targetClass" $
            doc "See https://www.w3.org/TR/shacl/#targetClass" $
            set $ element $ rdf "RdfsClass",

          field "targetNode" $
            doc "See https://www.w3.org/TR/shacl/#targetNode" $
            optional $ rdf "IriOrLiteral",

          field "targetObjectsOf" $
            doc "See https://www.w3.org/TR/shacl/#targetObjectsOf" $
            set $ element $ rdf "Property",

          field "targetSubjectsOf" $
            doc "See https://www.w3.org/TR/shacl/#targetSubjectsOf" $
            set $ element $ rdf "Property"],

      def "NodeShape" $
        doc "A SHACL node shape. See https://www.w3.org/TR/shacl/#node-shapes" $
        record [
          field "common" $ shacl "CommonProperties"],

      def "PropertyShape" $
        doc "A SHACL property shape. See https://www.w3.org/TR/shacl/#property-shapes" $
        record [
          field "common" $ shacl "CommonProperties",

          field "constraints" $
            doc "Any property shape -specific constraint parameters" $
            shacl "PropertyShapeConstraints",

          field "defaultValue" $
            doc "See https://www.w3.org/TR/shacl/#defaultValue" $
            optional $ rdf "Node",

          field "description" $
            doc "See https://www.w3.org/TR/shacl/#name" $
            rdf "LangStrings",

          field "name" $
            doc "See https://www.w3.org/TR/shacl/#name" $
            rdf "LangStrings",

          field "order" $
            doc "See https://www.w3.org/TR/shacl/#order" $
            optional int32,

          field "path"  $ rdf "Resource"], -- TODO
          -- Note: sh:group is omitted for now, for lack of a clear definition of PropertyGroup

      def "PropertyShapeConstraints" $
        doc "A number of constraint parameters which are specific to property shapes, and cannot be applied to node shapes" $
        record [
          field "maxCount" $
            doc ("The maximum cardinality. Node shapes cannot have any value for sh:maxCount. " ++
                 "See https://www.w3.org/TR/shacl/#MaxCountConstraintComponent") $
            optional int32,

          field "minCount" $
            doc ("The minimum cardinality. Node shapes cannot have any value for sh:minCount. " ++
                 "See https://www.w3.org/TR/shacl/#MinCountConstraintComponent") $
            optional int32],

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
