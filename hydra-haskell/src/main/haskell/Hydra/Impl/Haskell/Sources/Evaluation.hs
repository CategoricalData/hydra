module Hydra.Impl.Haskell.Sources.Evaluation where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Graph

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraEvaluationModule :: Module Meta
hydraEvaluationModule = Module hydraEvaluation [hydraCoreModule, hydraGraphModule]

hydraEvaluationName :: GraphName
hydraEvaluationName = GraphName "hydra/evaluation"

hydraEvaluation :: Graph Meta
hydraEvaluation = Graph hydraEvaluationName elements (const True) hydraCoreName
  where
    core = nominal . qualify hydraCoreName . Name
    graph = nominal . qualify hydraGraphName . Name
    evaluation = nominal . qualify hydraEvaluationName . Name

    def = datatype hydraEvaluationName
    elements = [

      def "Context" $
        doc "A pointed set of graph modules; a graph in the logical sense" $
        universal "m" $ record [
          field "graphs" $ universal "m" $ graph "GraphSet",
          field "elements" $ Types.map (core "Name") (universal "m" $ graph "Element"),
          field "functions" $ Types.map (core "Name") (universal "m" $ evaluation "PrimitiveFunction"),
          field "strategy" $ evaluation "EvaluationStrategy",
          field "descriptionOf" $ function (variable "m") (universal "(Maybe String)" $ evaluation "Result"), -- TODO: hack
          field "typeOf" $ function (variable "m") (universal "(Maybe (Core.Type m))" $ evaluation "Result"), -- TODO: hack
          field "setDescriptionOf" $ function (optional string) (function (variable "m") (variable "m")),
          field "setTypeOf" $ function (optional $ universal "m" $ core "Type") (function (variable "m") (variable "m"))],

      def "EvaluationStrategy" $
        doc "Settings which determine how terms are evaluated" $
        record [
          field "opaqueDataVariants" (set $ core "DataVariant")],

      def "InputSpec" $
        doc "A helper object for specifying and unmarshalling an argument to a primitive function" $
        universal "a" $ universal "m" $ record [
          field "type" $ universal "m" $ core "Type",
          field "unmarshal" $ function (universal "m" $ core "Data") (universal "a" $ evaluation "Result")],

      def "OutputSpec" $
        doc "A helper object for specifying and marshalling the output of a primitive function" $
        universal "a" $ universal "m" $ record [
          field "type" $ universal "m" $ core "Type",
          field "marshal" $ function (variable "a") (universal "m" $ core "Data")],

      def "PrimitiveFunction" $
        doc "A built-in function" $
        universal "m" $ record [
          field "name" $ core "Name",
          field "type" $ universal "m" $ core "FunctionType",
          field "implementation" $ function (list $ universal "m" $ core "Data")
            (universal "(Core.Data m)" $ evaluation "Result")], -- TODO: this is a hack

      def "Result" $
        doc "A qualified result; success with a value or failure with an error message" $
        universal "m" $ union [
          field "success" $ variable "m",
          field "failure" string],

      def "Step" $
        doc "A qualified bidirectional transformation" $
        universal "a" $ universal "b" $ record [
          field "out" $ function (variable "a") (variable "Result b"), -- TODO: this is a hack
          field "in" $ function (variable "b") (variable "Result a")], -- TODO: this is a hack

      def "StepDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a step" $
        enum [
          "out",
          "in"]]
