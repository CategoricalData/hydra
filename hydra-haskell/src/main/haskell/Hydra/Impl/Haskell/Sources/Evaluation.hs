module Hydra.Impl.Haskell.Sources.Evaluation where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Graph

import Hydra.Core
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraEvaluationModule :: Module Meta
hydraEvaluationModule = Module hydraEvaluation [hydraGraphModule]

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
        lambda "m" $ record [
          field "graphs" $ apply (graph "GraphSet") (variable "m"),
          field "elements" $ Types.map (core "Name") (apply (graph "Element") (variable "m")),
          field "functions" $ Types.map (core "Name") (apply (evaluation "PrimitiveFunction") (variable "m")),
          field "strategy" $ evaluation "EvaluationStrategy",

          -- TODO: simplify
          field "description_OfTerm" $ function
            (apply (core "Term") (variable "m"))
            (apply (evaluation "Result") (optional string)),
          field "description_OfType" $ function
            (apply (core "Type") (variable "m"))
            (apply (evaluation "Result") (optional string)),
          field "type_OfTerm" $ function
            (apply (core "Term") (variable "m"))
            (apply (evaluation "Result") (optional $ apply (core "Type") (variable "m"))),
          field "setDescription_OfTerm" $ function
            (optional string)
            (function (apply (core "Term") (variable "m")) (apply (core "Term") (variable "m"))),
          field "setType_OfTerm" $ function
            (optional $ apply (core "Type") (variable "m"))
            (function (apply (core "Term") (variable "m")) (apply (core "Term") (variable "m"))),
          field "typeOf" $ function
            (variable "m")
            (apply (evaluation "Result") (optional $ apply (core "Type") (variable "m"))),
          field "setTypeOf" $ function
            (optional $ apply (core "Type") (variable "m"))
            (function (variable "m") (variable "m"))],

      def "EvaluationStrategy" $
        doc "Settings which determine how terms are evaluated" $
        record [
          field "opaqueTermVariants" (set $ core "TermVariant")],

      def "InputSpec" $
        doc "A helper object for specifying and unmarshalling an argument to a primitive function" $
        lambda "a" $ lambda "m" $ record [
          field "type" $ apply (core "Type") (variable "m"),
          field "unmarshal" $ function
            (apply (core "Term") (variable "m"))
            (apply (evaluation "Result") (variable "a"))],

      def "OutputSpec" $
        doc "A helper object for specifying and marshalling the output of a primitive function" $
        lambda "a" $ lambda "m" $ record [
          field "type" $ apply (core "Type") (variable "m"),
          field "marshal" $ function (variable "a") (apply (core "Term") (variable "m"))],

      def "PrimitiveFunction" $
        doc "A built-in function" $
        lambda "m" $ record [
          field "name" $ core "Name",
          field "type" $ apply (core "FunctionType") (variable "m"),
          field "implementation" $ function (list $ apply (core "Term") (variable "m"))
            (apply (evaluation "Result") (apply (core "Term") (variable "m")))],

      def "Result" $
        doc "A qualified result; success with a value or failure with an error message" $
        lambda "m" $ union [
          field "success" $ variable "m",
          field "failure" string],

      def "Step" $
        doc "A qualified bidirectional transformation" $
        lambda "a" $ lambda "b" $ record [
          field "out" $ function (variable "a") (apply (evaluation "Result") (variable "b")),
          field "in" $ function (variable "b") (apply (evaluation "Result") (variable "a"))],

      def "StepDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a step" $
        enum [
          "out",
          "in"]]
