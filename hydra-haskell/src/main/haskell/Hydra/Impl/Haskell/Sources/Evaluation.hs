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
    core = nsref hydraCoreName
    graph = nsref hydraGraphName
    evaluation = nsref hydraEvaluationName

    def = datatype hydraEvaluationName
    
    elements = [

      def "Coder" $
        doc "An encoder and decoder; a qualified bidirectional transformation between instances of two types" $
        lambda "a" $ lambda "b" $ record [
          field "encode" $ function (variable "a") (apply (evaluation "Result") (variable "b")),
          field "decode" $ function (variable "b") (apply (evaluation "Result") (variable "a"))],

      def "Context" $
        doc "A pointed set of graph modules; a graph in the logical sense" $
        lambda "m" $ record [
          field "graphs" $ apply (graph "GraphSet") (variable "m"),
          field "elements" $ Types.map (core "Name") (apply (graph "Element") (variable "m")),
          field "functions" $ Types.map (core "Name") (apply (evaluation "PrimitiveFunction") (variable "m")),
          field "strategy" $ evaluation "EvaluationStrategy",
          field "annotations" $ apply (evaluation "AnnotationClass") (variable "m")],

      def "AnnotationClass" $
        doc "A typeclass-like construct providing common functions for working with annotations" $
        lambda "m" $ record [
          field "default" $ variable "m",
          field "equal" $ function (variable "m") (function (variable "m") boolean),
          field "compare" $ function (variable "m") (function (variable "m") (core "Comparison")),
          field "show" $ function (variable "m") string,
          field "read" $ function string (optional $ variable "m"),
          
          -- TODO: simplify
          field "termDescription" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (apply (core "Term") (variable "m"))
              (apply (evaluation "Result") (optional string))),
          field "typeDescription" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (apply (core "Type") (variable "m"))
              (apply (evaluation "Result") (optional string))),
          field "termType" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (apply (core "Term") (variable "m"))
              (apply (evaluation "Result") (optional $ apply (core "Type") (variable "m")))),
          field "setTermDescription" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (optional string)
              (function (apply (core "Term") (variable "m")) (apply (core "Term") (variable "m")))),
          field "setTermType" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (optional $ apply (core "Type") (variable "m"))
              (function (apply (core "Term") (variable "m")) (apply (core "Term") (variable "m")))),
          field "typeOf" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (variable "m")
              (apply (evaluation "Result") (optional $ apply (core "Type") (variable "m")))),
          field "setTypeOf" $ function
            (apply (evaluation "Context") (variable "m"))
            (function
              (optional $ apply (core "Type") (variable "m"))
              (function (variable "m") (variable "m")))],
        
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

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"]]
