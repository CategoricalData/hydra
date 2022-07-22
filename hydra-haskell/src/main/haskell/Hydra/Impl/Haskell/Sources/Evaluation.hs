{-# LANGUAGE OverloadedStrings #-}

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
          field "encode" ("a" --> evaluation "Result" @@ "b"),
          field "decode" ("b" --> evaluation "Result" @@ "a")],

      def "Context" $
        doc "A pointed set of graph modules; a graph in the logical sense" $
        lambda "m" $ record [
          field "graphs" $ graph "GraphSet" @@ "m",
          field "elements" $ Types.map (core "Name") (graph "Element" @@ "m"),
          field "functions" $ Types.map (core "Name") (evaluation "PrimitiveFunction" @@ "m"),
          field "strategy" $ evaluation "EvaluationStrategy",
          field "annotations" $ evaluation "AnnotationClass" @@ "m"],

      def "AnnotationClass" $
        doc "A typeclass-like construct providing common functions for working with annotations" $
        lambda "m" $ record [
          field "default" "m",
          field "equal" $ "m" --> "m" --> boolean,
          field "compare" $ "m" --> "m" --> core "Comparison",
          field "show" $ "m" --> string,
          field "read" $ string --> optional "m",
          
          -- TODO: simplify
          field "termExpr" $
            core "Term" @@ "m" --> core "Term" @@ "m",
          field "termMeta" $
            core "Term" @@ "m" --> "m",
          field "typeExpr" $
            core "Type" @@ "m" --> core "Type" @@ "m",
          field "typeMeta" $
            core "Type" @@ "m" --> "m",
          field "termDescription" $
            evaluation "Context" @@ "m" --> core "Term" @@ "m" --> evaluation "Result" @@ optional string,
          field "typeDescription" $
            evaluation "Context" @@ "m" --> core "Type" @@ "m" --> evaluation "Result" @@ optional string,
          field "termType" $
            evaluation "Context" @@ "m" --> core "Term" @@ "m" --> evaluation "Result" @@ optional (core "Type" @@ "m"),
          field "setTermDescription" $
            evaluation "Context" @@ "m" --> optional string --> core "Term" @@ "m" --> core "Term" @@ "m",
          field "setTermType" $
            evaluation "Context" @@ "m" --> optional (core "Type" @@ "m") --> core "Term" @@ "m" --> core "Term" @@ "m",
          field "typeOf" $
            evaluation "Context" @@ "m" --> "m" --> evaluation "Result" @@ optional (core "Type" @@ "m"),
          field "setTypeOf" $
            evaluation "Context" @@ "m" --> optional (core "Type" @@ "m") --> "m" --> "m"],

      def "EvaluationStrategy" $
        doc "Settings which determine how terms are evaluated" $
        record [
          field "opaqueTermVariants" (set $ core "TermVariant")],

      def "InputSpec" $
        doc "A helper object for specifying and unmarshalling an argument to a primitive function" $
        lambda "a" $ lambda "m" $ record [
          field "type" $ core "Type" @@ "m",
          field "unmarshal" $ core "Term" @@ "m" --> evaluation "Result" @@ "a"],

      def "OutputSpec" $
        doc "A helper object for specifying and marshalling the output of a primitive function" $
        lambda "a" $ lambda "m" $ record [
          field "type" $ core "Type" @@ "m",
          field "marshal" $ "a" --> core "Term" @@ "m"],

      def "PrimitiveFunction" $
        doc "A built-in function" $
        lambda "m" $ record [
          field "name" $ core "Name",
          field "type" $ core "FunctionType" @@ "m",
          field "implementation" $
            list (core "Term" @@ "m") --> (evaluation "Result" @@ (core "Term" @@ "m"))],

      def "Result" $
        doc "A qualified result; success with a value or failure with an error message" $
        lambda "m" $ union [
          field "success" "m",
          field "failure" string],

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"]]
