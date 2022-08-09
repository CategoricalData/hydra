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

    def = datatype coreContext hydraEvaluationName
    
    elements = [

      def "Coder" $
        doc "An encoder and decoder; a qualified bidirectional transformation between instances of two types" $
        lambda "a" $ lambda "b" $ record [
          "encode">: ("a" --> evaluation "Result" @@ "b"),
          "decode">: ("b" --> evaluation "Result" @@ "a")],

      def "Context" $
        doc "A pointed set of graph modules; a graph in the logical sense" $
        lambda "m" $ record [
          "graphs">: graph "GraphSet" @@ "m",
          "elements">: Types.map (core "Name") (graph "Element" @@ "m"),
          "functions">: Types.map (core "Name") (evaluation "PrimitiveFunction" @@ "m"),
          "strategy">: evaluation "EvaluationStrategy",
          "annotations">: evaluation "AnnotationClass" @@ "m",
          "trace">: list string],

      def "AnnotationClass" $
        doc "A typeclass-like construct providing common functions for working with annotations" $
        lambda "m" $ record [
          "default">: "m",
          "equal">: "m" --> "m" --> boolean,
          "compare">: "m" --> "m" --> core "Comparison",
          "show">: "m" --> string,
          "read">: string --> optional "m",
          
          -- TODO: simplify
          "termExpr">:
            core "Term" @@ "m" --> core "Term" @@ "m",
          "termMeta">:
            core "Term" @@ "m" --> "m",
          "typeExpr">:
            core "Type" @@ "m" --> core "Type" @@ "m",
          "typeMeta">:
            core "Type" @@ "m" --> "m",
          "termDescription">:
            evaluation "Context" @@ "m" --> core "Term" @@ "m" --> evaluation "Result" @@ optional string,
          "typeDescription">:
            evaluation "Context" @@ "m" --> core "Type" @@ "m" --> evaluation "Result" @@ optional string,
          "termType">:
            evaluation "Context" @@ "m" --> core "Term" @@ "m" --> evaluation "Result" @@ optional (core "Type" @@ "m"),
          "setTermDescription">:
            evaluation "Context" @@ "m" --> optional string --> core "Term" @@ "m" --> core "Term" @@ "m",
          "setTermType">:
            evaluation "Context" @@ "m" --> optional (core "Type" @@ "m") --> core "Term" @@ "m" --> core "Term" @@ "m",
          "typeOf">:
            evaluation "Context" @@ "m" --> "m" --> evaluation "Result" @@ optional (core "Type" @@ "m"),
          "setTypeOf">:
            evaluation "Context" @@ "m" --> optional (core "Type" @@ "m") --> "m" --> "m"],

      def "EvaluationStrategy" $
        doc "Settings which determine how terms are evaluated" $
        record [
          "opaqueTermVariants">: set (core "TermVariant")],

      def "InputSpec" $
        doc "A helper object for specifying and unmarshalling an argument to a primitive function" $
        lambda "a" $ lambda "m" $ record [
          "type">: core "Type" @@ "m",
          "unmarshal">: core "Term" @@ "m" --> evaluation "Result" @@ "a"],

      def "OutputSpec" $
        doc "A helper object for specifying and marshalling the output of a primitive function" $
        lambda "a" $ lambda "m" $ record [
          "type">: core "Type" @@ "m",
          "marshal">: "a" --> core "Term" @@ "m"],

      def "PrimitiveFunction" $
        doc "A built-in function" $
        lambda "m" $ record [
          "name">: core "Name",
          "type">: core "FunctionType" @@ "m",
          "implementation">:
            list (core "Term" @@ "m") --> (evaluation "Result" @@ (core "Term" @@ "m"))],

      def "Result" $
        doc "A qualified result; success with a value or failure with an error message" $
        lambda "m" $ union [
          "success">: "m",
          "failure">: string],

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"]]
