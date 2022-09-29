{-# LANGUAGE OverloadedStrings #-}

module Hydra.Impl.Haskell.Sources.Evaluation where

import Hydra.Impl.Haskell.Sources.Core
import Hydra.Impl.Haskell.Sources.Graph

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Standard


hydraEvaluationModule :: Module Meta
hydraEvaluationModule = Module ns elements [hydraGraphModule] $
    Just "Abstractions for evaluation and transformations"
  where
    ns = Namespace "hydra/evaluation"
    core = nsref $ moduleNamespace hydraCoreModule
    evaluation = nsref ns
    graph = nsref $ moduleNamespace hydraGraphModule

    def = datatype ns

    elements = [
      def "AnnotationClass" $
        doc "A typeclass-like construct providing common functions for working with annotations" $
        lambda "m" $ record [
          "default">: "m",
          "equal">: "m" --> "m" --> boolean,
          "compare">: "m" --> "m" --> core "Comparison",
          "show">: "m" --> string,
          "read">: string --> optional "m",

          -- TODO: simplify
          "termMeta">:
            core "Term" @@ "m" --> "m",
          "typeMeta">:
            core "Type" @@ "m" --> "m",
          "termDescription">:
            core "Term" @@ "m" --> evaluation "Flow" @@ (evaluation "Context" @@ "m") @@ optional string,
          "typeDescription">:
            core "Type" @@ "m" --> evaluation "Flow" @@ (evaluation "Context" @@ "m") @@ optional string,
          "termType">:
            core "Term" @@ "m" --> evaluation "Flow" @@ (evaluation "Context" @@ "m") @@ optional (core "Type" @@ "m"),
          "setTermDescription">:
            evaluation "Context" @@ "m" --> optional string --> core "Term" @@ "m" --> core "Term" @@ "m",
          "setTermType">:
            evaluation "Context" @@ "m" --> optional (core "Type" @@ "m") --> core "Term" @@ "m" --> core "Term" @@ "m",
          "typeOf">:
            "m" --> evaluation "Flow" @@ (evaluation "Context" @@ "m") @@ optional (core "Type" @@ "m"),
          "setTypeOf">:
            optional (core "Type" @@ "m") --> "m" --> "m"],

      def "Coder" $
        doc "An encoder and decoder; a qualified bidirectional transformation between instances of two types" $
        lambda "s" $ lambda "a" $ lambda "b" $ record [
          "encode">: ("a" --> evaluation "Flow" @@ "s" @@ "b"),
          "decode">: ("b" --> evaluation "Flow" @@ "s" @@ "a")],

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"],

      def "Context" $
        doc "An environment containing a graph together with primitive functions and other necessary components for evaluation" $
        lambda "m" $ record [
          "graph">: graph "Graph" @@ "m",
          "functions">: Types.map (core "Name") (evaluation "PrimitiveFunction" @@ "m"),
          "strategy">: evaluation "EvaluationStrategy",
          "annotations">: evaluation "AnnotationClass" @@ "m"],

      def "EvaluationStrategy" $
        doc "Settings which determine how terms are evaluated" $
        record [
          "opaqueTermVariants">: set (core "TermVariant")],

      def "Flow" $
        doc "A variant of the State monad with built-in logging and error handling" $
        lambda "s" $ lambda "a" $
        function "s" (evaluation "Trace" --> evaluation "FlowWrapper" @@ "s" @@ "a"),

      def "FlowWrapper" $
        lambda "s" $ lambda "a" $ record [
          "value">: optional "a",
          "state">: "s",
          "trace">: evaluation "Trace"],

      def "Meta" $
        doc "A built-in metadata container for terms" $
        record [
          "annotations">:
            doc "A map of annotation names to annotation values" $
            Types.map string (core "Term" @@ evaluation "Meta")],

      def "PrimitiveFunction" $
        doc "A built-in function" $
        lambda "m" $ record [
          "name">: core "Name",
          "type">: core "FunctionType" @@ "m",
          "implementation">:
            list (core "Term" @@ "m") --> evaluation "Flow" @@ (evaluation "Context" @@ "m") @@ (core "Term" @@ "m")],

      def "TermCoder" $
        doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms" $
        lambda "m" $ lambda "a" $ record [
          "type">: core "Type" @@ "m",
          "coder">: evaluation "Coder" @@ (evaluation "Context" @@ "m") @@ (core "Term" @@ "m") @@ "a"],

      def "Trace" $
        doc "A container for logging and error information" $
        record [
          "stack">: list string,
          "messages">: list string,
          "other">: Types.map string (core "Literal")]]
