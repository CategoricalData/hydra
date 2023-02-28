{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Graph where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Compute
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


hydraGraphModule :: Module Kv
hydraGraphModule = Module ns elements [hydraComputeModule] $
    Just "The extension to graphs of Hydra's core type system (hydra/core)"
  where
    ns = Namespace "hydra/graph"
    core = nsref $ moduleNamespace hydraCoreModule
    compute = nsref $ moduleNamespace hydraComputeModule
    graph = nsref ns
    def = datatype ns

    elements = [

      def "AnnotationClass" $
        doc "A typeclass-like construct providing common functions for working with annotations" $
        lambda "m" $ record [
          "default">: "m",
          "equal">: "m" --> "m" --> boolean,
          "compare">: "m" --> "m" --> graph "Comparison",
          "show">: "m" --> string,
          "read">: string --> optional "m",

          -- TODO: simplify
          "termAnnotation">:
            core "Term" @@ "m" --> "m",
          "typeAnnotation">:
            core "Type" @@ "m" --> "m",
          "termDescription">:
            core "Term" @@ "m" --> compute "Flow" @@ (graph "Graph" @@ "m") @@ optional string,
          "typeDescription">:
            core "Type" @@ "m" --> compute "Flow" @@ (graph "Graph" @@ "m") @@ optional string,
          "termType">:
            core "Term" @@ "m" --> compute "Flow" @@ (graph "Graph" @@ "m") @@ optional (core "Type" @@ "m"),
          "setTermDescription">:
            graph "Graph" @@ "m" --> optional string --> core "Term" @@ "m" --> core "Term" @@ "m",
          "setTermType">:
            graph "Graph" @@ "m" --> optional (core "Type" @@ "m") --> core "Term" @@ "m" --> core "Term" @@ "m",
          "typeOf">:
            "m" --> compute "Flow" @@ (graph "Graph" @@ "m") @@ optional (core "Type" @@ "m"),
          "setTypeOf">:
            optional (core "Type" @@ "m") --> "m" --> "m"],

      def "Comparison" $
        doc "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      def "Graph" $
        doc "A graph, or set of name/term bindings together with parameters (annotations, primitives) and a schema graph" $
        lambda "m" $ record [

          -- TODO: remove this; replace it with 'environment'
          "elements">:
            doc "All of the elements in the graph" $
            Types.map (core "Name") (graph "Element" @@ "m"),

          "environment">:
            doc "The lambda environment of this graph context; it indicates whether a variable is bound by a lambda (Nothing) or a let (Just term)" $
            Types.map (core "Name") (optional $ core "Term" @@ "m"),
          "body">:
            doc "The body of the term which generated this context" $
            core "Term" @@ "m",
          "primitives">:
            doc "All supported primitive constants and functions, by name" $
            Types.map (core "Name") (graph "Primitive" @@ "m"),
          "annotations">:
            doc "The annotation class which is supported in this context" $
            graph "AnnotationClass" @@ "m",
          "schema">:
            doc "The schema of this graph. If this parameter is omitted (nothing), the graph is its own schema graph." $
            optional $ graph "Graph" @@ "m"],

      def "Element" $
        doc "A graph element, having a name, data term (value), and schema term (type)" $
        lambda "m" $ record [
          "name">: core "Name",
          "schema">: core "Term" @@ "m",
          "data">: core "Term" @@ "m"],

      def "Primitive" $
        doc "A built-in function" $
        lambda "m" $ record [
          "name">:
            doc "The unique name of the primitive function" $
            core "Name",
          "type">:
            doc "The type signature of the primitive function" $
            core "Type" @@ "m",
          "implementation">:
            doc "A concrete implementation of the primitive function" $
            list (core "Term" @@ "m") --> compute "Flow" @@ Types.product [] @@ (core "Term" @@ "m")],

      def "TermCoder" $
        doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms" $
        lambda "m" $ lambda "a" $ record [
          "type">: core "Type" @@ "m",
          "coder">: compute "Coder" @@ Types.product [] @@ Types.product [] @@ (core "Term" @@ "m") @@ "a"]]
