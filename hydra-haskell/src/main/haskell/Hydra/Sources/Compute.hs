{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Compute where

import Hydra.Kernel
import Hydra.Dsl.Types as Types
import Hydra.Dsl.Standard
import Hydra.Sources.Core
import Hydra.Sources.Mantle


hydraComputeModule :: Module Meta
hydraComputeModule = Module ns elements [hydraMantleModule] $
    Just "Abstractions for evaluation and transformations"
  where
    ns = Namespace "hydra/compute"
    core = nsref $ moduleNamespace hydraCoreModule
    mantle = nsref $ moduleNamespace hydraMantleModule
    compute = nsref ns

    def = datatype ns

    elements = [
      def "Adapter" $
        doc "A two-level bidirectional encoder which adapts types to types and terms to terms" $
        lambda "s1" $ lambda "s2" $ lambda "t1" $ lambda "t2" $ lambda "v1" $ lambda "v2" $ record [
          "isLossy">: boolean,
          "source">: variable "t1",
          "target">: variable "t2",
          "coder">: compute "Coder" @@ "s1" @@ "s2" @@ "v1" @@ "v2"],

      def "AdapterContext" $
        doc "An evaluation context together with a source language and a target language" $
        lambda "m" $ record [
          "evaluation">: apply (compute "Context") (variable "m"),
          "source">: apply (compute "Language") (variable "m"),
          "target">: apply (compute "Language") (variable "m")],

      def "AnnotationClass" $
        doc "A typeclass-like construct providing common functions for working with annotations" $
        lambda "m" $ record [
          "default">: "m",
          "equal">: "m" --> "m" --> boolean,
          "compare">: "m" --> "m" --> mantle "Comparison",
          "show">: "m" --> string,
          "read">: string --> optional "m",

          -- TODO: simplify
          "termMeta">:
            core "Term" @@ "m" --> "m",
          "typeMeta">:
            core "Type" @@ "m" --> "m",
          "termDescription">:
            core "Term" @@ "m" --> compute "Flow" @@ (compute "Context" @@ "m") @@ optional string,
          "typeDescription">:
            core "Type" @@ "m" --> compute "Flow" @@ (compute "Context" @@ "m") @@ optional string,
          "termType">:
            core "Term" @@ "m" --> compute "Flow" @@ (compute "Context" @@ "m") @@ optional (core "Type" @@ "m"),
          "setTermDescription">:
            compute "Context" @@ "m" --> optional string --> core "Term" @@ "m" --> core "Term" @@ "m",
          "setTermType">:
            compute "Context" @@ "m" --> optional (core "Type" @@ "m") --> core "Term" @@ "m" --> core "Term" @@ "m",
          "typeOf">:
            "m" --> compute "Flow" @@ (compute "Context" @@ "m") @@ optional (core "Type" @@ "m"),
          "setTypeOf">:
            optional (core "Type" @@ "m") --> "m" --> "m"],

      def "Coder" $
        doc "An encoder and decoder; a bidirectional flow between two types" $
        lambda "s1" $ lambda "s2" $ lambda "v1" $ lambda "v2" $ record [
          "encode">: ("v1" --> compute "Flow" @@ "s1" @@ "v2"),
          "decode">: ("v2" --> compute "Flow" @@ "s2" @@ "v1")],

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"],

      def "Context" $
        doc "An environment containing a graph together with primitive functions and other necessary components for evaluation" $
        lambda "m" $ record [
          "graph">:
            doc "The graph itself" $
            mantle "Graph" @@ "m",
          "functions">:
            doc "All supported primitive functions, by name" $
            Types.map (core "Name") (compute "PrimitiveFunction" @@ "m"),
          "strategy">:
            doc "The evaluation strategy which is to be used in this context" $
            compute "EvaluationStrategy",
          "annotations">:
            doc "The annotation class which is supported in this context" $
            compute "AnnotationClass" @@ "m"],

      def "EvaluationStrategy" $
        doc "Settings which determine how terms are evaluated" $
        record [
          "opaqueTermVariants">: set (mantle "TermVariant")],

      def "Flow" $
        doc "A variant of the State monad with built-in logging and error handling" $
        lambda "s" $ lambda "a" $
        function "s" (compute "Trace" --> compute "FlowState" @@ "s" @@ "a"),

      def "FlowState" $
        doc "The result of evaluating a Flow" $
        lambda "s" $ lambda "a" $ record [
          "value">: optional "a",
          "state">: "s",
          "trace">: compute "Trace"],

      def "Language" $
        doc "A named language together with language-specific constraints" $
        lambda "m" $ record [
          "name">: compute "LanguageName",
          "constraints">: apply (compute "LanguageConstraints") (variable "m")],

      def "LanguageConstraints" $
        doc "A set of constraints on valid type and term expressions, characterizing a language" $
        lambda "m" $ record [
          "eliminationVariants">:
            doc "All supported elimination variants" $
            Types.set $ mantle "EliminationVariant",
          "literalVariants">:
            doc "All supported literal variants" $
            Types.set $ mantle "LiteralVariant",
          "floatTypes">:
            doc "All supported float types" $
            Types.set $ core "FloatType",
          "functionVariants">:
            doc "All supported function variants" $
            Types.set $ mantle "FunctionVariant",
          "integerTypes">:
            doc "All supported integer types" $
            Types.set $ core "IntegerType",
          "termVariants">:
            doc "All supported term variants" $
            Types.set $ mantle "TermVariant",
          "typeVariants">:
            doc "All supported type variants" $
            Types.set $ mantle "TypeVariant",
          "types">:
            doc "A logical set of types, as a predicate which tests a type for inclusion" $
            core "Type" @@ "m" --> boolean],

      def "LanguageName" $
        doc "The unique name of a language" string,

      def "Meta" $
        doc "A built-in metadata container for terms" $
        record [
          "annotations">:
            doc "A map of annotation names to annotation values" $
            Types.map string (core "Term" @@ compute "Meta")],

      def "PrimitiveFunction" $
        doc "A built-in function" $
        lambda "m" $ record [
          "name">:
            doc "The unique name of the primitive function" $
            core "Name",
          "type">:
            doc "The type signature of the primitive function" $
            core "FunctionType" @@ "m",
          "implementation">:
            doc "A concrete implementation of the primitive function" $
            list (core "Term" @@ "m") --> compute "Flow" @@ (compute "Context" @@ "m") @@ (core "Term" @@ "m")],

      def "TermCoder" $
        doc "A type together with a coder for mapping terms into arguments for primitive functions, and mapping computed results into terms" $
        lambda "m" $ lambda "a" $ record [
          "type">: core "Type" @@ "m",
          "coder">: compute "Coder" @@ (compute "Context" @@ "m") @@ (compute "Context" @@ "m") @@ (core "Term" @@ "m") @@ "a"],

      def "Trace" $
        doc "A container for logging and error information" $
        record [
          "stack">: list string,
          "messages">: list string,
          "other">:
            doc "A map of string keys to arbitrary terms as values, for application-specific use" $
            Types.map string (core "Term" @@ compute "Meta")],

      def "TraversalOrder" $
        doc "Specifies either a pre-order or post-order traversal" $
        union [
          "pre">: doc "Pre-order traversal" unit,
          "post">: doc "Post-order traversal" unit]]
