{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Coders where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Sources.Compute
import Hydra.Sources.Graph
import Hydra.Sources.Mantle
import Hydra.Dsl.Types as Types


hydraCodersModule :: Module Kv
hydraCodersModule = Module ns elements [hydraMantleModule, hydraComputeModule, hydraGraphModule] $
    Just "Abstractions for paired transformations between languages"
  where
    ns = Namespace "hydra/coders"
    core = nsref $ moduleNamespace hydraCoreModule
    compute = nsref $ moduleNamespace hydraComputeModule
    graph = nsref $ moduleNamespace hydraGraphModule
    mantle = nsref $ moduleNamespace hydraMantleModule
    coders = nsref ns

    def = datatype ns

    elements = [

      def "AdapterContext" $
        doc "An evaluation context together with a source language and a target language" $
        lambda "a" $ record [
          "graph">: apply (graph "Graph") (variable "a"),
          "language">: apply (coders "Language") (variable "a"),
          "adapters">: Types.map (core "Name") (compute "Adapter"
            @@ (coders "AdapterContext" @@ "a") @@ (coders "AdapterContext" @@ "a")
            @@ (core "Type" @@ "a") @@ (core "Type" @@ "a")
            @@ (core "Term" @@ "a") @@ (core "Term" @@ "a"))],

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"],

      def "Language" $
        doc "A named language together with language-specific constraints" $
        lambda "a" $ record [
          "name">: coders "LanguageName",
          "constraints">: apply (coders "LanguageConstraints") (variable "a")],

      def "LanguageConstraints" $
        doc "A set of constraints on valid type and term expressions, characterizing a language" $
        lambda "a" $ record [
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
            core "Type" @@ "a" --> boolean],

      def "LanguageName" $
        doc "The unique name of a language" string,

      def "TraversalOrder" $
        doc "Specifies either a pre-order or post-order traversal" $
        union [
          "pre">: doc "Pre-order traversal" unit,
          "post">: doc "Post-order traversal" unit]]
