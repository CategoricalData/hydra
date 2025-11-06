{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Coders where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms                 as Terms
import           Hydra.Dsl.Types                 as Types
import qualified Hydra.Sources.Kernel.Types.Core as Core
import qualified Data.List                       as L
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import qualified Data.Maybe                      as Y

import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Mantle as Mantle


module_ :: Module
module_ = Module ns elements [Graph.module_, Compute.module_] [Core.module_] $
    Just "Abstractions for paired transformations between languages"
  where
    ns = Namespace "hydra.coders"
    core = typeref $ moduleNamespace Core.module_
    compute = typeref $ moduleNamespace Compute.module_
    graph = typeref $ moduleNamespace Graph.module_
    mantle = typeref $ moduleNamespace Mantle.module_
    coders = typeref ns

    def = datatype ns

    elements = [

      def "AdapterContext" $
        doc "An evaluation context together with a source language and a target language" $
        record [
          "graph">:
            doc "The underlying graph of elements and primitives" $
            graph "Graph",
          "language">:
            doc "The language being encoded or decoded" $
            coders "Language",
          "adapters">:
            doc "A map of type names to adapters for those types" $
            Types.map (core "Name") (compute "Adapter"
            @@ coders "AdapterContext" @@ coders "AdapterContext"
            @@ core "Type" @@ core "Type"
            @@ core "Term" @@ core "Term")],

      def "CoderDirection" $
        doc "Indicates either the 'out' or the 'in' direction of a coder" $
        enum [
          "encode",
          "decode"],

      def "Language" $
        doc "A named language together with language-specific constraints" $
        record [
          "name">:
            doc "The unique name of the language" $
            coders "LanguageName",
          "constraints">:
            doc "The constraints which characterize the language" $
            coders "LanguageConstraints"],

      def "LanguageConstraints" $
        doc "A set of constraints on valid type and term expressions, characterizing a language" $
        record [
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
            core "Type" --> boolean],

      def "LanguageName" $
        doc "The unique name of a language" $
        wrap string,

      def "SymmetricAdapter" $
        doc "A bidirectional encoder which maps between the same type and term languages on either side" $
        forAlls ["s", "t", "v"] $ compute "Adapter" @@ "s" @@ "s" @@ "t" @@ "t" @@ "v" @@ "v",

      def "TraversalOrder" $
        doc "Specifies either a pre-order or post-order traversal" $
        union [
          "pre">: doc "Pre-order traversal" unit,
          "post">: doc "Post-order traversal" unit],

      def "TypeAdapter" $
        doc "A function which maps a Hydra type to a symmetric adapter between types and terms" $
        core "Type" --> compute "Flow" @@ coders "AdapterContext" @@
          (coders "SymmetricAdapter" @@ coders "AdapterContext" @@ core "Type" @@ core "Term")]
