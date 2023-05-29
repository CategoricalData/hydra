{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Mantle where

import Hydra.Kernel
import Hydra.Dsl.Annotations
import Hydra.Sources.Core
import Hydra.Dsl.Types as Types


hydraMantleModule :: Module Kv
hydraMantleModule = Module ns elements [hydraCoreModule] $
    Just "A set of types which supplement hydra/core with type variants, graphs, and elements"
  where
    ns = Namespace "hydra/mantle"
    core = nsref $ moduleNamespace hydraCoreModule
    mantle = nsref ns
    def = datatype ns

    elements = [

      def "EliminationVariant" $
        doc "The identifier of an elimination constructor" $
        enum [
          "list",
          "optional",
          "record",
          "union",
          "wrap"],

      def "FunctionVariant" $
        doc "The identifier of a function constructor" $
        enum [
          "elimination",
          "lambda",
          "primitive"],

      def "LiteralVariant" $
        doc "The identifier of a literal constructor" $
        enum [
          "binary",
          "boolean",
          "float",
          "integer",
          "string"],

      def "Precision" $
        doc "Numeric precision: arbitrary precision, or precision to a specified number of bits" $
        union [
          "arbitrary">: unit,
          "bits">: int32],

      def "TermVariant" $
        doc "The identifier of a term expression constructor" $
        enum [
          "annotated",
          "application",
          "function",
          "let",
          "list",
          "literal",
          "map",
          "optional",
          "product",
          "record",
          "set",
          "stream",
          "sum",
          "union",
          "variable",
          "wrap"],

      def "TypeScheme" $
        doc "A type expression together with free type variables occurring in the expression" $
        lambda "a" $ record [
          "variables">: list $ core "Name",
          "type">: core "Type" @@ "a"],

      def "TypeVariant" $
        doc "The identifier of a type constructor" $
        enum [
          "annotated",
          "application",
          "function",
          "lambda",
          "list",
          "literal",
          "map",
          "optional",
          "product",
          "record",
          "set",
          "stream",
          "sum",
          "union",
          "variable",
          "wrap"],

      def "TypedTerm" $
        doc "A type together with an instance of the type" $
        lambda "a" $ record [
          "type">: core "Type" @@ "a",
          "term">: core "Term" @@ "a"]]
