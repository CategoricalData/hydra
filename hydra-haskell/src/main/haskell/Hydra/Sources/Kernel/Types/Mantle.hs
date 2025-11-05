{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Mantle where

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


module_ :: Module
module_ = Module ns elements [Core.module_] [Core.module_] $
    Just ("A set of types which supplement hydra.core, but are not referenced by hydra.core.")
  where
    ns = Namespace "hydra.mantle"
    core = typeref $ moduleNamespace Core.module_
    mantle = typeref ns
    def = datatype ns

    elements = [

      -- TODO: find another home for CaseConvention; it doesn't really belong in hydra.mantle
      def "CaseConvention" $
        Types.enum ["camel", "pascal", "lowerSnake", "upperSnake"],

      def "Comparison" $
        doc "An equality judgement: less than, equal to, or greater than" $
        enum [
          "lessThan",
          "equalTo",
          "greaterThan"],

      def "Either" $
        doc "A disjoint union between a 'left' type and a 'right' type" $
        forAlls ["a", "b"] $ union [
          "left">: "a",
          "right">: "b"],

      def "EliminationVariant" $
        doc "The identifier of an elimination constructor" $
        enum [
          "product",
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
          "either",
          "function",
          "let",
          "list",
          "literal",
          "map",
          "maybe",
          "product",
          "record",
          "set",
          "sum",
          "typeApplication",
          "typeLambda",
          "union",
          "unit",
          "variable",
          "wrap"],

      def "TypeClass" $
        doc "Any of a small number of built-in type classes" $
        enum [
          "equality",
          "ordering"],

      def "TypeVariant" $
        doc "The identifier of a type constructor" $
        enum [
          "annotated",
          "application",
          "either",
          "forall",
          "function",
          "list",
          "literal",
          "map",
          "maybe",
          "product",
          "record",
          "set",
          "sum",
          "union",
          "unit",
          "variable",
          "wrap"]]
