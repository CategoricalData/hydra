{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Variants where

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
    Just ("Variant types which describe the structure of Hydra core types and terms.")
  where
    ns = Namespace "hydra.variants"
    core = typeref $ moduleNamespace Core.module_
    meta = typeref ns
    def = datatype ns

    elements = [

      def "EliminationVariant" $
        doc "The identifier of an elimination constructor" $
        enum [
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
          "pair",
          "record",
          "set",
          "typeApplication",
          "typeLambda",
          "union",
          "unit",
          "variable",
          "wrap"],

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
          "pair",
          "record",
          "set",
          "union",
          "unit",
          "variable",
          "wrap"]]
