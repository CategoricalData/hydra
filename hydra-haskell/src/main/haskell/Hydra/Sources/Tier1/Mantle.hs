{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1.Mantle where

-- Standard type-level Tier-1 imports
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Tier0.Core
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y


hydraMantleModule :: Module
hydraMantleModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just ("A set of types which supplement hydra.core with variants and accessors."
      ++ " Currently contains miscellaneous additional types including CaseConvention and Either.")
  where
    ns = Namespace "hydra.mantle"
    core = typeref $ moduleNamespace hydraCoreModule
    mantle = typeref ns
    def = datatype ns

    elements = [

      def "AccessorEdge" $
        record [
          "source">: mantle "AccessorNode",
          "path">: mantle "AccessorPath",
          "target">: mantle "AccessorNode"],

      def "AccessorGraph" $
        record [
          "nodes">: list $ mantle "AccessorNode",
          "edges">: list $ mantle "AccessorEdge"],

      def "AccessorNode" $
        record [
          "name">: core "Name",
          "label">: string,
          "id" >: string],

      def "AccessorPath" $
        list $ mantle "TermAccessor",

      def "CaseConvention" $
        Types.enum ["camel", "pascal", "lowerSnake", "upperSnake"],

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

      def "TermAccessor" $
        doc "A function which maps from a term to a particular immediate subterm" $
        union [
          "annotatedSubject">: unit,
          "applicationFunction">: unit,
          "applicationArgument">: unit,
          "lambdaBody">: unit,
          "unionCasesDefault">: unit,
          "unionCasesBranch">: core "Name",
          "letEnvironment">: unit,
          "letBinding">: core "Name",
          "listElement">: int32,
          "mapKey">: int32,
          "mapValue">: int32,
          "optionalTerm">: unit,
          "productTerm">: int32,
          "recordField">: core "Name",
          "setElement">: int32,
          "sumTerm">: unit,
          "typeAbstractionBody">: unit,
          "typeApplicationTerm">: unit,
          "typedTerm">: unit,
          "injectionTerm">: unit,
          "wrappedTerm">: unit],

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
          "sum",
          "typeAbstraction",
          "typeApplication",
          "typed",
          "union",
          "variable",
          "wrap"],

      def "TypeVariant" $
        doc "The identifier of a type constructor" $
        enum [
          "annotated",
          "application",
          "forall",
          "function",
          "list",
          "literal",
          "map",
          "optional",
          "product",
          "record",
          "set",
          "sum",
          "union",
          "variable",
          "wrap"]]
