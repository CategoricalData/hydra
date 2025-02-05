{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier0.Mantle where

-- Standard type-level Tier-0 imports
import qualified Data.List             as L
import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Maybe            as Y
import           Hydra.Dsl.Annotations
import           Hydra.Dsl.Bootstrap
import qualified Hydra.Dsl.Terms       as Terms
import           Hydra.Dsl.Types       as Types
import           Hydra.Sources.Core


hydraMantleModule :: Module
hydraMantleModule = Module ns elements [hydraCoreModule] [hydraCoreModule] $
    Just "A set of types which supplement hydra/core with variants and accessors"
  where
    ns = Namespace "hydra/mantle"
    core = typeref $ moduleNamespace hydraCoreModule
    mantle = typeref ns
    def = datatype ns

    elements = [

      def "Either" $
        doc "A disjoint union between a 'left' type and a 'right' type" $
        lambda "a" $ lambda "b" $ union [
          "left">: "a",
          "right">: "b"],

      def "EliminationVariant" $
        doc "The identifier of an elimination constructor" $
        enum [
          "list",
          "optional",
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
          "listFold">: unit,
          "optionalCasesNothing">: unit,
          "optionalCasesJust">: unit,
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

      def "TypeConstraint" $
        doc "An assertion that two types can be unified into a single type" $
        record [
          "left">: core "Type",
          "right">: core "Type",
          "comment">:
            doc "An optional description of the type constraint. This may be used for tracing or debugging." $
            optional string],

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
          "sum",
          "union",
          "variable",
          "wrap"]]
