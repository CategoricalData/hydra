{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Tier1.Testing where

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

import Hydra.Sources.Tier1.Mantle


hydraTestingModule :: Module
hydraTestingModule = Module ns elements [hydraCoreModule, hydraMantleModule] [hydraCoreModule] $
    Just "A model for unit testing"
  where
    ns = Namespace "hydra/testing"
    def = datatype ns
    core = typeref $ moduleNamespace hydraCoreModule
    mantle = typeref $ moduleNamespace hydraMantleModule
    testing = typeref ns

    elements = [

      def "EvaluationStyle" $
        doc "One of two evaluation styles: eager or lazy" $
        enum ["eager", "lazy"],

      def "CaseConversionTestCase" $
        doc "A test case which checks that strings are converted between different case conventions correctly" $
        record [
          "fromConvention">: mantle "CaseConvention",
          "toConvention">: mantle "CaseConvention",
          "fromString">: string,
          "toString">: string],

      def "EvaluationTestCase" $
        doc "A test case which evaluates (reduces) a given term and compares it with the expected result" $
        record [
          "evaluationStyle">: testing "EvaluationStyle",
          "input">: core "Term",
          "output">: core "Term"],

      def "InferenceTestCase" $
        doc "A test case which performs type inference on a given term and compares the result with an expected type scheme" $
        record [
          "input">: core "Term",
          "output">: core "TypeScheme"],

      def "Tag" string,

      def "TestCase" $
        doc "A simple test case with an input and an expected output" $
        union [
          "caseConversion">: testing "CaseConversionTestCase",
          "evaluation">: testing "EvaluationTestCase",
          "inference">: testing "InferenceTestCase"],

      def "TestCaseWithMetadata" $
        doc "One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags" $
        record [
          "name">: string,
          "case">: testing "TestCase",
          "description">: optional string,
          "tags">: list $ testing "Tag"],

      def "TestGroup" $
        doc "A collection of test cases with a name and optional description" $
        record [
          "name">: string,
          "description">: optional string,
          "subgroups">: list (testing "TestGroup"),
          "cases">: list (testing "TestCaseWithMetadata")]]
