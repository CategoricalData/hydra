{-# LANGUAGE OverloadedStrings #-}

module Hydra.Sources.Kernel.Types.Testing where

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

import qualified Hydra.Sources.Kernel.Types.Mantle as Mantle


module_ :: Module
module_ = Module ns elements [Mantle.module_] [Core.module_] $
    Just "A model for unit testing"
  where
    ns = Namespace "hydra.testing"
    def = datatype ns
    core = typeref $ moduleNamespace Core.module_
    mantle = typeref $ moduleNamespace Mantle.module_
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

      def "InferenceFailureTestCase" $
        doc "A test case providing a term for which type inference is expected to fail" $
        record [
          "input">: core "Term"],

      def "InferenceTestCase" $
        doc "A test case which performs type inference on a given term and compares the result with an expected type scheme" $
        record [
          "input">: core "Term",
          "output">: core "TypeScheme"],

      def "Tag" $ wrap string,

      def "TestCase" $
        doc "A simple test case with an input and an expected output" $
        union [
          "caseConversion">: testing "CaseConversionTestCase",
          "evaluation">: testing "EvaluationTestCase",
          "inference">: testing "InferenceTestCase",
          "inferenceFailure">: testing "InferenceFailureTestCase"],

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
