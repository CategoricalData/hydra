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

import qualified Hydra.Sources.Kernel.Types.Util as Util


module_ :: Module
module_ = Module ns elements [Util.module_] [Core.module_] $
    Just "A model for unit testing"
  where
    ns = Namespace "hydra.testing"
    def = datatype ns
    core = typeref $ moduleNamespace Core.module_
    util = typeref $ moduleNamespace Util.module_
    testing = typeref ns

    elements = [

      def "EvaluationStyle" $
        doc "One of two evaluation styles: eager or lazy" $
        enum ["eager", "lazy"],

      def "CaseConversionTestCase" $
        doc "A test case which checks that strings are converted between different case conventions correctly" $
        record [
          "fromConvention">:
            doc "The source case convention" $
            util "CaseConvention",
          "toConvention">:
            doc "The target case convention" $
            util "CaseConvention",
          "fromString">:
            doc "The input string" $
            string,
          "toString">:
            doc "The expected output string" $
            string],

      def "EtaExpansionTestCase" $
        doc ("A test case which performs eta expansion (adding missing lambda abstractions) on a given term"
          <> " and compares the result with the expected result") $
        record [
          "input">:
            doc "The term to eta expand" $
            core "Term",
          "output">:
            doc "The expected result" $
            core "Term"],

      def "EvaluationTestCase" $
        doc "A test case which evaluates (reduces) a given term and compares it with the expected result" $
        record [
          "evaluationStyle">:
            doc "The evaluation style (eager or lazy)" $
            testing "EvaluationStyle",
          "input">:
            doc "The term to evaluate" $
            core "Term",
          "output">:
            doc "The expected result" $
            core "Term"],

      def "InferenceFailureTestCase" $
        doc "A test case providing a term for which type inference is expected to fail" $
        record [
          "input">:
            doc "The term for which inference should fail" $
            core "Term"],

      def "InferenceTestCase" $
        doc "A test case which performs type inference on a given term and compares the result with an expected type scheme" $
        record [
          "input">:
            doc "The term to infer" $
            core "Term",
          "output">:
            doc "The expected type scheme" $
            core "TypeScheme"],

      def "Tag" $
        doc "A tag for categorizing test cases" $
        wrap string,

      def "TestCase" $
        doc "A simple test case with an input and an expected output" $
        union [
          "caseConversion">:
            doc "A case conversion test" $
            testing "CaseConversionTestCase",
          "etaExpansion">:
            doc "An eta expansion test" $
            testing "EtaExpansionTestCase",
          "evaluation">:
            doc "A term evaluation test" $
            testing "EvaluationTestCase",
          "inference">:
            doc "A type inference test" $
            testing "InferenceTestCase",
          "inferenceFailure">:
            doc "A type inference failure test" $
            testing "InferenceFailureTestCase",
          "typeChecking">:
            doc "A type checking test" $
            testing "TypeCheckingTestCase",
          "typeCheckingFailure">:
            doc "A type checking failure test (currently unused)" $
            testing "TypeCheckingFailureTestCase"],

      def "TestCaseWithMetadata" $
        doc "One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags" $
        record [
          "name">:
            doc "The name of the test case" $
            string,
          "case">:
            doc "The test case itself" $
            testing "TestCase",
          "description">:
            doc "An optional description of the test" $
            optional string,
          "tags">:
            doc "Zero or more tags for categorizing the test" $
            list $ testing "Tag"],

      def "TestGroup" $
        doc "A collection of test cases with a name and optional description" $
        record [
          "name">:
            doc "The name of the test group" $
            string,
          "description">:
            doc "An optional description of the group" $
            optional string,
          "subgroups">:
            doc "Nested test groups" $
            list (testing "TestGroup"),
          "cases">:
            doc "The test cases in this group" $
            list (testing "TestCaseWithMetadata")],

      def "TypeCheckingTestCase" $
        doc "A test case which performs type checking on a given term and compares the result with an expected annotated term and type" $
        record [
          "input">:
            doc "An untyped term on which to perform inference, then type check" $
            core "Term",
          "outputTerm">:
            doc "The expected fully annotated System F term after type inference" $
            core "Term",
          "outputType">:
            doc "The expected inferred type" $
            core "Type"],

      def "TypeCheckingFailureTestCase" $
        doc "A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases." $
        record [
          "input">:
            doc "The term for which type checking should fail" $
            core "Term"]]
