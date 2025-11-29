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

import qualified Hydra.Sources.Kernel.Types.Ast as Ast
import qualified Hydra.Sources.Kernel.Types.Coders as Coders
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Json as Json
import qualified Hydra.Sources.Kernel.Types.Module as Module
import qualified Hydra.Sources.Kernel.Types.Parsing as Parsing
import qualified Hydra.Sources.Kernel.Types.Util as Util


module_ :: Module
module_ = Module ns elements [Ast.module_, Coders.module_, Compute.module_, Graph.module_, Json.module_, Module.module_, Parsing.module_, Util.module_] [Core.module_] $
    Just "A model for unit testing"
  where
    ns = Namespace "hydra.testing"
    def = datatype ns
    ast = typeref $ moduleNamespace Ast.module_
    coders = typeref $ moduleNamespace Coders.module_
    compute = typeref $ moduleNamespace Compute.module_
    graph = typeref $ moduleNamespace Graph.module_
    json = typeref $ moduleNamespace Json.module_
    core = typeref $ moduleNamespace Core.module_
    modulemod = typeref $ moduleNamespace Module.module_
    parsing = typeref $ moduleNamespace Parsing.module_
    util = typeref $ moduleNamespace Util.module_
    testing = typeref ns

    elements = [

      def "AlphaConversionTestCase" $
        doc "A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term" $
        record [
          "term">:
            doc "The term on which to perform alpha conversion" $
            core "Term",
          "oldVariable">:
            doc "The variable name to replace" $
            core "Name",
          "newVariable">:
            doc "The new variable name" $
            core "Name",
          "result">:
            doc "The expected result term after alpha conversion" $
            core "Term"],

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

      def "DelegatedEvaluationTestCase" $
        doc ("A test case in which we delegate evaluation of an input term and an expected output term"
          <> " to a target programming language like Haskell, Java, or Python, checking whether the term evaluates"
          <> " as expected when translated into that language") $
        record [
          "input">:
            doc "The first of two terms which should evaluate to the same expression" $
            core "Term",
          "output">:
            doc "The second of two terms which should evaluate to the same expression" $
            core "Term"],

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

      def "JsonCoderTestCase" $
        doc ("A test case which encodes a Hydra term to JSON using a type-directed coder,"
          <> " and verifies that decoding produces the original term (round-trip)") $
        record [
          "type">:
            doc "The Hydra type that determines how the term is encoded/decoded" $
            core "Type",
          "term">:
            doc "The Hydra term to encode" $
            core "Term",
          "json">:
            doc "The expected JSON value" $
            json "Value"],

      def "JsonParserTestCase" $
        doc "A test case which parses a JSON string and compares the result with an expected JSON value" $
        testing "ParserTestCase" @@ json "Value",

      def "JsonWriterTestCase" $
        doc "A test case which serializes a JSON value to a string and compares it to the expected string" $
        testing "WriterTestCase" @@ json "Value",

      def "ParserTestCase" $
        doc "A test case which parses an input string and compares the result with an expected value" $
        forAll "a" $ record [
          "input">:
            doc "The input string to parse" $
            string,
          "output">:
            doc "The expected parse result" $
            parsing "ParseResult" @@ "a"],

      def "Tag" $
        doc "A tag for categorizing test cases" $
        wrap string,

      def "TestCodec" $
        doc "A codec for generating compiled test files from test groups into a target programming language" $
        record [
          "language">:
            doc "The name of the target programming language" $
            coders "LanguageName",
          "fileExtension">:
            doc "The file extension for test files (e.g., 'hs', 'java', 'py')" $
            modulemod "FileExtension",
          "encodeTerm">:
            doc "A function for encoding Hydra terms into the target language" $
            function (core "Term") (compute "Flow" @@ graph "Graph" @@ string),
          "encodeType">:
            doc "A function for encoding Hydra types into the target language" $
            function (core "Type") (compute "Flow" @@ graph "Graph" @@ string),
          "formatTestName">:
            doc "A function for formatting test case names according to the target language's conventions" $
            function string string,
          "formatModuleName">:
            doc "A function for formatting module names according to the target language's conventions" $
            function (modulemod "Namespace") string,
          "testCaseTemplate">:
            doc "A template string for individual test case assertions" $
            string,
          "testGroupTemplate">:
            doc "A template string for wrapping a group of test cases" $
            string,
          "moduleTemplate">:
            doc "A template string for the overall test module structure" $
            string,
          "importTemplate">:
            doc "A template string for import/include statements" $
            string,
          "findImports">:
            doc "A function that determines the necessary imports for a given set of dependencies" $
            function (set $ core "Name") (list string)],

      def "TestCase" $
        doc "A simple test case with an input and an expected output" $
        union [
          "alphaConversion">:
            doc "An alpha conversion test" $
            testing "AlphaConversionTestCase",
          "caseConversion">:
            doc "A case conversion test" $
            testing "CaseConversionTestCase",
          "delegatedEvaluation">:
            doc "A delegated evaluation test" $
            testing "DelegatedEvaluationTestCase",
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
          "jsonCoder">:
            doc "A JSON coder (round-trip) test" $
            testing "JsonCoderTestCase",
          "jsonParser">:
            doc "A JSON parser test" $
            testing "JsonParserTestCase",
          "jsonWriter">:
            doc "A JSON writer test" $
            testing "JsonWriterTestCase",
          "serialization">:
            doc "An AST serialization test" $
            testing "SerializationTestCase",
          "topologicalSort">:
            doc "A topological sort test" $
            testing "TopologicalSortTestCase",
          "topologicalSortSCC">:
            doc "A topological sort with SCC detection test" $
            testing "TopologicalSortSCCTestCase",
          "typeChecking">:
            doc "A type checking test" $
            testing "TypeCheckingTestCase",
          "typeCheckingFailure">:
            doc "A type checking failure test (currently unused)" $
            testing "TypeCheckingFailureTestCase",
          "typeReduction">:
            doc "A type reduction test" $
            testing "TypeReductionTestCase"],

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
            core "Term"],

      def "TopologicalSortTestCase" $
        doc ("A test case which performs topological sort on a directed graph and compares the result"
          <> " with either an expected sorted list or expected cycles") $
        record [
          "adjacencyList">:
            doc "The directed graph as an adjacency list (node to list of dependencies)" $
            list (pair int32 (list int32)),
          "expected">:
            doc "The expected result: Left for cycles, Right for sorted nodes" $
            Types.either_ (list (list int32)) (list int32)],

      def "TopologicalSortSCCTestCase" $
        doc ("A test case which performs topological sort with strongly connected component detection"
          <> " and compares the result with expected components") $
        record [
          "adjacencyList">:
            doc "The directed graph as an adjacency list" $
            list (pair int32 (list int32)),
          "expected">:
            doc "The expected strongly connected components in topological order" $
            list (list int32)],

      def "SerializationTestCase" $
        doc "A test case which serializes an AST expression to a string and compares it with the expected output" $
        record [
          "input">:
            doc "The AST expression to serialize" $
            ast "Expr",
          "output">:
            doc "The expected serialized string" $
            string],

      def "TypeReductionTestCase" $
        doc "A test case which performs beta reduction on a type (reducing type applications) and compares the result with the expected type" $
        record [
          "input">:
            doc "The type to reduce" $
            core "Type",
          "output">:
            doc "The expected reduced type" $
            core "Type"],

      def "WriterTestCase" $
        doc "A test case which writes a value to a string and compares it to the expected string" $
        forAll "a" $ record [
          "input">:
            doc "The input value to write" $
            "a",
          "output">:
            doc "The expected string" $
            string]]
