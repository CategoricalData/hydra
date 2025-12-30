module Hydra.Sources.Kernel.Types.Testing where

-- Standard type-level kernel imports
import           Hydra.Kernel
import           Hydra.Dsl.Annotations (doc)
import           Hydra.Dsl.Bootstrap
import           Hydra.Dsl.Types ((>:), (@@), (~>))
import qualified Hydra.Dsl.Types as T
import qualified Hydra.Sources.Kernel.Types.Core as Core

import qualified Hydra.Sources.Kernel.Types.Ast as Ast
import qualified Hydra.Sources.Kernel.Types.Coders as Coders
import qualified Hydra.Sources.Kernel.Types.Compute as Compute
import qualified Hydra.Sources.Kernel.Types.Graph as Graph
import qualified Hydra.Sources.Kernel.Types.Json as Json
import qualified Hydra.Sources.Kernel.Types.Module as Module
import qualified Hydra.Sources.Kernel.Types.Parsing as Parsing
import qualified Hydra.Sources.Kernel.Types.Util as Util


ns :: Namespace
ns = Namespace "hydra.testing"

define :: String -> Type -> Binding
define = defineType ns

module_ :: Module
module_ = Module ns elements [Ast.ns, Coders.ns, Compute.ns, Graph.ns, Json.ns, Module.ns, Parsing.ns, Util.ns] [Core.ns] $
    Just "A model for unit testing"
  where
    elements = [
      alphaConversionTestCase,
      evaluationStyle,
      caseConversionTestCase,
      delegatedEvaluationTestCase,
      etaExpansionTestCase,
      deannotateTermTestCase,
      deannotateTypeTestCase,
      flattenLetTermsTestCase,
      foldOperation,
      foldOverTermTestCase,
      freeVariablesTestCase,
      hoistPredicate,
      hoistSubtermsTestCase,
      termRewriter,
      rewriteTermTestCase,
      typeRewriter,
      rewriteTypeTestCase,
      evaluationTestCase,
      inferenceFailureTestCase,
      inferenceTestCase,
      jsonCoderTestCase,
      jsonParserTestCase,
      liftLambdaAboveLetTestCase,
      jsonWriterTestCase,
      parserTestCase,
      tag,
      testCodec,
      testCase,
      testCaseWithMetadata,
      testGroup,
      typeCheckingTestCase,
      typeCheckingFailureTestCase,
      topologicalSortBindingsTestCase,
      topologicalSortTestCase,
      topologicalSortSCCTestCase,
      serializationTestCase,
      simplifyTermTestCase,
      normalizeTypeVariablesTestCase,
      typeReductionTestCase,
      writerTestCase]

alphaConversionTestCase :: Binding
alphaConversionTestCase = define "AlphaConversionTestCase" $
  doc "A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term" $
  T.record [
    "term">:
      doc "The term on which to perform alpha conversion"
      Core.term,
    "oldVariable">:
      doc "The variable name to replace"
      Core.name,
    "newVariable">:
      doc "The new variable name"
      Core.name,
    "result">:
      doc "The expected result term after alpha conversion"
      Core.term]

evaluationStyle :: Binding
evaluationStyle = define "EvaluationStyle" $
  doc "One of two evaluation styles: eager or lazy" $
  T.enum ["eager", "lazy"]

caseConversionTestCase :: Binding
caseConversionTestCase = define "CaseConversionTestCase" $
  doc "A test case which checks that strings are converted between different case conventions correctly" $
  T.record [
    "fromConvention">:
      doc "The source case convention"
      Util.caseConvention,
    "toConvention">:
      doc "The target case convention"
      Util.caseConvention,
    "fromString">:
      doc "The input string" $
      T.string,
    "toString">:
      doc "The expected output string" $
      T.string]

delegatedEvaluationTestCase :: Binding
delegatedEvaluationTestCase = define "DelegatedEvaluationTestCase" $
  doc ("A test case in which we delegate evaluation of an input term and an expected output term"
    <> " to a target programming language like Haskell, Java, or Python, checking whether the term evaluates"
    <> " as expected when translated into that language") $
  T.record [
    "input">:
      doc "The first of two terms which should evaluate to the same expression"
      Core.term,
    "output">:
      doc "The second of two terms which should evaluate to the same expression"
      Core.term]

etaExpansionTestCase :: Binding
etaExpansionTestCase = define "EtaExpansionTestCase" $
  doc ("A test case which performs eta expansion (adding missing lambda abstractions) on a given term"
    <> " and compares the result with the expected result") $
  T.record [
    "input">:
      doc "The term to eta expand"
      Core.term,
    "output">:
      doc "The expected result"
      Core.term]

deannotateTermTestCase :: Binding
deannotateTermTestCase = define "DeannotateTermTestCase" $
  doc "A test case which strips all annotations from a term and compares the result with the expected term" $
  T.record [
    "input">:
      doc "The term to deannotate"
      Core.term,
    "output">:
      doc "The expected deannotated term"
      Core.term]

deannotateTypeTestCase :: Binding
deannotateTypeTestCase = define "DeannotateTypeTestCase" $
  doc "A test case which strips all annotations from a type and compares the result with the expected type" $
  T.record [
    "input">:
      doc "The type to deannotate"
      Core.type_,
    "output">:
      doc "The expected deannotated type"
      Core.type_]

flattenLetTermsTestCase :: Binding
flattenLetTermsTestCase = define "FlattenLetTermsTestCase" $
  doc ("A test case which flattens nested let terms,"
    <> " lifting inner bindings to the outer let, and compares the result with the expected term") $
  T.record [
    "input">:
      doc "The term to flatten"
      Core.term,
    "output">:
      doc "The expected flattened term"
      Core.term]

foldOperation :: Binding
foldOperation = define "FoldOperation" $
  doc "A predefined fold operation for testing foldOverTerm" $
  T.union [
    "sumInt32Literals">:
      doc "Sum all Int32 literals in a term" $
      T.unit,
    "collectListLengths">:
      doc "Collect the lengths of all list terms (returns list of integers in traversal order)" $
      T.unit,
    "collectLabels">:
      doc "Collect labels (first element of pairs where first is a string literal)" $
      T.unit]

foldOverTermTestCase :: Binding
foldOverTermTestCase = define "FoldOverTermTestCase" $
  doc "A test case which applies a fold operation over a term and compares the result" $
  T.record [
    "input">:
      doc "The term to fold over"
      Core.term,
    "traversalOrder">:
      doc "The traversal order (pre or post)"
      Coders.traversalOrder,
    "operation">:
      doc "The fold operation to apply"
      foldOperation,
    "output">:
      doc "The expected result of the fold"
      Core.term]

freeVariablesTestCase :: Binding
freeVariablesTestCase = define "FreeVariablesTestCase" $
  doc "A test case which computes the free variables of a term and compares the result with an expected set of names" $
  T.record [
    "input">:
      doc "The term to analyze"
      Core.term,
    "output">:
      doc "The expected set of free variable names" $
      T.set Core.name]

hoistPredicate :: Binding
hoistPredicate = define "HoistPredicate" $
  doc "A predefined predicate for testing hoistSubterms. Each predicate determines which subterms should be hoisted into let bindings." $
  T.union [
    "caseStatements">:
      doc "Hoist case statements (elimination unions) that appear in non-top-level positions" $
      T.unit,
    "applications">:
      doc "Hoist function applications that appear in non-top-level positions" $
      T.unit,
    "lists">:
      doc "Hoist list terms that appear in non-top-level positions" $
      T.unit,
    "nothing">:
      doc "Never hoist anything (identity transformation for let terms)" $
      T.unit]

hoistSubtermsTestCase :: Binding
hoistSubtermsTestCase = define "HoistSubtermsTestCase" $
  doc ("A test case which hoists subterms into let bindings based on a predicate,"
    <> " and compares the result with the expected term."
    <> " The predicate decides which subterms at which positions should be extracted into new bindings.") $
  T.record [
    "predicate">:
      doc "The predicate that determines which subterms to hoist"
      hoistPredicate,
    "input">:
      doc "The input term (must contain a let expression for hoisting to occur)"
      Core.term,
    "output">:
      doc "The expected output term with hoisted subterms as new bindings"
      Core.term]

termRewriter :: Binding
termRewriter = define "TermRewriter" $
  doc "A predefined term rewriter for testing rewriteTerm" $
  T.union [
    "replaceFooWithBar">:
      doc "Replace all string literal 'foo' with 'bar'" $
      T.unit,
    "replaceInt32WithInt64">:
      doc "Replace all Int32 literals with Int64 literals of the same value" $
      T.unit]

rewriteTermTestCase :: Binding
rewriteTermTestCase = define "RewriteTermTestCase" $
  doc "A test case which applies a term rewriter and compares the result" $
  T.record [
    "input">:
      doc "The term to rewrite"
      Core.term,
    "rewriter">:
      doc "The rewriter to apply"
      termRewriter,
    "output">:
      doc "The expected rewritten term"
      Core.term]

typeRewriter :: Binding
typeRewriter = define "TypeRewriter" $
  doc "A predefined type rewriter for testing rewriteType" $
  T.union [
    "replaceStringWithInt32">:
      doc "Replace all String types with Int32 types" $
      T.unit]

rewriteTypeTestCase :: Binding
rewriteTypeTestCase = define "RewriteTypeTestCase" $
  doc "A test case which applies a type rewriter and compares the result" $
  T.record [
    "input">:
      doc "The type to rewrite"
      Core.type_,
    "rewriter">:
      doc "The rewriter to apply"
      typeRewriter,
    "output">:
      doc "The expected rewritten type"
      Core.type_]

evaluationTestCase :: Binding
evaluationTestCase = define "EvaluationTestCase" $
  doc "A test case which evaluates (reduces) a given term and compares it with the expected result" $
  T.record [
    "evaluationStyle">:
      doc "The evaluation style (eager or lazy)"
      evaluationStyle,
    "input">:
      doc "The term to evaluate"
      Core.term,
    "output">:
      doc "The expected result"
      Core.term]

inferenceFailureTestCase :: Binding
inferenceFailureTestCase = define "InferenceFailureTestCase" $
  doc "A test case providing a term for which type inference is expected to fail" $
  T.record [
    "input">:
      doc "The term for which inference should fail"
      Core.term]

inferenceTestCase :: Binding
inferenceTestCase = define "InferenceTestCase" $
  doc "A test case which performs type inference on a given term and compares the result with an expected type scheme" $
  T.record [
    "input">:
      doc "The term to infer"
      Core.term,
    "output">:
      doc "The expected type scheme"
      Core.typeScheme]

jsonCoderTestCase :: Binding
jsonCoderTestCase = define "JsonCoderTestCase" $
  doc ("A test case which encodes a Hydra term to JSON using a type-directed coder,"
    <> " and verifies that decoding produces the original term (round-trip)") $
  T.record [
    "type">:
      doc "The Hydra type that determines how the term is encoded/decoded"
      Core.type_,
    "term">:
      doc "The Hydra term to encode"
      Core.term,
    "json">:
      doc "The expected JSON value"
      Json.value]

jsonParserTestCase :: Binding
jsonParserTestCase = define "JsonParserTestCase" $
  doc "A test case which parses a JSON string and compares the result with an expected JSON value" $
  parserTestCase @@ Json.value

liftLambdaAboveLetTestCase :: Binding
liftLambdaAboveLetTestCase = define "LiftLambdaAboveLetTestCase" $
  doc ("A test case which lifts lambda abstractions above let expressions"
    <> " and compares the result with the expected term") $
  T.record [
    "input">:
      doc "The term to transform"
      Core.term,
    "output">:
      doc "The expected transformed term"
      Core.term]

jsonWriterTestCase :: Binding
jsonWriterTestCase = define "JsonWriterTestCase" $
  doc "A test case which serializes a JSON value to a string and compares it to the expected string" $
  writerTestCase @@ Json.value

parserTestCase :: Binding
parserTestCase = define "ParserTestCase" $
  doc "A test case which parses an input string and compares the result with an expected value" $
  T.forAll "a" $ T.record [
    "input">:
      doc "The input string to parse" $
      T.string,
    "output">:
      doc "The expected parse result" $
      Parsing.parseResult @@ "a"]

tag :: Binding
tag = define "Tag" $
  doc "A tag for categorizing test cases" $
  T.wrap T.string

testCodec :: Binding
testCodec = define "TestCodec" $
  doc "A codec for generating compiled test files from test groups into a target programming language" $
  T.record [
    "language">:
      doc "The name of the target programming language"
      Coders.languageName,
    "fileExtension">:
      doc "The file extension for test files (e.g., 'hs', 'java', 'py')"
      Module.fileExtension,
    "encodeTerm">:
      doc "A function for encoding Hydra terms into the target language" $
      Core.term ~> Compute.flow @@ Graph.graph @@ T.string,
    "encodeType">:
      doc "A function for encoding Hydra types into the target language" $
      Core.type_ ~> Compute.flow @@ Graph.graph @@ T.string,
    "formatTestName">:
      doc "A function for formatting test case names according to the target language's conventions" $
      T.string ~> T.string,
    "formatModuleName">:
      doc "A function for formatting module names according to the target language's conventions" $
      Module.namespace ~> T.string,
    "testCaseTemplate">:
      doc "A template string for individual test case assertions" $
      T.string,
    "testGroupTemplate">:
      doc "A template string for wrapping a group of test cases" $
      T.string,
    "moduleTemplate">:
      doc "A template string for the overall test module structure" $
      T.string,
    "importTemplate">:
      doc "A template string for import/include statements" $
      T.string,
    "findImports">:
      doc "A function that determines the necessary imports for a given set of dependencies" $
      T.set Core.name ~> T.list T.string]

testCase :: Binding
testCase = define "TestCase" $
  doc "A simple test case with an input and an expected output" $
  T.union [
    "alphaConversion">:
      doc "An alpha conversion test"
      alphaConversionTestCase,
    "caseConversion">:
      doc "A case conversion test"
      caseConversionTestCase,
    "deannotateTerm">:
      doc "A deannotate term test"
      deannotateTermTestCase,
    "deannotateType">:
      doc "A deannotate type test"
      deannotateTypeTestCase,
    "delegatedEvaluation">:
      doc "A delegated evaluation test"
      delegatedEvaluationTestCase,
    "etaExpansion">:
      doc "An eta expansion test"
      etaExpansionTestCase,
    "flattenLetTerms">:
      doc "A flatten let terms test"
      flattenLetTermsTestCase,
    "freeVariables">:
      doc "A free variables test"
      freeVariablesTestCase,
    "evaluation">:
      doc "A term evaluation test"
      evaluationTestCase,
    "inference">:
      doc "A type inference test"
      inferenceTestCase,
    "inferenceFailure">:
      doc "A type inference failure test"
      inferenceFailureTestCase,
    "jsonCoder">:
      doc "A JSON coder (round-trip) test"
      jsonCoderTestCase,
    "jsonParser">:
      doc "A JSON parser test"
      jsonParserTestCase,
    "jsonWriter">:
      doc "A JSON writer test"
      jsonWriterTestCase,
    "liftLambdaAboveLet">:
      doc "A lift lambda above let test"
      liftLambdaAboveLetTestCase,
    "serialization">:
      doc "An AST serialization test"
      serializationTestCase,
    "simplifyTerm">:
      doc "A simplify term test"
      simplifyTermTestCase,
    "topologicalSort">:
      doc "A topological sort test"
      topologicalSortTestCase,
    "topologicalSortBindings">:
      doc "A topological sort bindings test"
      topologicalSortBindingsTestCase,
    "topologicalSortSCC">:
      doc "A topological sort with SCC detection test"
      topologicalSortSCCTestCase,
    "typeChecking">:
      doc "A type checking test"
      typeCheckingTestCase,
    "typeCheckingFailure">:
      doc "A type checking failure test (currently unused)"
      typeCheckingFailureTestCase,
    "typeReduction">:
      doc "A type reduction test"
      typeReductionTestCase,
    "normalizeTypeVariables">:
      doc "A normalize type variables test"
      normalizeTypeVariablesTestCase,
    "foldOverTerm">:
      doc "A fold over term test"
      foldOverTermTestCase,
    "rewriteTerm">:
      doc "A rewrite term test"
      rewriteTermTestCase,
    "rewriteType">:
      doc "A rewrite type test"
      rewriteTypeTestCase,
    "hoistSubterms">:
      doc "A hoist subterms test"
      hoistSubtermsTestCase]

testCaseWithMetadata :: Binding
testCaseWithMetadata = define "TestCaseWithMetadata" $
  doc "One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags" $
  T.record [
    "name">:
      doc "The name of the test case" $
      T.string,
    "case">:
      doc "The test case itself"
      testCase,
    "description">:
      doc "An optional description of the test" $
      T.maybe T.string,
    "tags">:
      doc "Zero or more tags for categorizing the test" $
      T.list tag]

testGroup :: Binding
testGroup = define "TestGroup" $
  doc "A collection of test cases with a name and optional description" $
  T.record [
    "name">:
      doc "The name of the test group" $
      T.string,
    "description">:
      doc "An optional description of the group" $
      T.maybe T.string,
    "subgroups">:
      doc "Nested test groups" $
      T.list testGroup,
    "cases">:
      doc "The test cases in this group" $
      T.list testCaseWithMetadata]

typeCheckingTestCase :: Binding
typeCheckingTestCase = define "TypeCheckingTestCase" $
  doc "A test case which performs type checking on a given term and compares the result with an expected annotated term and type" $
  T.record [
    "input">:
      doc "An untyped term on which to perform inference, then type check"
      Core.term,
    "outputTerm">:
      doc "The expected fully annotated System F term after type inference"
      Core.term,
    "outputType">:
      doc "The expected inferred type"
      Core.type_]

typeCheckingFailureTestCase :: Binding
typeCheckingFailureTestCase = define "TypeCheckingFailureTestCase" $
  doc "A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases." $
  T.record [
    "input">:
      doc "The term for which type checking should fail"
      Core.term]

topologicalSortBindingsTestCase :: Binding
topologicalSortBindingsTestCase = define "TopologicalSortBindingsTestCase" $
  doc ("A test case which performs topological sort on a map of bindings (name -> term)"
    <> " and compares the result with expected groups of bindings in topological order") $
  T.record [
    "bindings">:
      doc "The bindings as a list of (name, term) pairs" $
      T.list (T.pair Core.name Core.term),
    "expected">:
      doc "The expected groups of bindings in topological order" $
      T.list (T.list (T.pair Core.name Core.term))]

topologicalSortTestCase :: Binding
topologicalSortTestCase = define "TopologicalSortTestCase" $
  doc ("A test case which performs topological sort on a directed graph and compares the result"
    <> " with either an expected sorted list or expected cycles") $
  T.record [
    "adjacencyList">:
      doc "The directed graph as an adjacency list (node to list of dependencies)" $
      T.list (T.pair T.int32 (T.list T.int32)),
    "expected">:
      doc "The expected result: Left for cycles, Right for sorted nodes" $
      T.either_ (T.list (T.list T.int32)) (T.list T.int32)]

topologicalSortSCCTestCase :: Binding
topologicalSortSCCTestCase = define "TopologicalSortSCCTestCase" $
  doc ("A test case which performs topological sort with strongly connected component detection"
    <> " and compares the result with expected components") $
  T.record [
    "adjacencyList">:
      doc "The directed graph as an adjacency list" $
      T.list (T.pair T.int32 (T.list T.int32)),
    "expected">:
      doc "The expected strongly connected components in topological order" $
      T.list (T.list T.int32)]

serializationTestCase :: Binding
serializationTestCase = define "SerializationTestCase" $
  doc "A test case which serializes an AST expression to a string and compares it with the expected output" $
  T.record [
    "input">:
      doc "The AST expression to serialize"
      Ast.expr,
    "output">:
      doc "The expected serialized string" $
      T.string]

simplifyTermTestCase :: Binding
simplifyTermTestCase = define "SimplifyTermTestCase" $
  doc ("A test case which performs term simplification (beta reduction and optimization)"
    <> " and compares the result with the expected term") $
  T.record [
    "input">:
      doc "The term to simplify"
      Core.term,
    "output">:
      doc "The expected simplified term"
      Core.term]

normalizeTypeVariablesTestCase :: Binding
normalizeTypeVariablesTestCase = define "NormalizeTypeVariablesTestCase" $
  doc ("A test case which normalizes type variables in a term"
    <> " (renaming them to t0, t1, t2, etc.) and compares the result with the expected term") $
  T.record [
    "input">:
      doc "The term with type annotations to normalize"
      Core.term,
    "output">:
      doc "The expected term with normalized type variable names"
      Core.term]

typeReductionTestCase :: Binding
typeReductionTestCase = define "TypeReductionTestCase" $
  doc "A test case which performs beta reduction on a type (reducing type applications) and compares the result with the expected type" $
  T.record [
    "input">:
      doc "The type to reduce"
      Core.type_,
    "output">:
      doc "The expected reduced type"
      Core.type_]

writerTestCase :: Binding
writerTestCase = define "WriterTestCase" $
  doc "A test case which writes a value to a string and compares it to the expected string" $
  T.forAll "a" $ T.record [
    "input">:
      doc "The input value to write"
      "a",
    "output">:
      doc "The expected string" $
      T.string]
