# Note: this is an automatically generated file. Do not edit.

r"""A model for unit testing."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Either, Maybe, Node, frozenlist
from typing import Annotated, Generic, TypeAlias, TypeVar
import hydra.ast
import hydra.coders
import hydra.compute
import hydra.core
import hydra.graph
import hydra.json.model
import hydra.module
import hydra.parsing
import hydra.typing
import hydra.util

A = TypeVar("A")

@dataclass(frozen=True)
class AlphaConversionTestCase:
    r"""A test case which performs alpha conversion (variable renaming) on a term and compares the result with the expected term."""
    
    term: Annotated[hydra.core.Term, "The term on which to perform alpha conversion"]
    old_variable: Annotated[hydra.core.Name, "The variable name to replace"]
    new_variable: Annotated[hydra.core.Name, "The new variable name"]
    result: Annotated[hydra.core.Term, "The expected result term after alpha conversion"]

ALPHA_CONVERSION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.AlphaConversionTestCase")
ALPHA_CONVERSION_TEST_CASE__TERM__NAME = hydra.core.Name("term")
ALPHA_CONVERSION_TEST_CASE__OLD_VARIABLE__NAME = hydra.core.Name("oldVariable")
ALPHA_CONVERSION_TEST_CASE__NEW_VARIABLE__NAME = hydra.core.Name("newVariable")
ALPHA_CONVERSION_TEST_CASE__RESULT__NAME = hydra.core.Name("result")

class EvaluationStyle(Enum):
    r"""One of two evaluation styles: eager or lazy."""
    
    EAGER = "eager"
    
    LAZY = "lazy"

EVALUATION_STYLE__NAME = hydra.core.Name("hydra.testing.EvaluationStyle")
EVALUATION_STYLE__EAGER__NAME = hydra.core.Name("eager")
EVALUATION_STYLE__LAZY__NAME = hydra.core.Name("lazy")

@dataclass(frozen=True)
class CaseConversionTestCase:
    r"""A test case which checks that strings are converted between different case conventions correctly."""
    
    from_convention: Annotated[hydra.util.CaseConvention, "The source case convention"]
    to_convention: Annotated[hydra.util.CaseConvention, "The target case convention"]
    from_string: Annotated[str, "The input string"]
    to_string: Annotated[str, "The expected output string"]

CASE_CONVERSION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.CaseConversionTestCase")
CASE_CONVERSION_TEST_CASE__FROM_CONVENTION__NAME = hydra.core.Name("fromConvention")
CASE_CONVERSION_TEST_CASE__TO_CONVENTION__NAME = hydra.core.Name("toConvention")
CASE_CONVERSION_TEST_CASE__FROM_STRING__NAME = hydra.core.Name("fromString")
CASE_CONVERSION_TEST_CASE__TO_STRING__NAME = hydra.core.Name("toString")

@dataclass(frozen=True)
class DelegatedEvaluationTestCase:
    r"""A test case in which we delegate evaluation of an input term and an expected output term to a target programming language like Haskell, Java, or Python, checking whether the term evaluates as expected when translated into that language."""
    
    input: Annotated[hydra.core.Term, "The first of two terms which should evaluate to the same expression"]
    output: Annotated[hydra.core.Term, "The second of two terms which should evaluate to the same expression"]

DELEGATED_EVALUATION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase")
DELEGATED_EVALUATION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
DELEGATED_EVALUATION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class EtaExpansionTestCase:
    r"""A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result."""
    
    input: Annotated[hydra.core.Term, "The term to eta expand"]
    output: Annotated[hydra.core.Term, "The expected result"]

ETA_EXPANSION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.EtaExpansionTestCase")
ETA_EXPANSION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
ETA_EXPANSION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class DeannotateTermTestCase:
    r"""A test case which strips all annotations from a term and compares the result with the expected term."""
    
    input: Annotated[hydra.core.Term, "The term to deannotate"]
    output: Annotated[hydra.core.Term, "The expected deannotated term"]

DEANNOTATE_TERM_TEST_CASE__NAME = hydra.core.Name("hydra.testing.DeannotateTermTestCase")
DEANNOTATE_TERM_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
DEANNOTATE_TERM_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class DeannotateTypeTestCase:
    r"""A test case which strips all annotations from a type and compares the result with the expected type."""
    
    input: Annotated[hydra.core.Type, "The type to deannotate"]
    output: Annotated[hydra.core.Type, "The expected deannotated type"]

DEANNOTATE_TYPE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.DeannotateTypeTestCase")
DEANNOTATE_TYPE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
DEANNOTATE_TYPE_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class FlattenLetTermsTestCase:
    r"""A test case which flattens nested let terms, lifting inner bindings to the outer let, and compares the result with the expected term."""
    
    input: Annotated[hydra.core.Term, "The term to flatten"]
    output: Annotated[hydra.core.Term, "The expected flattened term"]

FLATTEN_LET_TERMS_TEST_CASE__NAME = hydra.core.Name("hydra.testing.FlattenLetTermsTestCase")
FLATTEN_LET_TERMS_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
FLATTEN_LET_TERMS_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class FoldOperation(Enum):
    r"""A predefined fold operation for testing foldOverTerm."""
    
    SUM_INT32_LITERALS = "sumInt32Literals"
    r"""Sum all Int32 literals in a term."""
    
    COLLECT_LIST_LENGTHS = "collectListLengths"
    r"""Collect the lengths of all list terms (returns list of integers in traversal order)."""
    
    COLLECT_LABELS = "collectLabels"
    r"""Collect labels (first element of pairs where first is a string literal)."""

FOLD_OPERATION__NAME = hydra.core.Name("hydra.testing.FoldOperation")
FOLD_OPERATION__SUM_INT32_LITERALS__NAME = hydra.core.Name("sumInt32Literals")
FOLD_OPERATION__COLLECT_LIST_LENGTHS__NAME = hydra.core.Name("collectListLengths")
FOLD_OPERATION__COLLECT_LABELS__NAME = hydra.core.Name("collectLabels")

@dataclass(frozen=True)
class FoldOverTermTestCase:
    r"""A test case which applies a fold operation over a term and compares the result."""
    
    input: Annotated[hydra.core.Term, "The term to fold over"]
    traversal_order: Annotated[hydra.coders.TraversalOrder, "The traversal order (pre or post)"]
    operation: Annotated[FoldOperation, "The fold operation to apply"]
    output: Annotated[hydra.core.Term, "The expected result of the fold"]

FOLD_OVER_TERM_TEST_CASE__NAME = hydra.core.Name("hydra.testing.FoldOverTermTestCase")
FOLD_OVER_TERM_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
FOLD_OVER_TERM_TEST_CASE__TRAVERSAL_ORDER__NAME = hydra.core.Name("traversalOrder")
FOLD_OVER_TERM_TEST_CASE__OPERATION__NAME = hydra.core.Name("operation")
FOLD_OVER_TERM_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class FreeVariablesTestCase:
    r"""A test case which computes the free variables of a term and compares the result with an expected set of names."""
    
    input: Annotated[hydra.core.Term, "The term to analyze"]
    output: Annotated[frozenset[hydra.core.Name], "The expected set of free variable names"]

FREE_VARIABLES_TEST_CASE__NAME = hydra.core.Name("hydra.testing.FreeVariablesTestCase")
FREE_VARIABLES_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
FREE_VARIABLES_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class HoistPredicate(Enum):
    r"""A predefined predicate for testing hoistSubterms. Each predicate determines which subterms should be hoisted into let bindings."""
    
    CASE_STATEMENTS = "caseStatements"
    r"""Hoist case statements (elimination unions) that appear in non-top-level positions."""
    
    APPLICATIONS = "applications"
    r"""Hoist function applications that appear in non-top-level positions."""
    
    LISTS = "lists"
    r"""Hoist list terms that appear in non-top-level positions."""
    
    NOTHING = "nothing"
    r"""Never hoist anything (identity transformation for let terms)."""

HOIST_PREDICATE__NAME = hydra.core.Name("hydra.testing.HoistPredicate")
HOIST_PREDICATE__CASE_STATEMENTS__NAME = hydra.core.Name("caseStatements")
HOIST_PREDICATE__APPLICATIONS__NAME = hydra.core.Name("applications")
HOIST_PREDICATE__LISTS__NAME = hydra.core.Name("lists")
HOIST_PREDICATE__NOTHING__NAME = hydra.core.Name("nothing")

@dataclass(frozen=True)
class HoistLetBindingsTestCase:
    r"""A test case for hoistLetBindings with hoistAll=True, which hoists ALL nested let bindings to the top level of a let term, not just polymorphic ones. This is used for targets like Java that cannot have let expressions in arbitrary positions."""
    
    input: Annotated[hydra.core.Let, "The input let term"]
    output: Annotated[hydra.core.Let, "The expected output let term with all nested bindings hoisted to top"]

HOIST_LET_BINDINGS_TEST_CASE__NAME = hydra.core.Name("hydra.testing.HoistLetBindingsTestCase")
HOIST_LET_BINDINGS_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
HOIST_LET_BINDINGS_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class HoistPolymorphicLetBindingsTestCase:
    r"""A test case for the hoistPolymorphicLetBindings function, which hoists polymorphic let bindings to the top level of a let term. This is used for targets like Java which don't support polymorphic lambdas."""
    
    input: Annotated[hydra.core.Let, "The input let term"]
    output: Annotated[hydra.core.Let, "The expected output let term with polymorphic bindings hoisted to top"]

HOIST_POLYMORPHIC_LET_BINDINGS_TEST_CASE__NAME = hydra.core.Name("hydra.testing.HoistPolymorphicLetBindingsTestCase")
HOIST_POLYMORPHIC_LET_BINDINGS_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
HOIST_POLYMORPHIC_LET_BINDINGS_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class HoistSubtermsTestCase:
    r"""A test case which hoists subterms into let bindings based on a predicate, and compares the result with the expected term. The predicate decides which subterms at which positions should be extracted into new bindings."""
    
    predicate: Annotated[HoistPredicate, "The predicate that determines which subterms to hoist"]
    input: Annotated[hydra.core.Term, "The input term (must contain a let expression for hoisting to occur)"]
    output: Annotated[hydra.core.Term, "The expected output term with hoisted subterms as new bindings"]

HOIST_SUBTERMS_TEST_CASE__NAME = hydra.core.Name("hydra.testing.HoistSubtermsTestCase")
HOIST_SUBTERMS_TEST_CASE__PREDICATE__NAME = hydra.core.Name("predicate")
HOIST_SUBTERMS_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
HOIST_SUBTERMS_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class HoistCaseStatementsTestCase:
    r"""A test case for the hoistCaseStatements function, which hoists case statements into let bindings, but only when they appear inside a lambda body. This is used for targets like Python which don't support inline match expressions."""
    
    input: Annotated[hydra.core.Term, "The input term"]
    output: Annotated[hydra.core.Term, "The expected output term with hoisted case statements"]

HOIST_CASE_STATEMENTS_TEST_CASE__NAME = hydra.core.Name("hydra.testing.HoistCaseStatementsTestCase")
HOIST_CASE_STATEMENTS_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
HOIST_CASE_STATEMENTS_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class TermRewriter(Enum):
    r"""A predefined term rewriter for testing rewriteTerm."""
    
    REPLACE_FOO_WITH_BAR = "replaceFooWithBar"
    r"""Replace all string literal 'foo' with 'bar'."""
    
    REPLACE_INT32_WITH_INT64 = "replaceInt32WithInt64"
    r"""Replace all Int32 literals with Int64 literals of the same value."""

TERM_REWRITER__NAME = hydra.core.Name("hydra.testing.TermRewriter")
TERM_REWRITER__REPLACE_FOO_WITH_BAR__NAME = hydra.core.Name("replaceFooWithBar")
TERM_REWRITER__REPLACE_INT32_WITH_INT64__NAME = hydra.core.Name("replaceInt32WithInt64")

@dataclass(frozen=True)
class RewriteTermTestCase:
    r"""A test case which applies a term rewriter and compares the result."""
    
    input: Annotated[hydra.core.Term, "The term to rewrite"]
    rewriter: Annotated[TermRewriter, "The rewriter to apply"]
    output: Annotated[hydra.core.Term, "The expected rewritten term"]

REWRITE_TERM_TEST_CASE__NAME = hydra.core.Name("hydra.testing.RewriteTermTestCase")
REWRITE_TERM_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
REWRITE_TERM_TEST_CASE__REWRITER__NAME = hydra.core.Name("rewriter")
REWRITE_TERM_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class TypeRewriter(Enum):
    r"""A predefined type rewriter for testing rewriteType."""
    
    REPLACE_STRING_WITH_INT32 = "replaceStringWithInt32"
    r"""Replace all String types with Int32 types."""

TYPE_REWRITER__NAME = hydra.core.Name("hydra.testing.TypeRewriter")
TYPE_REWRITER__REPLACE_STRING_WITH_INT32__NAME = hydra.core.Name("replaceStringWithInt32")

@dataclass(frozen=True)
class RewriteTypeTestCase:
    r"""A test case which applies a type rewriter and compares the result."""
    
    input: Annotated[hydra.core.Type, "The type to rewrite"]
    rewriter: Annotated[TypeRewriter, "The rewriter to apply"]
    output: Annotated[hydra.core.Type, "The expected rewritten type"]

REWRITE_TYPE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.RewriteTypeTestCase")
REWRITE_TYPE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
REWRITE_TYPE_TEST_CASE__REWRITER__NAME = hydra.core.Name("rewriter")
REWRITE_TYPE_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class EvaluationTestCase:
    r"""A test case which evaluates (reduces) a given term and compares it with the expected result."""
    
    evaluation_style: Annotated[EvaluationStyle, "The evaluation style (eager or lazy)"]
    input: Annotated[hydra.core.Term, "The term to evaluate"]
    output: Annotated[hydra.core.Term, "The expected result"]

EVALUATION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.EvaluationTestCase")
EVALUATION_TEST_CASE__EVALUATION_STYLE__NAME = hydra.core.Name("evaluationStyle")
EVALUATION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
EVALUATION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class InferenceFailureTestCase:
    r"""A test case providing a term for which type inference is expected to fail."""
    
    input: Annotated[hydra.core.Term, "The term for which inference should fail"]

INFERENCE_FAILURE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.InferenceFailureTestCase")
INFERENCE_FAILURE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")

@dataclass(frozen=True)
class InferenceTestCase:
    r"""A test case which performs type inference on a given term and compares the result with an expected type scheme."""
    
    input: Annotated[hydra.core.Term, "The term to infer"]
    output: Annotated[hydra.core.TypeScheme, "The expected type scheme"]

INFERENCE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.InferenceTestCase")
INFERENCE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
INFERENCE_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class JsonCoderTestCase:
    r"""A test case which encodes a Hydra term to JSON using a type-directed coder, and verifies that decoding produces the original term (round-trip)."""
    
    type: Annotated[hydra.core.Type, "The Hydra type that determines how the term is encoded/decoded"]
    term: Annotated[hydra.core.Term, "The Hydra term to encode"]
    json: Annotated[hydra.json.model.Value, "The expected JSON value"]

JSON_CODER_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JsonCoderTestCase")
JSON_CODER_TEST_CASE__TYPE__NAME = hydra.core.Name("type")
JSON_CODER_TEST_CASE__TERM__NAME = hydra.core.Name("term")
JSON_CODER_TEST_CASE__JSON__NAME = hydra.core.Name("json")

@dataclass(frozen=True)
class JsonDecodeTestCase:
    r"""A test case for the Either-based JSON decoder. Takes a type, input JSON, and expected result (Either String Term)."""
    
    type: Annotated[hydra.core.Type, "The Hydra type to decode into"]
    json: Annotated[hydra.json.model.Value, "The input JSON value"]
    expected: Annotated[Either[str, hydra.core.Term], "The expected result: Left for error, Right for decoded term"]

JSON_DECODE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JsonDecodeTestCase")
JSON_DECODE_TEST_CASE__TYPE__NAME = hydra.core.Name("type")
JSON_DECODE_TEST_CASE__JSON__NAME = hydra.core.Name("json")
JSON_DECODE_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

@dataclass(frozen=True)
class JsonEncodeTestCase:
    r"""A test case for the Either-based JSON encoder. Takes an input term and expected result (Either String Value)."""
    
    term: Annotated[hydra.core.Term, "The Hydra term to encode"]
    expected: Annotated[Either[str, hydra.json.model.Value], "The expected result: Left for error, Right for encoded JSON"]

JSON_ENCODE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JsonEncodeTestCase")
JSON_ENCODE_TEST_CASE__TERM__NAME = hydra.core.Name("term")
JSON_ENCODE_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

# A test case which parses a JSON string and compares the result with an expected JSON value.
JsonParserTestCase: TypeAlias = "ParserTestCase[hydra.json.model.Value]"

JSON_PARSER_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JsonParserTestCase")

@dataclass(frozen=True)
class JsonRoundtripTestCase:
    r"""A test case for round-trip encoding/decoding using the Either-based JSON functions. Encodes a term, then decodes it back, verifying the result equals the original."""
    
    type: Annotated[hydra.core.Type, "The Hydra type for encoding/decoding"]
    term: Annotated[hydra.core.Term, "The Hydra term to round-trip"]

JSON_ROUNDTRIP_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JsonRoundtripTestCase")
JSON_ROUNDTRIP_TEST_CASE__TYPE__NAME = hydra.core.Name("type")
JSON_ROUNDTRIP_TEST_CASE__TERM__NAME = hydra.core.Name("term")

@dataclass(frozen=True)
class LiftLambdaAboveLetTestCase:
    r"""A test case which lifts lambda abstractions above let expressions and compares the result with the expected term."""
    
    input: Annotated[hydra.core.Term, "The term to transform"]
    output: Annotated[hydra.core.Term, "The expected transformed term"]

LIFT_LAMBDA_ABOVE_LET_TEST_CASE__NAME = hydra.core.Name("hydra.testing.LiftLambdaAboveLetTestCase")
LIFT_LAMBDA_ABOVE_LET_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
LIFT_LAMBDA_ABOVE_LET_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

# A test case which serializes a JSON value to a string and compares it to the expected string.
JsonWriterTestCase: TypeAlias = "WriterTestCase[hydra.json.model.Value]"

JSON_WRITER_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JsonWriterTestCase")

@dataclass(frozen=True)
class ParserTestCase(Generic[A]):
    r"""A test case which parses an input string and compares the result with an expected value."""
    
    input: Annotated[str, "The input string to parse"]
    output: Annotated[hydra.parsing.ParseResult[A], "The expected parse result"]

PARSER_TEST_CASE__NAME = hydra.core.Name("hydra.testing.ParserTestCase")
PARSER_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
PARSER_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class Tag(Node[str]):
    r"""A tag for categorizing test cases."""

TAG__NAME = hydra.core.Name("hydra.testing.Tag")

@dataclass(frozen=True)
class TestCodec:
    r"""A codec for generating compiled test files from test groups into a target programming language."""
    
    language: Annotated[hydra.coders.LanguageName, "The name of the target programming language"]
    file_extension: Annotated[hydra.module.FileExtension, "The file extension for test files (e.g., 'hs', 'java', 'py')"]
    encode_term: Annotated[Callable[[hydra.core.Term], hydra.compute.Flow[hydra.graph.Graph, str]], "A function for encoding Hydra terms into the target language"]
    encode_type: Annotated[Callable[[hydra.core.Type], hydra.compute.Flow[hydra.graph.Graph, str]], "A function for encoding Hydra types into the target language"]
    format_test_name: Annotated[Callable[[str], str], "A function for formatting test case names according to the target language's conventions"]
    format_module_name: Annotated[Callable[[hydra.module.Namespace], str], "A function for formatting module names according to the target language's conventions"]
    test_case_template: Annotated[str, "A template string for individual test case assertions"]
    test_group_template: Annotated[str, "A template string for wrapping a group of test cases"]
    module_template: Annotated[str, "A template string for the overall test module structure"]
    import_template: Annotated[str, "A template string for import/include statements"]
    find_imports: Annotated[Callable[[frozenset[hydra.core.Name]], frozenlist[str]], "A function that determines the necessary imports for a given set of dependencies"]

TEST_CODEC__NAME = hydra.core.Name("hydra.testing.TestCodec")
TEST_CODEC__LANGUAGE__NAME = hydra.core.Name("language")
TEST_CODEC__FILE_EXTENSION__NAME = hydra.core.Name("fileExtension")
TEST_CODEC__ENCODE_TERM__NAME = hydra.core.Name("encodeTerm")
TEST_CODEC__ENCODE_TYPE__NAME = hydra.core.Name("encodeType")
TEST_CODEC__FORMAT_TEST_NAME__NAME = hydra.core.Name("formatTestName")
TEST_CODEC__FORMAT_MODULE_NAME__NAME = hydra.core.Name("formatModuleName")
TEST_CODEC__TEST_CASE_TEMPLATE__NAME = hydra.core.Name("testCaseTemplate")
TEST_CODEC__TEST_GROUP_TEMPLATE__NAME = hydra.core.Name("testGroupTemplate")
TEST_CODEC__MODULE_TEMPLATE__NAME = hydra.core.Name("moduleTemplate")
TEST_CODEC__IMPORT_TEMPLATE__NAME = hydra.core.Name("importTemplate")
TEST_CODEC__FIND_IMPORTS__NAME = hydra.core.Name("findImports")

class TestCaseAlphaConversion(Node["AlphaConversionTestCase"]):
    r"""An alpha conversion test."""

class TestCaseCaseConversion(Node["CaseConversionTestCase"]):
    r"""A case conversion test."""

class TestCaseDeannotateTerm(Node["DeannotateTermTestCase"]):
    r"""A deannotate term test."""

class TestCaseDeannotateType(Node["DeannotateTypeTestCase"]):
    r"""A deannotate type test."""

class TestCaseDelegatedEvaluation(Node["DelegatedEvaluationTestCase"]):
    r"""A delegated evaluation test."""

class TestCaseEtaExpansion(Node["EtaExpansionTestCase"]):
    r"""An eta expansion test."""

class TestCaseFlattenLetTerms(Node["FlattenLetTermsTestCase"]):
    r"""A flatten let terms test."""

class TestCaseFreeVariables(Node["FreeVariablesTestCase"]):
    r"""A free variables test."""

class TestCaseEvaluation(Node["EvaluationTestCase"]):
    r"""A term evaluation test."""

class TestCaseInference(Node["InferenceTestCase"]):
    r"""A type inference test."""

class TestCaseInferenceFailure(Node["InferenceFailureTestCase"]):
    r"""A type inference failure test."""

class TestCaseJsonCoder(Node["JsonCoderTestCase"]):
    r"""A JSON coder (round-trip) test using Flow-based coder."""

class TestCaseJsonDecode(Node["JsonDecodeTestCase"]):
    r"""A JSON decode test using Either-based decoder."""

class TestCaseJsonEncode(Node["JsonEncodeTestCase"]):
    r"""A JSON encode test using Either-based encoder."""

class TestCaseJsonParser(Node["JsonParserTestCase"]):
    r"""A JSON parser test."""

class TestCaseJsonRoundtrip(Node["JsonRoundtripTestCase"]):
    r"""A JSON round-trip test using Either-based encoder/decoder."""

class TestCaseJsonWriter(Node["JsonWriterTestCase"]):
    r"""A JSON writer test."""

class TestCaseLiftLambdaAboveLet(Node["LiftLambdaAboveLetTestCase"]):
    r"""A lift lambda above let test."""

class TestCaseSerialization(Node["SerializationTestCase"]):
    r"""An AST serialization test."""

class TestCaseSimplifyTerm(Node["SimplifyTermTestCase"]):
    r"""A simplify term test."""

class TestCaseTopologicalSort(Node["TopologicalSortTestCase"]):
    r"""A topological sort test."""

class TestCaseTopologicalSortBindings(Node["TopologicalSortBindingsTestCase"]):
    r"""A topological sort bindings test."""

class TestCaseTopologicalSortSCC(Node["TopologicalSortSCCTestCase"]):
    r"""A topological sort with SCC detection test."""

class TestCaseTypeChecking(Node["TypeCheckingTestCase"]):
    r"""A type checking test."""

class TestCaseTypeCheckingFailure(Node["TypeCheckingFailureTestCase"]):
    r"""A type checking failure test (currently unused)."""

class TestCaseTypeReduction(Node["TypeReductionTestCase"]):
    r"""A type reduction test."""

class TestCaseNormalizeTypeVariables(Node["NormalizeTypeVariablesTestCase"]):
    r"""A normalize type variables test."""

class TestCaseFoldOverTerm(Node["FoldOverTermTestCase"]):
    r"""A fold over term test."""

class TestCaseRewriteTerm(Node["RewriteTermTestCase"]):
    r"""A rewrite term test."""

class TestCaseRewriteType(Node["RewriteTypeTestCase"]):
    r"""A rewrite type test."""

class TestCaseHoistSubterms(Node["HoistSubtermsTestCase"]):
    r"""A hoist subterms test."""

class TestCaseHoistCaseStatements(Node["HoistCaseStatementsTestCase"]):
    r"""A hoist case statements test."""

class TestCaseHoistLetBindings(Node["HoistLetBindingsTestCase"]):
    r"""A hoist all let bindings test (hoistAll=True, for Java)."""

class TestCaseHoistPolymorphicLetBindings(Node["HoistPolymorphicLetBindingsTestCase"]):
    r"""A hoist polymorphic let bindings test."""

class TestCaseSubstInType(Node["SubstInTypeTestCase"]):
    r"""A type substitution test."""

class TestCaseVariableOccursInType(Node["VariableOccursInTypeTestCase"]):
    r"""An occur check test for type unification."""

class TestCaseUnifyTypes(Node["UnifyTypesTestCase"]):
    r"""A type unification test."""

class TestCaseJoinTypes(Node["JoinTypesTestCase"]):
    r"""A join types test (produce type constraints)."""

class _TestCaseMeta(type):
    def __getitem__(cls, item):
        return object

# A simple test case with an input and an expected output.
class TestCase(metaclass=_TestCaseMeta):
    r"""TestCaseAlphaConversion | TestCaseCaseConversion | TestCaseDeannotateTerm | TestCaseDeannotateType | TestCaseDelegatedEvaluation | TestCaseEtaExpansion | TestCaseFlattenLetTerms | TestCaseFreeVariables | TestCaseEvaluation | TestCaseInference | TestCaseInferenceFailure | TestCaseJsonCoder | TestCaseJsonDecode | TestCaseJsonEncode | TestCaseJsonParser | TestCaseJsonRoundtrip | TestCaseJsonWriter | TestCaseLiftLambdaAboveLet | TestCaseSerialization | TestCaseSimplifyTerm | TestCaseTopologicalSort | TestCaseTopologicalSortBindings | TestCaseTopologicalSortSCC | TestCaseTypeChecking | TestCaseTypeCheckingFailure | TestCaseTypeReduction | TestCaseNormalizeTypeVariables | TestCaseFoldOverTerm | TestCaseRewriteTerm | TestCaseRewriteType | TestCaseHoistSubterms | TestCaseHoistCaseStatements | TestCaseHoistLetBindings | TestCaseHoistPolymorphicLetBindings | TestCaseSubstInType | TestCaseVariableOccursInType | TestCaseUnifyTypes | TestCaseJoinTypes"""
    
    pass

TEST_CASE__NAME = hydra.core.Name("hydra.testing.TestCase")
TEST_CASE__ALPHA_CONVERSION__NAME = hydra.core.Name("alphaConversion")
TEST_CASE__CASE_CONVERSION__NAME = hydra.core.Name("caseConversion")
TEST_CASE__DEANNOTATE_TERM__NAME = hydra.core.Name("deannotateTerm")
TEST_CASE__DEANNOTATE_TYPE__NAME = hydra.core.Name("deannotateType")
TEST_CASE__DELEGATED_EVALUATION__NAME = hydra.core.Name("delegatedEvaluation")
TEST_CASE__ETA_EXPANSION__NAME = hydra.core.Name("etaExpansion")
TEST_CASE__FLATTEN_LET_TERMS__NAME = hydra.core.Name("flattenLetTerms")
TEST_CASE__FREE_VARIABLES__NAME = hydra.core.Name("freeVariables")
TEST_CASE__EVALUATION__NAME = hydra.core.Name("evaluation")
TEST_CASE__INFERENCE__NAME = hydra.core.Name("inference")
TEST_CASE__INFERENCE_FAILURE__NAME = hydra.core.Name("inferenceFailure")
TEST_CASE__JSON_CODER__NAME = hydra.core.Name("jsonCoder")
TEST_CASE__JSON_DECODE__NAME = hydra.core.Name("jsonDecode")
TEST_CASE__JSON_ENCODE__NAME = hydra.core.Name("jsonEncode")
TEST_CASE__JSON_PARSER__NAME = hydra.core.Name("jsonParser")
TEST_CASE__JSON_ROUNDTRIP__NAME = hydra.core.Name("jsonRoundtrip")
TEST_CASE__JSON_WRITER__NAME = hydra.core.Name("jsonWriter")
TEST_CASE__LIFT_LAMBDA_ABOVE_LET__NAME = hydra.core.Name("liftLambdaAboveLet")
TEST_CASE__SERIALIZATION__NAME = hydra.core.Name("serialization")
TEST_CASE__SIMPLIFY_TERM__NAME = hydra.core.Name("simplifyTerm")
TEST_CASE__TOPOLOGICAL_SORT__NAME = hydra.core.Name("topologicalSort")
TEST_CASE__TOPOLOGICAL_SORT_BINDINGS__NAME = hydra.core.Name("topologicalSortBindings")
TEST_CASE__TOPOLOGICAL_SORT_S_C_C__NAME = hydra.core.Name("topologicalSortSCC")
TEST_CASE__TYPE_CHECKING__NAME = hydra.core.Name("typeChecking")
TEST_CASE__TYPE_CHECKING_FAILURE__NAME = hydra.core.Name("typeCheckingFailure")
TEST_CASE__TYPE_REDUCTION__NAME = hydra.core.Name("typeReduction")
TEST_CASE__NORMALIZE_TYPE_VARIABLES__NAME = hydra.core.Name("normalizeTypeVariables")
TEST_CASE__FOLD_OVER_TERM__NAME = hydra.core.Name("foldOverTerm")
TEST_CASE__REWRITE_TERM__NAME = hydra.core.Name("rewriteTerm")
TEST_CASE__REWRITE_TYPE__NAME = hydra.core.Name("rewriteType")
TEST_CASE__HOIST_SUBTERMS__NAME = hydra.core.Name("hoistSubterms")
TEST_CASE__HOIST_CASE_STATEMENTS__NAME = hydra.core.Name("hoistCaseStatements")
TEST_CASE__HOIST_LET_BINDINGS__NAME = hydra.core.Name("hoistLetBindings")
TEST_CASE__HOIST_POLYMORPHIC_LET_BINDINGS__NAME = hydra.core.Name("hoistPolymorphicLetBindings")
TEST_CASE__SUBST_IN_TYPE__NAME = hydra.core.Name("substInType")
TEST_CASE__VARIABLE_OCCURS_IN_TYPE__NAME = hydra.core.Name("variableOccursInType")
TEST_CASE__UNIFY_TYPES__NAME = hydra.core.Name("unifyTypes")
TEST_CASE__JOIN_TYPES__NAME = hydra.core.Name("joinTypes")

@dataclass(frozen=True)
class TestCaseWithMetadata:
    r"""One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags."""
    
    name: Annotated[str, "The name of the test case"]
    case: Annotated[TestCase, "The test case itself"]
    description: Annotated[Maybe[str], "An optional description of the test"]
    tags: Annotated[frozenlist[Tag], "Zero or more tags for categorizing the test"]

TEST_CASE_WITH_METADATA__NAME = hydra.core.Name("hydra.testing.TestCaseWithMetadata")
TEST_CASE_WITH_METADATA__NAME__NAME = hydra.core.Name("name")
TEST_CASE_WITH_METADATA__CASE__NAME = hydra.core.Name("case")
TEST_CASE_WITH_METADATA__DESCRIPTION__NAME = hydra.core.Name("description")
TEST_CASE_WITH_METADATA__TAGS__NAME = hydra.core.Name("tags")

@dataclass(frozen=True)
class TestGroup:
    r"""A collection of test cases with a name and optional description."""
    
    name: Annotated[str, "The name of the test group"]
    description: Annotated[Maybe[str], "An optional description of the group"]
    subgroups: Annotated[frozenlist[TestGroup], "Nested test groups"]
    cases: Annotated[frozenlist[TestCaseWithMetadata], "The test cases in this group"]

TEST_GROUP__NAME = hydra.core.Name("hydra.testing.TestGroup")
TEST_GROUP__NAME__NAME = hydra.core.Name("name")
TEST_GROUP__DESCRIPTION__NAME = hydra.core.Name("description")
TEST_GROUP__SUBGROUPS__NAME = hydra.core.Name("subgroups")
TEST_GROUP__CASES__NAME = hydra.core.Name("cases")

@dataclass(frozen=True)
class TypeCheckingTestCase:
    r"""A test case which performs type checking on a given term and compares the result with an expected annotated term and type."""
    
    input: Annotated[hydra.core.Term, "An untyped term on which to perform inference, then type check"]
    output_term: Annotated[hydra.core.Term, "The expected fully annotated System F term after type inference"]
    output_type: Annotated[hydra.core.Type, "The expected inferred type"]

TYPE_CHECKING_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TypeCheckingTestCase")
TYPE_CHECKING_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
TYPE_CHECKING_TEST_CASE__OUTPUT_TERM__NAME = hydra.core.Name("outputTerm")
TYPE_CHECKING_TEST_CASE__OUTPUT_TYPE__NAME = hydra.core.Name("outputType")

@dataclass(frozen=True)
class TypeCheckingFailureTestCase:
    r"""A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases."""
    
    input: Annotated[hydra.core.Term, "The term for which type checking should fail"]

TYPE_CHECKING_FAILURE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase")
TYPE_CHECKING_FAILURE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")

@dataclass(frozen=True)
class TopologicalSortBindingsTestCase:
    r"""A test case which performs topological sort on a map of bindings (name -> term) and compares the result with expected groups of bindings in topological order."""
    
    bindings: Annotated[frozenlist[tuple[hydra.core.Name, hydra.core.Term]], "The bindings as a list of (name, term) pairs"]
    expected: Annotated[frozenlist[frozenlist[tuple[hydra.core.Name, hydra.core.Term]]], "The expected groups of bindings in topological order"]

TOPOLOGICAL_SORT_BINDINGS_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TopologicalSortBindingsTestCase")
TOPOLOGICAL_SORT_BINDINGS_TEST_CASE__BINDINGS__NAME = hydra.core.Name("bindings")
TOPOLOGICAL_SORT_BINDINGS_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

@dataclass(frozen=True)
class TopologicalSortTestCase:
    r"""A test case which performs topological sort on a directed graph and compares the result with either an expected sorted list or expected cycles."""
    
    adjacency_list: Annotated[frozenlist[tuple[int, frozenlist[int]]], "The directed graph as an adjacency list (node to list of dependencies)"]
    expected: Annotated[Either[frozenlist[frozenlist[int]], frozenlist[int]], "The expected result: Left for cycles, Right for sorted nodes"]

TOPOLOGICAL_SORT_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TopologicalSortTestCase")
TOPOLOGICAL_SORT_TEST_CASE__ADJACENCY_LIST__NAME = hydra.core.Name("adjacencyList")
TOPOLOGICAL_SORT_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

@dataclass(frozen=True)
class TopologicalSortSCCTestCase:
    r"""A test case which performs topological sort with strongly connected component detection and compares the result with expected components."""
    
    adjacency_list: Annotated[frozenlist[tuple[int, frozenlist[int]]], "The directed graph as an adjacency list"]
    expected: Annotated[frozenlist[frozenlist[int]], "The expected strongly connected components in topological order"]

TOPOLOGICAL_SORT_S_C_C_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TopologicalSortSCCTestCase")
TOPOLOGICAL_SORT_S_C_C_TEST_CASE__ADJACENCY_LIST__NAME = hydra.core.Name("adjacencyList")
TOPOLOGICAL_SORT_S_C_C_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

@dataclass(frozen=True)
class SerializationTestCase:
    r"""A test case which serializes an AST expression to a string and compares it with the expected output."""
    
    input: Annotated[hydra.ast.Expr, "The AST expression to serialize"]
    output: Annotated[str, "The expected serialized string"]

SERIALIZATION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.SerializationTestCase")
SERIALIZATION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
SERIALIZATION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class SimplifyTermTestCase:
    r"""A test case which performs term simplification (beta reduction and optimization) and compares the result with the expected term."""
    
    input: Annotated[hydra.core.Term, "The term to simplify"]
    output: Annotated[hydra.core.Term, "The expected simplified term"]

SIMPLIFY_TERM_TEST_CASE__NAME = hydra.core.Name("hydra.testing.SimplifyTermTestCase")
SIMPLIFY_TERM_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
SIMPLIFY_TERM_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class NormalizeTypeVariablesTestCase:
    r"""A test case which normalizes type variables in a term (renaming them to t0, t1, t2, etc.) and compares the result with the expected term."""
    
    input: Annotated[hydra.core.Term, "The term with type annotations to normalize"]
    output: Annotated[hydra.core.Term, "The expected term with normalized type variable names"]

NORMALIZE_TYPE_VARIABLES_TEST_CASE__NAME = hydra.core.Name("hydra.testing.NormalizeTypeVariablesTestCase")
NORMALIZE_TYPE_VARIABLES_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
NORMALIZE_TYPE_VARIABLES_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class TypeReductionTestCase:
    r"""A test case which performs beta reduction on a type (reducing type applications) and compares the result with the expected type."""
    
    input: Annotated[hydra.core.Type, "The type to reduce"]
    output: Annotated[hydra.core.Type, "The expected reduced type"]

TYPE_REDUCTION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TypeReductionTestCase")
TYPE_REDUCTION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
TYPE_REDUCTION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class WriterTestCase(Generic[A]):
    r"""A test case which writes a value to a string and compares it to the expected string."""
    
    input: Annotated[A, "The input value to write"]
    output: Annotated[str, "The expected string"]

WRITER_TEST_CASE__NAME = hydra.core.Name("hydra.testing.WriterTestCase")
WRITER_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
WRITER_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class SubstInTypeTestCase:
    r"""A test case which applies a type substitution to a type and compares the result. The substitution is provided as a list of (variable name, replacement type) pairs."""
    
    substitution: Annotated[frozenlist[tuple[hydra.core.Name, hydra.core.Type]], "The type substitution as a list of (name, type) pairs"]
    input: Annotated[hydra.core.Type, "The type to substitute into"]
    output: Annotated[hydra.core.Type, "The expected result type"]

SUBST_IN_TYPE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.SubstInTypeTestCase")
SUBST_IN_TYPE_TEST_CASE__SUBSTITUTION__NAME = hydra.core.Name("substitution")
SUBST_IN_TYPE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
SUBST_IN_TYPE_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass(frozen=True)
class VariableOccursInTypeTestCase:
    r"""A test case which checks whether a type variable occurs in a type expression. This is the occur check used in type unification."""
    
    variable: Annotated[hydra.core.Name, "The variable name to search for"]
    type: Annotated[hydra.core.Type, "The type to search within"]
    expected: Annotated[bool, "Whether the variable occurs in the type"]

VARIABLE_OCCURS_IN_TYPE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.VariableOccursInTypeTestCase")
VARIABLE_OCCURS_IN_TYPE_TEST_CASE__VARIABLE__NAME = hydra.core.Name("variable")
VARIABLE_OCCURS_IN_TYPE_TEST_CASE__TYPE__NAME = hydra.core.Name("type")
VARIABLE_OCCURS_IN_TYPE_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

@dataclass(frozen=True)
class UnifyTypesTestCase:
    r"""A test case which attempts to unify two types and compares the result. The expected result is either Left (failure message substring) or Right (substitution)."""
    
    schema_types: Annotated[frozenlist[hydra.core.Name], "The schema types map (type variable names that should not be bound)"]
    left: Annotated[hydra.core.Type, "The left type to unify"]
    right: Annotated[hydra.core.Type, "The right type to unify"]
    expected: Annotated[Either[str, hydra.typing.TypeSubst], "The expected result: Left for failure (substring of error), Right for substitution"]

UNIFY_TYPES_TEST_CASE__NAME = hydra.core.Name("hydra.testing.UnifyTypesTestCase")
UNIFY_TYPES_TEST_CASE__SCHEMA_TYPES__NAME = hydra.core.Name("schemaTypes")
UNIFY_TYPES_TEST_CASE__LEFT__NAME = hydra.core.Name("left")
UNIFY_TYPES_TEST_CASE__RIGHT__NAME = hydra.core.Name("right")
UNIFY_TYPES_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")

@dataclass(frozen=True)
class JoinTypesTestCase:
    r"""A test case which joins two types (producing type constraints or failing). The expected result is either Left (failure) or Right (list of constraints)."""
    
    left: Annotated[hydra.core.Type, "The left type to join"]
    right: Annotated[hydra.core.Type, "The right type to join"]
    expected: Annotated[Either[None, frozenlist[hydra.typing.TypeConstraint]], "The expected result: Left for failure, Right for constraints"]

JOIN_TYPES_TEST_CASE__NAME = hydra.core.Name("hydra.testing.JoinTypesTestCase")
JOIN_TYPES_TEST_CASE__LEFT__NAME = hydra.core.Name("left")
JOIN_TYPES_TEST_CASE__RIGHT__NAME = hydra.core.Name("right")
JOIN_TYPES_TEST_CASE__EXPECTED__NAME = hydra.core.Name("expected")
