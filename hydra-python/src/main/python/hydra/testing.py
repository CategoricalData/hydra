# Note: this is an automatically generated file. Do not edit.

r"""A model for unit testing."""

from __future__ import annotations
from collections.abc import Callable
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated
import hydra.coders
import hydra.compute
import hydra.core
import hydra.graph
import hydra.module
import hydra.util

class EvaluationStyle(Enum):
    r"""One of two evaluation styles: eager or lazy."""
    
    EAGER = "eager"
    
    LAZY = "lazy"

EVALUATION_STYLE__NAME = hydra.core.Name("hydra.testing.EvaluationStyle")
EVALUATION_STYLE__EAGER__NAME = hydra.core.Name("eager")
EVALUATION_STYLE__LAZY__NAME = hydra.core.Name("lazy")

@dataclass
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

@dataclass
class DelegatedEvaluationTestCase:
    r"""A test case in which we delegate evaluation of an input term and an expected output term to a target programming language like Haskell, Java, or Python, checking whether the term evaluates as expected when translated into that language."""
    
    input: Annotated[hydra.core.Term, "The first of two terms which should evaluate to the same expression"]
    output: Annotated[hydra.core.Term, "The second of two terms which should evaluate to the same expression"]

DELEGATED_EVALUATION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.DelegatedEvaluationTestCase")
DELEGATED_EVALUATION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
DELEGATED_EVALUATION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass
class EtaExpansionTestCase:
    r"""A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result."""
    
    input: Annotated[hydra.core.Term, "The term to eta expand"]
    output: Annotated[hydra.core.Term, "The expected result"]

ETA_EXPANSION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.EtaExpansionTestCase")
ETA_EXPANSION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
ETA_EXPANSION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass
class EvaluationTestCase:
    r"""A test case which evaluates (reduces) a given term and compares it with the expected result."""
    
    evaluation_style: Annotated[EvaluationStyle, "The evaluation style (eager or lazy)"]
    input: Annotated[hydra.core.Term, "The term to evaluate"]
    output: Annotated[hydra.core.Term, "The expected result"]

EVALUATION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.EvaluationTestCase")
EVALUATION_TEST_CASE__EVALUATION_STYLE__NAME = hydra.core.Name("evaluationStyle")
EVALUATION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
EVALUATION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass
class InferenceFailureTestCase:
    r"""A test case providing a term for which type inference is expected to fail."""
    
    input: Annotated[hydra.core.Term, "The term for which inference should fail"]

INFERENCE_FAILURE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.InferenceFailureTestCase")
INFERENCE_FAILURE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")

@dataclass
class InferenceTestCase:
    r"""A test case which performs type inference on a given term and compares the result with an expected type scheme."""
    
    input: Annotated[hydra.core.Term, "The term to infer"]
    output: Annotated[hydra.core.TypeScheme, "The expected type scheme"]

INFERENCE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.InferenceTestCase")
INFERENCE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
INFERENCE_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class Tag(Node[str]):
    r"""A tag for categorizing test cases."""

TAG__NAME = hydra.core.Name("hydra.testing.Tag")

@dataclass
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

class TestCaseCaseConversion(Node["CaseConversionTestCase"]):
    r"""A case conversion test."""

class TestCaseDelegatedEvaluation(Node["DelegatedEvaluationTestCase"]):
    r"""A delegated evaluation test."""

class TestCaseEtaExpansion(Node["EtaExpansionTestCase"]):
    r"""An eta expansion test."""

class TestCaseEvaluation(Node["EvaluationTestCase"]):
    r"""A term evaluation test."""

class TestCaseInference(Node["InferenceTestCase"]):
    r"""A type inference test."""

class TestCaseInferenceFailure(Node["InferenceFailureTestCase"]):
    r"""A type inference failure test."""

class TestCaseTypeChecking(Node["TypeCheckingTestCase"]):
    r"""A type checking test."""

class TestCaseTypeCheckingFailure(Node["TypeCheckingFailureTestCase"]):
    r"""A type checking failure test (currently unused)."""

# A simple test case with an input and an expected output.
type TestCase = TestCaseCaseConversion | TestCaseDelegatedEvaluation | TestCaseEtaExpansion | TestCaseEvaluation | TestCaseInference | TestCaseInferenceFailure | TestCaseTypeChecking | TestCaseTypeCheckingFailure

TEST_CASE__NAME = hydra.core.Name("hydra.testing.TestCase")
TEST_CASE__CASE_CONVERSION__NAME = hydra.core.Name("caseConversion")
TEST_CASE__DELEGATED_EVALUATION__NAME = hydra.core.Name("delegatedEvaluation")
TEST_CASE__ETA_EXPANSION__NAME = hydra.core.Name("etaExpansion")
TEST_CASE__EVALUATION__NAME = hydra.core.Name("evaluation")
TEST_CASE__INFERENCE__NAME = hydra.core.Name("inference")
TEST_CASE__INFERENCE_FAILURE__NAME = hydra.core.Name("inferenceFailure")
TEST_CASE__TYPE_CHECKING__NAME = hydra.core.Name("typeChecking")
TEST_CASE__TYPE_CHECKING_FAILURE__NAME = hydra.core.Name("typeCheckingFailure")

@dataclass
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

@dataclass
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

@dataclass
class TypeCheckingTestCase:
    r"""A test case which performs type checking on a given term and compares the result with an expected annotated term and type."""
    
    input: Annotated[hydra.core.Term, "An untyped term on which to perform inference, then type check"]
    output_term: Annotated[hydra.core.Term, "The expected fully annotated System F term after type inference"]
    output_type: Annotated[hydra.core.Type, "The expected inferred type"]

TYPE_CHECKING_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TypeCheckingTestCase")
TYPE_CHECKING_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
TYPE_CHECKING_TEST_CASE__OUTPUT_TERM__NAME = hydra.core.Name("outputTerm")
TYPE_CHECKING_TEST_CASE__OUTPUT_TYPE__NAME = hydra.core.Name("outputType")

@dataclass
class TypeCheckingFailureTestCase:
    r"""A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases."""
    
    input: Annotated[hydra.core.Term, "The term for which type checking should fail"]

TYPE_CHECKING_FAILURE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.TypeCheckingFailureTestCase")
TYPE_CHECKING_FAILURE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
