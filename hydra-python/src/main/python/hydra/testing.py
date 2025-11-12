# Note: this is an automatically generated file. Do not edit.

r"""A model for unit testing."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Maybe, Node, frozenlist
from typing import Annotated
import hydra.core
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

class TestCaseCaseConversion(Node["CaseConversionTestCase"]):
    r"""A case conversion test."""

class TestCaseEvaluation(Node["EvaluationTestCase"]):
    r"""A term evaluation test."""

class TestCaseInference(Node["InferenceTestCase"]):
    r"""A type inference test."""

class TestCaseInferenceFailure(Node["InferenceFailureTestCase"]):
    r"""A type inference failure test."""

# A simple test case with an input and an expected output.
type TestCase = TestCaseCaseConversion | TestCaseEvaluation | TestCaseInference | TestCaseInferenceFailure

TEST_CASE__NAME = hydra.core.Name("hydra.testing.TestCase")
TEST_CASE__CASE_CONVERSION__NAME = hydra.core.Name("caseConversion")
TEST_CASE__EVALUATION__NAME = hydra.core.Name("evaluation")
TEST_CASE__INFERENCE__NAME = hydra.core.Name("inference")
TEST_CASE__INFERENCE_FAILURE__NAME = hydra.core.Name("inferenceFailure")

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
