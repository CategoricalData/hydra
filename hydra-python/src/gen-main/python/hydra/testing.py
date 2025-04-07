"""A model for unit testing."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import frozenlist, Node
import hydra.core
import hydra.mantle

class EvaluationStyle(Enum):
    """One of two evaluation styles: eager or lazy."""
    
    EAGER = "eager"
    
    LAZY = "lazy"

EVALUATION_STYLE__NAME = hydra.core.Name("hydra.testing.EvaluationStyle")
EVALUATION_STYLE__EAGER__NAME = hydra.core.Name("eager")
EVALUATION_STYLE__LAZY__NAME = hydra.core.Name("lazy")

@dataclass
class CaseConversionTestCase:
    """A test case which checks that strings are converted between different case conventions correctly."""
    
    from_convention: hydra.mantle.CaseConvention
    to_convention: hydra.mantle.CaseConvention
    from_string: str
    to_string: str

CASE_CONVERSION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.CaseConversionTestCase")
CASE_CONVERSION_TEST_CASE__FROM_CONVENTION__NAME = hydra.core.Name("fromConvention")
CASE_CONVERSION_TEST_CASE__TO_CONVENTION__NAME = hydra.core.Name("toConvention")
CASE_CONVERSION_TEST_CASE__FROM_STRING__NAME = hydra.core.Name("fromString")
CASE_CONVERSION_TEST_CASE__TO_STRING__NAME = hydra.core.Name("toString")

@dataclass
class EvaluationTestCase:
    """A test case which evaluates (reduces) a given term and compares it with the expected result."""
    
    evaluation_style: EvaluationStyle
    input: hydra.core.Term
    output: hydra.core.Term

EVALUATION_TEST_CASE__NAME = hydra.core.Name("hydra.testing.EvaluationTestCase")
EVALUATION_TEST_CASE__EVALUATION_STYLE__NAME = hydra.core.Name("evaluationStyle")
EVALUATION_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
EVALUATION_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

@dataclass
class InferenceTestCase:
    """A test case which performs type inference on a given term and compares the result with an expected type scheme."""
    
    input: hydra.core.Term
    output: hydra.core.TypeScheme

INFERENCE_TEST_CASE__NAME = hydra.core.Name("hydra.testing.InferenceTestCase")
INFERENCE_TEST_CASE__INPUT__NAME = hydra.core.Name("input")
INFERENCE_TEST_CASE__OUTPUT__NAME = hydra.core.Name("output")

class Tag(Node[str]): ...

TAG__NAME = hydra.core.Name("hydra.testing.Tag")

class TestCaseCaseConversion(Node["CaseConversionTestCase"]): ...

class TestCaseEvaluation(Node["EvaluationTestCase"]): ...

class TestCaseInference(Node["InferenceTestCase"]): ...

# A simple test case with an input and an expected output.
type TestCase = TestCaseCaseConversion | TestCaseEvaluation | TestCaseInference

TEST_CASE__NAME = hydra.core.Name("hydra.testing.TestCase")
TEST_CASE__CASE_CONVERSION__NAME = hydra.core.Name("caseConversion")
TEST_CASE__EVALUATION__NAME = hydra.core.Name("evaluation")
TEST_CASE__INFERENCE__NAME = hydra.core.Name("inference")

@dataclass
class TestCaseWithMetadata:
    """One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags."""
    
    name: str
    case: TestCase
    description: str | None
    tags: frozenlist[Tag]

TEST_CASE_WITH_METADATA__NAME = hydra.core.Name("hydra.testing.TestCaseWithMetadata")
TEST_CASE_WITH_METADATA__NAME__NAME = hydra.core.Name("name")
TEST_CASE_WITH_METADATA__CASE__NAME = hydra.core.Name("case")
TEST_CASE_WITH_METADATA__DESCRIPTION__NAME = hydra.core.Name("description")
TEST_CASE_WITH_METADATA__TAGS__NAME = hydra.core.Name("tags")

@dataclass
class TestGroup:
    """A collection of test cases with a name and optional description."""
    
    name: str
    description: str | None
    subgroups: frozenlist[TestGroup]
    cases: frozenlist[TestCaseWithMetadata]

TEST_GROUP__NAME = hydra.core.Name("hydra.testing.TestGroup")
TEST_GROUP__NAME__NAME = hydra.core.Name("name")
TEST_GROUP__DESCRIPTION__NAME = hydra.core.Name("description")
TEST_GROUP__SUBGROUPS__NAME = hydra.core.Name("subgroups")
TEST_GROUP__CASES__NAME = hydra.core.Name("cases")
