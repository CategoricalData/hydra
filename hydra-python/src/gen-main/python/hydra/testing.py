"""A model for unit testing."""

from __future__ import annotations
from dataclasses import dataclass
from enum import Enum
from hydra.dsl.python import Node
import hydra.core
import hydra.mantle

class EvaluationStyle(Enum):
    """One of two evaluation styles: eager or lazy."""
    
    EAGER = "eager"
    
    LAZY = "lazy"

@dataclass
class CaseConversionTestCase:
    """A test case which checks that strings are converted between different case conventions correctly."""
    
    from_convention: hydra.mantle.CaseConvention
    to_convention: hydra.mantle.CaseConvention
    from_string: str
    to_string: str

@dataclass
class EvaluationTestCase:
    """A test case which evaluates (reduces) a given term and compares it with the expected result."""
    
    evaluation_style: EvaluationStyle
    input: hydra.core.Term
    output: hydra.core.Term

@dataclass
class InferenceTestCase:
    """A test case which performs type inference on a given term and compares the result with an expected type scheme."""
    
    input: hydra.core.Term
    output: hydra.core.TypeScheme

type Tag = str

class TestCaseCaseConversion(Node["CaseConversionTestCase"]): ...

class TestCaseEvaluation(Node["EvaluationTestCase"]): ...

class TestCaseInference(Node["InferenceTestCase"]): ...

# A simple test case with an input and an expected output.
type TestCase = TestCaseCaseConversion | TestCaseEvaluation | TestCaseInference

@dataclass
class TestCaseWithMetadata:
    """One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags."""
    
    name: str
    case: TestCase
    description: str | None
    tags: list[Tag]

@dataclass
class TestGroup:
    """A collection of test cases with a name and optional description."""
    
    name: str
    description: str | None
    subgroups: list[TestGroup]
    cases: list[TestCaseWithMetadata]