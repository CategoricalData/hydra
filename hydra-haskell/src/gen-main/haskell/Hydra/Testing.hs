-- | A model for unit testing

module Hydra.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Mantle as Mantle
import Prelude hiding  (Enum, Ordering, fail, map, pure, sum)
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | One of two evaluation styles: eager or lazy
data EvaluationStyle = 
  EvaluationStyleEager  |
  EvaluationStyleLazy 
  deriving (Eq, Ord, Read, Show)

_EvaluationStyle = (Core.Name "hydra.testing.EvaluationStyle")

_EvaluationStyle_eager = (Core.Name "eager")

_EvaluationStyle_lazy = (Core.Name "lazy")

-- | A test case which checks that strings are converted between different case conventions correctly
data CaseConversionTestCase = 
  CaseConversionTestCase {
    caseConversionTestCaseFromConvention :: Mantle.CaseConvention,
    caseConversionTestCaseToConvention :: Mantle.CaseConvention,
    caseConversionTestCaseFromString :: String,
    caseConversionTestCaseToString :: String}
  deriving (Eq, Ord, Read, Show)

_CaseConversionTestCase = (Core.Name "hydra.testing.CaseConversionTestCase")

_CaseConversionTestCase_fromConvention = (Core.Name "fromConvention")

_CaseConversionTestCase_toConvention = (Core.Name "toConvention")

_CaseConversionTestCase_fromString = (Core.Name "fromString")

_CaseConversionTestCase_toString = (Core.Name "toString")

-- | A test case which evaluates (reduces) a given term and compares it with the expected result
data EvaluationTestCase = 
  EvaluationTestCase {
    evaluationTestCaseEvaluationStyle :: EvaluationStyle,
    evaluationTestCaseInput :: Core.Term,
    evaluationTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_EvaluationTestCase = (Core.Name "hydra.testing.EvaluationTestCase")

_EvaluationTestCase_evaluationStyle = (Core.Name "evaluationStyle")

_EvaluationTestCase_input = (Core.Name "input")

_EvaluationTestCase_output = (Core.Name "output")

-- | A test case providing a term for which type inference is expected to fail
data InferenceFailureTestCase = 
  InferenceFailureTestCase {
    inferenceFailureTestCaseInput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_InferenceFailureTestCase = (Core.Name "hydra.testing.InferenceFailureTestCase")

_InferenceFailureTestCase_input = (Core.Name "input")

-- | A test case which performs type inference on a given term and compares the result with an expected type scheme
data InferenceTestCase = 
  InferenceTestCase {
    inferenceTestCaseInput :: Core.Term,
    inferenceTestCaseOutput :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)

_InferenceTestCase = (Core.Name "hydra.testing.InferenceTestCase")

_InferenceTestCase_input = (Core.Name "input")

_InferenceTestCase_output = (Core.Name "output")

newtype Tag = 
  Tag {
    unTag :: String}
  deriving (Eq, Ord, Read, Show)

_Tag = (Core.Name "hydra.testing.Tag")

-- | A simple test case with an input and an expected output
data TestCase = 
  TestCaseCaseConversion CaseConversionTestCase |
  TestCaseEvaluation EvaluationTestCase |
  TestCaseInference InferenceTestCase |
  TestCaseInferenceFailure InferenceFailureTestCase
  deriving (Eq, Ord, Read, Show)

_TestCase = (Core.Name "hydra.testing.TestCase")

_TestCase_caseConversion = (Core.Name "caseConversion")

_TestCase_evaluation = (Core.Name "evaluation")

_TestCase_inference = (Core.Name "inference")

_TestCase_inferenceFailure = (Core.Name "inferenceFailure")

-- | One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags
data TestCaseWithMetadata = 
  TestCaseWithMetadata {
    testCaseWithMetadataName :: String,
    testCaseWithMetadataCase :: TestCase,
    testCaseWithMetadataDescription :: (Maybe String),
    testCaseWithMetadataTags :: [Tag]}
  deriving (Eq, Ord, Read, Show)

_TestCaseWithMetadata = (Core.Name "hydra.testing.TestCaseWithMetadata")

_TestCaseWithMetadata_name = (Core.Name "name")

_TestCaseWithMetadata_case = (Core.Name "case")

_TestCaseWithMetadata_description = (Core.Name "description")

_TestCaseWithMetadata_tags = (Core.Name "tags")

-- | A collection of test cases with a name and optional description
data TestGroup = 
  TestGroup {
    testGroupName :: String,
    testGroupDescription :: (Maybe String),
    testGroupSubgroups :: [TestGroup],
    testGroupCases :: [TestCaseWithMetadata]}
  deriving (Eq, Ord, Read, Show)

_TestGroup = (Core.Name "hydra.testing.TestGroup")

_TestGroup_name = (Core.Name "name")

_TestGroup_description = (Core.Name "description")

_TestGroup_subgroups = (Core.Name "subgroups")

_TestGroup_cases = (Core.Name "cases")
