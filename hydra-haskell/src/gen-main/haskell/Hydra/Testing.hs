-- Note: this is an automatically generated file. Do not edit.

-- | A model for unit testing

module Hydra.Testing where

import qualified Hydra.Core as Core
import qualified Hydra.Util as Util
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
    -- | The source case convention
    caseConversionTestCaseFromConvention :: Util.CaseConvention,
    -- | The target case convention
    caseConversionTestCaseToConvention :: Util.CaseConvention,
    -- | The input string
    caseConversionTestCaseFromString :: String,
    -- | The expected output string
    caseConversionTestCaseToString :: String}
  deriving (Eq, Ord, Read, Show)

_CaseConversionTestCase = (Core.Name "hydra.testing.CaseConversionTestCase")

_CaseConversionTestCase_fromConvention = (Core.Name "fromConvention")

_CaseConversionTestCase_toConvention = (Core.Name "toConvention")

_CaseConversionTestCase_fromString = (Core.Name "fromString")

_CaseConversionTestCase_toString = (Core.Name "toString")

-- | A test case which performs eta expansion (adding missing lambda abstractions) on a given term and compares the result with the expected result
data EtaExpansionTestCase = 
  EtaExpansionTestCase {
    -- | The term to eta expand
    etaExpansionTestCaseInput :: Core.Term,
    -- | The expected result
    etaExpansionTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_EtaExpansionTestCase = (Core.Name "hydra.testing.EtaExpansionTestCase")

_EtaExpansionTestCase_input = (Core.Name "input")

_EtaExpansionTestCase_output = (Core.Name "output")

-- | A test case which evaluates (reduces) a given term and compares it with the expected result
data EvaluationTestCase = 
  EvaluationTestCase {
    -- | The evaluation style (eager or lazy)
    evaluationTestCaseEvaluationStyle :: EvaluationStyle,
    -- | The term to evaluate
    evaluationTestCaseInput :: Core.Term,
    -- | The expected result
    evaluationTestCaseOutput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_EvaluationTestCase = (Core.Name "hydra.testing.EvaluationTestCase")

_EvaluationTestCase_evaluationStyle = (Core.Name "evaluationStyle")

_EvaluationTestCase_input = (Core.Name "input")

_EvaluationTestCase_output = (Core.Name "output")

-- | A test case providing a term for which type inference is expected to fail
data InferenceFailureTestCase = 
  InferenceFailureTestCase {
    -- | The term for which inference should fail
    inferenceFailureTestCaseInput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_InferenceFailureTestCase = (Core.Name "hydra.testing.InferenceFailureTestCase")

_InferenceFailureTestCase_input = (Core.Name "input")

-- | A test case which performs type inference on a given term and compares the result with an expected type scheme
data InferenceTestCase = 
  InferenceTestCase {
    -- | The term to infer
    inferenceTestCaseInput :: Core.Term,
    -- | The expected type scheme
    inferenceTestCaseOutput :: Core.TypeScheme}
  deriving (Eq, Ord, Read, Show)

_InferenceTestCase = (Core.Name "hydra.testing.InferenceTestCase")

_InferenceTestCase_input = (Core.Name "input")

_InferenceTestCase_output = (Core.Name "output")

-- | A tag for categorizing test cases
newtype Tag = 
  Tag {
    unTag :: String}
  deriving (Eq, Ord, Read, Show)

_Tag = (Core.Name "hydra.testing.Tag")

-- | A simple test case with an input and an expected output
data TestCase = 
  -- | A case conversion test
  TestCaseCaseConversion CaseConversionTestCase |
  -- | An eta expansion test
  TestCaseEtaExpansion EtaExpansionTestCase |
  -- | A term evaluation test
  TestCaseEvaluation EvaluationTestCase |
  -- | A type inference test
  TestCaseInference InferenceTestCase |
  -- | A type inference failure test
  TestCaseInferenceFailure InferenceFailureTestCase |
  -- | A type checking test
  TestCaseTypeChecking TypeCheckingTestCase |
  -- | A type checking failure test (currently unused)
  TestCaseTypeCheckingFailure TypeCheckingFailureTestCase
  deriving (Eq, Ord, Read, Show)

_TestCase = (Core.Name "hydra.testing.TestCase")

_TestCase_caseConversion = (Core.Name "caseConversion")

_TestCase_etaExpansion = (Core.Name "etaExpansion")

_TestCase_evaluation = (Core.Name "evaluation")

_TestCase_inference = (Core.Name "inference")

_TestCase_inferenceFailure = (Core.Name "inferenceFailure")

_TestCase_typeChecking = (Core.Name "typeChecking")

_TestCase_typeCheckingFailure = (Core.Name "typeCheckingFailure")

-- | One of a number of test case variants, together with metadata including a test name, an optional description, and optional tags
data TestCaseWithMetadata = 
  TestCaseWithMetadata {
    -- | The name of the test case
    testCaseWithMetadataName :: String,
    -- | The test case itself
    testCaseWithMetadataCase :: TestCase,
    -- | An optional description of the test
    testCaseWithMetadataDescription :: (Maybe String),
    -- | Zero or more tags for categorizing the test
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
    -- | The name of the test group
    testGroupName :: String,
    -- | An optional description of the group
    testGroupDescription :: (Maybe String),
    -- | Nested test groups
    testGroupSubgroups :: [TestGroup],
    -- | The test cases in this group
    testGroupCases :: [TestCaseWithMetadata]}
  deriving (Eq, Ord, Read, Show)

_TestGroup = (Core.Name "hydra.testing.TestGroup")

_TestGroup_name = (Core.Name "name")

_TestGroup_description = (Core.Name "description")

_TestGroup_subgroups = (Core.Name "subgroups")

_TestGroup_cases = (Core.Name "cases")

-- | A test case which performs type checking on a given term and compares the result with an expected annotated term and type
data TypeCheckingTestCase = 
  TypeCheckingTestCase {
    -- | An untyped term on which to perform inference, then type check
    typeCheckingTestCaseInput :: Core.Term,
    -- | The expected fully annotated System F term after type inference
    typeCheckingTestCaseOutputTerm :: Core.Term,
    -- | The expected inferred type
    typeCheckingTestCaseOutputType :: Core.Type}
  deriving (Eq, Ord, Read, Show)

_TypeCheckingTestCase = (Core.Name "hydra.testing.TypeCheckingTestCase")

_TypeCheckingTestCase_input = (Core.Name "input")

_TypeCheckingTestCase_outputTerm = (Core.Name "outputTerm")

_TypeCheckingTestCase_outputType = (Core.Name "outputType")

-- | A test case providing a term for which type checking is expected to fail. Note: there are currently no such test cases.
data TypeCheckingFailureTestCase = 
  TypeCheckingFailureTestCase {
    -- | The term for which type checking should fail
    typeCheckingFailureTestCaseInput :: Core.Term}
  deriving (Eq, Ord, Read, Show)

_TypeCheckingFailureTestCase = (Core.Name "hydra.testing.TypeCheckingFailureTestCase")

_TypeCheckingFailureTestCase_input = (Core.Name "input")
