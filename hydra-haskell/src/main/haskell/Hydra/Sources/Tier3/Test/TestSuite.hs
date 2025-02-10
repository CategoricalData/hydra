module Hydra.Sources.Tier3.Test.TestSuite (testSuiteModule) where

import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier2.All
import Hydra.Dsl.Base as Base

import Hydra.Sources.Tier3.Test.Lib.Lists
import Hydra.Sources.Tier3.Test.Lib.Strings
import Hydra.Sources.Tier3.Test.Inference


testSuiteNs = Namespace "hydra/test/testSuite"

testSuiteModule :: Module
testSuiteModule = Module testSuiteNs elements [] [hydraCoreModule, hydraTestingModule] $
    Just "Test cases for primitive functions"
  where
    elements = [
      groupElement "allTests" allTests]

groupElement :: String -> TestGroup -> Element
groupElement lname group = Element name $ setTermType (Just typ) $ encodeGroup group
  where
    encodeGroup (TestGroup name desc groups cases) = Terms.record _TestGroup [
      Field _TestGroup_name $ Terms.string name,
      Field _TestGroup_description $ Terms.optional (Terms.string <$> desc),
      Field _TestGroup_subgroups $ Terms.list (encodeGroup <$> groups),
      Field _TestGroup_cases $ Terms.list (encodeCaseWithMetadata <$> cases)]
    encodeCaseWithMetadata (TestCaseWithMetadata name tcase mdesc tags) = Terms.record _TestCaseWithMetadata [
      Field _TestCaseWithMetadata_name $ Terms.string name,
      Field _TestCaseWithMetadata_case $ encodeCase tcase,
      Field _TestCaseWithMetadata_description $ Terms.optional (Terms.string <$> mdesc),
      Field _TestCaseWithMetadata_tags $ Terms.list (Terms.string . unTag <$> tags)]
    encodeCase tcase = case tcase of
      TestCaseEvaluation ecase -> Terms.variant _TestCase _TestCase_evaluation $ encodeEvaluationTestCase ecase
      TestCaseInference icase -> Terms.variant _TestCase _TestCase_inference $ encodeInferenceTestCase icase
    encodeEvaluationTestCase (EvaluationTestCase style input output) = Terms.record _EvaluationTestCase [
      Field _EvaluationTestCase_evaluationStyle $ Terms.variant _EvaluationStyle (case style of
        EvaluationStyleEager -> _EvaluationStyle_eager
        EvaluationStyleLazy -> _EvaluationStyle_lazy) Terms.unit,
      Field _EvaluationTestCase_input $ coreEncodeTerm input,
      Field _EvaluationTestCase_output $ coreEncodeTerm output]
    encodeInferenceTestCase (InferenceTestCase input output) = Terms.record _InferenceTestCase [
      Field _InferenceTestCase_input $ coreEncodeTerm input,
      Field _InferenceTestCase_output $ coreEncodeTypeScheme output]
    name = unqualifyName $ QualifiedName (Just testSuiteNs) lname
    typ = TypeVariable _TestGroup

allTests :: TestGroup
allTests = TestGroup "All tests" Nothing (primTests ++ [inferenceTests]) []
  where
    primTests = [
      listPrimitiveTests,
      stringPrimitiveTests]
