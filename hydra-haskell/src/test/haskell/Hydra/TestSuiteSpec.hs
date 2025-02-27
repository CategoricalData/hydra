{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.TestSuiteSpec.spec
-}

module Hydra.TestSuiteSpec where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.TestUtils
import Hydra.Testing
import Hydra.Staging.Inference.Inference
import Hydra.Test.TestSuite
import qualified Hydra.Dsl.Testing as Testing

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


type TestRunner = TestCaseWithMetadata -> Y.Maybe H.Expectation

defaultTestRunner :: TestRunner
defaultTestRunner tcase = if Testing.isDisabled tcase || Testing.isDisabledForDefaultInference tcase
  then Nothing
  else Just $ case testCaseWithMetadataCase tcase of
    TestCaseCaseConversion (CaseConversionTestCase fromConvention toConvention fromString toString) -> H.shouldBe
      (convertCase fromConvention toConvention fromString)
      toString
    TestCaseEvaluation (EvaluationTestCase _ input output) -> shouldSucceedWith
      (eval input)
      output
    TestCaseInference (InferenceTestCase input output) -> shouldSucceedWith
      (snd <$> inferTypeAndConstraints input)
      output

runTestCase :: TestRunner -> TestCaseWithMetadata -> H.SpecWith ()
runTestCase runner tcase@(TestCaseWithMetadata name _ mdesc _) = case runner tcase of
    Nothing -> return ()
    Just e -> H.it desc e
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc

runTestGroup :: TestRunner -> TestGroup -> H.SpecWith ()
runTestGroup runner tg = do
    H.describe desc $ do
      CM.mapM (runTestCase runner) $ testGroupCases tg
      CM.sequence (runTestGroup runner <$> (testGroupSubgroups tg))
      return ()
  where
    desc = testGroupName tg ++ descSuffix
    descSuffix = case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"

spec :: H.Spec
spec = runTestGroup defaultTestRunner allTests
