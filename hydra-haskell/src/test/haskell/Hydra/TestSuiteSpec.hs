module Hydra.TestSuiteSpec where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.TestUtils
import Hydra.Testing
import Hydra.Staging.Inference

import Hydra.Test.TestSuite

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.TestSuiteSpec.spec
-}

runTestCase :: TestCaseWithMetadata -> H.SpecWith ()
runTestCase (TestCaseWithMetadata name tcase mdesc _) = H.it desc $
    case tcase of
      TestCaseCaseConversion ccase -> runCaseConversionTestCase ccase
      TestCaseEvaluation ecase -> runEvaluationTestCase ecase
      TestCaseInference icase -> runInferenceTestCase icase
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc
    runCaseConversionTestCase (CaseConversionTestCase fromConvention toConvention fromString toString) = H.shouldBe
      (convertCase fromConvention toConvention fromString)
      toString
    runEvaluationTestCase (EvaluationTestCase _ input output) = shouldSucceedWith
      (eval input)
      output
    runInferenceTestCase (InferenceTestCase input output) = shouldSucceedWith
      (snd <$> inferTypeAndConstraints input)
      output

runTestGroup :: TestGroup -> H.SpecWith ()
runTestGroup tg = do
    H.describe desc $ do
      CM.mapM runTestCase nonDisabled
      CM.sequence (runTestGroup <$> (testGroupSubgroups tg))
      return ()
  where
    cases = testGroupCases tg
    desc = testGroupName tg ++ disabledSuffix ++ descSuffix
    descSuffix = case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"
    disabledSuffix = if count > 0 then " [" ++ show count ++ " disabled]" else ""
      where
        count = L.length cases - L.length nonDisabled
    isDisabled tcase = disabledTag `L.elem` testCaseWithMetadataTags tcase
    disabledTag = Tag "disabled"
    nonDisabled = L.filter (not . isDisabled) cases

spec :: H.Spec
spec = runTestGroup allTests
