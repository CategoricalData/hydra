module Hydra.TestSuiteSpec where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.TestUtils
import Hydra.Testing

import Hydra.Test.TestSuite

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as Y


runTestSuite :: H.SpecWith ()
runTestSuite = do
    runTestGroup allTests

runTestCase :: TestCaseWithMetadata -> H.SpecWith ()
runTestCase (TestCaseWithMetadata name tcase mdesc _) = H.it desc $
    case tcase of
      TestCaseEvaluation ecase -> runEvaluationTestCase ecase
      TestCaseInference icase -> runInferenceTestCase icase
  where
    desc = name ++ Y.maybe ("") (\d -> ": " ++ d) mdesc
    runEvaluationTestCase (EvaluationTestCase _ input output) = shouldSucceedWith
      (eval input)
      output
    runInferenceTestCase _ = fail "inference test cases are not yet supported"

runTestGroup :: TestGroup -> H.SpecWith ()
runTestGroup tg = do
    H.describe desc $ do
      CM.mapM runTestCase $ testGroupCases tg
      CM.sequence (runTestGroup <$> (testGroupSubgroups tg))
      return ()
  where
    desc = testGroupName tg ++ case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"

spec :: H.Spec
spec = do
  runTestSuite
