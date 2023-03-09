module Hydra.TestSuiteSpec where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.TestUtils
import Hydra.Testing

import Hydra.Test.TestSuite

import qualified Control.Monad as CM
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.Map as M
import qualified Data.Maybe as Y


runTestSuite :: H.SpecWith ()
runTestSuite = do
    runTestGroup allTests

runTestCase :: TestCase Kv -> H.SpecWith ()
runTestCase tc = H.it desc $
  shouldSucceedWith
    (eval $ testCaseInput tc)
    (testCaseOutput tc)
  where
    desc = Y.fromMaybe "Anonymous test case" $ testCaseDescription tc

runTestGroup :: TestGroup Kv -> H.SpecWith ()
runTestGroup tg = do
    H.describe desc $ do
      CM.sequence (runTestCase <$> (testGroupCases tg))
      
      CM.sequence (runTestGroup <$> (testGroupSubgroups tg))
      return ()
  where
    desc = testGroupName tg ++ case testGroupDescription tg of
      Nothing -> ""
      Just d -> " (" ++ d ++ ")"

spec :: H.Spec
spec = do
  runTestSuite
