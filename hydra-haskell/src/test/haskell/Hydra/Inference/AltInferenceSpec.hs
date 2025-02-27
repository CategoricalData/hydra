{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.Inference.AltInferenceSpec.spec
-}
module Hydra.Inference.AltInferenceSpec where

import Hydra.Kernel
import Hydra.Staging.Inference.AltInference

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Terms as Terms

import qualified Hydra.TestUtils as TU
import Hydra.Testing
import Hydra.TestSuiteSpec
import Hydra.Test.TestSuite
import qualified Hydra.Dsl.Testing as Testing


_unify t1 t2 = uUnify [TypeConstraint t1 t2 $ Just "ctx"]

sTestLexicon = M.fromList [
  (Name "add", Types.mono $ Types.function Types.int32 Types.int32),
  (Name "primPred", Types.mono $ Types.function Types.int32 Types.int32),
  (Name "primSucc", Types.mono $ Types.function Types.int32 Types.int32)]

sInitialContext = SInferenceContext sTestLexicon 0 M.empty

expectType :: Term -> TypeScheme -> H.Expectation
expectType term expected = shouldSucceedWith (sInferType term) expected

shouldSucceedWith :: (Eq a, Show a) => Flow SInferenceContext a -> a -> H.Expectation
shouldSucceedWith f x = case my of
    Nothing -> HL.assertFailure $ "Error: " ++ traceSummary trace
    Just y -> y `H.shouldBe` x
  where
    FlowState my _ trace = unFlow f sInitialContext emptyTrace

altInferenceTestRunner :: TestRunner
altInferenceTestRunner tcase = if Testing.isDisabled tcase || Testing.isDisabledForAltInference tcase
  then Nothing
  else case testCaseWithMetadataCase tcase of
    TestCaseInference (InferenceTestCase input output) -> Just $ expectType input output
    _ -> Nothing

spec :: H.Spec
spec = runTestGroup altInferenceTestRunner allTests
