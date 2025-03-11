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
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.Terms as Terms
import Hydra.Sources.Libraries

import qualified Hydra.TestUtils as TU
import Hydra.Testing
import Hydra.TestSuiteSpec
import Hydra.Test.TestSuite
import Hydra.Staging.TestGraph
import qualified Hydra.Dsl.Testing as Testing


initialContext :: AltInferenceContext
initialContext = AltInferenceContext schemaTypes primTypes varTypes
  where
    primTypes = M.fromList $ fmap (\p -> (primitiveName p, primitiveType p)) (L.concat (libraryPrimitives <$> standardLibraries))
    schemaTypes = fromFlow M.empty testGraph $ schemaGraphToTypingEnvironment testSchemaGraph
    varTypes = M.empty

expectType :: String -> Term -> TypeScheme -> H.Expectation
expectType desc term expected = shouldSucceedWith desc (inferTypeOf initialContext term) expected

shouldSucceedWith :: (Eq a, Show a) => String -> Flow () a -> a -> H.Expectation
shouldSucceedWith desc f x = case my of
    Nothing -> HL.assertFailure $ "Error: " ++ traceSummary trace
    Just y -> y `H.shouldBe` x
  where
    FlowState my _ trace = unFlow f2 () emptyTrace
    f2 = do
      putAttr key_debugId $ Terms.string desc
      f

altInferenceTestRunner :: TestRunner
--altInferenceTestRunner desc tcase = if False
altInferenceTestRunner desc tcase = if Testing.isDisabled tcase
  then Nothing
  else case testCaseWithMetadataCase tcase of
    TestCaseInference (InferenceTestCase input output) -> Just $ expectType desc input output
    _ -> Nothing

spec :: H.Spec
spec = runTestGroup "" altInferenceTestRunner allTests
