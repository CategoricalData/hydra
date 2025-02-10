module Hydra.Sources.Tier3.Test.Inference (inferenceTests) where

import Hydra.Kernel
import Hydra.Dsl.Tests
import Hydra.Dsl.Terms
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Types as Types


inferenceTests :: TestGroup
inferenceTests = TestGroup "hydra/lib/lists primitives" Nothing groups []
  where
    groups = [
      applicationTerms]

infTest :: String -> Term -> TypeScheme -> TestCaseWithMetadata
infTest name term ts = TestCaseWithMetadata name (TestCaseInference $ InferenceTestCase term ts) Nothing []

---

applicationTerms :: TestGroup
applicationTerms = TestGroup "application terms" (Just "Check a few hand-picked application terms") [] [
    infTest "check lambda applications"
      ((lambda "x" $ var "x") @@ string "foo")
      (tMono tString),
    infTest "Check mixed expressions with lambdas, constants, and primitive functions"
      (lambda "x" $ primitive _math_sub @@ (primitive _math_add @@ var "x" @@ var "x") @@ int32 1)
      (tMono $ tFun tInt32 tInt32)]
