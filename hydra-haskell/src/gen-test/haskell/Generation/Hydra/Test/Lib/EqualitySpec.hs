-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.equality"},ModuleName {unModuleName = "Equality"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.equality"},ModuleName {unModuleName = "Equality"})]

module Generation.Hydra.Test.Lib.EqualitySpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Hydra.Lib.Equality as Equality

spec :: H.Spec
spec = H.describe "hydra.lib.equality primitives" $ do
  H.describe "equal" $ do
    H.it "equal integers" $ H.shouldBe
      (Equality.equal 5 5)
      (True)
    H.it "unequal integers" $ H.shouldBe
      (Equality.equal 5 3)
      (False)
  H.describe "gt" $ do
    H.it "greater" $ H.shouldBe
      (Equality.gt 5 3)
      (True)
    H.it "equal" $ H.shouldBe
      (Equality.gt 5 5)
      (False)
    H.it "less" $ H.shouldBe
      (Equality.gt 3 5)
      (False)
  H.describe "gte" $ do
    H.it "greater" $ H.shouldBe
      (Equality.gte 5 3)
      (True)
    H.it "equal" $ H.shouldBe
      (Equality.gte 5 5)
      (True)
    H.it "less" $ H.shouldBe
      (Equality.gte 3 5)
      (False)
  H.describe "identity" $ do
    H.it "integer" $ H.shouldBe
      (Equality.identity 42)
      (42)
  H.describe "lt" $ do
    H.it "less" $ H.shouldBe
      (Equality.lt 3 5)
      (True)
    H.it "equal" $ H.shouldBe
      (Equality.lt 5 5)
      (False)
    H.it "greater" $ H.shouldBe
      (Equality.lt 5 3)
      (False)
  H.describe "lte" $ do
    H.it "less" $ H.shouldBe
      (Equality.lte 3 5)
      (True)
    H.it "equal" $ H.shouldBe
      (Equality.lte 5 5)
      (True)
    H.it "greater" $ H.shouldBe
      (Equality.lte 5 3)
      (False)
  H.describe "max" $ do
    H.it "first greater" $ H.shouldBe
      (Equality.max 5 3)
      (5)
    H.it "second greater" $ H.shouldBe
      (Equality.max 3 5)
      (5)
    H.it "equal" $ H.shouldBe
      (Equality.max 5 5)
      (5)
  H.describe "min" $ do
    H.it "first less" $ H.shouldBe
      (Equality.min 3 5)
      (3)
    H.it "second less" $ H.shouldBe
      (Equality.min 5 3)
      (3)
    H.it "equal" $ H.shouldBe
      (Equality.min 5 5)
      (5)
