-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.literals"},ModuleName {unModuleName = "Literals"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.literals"},ModuleName {unModuleName = "Literals"})]

module Generation.Hydra.Test.Lib.LiteralsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Literals as Literals

spec :: H.Spec
spec = H.describe "hydra.lib.literals primitives" $ do
  H.describe "bigintToInt32" $ do
    H.it "positive" $ H.shouldBe
      (Literals.bigintToInt32 42)
      (42)
    H.it "negative" $ H.shouldBe
      (Literals.bigintToInt32 (-42))
      ((-42))
    H.it "zero" $ H.shouldBe
      (Literals.bigintToInt32 0)
      (0)
  H.describe "int32ToBigint" $ do
    H.it "positive" $ H.shouldBe
      (Literals.int32ToBigint 42)
      (42)
    H.it "negative" $ H.shouldBe
      (Literals.int32ToBigint (-42))
      ((-42))
    H.it "zero" $ H.shouldBe
      (Literals.int32ToBigint 0)
      (0)
