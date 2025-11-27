-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.pairs"},ModuleName {unModuleName = "Pairs"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.pairs"},ModuleName {unModuleName = "Pairs"})]

module Generation.Hydra.Test.Lib.PairsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Pairs as Pairs

spec :: H.Spec
spec = H.describe "hydra.lib.pairs primitives" $ do
  H.describe "first" $ do
    H.it "extract first element" $ H.shouldBe
      (Pairs.first (42, "hello"))
      (42)
    H.it "with zero" $ H.shouldBe
      (Pairs.first (0, "world"))
      (0)
    H.it "negative number" $ H.shouldBe
      (Pairs.first ((-5), "test"))
      ((-5))
  H.describe "second" $ do
    H.it "extract second element" $ H.shouldBe
      (Pairs.second (42, "hello"))
      ("hello")
    H.it "empty string" $ H.shouldBe
      (Pairs.second (0, ""))
      ("")
    H.it "long string" $ H.shouldBe
      (Pairs.second (123, "testing"))
      ("testing")
