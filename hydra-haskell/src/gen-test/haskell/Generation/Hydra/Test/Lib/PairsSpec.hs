-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.pairs"},ModuleName {unModuleName = "Pairs"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.lib.pairs"},ModuleName {unModuleName = "Pairs"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.PairsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.pairs primitives" $ do
  H.describe "bimap" $ do
    H.it "transform both elements" $ H.shouldBe
      (Pairs.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (5, "ab"))
      ((10, 2))
    H.it "with zero" $ H.shouldBe
      (Pairs.bimap (\x -> Math.mul x 2) (\s -> Strings.length s) (0, "hello"))
      ((0, 5))
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
