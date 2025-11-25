-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.math"},ModuleName {unModuleName = "Math"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"})]

module Generation.Hydra.Test.Lib.MathSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Hydra.Lib.Math as Math

spec :: H.Spec
spec = H.describe "hydra.lib.math primitives" $ do
  H.describe "abs" $ do
    H.it "positive" $ H.shouldBe
      (Math.abs 5)
      (5)
    H.it "negative" $ H.shouldBe
      (Math.abs (-5))
      (5)
    H.it "zero" $ H.shouldBe
      (Math.abs 0)
      (0)
  H.describe "add" $ do
    H.it "positive numbers" $ H.shouldBe
      (Math.add 3 5)
      (8)
    H.it "negative numbers" $ H.shouldBe
      (Math.add (-3) (-5))
      ((-8))
    H.it "mixed sign" $ H.shouldBe
      (Math.add 10 (-3))
      (7)
    H.it "with zero" $ H.shouldBe
      (Math.add 42 0)
      (42)
  H.describe "div" $ do
    H.it "exact division" $ H.shouldBe
      (Math.div 10 2)
      (5)
    H.it "truncates toward negative infinity" $ H.shouldBe
      (Math.div 10 3)
      (3)
    H.it "negative dividend" $ H.shouldBe
      (Math.div (-10) 3)
      ((-4))
    H.it "negative divisor" $ H.shouldBe
      (Math.div 10 (-3))
      ((-4))
  H.describe "even" $ do
    H.it "even positive" $ H.shouldBe
      (Math.even 4)
      (True)
    H.it "odd positive" $ H.shouldBe
      (Math.even 5)
      (False)
    H.it "even negative" $ H.shouldBe
      (Math.even (-4))
      (True)
    H.it "odd negative" $ H.shouldBe
      (Math.even (-5))
      (False)
    H.it "zero" $ H.shouldBe
      (Math.even 0)
      (True)
  H.describe "mod" $ do
    H.it "basic modulo" $ H.shouldBe
      (Math.mod 10 3)
      (1)
    H.it "exact division" $ H.shouldBe
      (Math.mod 10 2)
      (0)
    H.it "negative dividend" $ H.shouldBe
      (Math.mod (-10) 3)
      (2)
    H.it "negative divisor" $ H.shouldBe
      (Math.mod 10 (-3))
      ((-2))
  H.describe "mul" $ do
    H.it "positive numbers" $ H.shouldBe
      (Math.mul 3 5)
      (15)
    H.it "negative numbers" $ H.shouldBe
      (Math.mul (-3) (-5))
      (15)
    H.it "mixed sign" $ H.shouldBe
      (Math.mul 3 (-5))
      ((-15))
    H.it "with zero" $ H.shouldBe
      (Math.mul 42 0)
      (0)
    H.it "with one" $ H.shouldBe
      (Math.mul 42 1)
      (42)
  H.describe "negate" $ do
    H.it "positive" $ H.shouldBe
      (Math.negate 5)
      ((-5))
    H.it "negative" $ H.shouldBe
      (Math.negate (-5))
      (5)
    H.it "zero" $ H.shouldBe
      (Math.negate 0)
      (0)
  H.describe "odd" $ do
    H.it "odd positive" $ H.shouldBe
      (Math.odd 5)
      (True)
    H.it "even positive" $ H.shouldBe
      (Math.odd 4)
      (False)
    H.it "odd negative" $ H.shouldBe
      (Math.odd (-5))
      (True)
    H.it "even negative" $ H.shouldBe
      (Math.odd (-4))
      (False)
    H.it "zero" $ H.shouldBe
      (Math.odd 0)
      (False)
  H.describe "pred" $ do
    H.it "positive" $ H.shouldBe
      (Math.pred 5)
      (4)
    H.it "zero" $ H.shouldBe
      (Math.pred 0)
      ((-1))
    H.it "negative" $ H.shouldBe
      (Math.pred (-5))
      ((-6))
  H.describe "range" $ do
    H.it "ascending range" $ H.shouldBe
      (Math.range 1 5)
      ([
          1,
          2,
          3,
          4,
          5])
    H.it "single element" $ H.shouldBe
      (Math.range 5 5)
      ([
          5])
    H.it "two elements" $ H.shouldBe
      (Math.range 3 4)
      ([
          3,
          4])
    H.it "negative start" $ H.shouldBe
      (Math.range (-2) 2)
      ([
          (-2),
          (-1),
          0,
          1,
          2])
  H.describe "rem" $ do
    H.it "basic remainder" $ H.shouldBe
      (Math.rem 10 3)
      (1)
    H.it "exact division" $ H.shouldBe
      (Math.rem 10 2)
      (0)
    H.it "negative dividend" $ H.shouldBe
      (Math.rem (-10) 3)
      ((-1))
    H.it "negative divisor" $ H.shouldBe
      (Math.rem 10 (-3))
      (1)
  H.describe "signum" $ do
    H.it "positive" $ H.shouldBe
      (Math.signum 5)
      (1)
    H.it "negative" $ H.shouldBe
      (Math.signum (-5))
      ((-1))
    H.it "zero" $ H.shouldBe
      (Math.signum 0)
      (0)
  H.describe "sub" $ do
    H.it "positive numbers" $ H.shouldBe
      (Math.sub 10 3)
      (7)
    H.it "negative numbers" $ H.shouldBe
      (Math.sub (-10) (-3))
      ((-7))
    H.it "mixed sign" $ H.shouldBe
      (Math.sub 10 (-3))
      (13)
    H.it "with zero" $ H.shouldBe
      (Math.sub 42 0)
      (42)
  H.describe "succ" $ do
    H.it "positive" $ H.shouldBe
      (Math.succ 5)
      (6)
    H.it "zero" $ H.shouldBe
      (Math.succ 0)
      (1)
    H.it "negative" $ H.shouldBe
      (Math.succ (-5))
      ((-4))
