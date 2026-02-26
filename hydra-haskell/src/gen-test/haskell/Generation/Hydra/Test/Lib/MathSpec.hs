-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.math"},ModuleName {unModuleName = "Math"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lexical"},ModuleName {unModuleName = "Lexical"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"})]

module Generation.Hydra.Test.Lib.MathSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lexical as Lexical
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
  H.describe "max" $ do
    H.it "first is larger" $ H.shouldBe
      (Math.max 10 5)
      (10)
    H.it "second is larger" $ H.shouldBe
      (Math.max 5 10)
      (10)
    H.it "equal values" $ H.shouldBe
      (Math.max 7 7)
      (7)
    H.it "negative numbers" $ H.shouldBe
      (Math.max (-3) (-5))
      ((-3))
    H.it "mixed sign" $ H.shouldBe
      (Math.max (-5) 5)
      (5)
    H.it "with zero" $ H.shouldBe
      (Math.max 0 42)
      (42)
  H.describe "min" $ do
    H.it "first is smaller" $ H.shouldBe
      (Math.min 5 10)
      (5)
    H.it "second is smaller" $ H.shouldBe
      (Math.min 10 5)
      (5)
    H.it "equal values" $ H.shouldBe
      (Math.min 7 7)
      (7)
    H.it "negative numbers" $ H.shouldBe
      (Math.min (-3) (-5))
      ((-5))
    H.it "mixed sign" $ H.shouldBe
      (Math.min (-5) 5)
      ((-5))
    H.it "with zero" $ H.shouldBe
      (Math.min 0 42)
      (0)
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
  H.describe "e" $ do
    H.it "Euler's number" $ H.shouldBe
      (Math.e)
      (2.718281828459045)
  H.describe "pi" $ do
    H.it "pi constant" $ H.shouldBe
      (Math.pi)
      (3.141592653589793)
  H.describe "sin" $ do
    H.it "sin 0" $ H.shouldBe
      (Math.sin 0.0)
      (0.0)
    H.it "sin pi/2" $ H.shouldBe
      (Math.sin 1.5707963267948966)
      (1.0)
    H.it "sin pi" $ H.shouldBe
      (Math.sin 3.141592653589793)
      (1.2246467991473532e-16)
  H.describe "cos" $ do
    H.it "cos 0" $ H.shouldBe
      (Math.cos 0.0)
      (1.0)
    H.it "cos pi/2" $ H.shouldBe
      (Math.cos 1.5707963267948966)
      (6.123233995736766e-17)
    H.it "cos pi" $ H.shouldBe
      (Math.cos 3.141592653589793)
      ((-1.0))
  H.describe "tan" $ do
    H.it "tan 0" $ H.shouldBe
      (Math.tan 0.0)
      (0.0)
    H.it "tan pi/4" $ H.shouldBe
      (Math.tan 0.7853981633974483)
      (0.9999999999999999)
  H.describe "asin" $ do
    H.it "asin 0" $ H.shouldBe
      (Math.asin 0.0)
      (0.0)
    H.it "asin 1" $ H.shouldBe
      (Math.asin 1.0)
      (1.5707963267948966)
    H.it "asin -1" $ H.shouldBe
      (Math.asin (-1.0))
      ((-1.5707963267948966))
  H.describe "acos" $ do
    H.it "acos 1" $ H.shouldBe
      (Math.acos 1.0)
      (0.0)
    H.it "acos 0" $ H.shouldBe
      (Math.acos 0.0)
      (1.5707963267948966)
    H.it "acos -1" $ H.shouldBe
      (Math.acos (-1.0))
      (3.141592653589793)
  H.describe "atan" $ do
    H.it "atan 0" $ H.shouldBe
      (Math.atan 0.0)
      (0.0)
    H.it "atan 1" $ H.shouldBe
      (Math.atan 1.0)
      (0.7853981633974483)
  H.describe "atan2" $ do
    H.it "atan2 1 1" $ H.shouldBe
      (Math.atan2 1.0 1.0)
      (0.7853981633974483)
    H.it "atan2 1 0" $ H.shouldBe
      (Math.atan2 1.0 0.0)
      (1.5707963267948966)
    H.it "atan2 0 1" $ H.shouldBe
      (Math.atan2 0.0 1.0)
      (0.0)
  H.describe "sinh" $ do
    H.it "sinh 0" $ H.shouldBe
      (Math.sinh 0.0)
      (0.0)
    H.it "sinh 1" $ H.shouldBe
      (Math.sinh 1.0)
      (1.1752011936438014)
  H.describe "cosh" $ do
    H.it "cosh 0" $ H.shouldBe
      (Math.cosh 0.0)
      (1.0)
    H.it "cosh 1" $ H.shouldBe
      (Math.cosh 1.0)
      (1.5430806348152437)
  H.describe "tanh" $ do
    H.it "tanh 0" $ H.shouldBe
      (Math.tanh 0.0)
      (0.0)
    H.it "tanh 1" $ H.shouldBe
      (Math.tanh 1.0)
      (0.7615941559557649)
  H.describe "asinh" $ do
    H.it "asinh 0" $ H.shouldBe
      (Math.asinh 0.0)
      (0.0)
    H.it "asinh 1" $ H.shouldBe
      (Math.asinh 1.0)
      (0.881373587019543)
  H.describe "acosh" $ do
    H.it "acosh 1" $ H.shouldBe
      (Math.acosh 1.0)
      (0.0)
    H.it "acosh 2" $ H.shouldBe
      (Math.acosh 2.0)
      (1.3169578969248166)
  H.describe "atanh" $ do
    H.it "atanh 0" $ H.shouldBe
      (Math.atanh 0.0)
      (0.0)
    H.it "atanh 0.5" $ H.shouldBe
      (Math.atanh 0.5)
      (0.5493061443340549)
  H.describe "exp" $ do
    H.it "exp 0" $ H.shouldBe
      (Math.exp 0.0)
      (1.0)
    H.it "exp 1" $ H.shouldBe
      (Math.exp 1.0)
      (2.718281828459045)
    H.it "exp -1" $ H.shouldBe
      (Math.exp (-1.0))
      (0.36787944117144233)
  H.describe "log" $ do
    H.it "log 1" $ H.shouldBe
      (Math.log 1.0)
      (0.0)
    H.it "log e" $ H.shouldBe
      (Math.log 2.718281828459045)
      (1.0)
  H.describe "logBase" $ do
    H.it "log10 1" $ H.shouldBe
      (Math.logBase 10.0 1.0)
      (0.0)
    H.it "log10 10" $ H.shouldBe
      (Math.logBase 10.0 10.0)
      (1.0)
    H.it "log10 100" $ H.shouldBe
      (Math.logBase 10.0 100.0)
      (2.0)
    H.it "log2 8" $ H.shouldBe
      (Math.logBase 2.0 8.0)
      (3.0)
  H.describe "pow" $ do
    H.it "2^3" $ H.shouldBe
      (Math.pow 2.0 3.0)
      (8.0)
    H.it "10^0" $ H.shouldBe
      (Math.pow 10.0 0.0)
      (1.0)
    H.it "2^-1" $ H.shouldBe
      (Math.pow 2.0 (-1.0))
      (0.5)
  H.describe "sqrt" $ do
    H.it "sqrt 4" $ H.shouldBe
      (Math.sqrt 4.0)
      (2.0)
    H.it "sqrt 9" $ H.shouldBe
      (Math.sqrt 9.0)
      (3.0)
    H.it "sqrt 2" $ H.shouldBe
      (Math.sqrt 2.0)
      (1.4142135623730951)
    H.it "sqrt 0" $ H.shouldBe
      (Math.sqrt 0.0)
      (0.0)
  H.describe "ceiling" $ do
    H.it "ceiling 3.2" $ H.shouldBe
      (Math.ceiling 3.2)
      (4)
    H.it "ceiling 3.0" $ H.shouldBe
      (Math.ceiling 3.0)
      (3)
    H.it "ceiling -3.2" $ H.shouldBe
      (Math.ceiling (-3.2))
      ((-3))
    H.it "ceiling -3.0" $ H.shouldBe
      (Math.ceiling (-3.0))
      ((-3))
  H.describe "floor" $ do
    H.it "floor 3.8" $ H.shouldBe
      (Math.floor 3.8)
      (3)
    H.it "floor 3.0" $ H.shouldBe
      (Math.floor 3.0)
      (3)
    H.it "floor -3.2" $ H.shouldBe
      (Math.floor (-3.2))
      ((-4))
    H.it "floor -3.0" $ H.shouldBe
      (Math.floor (-3.0))
      ((-3))
  H.describe "round" $ do
    H.it "round 3.4" $ H.shouldBe
      (Math.round 3.4)
      (3)
    H.it "round 3.5" $ H.shouldBe
      (Math.round 3.5)
      (4)
    H.it "round 3.6" $ H.shouldBe
      (Math.round 3.6)
      (4)
    H.it "round -3.4" $ H.shouldBe
      (Math.round (-3.4))
      ((-3))
    H.it "round -3.5" $ H.shouldBe
      (Math.round (-3.5))
      ((-4))
  H.describe "truncate" $ do
    H.it "truncate 3.8" $ H.shouldBe
      (Math.truncate 3.8)
      (3)
    H.it "truncate 3.2" $ H.shouldBe
      (Math.truncate 3.2)
      (3)
    H.it "truncate -3.8" $ H.shouldBe
      (Math.truncate (-3.8))
      ((-3))
    H.it "truncate -3.2" $ H.shouldBe
      (Math.truncate (-3.2))
      ((-3))
