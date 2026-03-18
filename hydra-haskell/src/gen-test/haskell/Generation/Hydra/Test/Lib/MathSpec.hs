-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (see generated module)
-- DEBUG: Namespace mappings: (see generated module)

module Generation.Hydra.Test.Lib.MathSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
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
    H.it "sin 1" $ H.shouldBe
      (Math.roundFloat64 12 (Math.sin 1.0))
      (0.841470984808)
    H.it "sin 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.sin 0.5))
      (0.479425538604)
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
    H.it "cos 1" $ H.shouldBe
      (Math.roundFloat64 12 (Math.cos 1.0))
      (0.540302305868)
    H.it "cos 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.cos 0.5))
      (0.87758256189)
  H.describe "tan" $ do
    H.it "tan 0" $ H.shouldBe
      (Math.tan 0.0)
      (0.0)
    H.it "tan pi/4" $ H.shouldBe
      (Math.tan 0.7853981633974483)
      (0.9999999999999999)
    H.it "tan 1" $ H.shouldBe
      (Math.roundFloat64 12 (Math.tan 1.0))
      (1.55740772465)
    H.it "tan 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.tan 0.5))
      (0.546302489844)
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
    H.it "asin 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.asin 0.5))
      (0.523598775598)
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
    H.it "acos 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.acos 0.5))
      (1.0471975512)
  H.describe "atan" $ do
    H.it "atan 0" $ H.shouldBe
      (Math.atan 0.0)
      (0.0)
    H.it "atan 1" $ H.shouldBe
      (Math.atan 1.0)
      (0.7853981633974483)
    H.it "atan 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.atan 0.5))
      (0.463647609001)
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
    H.it "atan2 3 4" $ H.shouldBe
      (Math.roundFloat64 12 (Math.atan2 3.0 4.0))
      (0.643501108793)
  H.describe "sinh" $ do
    H.it "sinh 0" $ H.shouldBe
      (Math.sinh 0.0)
      (0.0)
    H.it "sinh 1" $ H.shouldBe
      (Math.sinh 1.0)
      (1.1752011936438014)
    H.it "sinh 2" $ H.shouldBe
      (Math.roundFloat64 12 (Math.sinh 2.0))
      (3.62686040785)
  H.describe "cosh" $ do
    H.it "cosh 0" $ H.shouldBe
      (Math.cosh 0.0)
      (1.0)
    H.it "cosh 1" $ H.shouldBe
      (Math.cosh 1.0)
      (1.5430806348152437)
    H.it "cosh 2" $ H.shouldBe
      (Math.roundFloat64 12 (Math.cosh 2.0))
      (3.76219569108)
  H.describe "tanh" $ do
    H.it "tanh 0" $ H.shouldBe
      (Math.tanh 0.0)
      (0.0)
    H.it "tanh 1" $ H.shouldBe
      (Math.tanh 1.0)
      (0.7615941559557649)
    H.it "tanh 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.tanh 0.5))
      (0.46211715726)
  H.describe "asinh" $ do
    H.it "asinh 0" $ H.shouldBe
      (Math.asinh 0.0)
      (0.0)
    H.it "asinh 1" $ H.shouldBe
      (Math.roundFloat64 12 (Math.asinh 1.0))
      (0.88137358702)
    H.it "asinh 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.asinh 0.5))
      (0.48121182506)
  H.describe "acosh" $ do
    H.it "acosh 1" $ H.shouldBe
      (Math.acosh 1.0)
      (0.0)
    H.it "acosh 2" $ H.shouldBe
      (Math.roundFloat64 12 (Math.acosh 2.0))
      (1.31695789692)
    H.it "acosh 3" $ H.shouldBe
      (Math.roundFloat64 12 (Math.acosh 3.0))
      (1.76274717404)
  H.describe "atanh" $ do
    H.it "atanh 0" $ H.shouldBe
      (Math.atanh 0.0)
      (0.0)
    H.it "atanh 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.atanh 0.5))
      (0.549306144334)
    H.it "atanh 0.1" $ H.shouldBe
      (Math.roundFloat64 12 (Math.atanh 0.1))
      (0.100335347731)
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
    H.it "exp 2" $ H.shouldBe
      (Math.roundFloat64 12 (Math.exp 2.0))
      (7.38905609893)
    H.it "exp 0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.exp 0.5))
      (1.6487212707)
  H.describe "log" $ do
    H.it "log 1" $ H.shouldBe
      (Math.log 1.0)
      (0.0)
    H.it "log e" $ H.shouldBe
      (Math.log 2.718281828459045)
      (1.0)
    H.it "log 2" $ H.shouldBe
      (Math.roundFloat64 12 (Math.log 2.0))
      (0.69314718056)
    H.it "log 10" $ H.shouldBe
      (Math.roundFloat64 12 (Math.log 10.0))
      (2.30258509299)
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
    H.it "log2 10" $ H.shouldBe
      (Math.roundFloat64 12 (Math.logBase 2.0 10.0))
      (3.32192809489)
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
    H.it "2^0.5" $ H.shouldBe
      (Math.roundFloat64 12 (Math.pow 2.0 0.5))
      (1.41421356237)
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
    H.it "sqrt 3" $ H.shouldBe
      (Math.roundFloat64 12 (Math.sqrt 3.0))
      (1.73205080757)
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
  H.describe "roundBigfloat" $ do
    H.it "zero" $ H.shouldBe
      (Math.roundBigfloat 5 0.0)
      (0.0)
    H.it "round pi to 4 digits" $ H.shouldBe
      (Math.roundBigfloat 4 3.141592653589793)
      (3.142)
    H.it "round 1234.5 to 3 digits" $ H.shouldBe
      (Math.roundBigfloat 3 1234.5)
      (1230.0)
    H.it "round 0.001234 to 2 digits" $ H.shouldBe
      (Math.roundBigfloat 2 1.234e-3)
      (1.2e-3)
    H.it "negative" $ H.shouldBe
      (Math.roundBigfloat 3 (-1234.5))
      ((-1230.0))
  H.describe "roundFloat32" $ do
    H.it "zero" $ H.shouldBe
      (Math.roundFloat32 5 0.0)
      (0.0)
    H.it "round pi to 4 digits" $ H.shouldBe
      (Math.roundFloat32 4 3.1415927)
      (3.142)
    H.it "round 1234.5 to 3 digits" $ H.shouldBe
      (Math.roundFloat32 3 1234.5)
      (1230.0)
    H.it "negative" $ H.shouldBe
      (Math.roundFloat32 3 (-1234.5))
      ((-1230.0))
  H.describe "roundFloat64" $ do
    H.it "zero" $ H.shouldBe
      (Math.roundFloat64 5 0.0)
      (0.0)
    H.it "round pi to 4 digits" $ H.shouldBe
      (Math.roundFloat64 4 3.141592653589793)
      (3.142)
    H.it "round pi to 10 digits" $ H.shouldBe
      (Math.roundFloat64 10 3.141592653589793)
      (3.141592654)
    H.it "round 1234.5 to 3 digits" $ H.shouldBe
      (Math.roundFloat64 3 1234.5)
      (1230.0)
    H.it "round 0.001234 to 2 digits" $ H.shouldBe
      (Math.roundFloat64 2 1.234e-3)
      (1.2e-3)
    H.it "negative" $ H.shouldBe
      (Math.roundFloat64 3 (-1234.5))
      ((-1230.0))
    H.it "round 1 digit" $ H.shouldBe
      (Math.roundFloat64 1 9.876)
      (10.0)
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
