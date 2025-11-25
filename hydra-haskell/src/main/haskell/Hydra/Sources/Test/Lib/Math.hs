module Hydra.Sources.Test.Lib.Math where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes
import qualified Hydra.Sources.Test.TestGraph as TestGraph


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.math") elements [] [] $
    Just "Test cases for hydra.lib.math primitives"
  where
    elements = [el allTestsDef]

-- Test groups for hydra.lib.math primitives

mathAbs :: TTerm TestGroup
mathAbs = subgroup "abs" [
  test "positive" 5 5,
  test "negative" (-5) 5,
  test "zero" 0 0]
  where
    test name x result = primCase name _math_abs [int32 x] (int32 result)

mathAdd :: TTerm TestGroup
mathAdd = subgroup "add" [
  test "positive numbers" 3 5 8,
  test "negative numbers" (-3) (-5) (-8),
  test "mixed sign" 10 (-3) 7,
  test "with zero" 42 0 42]
  where
    test name x y result = primCase name _math_add [int32 x, int32 y] (int32 result)

mathDiv :: TTerm TestGroup
mathDiv = subgroup "div" [
  test "exact division" 10 2 5,
  test "truncates toward negative infinity" 10 3 3,
  test "negative dividend" (-10) 3 (-4),
  test "negative divisor" 10 (-3) (-4)]
  where
    test name x y result = primCase name _math_div [int32 x, int32 y] (int32 result)

mathEven :: TTerm TestGroup
mathEven = subgroup "even" [
  test "even positive" 4 true,
  test "odd positive" 5 false,
  test "even negative" (-4) true,
  test "odd negative" (-5) false,
  test "zero" 0 true]
  where
    test name x result = primCase name _math_even [int32 x] result

mathMod :: TTerm TestGroup
mathMod = subgroup "mod" [
  test "basic modulo" 10 3 1,
  test "exact division" 10 2 0,
  test "negative dividend" (-10) 3 2,
  test "negative divisor" 10 (-3) (-2)]
  where
    test name x y result = primCase name _math_mod [int32 x, int32 y] (int32 result)

mathMul :: TTerm TestGroup
mathMul = subgroup "mul" [
  test "positive numbers" 3 5 15,
  test "negative numbers" (-3) (-5) 15,
  test "mixed sign" 3 (-5) (-15),
  test "with zero" 42 0 0,
  test "with one" 42 1 42]
  where
    test name x y result = primCase name _math_mul [int32 x, int32 y] (int32 result)

mathNegate :: TTerm TestGroup
mathNegate = subgroup "negate" [
  test "positive" 5 (-5),
  test "negative" (-5) 5,
  test "zero" 0 0]
  where
    test name x result = primCase name _math_negate [int32 x] (int32 result)

mathOdd :: TTerm TestGroup
mathOdd = subgroup "odd" [
  test "odd positive" 5 true,
  test "even positive" 4 false,
  test "odd negative" (-5) true,
  test "even negative" (-4) false,
  test "zero" 0 false]
  where
    test name x result = primCase name _math_odd [int32 x] result

mathPred :: TTerm TestGroup
mathPred = subgroup "pred" [
  test "positive" 5 4,
  test "zero" 0 (-1),
  test "negative" (-5) (-6)]
  where
    test name x result = primCase name _math_pred [int32 x] (int32 result)

mathRange :: TTerm TestGroup
mathRange = subgroup "range" [
  test "ascending range" 1 5 [1, 2, 3, 4, 5],
  test "single element" 5 5 [5],
  test "two elements" 3 4 [3, 4],
  test "negative start" (-2) 2 [(-2), (-1), 0, 1, 2]]
  where
    test name start end result = primCase name _math_range [int32 start, int32 end] (list $ int32 <$> result)

mathRem :: TTerm TestGroup
mathRem = subgroup "rem" [
  test "basic remainder" 10 3 1,
  test "exact division" 10 2 0,
  test "negative dividend" (-10) 3 (-1),
  test "negative divisor" 10 (-3) 1]
  where
    test name x y result = primCase name _math_rem [int32 x, int32 y] (int32 result)

mathSignum :: TTerm TestGroup
mathSignum = subgroup "signum" [
  test "positive" 5 1,
  test "negative" (-5) (-1),
  test "zero" 0 0]
  where
    test name x result = primCase name _math_signum [int32 x] (int32 result)

mathSub :: TTerm TestGroup
mathSub = subgroup "sub" [
  test "positive numbers" 10 3 7,
  test "negative numbers" (-10) (-3) (-7),
  test "mixed sign" 10 (-3) 13,
  test "with zero" 42 0 42]
  where
    test name x y result = primCase name _math_sub [int32 x, int32 y] (int32 result)

mathSucc :: TTerm TestGroup
mathSucc = subgroup "succ" [
  test "positive" 5 6,
  test "zero" 0 1,
  test "negative" (-5) (-4)]
  where
    test name x result = primCase name _math_succ [int32 x] (int32 result)

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.math primitives" $
    supergroup "hydra.lib.math primitives" [
      mathAbs,
      mathAdd,
      mathDiv,
      mathEven,
      mathMod,
      mathMul,
      mathNegate,
      mathOdd,
      mathPred,
      mathRange,
      mathRem,
      mathSignum,
      mathSub,
      mathSucc]
