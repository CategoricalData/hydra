module Hydra.Sources.Test.Lib.Equality where

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


ns :: Namespace
ns = Namespace "hydra.test.lib.equality"

module_ :: Module
module_ = Module ns elements [] [] $
    Just "Test cases for hydra.lib.equality primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Test groups for hydra.lib.equality primitives

equalityCompare :: TTerm TestGroup
equalityCompare = subgroup "compare" [
  test "less than" 3 5 "lessThan",
  test "equal" 5 5 "equalTo",
  test "greater than" 5 3 "greaterThan"]
  where
    test testName x y resultField = primCase testName _equality_compare [int32 x, int32 y] (injectUnit (name "hydra.util.Comparison") resultField)

equalityEqual :: TTerm TestGroup
equalityEqual = subgroup "equal" [
  test "equal integers" 5 5 true,
  test "unequal integers" 5 3 false]
  where
    test name x y result = primCase name _equality_equal [int32 x, int32 y] result

equalityGt :: TTerm TestGroup
equalityGt = subgroup "gt" [
  test "greater" 5 3 true,
  test "equal" 5 5 false,
  test "less" 3 5 false]
  where
    test name x y result = primCase name _equality_gt [int32 x, int32 y] result

equalityGte :: TTerm TestGroup
equalityGte = subgroup "gte" [
  test "greater" 5 3 true,
  test "equal" 5 5 true,
  test "less" 3 5 false]
  where
    test name x y result = primCase name _equality_gte [int32 x, int32 y] result

equalityIdentity :: TTerm TestGroup
equalityIdentity = subgroup "identity" [
  test "integer" 42 42]
  where
    test name x result = primCase name _equality_identity [int32 x] (int32 result)

equalityLt :: TTerm TestGroup
equalityLt = subgroup "lt" [
  test "less" 3 5 true,
  test "equal" 5 5 false,
  test "greater" 5 3 false]
  where
    test name x y result = primCase name _equality_lt [int32 x, int32 y] result

equalityLte :: TTerm TestGroup
equalityLte = subgroup "lte" [
  test "less" 3 5 true,
  test "equal" 5 5 true,
  test "greater" 5 3 false]
  where
    test name x y result = primCase name _equality_lte [int32 x, int32 y] result

equalityMax :: TTerm TestGroup
equalityMax = subgroup "max" [
  test "first greater" 5 3 5,
  test "second greater" 3 5 5,
  test "equal" 5 5 5]
  where
    test name x y result = primCase name _equality_max [int32 x, int32 y] (int32 result)

equalityMin :: TTerm TestGroup
equalityMin = subgroup "min" [
  test "first less" 3 5 3,
  test "second less" 5 3 3,
  test "equal" 5 5 5]
  where
    test name x y result = primCase name _equality_min [int32 x, int32 y] (int32 result)

-- Tests for ordering with string values
equalityCompareStrings :: TTerm TestGroup
equalityCompareStrings = subgroup "compare strings" [
  test "less than (lexicographic)" "apple" "banana" "lessThan",
  test "equal" "hello" "hello" "equalTo",
  test "greater than (lexicographic)" "zebra" "apple" "greaterThan",
  test "empty vs non-empty" "" "a" "lessThan",
  test "prefix vs longer" "ab" "abc" "lessThan"]
  where
    test testName x y resultField = primCase testName _equality_compare [string x, string y] (injectUnit (name "hydra.util.Comparison") resultField)

equalityLtStrings :: TTerm TestGroup
equalityLtStrings = subgroup "lt strings" [
  test "less (lexicographic)" "apple" "banana" true,
  test "equal" "hello" "hello" false,
  test "greater" "zebra" "apple" false]
  where
    test name x y result = primCase name _equality_lt [string x, string y] result

equalityGtStrings :: TTerm TestGroup
equalityGtStrings = subgroup "gt strings" [
  test "greater (lexicographic)" "zebra" "apple" true,
  test "equal" "hello" "hello" false,
  test "less" "apple" "banana" false]
  where
    test name x y result = primCase name _equality_gt [string x, string y] result

equalityMaxStrings :: TTerm TestGroup
equalityMaxStrings = subgroup "max strings" [
  test "first greater" "zebra" "apple" "zebra",
  test "second greater" "apple" "zebra" "zebra",
  test "equal" "hello" "hello" "hello"]
  where
    test name x y result = primCase name _equality_max [string x, string y] (string result)

equalityMinStrings :: TTerm TestGroup
equalityMinStrings = subgroup "min strings" [
  test "first less" "apple" "zebra" "apple",
  test "second less" "zebra" "apple" "apple",
  test "equal" "hello" "hello" "hello"]
  where
    test name x y result = primCase name _equality_min [string x, string y] (string result)

-- Tests for ordering with float values
equalityCompareFloats :: TTerm TestGroup
equalityCompareFloats = subgroup "compare floats" [
  test "less than" 1.5 2.5 "lessThan",
  test "equal" 3.14 3.14 "equalTo",
  test "greater than" 5.0 3.0 "greaterThan",
  test "negative vs positive" (-1.0) 1.0 "lessThan"]
  where
    test testName x y resultField = primCase testName _equality_compare [float64 x, float64 y] (injectUnit (name "hydra.util.Comparison") resultField)

equalityLtFloats :: TTerm TestGroup
equalityLtFloats = subgroup "lt floats" [
  test "less" 1.5 2.5 true,
  test "equal" 3.14 3.14 false,
  test "greater" 5.0 3.0 false]
  where
    test name x y result = primCase name _equality_lt [float64 x, float64 y] result

equalityGtFloats :: TTerm TestGroup
equalityGtFloats = subgroup "gt floats" [
  test "greater" 5.0 3.0 true,
  test "equal" 3.14 3.14 false,
  test "less" 1.5 2.5 false]
  where
    test name x y result = primCase name _equality_gt [float64 x, float64 y] result

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.equality primitives" $
    supergroup "hydra.lib.equality primitives" [
      -- Integer tests
      equalityCompare,
      equalityEqual,
      equalityGt,
      equalityGte,
      equalityIdentity,
      equalityLt,
      equalityLte,
      equalityMax,
      equalityMin,
      -- String tests
      equalityCompareStrings,
      equalityLtStrings,
      equalityGtStrings,
      equalityMaxStrings,
      equalityMinStrings,
      -- Float tests
      equalityCompareFloats,
      equalityLtFloats,
      equalityGtFloats]
