module Hydra.Sources.Test.Lib.Equality where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Equality as DefEquality


ns :: ModuleName
ns = ModuleName "hydra.test.lib.equality"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.show.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing", ModuleName "hydra.util"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.equality primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.equality primitives

allTests :: TypedTermDefinition TestGroup
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

equalityCompare :: TypedTerm TestGroup
equalityCompare = subgroup "compare" [
  test "less than" 3 5 "lessThan",
  test "equal" 5 5 "equalTo",
  test "greater than" 5 3 "greaterThan"]
  where
    test testName x y resultField = primCase testName DefEquality.compare [int32 x, int32 y] (injectUnit (name "hydra.util.Comparison") resultField)

-- Tests for ordering with float values
equalityCompareFloats :: TypedTerm TestGroup
equalityCompareFloats = subgroup "compare floats" [
  test "less than" 1.5 2.5 "lessThan",
  test "equal" 3.14 3.14 "equalTo",
  test "greater than" 5.0 3.0 "greaterThan",
  test "negative vs positive" (-1.0) 1.0 "lessThan"]
  where
    test testName x y resultField = primCase testName DefEquality.compare [float64 x, float64 y] (injectUnit (name "hydra.util.Comparison") resultField)

-- Tests for ordering with string values
equalityCompareStrings :: TypedTerm TestGroup
equalityCompareStrings = subgroup "compare strings" [
  test "less than (lexicographic)" "apple" "banana" "lessThan",
  test "equal" "hello" "hello" "equalTo",
  test "greater than (lexicographic)" "zebra" "apple" "greaterThan",
  test "empty vs non-empty" "" "a" "lessThan",
  test "prefix vs longer" "ab" "abc" "lessThan"]
  where
    test testName x y resultField = primCase testName DefEquality.compare [string x, string y] (injectUnit (name "hydra.util.Comparison") resultField)

equalityEqual :: TypedTerm TestGroup
equalityEqual = subgroup "equal" [
  test "equal integers" 5 5 true,
  test "unequal integers" 5 3 false]
  where
    test name x y result = primCase name DefEquality.equal [int32 x, int32 y] result

equalityGt :: TypedTerm TestGroup
equalityGt = subgroup "gt" [
  test "greater" 5 3 true,
  test "equal" 5 5 false,
  test "less" 3 5 false]
  where
    test name x y result = primCase name DefEquality.gt [int32 x, int32 y] result

equalityGtFloats :: TypedTerm TestGroup
equalityGtFloats = subgroup "gt floats" [
  test "greater" 5.0 3.0 true,
  test "equal" 3.14 3.14 false,
  test "less" 1.5 2.5 false]
  where
    test name x y result = primCase name DefEquality.gt [float64 x, float64 y] result

equalityGtStrings :: TypedTerm TestGroup
equalityGtStrings = subgroup "gt strings" [
  test "greater (lexicographic)" "zebra" "apple" true,
  test "equal" "hello" "hello" false,
  test "less" "apple" "banana" false]
  where
    test name x y result = primCase name DefEquality.gt [string x, string y] result

equalityGte :: TypedTerm TestGroup
equalityGte = subgroup "gte" [
  test "greater" 5 3 true,
  test "equal" 5 5 true,
  test "less" 3 5 false]
  where
    test name x y result = primCase name DefEquality.gte [int32 x, int32 y] result

equalityIdentity :: TypedTerm TestGroup
equalityIdentity = subgroup "identity" [
  test "integer" 42 42]
  where
    test name x result = primCase name DefEquality.identity [int32 x] (int32 result)

equalityLt :: TypedTerm TestGroup
equalityLt = subgroup "lt" [
  test "less" 3 5 true,
  test "equal" 5 5 false,
  test "greater" 5 3 false]
  where
    test name x y result = primCase name DefEquality.lt [int32 x, int32 y] result

equalityLtFloats :: TypedTerm TestGroup
equalityLtFloats = subgroup "lt floats" [
  test "less" 1.5 2.5 true,
  test "equal" 3.14 3.14 false,
  test "greater" 5.0 3.0 false]
  where
    test name x y result = primCase name DefEquality.lt [float64 x, float64 y] result

equalityLtStrings :: TypedTerm TestGroup
equalityLtStrings = subgroup "lt strings" [
  test "less (lexicographic)" "apple" "banana" true,
  test "equal" "hello" "hello" false,
  test "greater" "zebra" "apple" false]
  where
    test name x y result = primCase name DefEquality.lt [string x, string y] result

equalityLte :: TypedTerm TestGroup
equalityLte = subgroup "lte" [
  test "less" 3 5 true,
  test "equal" 5 5 true,
  test "greater" 5 3 false]
  where
    test name x y result = primCase name DefEquality.lte [int32 x, int32 y] result

equalityMax :: TypedTerm TestGroup
equalityMax = subgroup "max" [
  test "first greater" 5 3 5,
  test "second greater" 3 5 5,
  test "equal" 5 5 5]
  where
    test name x y result = primCase name DefEquality.max [int32 x, int32 y] (int32 result)

equalityMaxStrings :: TypedTerm TestGroup
equalityMaxStrings = subgroup "max strings" [
  test "first greater" "zebra" "apple" "zebra",
  test "second greater" "apple" "zebra" "zebra",
  test "equal" "hello" "hello" "hello"]
  where
    test name x y result = primCase name DefEquality.max [string x, string y] (string result)

equalityMin :: TypedTerm TestGroup
equalityMin = subgroup "min" [
  test "first less" 3 5 3,
  test "second less" 5 3 3,
  test "equal" 5 5 5]
  where
    test name x y result = primCase name DefEquality.min [int32 x, int32 y] (int32 result)

equalityMinStrings :: TypedTerm TestGroup
equalityMinStrings = subgroup "min strings" [
  test "first less" "apple" "zebra" "apple",
  test "second less" "zebra" "apple" "apple",
  test "equal" "hello" "hello" "hello"]
  where
    test name x y result = primCase name DefEquality.min [string x, string y] (string result)
