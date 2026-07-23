module Hydra.Sources.Test.Lib.Ordering where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Overlay.Haskell.Dsl.Typed.Testing                 as Testing
import Hydra.Overlay.Haskell.Dsl.Typed.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Core          as Core
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Phantoms      as Phantoms
import qualified Hydra.Overlay.Haskell.Dsl.Typed.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Testing
import qualified Hydra.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Lib.Ordering as DefOrdering


ns :: ModuleName
ns = ModuleName "hydra.test.lib.ordering"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.print.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing", ModuleName "hydra.util"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.ordering primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.ordering primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.ordering primitives" $
    supergroup "hydra.lib.ordering primitives" [
      -- Integer tests
      orderingCompare,
      orderingGt,
      orderingGte,
      orderingLt,
      orderingLte,
      orderingMax,
      orderingMin,
      -- String tests
      orderingCompareStrings,
      orderingLtStrings,
      orderingGtStrings,
      orderingMaxStrings,
      orderingMinStrings,
      -- Float tests
      orderingCompareFloats,
      orderingLtFloats,
      orderingGtFloats]

orderingCompare :: TypedTerm TestGroup
orderingCompare = subgroup "compare" [
  test "less than" 3 5 "lessThan",
  test "equal" 5 5 "equalTo",
  test "greater than" 5 3 "greaterThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [int32 x, int32 y] (injectUnit (name "hydra.util.Comparison") resultField)

-- Tests for ordering with float values
orderingCompareFloats :: TypedTerm TestGroup
orderingCompareFloats = subgroup "compare floats" [
  test "less than" 1.5 2.5 "lessThan",
  test "equal" 3.14 3.14 "equalTo",
  test "greater than" 5.0 3.0 "greaterThan",
  test "negative vs positive" (-1.0) 1.0 "lessThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [float64 x, float64 y] (injectUnit (name "hydra.util.Comparison") resultField)

-- Tests for ordering with string values
orderingCompareStrings :: TypedTerm TestGroup
orderingCompareStrings = subgroup "compare strings" [
  test "less than (lexicographic)" "apple" "banana" "lessThan",
  test "equal" "hello" "hello" "equalTo",
  test "greater than (lexicographic)" "zebra" "apple" "greaterThan",
  test "empty vs non-empty" "" "a" "lessThan",
  test "prefix vs longer" "ab" "abc" "lessThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [string x, string y] (injectUnit (name "hydra.util.Comparison") resultField)

orderingGt :: TypedTerm TestGroup
orderingGt = subgroup "gt" [
  test "greater" 5 3 true,
  test "equal" 5 5 false,
  test "less" 3 5 false]
  where
    test name x y result = primCase name DefOrdering.gt [int32 x, int32 y] result

orderingGtFloats :: TypedTerm TestGroup
orderingGtFloats = subgroup "gt floats" [
  test "greater" 5.0 3.0 true,
  test "equal" 3.14 3.14 false,
  test "less" 1.5 2.5 false]
  where
    test name x y result = primCase name DefOrdering.gt [float64 x, float64 y] result

orderingGtStrings :: TypedTerm TestGroup
orderingGtStrings = subgroup "gt strings" [
  test "greater (lexicographic)" "zebra" "apple" true,
  test "equal" "hello" "hello" false,
  test "less" "apple" "banana" false]
  where
    test name x y result = primCase name DefOrdering.gt [string x, string y] result

orderingGte :: TypedTerm TestGroup
orderingGte = subgroup "gte" [
  test "greater" 5 3 true,
  test "equal" 5 5 true,
  test "less" 3 5 false]
  where
    test name x y result = primCase name DefOrdering.gte [int32 x, int32 y] result

orderingLt :: TypedTerm TestGroup
orderingLt = subgroup "lt" [
  test "less" 3 5 true,
  test "equal" 5 5 false,
  test "greater" 5 3 false]
  where
    test name x y result = primCase name DefOrdering.lt [int32 x, int32 y] result

orderingLtFloats :: TypedTerm TestGroup
orderingLtFloats = subgroup "lt floats" [
  test "less" 1.5 2.5 true,
  test "equal" 3.14 3.14 false,
  test "greater" 5.0 3.0 false]
  where
    test name x y result = primCase name DefOrdering.lt [float64 x, float64 y] result

orderingLtStrings :: TypedTerm TestGroup
orderingLtStrings = subgroup "lt strings" [
  test "less (lexicographic)" "apple" "banana" true,
  test "equal" "hello" "hello" false,
  test "greater" "zebra" "apple" false]
  where
    test name x y result = primCase name DefOrdering.lt [string x, string y] result

orderingLte :: TypedTerm TestGroup
orderingLte = subgroup "lte" [
  test "less" 3 5 true,
  test "equal" 5 5 true,
  test "greater" 5 3 false]
  where
    test name x y result = primCase name DefOrdering.lte [int32 x, int32 y] result

orderingMax :: TypedTerm TestGroup
orderingMax = subgroup "max" [
  test "first greater" 5 3 5,
  test "second greater" 3 5 5,
  test "equal" 5 5 5]
  where
    test name x y result = primCase name DefOrdering.max [int32 x, int32 y] (int32 result)

orderingMaxStrings :: TypedTerm TestGroup
orderingMaxStrings = subgroup "max strings" [
  test "first greater" "zebra" "apple" "zebra",
  test "second greater" "apple" "zebra" "zebra",
  test "equal" "hello" "hello" "hello"]
  where
    test name x y result = primCase name DefOrdering.max [string x, string y] (string result)

orderingMin :: TypedTerm TestGroup
orderingMin = subgroup "min" [
  test "first less" 3 5 3,
  test "second less" 5 3 3,
  test "equal" 5 5 5]
  where
    test name x y result = primCase name DefOrdering.min [int32 x, int32 y] (int32 result)

orderingMinStrings :: TypedTerm TestGroup
orderingMinStrings = subgroup "min strings" [
  test "first less" "apple" "zebra" "apple",
  test "second less" "zebra" "apple" "apple",
  test "equal" "hello" "hello" "hello"]
  where
    test name x y result = primCase name DefOrdering.min [string x, string y] (string result)
