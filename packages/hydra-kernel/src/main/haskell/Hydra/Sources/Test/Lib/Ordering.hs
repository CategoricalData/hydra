module Hydra.Sources.Test.Lib.Ordering where

-- Standard imports for term-encoded tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Data.List                    as L
import qualified Data.Map                     as M

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Hydra.Core.Overlay.Haskell.Dsl.Prims as Prims
import qualified Hydra.Core.Lib.Ordering as DefOrdering
import qualified Data.Scientific as Sci


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.ordering"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.core.reduction", ModuleName "hydra.core.print.model", ModuleName "hydra.core.model", ModuleName "hydra.core.errors", ModuleName "hydra.core.test.testGraph", ModuleName "hydra.core.testing", ModuleName "hydra.core.util"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.ordering primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.core.lib.ordering primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.ordering primitives" $
    supergroup "hydra.core.lib.ordering primitives" [
      supergroup "compare" [
        orderingCompare, orderingCompareFloats, orderingCompareDecimals,
        orderingCompareStrings, orderingCompareLists, orderingCompareSets, orderingCompareMaps],
      supergroup "gt" [orderingGt, orderingGtFloats, orderingGtStrings],
      supergroup "gte" [orderingGte, orderingGteFloats, orderingGteStrings, orderingGteDecimals],
      supergroup "lt" [orderingLt, orderingLtFloats, orderingLtStrings],
      supergroup "lte" [orderingLte, orderingLteFloats, orderingLteStrings, orderingLteDecimals],
      supergroup "max" [orderingMax, orderingMaxStrings, orderingMaxFloats, orderingMaxDecimals],
      supergroup "min" [orderingMin, orderingMinStrings, orderingMinFloats, orderingMinDecimals]]

orderingCompare :: TypedTerm TestGroup
orderingCompare = subgroup "threeWayClassification" [
  test "less than (int32)" 3 5 "lessThan",
  test "equal (int32)" 5 5 "equalTo",
  test "greater than (int32)" 5 3 "greaterThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["threeWayClassification"] DefOrdering.compare [int32 x, int32 y] (injectUnit (name "hydra.core.util.Comparison") resultField)

-- Tests for ordering with float values
orderingCompareFloats :: TypedTerm TestGroup
orderingCompareFloats = subgroup "threeWayClassification" [
  test "less than (float64)" 1.5 2.5 "lessThan",
  test "equal (float64)" 3.14 3.14 "equalTo",
  test "greater than (float64)" 5.0 3.0 "greaterThan",
  test "negative vs positive" (-1.0) 1.0 "lessThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["threeWayClassification"] DefOrdering.compare [float64 x, float64 y] (injectUnit (name "hydra.core.util.Comparison") resultField)

-- Tests for ordering with decimal values: value first, then scale as tiebreak
-- (numerically equal decimals of different scale are distinct, smaller scale first).
orderingCompareDecimals :: TypedTerm TestGroup
orderingCompareDecimals = subgroup "decimalScaleTiebreak" [
  test "different value" (decimalOf 11 1) (decimalOf 12 1) "lessThan",
  test "same value, same scale" (decimalOf 11 1) (decimalOf 11 1) "equalTo",
  scaleDistinctTest "same value, scale tiebreak" (decimalOf 11 1) (decimalOf 110 2) "lessThan",
  scaleDistinctTest "same value, scale tiebreak (larger scale)" (decimalOf 110 2) (decimalOf 1100 3) "lessThan",
  scaleDistinctTest "same value, scale tiebreak (transitively)" (decimalOf 11 1) (decimalOf 1100 3) "lessThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["decimalScaleTiebreak"] DefOrdering.compare [x, y] (injectUnit (name "hydra.core.util.Comparison") resultField)
    scaleDistinctTest testName x y resultField = primCaseWithTagsAndProvisions testName [tag_scaleDistinct] ["decimalScaleTiebreak"] DefOrdering.compare [x, y] (injectUnit (name "hydra.core.util.Comparison") resultField)
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

-- Tests for ordering with string values
orderingCompareStrings :: TypedTerm TestGroup
orderingCompareStrings = subgroup "threeWayClassification" [
  test "less than (lexicographic)" "apple" "banana" "lessThan",
  test "equal (string)" "hello" "hello" "equalTo",
  test "greater than (lexicographic)" "zebra" "apple" "greaterThan",
  test "empty vs non-empty" "" "a" "lessThan",
  test "prefix vs longer" "ab" "abc" "lessThan",
  -- Astral (non-BMP) vs. BMP private-use character (#745): comparing by
  -- UTF-16 code unit instead of code point misorders an astral character
  -- (code point > 0xFFFF, encoded as a surrogate pair starting near 0xD800)
  -- relative to a BMP private-use character (U+E000-FFFF, a single unit).
  test "astral character greater than BMP private-use character" "\127757" "\xE000" "greaterThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["threeWayClassification"] DefOrdering.compare [string x, string y] (injectUnit (name "hydra.core.util.Comparison") resultField)

orderingGt :: TypedTerm TestGroup
orderingGt = subgroup "strictGreaterThan" [
  test "greater (int32)" 5 3 true,
  test "equal (int32)" 5 5 false,
  test "less (int32)" 3 5 false]
  where
    test name x y result = primCaseWithProvisions name ["strictGreaterThan"] DefOrdering.gt [int32 x, int32 y] result

orderingGtFloats :: TypedTerm TestGroup
orderingGtFloats = subgroup "strictGreaterThan" [
  test "greater (float64)" 5.0 3.0 true,
  test "equal (float64)" 3.14 3.14 false,
  test "less (float64)" 1.5 2.5 false]
  where
    test name x y result = primCaseWithProvisions name ["strictGreaterThan"] DefOrdering.gt [float64 x, float64 y] result

orderingGtStrings :: TypedTerm TestGroup
orderingGtStrings = subgroup "strictGreaterThan" [
  test "greater (lexicographic)" "zebra" "apple" true,
  test "equal (string)" "hello" "hello" false,
  test "less (string)" "apple" "banana" false]
  where
    test name x y result = primCaseWithProvisions name ["strictGreaterThan"] DefOrdering.gt [string x, string y] result

orderingGte :: TypedTerm TestGroup
orderingGte = subgroup "greaterThanOrEqual" [
  test "greater (int32)" 5 3 true,
  test "equal (int32)" 5 5 true,
  test "less (int32)" 3 5 false]
  where
    test name x y result = primCaseWithProvisions name ["greaterThanOrEqual"] DefOrdering.gte [int32 x, int32 y] result

-- gte on floats/strings/decimals (#749): gte was previously int-only.
orderingGteFloats :: TypedTerm TestGroup
orderingGteFloats = subgroup "greaterThanOrEqual" [
  test "greater (float64)" 5.0 3.0 true,
  test "equal (float64)" 3.14 3.14 true,
  test "less (float64)" 1.5 2.5 false]
  where
    test name x y result = primCaseWithProvisions name ["greaterThanOrEqual"] DefOrdering.gte [float64 x, float64 y] result

orderingGteStrings :: TypedTerm TestGroup
orderingGteStrings = subgroup "greaterThanOrEqual" [
  test "greater (lexicographic)" "zebra" "apple" true,
  test "equal (string)" "hello" "hello" true,
  test "less (string)" "apple" "banana" false]
  where
    test name x y result = primCaseWithProvisions name ["greaterThanOrEqual"] DefOrdering.gte [string x, string y] result

orderingGteDecimals :: TypedTerm TestGroup
orderingGteDecimals = subgroup "greaterThanOrEqual" [
  test "greater (decimal)" (decimalOf 12 1) (decimalOf 11 1) true,
  test "equal, same scale" (decimalOf 11 1) (decimalOf 11 1) true,
  test "equal value, scale tiebreak (smaller scale wins)" (decimalOf 110 2) (decimalOf 11 1) true,
  test "less (decimal)" (decimalOf 11 1) (decimalOf 12 1) false]
  where
    test name x y result = primCaseWithProvisions name ["greaterThanOrEqual"] DefOrdering.gte [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

orderingLt :: TypedTerm TestGroup
orderingLt = subgroup "strictLessThan" [
  test "less (int32)" 3 5 true,
  test "equal (int32)" 5 5 false,
  test "greater (int32)" 5 3 false]
  where
    test name x y result = primCaseWithProvisions name ["strictLessThan"] DefOrdering.lt [int32 x, int32 y] result

orderingLtFloats :: TypedTerm TestGroup
orderingLtFloats = subgroup "strictLessThan" [
  test "less (float64)" 1.5 2.5 true,
  test "equal (float64)" 3.14 3.14 false,
  test "greater (float64)" 5.0 3.0 false]
  where
    test name x y result = primCaseWithProvisions name ["strictLessThan"] DefOrdering.lt [float64 x, float64 y] result

orderingLtStrings :: TypedTerm TestGroup
orderingLtStrings = subgroup "strictLessThan" [
  test "less (lexicographic)" "apple" "banana" true,
  test "equal (string)" "hello" "hello" false,
  test "greater (string)" "zebra" "apple" false]
  where
    test name x y result = primCaseWithProvisions name ["strictLessThan"] DefOrdering.lt [string x, string y] result

orderingLte :: TypedTerm TestGroup
orderingLte = subgroup "lessThanOrEqual" [
  test "less (int32)" 3 5 true,
  test "equal (int32)" 5 5 true,
  test "greater (int32)" 5 3 false]
  where
    test name x y result = primCaseWithProvisions name ["lessThanOrEqual"] DefOrdering.lte [int32 x, int32 y] result

-- lte on floats/strings/decimals (#749): lte was previously int-only.
orderingLteFloats :: TypedTerm TestGroup
orderingLteFloats = subgroup "lessThanOrEqual" [
  test "less (float64)" 1.5 2.5 true,
  test "equal (float64)" 3.14 3.14 true,
  test "greater (float64)" 5.0 3.0 false]
  where
    test name x y result = primCaseWithProvisions name ["lessThanOrEqual"] DefOrdering.lte [float64 x, float64 y] result

orderingLteStrings :: TypedTerm TestGroup
orderingLteStrings = subgroup "lessThanOrEqual" [
  test "less (lexicographic)" "apple" "banana" true,
  test "equal (string)" "hello" "hello" true,
  test "greater (string)" "zebra" "apple" false]
  where
    test name x y result = primCaseWithProvisions name ["lessThanOrEqual"] DefOrdering.lte [string x, string y] result

orderingLteDecimals :: TypedTerm TestGroup
orderingLteDecimals = subgroup "lessThanOrEqual" [
  test "less (decimal)" (decimalOf 11 1) (decimalOf 12 1) true,
  test "equal, same scale" (decimalOf 11 1) (decimalOf 11 1) true,
  test "equal value, scale tiebreak (smaller scale wins)" (decimalOf 11 1) (decimalOf 110 2) true,
  test "greater (decimal)" (decimalOf 12 1) (decimalOf 11 1) false]
  where
    test name x y result = primCaseWithProvisions name ["lessThanOrEqual"] DefOrdering.lte [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

orderingMax :: TypedTerm TestGroup
orderingMax = subgroup "returnsLargerOperand" [
  test "first greater (int32)" 5 3 5,
  test "second greater (int32)" 3 5 5,
  test "equal (int32)" 5 5 5]
  where
    test name x y result = primCaseWithProvisions name ["returnsLargerOperand"] DefOrdering.max [int32 x, int32 y] (int32 result)

orderingMaxStrings :: TypedTerm TestGroup
orderingMaxStrings = subgroup "returnsLargerOperand" [
  test "first greater (string)" "zebra" "apple" "zebra",
  test "second greater (string)" "apple" "zebra" "zebra",
  test "equal (string)" "hello" "hello" "hello"]
  where
    test name x y result = primCaseWithProvisions name ["returnsLargerOperand"] DefOrdering.max [string x, string y] (string result)

-- max on floats/decimals (#749): max previously had no float variant.
orderingMaxFloats :: TypedTerm TestGroup
orderingMaxFloats = subgroup "returnsLargerOperand" [
  test "first greater (float64)" 5.0 3.0 5.0,
  test "second greater (float64)" 3.0 5.0 5.0,
  test "equal (float64)" 3.14 3.14 3.14]
  where
    test name x y result = primCaseWithProvisions name ["returnsLargerOperand"] DefOrdering.max [float64 x, float64 y] (float64 result)

orderingMaxDecimals :: TypedTerm TestGroup
orderingMaxDecimals = subgroup "returnsLargerOperand" [
  test "first greater (decimal)" (decimalOf 12 1) (decimalOf 11 1) (decimalOf 12 1),
  test "second greater (decimal)" (decimalOf 11 1) (decimalOf 12 1) (decimalOf 12 1)]
  where
    test name x y result = primCaseWithProvisions name ["returnsLargerOperand"] DefOrdering.max [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

orderingMin :: TypedTerm TestGroup
orderingMin = subgroup "returnsSmallerOperand" [
  test "first less (int32)" 3 5 3,
  test "second less (int32)" 5 3 3,
  test "equal (int32)" 5 5 5]
  where
    test name x y result = primCaseWithProvisions name ["returnsSmallerOperand"] DefOrdering.min [int32 x, int32 y] (int32 result)

orderingMinStrings :: TypedTerm TestGroup
orderingMinStrings = subgroup "returnsSmallerOperand" [
  test "first less (string)" "apple" "zebra" "apple",
  test "second less (string)" "zebra" "apple" "apple",
  test "equal (string)" "hello" "hello" "hello"]
  where
    test name x y result = primCaseWithProvisions name ["returnsSmallerOperand"] DefOrdering.min [string x, string y] (string result)

-- min on floats/decimals (#749): min previously had no float variant.
orderingMinFloats :: TypedTerm TestGroup
orderingMinFloats = subgroup "returnsSmallerOperand" [
  test "first less (float64)" 1.5 2.5 1.5,
  test "second less (float64)" 2.5 1.5 1.5,
  test "equal (float64)" 3.14 3.14 3.14]
  where
    test name x y result = primCaseWithProvisions name ["returnsSmallerOperand"] DefOrdering.min [float64 x, float64 y] (float64 result)

orderingMinDecimals :: TypedTerm TestGroup
orderingMinDecimals = subgroup "returnsSmallerOperand" [
  test "first less (decimal)" (decimalOf 11 1) (decimalOf 12 1) (decimalOf 11 1),
  test "second less (decimal)" (decimalOf 12 1) (decimalOf 11 1) (decimalOf 11 1)]
  where
    test name x y result = primCaseWithProvisions name ["returnsSmallerOperand"] DefOrdering.min [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

-- List/set/map ordering (#749): lists are order-sensitive (lexicographic);
-- sets/maps compare by canonical (sorted) element/entry order, independent
-- of construction order — mirrors equalityEqualLists/Sets/Maps above.
orderingCompareLists :: TypedTerm TestGroup
orderingCompareLists = subgroup "threeWayClassification" [
  test "less than (elementwise)" (intList [1, 2]) (intList [1, 3]) "lessThan",
  test "less than (shorter prefix)" (intList [1, 2]) (intList [1, 2, 3]) "lessThan",
  test "equal (list)" (intList [1, 2, 3]) (intList [1, 2, 3]) "equalTo",
  test "greater than (list)" (intList [1, 3]) (intList [1, 2]) "greaterThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["threeWayClassification"] DefOrdering.compare [x, y] (injectUnit (name "hydra.core.util.Comparison") resultField)
    intList els = Terms.list (int32 <$> els)

orderingCompareSets :: TypedTerm TestGroup
orderingCompareSets = subgroup "threeWayClassification" [
  test "equal, same construction order" (intSet [1, 2, 3]) (intSet [1, 2, 3]) "equalTo",
  test "equal, different construction order" (intSet [1, 2, 3]) (intSet [3, 2, 1]) "equalTo",
  test "less than (canonical order)" (intSet [1, 2]) (intSet [1, 3]) "lessThan",
  test "greater than (canonical order)" (intSet [1, 3]) (intSet [1, 2]) "greaterThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["threeWayClassification"] DefOrdering.compare [x, y] (injectUnit (name "hydra.core.util.Comparison") resultField)
    intSet els = Terms.set (int32 <$> els)

orderingCompareMaps :: TypedTerm TestGroup
orderingCompareMaps = subgroup "threeWayClassification" [
  test "equal, same insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(1, 10), (2, 20)]) "equalTo",
  test "equal, different insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(2, 20), (1, 10)]) "equalTo",
  test "less than (by key)" (intMap [(1, 10)]) (intMap [(2, 10)]) "lessThan",
  test "less than (by value, same key)" (intMap [(1, 10)]) (intMap [(1, 20)]) "lessThan"]
  where
    test testName x y resultField = primCaseWithProvisions testName ["threeWayClassification"] DefOrdering.compare [x, y] (injectUnit (name "hydra.core.util.Comparison") resultField)
    intMap pairs = Terms.map $ Phantoms.map $ M.fromList [(int32 k, int32 v) | (k, v) <- pairs]
