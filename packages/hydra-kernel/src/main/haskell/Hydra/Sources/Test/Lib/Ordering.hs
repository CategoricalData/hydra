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
import qualified Data.Scientific as Sci


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
      orderingGteStrings,
      orderingLteStrings,
      orderingMaxStrings,
      orderingMinStrings,
      -- Float tests
      orderingCompareFloats,
      orderingLtFloats,
      orderingGtFloats,
      orderingGteFloats,
      orderingLteFloats,
      orderingMaxFloats,
      orderingMinFloats,
      -- Decimal tests (#719): value first, scale as tiebreak
      orderingCompareDecimals,
      orderingGteDecimals,
      orderingLteDecimals,
      orderingMaxDecimals,
      orderingMinDecimals,
      -- Collection tests (#749): lists (order-sensitive, lexicographic),
      -- sets and maps (order-independent, canonical-order comparison)
      orderingCompareLists,
      orderingCompareSets,
      orderingCompareMaps]

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

-- Tests for ordering with decimal values: value first, then scale as tiebreak
-- (numerically equal decimals of different scale are distinct, smaller scale first).
orderingCompareDecimals :: TypedTerm TestGroup
orderingCompareDecimals = subgroup "compare decimals" [
  test "different value" (decimalOf 11 1) (decimalOf 12 1) "lessThan",
  test "same value, same scale" (decimalOf 11 1) (decimalOf 11 1) "equalTo",
  test "same value, scale tiebreak" (decimalOf 11 1) (decimalOf 110 2) "lessThan",
  test "same value, scale tiebreak (larger scale)" (decimalOf 110 2) (decimalOf 1100 3) "lessThan",
  test "same value, scale tiebreak (transitively)" (decimalOf 11 1) (decimalOf 1100 3) "lessThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [x, y] (injectUnit (name "hydra.util.Comparison") resultField)
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

-- Tests for ordering with string values
orderingCompareStrings :: TypedTerm TestGroup
orderingCompareStrings = subgroup "compare strings" [
  test "less than (lexicographic)" "apple" "banana" "lessThan",
  test "equal" "hello" "hello" "equalTo",
  test "greater than (lexicographic)" "zebra" "apple" "greaterThan",
  test "empty vs non-empty" "" "a" "lessThan",
  test "prefix vs longer" "ab" "abc" "lessThan",
  -- Astral (non-BMP) vs. BMP private-use character (#745): comparing by
  -- UTF-16 code unit instead of code point misorders an astral character
  -- (code point > 0xFFFF, encoded as a surrogate pair starting near 0xD800)
  -- relative to a BMP private-use character (U+E000-FFFF, a single unit).
  test "astral character greater than BMP private-use character" "\127757" "\xE000" "greaterThan"]
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

-- gte on floats/strings/decimals (#749): gte was previously int-only.
orderingGteFloats :: TypedTerm TestGroup
orderingGteFloats = subgroup "gte floats" [
  test "greater" 5.0 3.0 true,
  test "equal" 3.14 3.14 true,
  test "less" 1.5 2.5 false]
  where
    test name x y result = primCase name DefOrdering.gte [float64 x, float64 y] result

orderingGteStrings :: TypedTerm TestGroup
orderingGteStrings = subgroup "gte strings" [
  test "greater (lexicographic)" "zebra" "apple" true,
  test "equal" "hello" "hello" true,
  test "less" "apple" "banana" false]
  where
    test name x y result = primCase name DefOrdering.gte [string x, string y] result

orderingGteDecimals :: TypedTerm TestGroup
orderingGteDecimals = subgroup "gte decimals" [
  test "greater" (decimalOf 12 1) (decimalOf 11 1) true,
  test "equal, same scale" (decimalOf 11 1) (decimalOf 11 1) true,
  test "equal value, scale tiebreak (smaller scale wins)" (decimalOf 110 2) (decimalOf 11 1) true,
  test "less" (decimalOf 11 1) (decimalOf 12 1) false]
  where
    test name x y result = primCase name DefOrdering.gte [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

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

-- lte on floats/strings/decimals (#749): lte was previously int-only.
orderingLteFloats :: TypedTerm TestGroup
orderingLteFloats = subgroup "lte floats" [
  test "less" 1.5 2.5 true,
  test "equal" 3.14 3.14 true,
  test "greater" 5.0 3.0 false]
  where
    test name x y result = primCase name DefOrdering.lte [float64 x, float64 y] result

orderingLteStrings :: TypedTerm TestGroup
orderingLteStrings = subgroup "lte strings" [
  test "less (lexicographic)" "apple" "banana" true,
  test "equal" "hello" "hello" true,
  test "greater" "zebra" "apple" false]
  where
    test name x y result = primCase name DefOrdering.lte [string x, string y] result

orderingLteDecimals :: TypedTerm TestGroup
orderingLteDecimals = subgroup "lte decimals" [
  test "less" (decimalOf 11 1) (decimalOf 12 1) true,
  test "equal, same scale" (decimalOf 11 1) (decimalOf 11 1) true,
  test "equal value, scale tiebreak (smaller scale wins)" (decimalOf 11 1) (decimalOf 110 2) true,
  test "greater" (decimalOf 12 1) (decimalOf 11 1) false]
  where
    test name x y result = primCase name DefOrdering.lte [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

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

-- max on floats/decimals (#749): max previously had no float variant.
orderingMaxFloats :: TypedTerm TestGroup
orderingMaxFloats = subgroup "max floats" [
  test "first greater" 5.0 3.0 5.0,
  test "second greater" 3.0 5.0 5.0,
  test "equal" 3.14 3.14 3.14]
  where
    test name x y result = primCase name DefOrdering.max [float64 x, float64 y] (float64 result)

orderingMaxDecimals :: TypedTerm TestGroup
orderingMaxDecimals = subgroup "max decimals" [
  test "first greater" (decimalOf 12 1) (decimalOf 11 1) (decimalOf 12 1),
  test "second greater" (decimalOf 11 1) (decimalOf 12 1) (decimalOf 12 1)]
  where
    test name x y result = primCase name DefOrdering.max [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

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

-- min on floats/decimals (#749): min previously had no float variant.
orderingMinFloats :: TypedTerm TestGroup
orderingMinFloats = subgroup "min floats" [
  test "first less" 1.5 2.5 1.5,
  test "second less" 2.5 1.5 1.5,
  test "equal" 3.14 3.14 3.14]
  where
    test name x y result = primCase name DefOrdering.min [float64 x, float64 y] (float64 result)

orderingMinDecimals :: TypedTerm TestGroup
orderingMinDecimals = subgroup "min decimals" [
  test "first less" (decimalOf 11 1) (decimalOf 12 1) (decimalOf 11 1),
  test "second less" (decimalOf 12 1) (decimalOf 11 1) (decimalOf 11 1)]
  where
    test name x y result = primCase name DefOrdering.min [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

-- List/set/map ordering (#749): lists are order-sensitive (lexicographic);
-- sets/maps compare by canonical (sorted) element/entry order, independent
-- of construction order — mirrors equalityEqualLists/Sets/Maps above.
orderingCompareLists :: TypedTerm TestGroup
orderingCompareLists = subgroup "compare lists" [
  test "less than (elementwise)" (intList [1, 2]) (intList [1, 3]) "lessThan",
  test "less than (shorter prefix)" (intList [1, 2]) (intList [1, 2, 3]) "lessThan",
  test "equal" (intList [1, 2, 3]) (intList [1, 2, 3]) "equalTo",
  test "greater than" (intList [1, 3]) (intList [1, 2]) "greaterThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [x, y] (injectUnit (name "hydra.util.Comparison") resultField)
    intList els = Terms.list (int32 <$> els)

orderingCompareSets :: TypedTerm TestGroup
orderingCompareSets = subgroup "compare sets" [
  test "equal, same construction order" (intSet [1, 2, 3]) (intSet [1, 2, 3]) "equalTo",
  test "equal, different construction order" (intSet [1, 2, 3]) (intSet [3, 2, 1]) "equalTo",
  test "less than (canonical order)" (intSet [1, 2]) (intSet [1, 3]) "lessThan",
  test "greater than (canonical order)" (intSet [1, 3]) (intSet [1, 2]) "greaterThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [x, y] (injectUnit (name "hydra.util.Comparison") resultField)
    intSet els = Terms.set (int32 <$> els)

orderingCompareMaps :: TypedTerm TestGroup
orderingCompareMaps = subgroup "compare maps" [
  test "equal, same insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(1, 10), (2, 20)]) "equalTo",
  test "equal, different insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(2, 20), (1, 10)]) "equalTo",
  test "less than (by key)" (intMap [(1, 10)]) (intMap [(2, 10)]) "lessThan",
  test "less than (by value, same key)" (intMap [(1, 10)]) (intMap [(1, 20)]) "lessThan"]
  where
    test testName x y resultField = primCase testName DefOrdering.compare [x, y] (injectUnit (name "hydra.util.Comparison") resultField)
    intMap pairs = Terms.map $ Phantoms.map $ M.fromList [(int32 k, int32 v) | (k, v) <- pairs]
