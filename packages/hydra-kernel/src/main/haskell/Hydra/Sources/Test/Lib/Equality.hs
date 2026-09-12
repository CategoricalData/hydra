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
import qualified Data.Scientific as Sci


ns :: ModuleName
ns = ModuleName "hydra.test.lib.equality"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> [ModuleName "hydra.reduction", ModuleName "hydra.print.core", ModuleName "hydra.core", ModuleName "hydra.errors", ModuleName "hydra.test.testGraph", ModuleName "hydra.testing", ModuleName "hydra.util"],
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.lib.equality primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]

-- Test groups for hydra.lib.equality primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.equality primitives" $
    supergroup "hydra.lib.equality primitives" [
      equalityEqual,
      equalityEqualDecimals,
      equalityEqualCollections,
      equalityEqualFloats,
      equalityEqualLists,
      equalityEqualSets,
      equalityEqualMaps,
      equalityNotEqual,
      equalityNotEqualCollections]

equalityEqual :: TypedTerm TestGroup
equalityEqual = subgroup "equal" [
  test "equal integers" 5 5 true,
  test "unequal integers" 5 3 false]
  where
    test name x y result = primCase name DefEquality.equal [int32 x, int32 y] result

-- notEqual (#749): had zero test coverage prior to this addition. Scalar
-- baseline mirrors equalityEqual's cases with the inverse expectation.
equalityNotEqual :: TypedTerm TestGroup
equalityNotEqual = subgroup "notEqual" [
  test "equal integers" (int32 5) (int32 5) false,
  test "unequal integers" (int32 5) (int32 3) true,
  test "equal strings" (string "a") (string "a") false,
  test "unequal strings" (string "a") (string "b") true,
  test "equal floats" (float64 1.5) (float64 1.5) false,
  test "unequal floats" (float64 1.5) (float64 2.5) true]
  where
    test name x y result = primCase name DefEquality.notEqual [x, y] result

-- Decimal equality (#719): numerically equal decimals of different scale are
-- distinct, unequal values.
equalityEqualDecimals :: TypedTerm TestGroup
equalityEqualDecimals = subgroup "equal decimals" [
  test "same value, same scale" (decimalOf 11 1) (decimalOf 11 1) true,
  test "same value, different scale" (decimalOf 11 1) (decimalOf 110 2) false,
  test "different value, same scale" (decimalOf 11 1) (decimalOf 12 1) false]
  where
    test name x y result = primCase name DefEquality.equal [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))

-- Map/set equality with collection payloads (#742): a naive host
-- implementation may compare collections by an unreliable proxy (e.g. a
-- print/stringify representation that doesn't actually serialize the
-- collection's contents, or a decimal comparison that only fires for a bare
-- top-level decimal and not one nested inside a collection value) rather
-- than comparing elements/entries structurally. These cases regression-test
-- both failure modes found on the TypeScript and Clojure overlays: a
-- differing-contents non-empty map must not be reported equal, and a
-- decimal's scale-distinctness (#719) must still apply when the decimal is a
-- map value rather than a bare top-level term.
equalityEqualCollections :: TypedTerm TestGroup
equalityEqualCollections = subgroup "equal collections" [
  test "non-empty maps with different contents"
    (mapOf "k1" (int32 1)) (mapOf "k1" (int32 99)) false,
  test "non-empty maps with same contents"
    (mapOf "k1" (int32 1)) (mapOf "k1" (int32 1)) true,
  test "decimal nested in a map value, different scale"
    (mapOf "k" (decimalOf 11 1)) (mapOf "k" (decimalOf 110 2)) false,
  test "decimal nested in a map value, same scale"
    (mapOf "k" (decimalOf 11 1)) (mapOf "k" (decimalOf 11 1)) true]
  where
    test name x y result = primCase name DefEquality.equal [x, y] result
    decimalOf coefficient scale = decimal (Sci.scientific coefficient (negate scale))
    mapOf key value = Terms.map $ Phantoms.map $ M.singleton (string key) value

-- Float equality (#745): IEEE 754 extended totalOrder semantics
-- (docs/specification/ordering-and-equality.md) -- NaN is equal to itself, and
-- -0.0 is unequal to +0.0. A host that delegates to its native == disagrees on
-- both (NaN != NaN, -0.0 == 0.0 natively).
equalityEqualFloats :: TypedTerm TestGroup
equalityEqualFloats = subgroup "equal floats" [
  test "NaN equal to itself" (float64 nan) (float64 nan) true,
  test "negative zero unequal to positive zero" (float64 (-0.0)) (float64 0.0) false,
  test "positive zero equal to itself" (float64 0.0) (float64 0.0) true,
  test "ordinary equal values" (float64 1.5) (float64 1.5) true,
  test "ordinary unequal values" (float64 1.5) (float64 2.5) false]
  where
    test name x y result = primCase name DefEquality.equal [x, y] result
    nan = 0/0 :: Double

-- List equality (#749): lists are ORDER-SENSITIVE — same elements in a
-- different order must compare unequal, unlike sets/maps below.
equalityEqualLists :: TypedTerm TestGroup
equalityEqualLists = subgroup "equal lists" [
  test "equal lists" (intList [1, 2, 3]) (intList [1, 2, 3]) true,
  test "different elements" (intList [1, 2, 3]) (intList [1, 2, 4]) false,
  test "different length (prefix)" (intList [1, 2, 3]) (intList [1, 2]) false,
  test "same elements, different order" (intList [1, 2, 3]) (intList [3, 2, 1]) false,
  test "nested lists, equal" (intListList [[1, 2], [3]]) (intListList [[1, 2], [3]]) true,
  test "nested lists, unequal" (intListList [[1, 2], [3]]) (intListList [[1, 2], [4]]) false]
  where
    test name x y result = primCase name DefEquality.equal [x, y] result
    intList els = Terms.list (int32 <$> els)
    intListList lsts = Terms.list (intList <$> lsts)

-- Set equality (#749): sets are ORDER-INDEPENDENT — the defining property
-- that distinguishes set equality from list equality above.
equalityEqualSets :: TypedTerm TestGroup
equalityEqualSets = subgroup "equal sets" [
  test "equal sets, same construction order" (intSet [1, 2, 3]) (intSet [1, 2, 3]) true,
  test "equal sets, different construction order" (intSet [1, 2, 3]) (intSet [3, 2, 1]) true,
  test "different elements" (intSet [1, 2, 3]) (intSet [1, 2, 4]) false,
  test "subset, not equal" (intSet [1, 2, 3]) (intSet [1, 2]) false]
  where
    test name x y result = primCase name DefEquality.equal [x, y] result
    intSet els = Terms.set (int32 <$> els)

-- Map equality (#749): maps are KEY-ORDER-INDEPENDENT, extending #742's
-- equalityEqualCollections with missing-key and nested-map-value cases.
equalityEqualMaps :: TypedTerm TestGroup
equalityEqualMaps = subgroup "equal maps" [
  test "equal maps, same insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(1, 10), (2, 20)]) true,
  test "equal maps, different insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(2, 20), (1, 10)]) true,
  test "missing key" (intMap [(1, 10)]) (intMap [(1, 10), (2, 20)]) false,
  test "value mismatch" (intMap [(1, 10), (2, 20)]) (intMap [(1, 10), (2, 21)]) false,
  test "nested map value, equal"
    (nestedIntMap [(1, [(2, 20)])]) (nestedIntMap [(1, [(2, 20)])]) true,
  test "nested map value, unequal"
    (nestedIntMap [(1, [(2, 20)])]) (nestedIntMap [(1, [(2, 21)])]) false]
  where
    test name x y result = primCase name DefEquality.equal [x, y] result
    intMap pairs = Terms.map $ Phantoms.map $ M.fromList [(int32 k, int32 v) | (k, v) <- pairs]
    nestedIntMap pairs = Terms.map $ Phantoms.map $
      M.fromList [(int32 k, intMap inner) | (k, inner) <- pairs]

-- notEqual on collections (#749): mirrors equalityEqualCollections/
-- equalityEqualLists/Sets/Maps with the inverse expectation, since notEqual
-- had zero pre-existing coverage of any kind (see equalityNotEqual above for
-- the scalar baseline).
equalityNotEqualCollections :: TypedTerm TestGroup
equalityNotEqualCollections = subgroup "notEqual collections" [
  test "equal lists" (intList [1, 2, 3]) (intList [1, 2, 3]) false,
  test "unequal lists (order)" (intList [1, 2, 3]) (intList [3, 2, 1]) true,
  test "equal sets, different order" (intSet [1, 2, 3]) (intSet [3, 2, 1]) false,
  test "unequal sets" (intSet [1, 2, 3]) (intSet [1, 2, 4]) true,
  test "equal maps, different insertion order"
    (intMap [(1, 10), (2, 20)]) (intMap [(2, 20), (1, 10)]) false,
  test "unequal maps" (intMap [(1, 10)]) (intMap [(1, 10), (2, 20)]) true]
  where
    test name x y result = primCase name DefEquality.notEqual [x, y] result
    intList els = Terms.list (int32 <$> els)
    intSet els = Terms.set (int32 <$> els)
    intMap pairs = Terms.map $ Phantoms.map $ M.fromList [(int32 k, int32 v) | (k, v) <- pairs]
