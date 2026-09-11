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
      equalityEqualCollections]

equalityEqual :: TypedTerm TestGroup
equalityEqual = subgroup "equal" [
  test "equal integers" 5 5 true,
  test "unequal integers" 5 3 false]
  where
    test name x y result = primCase name DefEquality.equal [int32 x, int32 y] result

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
