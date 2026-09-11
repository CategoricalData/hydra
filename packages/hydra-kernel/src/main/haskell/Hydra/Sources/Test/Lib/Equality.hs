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
      equalityEqualFloats]

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

-- Float equality (#745): the IEEE 754 extended totalOrder (docs/specification/
-- ordering-and-equality.md), not native IEEE equality -- NaN is equal to itself, and -0.0 is
-- unequal to +0.0. A host that implements equal via native == (rather than routing through the
-- same comparator as compare) gets both wrong: NaN != NaN, and -0.0 == 0.0.
equalityEqualFloats :: TypedTerm TestGroup
equalityEqualFloats = subgroup "equal floats" [
  test "NaN equal to itself" (0/0) (0/0) true,
  test "negative zero unequal to positive zero" (-0.0) 0.0 false,
  test "positive zero equal to itself" 0.0 0.0 true,
  test "ordinary equal values" 3.14 3.14 true,
  test "ordinary unequal values" 3.14 2.71 false]
  where
    test name x y result = primCase name DefEquality.equal [float64 x, float64 y] result
