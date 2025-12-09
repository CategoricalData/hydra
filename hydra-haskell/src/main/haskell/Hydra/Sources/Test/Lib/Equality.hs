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


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.equality") elements [] [] $
    Just "Test cases for hydra.lib.equality primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Test groups for hydra.lib.equality primitives

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

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.equality primitives" $
    supergroup "hydra.lib.equality primitives" [
      equalityEqual,
      equalityGt,
      equalityGte,
      equalityIdentity,
      equalityLt,
      equalityLte,
      equalityMax,
      equalityMin]
