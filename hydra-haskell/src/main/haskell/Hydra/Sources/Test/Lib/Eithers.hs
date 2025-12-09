module Hydra.Sources.Test.Lib.Eithers where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Meta.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import Hydra.Dsl.Meta.Terms as MetaTerms
import qualified Hydra.Sources.Kernel.Types.All as KernelTypes
import qualified Hydra.Sources.Kernel.Types.Testing as TestingTypes
import qualified Hydra.Sources.Test.TestGraph as TestGraph


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.eithers") elements [] [] $
    Just "Test cases for hydra.lib.eithers primitives"
  where
    elements = [Phantoms.toBinding allTests]

-- Helper to create Left terms
leftInt32 :: Int -> TTerm Term
leftInt32 x = left (int32 x)

-- Helper to create Right terms
rightString :: String -> TTerm Term
rightString s = right (string s)

-- Test groups for hydra.lib.eithers primitives

eithersIsLeft :: TTerm TestGroup
eithersIsLeft = subgroup "isLeft" [
  test "left value" (leftInt32 42) true,
  test "right value" (rightString "test") false]
  where
    test name x result = primCase name _eithers_isLeft [x] result

eithersIsRight :: TTerm TestGroup
eithersIsRight = subgroup "isRight" [
  test "right value" (rightString "test") true,
  test "left value" (leftInt32 42) false]
  where
    test name x result = primCase name _eithers_isRight [x] result

eithersFromLeft :: TTerm TestGroup
eithersFromLeft = subgroup "fromLeft" [
  test "extract left" 99 (leftInt32 42) 42,
  test "use default for right" 99 (rightString "test") 99]
  where
    test name def x result = primCase name _eithers_fromLeft [int32 def, x] (int32 result)

eithersFromRight :: TTerm TestGroup
eithersFromRight = subgroup "fromRight" [
  test "extract right" "default" (rightString "test") "test",
  test "use default for left" "default" (leftInt32 42) "default"]
  where
    test name def x result = primCase name _eithers_fromRight [string def, x] (string result)

eithersEither :: TTerm TestGroup
eithersEither = subgroup "either" [
  test "apply left function" (leftInt32 5) 10,
  test "apply right function" (rightString "ab") 2]
  where
    test name x result = primCase name _eithers_either [
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      lambda "s" (primitive _strings_length @@ var "s"),
      x] (int32 result)

eithersLefts :: TTerm TestGroup
eithersLefts = subgroup "lefts" [
  test "filter left values" [leftInt32 1, rightString "a", leftInt32 2, rightString "b"] [1, 2],
  test "all lefts" [leftInt32 1, leftInt32 2] [1, 2],
  test "all rights" [rightString "a", rightString "b"] [],
  test "empty list" [] []]
  where
    test name input expected = primCase name _eithers_lefts [list input] (list $ fmap int32 expected)

eithersRights :: TTerm TestGroup
eithersRights = subgroup "rights" [
  test "filter right values" [leftInt32 1, rightString "a", leftInt32 2, rightString "b"] ["a", "b"],
  test "all rights" [rightString "a", rightString "b"] ["a", "b"],
  test "all lefts" [leftInt32 1, leftInt32 2] [],
  test "empty list" [] []]
  where
    test name input expected = primCase name _eithers_rights [list input] (list $ fmap string expected)

eithersPartitionEithers :: TTerm TestGroup
eithersPartitionEithers = subgroup "partitionEithers" [
  test "partition mixed" [leftInt32 1, rightString "a", leftInt32 2, rightString "b"] ([1, 2], ["a", "b"]),
  test "all lefts" [leftInt32 1, leftInt32 2] ([1, 2], []),
  test "all rights" [rightString "a", rightString "b"] ([], ["a", "b"]),
  test "empty list" [] ([], [])]
  where
    test name input (lefts, rights) = primCase name _eithers_partitionEithers [list input]
      (Core.termPair $ Phantoms.pair (list $ fmap int32 lefts) (list $ fmap string rights))

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.eithers primitives" $
    supergroup "hydra.lib.eithers primitives" [
      eithersIsLeft,
      eithersIsRight,
      eithersFromLeft,
      eithersFromRight,
      eithersEither,
      eithersLefts,
      eithersRights,
      eithersPartitionEithers]
