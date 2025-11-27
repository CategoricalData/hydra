module Hydra.Sources.Test.Lib.Sets where

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
import qualified Data.List as L


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.sets") elements [] [] $
    Just "Test cases for hydra.lib.sets primitives"
  where
    elements = [el allTestsDef]

emptyStringSet = intSet []
intSetOrEmpty = intSet

-- Helper to create set terms
intSet :: [Int] -> TTerm Term
intSet xs = Core.termSet $ Phantoms.set $ Prelude.map int32 xs

--intSetOrEmpty :: [Int] -> TTerm Term
--intSetOrEmpty xs = if L.null xs
--  then emptyStringSet
--  else intSet xs

-- Test groups for hydra.lib.sets primitives

setsEmpty :: TTerm TestGroup
setsEmpty = subgroup "empty" [
  test "empty set" []]
  where
    test name expected = primCase name _sets_empty [] emptyStringSet

setsSingleton :: TTerm TestGroup
setsSingleton = subgroup "singleton" [
  test "single element" 42 [42]]
  where
    test name x result = primCase name _sets_singleton [int32 x] (intSet result)

setsFromList :: TTerm TestGroup
setsFromList = subgroup "fromList" [
  test "create from list" [1, 2, 3] [1, 2, 3],
  test "duplicates removed" [1, 2, 1, 3] [1, 2, 3],
  test "empty list" [] []]
  where
    test name input expected = primCase name _sets_fromList [list $ Prelude.map int32 input] (intSetOrEmpty expected)

setsToList :: TTerm TestGroup
setsToList = subgroup "toList" [
  test "convert to list" [1, 2, 3] [1, 2, 3],
  test "empty set" [] []]
  where
    test name input expected = primCase name _sets_toList [intSetOrEmpty input] (list $ Prelude.map int32 expected)

setsInsert :: TTerm TestGroup
setsInsert = subgroup "insert" [
  test "insert new element" 4 [1, 2, 3] [1, 2, 3, 4],
  test "insert existing element" 2 [1, 2, 3] [1, 2, 3],
  test "insert into empty" 1 [] [1]]
  where
    test name x s result = primCase name _sets_insert [int32 x, intSet s] (intSet result)

setsDelete :: TTerm TestGroup
setsDelete = subgroup "delete" [
  test "delete existing" 2 [1, 2, 3] [1, 3],
  test "delete non-existing" 4 [1, 2, 3] [1, 2, 3],
  test "delete from empty" 1 [] []]
  where
    test name x s result = primCase name _sets_delete [int32 x, intSet s] (intSet result)

setsMember :: TTerm TestGroup
setsMember = subgroup "member" [
  test "element exists" 2 [1, 2, 3] true,
  test "element missing" 4 [1, 2, 3] false,
  test "empty set" 1 [] false]
  where
    test name x s result = primCase name _sets_member [int32 x, intSet s] result

setsSize :: TTerm TestGroup
setsSize = subgroup "size" [
  test "three elements" [1, 2, 3] 3,
  test "single element" [42] 1,
  test "empty set" [] 0]
  where
    test name s result = primCase name _sets_size [intSetOrEmpty s] (int32 result)

setsNull :: TTerm TestGroup
setsNull = subgroup "null" [
  test "empty set" [] true,
  test "non-empty set" [1, 2] false]
  where
    test name s result = primCase name _sets_null [intSetOrEmpty s] result

setsUnion :: TTerm TestGroup
setsUnion = subgroup "union" [
  test "union two sets" [1, 2] [2, 3] [1, 2, 3],
  test "union with empty" [1, 2] [] [1, 2],
  test "empty with non-empty" [] [1, 2] [1, 2]]
  where
    test name s1 s2 result = primCase name _sets_union [intSet s1, intSet s2] (intSet result)

setsIntersection :: TTerm TestGroup
setsIntersection = subgroup "intersection" [
  test "common elements" [1, 2, 3] [2, 3, 4] [2, 3],
  test "no common elements" [1, 2] [3, 4] [],
  test "intersection with empty" [1, 2] [] []]
  where
    test name s1 s2 result = primCase name _sets_intersection [intSet s1, intSet s2] (intSet result)

setsDifference :: TTerm TestGroup
setsDifference = subgroup "difference" [
  test "remove elements" [1, 2, 3] [2, 4] [1, 3],
  test "no overlap" [1, 2] [3, 4] [1, 2],
  test "difference with empty" [1, 2] [] [1, 2]]
  where
    test name s1 s2 result = primCase name _sets_difference [intSet s1, intSet s2] (intSet result)

setsMap :: TTerm TestGroup
setsMap = subgroup "map" [
  test "map function" [1, 2, 3] [2, 4, 6],
  test "map on empty" [] []]
  where
    test name s result = primCaseWithTags name [tag_requiresInterp] _sets_map [
      lambda "x" (primitive _math_mul @@ var "x" @@ int32 2),
      intSet s] (intSet result)

allTestsDef :: TBinding TestGroup
allTestsDef = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.sets primitives" $
    supergroup "hydra.lib.sets primitives" [
      setsEmpty,
      setsSingleton,
      setsFromList,
      setsToList,
      setsInsert,
      setsDelete,
      setsMember,
      setsSize,
      setsNull,
      setsUnion,
      setsIntersection,
      setsDifference,
      setsMap]
