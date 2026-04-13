{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Sets where

-- Standard imports for shallow DSL tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing                 as Testing
import Hydra.Dsl.Meta.Terms                   as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core          as Core
import qualified Hydra.Dsl.Meta.Phantoms      as Phantoms
import qualified Hydra.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Set                     as S

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Sets as Sets
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore

ns :: Namespace
ns = Namespace "hydra.test.lib.sets"

module_ :: Module
module_ = Module ns definitions
    [Namespace "hydra.reduction", ShowCore.ns]
    kernelTypesNamespaces $
    Just "Test cases for hydra.lib.sets primitives"
  where
    definitions = [Phantoms.toDefinition allTests]

(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

showInt32 :: TTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

showBool :: TTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.showBoolean (Phantoms.var "b")

showIntSet :: TTerm (S.Set Int -> String)
showIntSet = Phantoms.lambda "s" $ ShowCore.set_ # showInt32 # Phantoms.var "s"

showIntList :: TTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ ShowCore.list_ # showInt32 # Phantoms.var "xs"

-- Phantom-typed helper for int sets
pIntSet :: [Int] -> TTerm (S.Set Int)
pIntSet xs = Phantoms.set (Phantoms.int32 <$> xs)

-- Test groups for hydra.lib.sets primitives

setsEmpty :: TTerm TestGroup
setsEmpty = subgroup "empty" [
  test "empty set" []]
  where
    test name expected = evalPair name showIntSet
      (Sets.empty :: TTerm (S.Set Int))
      (pIntSet expected)

setsSingleton :: TTerm TestGroup
setsSingleton = subgroup "singleton" [
  test "single element" 42 [42]]
  where
    test name x result = evalPair name showIntSet
      (Sets.singleton (Phantoms.int32 x))
      (pIntSet result)

setsFromList :: TTerm TestGroup
setsFromList = subgroup "fromList" [
  test "create from list" [1, 2, 3] [1, 2, 3],
  test "duplicates removed" [1, 2, 1, 3] [1, 2, 3],
  test "empty list" [] []]
  where
    test name input expected = evalPair name showIntSet
      (Sets.fromList (Phantoms.list $ Phantoms.int32 <$> input))
      (pIntSet expected)

setsToList :: TTerm TestGroup
setsToList = subgroup "toList" [
  test "convert to list" [1, 2, 3] [1, 2, 3],
  test "unsorted input" [3, 1, 2] [1, 2, 3],
  test "empty set" [] []]
  where
    test name input expected = evalPair name showIntList
      (Sets.toList (pIntSet input))
      (Phantoms.list $ Phantoms.int32 <$> expected)

setsInsert :: TTerm TestGroup
setsInsert = subgroup "insert" [
  test "insert new element" 4 [1, 2, 3] [1, 2, 3, 4],
  test "insert existing element" 2 [1, 2, 3] [1, 2, 3],
  test "insert into empty" 1 [] [1]]
  where
    test name x s result = evalPair name showIntSet
      (Sets.insert (Phantoms.int32 x) (pIntSet s))
      (pIntSet result)

setsDelete :: TTerm TestGroup
setsDelete = subgroup "delete" [
  test "delete existing" 2 [1, 2, 3] [1, 3],
  test "delete non-existing" 4 [1, 2, 3] [1, 2, 3],
  test "delete from empty" 1 [] []]
  where
    test name x s result = evalPair name showIntSet
      (Sets.delete (Phantoms.int32 x) (pIntSet s))
      (pIntSet result)

setsMember :: TTerm TestGroup
setsMember = subgroup "member" [
  test "element exists" 2 [1, 2, 3] True,
  test "element missing" 4 [1, 2, 3] False,
  test "empty set" 1 [] False]
  where
    test name x s result = evalPair name showBool
      (Sets.member (Phantoms.int32 x) (pIntSet s))
      (Phantoms.boolean result)

setsSize :: TTerm TestGroup
setsSize = subgroup "size" [
  test "three elements" [1, 2, 3] 3,
  test "single element" [42] 1,
  test "empty set" [] 0]
  where
    test name s result = evalPair name showInt32
      (Sets.size (pIntSet s))
      (Phantoms.int32 result)

setsNull :: TTerm TestGroup
setsNull = subgroup "null" [
  test "empty set" [] True,
  test "non-empty set" [1, 2] False]
  where
    test name s result = evalPair name showBool
      (Sets.null (pIntSet s))
      (Phantoms.boolean result)

setsUnion :: TTerm TestGroup
setsUnion = subgroup "union" [
  test "union two sets" [1, 2] [2, 3] [1, 2, 3],
  test "union with empty" [1, 2] [] [1, 2],
  test "empty with non-empty" [] [1, 2] [1, 2]]
  where
    test name s1 s2 result = evalPair name showIntSet
      (Sets.union (pIntSet s1) (pIntSet s2))
      (pIntSet result)

setsIntersection :: TTerm TestGroup
setsIntersection = subgroup "intersection" [
  test "common elements" [1, 2, 3] [2, 3, 4] [2, 3],
  test "no common elements" [1, 2] [3, 4] [],
  test "intersection with empty" [1, 2] [] []]
  where
    test name s1 s2 result = evalPair name showIntSet
      (Sets.intersection (pIntSet s1) (pIntSet s2))
      (pIntSet result)

setsDifference :: TTerm TestGroup
setsDifference = subgroup "difference" [
  test "remove elements" [1, 2, 3] [2, 4] [1, 3],
  test "no overlap" [1, 2] [3, 4] [1, 2],
  test "difference with empty" [1, 2] [] [1, 2]]
  where
    test name s1 s2 result = evalPair name showIntSet
      (Sets.difference (pIntSet s1) (pIntSet s2))
      (pIntSet result)

setsUnions :: TTerm TestGroup
setsUnions = subgroup "unions" [
  test "union of multiple sets" [[1, 2], [2, 3], [3, 4]] [1, 2, 3, 4],
  test "union with empty sets" [[1, 2], [], [3]] [1, 2, 3],
  test "empty list of sets" [] [],
  test "single set" [[1, 2, 3]] [1, 2, 3]]
  where
    test name sets result = evalPair name showIntSet
      (Sets.unions (Phantoms.list $ pIntSet <$> sets))
      (pIntSet result)

setsMap :: TTerm TestGroup
setsMap = subgroup "map" [
  test "map function" [1, 2, 3] [2, 4, 6],
  test "map on empty" [] []]
  where
    test name s result = evalPair name showIntSet
      (Sets.map (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)) (pIntSet s))
      (pIntSet result)

allTests :: TTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
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
      setsUnions,
      setsIntersection,
      setsDifference,
      setsMap]
