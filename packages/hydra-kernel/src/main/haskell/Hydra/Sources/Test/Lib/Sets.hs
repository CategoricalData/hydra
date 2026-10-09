{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Sets where

-- Standard imports for tests
import Hydra.Kernel
import           Hydra.Core.Overlay.Haskell.Bootstrap (unqualifiedDep, descriptionMetadata)
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Testing                 as Testing
import Hydra.Core.Overlay.Haskell.Dsl.Meta.Terms                   as Terms hiding ((@@))
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Core          as Core
import qualified Hydra.Core.Overlay.Haskell.Dsl.Phantoms      as Phantoms
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms                ((@@))
import qualified Hydra.Core.Overlay.Haskell.Dsl.Meta.Types         as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List                    as L
import qualified Data.Map                     as M
import qualified Data.Set                     as S

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Math as Math
import qualified Hydra.Core.Dsl.Lib.Sets as Sets
import qualified Hydra.Core.Lib.Sets as DefSets
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore

ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.sets"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.core.reduction", PrintCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.sets primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]


-- Phantom-typed helper for int sets
pIntSet :: [Int] -> TypedTerm (S.Set Int)
pIntSet xs = Phantoms.set (Phantoms.int32 <$> xs)

showBool :: TypedTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.printBoolean (Phantoms.var "b")

showInt32 :: TypedTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.printInt32 (Phantoms.var "n")

showIntList :: TypedTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ PrintCore.list_ @@ showInt32 @@ Phantoms.var "xs"

showIntSet :: TypedTerm (S.Set Int -> String)
showIntSet = Phantoms.lambda "s" $ PrintCore.set_ @@ showInt32 @@ Phantoms.var "s"

-- Test groups for hydra.core.lib.sets primitives

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.sets primitives" $
    supergroup "hydra.core.lib.sets primitives" [
      setsEmpty,
      setsSingleton,
      setsFromList,
      setsToList,
      setsInsert,
      setsDelete,
      setsMember,
      setsSize,
      setsIsEmpty,
      setsUnion,
      setsUnions,
      setsIntersection,
      setsDifference,
      setsMap]

setsDelete :: TypedTerm TestGroup
setsDelete = supergroup "delete" [
  subgroup "removesElement" [
    test "delete existing" "removesElement" 2 [1, 2, 3] [1, 3]],
  subgroup "noOpWhenAbsentOrEmpty" [
    test "delete non-existing" "noOpWhenAbsentOrEmpty" 4 [1, 2, 3] [1, 2, 3],
    test "delete from empty" "noOpWhenAbsentOrEmpty" 1 [] []]]
  where
    test name segment x s result = evalPairWithProvisions name [segment] DefSets.delete showIntSet
      (Sets.delete (Phantoms.int32 x) (pIntSet s))
      (pIntSet result)

setsDifference :: TypedTerm TestGroup
setsDifference = supergroup "difference" [
  subgroup "removesElementsOfSecondSet" [
    test "remove elements" "removesElementsOfSecondSet" [1, 2, 3] [2, 4] [1, 3],
    test "no overlap" "removesElementsOfSecondSet" [1, 2] [3, 4] [1, 2]],
  subgroup "identityWhenSecondEmpty" [
    test "difference with empty" "identityWhenSecondEmpty" [1, 2] [] [1, 2]]]
  where
    test name segment s1 s2 result = evalPairWithProvisions name [segment] DefSets.difference showIntSet
      (Sets.difference (pIntSet s1) (pIntSet s2))
      (pIntSet result)

setsEmpty :: TypedTerm TestGroup
setsEmpty = subgroup "hasNoElements" [
  test "empty set" "hasNoElements" []]
  where
    test name segment expected = evalPairWithProvisions name [segment] DefSets.empty showIntSet
      (Sets.empty :: TypedTerm (S.Set Int))
      (pIntSet expected)

setsFromList :: TypedTerm TestGroup
setsFromList = supergroup "fromList" [
  subgroup "collectsDistinctElements" [
    test "create from list" "collectsDistinctElements" [1, 2, 3] [1, 2, 3],
    test "duplicates removed" "collectsDistinctElements" [1, 2, 1, 3] [1, 2, 3]],
  subgroup "emptyListYieldsEmptySet" [
    test "empty list" "emptyListYieldsEmptySet" [] []]]
  where
    test name segment input expected = evalPairWithProvisions name [segment] DefSets.fromList showIntSet
      (Sets.fromList (Phantoms.list $ Phantoms.int32 <$> input))
      (pIntSet expected)

setsInsert :: TypedTerm TestGroup
setsInsert = supergroup "insert" [
  subgroup "addsElement" [
    test "insert new element" "addsElement" 4 [1, 2, 3] [1, 2, 3, 4],
    test "insert into empty" "addsElement" 1 [] [1]],
  subgroup "noOpWhenAlreadyPresent" [
    test "insert existing element" "noOpWhenAlreadyPresent" 2 [1, 2, 3] [1, 2, 3]]]
  where
    test name segment x s result = evalPairWithProvisions name [segment] DefSets.insert showIntSet
      (Sets.insert (Phantoms.int32 x) (pIntSet s))
      (pIntSet result)

setsIntersection :: TypedTerm TestGroup
setsIntersection = supergroup "intersection" [
  subgroup "keepsElementsInBoth" [
    test "common elements" "keepsElementsInBoth" [1, 2, 3] [2, 3, 4] [2, 3]],
  subgroup "emptyWhenNoOverlapOrEitherEmpty" [
    test "no common elements" "emptyWhenNoOverlapOrEitherEmpty" [1, 2] [3, 4] [],
    test "intersection with empty" "emptyWhenNoOverlapOrEitherEmpty" [1, 2] [] []]]
  where
    test name segment s1 s2 result = evalPairWithProvisions name [segment] DefSets.intersection showIntSet
      (Sets.intersection (pIntSet s1) (pIntSet s2))
      (pIntSet result)

setsMap :: TypedTerm TestGroup
setsMap = supergroup "map" [
  subgroup "appliesFunctionToEachElement" [
    test "map function" "appliesFunctionToEachElement" [1, 2, 3] [2, 4, 6]],
  subgroup "emptyInput" [
    test "map on empty" "emptyInput" [] []]]
  where
    test name segment s result = evalPairWithProvisions name [segment] DefSets.map showIntSet
      (Sets.map (Phantoms.lambda "x" $ Math.mul (Phantoms.var "x") (Phantoms.int32 2)) (pIntSet s))
      (pIntSet result)

setsMember :: TypedTerm TestGroup
setsMember = supergroup "member" [
  subgroup "testsSetMembership" [
    test "element exists" "testsSetMembership" 2 [1, 2, 3] True],
  subgroup "falseWhenAbsentOrEmpty" [
    test "element missing" "falseWhenAbsentOrEmpty" 4 [1, 2, 3] False,
    test "empty set" "falseWhenAbsentOrEmpty" 1 [] False]]
  where
    test name segment x s result = evalPairWithProvisions name [segment] DefSets.member showBool
      (Sets.member (Phantoms.int32 x) (pIntSet s))
      (Phantoms.boolean result)

setsIsEmpty :: TypedTerm TestGroup
setsIsEmpty = subgroup "testsZeroSize" [
  test "empty set" "testsZeroSize" [] True,
  test "non-empty set" "testsZeroSize" [1, 2] False]
  where
    test name segment s result = evalPairWithProvisions name [segment] DefSets.isEmpty showBool
      (Sets.isEmpty (pIntSet s))
      (Phantoms.boolean result)

setsSingleton :: TypedTerm TestGroup
setsSingleton = subgroup "wrapsOneElement" [
  test "single element" "wrapsOneElement" 42 [42]]
  where
    test name segment x result = evalPairWithProvisions name [segment] DefSets.singleton showIntSet
      (Sets.singleton (Phantoms.int32 x))
      (pIntSet result)

setsSize :: TypedTerm TestGroup
setsSize = subgroup "countsElements" [
  test "three elements" "countsElements" [1, 2, 3] 3,
  test "single element" "countsElements" [42] 1,
  test "empty set" "countsElements" [] 0]
  where
    test name segment s result = evalPairWithProvisions name [segment] DefSets.size showInt32
      (Sets.size (pIntSet s))
      (Phantoms.int32 result)

setsToList :: TypedTerm TestGroup
setsToList = supergroup "toList" [
  subgroup "ordersAscending" [
    test "convert to list" "ordersAscending" [1, 2, 3] [1, 2, 3],
    test "unsorted input" "ordersAscending" [3, 1, 2] [1, 2, 3]],
  subgroup "emptySetYieldsEmptyList" [
    test "empty set" "emptySetYieldsEmptyList" [] []]]
  where
    test name segment input expected = evalPairWithProvisions name [segment] DefSets.toList showIntList
      (Sets.toList (pIntSet input))
      (Phantoms.list $ Phantoms.int32 <$> expected)

setsUnion :: TypedTerm TestGroup
setsUnion = supergroup "union" [
  subgroup "keepsElementsInEither" [
    test "union two sets" "keepsElementsInEither" [1, 2] [2, 3] [1, 2, 3]],
  subgroup "identityWhenEitherEmpty" [
    test "union with empty" "identityWhenEitherEmpty" [1, 2] [] [1, 2],
    test "empty with non-empty" "identityWhenEitherEmpty" [] [1, 2] [1, 2]]]
  where
    test name segment s1 s2 result = evalPairWithProvisions name [segment] DefSets.union showIntSet
      (Sets.union (pIntSet s1) (pIntSet s2))
      (pIntSet result)

setsUnions :: TypedTerm TestGroup
setsUnions = supergroup "unions" [
  subgroup "keepsElementsInAny" [
    test "union of multiple sets" "keepsElementsInAny" [[1, 2], [2, 3], [3, 4]] [1, 2, 3, 4],
    test "union with empty sets" "keepsElementsInAny" [[1, 2], [], [3]] [1, 2, 3],
    test "single set" "keepsElementsInAny" [[1, 2, 3]] [1, 2, 3]],
  subgroup "emptyListOfSetsYieldsEmptySet" [
    test "empty list of sets" "emptyListOfSetsYieldsEmptySet" [] []]]
  where
    test name segment sets result = evalPairWithProvisions name [segment] DefSets.unions showIntSet
      (Sets.unions (Phantoms.list $ pIntSet <$> sets))
      (pIntSet result)
