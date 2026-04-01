{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Maps where

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

-- Additional imports specific to this file
import Hydra.Testing
import Hydra.Sources.Libraries
import qualified Hydra.Dsl.Meta.Lib.Equality as Equality
import qualified Hydra.Dsl.Meta.Lib.Literals as Literals
import qualified Hydra.Dsl.Meta.Lib.Maps as Maps
import qualified Hydra.Dsl.Meta.Lib.Math as Math
import qualified Hydra.Dsl.Meta.Lib.Maybes as Maybes
import qualified Hydra.Dsl.Meta.Lib.Strings as Strings
import qualified Hydra.Dsl.Meta.Lib.Chars as Chars
import qualified Hydra.Sources.Kernel.Terms.Show.Core as ShowCore


ns :: Namespace
ns = Namespace "hydra.test.lib.maps"

module_ :: Module
module_ = Module ns elements
    [Namespace "hydra.reduction", ShowCore.ns]
    kernelTypesNamespaces $
    Just "Test cases for hydra.lib.maps primitives"
  where
    elements = [Phantoms.toTermDefinition allTests]

(#) :: (AsTerm f (a -> b), AsTerm g a) => f -> g -> TTerm b
(#) = (Phantoms.@@)
infixl 1 #

showInt32 :: TTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.showInt32 (Phantoms.var "n")

showString' :: TTerm (String -> String)
showString' = Phantoms.lambda "s" $ Literals.showString (Phantoms.var "s")

showBool :: TTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.showBoolean (Phantoms.var "b")

showIntStringMap :: TTerm (M.Map Int String -> String)
showIntStringMap = Phantoms.lambda "m" $ ShowCore.map_ # showInt32 # showString' # Phantoms.var "m"

showMaybeString :: TTerm (Maybe String -> String)
showMaybeString = Phantoms.lambda "mx" $ ShowCore.maybe_ # showString' # Phantoms.var "mx"

showIntList :: TTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ ShowCore.list_ # showInt32 # Phantoms.var "xs"

showStringList :: TTerm ([String] -> String)
showStringList = Phantoms.lambda "xs" $ ShowCore.list_ # showString' # Phantoms.var "xs"

showPairList :: TTerm ([(Int, String)] -> String)
showPairList = Phantoms.lambda "xs" $ ShowCore.list_ # (Phantoms.lambda "p" $ ShowCore.pair_ # showInt32 # showString' # Phantoms.var "p") # Phantoms.var "xs"

-- Phantom-typed map helper
pMap :: [(Int, String)] -> TTerm (M.Map Int String)
pMap pairs = Phantoms.map $ M.fromList $ fmap (\(k, v) -> (Phantoms.int32 k, Phantoms.string v)) pairs

pPairList :: [(Int, String)] -> TTerm [(Int, String)]
pPairList pairs = Phantoms.list $ fmap (\(k, v) -> Phantoms.pair (Phantoms.int32 k) (Phantoms.string v)) pairs

-- Test groups

mapsEmpty :: TTerm TestGroup
mapsEmpty = subgroup "empty" [
  test "empty map"]
  where
    test name = evalPair name showIntStringMap
      (Maps.empty :: TTerm (M.Map Int String))
      (pMap [])

mapsSingleton :: TTerm TestGroup
mapsSingleton = subgroup "singleton" [
  test "single entry" 42 "hello" [(42, "hello")]]
  where
    test name k v result = evalPair name showIntStringMap
      (Maps.singleton (Phantoms.int32 k) (Phantoms.string v))
      (pMap result)

mapsFromList :: TTerm TestGroup
mapsFromList = subgroup "fromList" [
  test "create from pairs" [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
  test "duplicate keys" [(1, "a"), (1, "b")] [(1, "b")],
  test "empty list" [] []]
  where
    test name input expected = evalPair name showIntStringMap
      (Maps.fromList (pPairList input))
      (pMap expected)

mapsToList :: TTerm TestGroup
mapsToList = subgroup "toList" [
  test "convert to pairs" [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
  test "unsorted keys" [(3, "c"), (1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "c")],
  test "empty map" [] []]
  where
    test name input expected = evalPair name showPairList
      (Maps.toList (pMap input))
      (pPairList expected)

mapsInsert :: TTerm TestGroup
mapsInsert = subgroup "insert" [
  test "insert new key" 3 "c" [(1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "c")],
  test "update existing" 2 "updated" [(1, "a"), (2, "b")] [(1, "a"), (2, "updated")],
  test "insert into empty" 1 "x" [] [(1, "x")]]
  where
    test name k v m result = evalPair name showIntStringMap
      (Maps.insert (Phantoms.int32 k) (Phantoms.string v) (pMap m))
      (pMap result)

mapsRemove :: TTerm TestGroup
mapsRemove = subgroup "remove" [
  test "remove existing" 2 [(1, "a"), (2, "b"), (3, "c")] [(1, "a"), (3, "c")],
  test "remove non-existing" 4 [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
  test "remove from empty" 1 [] []]
  where
    test name k m result = evalPair name showIntStringMap
      (Maps.delete (Phantoms.int32 k) (pMap m))
      (pMap result)

mapsLookup :: TTerm TestGroup
mapsLookup = subgroup "lookup" [
  test "find existing key" 2 [(1, "a"), (2, "b")] (Just "b"),
  test "key not found" 3 [(1, "a"), (2, "b")] Nothing,
  test "lookup in empty" 1 [] Nothing]
  where
    test name k m result = evalPair name showMaybeString
      (Maps.lookup (Phantoms.int32 k) (pMap m))
      (maybe Phantoms.nothing (Phantoms.just . Phantoms.string) result)

mapsMember :: TTerm TestGroup
mapsMember = subgroup "member" [
  test "key exists" 2 [(1, "a"), (2, "b")] True,
  test "key missing" 3 [(1, "a"), (2, "b")] False,
  test "empty map" 1 [] False]
  where
    test name k m result = evalPair name showBool
      (Maps.member (Phantoms.int32 k) (pMap m))
      (Phantoms.boolean result)

mapsSize :: TTerm TestGroup
mapsSize = subgroup "size" [
  test "three entries" [(1, "a"), (2, "b"), (3, "c")] 3,
  test "single entry" [(42, "test")] 1,
  test "empty map" [] 0]
  where
    test name m result = evalPair name showInt32
      (Maps.size (pMap m))
      (Phantoms.int32 result)

mapsNull :: TTerm TestGroup
mapsNull = subgroup "null" [
  test "empty map" [] True,
  test "non-empty map" [(1, "a")] False]
  where
    test name m result = evalPair name showBool
      (Maps.null (pMap m))
      (Phantoms.boolean result)

mapsKeys :: TTerm TestGroup
mapsKeys = subgroup "keys" [
  test "get all keys" [(1, "a"), (2, "b"), (3, "c")] [1, 2, 3],
  test "unsorted keys" [(3, "c"), (1, "a"), (2, "b")] [1, 2, 3],
  test "empty map" [] []]
  where
    test name m result = evalPair name showIntList
      (Maps.keys (pMap m))
      (Phantoms.list $ Phantoms.int32 <$> result)

mapsElems :: TTerm TestGroup
mapsElems = subgroup "elems" [
  test "get all elements" [(1, "a"), (2, "b")] ["a", "b"],
  test "unsorted keys" [(3, "c"), (1, "a"), (2, "b")] ["a", "b", "c"],
  test "empty map" [] []]
  where
    test name m result = evalPair name showStringList
      (Maps.elems (pMap m))
      (Phantoms.list $ Phantoms.string <$> result)

mapsMap :: TTerm TestGroup
mapsMap = subgroup "map" [
  test "map over values" [(1, "a"), (2, "b")] [(1, "A"), (2, "B")],
  test "map empty" [] []]
  where
    test name m result = evalPair name showIntStringMap
      (Maps.map (Phantoms.lambda "s" $ Strings.toUpper (Phantoms.var "s")) (pMap m))
      (pMap result)

mapsFindWithDefault :: TTerm TestGroup
mapsFindWithDefault = subgroup "findWithDefault" [
  test "find existing" "default" 2 [(1, "a"), (2, "b")] "b",
  test "use default" "default" 3 [(1, "a"), (2, "b")] "default"]
  where
    test name def k m result = stringEvalPair name
      (Maps.findWithDefault (Phantoms.string def) (Phantoms.int32 k) (pMap m))
      (Phantoms.string result)

mapsUnion :: TTerm TestGroup
mapsUnion = subgroup "union" [
  test "union two maps" [(1, "a"), (2, "b")] [(2, "x"), (3, "c")] [(1, "a"), (2, "b"), (3, "c")],
  test "union with empty" [(1, "a")] [] [(1, "a")],
  test "empty with map" [] [(1, "a")] [(1, "a")]]
  where
    test name m1 m2 result = evalPair name showIntStringMap
      (Maps.union (pMap m1) (pMap m2))
      (pMap result)

mapsMapKeys :: TTerm TestGroup
mapsMapKeys = subgroup "mapKeys" [
  test "double keys" [(1, "a"), (2, "b")] [(2, "a"), (4, "b")],
  test "empty map" [] []]
  where
    test name m result = evalPair name showIntStringMap
      (Maps.mapKeys (Phantoms.lambda "k" $ Math.mul (Phantoms.var "k") (Phantoms.int32 2)) (pMap m))
      (pMap result)

mapsFilter :: TTerm TestGroup
mapsFilter = subgroup "filter" [
  test "filter values starting with a" [(1, "a"), (2, "b"), (3, "ab")] [(1, "a"), (3, "ab")],
  test "filter all" [(1, "b"), (2, "c")] [],
  test "empty map" [] []]
  where
    test name m result = evalPair name showIntStringMap
      (Maps.filter (Phantoms.lambda "v" $ Equality.equal (Chars.toLower (Strings.charAt (Phantoms.int32 0) (Phantoms.var "v"))) (Phantoms.int32 97)) (pMap m))
      (pMap result)

mapsFilterWithKey :: TTerm TestGroup
mapsFilterWithKey = subgroup "filterWithKey" [
  test "filter by key > 1" [(1, "a"), (2, "b"), (3, "c")] [(2, "b"), (3, "c")],
  test "filter all" [(1, "a")] [],
  test "empty map" [] []]
  where
    test name m result = evalPair name showIntStringMap
      (Maps.filterWithKey (Phantoms.lambda "k" $ Phantoms.lambda "v" $ Equality.gt (Phantoms.var "k") (Phantoms.int32 1)) (pMap m))
      (pMap result)

mapsBimap :: TTerm TestGroup
mapsBimap = subgroup "bimap" [
  test "transform both" [(1, "a"), (2, "b")] [(2, "A"), (4, "B")],
  test "empty map" [] []]
  where
    test name m result = evalPair name showIntStringMap
      (Maps.bimap
        (Phantoms.lambda "k" $ Math.mul (Phantoms.var "k") (Phantoms.int32 2))
        (Phantoms.lambda "v" $ Strings.toUpper (Phantoms.var "v"))
        (pMap m))
      (pMap result)

mapsAlter :: TTerm TestGroup
mapsAlter = subgroup "alter" [
  testInsert "insert new key" 3 [(1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "new")],
  testUpdate "update existing key" 2 [(1, "a"), (2, "b")] [(1, "a"), (2, "updated")],
  testDelete "delete key" 2 [(1, "a"), (2, "b")] [(1, "a")]]
  where
    testInsert name k m result = evalPair name showIntStringMap
      (Maps.alter (Phantoms.lambda "_" $ Phantoms.just (Phantoms.string "new")) (Phantoms.int32 k) (pMap m))
      (pMap result)
    testUpdate name k m result = evalPair name showIntStringMap
      (Maps.alter (Phantoms.lambda "_" $ Phantoms.just (Phantoms.string "updated")) (Phantoms.int32 k) (pMap m))
      (pMap result)
    testDelete name k m result = evalPair name showIntStringMap
      (Maps.alter (Phantoms.lambda "_" $ (Phantoms.nothing :: TTerm (Maybe String))) (Phantoms.int32 k) (pMap m))
      (pMap result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.maps primitives" $
    supergroup "hydra.lib.maps primitives" [
      mapsAlter,
      mapsBimap,
      mapsElems,
      mapsEmpty,
      mapsFilter,
      mapsFilterWithKey,
      mapsFindWithDefault,
      mapsFromList,
      mapsInsert,
      mapsKeys,
      mapsLookup,
      mapsMap,
      mapsMapKeys,
      mapsMember,
      mapsNull,
      mapsRemove,
      mapsSingleton,
      mapsSize,
      mapsToList,
      mapsUnion]
