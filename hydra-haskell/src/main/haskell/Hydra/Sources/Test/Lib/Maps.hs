module Hydra.Sources.Test.Lib.Maps where

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
import qualified Data.Map as M

import qualified Hydra.Dsl.Meta.Lib.Maps as Maps


module_ :: Module
module_ = Module (Namespace "hydra.test.lib.maps") elements [] [] $
    Just "Test cases for hydra.lib.maps primitives"
  where
    elements = [Phantoms.toBinding allTests]

emptyStringMap = intStringMap []
intStringMapOrEmpty = intStringMap

---- This is a hack for HSpec. We create a map<string, string>, because that is the map type we default to when there are no elements.
--emptyStringMap :: TTerm Term
--emptyStringMap = primitive _maps_fromList @@ (primitive _lists_drop @@ int32 1 @@ list [pair (string "") (string "")])

-- Helper to create map terms (Int -> String maps)
intStringMap :: [(Int, String)] -> TTerm Term
intStringMap pairs = Core.termMap $ Phantoms.map $ M.fromList $ fmap toPair pairs
  where
    toPair (k, v) = (int32 k, string v)

--intStringMapOrEmpty :: [(Int, String)] -> TTerm Term
--intStringMapOrEmpty pairs = if L.null pairs then emptyStringMap else intStringMap pairs

-- Helper for optional values
optionalString :: Maybe String -> TTerm Term
optionalString Nothing = Core.termMaybe nothing
optionalString (Just s) = Core.termMaybe $ just (MetaTerms.string s)

-- Test groups for hydra.lib.maps primitives

mapsEmpty :: TTerm TestGroup
mapsEmpty = subgroup "empty" [
  test "empty map"]
  where
    test name = primCase name _maps_empty [] emptyStringMap

mapsSingleton :: TTerm TestGroup
mapsSingleton = subgroup "singleton" [
  test "single entry" 42 "hello" [(42, "hello")]]
  where
    test name k v result = primCase name _maps_singleton [int32 k, MetaTerms.string v] (intStringMap result)

mapsFromList :: TTerm TestGroup
mapsFromList = subgroup "fromList" [
  test "create from pairs" [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
  test "duplicate keys" [(1, "a"), (1, "b")] [(1, "b")],
  test "empty list" [] []]
  where
    test name input expected = primCase name _maps_fromList [
      list $ Prelude.map (\(k, v) -> Core.termPair $ Phantoms.pair (int32 k) (MetaTerms.string v)) input
      ] $ intStringMap expected

mapsToList :: TTerm TestGroup
mapsToList = subgroup "toList" [
  test "convert to pairs" [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
  test "empty map" [] []]
  where
    test name input expected = primCase name _maps_toList [intStringMap input] (
      list $ Prelude.map (\(k, v) -> Core.termPair $ Phantoms.pair (int32 k) (MetaTerms.string v)) expected)

mapsInsert :: TTerm TestGroup
mapsInsert = subgroup "insert" [
  test "insert new key" 3 "c" [(1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "c")],
  test "update existing" 2 "updated" [(1, "a"), (2, "b")] [(1, "a"), (2, "updated")],
  test "insert into empty" 1 "x" [] [(1, "x")]]
  where
    test name k v m result = primCase name _maps_insert [int32 k, MetaTerms.string v, intStringMap m] (intStringMap result)

mapsRemove :: TTerm TestGroup
mapsRemove = subgroup "remove" [
  test "remove existing" 2 [(1, "a"), (2, "b"), (3, "c")] [(1, "a"), (3, "c")],
  test "remove non-existing" 4 [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
  test "remove from empty" 1 [] []]
  where
    test name k m result = primCase name _maps_delete [int32 k, intStringMap m] (intStringMap result)

mapsLookup :: TTerm TestGroup
mapsLookup = subgroup "lookup" [
  test "find existing key" 2 [(1, "a"), (2, "b")] (Just "b"),
  test "key not found" 3 [(1, "a"), (2, "b")] Nothing,
  test "lookup in empty" 1 [] Nothing]
  where
    test name k m result = primCase name _maps_lookup [int32 k, intStringMap m] (optionalString result)

mapsMember :: TTerm TestGroup
mapsMember = subgroup "member" [
  test "key exists" 2 [(1, "a"), (2, "b")] true,
  test "key missing" 3 [(1, "a"), (2, "b")] false,
  test "empty map" 1 [] false]
  where
    test name k m result = primCase name _maps_member [int32 k, intStringMap m] result

mapsSize :: TTerm TestGroup
mapsSize = subgroup "size" [
  test "three entries" [(1, "a"), (2, "b"), (3, "c")] 3,
  test "single entry" [(42, "test")] 1,
  test "empty map" [] 0]
  where
    test name m result = primCase name _maps_size [intStringMapOrEmpty m] (int32 result)

mapsNull :: TTerm TestGroup
mapsNull = subgroup "null" [
  test "empty map" [] true,
  test "non-empty map" [(1, "a")] false]
  where
    test name m result = primCase name _maps_null [intStringMapOrEmpty m] result

mapsKeys :: TTerm TestGroup
mapsKeys = subgroup "keys" [
  test "get all keys" [(1, "a"), (2, "b"), (3, "c")] [1, 2, 3],
  test "empty map" [] []]
  where
    test name m result = primCase name _maps_keys [intStringMapOrEmpty m] (list $ Prelude.map int32 result)

mapsElems :: TTerm TestGroup
mapsElems = subgroup "elems" [
  test "get all elements" [(1, "a"), (2, "b")] ["a", "b"],
  test "empty map" [] []]
  where
    test name m result = primCase name _maps_elems [intStringMapOrEmpty m] (list $ Prelude.map MetaTerms.string result)

mapsMap :: TTerm TestGroup
mapsMap = subgroup "map" [
  test "map over values" [(1, "a"), (2, "b")] [(1, "A"), (2, "B")],
  test "map empty" [] []]
  where
    test name m result = primCase name _maps_map [
      lambda "s" (primitive _strings_toUpper @@ var "s"),
      intStringMapOrEmpty m] (intStringMapOrEmpty result)

mapsFindWithDefault :: TTerm TestGroup
mapsFindWithDefault = subgroup "findWithDefault" [
  test "find existing" "default" 2 [(1, "a"), (2, "b")] "b",
  test "use default" "default" 3 [(1, "a"), (2, "b")] "default"]
  where
    test name def k m result = primCase name _maps_findWithDefault [
      MetaTerms.string def, int32 k, intStringMap m] (MetaTerms.string result)

allTests :: TBinding TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.lib.maps primitives" $
    supergroup "hydra.lib.maps primitives" [
      mapsEmpty,
      mapsSingleton,
      mapsFromList,
      mapsToList,
      mapsInsert,
      mapsRemove,
      mapsLookup,
      mapsMember,
      mapsSize,
      mapsNull,
      mapsKeys,
      mapsElems,
      mapsMap,
      mapsFindWithDefault]
