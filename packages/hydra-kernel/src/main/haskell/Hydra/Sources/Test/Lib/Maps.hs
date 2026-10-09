{-# LANGUAGE FlexibleContexts #-}

module Hydra.Sources.Test.Lib.Maps where

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

-- Additional imports specific to this file
import Hydra.Core.Testing
import qualified Hydra.Core.Dsl.Lib.Equality as Equality
import qualified Hydra.Core.Dsl.Lib.Ordering as Ordering
import qualified Hydra.Core.Dsl.Lib.Literals as Literals
import qualified Hydra.Core.Dsl.Lib.Maps as Maps
import qualified Hydra.Core.Dsl.Lib.Math as Math
import qualified Hydra.Core.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Core.Dsl.Lib.Strings as Strings
import qualified Hydra.Core.Dsl.Lib.Chars as Chars
import qualified Hydra.Core.Lib.Maps as DefMaps
import qualified Hydra.Sources.Kernel.Terms.Print.Core as PrintCore


ns :: ModuleName
ns = ModuleName "hydra.core.test.lib.maps"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = definitions,
            moduleDependencies = unqualifiedDep <$> ([ModuleName "hydra.core.reduction", PrintCore.ns] ++ kernelTypesModuleNames),
            moduleMetadata = descriptionMetadata (Just "Test cases for hydra.core.lib.maps primitives")}
  where
    definitions = [Phantoms.toDefinition allTests]


-- Phantom-typed map helper
pMap :: [(Int, String)] -> TypedTerm (M.Map Int String)
pMap pairs = Phantoms.map $ M.fromList $ fmap (\(k, v) -> (Phantoms.int32 k, Phantoms.string v)) pairs

pPairList :: [(Int, String)] -> TypedTerm [(Int, String)]
pPairList pairs = Phantoms.list $ fmap (\(k, v) -> Phantoms.pair (Phantoms.int32 k) (Phantoms.string v)) pairs

showBool :: TypedTerm (Bool -> String)
showBool = Phantoms.lambda "b" $ Literals.printBoolean (Phantoms.var "b")

showInt32 :: TypedTerm (Int -> String)
showInt32 = Phantoms.lambda "n" $ Literals.printInt32 (Phantoms.var "n")

showIntList :: TypedTerm ([Int] -> String)
showIntList = Phantoms.lambda "xs" $ PrintCore.list_ @@ showInt32 @@ Phantoms.var "xs"

showIntStringMap :: TypedTerm (M.Map Int String -> String)
showIntStringMap = Phantoms.lambda "m" $ PrintCore.map_ @@ showInt32 @@ showString' @@ Phantoms.var "m"

showMaybeString :: TypedTerm (Maybe String -> String)
showMaybeString = Phantoms.lambda "mx" $ PrintCore.optional_ @@ showString' @@ Phantoms.var "mx"

showPairList :: TypedTerm ([(Int, String)] -> String)
showPairList = Phantoms.lambda "xs" $ PrintCore.list_ @@ (Phantoms.lambda "p" $ PrintCore.pair_ @@ showInt32 @@ showString' @@ Phantoms.var "p") @@ Phantoms.var "xs"

showString' :: TypedTerm (String -> String)
showString' = Phantoms.lambda "s" $ Literals.printString (Phantoms.var "s")

showStringList :: TypedTerm ([String] -> String)
showStringList = Phantoms.lambda "xs" $ PrintCore.list_ @@ showString' @@ Phantoms.var "xs"

-- Test groups

allTests :: TypedTermDefinition TestGroup
allTests = definitionInModule module_ "allTests" $
    Phantoms.doc "Test cases for hydra.core.lib.maps primitives" $
    supergroup "hydra.core.lib.maps primitives" [
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
      mapsIsEmpty,
      mapsRemove,
      mapsSingleton,
      mapsSize,
      mapsToList,
      mapsUnion]

mapsAlter :: TypedTerm TestGroup
mapsAlter = subgroup "insertsUpdatesOrDeletes" [
  testInsert "insert new key" 3 [(1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "new")],
  testUpdate "update existing key" 2 [(1, "a"), (2, "b")] [(1, "a"), (2, "updated")],
  testDelete "delete key" 2 [(1, "a"), (2, "b")] [(1, "a")]]
  where
    testInsert name k m result = evalPairWithProvisions name ["insertsUpdatesOrDeletes"] DefMaps.alter showIntStringMap
      (Maps.alter (Phantoms.lambda "_" $ Phantoms.just (Phantoms.string "new")) (Phantoms.int32 k) (pMap m))
      (pMap result)
    testUpdate name k m result = evalPairWithProvisions name ["insertsUpdatesOrDeletes"] DefMaps.alter showIntStringMap
      (Maps.alter (Phantoms.lambda "_" $ Phantoms.just (Phantoms.string "updated")) (Phantoms.int32 k) (pMap m))
      (pMap result)
    testDelete name k m result = evalPairWithProvisions name ["insertsUpdatesOrDeletes"] DefMaps.alter showIntStringMap
      (Maps.alter (Phantoms.lambda "_" $ (Phantoms.nothing :: TypedTerm (Maybe String))) (Phantoms.int32 k) (pMap m))
      (pMap result)

mapsBimap :: TypedTerm TestGroup
mapsBimap = supergroup "bimap" [
  subgroup "transformsKeysAndValues" [
    test "transform both" "transformsKeysAndValues" [(1, "a"), (2, "b")] [(2, "A"), (4, "B")]],
  subgroup "emptyInput" [
    test "empty map" "emptyInput" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.bimap showIntStringMap
      (Maps.bimap
        (Phantoms.lambda "k" $ Math.mul (Phantoms.var "k") (Phantoms.int32 2))
        (Phantoms.lambda "v" $ Strings.toUpper (Phantoms.var "v"))
        (pMap m))
      (pMap result)

mapsElems :: TypedTerm TestGroup
mapsElems = supergroup "elems" [
  subgroup "returnsValuesInKeyOrder" [
    test "get all elements" "returnsValuesInKeyOrder" [(1, "a"), (2, "b")] ["a", "b"],
    test "unsorted keys" "returnsValuesInKeyOrder" [(3, "c"), (1, "a"), (2, "b")] ["a", "b", "c"]],
  subgroup "emptyInput" [
    test "empty map" "emptyInput" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.elems showStringList
      (Maps.elems (pMap m))
      (Phantoms.list $ Phantoms.string <$> result)

mapsEmpty :: TypedTerm TestGroup
mapsEmpty = subgroup "hasNoBindings" [
  test "empty map" "hasNoBindings"]
  where
    test name segment = evalPairWithProvisions name [segment] DefMaps.empty showIntStringMap
      (Maps.empty :: TypedTerm (M.Map Int String))
      (pMap [])

mapsFilter :: TypedTerm TestGroup
mapsFilter = supergroup "filter" [
  subgroup "keepsMatchingValues" [
    test "filter values starting with a" "keepsMatchingValues" [(1, "a"), (2, "b"), (3, "ab")] [(1, "a"), (3, "ab")]],
  subgroup "boundaryCases" [
    test "filter all" "boundaryCases" [(1, "b"), (2, "c")] [],
    test "empty map" "boundaryCases" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.filter showIntStringMap
      (Maps.filter
        (Phantoms.lambda "v" $ Equality.equal
          (Optionals.withDefault (Phantoms.int32 0)
            (Optionals.map (Phantoms.lambda "c" $ Chars.toLower (Phantoms.var "c"))
              (Strings.charAt (Phantoms.int32 0) (Phantoms.var "v"))))
          (Phantoms.int32 97))
        (pMap m))
      (pMap result)

mapsFilterWithKey :: TypedTerm TestGroup
mapsFilterWithKey = supergroup "filterWithKey" [
  subgroup "keepsMatchingBindings" [
    test "filter by key > 1" "keepsMatchingBindings" [(1, "a"), (2, "b"), (3, "c")] [(2, "b"), (3, "c")]],
  subgroup "boundaryCases" [
    test "filter all" "boundaryCases" [(1, "a")] [],
    test "empty map" "boundaryCases" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.filterWithKey showIntStringMap
      (Maps.filterWithKey (Phantoms.lambda "k" $ Phantoms.lambda "v" $ Ordering.gt (Phantoms.var "k") (Phantoms.int32 1)) (pMap m))
      (pMap result)

mapsFindWithDefault :: TypedTerm TestGroup
mapsFindWithDefault = subgroup "returnsBoundValueOrDefault" [
  test "find existing" "default" 2 [(1, "a"), (2, "b")] "b",
  test "use default" "default" 3 [(1, "a"), (2, "b")] "default"]
  where
    test name def k m result = stringEvalPairWithProvisions name ["returnsBoundValueOrDefault"] DefMaps.findWithDefault
      (Maps.findWithDefault (Phantoms.string def) (Phantoms.int32 k) (pMap m))
      (Phantoms.string result)

mapsFromList :: TypedTerm TestGroup
mapsFromList = supergroup "fromList" [
  subgroup "buildsMapFromPairs" [
    test "create from pairs" "buildsMapFromPairs" [(1, "a"), (2, "b")] [(1, "a"), (2, "b")]],
  subgroup "lastDuplicateWins" [
    test "duplicate keys" "lastDuplicateWins" [(1, "a"), (1, "b")] [(1, "b")]],
  subgroup "emptyInput" [
    test "empty list" "emptyInput" [] []]]
  where
    test name segment input expected = evalPairWithProvisions name [segment] DefMaps.fromList showIntStringMap
      (Maps.fromList (pPairList input))
      (pMap expected)

mapsInsert :: TypedTerm TestGroup
mapsInsert = subgroup "addsOrOverwritesBinding" [
  test "insert new key" 3 "c" [(1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "c")],
  test "update existing" 2 "updated" [(1, "a"), (2, "b")] [(1, "a"), (2, "updated")],
  test "insert into empty" 1 "x" [] [(1, "x")]]
  where
    test name k v m result = evalPairWithProvisions name ["addsOrOverwritesBinding"] DefMaps.insert showIntStringMap
      (Maps.insert (Phantoms.int32 k) (Phantoms.string v) (pMap m))
      (pMap result)

mapsKeys :: TypedTerm TestGroup
mapsKeys = supergroup "keys" [
  subgroup "returnsKeysInOrder" [
    test "get all keys" "returnsKeysInOrder" [(1, "a"), (2, "b"), (3, "c")] [1, 2, 3],
    test "unsorted keys" "returnsKeysInOrder" [(3, "c"), (1, "a"), (2, "b")] [1, 2, 3]],
  subgroup "emptyInput" [
    test "empty map" "emptyInput" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.keys showIntList
      (Maps.keys (pMap m))
      (Phantoms.list $ Phantoms.int32 <$> result)

mapsLookup :: TypedTerm TestGroup
mapsLookup = supergroup "lookup" [
  subgroup "returnsBoundValue" [
    test "find existing key" "returnsBoundValue" 2 [(1, "a"), (2, "b")] (Just "b")],
  subgroup "noneWhenAbsentOrEmpty" [
    test "key not found" "noneWhenAbsentOrEmpty" 3 [(1, "a"), (2, "b")] Nothing,
    test "lookup in empty" "noneWhenAbsentOrEmpty" 1 [] Nothing]]
  where
    test name segment k m result = evalPairWithProvisions name [segment] DefMaps.lookup showMaybeString
      (Maps.lookup (Phantoms.int32 k) (pMap m))
      (maybe Phantoms.nothing (Phantoms.just . Phantoms.string) result)

mapsMap :: TypedTerm TestGroup
mapsMap = supergroup "map" [
  subgroup "appliesFunctionToValues" [
    test "map over values" "appliesFunctionToValues" [(1, "a"), (2, "b")] [(1, "A"), (2, "B")]],
  subgroup "emptyInput" [
    test "map empty" "emptyInput" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.map showIntStringMap
      (Maps.map (Phantoms.lambda "s" $ Strings.toUpper (Phantoms.var "s")) (pMap m))
      (pMap result)

mapsMapKeys :: TypedTerm TestGroup
mapsMapKeys = supergroup "mapKeys" [
  subgroup "appliesFunctionToKeys" [
    test "double keys" "appliesFunctionToKeys" [(1, "a"), (2, "b")] [(2, "a"), (4, "b")]],
  subgroup "emptyInput" [
    test "empty map" "emptyInput" [] []]]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.mapKeys showIntStringMap
      (Maps.mapKeys (Phantoms.lambda "k" $ Math.mul (Phantoms.var "k") (Phantoms.int32 2)) (pMap m))
      (pMap result)

mapsMember :: TypedTerm TestGroup
mapsMember = supergroup "member" [
  subgroup "testsKeyPresence" [
    test "key exists" "testsKeyPresence" 2 [(1, "a"), (2, "b")] True],
  subgroup "falseWhenAbsentOrEmpty" [
    test "key missing" "falseWhenAbsentOrEmpty" 3 [(1, "a"), (2, "b")] False,
    test "empty map" "falseWhenAbsentOrEmpty" 1 [] False]]
  where
    test name segment k m result = evalPairWithProvisions name [segment] DefMaps.member showBool
      (Maps.member (Phantoms.int32 k) (pMap m))
      (Phantoms.boolean result)

mapsIsEmpty :: TypedTerm TestGroup
mapsIsEmpty = subgroup "testsZeroBindings" [
  test "empty map" "testsZeroBindings" [] True,
  test "non-empty map" "testsZeroBindings" [(1, "a")] False]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.isEmpty showBool
      (Maps.isEmpty (pMap m))
      (Phantoms.boolean result)

mapsRemove :: TypedTerm TestGroup
mapsRemove = supergroup "remove" [
  subgroup "removesKeyIfPresent" [
    test "remove existing" "removesKeyIfPresent" 2 [(1, "a"), (2, "b"), (3, "c")] [(1, "a"), (3, "c")]],
  subgroup "noOpWhenAbsentOrEmpty" [
    test "remove non-existing" "noOpWhenAbsentOrEmpty" 4 [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
    test "remove from empty" "noOpWhenAbsentOrEmpty" 1 [] []]]
  where
    test name segment k m result = evalPairWithProvisions name [segment] DefMaps.delete showIntStringMap
      (Maps.delete (Phantoms.int32 k) (pMap m))
      (pMap result)

mapsSingleton :: TypedTerm TestGroup
mapsSingleton = subgroup "wrapsOneBinding" [
  test "single entry" "wrapsOneBinding" 42 "hello" [(42, "hello")]]
  where
    test name segment k v result = evalPairWithProvisions name [segment] DefMaps.singleton showIntStringMap
      (Maps.singleton (Phantoms.int32 k) (Phantoms.string v))
      (pMap result)

mapsSize :: TypedTerm TestGroup
mapsSize = subgroup "countsBindings" [
  test "three entries" "countsBindings" [(1, "a"), (2, "b"), (3, "c")] 3,
  test "single entry" "countsBindings" [(42, "test")] 1,
  test "empty map" "countsBindings" [] 0]
  where
    test name segment m result = evalPairWithProvisions name [segment] DefMaps.size showInt32
      (Maps.size (pMap m))
      (Phantoms.int32 result)

mapsToList :: TypedTerm TestGroup
mapsToList = supergroup "toList" [
  subgroup "returnsPairsInKeyOrder" [
    test "convert to pairs" "returnsPairsInKeyOrder" [(1, "a"), (2, "b")] [(1, "a"), (2, "b")],
    test "unsorted keys" "returnsPairsInKeyOrder" [(3, "c"), (1, "a"), (2, "b")] [(1, "a"), (2, "b"), (3, "c")]],
  subgroup "emptyInput" [
    test "empty map" "emptyInput" [] []]]
  where
    test name segment input expected = evalPairWithProvisions name [segment] DefMaps.toList showPairList
      (Maps.toList (pMap input))
      (pPairList expected)

mapsUnion :: TypedTerm TestGroup
mapsUnion = supergroup "union" [
  subgroup "firstMapTakesPrecedence" [
    test "union two maps" "firstMapTakesPrecedence" [(1, "a"), (2, "b")] [(2, "x"), (3, "c")] [(1, "a"), (2, "b"), (3, "c")]],
  subgroup "identityWithEmpty" [
    test "union with empty" "identityWithEmpty" [(1, "a")] [] [(1, "a")],
    test "empty with map" "identityWithEmpty" [] [(1, "a")] [(1, "a")]]]
  where
    test name segment m1 m2 result = evalPairWithProvisions name [segment] DefMaps.union showIntStringMap
      (Maps.union (pMap m1) (pMap m2))
      (pMap result)
