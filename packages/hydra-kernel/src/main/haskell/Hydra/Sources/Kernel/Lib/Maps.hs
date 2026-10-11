-- | Primitive declarations for the hydra.core.lib.maps namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Maps where

import Hydra.Kernel
import qualified Hydra.Core.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Core.Dsl.Lib.Lists    as Lists
import qualified Hydra.Core.Dsl.Lib.Maps     as Maps
import qualified Hydra.Core.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Core.Dsl.Lib.Pairs    as Pairs
import           Hydra.Core.Overlay.Haskell.Dsl.Phantoms     as Phantoms hiding (map)
import qualified Hydra.Core.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), filter, lookup, map)
import qualified Data.Map                    as M


ns :: ModuleName
ns = ModuleName "hydra.core.lib.maps"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = descriptionMetadataWithComments (Just "Primitives in the hydra.core.lib.maps module.") [
              "Ordered maps: finite collections of key-value bindings, with at most one value per key.",
              "Hydra maps are ordered collections: iteration -- elems, keys, toList -- is in ascending key\
              \ order, under the total order defined in the ordering-and-equality specification.",
              "Because key ordering is part of the map type's contract, every map-typed variable in this\
              \ module's signatures carries the ordering constraint on its key type, including in primitives\
              \ that perform no comparison themselves."]}
  where
    -- The default-impl args below carry an ':: ...Int...' placeholder: the generated 'Hydra.Dsl.Lib.*'
    -- (unlike old 'Meta.Lib.*') exposes the primitive's Ord constraint, so these polymorphic defs need a
    -- concrete type here to satisfy GHC. 'Int' is arbitrary and carries no meaning — the emitted primitive
    -- is type-agnostic and fully polymorphic. See #467.
    definitions = [alter, bimap, delete, difference, elems, empty, filter, filterWithKey,
                   findWithDefault, fromList, insert, intersection, isEmpty, keys, lookup, map,
                   mapKeys, member, singleton, size, toList, union, unions]

define :: String -> String -> TermSignature -> [String] -> PrimitiveDefinition
define = primitiveInModule module_

defineWithDefault :: String -> String -> TermSignature -> [String] -> TypedTerm a -> PrimitiveDefinition
defineWithDefault = primitiveWithDefaultInModule module_

-- Type-var shortcuts
mp :: Type -> Type -> Type
mp = Types.map

tk, tk1, tk2, tv, tv1, tv2 :: Type
tk  = Types.var "k"
tk1 = Types.var "k1"
tk2 = Types.var "k2"
tv  = Types.var "v"
tv1 = Types.var "v1"
tv2 = Types.var "v2"

-- Ordering-constrained key signature helper (with authored parameter names/descriptions)
ordKey :: [(String, String)] -> [(String, [Name])] -> Type -> TermSignature
ordKey params extra body = sigWithParams params $ Types.polyConstrained (("k", [Name "ordering"]) : extra) body

alter :: PrimitiveDefinition
alter = availableSince "0.15" $ withProvisions [
  requirement "insertsUpdatesOrDeletes"
    "alter applies the given function to the optional current value at the key: if the function\
    \ returns given(v'), that binding is set (inserting if absent, overwriting if present); if it\
    \ returns none, the key is removed."]
  $ defineWithDefault "alter" "Alter a value at a key using a function which sees the optional current value."
  (sigWithParams
    [("f", "the update function, applied to the optional current value at the key"),
     ("k", "the key whose binding to alter"),
     ("m", "the map to update")] $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
    ((Types.optional tv Types.~> Types.optional tv) Types.~> tk Types.~> mp tk tv Types.~> mp tk tv))
  ["alter(f, k, m) applies f to Just(v) when m contains key k with value v, or to Nothing when k is\
  \ absent. If f returns Just(v'), the binding (k, v') is set in the result; if f returns Nothing, k\
  \ is removed from the result.",
   "A single primitive that subsumes insert, delete, and adjust.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v ->\
  \ Map k v."]
  (("f" ~> "k" ~> "m" ~>
    Optionals.match
      (var "f" @@ Maps.lookup (var "k" :: TypedTerm Int) (var "m"))
      (Maps.delete (var "k" :: TypedTerm Int) (var "m"))
      ("vNew" ~> Maps.insert (var "k" :: TypedTerm Int) (var "vNew") (var "m")))
    :: TypedTerm ((Maybe v -> Maybe v) -> Int -> M.Map Int v -> M.Map Int v))

bimap :: PrimitiveDefinition
bimap = availableSince "0.15" $ withProvisions [
  requirement "transformsKeysAndValues"
    "bimap returns a new map where each binding (k, v) becomes (fk(k), fv(v)), applying the key\
    \ function and value function independently to every binding.",
  requirement "emptyInput"
    "bimap of the empty map is the empty map, regardless of the key and value functions."]
  $ defineWithDefault "bimap" "Map functions over both the keys and values of a map."
  (sigWithParams
    [("fk", "the function to apply to each key"),
     ("fv", "the function to apply to each value"),
     ("m", "the map to transform")] $ Types.polyConstrained
    [("k1", [Name "ordering"]), ("k2", [Name "ordering"]), ("v1", []), ("v2", [])]
    ((tk1 Types.~> tk2) Types.~> (tv1 Types.~> tv2) Types.~> mp tk1 tv1 Types.~> mp tk2 tv2))
  ["bimap(fk, fv, m) returns a new map with key fk(k) and value fv(v) for each binding (k, v) in m.",
   "Key collisions after applying fk are resolved by keeping the last binding encountered (host may\
  \ differ on collision policy if fk is not injective).",
   "Requires 'ordering' constraints on both the input and output key types.",
   "Total. Corresponds to a key-and-value lift of Haskell's Data.Map.fromList . map (\\(k,v) ->\
  \ (fk k, fv v)) . toList."]
  (("fk" ~> "fv" ~> "m" ~>
    ((Maps.fromList $ Lists.map
      ("p" ~> pair (var "fk" @@ Pairs.first (var "p")) (var "fv" @@ Pairs.second (var "p")))
      (Maps.toList (var "m" :: TypedTerm (M.Map Int v1)))) :: TypedTerm (M.Map Int v2)))
    :: TypedTerm ((Int -> Int) -> (v1 -> v2) -> M.Map Int v1 -> M.Map Int v2))

delete :: PrimitiveDefinition
delete = availableSince "0.15" $ withProvisions [
  requirement "removesKeyIfPresent"
    "delete removes the binding for the given key if present, leaving the rest of the map\
    \ unchanged.",
  requirement "noOpWhenAbsentOrEmpty"
    "delete leaves the map unchanged when the key is absent, including when the map is empty."]
  $ define "delete" "Remove a key from a map."
  (ordKey [("k", "the key to remove"), ("m", "the map to remove the key from")]
    [("v", [])] (tk Types.~> mp tk tv Types.~> mp tk tv))
  ["delete(k, m) returns m with the binding for k removed; if k is not present, m is returned\
  \ unchanged.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.delete :: Ord k => k -> Map k v -> Map k v."]

difference :: PrimitiveDefinition
difference = availableSince "0.17.6" $ withProvisions [
  requirement "removesBindingsWithKeysInSecondMap"
    "difference returns the bindings of the first map whose keys do not occur in the second map.",
  requirement "identityWhenSecondEmpty"
    "difference with an empty second map returns the first map unchanged."]
  $ define "difference" "Compute the difference of two maps by key."
  (ordKey [("m1", "the map whose bindings are retained"), ("m2", "the map whose keys are subtracted")]
    [("v", [])] (mp tk tv Types.~> mp tk tv Types.~> mp tk tv))
  ["difference(m1, m2) returns the map containing exactly the bindings of m1 whose keys do not appear\
  \ in m2. Only the key set of m2 matters; its values are ignored.",
   "The key-set analogue is hydra.core.lib.sets.difference.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.difference :: Ord k => Map k v -> Map k v -> Map k v."]

elems :: PrimitiveDefinition
elems = availableSince "0.15" $ withProvisions [
  requirement "returnsValuesInKeyOrder"
    "elems returns the map's values as a list ordered by ascending key, including when keys were\
    \ inserted out of order.",
  requirement "emptyInput"
    "elems of the empty map is the empty list."]
  $ define "elems" "Return the values of a map (in key order)."
  (ordKey [("m", "the map whose values to return")]
    [("v", [])] (mp tk tv Types.~> Types.list tv))
  ["elems(m) returns the values of m as a list, ordered by their keys' ascending order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.elems :: Map k v -> [v]."]

empty :: PrimitiveDefinition
empty = availableSince "0.15" $ withProvisions [
  requirement "hasNoBindings"
    "empty is the map containing no bindings."]
  $ define "empty" "The empty map."
  (sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])] (mp tk tv))
  ["empty is the map with no bindings.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.empty :: Map k v."]

filter :: PrimitiveDefinition
filter = availableSince "0.15" $ withProvisions [
  requirement "keepsMatchingValues"
    "filter returns the submap containing exactly the bindings whose value satisfies the\
    \ predicate, preserving all matching keys.",
  requirement "boundaryCases"
    "filter degrades correctly at the boundaries: a predicate matching no value yields the\
    \ empty map; filtering the empty map yields the empty map."]
  $ defineWithDefault "filter" "Filter a map by value."
  (sigWithParams
    [("p", "the predicate to test each value"),
     ("m", "the map to filter")] $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
    ((tv Types.~> Types.boolean) Types.~> mp tk tv Types.~> mp tk tv))
  ["filter(p, m) returns the submap of m containing exactly the bindings (k, v) for which p(v) is\
  \ true.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.filter :: (v -> Bool) -> Map k v -> Map k v."]
  (("p" ~> "m" ~>
    ((Maps.fromList $ Lists.filter
      ("pr" ~> var "p" @@ Pairs.second (var "pr"))
      (Maps.toList (var "m" :: TypedTerm (M.Map Int v)))) :: TypedTerm (M.Map Int v)))
    :: TypedTerm ((v -> Bool) -> M.Map Int v -> M.Map Int v))

filterWithKey :: PrimitiveDefinition
filterWithKey = availableSince "0.15" $ withProvisions [
  requirement "keepsMatchingBindings"
    "filterWithKey returns the submap containing exactly the bindings for which the predicate,\
    \ given both key and value, is true.",
  requirement "boundaryCases"
    "filterWithKey degrades correctly at the boundaries: a predicate matching no binding yields\
    \ the empty map; filtering the empty map yields the empty map."]
  $ defineWithDefault "filterWithKey" "Filter a map by key and value."
  (sigWithParams
    [("p", "the predicate to test each key and value"),
     ("m", "the map to filter")] $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
    ((tk Types.~> tv Types.~> Types.boolean) Types.~> mp tk tv Types.~> mp tk tv))
  ["filterWithKey(p, m) returns the submap of m containing exactly the bindings (k, v) for which\
  \ p(k, v) is true.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.filterWithKey :: (k -> v -> Bool) -> Map k v -> Map k v."]
  (("p" ~> "m" ~>
    ((Maps.fromList $ Lists.filter
      ("pr" ~> var "p" @@ Pairs.first (var "pr") @@ Pairs.second (var "pr"))
      (Maps.toList (var "m" :: TypedTerm (M.Map Int v)))) :: TypedTerm (M.Map Int v)))
    :: TypedTerm ((Int -> v -> Bool) -> M.Map Int v -> M.Map Int v))

-- The default value (position 0) is lazy: it is only evaluated when the key is absent.
findWithDefault :: PrimitiveDefinition
findWithDefault = availableSince "0.15" $ withProvisions [
  requirement "returnsBoundValueOrDefault"
    "findWithDefault returns the value bound to the key when present, or the supplied default\
    \ value when the key is absent."]
  $ defineWithDefault "findWithDefault" "Look up a value with a default if the key is absent."
  (markLazyParams [0] $ sigWithParams
    [("def", "the default value, returned when the key is absent"),
     ("k", "the key to look up"),
     ("m", "the map to search")] $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
    (tv Types.~> tk Types.~> mp tk tv Types.~> tv))
  ["findWithDefault(def, k, m) returns the value bound to k in m if k is present, or def otherwise.\
  \ Equivalent to maybe(def, identity, lookup(k, m)).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.findWithDefault :: Ord k => v -> k -> Map k v -> v."]
  (("def" ~> "k" ~> "m" ~>
    Optionals.withDefault (var "def") (Maps.lookup (var "k" :: TypedTerm Int) (var "m")))
    :: TypedTerm (v -> Int -> M.Map Int v -> v))

fromList :: PrimitiveDefinition
fromList = availableSince "0.15" $ withProvisions [
  requirement "buildsMapFromPairs"
    "fromList returns the map containing exactly the bindings in the input list of key-value\
    \ pairs.",
  requirement "lastDuplicateWins"
    "fromList resolves multiple entries for the same key by keeping the value from the last\
    \ occurrence in the input list.",
  requirement "emptyInput"
    "fromList of the empty list of pairs is the empty map."]
  $ define "fromList" "Build a map from a list of key-value pairs."
  (ordKey [("xs", "the list of key-value pairs to build the map from")]
    [("v", [])] (Types.list (Types.pair tk tv) Types.~> mp tk tv))
  ["fromList(xs) returns the map containing exactly the bindings in xs. If xs contains multiple\
  \ entries for the same key, the last one wins (matching Haskell's fromList behavior).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.fromList :: Ord k => [(k, v)] -> Map k v."]

insert :: PrimitiveDefinition
insert = availableSince "0.15" $ withProvisions [
  requirement "addsOrOverwritesBinding"
    "insert adds the given key-value binding to the map, overwriting any existing value at that\
    \ key, including when the map was empty."]
  $ define "insert" "Insert a key-value pair into a map."
  (ordKey [("k", "the key to insert"), ("v", "the value to bind to the key"), ("m", "the map to insert into")]
    [("v", [])] (tk Types.~> tv Types.~> mp tk tv Types.~> mp tk tv))
  ["insert(k, v, m) returns m with the binding (k, v) added or updated. If k is already present, its\
  \ value is overwritten.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.insert :: Ord k => k -> v -> Map k v -> Map k v."]

intersection :: PrimitiveDefinition
intersection = availableSince "0.17.6" $ withProvisions [
  requirement "keepsBindingsWithKeysInBoth"
    "intersection returns the bindings of the first map whose keys also occur in the second map,\
    \ taking the value from the first map on each common key.",
  requirement "emptyWhenNoOverlapOrEitherEmpty"
    "intersection is the empty map when the two maps share no keys, or when either map is empty."]
  $ define "intersection" "Compute the intersection of two maps by key."
  (ordKey [("m1", "the map whose bindings are retained"), ("m2", "the map whose key set is intersected")]
    [("v", [])] (mp tk tv Types.~> mp tk tv Types.~> mp tk tv))
  ["intersection(m1, m2) returns the map containing exactly the bindings of m1 whose keys also appear\
  \ in m2. On each common key the value is taken from m1; only the key set of m2 matters, and its\
  \ values are ignored.",
   "The key-set analogue is hydra.core.lib.sets.intersection.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.intersection :: Ord k => Map k v -> Map k v -> Map k v."]

isEmpty :: PrimitiveDefinition
isEmpty = availableSince "0.15" $ withProvisions [
  requirement "testsZeroBindings"
    "isEmpty returns true exactly when the map has no bindings."]
  $ define "isEmpty" "Test whether a map is empty."
  (ordKey [("m", "the map to test")]
    [("v", [])] (mp tk tv Types.~> Types.boolean))
  ["isEmpty(m) returns true iff m has no bindings.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.null :: Map k v -> Bool."]

keys :: PrimitiveDefinition
keys = availableSince "0.15" $ withProvisions [
  requirement "returnsKeysInOrder"
    "keys returns the map's keys as a list in ascending order, including when keys were\
    \ inserted out of order.",
  requirement "emptyInput"
    "keys of the empty map is the empty list."]
  $ define "keys" "Return the keys of a map (in key order)."
  (ordKey [("m", "the map whose keys to return")]
    [("v", [])] (mp tk tv Types.~> Types.list tk))
  ["keys(m) returns the keys of m as a list, in ascending order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.keys :: Map k v -> [k]."]

lookup :: PrimitiveDefinition
lookup = availableSince "0.15" $ withProvisions [
  requirement "returnsBoundValue"
    "lookup returns the value bound to the given key, wrapped, when the key is present.",
  requirement "noneWhenAbsentOrEmpty"
    "lookup returns none when the key is absent, including when the map is empty."]
  $ define "lookup" "Look up a value in a map by key, returning Nothing if absent."
  (ordKey [("k", "the key to look up"), ("m", "the map to search")]
    [("v", [])] (tk Types.~> mp tk tv Types.~> Types.optional tv))
  ["lookup(k, m) returns Just(v) where v is the value bound to k in m, or Nothing if k is not\
  \ present.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.lookup :: Ord k => k -> Map k v -> Maybe v."]

map :: PrimitiveDefinition
map = availableSince "0.15" $ withProvisions [
  requirement "appliesFunctionToValues"
    "map applies the given function to every value in the map, leaving keys unchanged.",
  requirement "emptyInput"
    "map over the empty map is the empty map, regardless of the function."]
  $ defineWithDefault "map" "Map a function over the values of a map."
  (sigWithParams
    [("f", "the function to apply to each value"),
     ("m", "the map to transform")] $ Types.polyConstrained [("v1", []), ("v2", []), ("k", [Name "ordering"])]
    ((tv1 Types.~> tv2) Types.~> mp tk tv1 Types.~> mp tk tv2))
  ["map(f, m) returns a map with the same keys as m and value f(v) for each binding (k, v).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.map :: (v -> w) -> Map k v -> Map k w / fmap on Map."]
  (("f" ~> "m" ~>
    ((Maps.fromList $ Lists.map
      ("p" ~> pair (Pairs.first (var "p")) (var "f" @@ Pairs.second (var "p")))
      (Maps.toList (var "m" :: TypedTerm (M.Map Int v1)))) :: TypedTerm (M.Map Int v2)))
    :: TypedTerm ((v1 -> v2) -> M.Map Int v1 -> M.Map Int v2))

mapKeys :: PrimitiveDefinition
mapKeys = availableSince "0.15" $ withProvisions [
  requirement "appliesFunctionToKeys"
    "mapKeys applies the given function to every key in the map, leaving the bound values\
    \ unchanged.",
  requirement "emptyInput"
    "mapKeys over the empty map is the empty map, regardless of the function."]
  $ defineWithDefault "mapKeys" "Map a function over the keys of a map."
  (sigWithParams
    [("f", "the function to apply to each key"),
     ("m", "the map to transform")] $ Types.polyConstrained
    [("k1", [Name "ordering"]), ("k2", [Name "ordering"]), ("v", [])]
    ((tk1 Types.~> tk2) Types.~> mp tk1 tv Types.~> mp tk2 tv))
  ["mapKeys(f, m) returns a map where each binding (k, v) becomes (f(k), v). If f maps multiple keys\
  \ to the same image, key collisions are resolved by keeping the binding with the greater original\
  \ key (matching Haskell's mapKeys behavior).",
   "Requires 'ordering' constraints on both the input and output key types.",
   "Total. Corresponds to Haskell's Data.Map.mapKeys :: (Ord k1, Ord k2) => (k1 -> k2) -> Map k1 v ->\
  \ Map k2 v."]
  (("f" ~> "m" ~>
    ((Maps.fromList $ Lists.map
      ("p" ~> pair (var "f" @@ Pairs.first (var "p")) (Pairs.second (var "p")))
      (Maps.toList (var "m" :: TypedTerm (M.Map Int v)))) :: TypedTerm (M.Map Int v)))
    :: TypedTerm ((Int -> Int) -> M.Map Int v -> M.Map Int v))

member :: PrimitiveDefinition
member = availableSince "0.15" $ withProvisions [
  requirement "testsKeyPresence"
    "member returns true exactly when the given key is present in the map.",
  requirement "falseWhenAbsentOrEmpty"
    "member returns false when the key is absent, including when the map is empty."]
  $ define "member" "Test whether a key is present in a map."
  (ordKey [("k", "the key to test for"), ("m", "the map to test")]
    [("v", [])] (tk Types.~> mp tk tv Types.~> Types.boolean))
  ["member(k, m) returns true iff k is a key in m.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.member :: Ord k => k -> Map k v -> Bool."]

singleton :: PrimitiveDefinition
singleton = availableSince "0.15" $ withProvisions [
  requirement "wrapsOneBinding"
    "singleton returns the map containing exactly the one given key-value binding."]
  $ define "singleton" "Construct a map with a single key-value pair."
  (ordKey [("k", "the key of the single binding"), ("v", "the value of the single binding")]
    [("v", [])] (tk Types.~> tv Types.~> mp tk tv))
  ["singleton(k, v) returns the map containing exactly the binding (k, v).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.singleton :: k -> v -> Map k v."]

size :: PrimitiveDefinition
size = availableSince "0.15" $ withProvisions [
  requirement "countsBindings"
    "size returns the number of bindings in the map, including zero for the empty map."]
  $ define "size" "Return the number of key-value pairs in a map."
  (ordKey [("m", "the map whose bindings to count")]
    [("v", [])] (mp tk tv Types.~> Types.int32))
  ["size(m) returns the number of bindings in m as an int32.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.size :: Map k v -> Int (with narrowing to int32)."]

toList :: PrimitiveDefinition
toList = availableSince "0.15" $ withProvisions [
  requirement "returnsPairsInKeyOrder"
    "toList returns the map's bindings as a list of key-value pairs ordered by ascending key,\
    \ including when keys were inserted out of order.",
  requirement "emptyInput"
    "toList of the empty map is the empty list."]
  $ define "toList" "Convert a map to a list of key-value pairs (in key order)."
  (ordKey [("m", "the map to convert")]
    [("v", [])] (mp tk tv Types.~> Types.list (Types.pair tk tv)))
  ["toList(m) returns the bindings of m as a list of (key, value) pairs, in ascending key order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.toList :: Map k v -> [(k, v)]."]

union :: PrimitiveDefinition
union = availableSince "0.15" $ withProvisions [
  requirement "firstMapTakesPrecedence"
    "union returns the map containing all bindings from the first map plus the bindings of the\
    \ second map whose keys are not in the first; on key collision the first map's binding is\
    \ kept.",
  requirement "identityWithEmpty"
    "union with either map empty returns the other map unchanged."]
  $ define "union" "Compute the union of two maps; the first map's bindings take precedence on key collision."
  (ordKey [("m1", "the map whose bindings take precedence"), ("m2", "the map whose remaining bindings are added")]
    [("v", [])] (mp tk tv Types.~> mp tk tv Types.~> mp tk tv))
  ["union(m1, m2) returns the map containing all bindings from m1 plus the bindings of m2 whose keys\
  \ are not in m1. On key collision, the binding from m1 is preferred.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.union :: Ord k => Map k v -> Map k v -> Map k v."]

unions :: PrimitiveDefinition
unions = availableSince "0.17.6" $ withProvisions [
  requirement "earliestMapTakesPrecedence"
    "unions returns the bindings of every map in the list; when a key occurs in more than one map,\
    \ the binding from the earliest such map in the list is kept.",
  requirement "emptyListOfMapsYieldsEmptyMap"
    "unions of an empty list of maps is the empty map."]
  $ define "unions" "Compute the left-biased union of a list of maps."
  (ordKey [("ms", "the list of maps to combine")]
    [("v", [])] (Types.list (mp tk tv) Types.~> mp tk tv))
  ["unions(ms) returns the map containing every binding of every map in ms; when a key occurs in more\
  \ than one map, the binding from the earliest such map in the list wins.",
   "unions(ms) is equivalent to folding union over ms from the left, starting from empty; unions([])\
  \ is empty, and the bias matches union and hydra.core.lib.sets.unions.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.unions :: Ord k => [Map k v] -> Map k v."]
