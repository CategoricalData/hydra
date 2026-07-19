-- | Primitive declarations for the hydra.lib.maps namespace.
{-# LANGUAGE ScopedTypeVariables #-}

module Hydra.Sources.Kernel.Lib.Maps where

import Hydra.Kernel
import qualified Hydra.Overlay.Haskell.Bootstrap         as Bootstrap
import qualified Hydra.Dsl.Lib.Lists    as Lists
import qualified Hydra.Dsl.Lib.Maps     as Maps
import qualified Hydra.Dsl.Lib.Optionals as Optionals
import qualified Hydra.Dsl.Lib.Pairs    as Pairs
import           Hydra.Overlay.Haskell.Dsl.Typed.Phantoms     as Phantoms hiding (map)
import qualified Hydra.Overlay.Haskell.Dsl.Types             as Types
import           Hydra.Sources.Kernel.Types.All
import           Prelude hiding ((++), filter, lookup, map, null)
import qualified Data.Map                    as M


ns :: ModuleName
ns = ModuleName "hydra.lib.maps"

module_ :: Module
module_ = Module {
            moduleName = ns,
            moduleDefinitions = DefinitionPrimitive <$> definitions,
            moduleDependencies = Bootstrap.unqualifiedDep <$> kernelTypesModuleNames,
            moduleMetadata = Bootstrap.descriptionMetadata (Just "Primitives in the hydra.lib.maps module.")}
  where
    -- The default-impl args below carry an ':: ...Int...' placeholder: the generated 'Hydra.Dsl.Lib.*'
    -- (unlike old 'Meta.Lib.*') exposes the primitive's Ord constraint, so these polymorphic defs need a
    -- concrete type here to satisfy GHC. 'Int' is arbitrary and carries no meaning — the emitted primitive
    -- is type-agnostic and fully polymorphic. See #467.
    definitions = [alter, bimap, delete, difference, elems, empty, filter, filterWithKey,
                   findWithDefault, fromList, insert, intersection, keys, lookup, map, mapKeys,
                   member, null, singleton, size, toList, union, unions]

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

-- Ordering-constrained key signature helper
ordKey :: [(String, [Name])] -> Type -> TermSignature
ordKey extra body = sig $ Types.polyConstrained (("k", [Name "ordering"]) : extra) body

alter :: PrimitiveDefinition
alter = defineWithDefault "alter" "Alter a value at a key using a function which sees the optional current value."
  (sig $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
    ((Types.optional tv Types.~> Types.optional tv) Types.~> tk Types.~> mp tk tv Types.~> mp tk tv))
  ["alter(f, k, m) applies f to Just(v) when m contains key k with value v, or to Nothing when k is\
  \ absent. If f returns Just(v'), the binding (k, v') is set in the result; if f returns Nothing, k\
  \ is removed from the result.",
   "A single primitive that subsumes insert, delete, and adjust.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.alter :: Ord k => (Maybe v -> Maybe v) -> k -> Map k v ->\
  \ Map k v."]
  (("f" ~> "k" ~> "m" ~>
    Optionals.cases
      (var "f" @@ Maps.lookup (var "k" :: TypedTerm Int) (var "m"))
      (Maps.delete (var "k" :: TypedTerm Int) (var "m"))
      ("vNew" ~> Maps.insert (var "k" :: TypedTerm Int) (var "vNew") (var "m")))
    :: TypedTerm ((Maybe v -> Maybe v) -> Int -> M.Map Int v -> M.Map Int v))

bimap :: PrimitiveDefinition
bimap = defineWithDefault "bimap" "Map functions over both the keys and values of a map."
  (sig $ Types.polyConstrained
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
delete = define "delete" "Remove a key from a map."
  (ordKey [("v", [])] (tk Types.~> mp tk tv Types.~> mp tk tv))
  ["delete(k, m) returns m with the binding for k removed; if k is not present, m is returned\
  \ unchanged.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.delete :: Ord k => k -> Map k v -> Map k v."]

difference :: PrimitiveDefinition
difference = define "difference" "Compute the difference of two maps by key."
  (ordKey [("v", [])] (mp tk tv Types.~> mp tk tv Types.~> mp tk tv))
  ["difference(m1, m2) returns the map containing exactly the bindings of m1 whose keys do not appear\
  \ in m2. Only the key set of m2 matters; its values are ignored.",
   "The key-set analogue is hydra.lib.sets.difference.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.difference :: Ord k => Map k v -> Map k v -> Map k v."]

elems :: PrimitiveDefinition
elems = define "elems" "Return the values of a map (in key order)."
  (ordKey [("v", [])] (mp tk tv Types.~> Types.list tv))
  ["elems(m) returns the values of m as a list, ordered by their keys' ascending order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.elems :: Map k v -> [v]."]

empty :: PrimitiveDefinition
empty = define "empty" "The empty map."
  (ordKey [("v", [])] (mp tk tv))
  ["empty is the map with no bindings.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.empty :: Map k v."]

filter :: PrimitiveDefinition
filter = defineWithDefault "filter" "Filter a map by value."
  (sig $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
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
filterWithKey = defineWithDefault "filterWithKey" "Filter a map by key and value."
  (sig $ Types.polyConstrained [("k", [Name "ordering"]), ("v", [])]
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
findWithDefault = defineWithDefault "findWithDefault" "Look up a value with a default if the key is absent."
  (lazySig [0] $ Types.polyConstrained [("v", []), ("k", [Name "ordering"])]
    (tv Types.~> tk Types.~> mp tk tv Types.~> tv))
  ["findWithDefault(def, k, m) returns the value bound to k in m if k is present, or def otherwise.\
  \ Equivalent to maybe(def, identity, lookup(k, m)).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.findWithDefault :: Ord k => v -> k -> Map k v -> v."]
  (("def" ~> "k" ~> "m" ~>
    Optionals.fromOptional (var "def") (Maps.lookup (var "k" :: TypedTerm Int) (var "m")))
    :: TypedTerm (v -> Int -> M.Map Int v -> v))

fromList :: PrimitiveDefinition
fromList = define "fromList" "Build a map from a list of key-value pairs."
  (ordKey [("v", [])] (Types.list (Types.pair tk tv) Types.~> mp tk tv))
  ["fromList(xs) returns the map containing exactly the bindings in xs. If xs contains multiple\
  \ entries for the same key, the last one wins (matching Haskell's fromList behavior).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.fromList :: Ord k => [(k, v)] -> Map k v."]

insert :: PrimitiveDefinition
insert = define "insert" "Insert a key-value pair into a map."
  (ordKey [("v", [])] (tk Types.~> tv Types.~> mp tk tv Types.~> mp tk tv))
  ["insert(k, v, m) returns m with the binding (k, v) added or updated. If k is already present, its\
  \ value is overwritten.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.insert :: Ord k => k -> v -> Map k v -> Map k v."]

intersection :: PrimitiveDefinition
intersection = define "intersection" "Compute the intersection of two maps by key."
  (ordKey [("v", [])] (mp tk tv Types.~> mp tk tv Types.~> mp tk tv))
  ["intersection(m1, m2) returns the map containing exactly the bindings of m1 whose keys also appear\
  \ in m2. On each common key the value is taken from m1; only the key set of m2 matters, and its\
  \ values are ignored.",
   "The key-set analogue is hydra.lib.sets.intersection.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.intersection :: Ord k => Map k v -> Map k v -> Map k v."]

keys :: PrimitiveDefinition
keys = define "keys" "Return the keys of a map (in key order)."
  (ordKey [("v", [])] (mp tk tv Types.~> Types.list tk))
  ["keys(m) returns the keys of m as a list, in ascending order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.keys :: Map k v -> [k]."]

lookup :: PrimitiveDefinition
lookup = define "lookup" "Look up a value in a map by key, returning Nothing if absent."
  (ordKey [("v", [])] (tk Types.~> mp tk tv Types.~> Types.optional tv))
  ["lookup(k, m) returns Just(v) where v is the value bound to k in m, or Nothing if k is not\
  \ present.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.lookup :: Ord k => k -> Map k v -> Maybe v."]

map :: PrimitiveDefinition
map = defineWithDefault "map" "Map a function over the values of a map."
  (sig $ Types.polyConstrained [("v1", []), ("v2", []), ("k", [Name "ordering"])]
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
mapKeys = defineWithDefault "mapKeys" "Map a function over the keys of a map."
  (sig $ Types.polyConstrained
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
member = define "member" "Test whether a key is present in a map."
  (ordKey [("v", [])] (tk Types.~> mp tk tv Types.~> Types.boolean))
  ["member(k, m) returns true iff k is a key in m.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.member :: Ord k => k -> Map k v -> Bool."]

null :: PrimitiveDefinition
null = define "null" "Test whether a map is empty."
  (ordKey [("v", [])] (mp tk tv Types.~> Types.boolean))
  ["null(m) returns true iff m has no bindings.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.null :: Map k v -> Bool."]

singleton :: PrimitiveDefinition
singleton = define "singleton" "Construct a map with a single key-value pair."
  (ordKey [("v", [])] (tk Types.~> tv Types.~> mp tk tv))
  ["singleton(k, v) returns the map containing exactly the binding (k, v).",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.singleton :: k -> v -> Map k v."]

size :: PrimitiveDefinition
size = define "size" "Return the number of key-value pairs in a map."
  (ordKey [("v", [])] (mp tk tv Types.~> Types.int32))
  ["size(m) returns the number of bindings in m as an int32.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.size :: Map k v -> Int (with narrowing to int32)."]

toList :: PrimitiveDefinition
toList = define "toList" "Convert a map to a list of key-value pairs (in key order)."
  (ordKey [("v", [])] (mp tk tv Types.~> Types.list (Types.pair tk tv)))
  ["toList(m) returns the bindings of m as a list of (key, value) pairs, in ascending key order.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.toList :: Map k v -> [(k, v)]."]

union :: PrimitiveDefinition
union = define "union" "Compute the union of two maps; the first map's bindings take precedence on key collision."
  (ordKey [("v", [])] (mp tk tv Types.~> mp tk tv Types.~> mp tk tv))
  ["union(m1, m2) returns the map containing all bindings from m1 plus the bindings of m2 whose keys\
  \ are not in m1. On key collision, the binding from m1 is preferred.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.union :: Ord k => Map k v -> Map k v -> Map k v."]

unions :: PrimitiveDefinition
unions = define "unions" "Compute the left-biased union of a list of maps."
  (ordKey [("v", [])] (Types.list (mp tk tv) Types.~> mp tk tv))
  ["unions(ms) returns the map containing every binding of every map in ms; when a key occurs in more\
  \ than one map, the binding from the earliest such map in the list wins.",
   "unions(ms) is equivalent to folding union over ms from the left, starting from empty; unions([])\
  \ is empty, and the bias matches union and hydra.lib.sets.unions.",
   "Requires an 'ordering' constraint on the key type.",
   "Total. Corresponds to Haskell's Data.Map.unions :: Ord k => [Map k v] -> Map k v."]
